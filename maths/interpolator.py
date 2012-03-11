"""Interpolation of distributions.
"""
from study.cache.property import Cached, lazyprop
from study.snake.decorate import postcompose
from study.snake.sequence import iterable, Tuple
import math

class Interpolator (Cached):
    """Base class for interpolators for distributions.

    Each derived class should implement a model of how the mass in each
    interval between cuts is distributed; based on this model, it should
    implement the properties and methods that raise NotImplementedError,
    below.  Use of @lazyprop for the properties is encouraged; to make that
    easier, Interpolator is based on study.cache.property.Cached.

    Classes which describe distributions in other ways than in terms of
    weights in intervals, but want to interact gracefully with interpolators,
    should be able to do so if they implement, with reasonably compatible
    behaviour, the methods and properties for which this base-class does
    provide implementations.\n"""
    def __init__(self, cuts, mass):
        """Set up interpolator.

        Takes two arguments, cuts and mass, tuple() of which are saved on
        eponymous member variables.  Each must (once tuple() has eaten it;
        they may be iterables) be a sequence; and cuts must be sorted in
        increasing order.  The distribution described has weight mass[i]
        between cuts[i] and cuts[i+1].\n"""
        assert len(mass) + 1 == len(cuts)
        assert not filter(lambda x: x < 0, mass)
        self.cuts, self.mass = tuple(cuts), tuple(mass)
        assert not filter(None, self.map(lambda x, y, w: y < x)), \
            ("Cuts should be sorted", self.cuts)

    @classmethod
    def __interpolator__(cls, cuts, mass):
        """Construct something like self from given cuts and mass.

        A derived class whose constructor's signature doesn't match that of
        this base class should over-ride this method to support the base-class
        signature as best it can.\n"""
        return cls(cuts, mass)

    def __len__(self): return len(self.mass)
    def __cmp__(self, other):
        """Comparison: which is probably greater ?

        Uses self.combine(cmp, other), relying on cmp()'s sensible habit of
        returning -1, 0 or 1 to give fairly sensible results, so that we can
        .weigh((-.5, .5)) and get probabilities for the three answers for
        comparison of self vs others.  If either < or > has probability better
        than a half, it wins; otherwise, if one hits half and the other
        doesn't, the one at half wins; otherwise, call it a draw.\n"""
        split = self.combine(cmp, other)
        assert set(split.cuts).issubset([-1, 0, 1])
        low, mid, hie = self.weigh((-.5, .5), 2)
        if low < 1: b = 0
        else: b = -1
        if hie < 1: return b
        return b + 1

    @lazyprop
    def total(self, cls=None): return sum(self.mass)
    @lazyprop
    def span(self, cls=None): return self.cuts[-1] - self.cuts[0]
    @lazyprop
    @postcompose(tuple)
    def spikes(self, cls=None):
        """Returns a tuple of self's degenerate values.

        If self has non-zero weight in an interval that ends where it starts,
        this indicates a spike in the distribution - a delta function in the
        density.  These tend to need special handling; this property lists
        them in a tuple - if you're lucky, it's empty.

        Note that combining spikes with non-zero density intervals presents
        problems for analysis of correct behaviour.\n"""
        for (l, h, w) in self.filter(lambda l, h, w: l == h):
            yield h

    def simplify(self, count):
        """Returns a simplified version of self.

        Single argument, count, is a target value for len() of the
        result.  This base implementation splits self into count equal
        intervals; derived classes might chose to do something more
        sophisticated.\n"""
        cuts = self.split([0] + [1] * count + [0])
        mass = self.weigh(cuts)
        assert not mass[0] and not mass[-1]
        return self.__interpolator__(cuts, mass[1:-1])

    @iterable
    def filter(self, test=lambda l, h, w: w):
        """Express self as a sequence of non-empty intervals.

        Single argument, test, is optional.  If given, it should be afunction
        taking a (start, end, weight) triple, as for map()'s func, and
        returning a true or false value according as this triple is to be
        included or excluded.  The default selects intervals of non-zero
        weight.

        Returns an iterator over triples (cuts[i], cuts[i+1], mass[i]) where
        test was true.\n"""
        for l, h, w in self:
            if test(l, h, w): yield l, h, w

    @iterable
    def __iter__(self):
        for (l, h, w) in self.map(lambda *args: args):
            yield l, h, w

    @staticmethod
    def density(lo, hi, wt):
        """Compute the average density on an interval, or None for a spike.

        This is suitable as a the first argument to .map(), q.v.  It computes
        the weight per unit width of the interval, unless the interval has no
        width, in which case the interval represents a spike in the
        distribution and None is returned, as a surrogate for infinity.\n"""
        if lo == hi: return None
        return wt / (hi - lo)

    @iterable
    def map(self, func, *more):
        """Gather up results of func applied to self's intervals.

        Single required argument, func, should be a callable taking (at least)
        three arguments:
           * start-value of an interval
           * end-value of the same interval
           * total weight in this interval
        For an example, see .density().

        If any other arguments are passed, each must be an iterable of length
        equal to len(self) and func must accept one further argument for each
        such extra; each call to func shall receive a value from each such
        iterable in addition to the arguments above.  If you can't guess the
        rest of the details of how that works, you're probably best off
        ignoring this paragraph and only ever passing a func that takes just
        the three parameters above !

        Returns the list of values obtained by calling func on each of the
        intervals making up self.\n"""
        return map(func, self.cuts[:-1], self.cuts[1:], self.mass, *more)

    def scale(self, by=None, to=None):
        """Returns a rescaled version of self.

        Optional arguments should usually be passed by name:
          by -- scaling to apply to each weight.
          to -- target value of .total for result.
        You should not pass both (if you do, a consistency check is performed
        and a ValueError is raised if it fails; otherwise, to is ignored).  If
        neither is passed, to = 1 is applied; this provides the natural
        normalisation for a probability distribution.  If by is not passed and
        self.total is zero, a ValueError is raised, as the desired scaling
        cannot be attained (unless to is zero, but asking for that would be
        silly anyway).

        Returns an Interpolator with the same cuts as self but mass scaled as
        specified.\n"""
        if by is None:
            if to is None: to = 1
            try: by = to / self.total
            except ZeroDivisionError:
                raise ValueError('Unattainable total weight requested',
                                 to, self.total)
        elif to is not None:
            if by * self.total != to:
                raise ValueError('Inconsistent scaling and target total',
                                 by, to, self.total)

        return self.__interpolator__(self.cuts,
                                     map(lambda x, b=by: x * b, self.mass))

    @property
    def dispersal(self, cls=None, log=math.log):
        """Computes an entropy-related shape property of the distribution.

        This is -self.entropy with a correction term to eliminate dependence
        on the units used to measure the values, in self.cuts, of the variate
        whose distribution self describes.

        The distribution described by self is really a density (: p :{u*x:
        scalar x}) for some unit u (as used for measurement of the quantity
        the distribution describes); and integrals of p over intervals
        (e.g. entries in self.mass) are dimensionless.  The dimensions of
        integral(p) are those of p's outputs times those of the integrating
        variable, i.e. u; so p's outputs must be of the same kind as 1/u.

        Thus log(p) isn't strictly meaningful (because p's values aren't pure
        numbers); however, u*p is dimensionless and we can take its log,
        giving us integral(p*log(u*p)).  This is then meaningful, but depends
        on our unit, u.  With the choice of u made by our client, we compute
        this integral as self.entropoid.  Using a different unit, w, in place
        of u will add log(w/u)*self.total to self.entropoid:
        integral(p*log(w*p)) = self.entropoid + log(w/u) * self.total

        Furthermore, self's distribution is meant to be understood as being
        independent of self.total, i.e. integral(p).  In general, scaling p
        down by a factor k also changes integral(: p*log(u*p) :), to
            integral(: log(u*p/k)*p/k :)
                 = (self.entropoid - self.total * log(k)) / k

        So we have to decide what unit to use and what overall scaling to
        apply.  For the overall scaling, a natural choice is k = self.total,
        so as to normalise p to yield self.total = 1.  If we replace our unit,
        u, used implicitly in computing self.entropoid, with some more apt
        unit w, this will give us, as integral(-log(w*p/k)*p/k),
            r = log(self.total*u/w) - self.entropoid/self.total

        The issue of chosing a sensible unit is, as ever, non-trivial.  I
        intuit that the dispersal should be translation-invariant;
        i.e. replacing p with (: p(x-z) &larr;x :) shouldn't change its
        dispersal, for constant z, e.g. an average of the distribution.  Thus
        a sensible unit, w, must needs be obtained from the width of the
        distribution, in one guise or another.  The combination of scale
        invariance and translation invariance implies that the resulting
        dispersal will describe the *shape* of the distribution, rather than
        anything else.  Standard deviation provides a reasonably sensible
        scale derived from the distribution itself, by which to eliminate
        dependence on the units used, so I use that here.  For delta
        distributions, with a single spike, the standard deviation is zero;
        this case is thus handled specially.

        For a simple uniform distribution, p(x) = 1/(b-a) for b<x<a, zero for
        x > a or x < b, we have .entropoid = log(u/(b-a)) and standard
        deviation (b-a)/2/sqrt(3).  Using this last as w gives r =
        log(u/(b-a)) +log(12)/2 -log(u/(b-a)) = log(12)/2, independent of b, a
        and u.  For a gaussian, with standard deviation s, we have normalised
        density p(x) = exp(-(x/s)**2/2).dx/s/sqrt(2*pi) so -.entropoid
        integrates p(x).((x/s)**2/2 +log(s/u) +log(2*pi)/2); the integral of
        p(x).(x/s)**2 is just 1, by definition of s (given mean zero); and the
        other terms are constants times integra(p) = 1, so we get 1/2
        +log(s/u) +log(2*pi)/2.  Using s as w, we get r = log(2*pi*e)/2.  For
        buth uniform and gaussian, we get answers independent of standard
        deviation, so they'll stay the same as we let the distribution tend
        towards a spike by reducing its width parameter; their values are 1.24
        and 1.42 respectively, so a spike's dispersal is ill-defined, but O(1)
        values are reasonable.  I thus chose to use 1 as fall-back value when
        the usual computation fails.\n"""

        a = self.total
        try: return log(a / self.normal[1]**.5) - self.entropoid / a
        except (ZeroDivisionError, ValueError): return 1

    __munge = Tuple.cartesian
    @classmethod
    def _combine(cls, *whom):
        """Tool for use by implementations of combine().

        Each argument should be an Interpolator.  Note that this is a class
        method, so calling it on an instance *doesn't* automagically include
        the instance as first argument; pass it explicitly if you want it
        (which you probably do).

        Returns an iterator over (box, weight) twoples, where box is a tuple -
        with each box[i] being a (lo, hi) twople of bounds of an interval of
        whom[i] - and weight is the product of the weights of the respective
        intervals.  (Intervals with zero weight are skipped, using .filter(),
        so each box should have non-zero weight.)  These yields are the atoms
        from which combine() must build up its results.\n"""
        return apply(cls.__munge,
                     (tuple,) + tuple(x.filter() for x in whom)
                     ).map(lambda ts: (tuple(ts.map(lambda x: x[:-1])),
                                       ts.map(lambda x: x[-1]).product()))

    # API to be implemented by derived classes:

    @classmethod
    def gaussian(cls, mean=0, variance=1, count=None):
        """Return a distribution modelling a normal distribution.

        All arguments are optional:
          mean -- mid-point of the distribution (default: 0)
          variance -- square of the standard deviation (default: 1)
          count -- suggested number of data-points to use, or None to let
                   implementation chose.

        Each implementation of Interpolator should provide at least some way
        to model a gaussian.  This should normally be implemented as a static
        or class method.  When count is given, and not None, implementations
        should ensure that len() of the result is at least reasonably close to
        count.  See also: the normal property.\n"""
        raise NotImplementedError

    @property
    def normal(self, cls=None):
        """Mean and variance tuple as property.

        Returns the mean and variance of the distribution, as a twople.  These
        are the data one would supply to gaussian() to obtain a distribution
        similar to self; .gaussian(m,v).normal should return at least a
        reasonable approximation to (m,v).\n"""
        assert cls is None
        raise NotImplementedError

    @property
    def entropoid(self, cls=None):
        """Raw entropy measure for the distribution, as property.

        Returns the integral of ln(p)*p, where p is the density function of
        the distribution and ln is the natural logarithm (to base e); this is
        a standard measure of entropy or (when divided by ln(2)) of
        information content.  Note, however, that the result depends on the
        unit of measurement, for the quantity whose distribution self
        describes, used in obtaining the entries in cuts.  See .dispersal for
        a scale-invariant alternative.\n"""
        assert cls is None
        raise NotImplementedError

    def split(self, weights):
        """Cuts the distribution into pieces in the proportions requested.

        Required argument, weights, is a list of non-negative values, having
        positive sum.  A scaling is applied to all entries in the list to make
        its sum equal to self.total().

        Returns a list, result, one entry shorter than weights, for which
        self.weigh(result, sum(weights)) == weights (give or take rounding
        errors).\n"""
        raise NotImplementedError

    def weigh(self, seq, total=None):
        """Integrates self's distribution between positions in a sequence.

        Required argument, seq, is a sequence of positions in the
        distribution.  The sequence is presumed to be sorted; results may be
        haphazard otherwise.

        Optional argument, total, is the desired total for the result list, or
        None.  If None, no normalisation is performed, so sum(result) shall be
        self.total; otherwise, the result list is normalised so that
        sum(result) == total.

        Returns a tuple of weights, result, one entry longer than the
        sequence, with each being the integral over the distribution between
        two bounds:
            result[0] -- from minus infinity to seq[0]
            result[1+i] -- from seq[i] to seq[1+i]
            result[-1] -- from seq[-1] to infinity

        If one of the seq[i] is in self.spikes, the weight of the spike is
        shared evenly between the intervals on either side, unless the spike
        value is repeated in seq, in which case all of the weight of the spike
        goes into the implied zero-width result interval.\n"""
        raise NotImplementedError

    def __add__(self, other):
        """Merges two distributions.

        Single argument, other, is the other distribution.

        When the densities represent two independent samples from a
        population, the weight each places within each interval should be
        added to the weight the other places in that interval to obtain the
        composite density describing both samples; the result is a pointwise
        addition of densities.  If one sample gathered more data than the
        other but both have since been normalised, you need to scale one of
        them by its data-size as a fraction of the other's when doing this.

        Where either has a spike, the sum shall also.  No normalisation should
        normally be performed.\n"""
        raise NotImplementedError

    def __mul__(self, other):
        """Pointwise product with another distribution.

        Takes one argument, other; another Interpolator.  Returns an
        interpolator representing a pointwise product of self and other.

        If we interpret each distribution as a probability (or likelihood)
        distribution for some common variate, each obtained by considering
        independent data, then taking both sets of data into account would
        imply a probability, for the variate being in any given interval,
        equal to the product of the two separate distributions' probabilities
        for the variate being in that interval.  The resulting distribution's
        density is thus the pointwise product of the densities of the two
        distributions.

        Care is required to handle spikes sensibly.  In an interval of width h
        about a value at which one distribution has a spike of weight w, where
        the other has density d, we have h.d and w as the two distributions'
        probabilities of being in the interval, with product h.d.w.  This
        tends to zero as the interval narrows in on the spike, so the product
        density shouldn't have a spike here.  However, if the distributions
        both have a spike at some value, the product has a spike there, too.

        If self and other are both normalised so .total is 1, the total of the
        pointwise product gives an indication of how well self and other are
        correlated with one another; if they have relatively high weights in
        the same places, the pointwise product's .total will tend to be
        larger; if each has its high weights where the other has low, in
        contrast, the .total will tend to be smaller, attaning zero if the two
        distributions don't overlap.  Two uniforms, 1/(b-a) from a to b and
        1/(d-c) from c to d, with a <= c < b <= d, have product 1/(b-a)/(d-c)
        from c to b, which wants normalisation to 1/(b-c), so rescaling by
        (b-a).(d-c)/(b-c).  Rescaling by (b-a).(d-c)/(d-a) would leave a good
        indicator of correlatedness, (b-c)/(d-a) as the product distribution's
        .total; implementations are encouraged to perform a rescaling along
        these lines, so that the result's .total does serve as a measure of
        correlatedness; self.span * other.span / result.span is the required
        scale factor to make .total be O(1).  Such a scaling also fixes some
        issues with the implicit dimensions of the weights; just be careful to
        adapt sensibly when any of the spans in question is zero.\n"""
        raise NotImplementedError

    def combine(self, func, *others):
        """Combine two interpolators.

        First argument, func, is a function to use in combining self with the
        rest.  Each subsequent argument should be an Interpolator.  The number
        of such other arguments, plus one for self, is the number of arguments
        that shall be passed to func; its first argument shall be drawn from
        self's range of values and each of its subsequent arguments from that
        of successive other arguments.  (Implementations may find ._combine()
        useful in packaging that.)  The returns from such calls shall be used
        to select cut-points for the result and to divide up weight between
        them.

        The result's length is the product of the lengths of self and all
        others; this is apt to be quite large.  Callers may benefit from
        calling .simplify(n) on the result, for some suitable n.\n"""
        raise NotImplementedError

del Cached, postcompose

class PiecewiseConstant (Interpolator):
    """Interpolator based on a piecewise constant function.

    Implements Interpolator by treating each interval between cuts as a
    uniform distribution with the indicated total weight, save that each spike
    is (as usual) treated as a simple weight at the spike value.  Crude but
    straightforward.  For method documentation, check matching methods of
    Interpolator.\n"""

    def split(self, weights):
        assert not filter(lambda x: x < 0, weights), \
            ('Weights should not be negative', weights)
        scale = self.total / sum(weights)

        cut, load = self.cuts, self.mass
        ans, prior, i = [], cut[0], 0
        for w in map(lambda x, s=scale: x*s, weights[:-1]):

            try: avail = load[i]
            except IndexError: avail = 0
            # Maybe starting part way through a band:
            if prior > cut[i]:
                avail *= (cut[1+i] - prior) / (cut[1+i] - cut[i])

            # Swallow any bands we can:
            try:
                while w >= avail:
                    w, i, prior = w - avail, 1+i, cut[1+i]
                    avail = load[i]
            except IndexError: # fell off end
                # There's no weight left to the right of prior
                assert 1e-6 * sum(weights) > sum(weights[len(ans)+1:])
                # Modulo rounding, w should now be zero
                assert 1e-6 * self.total > w
                w = 0 # don't let rounding errors mess us up below

            # grab what we need from present band:
            if w > 0: prior += w * (cut[i+1] - cut[i]) / load[i]
            ans.append(prior)

        return tuple(ans)

    @staticmethod
    def __share(weight, result, seq, s):
        """Handle spike at interval boundary for weigh().

        Arguments:
          weight -- weight of the spike
          result -- list among whose entries to share this weight
          seq -- split-points between intervals weighed by result
          s -- position of spike in seq; seq[s] is at the spike

        Returns the index, t, of the entry in result to which the weight of
        intervals after spike should be added.  This is also the least t > s
        for which seq[t] > seq[s], or len(seq) if there is no such t.

        See Interpolator.weigh() for the requirements.  If s is the only index
        in seq that's at the spike, half of weight goes into each of result[s]
        and result[s+1].  Otherwise, seq[s:t] has more than one entry, with
        all entries <= seq[s]; these delimit t-s degenerate intervals (usually
        just one) among which weight is shared evenly.\n"""
        t = s + 1
        if t >= len(seq) or seq[t] > seq[s]:
            w = weight * .5
            result[s] += w
            s += 1
            result[s] += w
        else:
            while t + 1 < len(seq) and seq[t + 1] <= seq[s]: t += 1
            if t == s + 1: w = weight
            else: w = weight * 1. / (t - s)
            s += 1
            while s <= t:
                result[s] += w
                s += 1
        return s

    def weigh(self, seq, total=None):
        result, load, cut = [ 0. ] * (1 + len(seq)), self.mass, self.cuts
        if not self.total: return tuple(result)
        if len(load) < 2:
            # special case: only one weight, delta function.
            s = len(filter(lambda x, r=cut[0]: x < r, seq))
            assert s == len(seq) or seq[s] >= cut[0], \
                ('mis-sorted positions', seq)
            if s < len(seq) and cut[0] == seq[s]: # even split
                self.__share(load[0], result, seq, s)
            else: result[s] = load[0]
        else:
            # sensible case where we have at least two weights.
            i = s = 0 # we're processing size[i] for result[s]
            last = None # last seq point if in present cut-gap, else None

            try: # step over any entries in seq that precede all cuts
                while seq[s] < cut[0]: s += 1
            except IndexError: pass

            try: # loop until end of row ... can happen from inner loop.
                while True:
                    # usually, cut[i] <= stop <= cut[i+1]
                    try: stop = seq[s]
                    except IndexError:
                        stop = cut[-1] # gather everything after seq[-1]

                    if stop < cut[i]:
                        # out-of-order entries in seq - or mis-incremented i.
                        assert seq[s-1] >= stop, \
                            'Apparently, I incremented i in error'
                        s += 1 # and leave last alone ...

                    elif stop < cut[1+i]:
                        if last is None: last = cut[i]
                        if stop > last:
                            result[s] += load[i] * (stop - last) / (cut[1+i] - cut[i])
                        last, s = stop, 1 + s

                    elif cut[i] == stop == cut[i+1]:
                        assert last is None
                        if s < 1 and seq[1] == stop: s += 1
                        else:
                            weight, i = load[i], i + 1
                            while cut[i+1] == stop:
                                weight += load[i]
                                i += 1

                            s = self.__share(weight, result, seq, s)
                            assert s >= len(seq) or seq[s] > stop

                    else:
                        if last is not None:
                            assert cut[i] <= last < cut[1+i]
                            result[s] += load[i] * (cut[1+i] - last) / (cut[1+i] - cut[i])
                            last, i = None, 1 + i

                        while stop >= cut[1+i]:
                            # Do we need to __share(load[i],...) ?
                            if stop <= cut[i] and 1 < s + 1 < len(seq): break
                            result[s] += load[i]
                            i += 1

            except IndexError:
                assert i == len(load), \
                    'algorithm exited loop surprisingly at %d/%d, %d/%d' \
                    % (i, len(load), s, len(seq))

        if total is not None:
            assert 0 != sum(result) # == self.total, known non-zero
            result = map(lambda x, r=total * 1. / sum(result): x * r, result)
            # assert sum(result) == total, give or take rounding errors.

        return tuple(result)

    @lazyprop
    def normal(self, cls=None):
        cut, siz = iter(self.cuts), iter(self.mass)
        zero = one = two = 0.
        # [zero,one,two][i] == integral(lambda x: x**i * p(x))

        last = cut.next()
        for c in cut:
            w = siz.next()
            # integral(: last < x < c; x**n *w/(c-last) &larr;x :)
            # is just w*(c**(1+n) -last**(1+n))/(1+n)/(c-last)
            # which is w*average(lambda i: last**i * c**(n-i), range(1+n))
            last = c
            zero += w
            one += w * (c + last) / 2
            two += w * (c * c + last * c + last * last) / 3

        if not zero: raise ValueError(
            'Degenerate distribution has no mean or variance', self)

        mean = one / zero
        two /= zero
        # If difference > 10%, assume the real difference drowned any rounding
        # errors:
        if mean**2 < .9 * two:
            return mean, two - mean**2

        # Probably too much rounding error;
        # recompute two as sum(: (x-mean)**2 * p(x) &larr;x :)
        siz, cut, two = iter(self.mass), iter(self.cuts), 0.
        last = cut.next() - mean
        for c in cut:
            c -= mean
            two += siz.next() * (c * c + last * c + last * last) / 3
            last = c

        return mean, two / zero

    @lazyprop
    def entropoid(self, cls=None,
                  each=lambda l, h, w, g=math.log: w * g(w / (h-l))):
        """See Interpolator.entropoid for definition.

        Since p is piecewise constant, the integral is a sum of simple terms:
        each term is an integral between two entries in self.cuts, h apart, in
        which lies the matching weight, w, in self.mass; this makes p = w/h
        over the interval, contributing h.(log(w/h).w/h) = w.log(w/h).  This
        makes the integration easy.

        It is not immediately clear what to do with a delta function, or a
        distribution including any such spikes; this shall divide by zero on
        such distributions.\n"""
        return self.map(each).sum()

    def __add__(self, other):
        spikes = set(self.spikes + other.spikes)
        # Deliberately duplicate spikes of either in cuts:
        cuts = list(set(self.cuts + other.cuts)) + list(spikes)
        cuts.sort()
        me, yo = self.weigh(cuts), other.weigh(cuts)
        assert 1e-6 * self.total > max(me[0], me[-1])
        assert 1e-6 * other.total > max(yo[0], yo[-1]), (self, other, cuts)
        me, yo = me[1:-1], yo[1:-1]
        assert len(cuts) - 1 == len(me) == len(yo) > 0

        # Addition of densities is nice and simple :-)
        cuts, mass = self.__clean(cuts, map(lambda x, y: x + y, me, yo))
        return self.__interpolator__(cuts, mass)

    @classmethod
    def __clean(cls, cuts, mass): # tool for __mul__
        """Eliminate cuts between intervals of equal density.

        Takes two arguments, a list of cuts and a list of masses in the
        intervals between the cuts.  No cut should be equal to more than one
        other.  Where dividing a mass by the width of its interval yields the
        same as the corresponding ratio for the next interval, the two
        intervals are merged into one: the cut between them is deleted, the
        mass in the second of them is added into that of the first and the
        entry for the second's mass is deleted (so later mass entries remain
        in sync with the modified cuts).\n"""
        dens = iter(map(cls.density, cuts[:-1], cuts[1:], mass))
        i, last = 0, dens.next()
        for d in dens:
            if d == last:
                assert d is not None, 'Adjacent spikes !'
                mass[i] += mass[i+1]
                del mass[i+1], cuts[i+1]
            else:
                i += 1
                last = d

        return cuts, mass

    @staticmethod
    def __mul(lo, hi, me, yo): # tool for __mul__
        """Implements the required per-interval multiplication.

        If lo == hi, we're on a mutual spike whose weight should be the
        product of weights; otherwise, we're in an ordinary interval whose
        density (weight / width) should be the product of densities.\n"""
        if lo == hi: return me * yo
        return me * yo / (hi - lo)

    def __mul__(self, other):
        spikes = set(self.spikes).intersection(other.spikes)
        # Deliberately duplicate joint spikes in cuts:
        cuts = list(set(self.cuts + other.cuts)) + list(spikes)
        cuts.sort()
        me, yo = self.weigh(cuts, 1), other.weigh(cuts, 1)
        assert 1e-6 * self.total > max(me[0], me[-1])
        assert 1e-6 * other.total > max(yo[0], yo[-1])
        me, yo = me[1:-1], yo[1:-1]
        assert max(map(abs, (sum(me)-1, sum(yo)-1))) < 1e-5
        assert len(cuts) - 1 == len(me) == len(yo) > 0

        # Want pointwise product of input densities; on an interval with
        # weights x, y and width g, the densities are x/g and y/g, with
        # product x*y/g/g, so the total weight of the result is x*y/g in that
        # interval.  On a joint spike, with weights x, y, we want weight x *
        # y.  We also apply the usual rescaling.
        if self.span and other.span:
            assert cuts[-1] > cuts[0]
            scale = self.span * other.span / (cuts[-1] - cuts[0])
        elif self.span: assert self.span == cuts[-1] - cuts[0]
        elif other.span: assert other.span == cuts[-1] - cuts[0]
        else: assert cuts[-1] == cuts[0]
        mass = map(lambda x, n=scale: x * n,
                   map(self.__mul, cuts[:-1], cuts[1:], me, yo))
        assert len(mass) + 1 == len(cuts)

        cuts, mass = self.__clean(cuts, mass)
        return self.__interpolator__(cuts, mass)

    # <tools for="combine">
    @iterable
    def plane(m, n, T=Tuple): # tool-function for slices
        """Return iterator over equal-sum corners of a cube.

        See slices(), below, for details; this provides the individual
        iterators it yields.  Each yield is in fact a Tuple() so that one can
        .map() it usefully.\n"""

        # Used to map a set to a sequence with 1 at each index in the set, 0
        # everywhere else (see yield):
        each = T(range(n)).map

        # Iterate over subsets of range(n) of size n-m:
        ks = range(m, n)
        try:
            while True:
                yield T(each(ks.__contains__))
                i = 0
                # This shall IndexError when we're done:
                while ks[i] <= i: i += 1
                last = ks[i] = ks[i] - 1
                while i > 0:
                    i -= 1
                    ks[i] = last = last - 1
                assert last >= 0
                assert ks[-1] + 1 + m >= n
        except IndexError: pass

    @iterable
    def slices(n, cut=plane): # tool-function for single(), evaluate()
        """Returns an iterator over iterators over corners of a cube.

        Single argument, n, is a positive integer.  Each 'corner of a cube' is
        a tuple of length n whose entries are either 0 or 1.  The return from
        planes(n) is an iterator; for each m from 1 through n-1, it yields an
        iterator over the corners whose sum is m.  Note that the diametrically
        opposite corners that are all 0 or all 1 are skipped.\n"""

        m = n # number of zero co-ordinates of each corner
        while m >= 0:
            yield cut(m, n)
            m -= 1
    del plane

    def single(f, xs, ys, traverse=slices): # tool-function for evaluate()
        """Kludge round divide-by-zero errors.

        First argument, f, is a function; second is a tuple of arguments
        suitable for passing to it, as apply(f, xs); third is another tuple,
        ys, of partners for the xs, for use if that divides by zero.

        Consider (first) the simple case of f(x) = 1/x with x = 0; you call
        this function as single(f, (x,), (y,)), where we're interested in f's
        range of outputs for inputs between x and y.  So we want the range of
        values of 1/z with x = 0 < z <= y (or y >= z > 0 = x).  We're really
        modelling a random variate, Z, distributed over the interval between x
        and y; and want to model the variate 1/Z; and we'll be modelling this
        last by a uniform density on some range.  That range can't be infinite
        in width, so we have to clip it, effectively relacing x with some
        value a little closer to y, to avoid dividing by zero.  If Z is
        uniformly distributed between x = 0 and y, 1/Z shall have its
        nearer-zero bound at 1/y and median at 2/y, so one obvious candidate
        representation is to use 3/y as the far-from-zero bound.

        The reader is welcome to consider what happens if Z's density is z**u
        for some u > 0, so as to give density zero at x = 0.  I find that this
        gives mean and median close beyond 1/y for huge u, tending to infinity
        and 2/y, respectively, as u tends to 0 from above.  This leaves no
        useful way to use the mean to suggest a half-way sensible answer,
        while median encourages the answer above.  However, this analysis does
        reveal that, for a near-uniform distribution between x and y, the mean
        is further out than the median would suggest, encouraging us to make
        the interval wider than we otherwise would.  So I chose to use 4/y
        as the far bound, interpreted as f((3*x+y)/4); replace x with the
        point in its interval a quarter of the way towards y.

        When we have several parameters, we don't necessarily know which of
        them are responsible for the division by zero; but we can try
        substituting for each and see what we get.  If that still gets
        nothing, we can try substituting in each possible pair of parameters;
        and so on for steadily more parameters substituted.  If substituting
        for all parameters still fails, fall back on actually re-raising the
        divide-by-zero error we originally got from apply(f, xs).\n"""

        assert len(xs) == len(ys)
        first = True # defer adjusting ys; we probably don't need to

        # Scan over index subsets, of increasing size, on which to use ys:
        for seq in traverse(len(xs)):
            row = []
            for ks in seq:
                assert set(ks).issubset([0, 1])
                try: row.append(apply(
                        f, ks.mapwith(lambda k, *vs: vs[k], xs, ys)))
                except ZeroDivisionError, err: pass

            # Use average over first size to give any results:
            if len(row) > 1: return sum(row) * 1. / len(row)
            try: return row[0]
            except IndexError: pass

            if first:
                assert ks == (0,) * len(xs) # so we didn't use ys yet
                what = err # seq only gave us one ks; save its error
                ys = map(lambda x, y: (3 * x + y) * .25, xs, ys)
                first = False # don't do this again

        assert not first # i.e. traversal gave us at least *something*
        # Final fall-back, re-using original error on failure:
        raise what

    def morebad(prior, more): # tool-function for evaluate
        """Helper for accumulating similar exceptions' .args values.

        First argument, prior, is a (possibly empty) tuple of accumulated
        .args entries from earlier errors.  Second argument, more, is the
        .args of a new error.  Commonly, the first entry in this shall have
        been seen in some earlier error, as most exceptions use a simple
        string as their sole argument.  When more[0] is in prior, it is
        omitted from the return; otherwise, more is returned.\n"""
        if more and more[0] in prior: return more[1:]
        return more

    # tool-function for __join:
    def evaluate(f, box,
                 s=single, more=morebad, all=slices):
        """Evaluate a function at corners of a cuboid.

        First argument, f, is a call; second, box, is a sequence of twoples
        (lo, hi) of bounds of intervals; len(box) is the number of parameters
        f shall be passed; f is expected to return values that behave like
        numbers.  The cartesian product of the intervals described by box's
        entries constitutes a cuboid in a len(box)-dimensional space; this
        function returns f's values at or near as many as possible of the
        corners of this cuboid.

        Some intervals may abut values, for the relevant input to f, at which
        f grows without bounded (expected to be manifest as division by zero);
        when this happens, there are corners of the cuboid (quite possibly
        whole faces) on which f raises ZeroDivisionError.  When this arises
        for a corner, points inside the cuboid or on its boundary are sought
        near the corner, at which to evaluate f; if none such can be found,
        the exception is remembered.  Otherwise, a representative value from
        some such values shall be used as f's value 'at' the corner.

        If values can be found for at least two corners, a list of the values
        found is returned; otherwise, a ZeroDivisionError is raised, using
        information from the exceptions at all corners that failed.\n"""

        row, bad = [], ()
        for seq in all(len(box)):
            for ks in seq:
                xs = tuple(map(lambda p, i: p[i], box, ks))
                ys = tuple(map(lambda p, i: p[1-i], box, ks))
                try: row.append(s(f, xs, ys))
                except ZeroDivisionError, what:
                    bad += more(bad, what.args)

        if len(row) < 2:
            if not bad: bad = ('Division by zero',)
            raise apply(ZeroDivisionError, bad + ('calling', f, 'on', box))
        return row
    del slices, single, morebad

    # tool-classes for __join
    class Tile (object):
        def __init__(self, xs, wt):
            assert wt, 'non-empty'
            assert len(xs) == len(self), 'right number of entries'
            assert not filter(None, map(lambda x, y: y < x,
                                        xs[:-1], xs[1:])), 'sorted'
            assert len(xs) < 2 or xs[0] < xs[-1], 'spike or interval'
            self.kinks, self.weight = xs, wt

        def __nonzero__(self): return self.kinks[-1] > self.kinks[0]
        def __cmp__(self, other):
            return cmp(self.start, other.start) or cmp(self.stop, other.stop)

        @lazyprop
        def start(self, cls=None):
            assert cls is None
            return self.kinks[0]

        @lazyprop
        def stop(self, cls=None):
            assert cls is None
            return self.kinks[-1]

        def weigh(self, lo, hi):
            """Returns how self.weight contributes to an interval.

            Two arguments, lo and hi, are the start and end values of an
            interval.  Returns a triple of weights; the middle one is how much
            of self's weight should be placed in the interval from lo to hi;
            the other two are zero unless: lo != hi and self gives a delta
            contribution at lo or hi.  Any such spike at an x with lo < x < hi
            contributes its whole weight to the middle; when lo == hi, so does
            any spike at this value.  When lo < hi, a spike at lo should
            contribute half its weight each to the first and middle entries of
            the triple; any spike at hi should contribute half its weight each
            to middle and third entries.\n"""
            raise NotImplementedError

        def meets(self, lo, hi):
            """Returns true if self has any weight between lo and hi.
            """
            raise NotImplementedError

    class Spike (Tile):
        @classmethod
        def __len__(cls): return 1
        def weigh(self, lo, hi):
            here = self.start
            if lo == hi == here or lo < here < hi:
                return self.weight
            # else: interval starts or ends at spike, but another interval
            # matches the spike exactly - and we give all our weight to it.
            return 0

        def meets(self, lo, hi):
            here = self.start
            return lo < here < hi or lo == here == hi

    class Interval (Tile):
        @staticmethod
        def linear(lo, hi, z, p, t):
            """Triangle-slicer.

            On the right triangle with elevation zero at z, rising to t at p,
            then dropping to zero beyind, compute the area between lo and
            hi. Caller should ensure lo < hi and z < p by negating all of
            them, and swapping bounds, if the slope is reversed.\n"""
            if p <= z: return 0
            assert hi > z and p > lo, (lo, hi, z, p)

            # (x, y) and (r, s) are points on the line from (z, 0) to (p, t);
            # x is max(lo, z), r is min(hi, p).
            g = t * 1. / (p - z)
            if lo <= z: x, y = z, 0
            else: x, y = lo, (lo - z) * g
            if hi >= p: s, t = p, t
            else: s, t = hi, (hi - z) * g
            assert s >= x and t >= 0 and y >= 0, (lo, hi, z, p, t)
            return (s - x) * .5 * (t + y)

        def meets(self, lo, hi):
            return lo < self.stop and self.start < hi
    del Tile

    class Flat (Interval):
        @classmethod
        def __len__(cls): return 2
        def weigh(self, lo, hi):
            l, h = self.kinks
            if lo <= l and h <= hi: return self.weight
            lo, hi = max(l, lo), min(h, hi)
            return (hi - lo) * self.weight * 1. / (h - l)
            
    class UpDown (Interval):
        @classmethod
        def __len__(cls): return 3
        def weigh(self, lo, hi):
            l, m, h = self.kinks
            tall = self.weight * 2. / (h - l) # height of middle of triangle
            if lo < m: tot = self.linear(lo, hi, l, m, tall)
            else: tot = 0
            if hi > m: tot += self.linear(-hi, -lo, -h, -m, tall)
            return tot

    class UpAlongDown (Interval):
        @classmethod
        def __len__(cls): return 4
        def weigh(self, lo, hi):
            a, b, c, d = self.kinks
            # if b == c we'll work just like UpDown, as it happens

            # Central plateau has width c-b and contributes tall*(c-b) to
            # weight; triangles at ends have widths d-c and b-a and
            # contribute tall * half the sum of these to weight; and
            # (c-b) + .5 * (d-c + b-a) = .5 * (d+c-b-a), so:
            tall = self.weight * 2. / (d + c - b - a)
            if lo < b: tot = self.linear(lo, hi, a, b, tall)
            else: tot = 0
            if lo < c and hi > b: tot += (min(hi, c) - max(lo, b)) * tall
            if hi > c: tot += self.linear(-hi, -lo, -d, -c, tall)
            return tot
    del Interval

    @staticmethod
    def __join(f, box, mass,
               each=evaluate, form=(Spike, Flat, UpDown, UpAlongDown)):
        """Combine my and yo according to f and update kink.

        First argument, f, is a function taking two inputs; next two
        arguments, my and yo, are triples (l, h, w) describing weight w in
        interval {x: l <= x <= h}; f's first argument shall be drawn from my's
        interval, second from yo's.  Returns a Tile object which describes how
        the product of weights of my and yo are distributed over a range of
        values obtained by such calls to f.  Values added to kink are the ones
        at which the density, of the distribution described by the returned
        object, isn't smooth.

        Because intervals may be bounded by values at which f is ill-behaved,
        ZeroDivisionError is handled specially; see single's doc.\n"""

        row = each(f, box)

        if len(row) < 2:
            raise apply(ZeroDivisionError,
                        bad + ('calling', f, 'on', (a, b), (x, y)))
        all = set(row)
        if len(all) == 1: cls, row = form[0], tuple(all)
        else:
            assert len(all) > 1, 'row is non-empty => so is all'
            row.sort()
            if len(row) > len(form):
                # Ditch middle entries (except maybe one):
                n, r = divmod(len(form), 2)
                if r: row = row[:n] + [ row[len(row)/2] ] + row[-n:]
                else: row = row[:n] + row[-n:]
            cls = form[len(row) - 1]

        return cls(tuple(row), mass)
    del evaluate, Spike, Flat, UpDown, UpAlongDown

    def combine(self, func, *others):
        mix, kink, spike, join = [], set(), set(), self.__join
        for s, m in self._combine(self, *others):
            here = join(func, s, m)
            mix.append(here)
            kink.update(here.kinks)
            if not here: spike.add(here.start)

        mix.sort()
        # Duplicate entries at which there are spikes, so that every spike in
        # our answers is preserved.  (Each spike of self combines with each
        # spike of each other to produce a spike in the result.)
        kink = list(kink) + list(spike)
        kink.sort()

        cut = iter(kink)
        try: lo = cut.next()
        except StopIteration:
            raise apply(ValueError, ('No data to combine', self) + others)
        try: hi = cut.next()
        except StopIteration:
            return self.__interpolator__((lo, lo), 1)

        zero = 0 * mix[0].weight # a zero weight
        live, wait, step, part = [], [], False, zero
        try:
            while True:
                assert lo <= hi
                if live and not live[0].meets(lo, hi): live = live[1:]
                elif mix and mix[0].meets(lo, hi):
                    mix, b, i = mix[1:], mix[0], len(live)
                    # life is ordered by live[i].stop
                    if i:
                        while i > 0:
                            if live[i-1].stop <= b.stop: break
                            i -= 1
                        live.insert(i, b)
                    else: live.append(b)
                elif step: hi, step = cut.next(), False
                else:
                    tot = zero
                    for tile in live:
                        assert tile.meets(lo, hi)
                        w = tile.weigh(lo, hi)
                        assert w >= 0, (tile, lo, hi)
                        tot += w

                    wait.append(tot)
                    step, lo = True, hi

        except StopIteration:
            # That was triggered by trying to step:
            assert step and lo is hi
            # We should have kept going, if we had more tiles:
            assert not mix
            # We should have drained live before trying to step:
            assert not live or live[-1].stop == lo

        return self.__interpolator__(kink, wait)
    # </tools>

del lazyprop, math, iterable, Tuple
