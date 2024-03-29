"""Interpolation of distributions.

See study.LICENSE for copyright and license information.
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
        cuts, mass = tuple(cuts), tuple(mass)
        # Drop leading and trailing zero-mass intervals.
        # Sometimes the easiest way to navigate around Sample's old
        # weight-scheme's eccentric tails is to use zero-weight
        # bounds, that we don't need here.
        while mass and not mass[0]:
            mass, cuts = mass[1:], cuts[1:]
        while mass and not mass[-1]:
            mass, cuts = mass[:-1], cuts[:-1]
        assert len(mass) + 1 == len(cuts)
        self.cuts, self.mass = cuts, mass
        assert all(x >= 0 for x in self.mass)
        assert all(x <= y for x, y in zip(self.cuts[:-1], self.cuts[1:])), \
            ("Cuts should be sorted", self.cuts)

    @classmethod
    def _interpolator_(cls, cuts, mass):
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
        low, mid, hie = split.weigh((-.5, .5), 2)
        if low < 1: b = 0
        else: b = -1
        if hie < 1: return b
        return b + 1

    @lazyprop
    def total(self): return sum(self.mass)
    @lazyprop
    def span(self): return self.cuts[-1] - self.cuts[0]
    @lazyprop
    def spikes(self):
        """Returns a tuple of self's degenerate values.

        If self has non-zero weight in an interval that ends where it starts,
        this indicates a spike in the distribution - a delta function in the
        density.  These tend to need special handling; this property lists
        them in a tuple - if you're lucky, it's empty.

        Note that combining spikes with non-zero density intervals presents
        problems for analysis of correct behaviour.\n"""
        return tuple(h for l, h, w in self if l == h)

    def simplify(self, count):
        """Returns a simplified version of self.

        Single argument, count, is a target value for len() of the
        result.  This base implementation splits self into count equal
        intervals; derived classes might chose to do something more
        sophisticated.\n"""
        cuts = self.split([0] + [1] * count + [0]) # TODO: don't blur spikes
        mass = self.weigh(cuts)
        assert not mass[0] and not mass[-1], (mass, cuts, self)
        return self._interpolator_(cuts, mass[1:-1])

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
        for l, h, w in self.map(lambda *args: args):
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
        return [func(*a) for a in zip(self.cuts[:-1], self.cuts[1:], self.mass, *more)]

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

        return self._interpolator_(self.cuts, (x * by for x in self.mass))

    def clip(self, lo=None, hi=None):
        """Returns self with its weight outside a range discarded.

        Both arguments, lo and hi, are optional, defaulting to None.  If
        given, they are a lower and upper bound on an interval; if omitted or
        None, they default to self.cuts[0] and self.cuts[-1], respectively.

        If the resulting interval is empty (upper bound isn't greater than
        lower) or self has zero weight in it, a ValueError is raised.  The
        returned Interpolator agrees with self on the interval and has zero
        weight outside it.\n"""

        if lo is None: lo = self.cuts[0]
        if hi is None: hi = self.cuts[-1]

        if lo >= hi or lo >= self.cuts[-1] or hi <= self.cuts[0]:
            raise ValueError('Excessive clipping', (lo, hi), (self.cuts[-1], self.cuts[0]))

        cuts, mass = list(self.cuts), list(self.mass)
        if cuts[0] < lo:
            # First, discard any whole bands:
            i = 1
            while i < len(cuts) and cuts[i] <= lo: i += 1
            i -= 1
            assert i < len(mass), 'We should have ValueError()ed earlier'
            if i: cuts, mass = cuts[i:], mass[i:]

            # Now any partial band:
            assert cuts[0] <= lo < cuts[1]
            if cuts[0] < lo:
                mass[0] *= (cuts[1] - lo) / (cuts[1] - cuts[0])
                cuts[0] = lo

        if cuts[-1] > hi:
            # First, discard any whole bands:
            i = len(cuts) - 2
            while i >= 0 and cuts[i] <= hi: i -= 1
            i += 1
            assert i > 0, 'We should have ValueError()ed earlier'
            if i + 1 < len(cuts): cuts, mass = cuts[:i], mass[:i-1]

            # Now any partial band:
            assert cuts[-1] >= hi > cuts[-2]
            if cuts[-1] > hi:
                mass[-1] *= (hi - cuts[-2]) / (cuts[-1] - cuts[-1])
                cuts[-1] = hi

        if sum(mass, 0) <= 0:
            raise ValueError('No weight in interval', (lo, hi, self))

        return self._interpolator_(cuts, mass)

    # Tools for round():
    def wop(val, base, test):
        """Greatest natural n: test(val, base**n)

        Works only if test is s.t. dividing val by base reduces n by 1.\n"""
        ps = [ base ]
        while test(val, ps[-1]): ps.append(ps[-1]**2)
        bit, ans = 1 << len(ps), 0
        while ps:
            bit >>= 1
            p = ps.pop() # base ** bit
            if test(val, p):
                ans |= bit
                val /= p
                assert not test(val, p)

        assert not test(val, base)
        return ans, val

    @staticmethod
    def __debase(val, base, ask=wop):
        """greatest natural n: base**n divides val"""
        return ask(val, base, lambda v, b: not v % b)[0]

    @staticmethod
    def __log(val, base, ask=wop, low=lambda v, b: b < v):
        """least int n: base**n > abs(val)"""
        # We only care about abs(val); and want it as a non-integral type:
        if   val < 0: val *= -1.
        elif val > 0: val *= 1.
        else: return None # surrogate for minus infinity

        # If < 1, compute -log(1/val) to simplify computation, then adjust:
        if val < 1:
            inv, val = ask(1 / val, base, low)
            if val == 1: return 1 - inv
            return - inv

        return 1 + ask(val, base, low)[0] # round up
    del wop

    @staticmethod
    def __whole(val): # round to nearest, preferring even on a tie
        ans = int((.5 + val) // 1)
        if ans % 2 and ans == .5 + val: return ans - 1
        return ans

    def round(self, best=None, base=10, spike=3e7):
        """Representation by rounding-to-nearest.

        Arguments are optional:
          best -- representation shall be a valid rounding-to-nearest of this
                  value; default, None, means to use the median of self's
                  distribution.
          base -- the number base to use; defaults to ten; must be an integer
                  and at least two.
          spike -- scale factor for use when best hits a big spike; defaults
                   to 3e7 (see below).

        Finds that exponent e for which the integer nearest best / base**e
        has, as its digits, the siginficant digits of best, in the given
        base.  Returns the twople (best, e), using the value actually used for
        best (so None shall have been replaced by self's median) expressed as
        a non-integral type (i.e. dividing it by a whole number, such as
        base**e, won't round).

        For each natural i, let n(i) be the real interval of width 1 centred
        on the integer closest to best / base**i; then b(i) = {r * base**i: r
        in n(i)} is the interval about best represented by using n(i)'s
        digits, with a suitable fractional part and exponent, to represent
        best.  The chosen e is the greatest integer for which less than half
        of self's weight falls in b(e), when there is some such e.

        The only case where there is no such e is when at least half of self's
        weight is in a spike *at* best.  Handling of this case is 'somewhat'
        heuristic, tuned by spike.  If best is zero, we use e = None so that
        the caller handles it as a special case.  Otherwise, we try the least
        integer, e, for which best / base**e is smaller than spike *
        base**len(self).  If the nearest integer to this is a multiple of
        base, we increase e until that's no longer true (i.e. we discard
        trailing zeros from our 'significant' digits; they may actually be
        legitimate significant digits, but they're boring).

            The choice of 3e7 as default for spike arises because the metre is
            (now) so defined that the speed of light (in vacuum) is an exact
            integer in m/s; this integer is slightly less than 3e8 and the
            len() of a simple spike at it is 1, so the above rule selects e =
            0 in base ten and allows for precise representation of the speed
            of light.  I may increase this default if we ever redefine the kg
            to make either Newton's or Planck's constant take some exact
            value.\n"""

        if base < 2 or long(base) != base:
            raise ValueError("Unworkable number base", base)

        log = self.__log
        if best is None: best = self.split((1, 1))[0] # median
        best *= 1. # make sure it's not of an integral type
        if self.weigh((best, best), 2)[1] >= 1:
            # Spke at best carries at least half the weight.
            ent = log(best / spike, base)
            if ent is None: return best, None
            ent -= len(self.mass)
            scale = base**ent
            near = int(best / scale)
            if scale * near == best: # trim trailing zeros from near's repr:
                ent += self.__debase(near, base)
            return best, ent

        lo, hi = self.cuts[0], self.cuts[-1]
        if lo > best: lo = best
        if hi < best: hi = best

        ent = log(hi - lo, base) # expon-ent
        assert ent is not None, "if lo, hi and best all coincide, all weight is there !"
        scale = base ** ent # initially > hi - lo; we'll be narrowing this
        while True:
            near = self.__whole(best / scale)
            # We want the value we display to have less than half
            # weight within the range it denotes; but we also want
            # that range to be narrow enough that the problem of being
            # within its width of the estimated value is also less
            # than a half.  When best is close to a rounidng boundary
            # at the given scale, e.g. 49.96 at scale = 100, all the
            # weight may be within scale/2 of it, even though less
            # than half is within scale/2 of what it rounds to at this
            # scale, leading to perverse results (49.96 displayed as 00
            # because p(<50) < 1/2, even though p(<99.96) was > .9).
            if (self.weigh((best -.5 * scale, best +.5 * scale), 2)[1] < 1 and
                self.weigh((scale * (near-.5), (near+.5) * scale), 2)[1] < 1):
                return best, ent
            ent -= 1
            if scale == 1: scale = 1. / base # force float
            else: scale /= base

    @property
    def dispersal(self, log=math.log):
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
        return cls.__munge(tuple, *tuple(x.filter() for x in whom)
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
        raise NotImplementedError(cls.__name__)

    @property
    def normal(self):
        """Mean and variance tuple as property.

        Returns the mean and variance of the distribution, as a twople.  These
        are the data one would supply to gaussian() to obtain a distribution
        similar to self; .gaussian(m,v).normal should return at least a
        reasonable approximation to (m,v).\n"""
        raise NotImplementedError(self.__class__.__name__)

    @property
    def entropoid(self):
        """Raw entropy measure for the distribution, as property.

        Returns the integral of ln(p)*p, where p is the density function of
        the distribution and ln is the natural logarithm (to base e); this is
        a standard measure of entropy or (when divided by ln(2)) of
        information content.  Note, however, that the result depends on the
        unit of measurement, for the quantity whose distribution self
        describes, used in obtaining the entries in cuts.  See .dispersal for
        a scale-invariant alternative.\n"""
        raise NotImplementedError(self.__class__.__name__)

    def split(self, weights):
        """Cuts the distribution into pieces in the proportions requested.

        Required argument, weights, is a list of non-negative values, having
        positive sum.  A scaling is applied to all entries in the list to make
        its sum equal to self.total().

        Returns a list, result, one entry shorter than weights, for which
        self.weigh(result, sum(weights)) == weights (give or take rounding
        errors).\n"""
        raise NotImplementedError(self.__class__.__name__)

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
        sequence, with each being the integral over a range,
        comprising those t that satisfy the condition given:
            result[0] -- t <= seq[0]
            result[1+i] -- seq[i] <= t <= seq[1+i]
            result[-1] -- t >= seq[-1]

        If one of the seq[i] is in self.spikes, the weight of the spike is
        shared evenly between the intervals on either side, unless the spike
        value is repeated in seq, in which case all of the weight of the spike
        goes into the implied zero-width result interval.  (If several entries
        in seq are equal to the spike, the spike's weight is shared (in an
        unspecified manner) between the implied coincident zero-width result
        intervals.)  If two adjacent entries in seq are equal but not at a spike
        of self, self has zero weight between them.  If a later entry in seq is
        less than an earlier entry, the behaviour is unspecified; don't do
        that.\n"""
        raise NotImplementedError(self.__class__.__name__)

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
        raise NotImplementedError(self.__class__.__name__)

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
        raise NotImplementedError(self.__class__.__name__)

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
        raise NotImplementedError(self.__class__.__name__)

del Cached, postcompose

class PiecewiseConstant (Interpolator):
    """Interpolator based on a piecewise constant function.

    Implements Interpolator by treating each interval between cuts as a
    uniform distribution with the indicated total weight, save that each spike
    is (as usual) treated as a simple weight at the spike value.  Crude but
    straightforward.  For method documentation, check matching methods of
    Interpolator.\n"""

    def __bands(self):
        for l, h, m in self: yield l, h, m, h - l, False
        yield h, h, 0, self.span, True

    def __split(self, weights):
        load, need = self.__bands(), weights[0]
        left, right, avail, wide, done = load.next()
        i = j = 1 # indexing into .mass and weights, respectively
        while True:
            # We've eaten i-1 bands of self and part of band i, which stretches
            # from left to right, leaving avail spread across wide as the rest
            # of this band.  That eating enabled us to yield j-1 cut-points;
            # need remains of weights[j-1], before we can yield the next;
            # eating reduces need via scaling to make sum(weights) match
            # sum(.mass); but avoid pre-scaling either .mass or weights, as
            # rounding errors muck up the scaled values; compute a revised
            # scaling each time round the loop.
            advance = False
            if done: # We've run out of .mass; hopefully also of need !
                assert sum(weights) > need * 1e6
                assert right is self.cuts[-1]
                yield right
                advance = True
            else:
                want = sum(weights[j:], need)
                have = sum(self.mass[i:], avail)
                # Modulo rounding: have may be zero, but only when want is.
                if want and have:
                    scale = want * 1. / have
                    bite = need / scale
                else:
                    assert have * 1e6 < self.total and want * 1e6 < sum(weights)
                    bite = scale = 0

                if bite >= avail:
                    # Eat the rest of avail
                    need -= avail * scale
                    left, right, avail, wide, done = load.next()
                    i += 1
                else: # we can complete this interval
                    step = wide * bite / avail
                    avail -= bite
                    # Rest of avail is spread across remaining wide:
                    wide -= step
                    yield right - wide
                    advance = True

            if advance:
                try: need = weights[j]
                except IndexError: break
                j += 1

        assert abs(sum(self.mass[i:], avail) / self.total - need / sum(weights)) < 1e-6, (
            done, need, weights, left, wide, right, avail, i, self)

    def split(self, weights):
        assert all(x >= 0 for x in weights), \
            ('Weights should not be negative', weights)

        return tuple(self.__split(weights))

    @staticmethod
    def __share(weight, result, seq, s):
        """Handle spike at interval boundary for weigh().

        Arguments:
          weight -- weight of the spike
          result -- list among whose entries to share this weight
          seq -- split-points between intervals weighed by result
          s -- position of spike in seq; seq[s] is at the spike

        Returns the index, t, of the first entry in result to which any weight
        from intervals after spike should be added.  This is also the least t
        > s for which seq[t] > seq[s], or len(seq) if there is no such t.

        See Interpolator.weigh() for the requirements.  If s is the only index
        in seq that's at the spike, half of weight goes into each of result[s]
        and result[s+1].  Otherwise, seq[s:t] has more than one entry, each of
        which is <= seq[s]; these delimit t-s degenerate intervals (usually just
        one) among which weight is shared evenly.\n"""
        t = s + 1
        if t >= len(seq) or seq[t] > seq[s]: # The usual case.
            w = weight * .5
            result[s] += w
            result[t] += w
        else: # spike at seq[s]
            while t + 1 < len(seq) and seq[t + 1] <= seq[s]: t += 1
            while s + 1 < t:
                w = weight * 1. / (t - s)
                s += 1
                result[s] += w
                weight -= w
            result[t] += weight
        return t

    def weigh(self, seq, total=None):
        result, load, cut = [ 0. ] * (1 + len(seq)), self.mass, self.cuts
        if not self.total: return tuple(result) # trivial short-cut

        i = s = 0 # we're adding a share of load[i] to result[s]
        last = None # last seq point if in present cut-gap, else None

        try: # step over any entries in seq that precede all cuts
            while seq[s] < cut[0]: s += 1
        except IndexError: pass # All entries in seq < cut[0]

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

                elif stop < cut[1+i]: # result[s] ends part-way through load[i]
                    if last is None: last = cut[i]
                    if stop > last:
                        result[s] += load[i] * (stop - last) / (cut[1+i] - cut[i])
                    last, s = stop, 1 + s

                elif cut[i] == stop == cut[i+1]: # results[s] ends *at* load[i] spike
                    assert last is None
                    if s < 1 and seq[1] == stop: s += 1
                    else:
                        weight, i = load[i], i + 1
                        # Any more spikes also at stop:
                        while i < len(load) and cut[i+1] == stop:
                            weight += load[i]
                            i += 1

                        s = self.__share(weight, result, seq, s)

                else: # result[s] gets the rest of load[i]:
                    if last is not None:
                        assert cut[i] <= last < cut[1+i]
                        result[s] += load[i] * (cut[1+i] - last) / (cut[1+i] - cut[i])
                        last, i = None, 1 + i

                    # Will IndexError to terminate i-loop.
                    while cut[i] < stop >= cut[1+i]:
                        result[s] += load[i]
                        i += 1

        except IndexError:
            assert i == len(load), \
                'algorithm exited loop surprisingly at %d/%d, %d/%d' \
                % (i, len(load), s, len(seq))

        if total is not None:
            assert 0 != sum(result) # == self.total, known non-zero
            total *= 1. / sum(result)
            result = (x * total for x in result)
            # assert sum(result) == total, give or take rounding errors.

        return tuple(result)

    @lazyprop
    def normal(self):
        cut, siz = iter(self.cuts), iter(self.mass)
        zero = one = two = 0.
        # [zero,one,two][i] == integral(lambda x: x**i * p(x))

        last = cut.next()
        for c in cut:
            w = siz.next()
            # integral(: last < x < c; x**n *w/(c-last) &larr;x :)
            # is just w*(c**(1+n) -last**(1+n))/(1+n)/(c-last)
            # which is w*average(lambda i: last**i * c**(n-i), range(1+n))
            zero += w
            one += w * (c + last) / 2
            two += w * (c * c + last * c + last * last) / 3
            last = c
            # TODO: can we adapt Welford's algorithm ?

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
    def entropoid(self,
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
        assert 1e-6 * self.total > max(me[0], me[-1]), (self, other, cuts)
        assert 1e-6 * other.total > max(yo[0], yo[-1]), (self, other, cuts)
        me, yo = me[1:-1], yo[1:-1]
        assert len(cuts) - 1 == len(me) == len(yo) > 0

        # Addition of densities is nice and simple :-)
        cuts, mass = self.__clean(cuts, [x + y for x, y in zip(me, yo)])
        return self._interpolator_(cuts, mass)

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
        dens = (cls.density(a, b, m) for a, b, m in zip(cuts[:-1], cuts[1:], mass))
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
        assert max(abs(v - 1) for v in (sum(me), sum(yo))) < 1e-5
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
        mass = [self.__mul(*v) * scale for v in
                zip(cuts[:-1], cuts[1:], me, yo)]
        assert len(mass) + 1 == len(cuts)

        cuts, mass = self.__clean(cuts, mass)
        return self._interpolator_(cuts, mass)

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
        plane(n) is an iterator; for each m from 1 through n-1, it yields an
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
        suitable for passing to it, as f(*xs); third is another tuple, ys, of
        partners for the xs, for use if that divides by zero.

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
        divide-by-zero error we originally got from f(*xs).\n"""

        assert len(xs) == len(ys)
        first = True # defer adjusting ys; we probably don't need to

        # Scan over index subsets, of increasing size, on which to use ys:
        for seq in traverse(len(xs)):
            row = []
            for ks in seq:
                assert set(ks).issubset([0, 1])
                try: row.append(f(*ks.mapwith(lambda k, *vs: vs[k], xs, ys)))
                except ZeroDivisionError, err: pass

            # Use average over first plane that gives any results:
            if len(row) > 1: return sum(row) * 1. / len(row)
            try: return row[0]
            except IndexError: pass

            if first:
                assert ks == (0,) * len(xs) # so we didn't use ys yet
                what = err # seq only gave us one ks; save its error
                ys = [(3 * x + y) * .25 for x, y in zip(xs, ys)]
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
                 s=single, more=morebad, each=slices):
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
        for seq in each(len(box)):
            for ks in seq:
                xs = tuple(p[i] for p, i in zip(box, ks))
                ys = tuple(p[1-i] for p, i in zip(box, ks))
                try: row.append(s(f, xs, ys))
                except ZeroDivisionError, what:
                    bad += more(bad, what.args)

        if len(row) < 2:
            bad = (bad or ('Division by zero',)) + ('calling', f, 'on', box)
            raise ZeroDivisionError(*bad)
        return row
    del slices, single, morebad

    # tool-classes for __join
    class Tile (object):
        def __init__(self, xs, wt):
            assert wt, 'non-empty'
            assert len(xs) == len(self), 'right number of entries'
            assert all(y >= x for x, y in zip(xs[:-1], xs[1:])), 'sorted'
            assert len(xs) < 2 or xs[0] < xs[-1], 'spike or interval'
            self.kinks, self.weight = xs, wt

        def __nonzero__(self): return self.kinks[-1] > self.kinks[0]
        def __cmp__(self, other):
            return cmp(self.start, other.start) or cmp(self.stop, other.stop)

        @lazyprop
        def start(self): return self.kinks[0]
        @lazyprop
        def stop(self): return self.kinks[-1]

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
            raise NotImplementedError(self.__class__.__name__)

        def meets(self, lo, hi):
            """Returns true if self has any weight between lo and hi.
            """
            raise NotImplementedError(self.__class__.__name__)

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
        """Compute f's value in a region indicated by box.

        First argument, f, is a function taking len(box) inputs; next
        argument, box, is a tuple of (lo, hi) twoples, giving a range of
        values for each argument to be passed to f; final argument, mass, is
        the product of weights associated with the inputs in those intervals.
        Returns a Tile object which describes how this weight is distributed
        over the range of values obtained by such calls to f.

        Because intervals may be bounded by values at which f is ill-behaved,
        ZeroDivisionError is handled specially; see single's doc.\n"""

        row = each(f, box)

        if len(row) < 2:
            raise ZeroDivisionError('calling', f, 'on', (a, b), (x, y))
        full = set(row)
        if len(full) == 1: cls, row = form[0], tuple(full)
        else:
            assert len(full) > 1, 'row is non-empty => so is full'
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
            raise ValueError('No data to combine', self, *others)
        try: hi = cut.next()
        except StopIteration:
            return self._interpolator_((lo, lo), 1)

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

        return self._interpolator_(kink, wait)
    # </tools>

del lazyprop, math, iterable, Tuple
