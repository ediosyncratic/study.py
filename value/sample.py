"""Representation of distributions.

This is about giving a numeric semantics to dictionaries whose keys are the
putative values of some quantity: to each, associate an interval (whose edges
aren't specified but lie between the given key and its neighbours); the value
for a key is the integral of the distribution across the interval associated
with that key.

In caricature at least, the value of a key is the probability that a random
selection from the distribution would be closer to the given key than to any of
the other keys in the same dictionary: and the keys are roughly the odd 2n-iles,
where n is the number of keys, so have roughly equal weights.

When combining two such `numeric' values, we want to combine each key of one
with each key of the other, using the result as a key in the composite
dictionary.  This key gets a contribution to its value which is the product of
the values of the two keys combined to produce it: if it is the result of
several such combinations, its value will be a sum of such products.  Then take
the resulting big bok of weighted values, compute its odd 2n-iles for some
chosen n, use these as the keys of a dictionary in which weight[key] is the sum
of the big bok's values for keys which are closer to this key of weight than to
any other.

Various classes with Weighted in their names provide the underlying
implementation for that; the class Sample packages this functionality up for
external consumption.
"""

_rcs_id_ = """
$Id: sample.py,v 1.27 2004-12-30 14:42:55 eddy Exp $
"""

class _baseWeighted:
    """Base class for weight dictionaries.

    A `weight dictionary' has `data points' as its keys and weights as the
    associated values.  It is to be read as a `distribution': typically, as a
    probability distribution (when the sum of the weights is one).  The presence
    of a given { key: value } within the dictionary indicates that there is some
    neighbourhood of the data point, key, in which there is a probability value
    of finding your random thing.  I aim to coerce things into a form where, to
    the best of my knowledge, the neighbourhood of each key lies between the
    neighbouring keys above and below.

    The various kinds of functionality layered together to make this are split
    from one another in this file to allow the possibility of re-use of some of
    the toolset by alloying with some other implementation of other parts of the
    toolset.  Each part's implementation assumes the exported functionality of
    the other parts, so only alloys analogous to Weighted are viable: instances
    of the component classes won't work.

    Sub-classes:

      statWeighted -- provides for statistical computations;
      joinWeighted -- provides for combining distributions (and transforming them);
      repWeighted -- integrating and rounding;
      _Weighted -- Object packaging a weights dictionary.

    These are all alloyed together to build the final class, Weighted, which is
    what Sample actually uses. """

    # these aren't ideal, but it's sometimes nice to know highest and lowest keys.
    def high(self): return max(self.keys())
    def low(self): return min(self.keys())

# First layer of functionality: interpret self as describing a curve.
import math # del at end of this page
from basEddy import Lazy # likewise

class curveWeighted (Lazy, _baseWeighted):
    """Interpretation of the weights dictionary as a curve.

    This introduces an implicit curve described by the weight dictionary.  If
    there is only one key, we have no idea how broadly it should spread: an
    exact `delta function' is assumed.  Otherwise, the gaps between adjacent
    keys are used for interpolation: and the boundary keys are extrapolated
    outwards to an extent comparable with their extent inwards.

    For this version, the curve is piecewise constant - i.e. it's a sum of
    uniform distributions.  Each weight in self's dictionary is spread evenly
    across the interval between the mid-points between the weight's position and
    that of its nearest neighbour on either side.  The outer extreme of the
    first and last weights' intervals are as far beyond the weight as it is
    beyond its neighbour (so the end interval's outer tail is twice as long as
    its inward tail; evenly spaced weights in the ratio 3:2:2:...:2:2:3 will
    yield a true uniform distribution).

    More sophisticated replacements for curveWeighted may be worth using in its
    place when alloying a modified Weighted for use by Sample.  The only thing
    you need to over-ride is (well, should be) interpolator; curveWeighted's
    cliends *should* assume nothing about the curve (but repWeighted's rounding
    infrastructure may still be entanged).  Your interpolator must support
    .weigh(), .split(), cross() with the same semantics as here and have a .cuts
    attribute with a suitable meaning. """

    tophat = {-1./6: 1, 1./6: 1} # produces uniform distribution from -.5 to +.5

    def reach(self, low=None, high=None, share=1e-6):
        """Ensure self's weights stretch as far as low and high, if non-None.

        Arguments:
          low -- None, or a lower bound that self should reach
          high -- None, or an upper bound that self should reach
          share -- fraction of self's total weight available to reach bounds

        Note that if low or high is None, or is already within self's range, it
        will be ignored.\n"""

        if len(self) < 1:
            # ignore share; low and high shall be our only weights
            if high is None:
                if low is not None: self.add({low: 1})
            elif low is None: self.add({high: 1})
            else: self.add({(low * 2 + high) / 3.: 1, (low + 2 * high) / 3.: 1})

        else:
            cut, sum = self.interpolator.cuts, self.interpolator.total * share
            bok = {}
            if low is not None and low < cut[0]:
                # arrange for low to be new cut[0]
                bok[.5 * (low + self.sortedkeys[0])] = sum

            if high is not None and high > cut[-1]:
                bok[.5 * (self.sortedkeys[-1] + high)] = sum

            if len(bok) > 1:
                for k in bok.keys(): bok[k] = bok[k] / 2

            if bok: self.add(bok)

        # that invalidates some attributes:
        try: del self.interpolator
        except AttributeError: pass

        try: del self.sortedkeys
        except AttributeError: pass

    # Support tool: interpolator to interpret the dictionary as a curve.
    class _lazy_get_interpolator_ (Lazy):
        """Integration of a curve interpolated from a weights-dictionary.

        What should be happening here ?
        repWeighted (or its replacement, e.g. a Bezier interpolator) provides:
          between([low, high]) -- defaults, None, mean relevant infinity; yeilds weight
          weights(row) -- map(self.between, [ None ] + row, row + [ None ])
          carve(weights) -- yields tuple for which map(self.between, (), yield) = weights.
          round([estim]) -- yields string describing estim to self's accuracy.

        The meaning of a distribution is:
        we have { position: weight, ... }
        and ks = sortedkeys lists the positions in increasing order.
        What an entry { ks[i]: w } in the mapping means is that the
        total weight between (ks[i-1]+ks[i])/2 and (ks[i]+ks[i+1])/2 is w.
        It doesn't say anything about mean or median.

        Doing it piecewise linearly looks a pig.  So do it piecewise constant,
        with steps at the mid-points between adjacent weights. """

        def __init__(self, weigher, ignored):
            self.cuts = self.__cuts(weigher.sortedkeys)
            self.size = tuple(map(lambda k, w=weigher: w[k], weigher.sortedkeys))
            # the curve described has integral size[i] between cut[i] and cut[1+i].

        def __cuts(self, row, mean=lambda a, b: (a+b)/2.):
            # computes cut points and ensures they're floats.
            if len(row) < 2:
                if not row: return ()
                x = 1. * row[0]
                return (x, x)

            return ( 2. * row[0] - row[1], ) + \
                   tuple(map(mean, row[:-1], row[1:])) + \
                   ( 2. * row[-1] - row[-2], )

        def _lazy_get_total_(self, ignored, add=lambda a, b: a+b):
            return reduce(add, self.size, 0)

        def split(self, weights, add=lambda a, b: a+b):
            """Cuts the distribution into pieces in the proportions requested.

            Required argument, weights, is a list of non-negative values, having
            positive sum.  A scaling is applied to all entries in the list to
            make its sum equal to self.total().

            Returns a list, result, one entry shorter than weights, for which
            self.weigh(result) equals the re-scaled weights-list. """

            assert not filter(lambda x: x < 0, weights), 'weights cannot be negative'
            scale = self.total / reduce(add, weights, 0.)

            cut, load = self.cuts, self.size
            ans, prior, i = [], cut[0], 0
            for w in map(lambda x, s=scale: x*s, weights[:-1]):

                avail = load[i]
                # Maybe starting part way through a band:
                if prior > cut[i]: avail = avail * (cut[1+i] - prior) / (cut[1+i] - cut[i])

                # Swallow any bands we can:
                while w >= avail:
                    w, i, prior = w - avail, 1+i, cut[1+i]
                    avail = load[i]

                # grab what we need from present band:
                if w > 0: prior = prior + w * (cut[i+1] - cut[i]) / load[i]
                ans.append(prior)

            return tuple(ans)

        def weigh(self, seq):
            """Integrates self's distribution between positions in a sequence.

            Single argument, seq, is a sequence of positions in the
            distribution.  The sequence is presumed to be sorted.

            Returns a tuple of weights, result, one entry longer than the
            sequence, with each being the integral over the distribution between
            two bounds:
              result[0] -- from minus infinity to seq[0]
              result[1+i] -- from seq[i] to seq[1+i]
              result[-1] -- from seq[-1] to infinity
            """

            result, load, cut = [ 0. ] * (1 + len(seq)), self.size, self.cuts
            if not self.size: return tuple(result)
            if len(self.size) < 2:
                # special case: only one weight, delta function.
                weight = self.size[0]
                i = len(filter(lambda x, r=self.cuts[0]: x < r, seq))
                assert i == len(seq) or seq[i] >= self.cuts[0], ('mis-sorted positions', seq)
                if i < len(seq) and self.cuts[0] == seq[i]: # even split
                    result[i] = result[i+1] = weight / 2
                else:
                    result[i] = weight

            else:
                # sensible case where we have at least two weights.
                i = s = 0 # we're processing size[i] for result[s]
                last = None # last seq point if in present cut-gap, else None

                try: # step over any entries in seq that precede all cuts
                    while seq[s] < cut[0]: s = 1 + s
                except IndexError: pass

                try: # loop until we run off end of row ... can happen from inner loop.
                    while 1:
                        try: stop = seq[s]
                        except IndexError: stop = cut[-1] # gather everything after seq[-1]

                        if stop < cut[i]:
                            # out-of-order entries in seq - unless i got incremented in error.
                            assert seq[s-1] >= stop, 'must have incremented i in error'
                            s = 1 + s # and leave last alone ...

                        elif stop < cut[1+i]:
                            if last is None: last = cut[i]
                            if stop > last:
                                result[s] = result[s] + load[i] * (stop - last) / (cut[1+i] - cut[i])
                            last, s = stop, 1 + s

                        else:
                            if last is not None:
                                result[s] = result[s] + load[i] * (cut[1+i] - last) / (cut[1+i] - cut[i])
                                last, i = None, 1 + i

                            while stop >= cut[1+i]:
                                result[s], i = result[s] + load[i], 1 + i

                except IndexError:
                    assert i == len(load), \
                           'algorithm exited loop surprisingly at %d/%d, %d/%d' \
                           % (i, len(load), s, len(seq))

            return tuple(result)

        def cross(self, other):
            # see joinWeighted.cross() - experimental, 2003/April/20th.
            cuts = {}
            for k in self.cuts: cuts[k] = None
            for k in other.cuts: cuts[k] = None
            cuts = cuts.keys()
            cuts.sort()

            me, you = self.weigh(cuts), other.weigh(cuts)
            assert me[0] == 0 == me[-1] and you[0] == 0 == you[-1]
            me, you = me[1:-1], you[1:-1]

            # <bodge> hmm ... but data are likely 95% confidence intervals, so
            # let's spread an extra 5% around in everyone ...
            w = cuts[-1] - cuts[0]
            if w:
                i, m, y = len(me), self.total * .05 / w, other.total * .05 / w
                p, me, you = cuts[i], list(me), list(you)
                while i > 0:
                    i = i - 1
                    n = cuts[i]
                    h, p = p - n, n
                    me[i], you[i] = me[i] + m * h, you[i] + y * h
            # </bodge> Otherwise, when two sources of data contradict one
            # another (e.g. K&L's value for fine structure and one computed from
            # a definitive formula and values from assorted sources), bits comes
            # out empty throughout.

            bits = map(lambda x, y: x * y, me, you)
            assert len(cuts) - 1 == len(bits) > 0
            bok = {}
            if len(bits) > 1:
                bok[(cuts[0] + 2*cuts[1])/3.] = bits[0]
                bok[(2*cuts[-2] + cuts[-1])/3] = bits[-1]
                bits = bits[1:-1]
                for k in map(lambda a, b: .5*(a+b), cuts[1:-2], cuts[2:-1]):
                    bok[k], bits = bits[0], bits[1:]
                assert len(bits) == 0
            else:
                assert len(cuts) == 2 # and I *think* its two entries are equal ...
                bok[(cuts[0] + cuts[1])*.5] = 1. # any value will do ...

            return bok

        # The rest of interpolator (and curveWeighted) is toys; not needed in replacements.
        # dispersal, a.k.a. entropy or information content, computations:

        def _lazy_get_entropoid_(self, ignored, log=math.log, add=lambda a, b: a+b,
                                 each=lambda l, h, w, g=math.log: w * g(w / (h-l))):
            """entropoid = integral(: p.log(p) :)

            Since p is piecewise constant, the integral is a sum of simple
            terms: each term is an integral between two entries in self.cuts, h
            apart, in which lies the matching weight, w, in self.size; this
            makes p = w/h over the interval, so e gets a contribution
            h.(log(w/h).w/h) = w.log(w/h).  This makes the integration easy.

            It is not immediately clear what to do with a delta function ...
            However, this datum is used via dispersal, which can handle that.

            Note, as documented for dispersal, that entropoid depends on the
            choice of unit of measurement for the quantity whose distribution
            self describes. """

            cut, siz = self.cuts, self.size
            if len(siz) < 2: return 0 # all zero, or a single spike

            return reduce(add, map(each, cut[:-1], cut[1:], siz))

        def _lazy_get_dispersal_(self, ignored, log=math.log):
            """Computes the dispersal (an analogue of entropy) of the distribution.

            The caricature of what we return is integral(: -p*log(p) :).

            However, the distribution, p, described by self is really a density
            (: p :{u*x: scalar x}) for some unit u (as used for measurement of
            the quantity whose distribution self describes); and integral(p) is
            dimensionless.  The dimensions of integral(p) are those of p's
            outputs times those of the integrating variable, i.e. u; so p's
            outputs must be of the same kind as 1/u.

            Thus log(p) isn't strictly meaningful; however, u*p is dimensionless
            and we can take its log, giving us integral(p*log(u*p)).  This is
            then meaningful, but depends on our unit, u.  With the choice of u
            made by our client, we compute this integral as self.entropoid.
            Using a different unit, w, in place of u will add
            log(w/u)*self.total to self.entropoid:
              integral(p*log(w*p)) = self.entropoid + log(w/u) * self.total

            Furthermore, self's distribution is meant to be understood as being
            independent of self.total, i.e. integral(p).  In general, scaling p
            down by a factor k also changes integral(: p*log(u*p) :), to
                integral(: log(u*p/k)*p/k :)
                 = (self.entropoid - self.total * log(k)) / k

            So we have to decide what unit to use and what overall scaling to
            apply.  For the overall scaling, a natural choice is k = self.total,
            so as to normalise p to yield self.total = 1.  If we replace our
            unit, u, used implicitly in computing self.entropoid, with some more
            apt unit w, this will give us, as integral(-log(w*p/k)*p/k),
                r = log(self.total*u/w) - self.entropoid/self.total

            The issue of chosing a sensible unit is, as ever, non-trivial.  I
            intuit that the dispersal should be translation-invariant;
            i.e. replacing p with (: p(x-z) &larr;x :) shouldn't change its
            dispersal, for constant z, e.g. an average of the distribution.
            Thus a sensible unit, w, must needs be obtained from the width of
            the distribution, in one guise or another.  The combination of scale
            invariance and translation invariance implies that the resulting
            dispersal will describe the *shape* of the distribution, rather than
            anything else.  See also the docs, below, of _lazy_get__unit_ and
            repWeighted.dispersor. """

            if len(self.size) < 2: return 0 # delta function - no dispersal ?

            a = self.total
            return log(a / self._unit) - self.entropoid / a

        def _lazy_get__unit_(self, ignored):
            """A `width of the distribution' unit for use in normalising dispersal.

            This is still exploratory.

            A suitable unit must be independent of applying an overall scaling
            to self's weights or overall translation of self's cuts; a uniform
            scaling of self's cuts should cale the unit proportionately.  Thus
            the unit must be a `width' of the distribution, such as the total
            span or standard deviation.

            Various units present themselves as candidates.  For each, I've
            examined the theoretical value for uniform and for Gaussians; I've
            also examined the limiting behaviour of binomial distributions.
            I've tried the following:

              standard deviation -- well, sqrt(variance) anyhow.  Gives positive
              answers (which is good); uniform is log(12)/2 = 1.24 and a bit;
              Gaussian is sqrt(pi/2) = 1.25 and a bit; binomials tend to about
              1.4189 from below; Planck.mass gets 0.414ish.

              total width -- i.e. cut[-1] - cut[0].  Gives negative answers
              (bad); uniform is 0, Gaussian has no width, binomials tend to
              about -pi from above; Planck.mass gets -0.592ish.

              90% confidence interval -- i.e. difference between entries in
              self.split([.5,9,.5]).  Uniforms get -log(.9) = 0.105 and a bit;
              Gaussian has width 3.29 so gets log(sqrt(2*pi)/3.29) +.5 = 0.228
              and a bit; binomials approximately stabilise on approximately this
              last value; Planck.mass gets -0.584ish.

              50% confidence interval -- i.e. similar for self.split([1,2,1]).
              Uniforms get -log(.5) = log(2) = 0.69 and a bit; Gaussian has
              width 1.348 so gets log(sqrt(2*pi)/1.348) +.5 = 1.12 and a bit;
              binomials stabilise on slightly less than 1.12, oscillating among
              1.118 and 1.119 mostly; Planck.mass gets -0.2765ish.

            Generally:

              uniform is, wlog, .5 between -1 and 1; integral(p) = 1,
              integral(p*log(p)) = .5 *log(.5) * 2 = log(.5), dispersal is thus
              log(u/w) -log(.5) = log(2*u/w); its exp is simply the actual total
              width of the distribution divided by the unit we select.

              Gaussian is, wlog, p = (: exp(-x*x/2) &larr;x :) with
              total = integral(p) = sqrt(2*pi),
              entropoid = integral(p*log(p)) = -total * variance / 2 = -total/2, so
              dispersal = log(total*u/w) -entropoid/total = log(sqrt(2*pi)*u/w) +0.5.

            Since I like +ve dispersals, I've settled on standard deviation ...
            """

            return self._deviation

        def _lazy_get__deviation_(self, ignored, sqrt=math.sqrt):
            """standard deviation"""

            cut, siz = self.cuts, self.size
            zero = one = two = 0.
            # [zero,one,two][i] == integral(: x**i * p(x) &larr;x :)

            last = cut[0]
            for c in cut[1:]:
                w, siz = siz[0], siz[1:]
                # integral(: last < x < c; x**n *w/(c-last) &larr;x :)
                # is just w*(c**(1+n) -last**(1+n))/(1+n)/(c-last)
                # which is w*(c**n + last * c**(n-1) + ... + last**(n-1) * c +last**n)/(1+n)
                last, zero, one, two = c, zero + w, one + w*(c +last)/2, two + w*(c*c +last*c +last*last)/3

            return sqrt(two/zero - (one/zero)**2)

    # end of inner class _lazy_get_interpolator_
    # Odd little fripperies that fascinate me, connected to entropoid, above:

    def dispersal(self):
        """Integrates log(density) using the density as measure; a.k.a. entropy.

        This also performs scale-invariance normalisations; the result is
        actually integral(-log(w*p/k)*p/k) with k = integral(p) and w a unit
        chosen based on the width of the distribution, p. """

        return self.interpolator.dispersal

    def dispersor(self, exp=math.exp):
        """Divide self by this to get a (dimensionless) value with zero entropoid.

        Continuing from interpolator's docs:

        Had we used w in place of u, self.entropoid would have been:
            integral(p*log(w*p)) = self.entropoid + log(w/u) * self.total
        which tells us that exp(self.entropoid/self.total), which is
        dimensionless, is proportional to u, the unit implicitly used in
        computing self.entropoid; as shown earlier when looking at entropoid's
        dependence on k, it's also proportional to self.total.  Thus
        u*self.total/exp(self.entropoid/self.total) is independent of self.total
        and u with the same dimensions as u, i.e. as the quantity whose
        distribution we're looking at.  Thus this quantity appears below as the
        `dispersor' of a Sample; dividing a Sample by its dispersor will give a
        Sample whose distribution has zero dispersal. """

        if len(self) == 1: return 0 # entropoid should be infinite for delta spike.
        i = self.interpolator
        if i.total == 0: return 0
        return i.total / exp(i.entropoid / i.total)

    def disperse(self, exp=math.exp):
        """Returns variant on self normalised to have zero entropoid."""
        i = self.interpolator

        if 1./i.total == 0:
            # cope with infinity by going via a half-way house:
            self = self.copy(scale = 1./max(self.values()))
            i = self.interpolator
        elif 1./(1./i.total) == 0:
            # cope with infinitessimals by going via two half-way houses:
            s = pow(max(self.values()), -.5)
            self = self.copy(scale=s).copy(scale=s)
            i = self.interpolator

        return self.copy(scale = exp( - i.entropoid / i.total))

del math, Lazy

# Integration over intervals, etc.; including rounding a `best estimate'.
# Makes heavy use of baseWeighted's interpolator.

class repWeighted (curveWeighted):
    """Base-class for rounding (whence representation) and integration.
    """

    def weights(self, seq): return self.interpolator.weigh(seq)

    def between(self, low=None, high=None):
        """Returns the weight associated with an interval.

        Arguments are the low and high bounds of the interval.  Either may be
        None, indicating an interval unbounded at that end. """

        row = []
        if low is not None: row.append(low)
        if high is not None: row.append(high)

        row = self.interpolator.weigh(row)

        if low is None: return row[0]
        if high is None: return row[-1]
        return row[1]

    def bounds(self, frac=1):
        """Bounds self's distribution.

        Optional argument, frac (default: 1), is the proportion of self's total
        weight which is to fall between the bounds; ignored (i.e. treated as 1)
        unless between 0 and 1.  Returns a 2-tuple (lo, hi) for which:

            apply(self.between, self.bounds(f)) == f * self.total()

        Thus, for instance, 95% of a distribution lies between the two entries
        in the .bounds(0.95) of the distribution; 2.5% lies below the first
        entry and 2.5% lies above the second. """

        # arguably:
        # a,b = bounds(u, 1-frac, v) with u+v=frac and a,b as close together as possible.

        if 0 <= frac < 1:
            gap = .5 * (1 - frac)
            return self.interpolator.split([ gap, frac, gap ])
        else:
            cut = self.interpolator.cuts
            return cut[0], cut[-1]

    def niles(self, n, mid=None):
        """Subdivides distribution into n equal bands.

        First argument, n, is the number of bands into which to divide self's
        distribution.  Second argument, mid (default false), selects the
        mid-points of the bands, instead of their ends; this, in fact, still
        delivers the ends of bands but starts (and ends) with half-bands.

        Returns a tuple of sample points (n+1 of them if mid is false, n if it
        is true) for self's distribution: between any two adjacent entries in
        the tuple, self.between() finds weight self.total()/n.  If mid is false
        (default) the first and last entries in the tuple are the top and bottom
        of self's distribution's tails (which may be further apart than self's
        highest and lowest keys).

        Contrast statWeighted.median(), which always returns a sample-point of
        the distribution, and joinWeighted.condense(), which uses sample-points
        for the equivalent of niles with mid = true.  Note that self's median
        can be obtained as the single entry in self.niles(1, true) or as the
        middle entry of self.niles(2). """

        if n < 1: raise ValueError('Can only subdivide range into positive number of parts')
        if mid: return self.interpolator.split([ 1 ] + [ 2 ] * n + [ 1 ])
        else:   return self.interpolator.split([ 0 ] + [ 1 ] * (1+n) + [ 0 ])

    # Is this interpolator-work ?
    def __embrace(self, total, about):
        """Finds a slice of self, returns its weight and width.

        Arguments:
          total -- lower-bound on total weight of the slice.
          about -- slice will span this value
        """

        row = self.sortedkeys
        hi = top = len(row)
        while hi > 0 and row[hi-1] > about: hi = hi - 1
        # assert hi is 0 or row[hi] > about >= row[hi-1]
        lo = hi

        # Expand [lo:hi] until it embraces more weight than total:
        weight = 0      # sum, for key in row[lo:hi], of self[key]
        while weight <= total:
            # Chose an end at which to expand [lo:hi]

            if lo <= 0: # we can only grow upwards
                if hi >= top: break
                sign = +1
            elif hi >= top: sign = -1 # we can only grow downwards
            else:
                # cmp(about, midpoint), but dodge int/2 gotcha
                sign = 2 * about - row[hi] - row[lo-1]

            if sign < 0:        # grow down
                lo = lo - 1
                weight = weight + self[row[lo]]
            else:               # grow up
                weight = weight + self[row[hi]]
                hi = hi + 1

        # assert weight > total or (lo, hi) == (0, top), 'while loop exit'

        # So, how wide is this range ?  Identify nominal end-points:
        if hi < top: right = .5 * (row[hi] + row[hi-1])
        elif top > 1: right = 2 * row[-1] - row[-2]
        else: right = row[hi-1]

        if lo > 0: left = .5 * (row[lo] + row[lo-1])
        elif top > 1: left = 2 * row[0] - row[1]
        else: left = row[lo]

        # Expand to embrace about:
        assert left <= right
        if right < about: right = about
        elif left > about: left = about

        return weight, right - left

    def __unit(self, what):
        """Returns a suitable power of 10 for examining what."""

        if not what: what = 1
        decade = 0

        while what >= 10: what, decade = what / 10., decade + 1
        while what < 1: what, decade = what * 10, decade - 1

        return pow(10. + what * 0, decade)

    # how far can we get with separating this from the interpolation kit ?
    def round(self, estim=None):
        """Returns a rounding-string for estim.

        Argument, estim, is optional: if omitted, the distribution's median (if
        available) or mean (likewise) will be used.

        Result is a string representing this value to some accuracy, in %e-style
        format.  This implicity represents an interval, given by `plus or minus
        a half in the last digit'.  This interval will contain estim.

        Normally, the interval denoted by the result string will contain less
        than half the weight of self's distribution and is the shortest such
        representation.  E.g. if self.between(3.05, 3.15) >= .5 >
        self.between(3.135, 3.145) then self.round(pi) will return '3.14'.

        That's impossible if half (or more) of self's weight sits at estim,
        i.e. self's half-width about estim is zero.  In this case, the result
        string will only give estim to as many significant digits as eight more
        than the number of sample-points of self; and if the next five digits
        would all have been 0, any trailing zeros will be elided from the ones
        given [for various sanity reasons].  Sugar: in this `exact' case, any
        exponent used will employ E rather than e (thus 1.2E1 for 12); and if no
        digits appear after the '.'  in an exact representation, the '.' is
        omitted. """

        # Handle argument default:
        if estim is None:
            try: estim = self.median()
            except (AttributeError, ValueError): estim = self.mean()

        # if self's keys include a non-numeric, e.g. Quantity(1,metre),
        # what should happen ?

        # Deal with dumb case:
        if len(self) < 1: return `estim`
        # Deal with infinity:
        if estim and estim / 10. == estim: return `estim`

        # Compute half-weight:
        threshold = .5 * self.total()
        assert threshold > 0, ('Weights need to be positive for rounding algorithm',
                               threshold, self.values())

        # Find half-width of self:
        weight, width = self.__embrace(threshold, estim)

        # Last pieces of initialisation:
        if estim < 0: head, sign = '-', -1
        else: head, sign = '', +1
        body, tweak, aim = '', 0, 0
        # head: sign
        # body: mantissa, the bit between sign and exponent, with its '.'
        #  omitted; the '.' is later added between body[0] and body[1].
        # tail: exponent, the trailing 'e+07' part, if any
        # tweak: tells us whether we need to do carrying.
        # unit: value of a 1 in the next position of the mantissa
        unit = self.__unit(max(abs(estim), width))

        if width <= 0:
            # Single-point distribution: special treatment.
            # No interval contains less than half the distribution ... so
            # the loop will not terminate without intervention !
            # Impose an upper limit on how many digits we'll produce: consider 1/3.
            stop = 7 + len(self.sortedkeys) # How many keys we have, +7 for good measure.
            # I'll actually produce up to one more than this (why 7 ? because
            # the speed of light is exact in nine digits; when obtained from a
            # single-point distribution, the following will give us exactly all
            # nine digits).

            # Our representation may get so close to exact that, to show the
            # difference, we'd need more than twice as many digits as that
            # allows: in such a case, truncate it - i.e. skip the long chain of
            # 0 that would follow.
            tiny = unit * pow(.1, stop + 5)

        else: tiny = 0 # suppress the special treatment

        # what to do here if unit is a Quantity() ?
        if unit == 1: tail = ''
        else:
            tail = str(unit)
            if tail[:2] == '1.' and 'e' in tail:
                tail = 'e' + tail.split('e')[1]
            else:
                tail = ('%.0e' % unit)[1:] # '1e93'[1:]
        # First, work out the un-rounded body (main loop); then round.

        # Loop until over-long string or as precise as we'll allow:
        while unit > width or weight > threshold:
            # Compute unrounded digit for present:
            dig = int((estim - aim) / unit / sign)
            # Compute unrounded approximation thereby implied:
            aim, body = aim + sign * unit * dig, body + `dig`

            # Determine whether to round:
            tweak = cmp((estim - aim) * sign, .5 * unit)
            # on exact half, round to even (c.f. IEEE):
            if tweak == 0: tweak = { 1: +1, 0: -1 } [dig % 2]

            # If rounding, use next digit up as mid-point: otherwise, use unrounded.
            if tweak < 0: mid = aim
            else: mid = aim + sign * unit

            if tiny:
                # Special treatment for single-point distributions:
                if abs(mid - estim) < tiny:
                    # So close to an exact match we should regard it as a rounding
                    # glitch: our representation is faithful
                    if tail:
                        # Use %E to flag exact match, %e otherwise ...
                        assert tail[0] == 'e', tail
                        tail = 'E' + tail[1:]

                    adddot = len(body) > 1
                    break

                if len(body) > stop:
                    adddot = 1
                    break

            # Compute weight in interval implied by suitably-rounded value:
            weight = self.between(mid - .5 * unit, mid + .5 * unit)

            # Prepare for next digit:
            unit = .1 * unit

        else:
            # didn't break out of the while loop:
            adddot = len(body) > 1 or width > 0 or estim != mid

        # Propagate rounding if necessary:
        try:
            # (if tweak > 0: while tweak: contracted to ...)
            while tweak > 0:
                # Take last character off body, transfer to tail, rounding up.
                last = body[-1]         # may fall off left end here ...
                body = body[:-1]

                try: tail = { '0': '1', '1': '2', '2': '3',
                              '3': '4', '4': '5', '5': '6',
                              '6': '7', '7': '8', '8': '9' }[last] + tail
                except KeyError: tail = '0' + tail # still rounding
                else: tweak = 0 # done !

        except IndexError: head = head + '1'    # ... fell off left end !
        # (added to right of head so '.' stays put within body)

        # Join body to tail and punctuate:
        body = body + tail
        if body and adddot: body = body[:1] + '.' + body[1:]

        # Join head to body and return:
        return head + body

# Combining two (whence several) distributions; adding to a distribution:

class joinWeighted (curveWeighted):
    """Interface-class defining how to stick distributions together.

    Provides apparatus for adding data to a distribution, re-sampling a
    distribution, obtaining a canonical distribution (i.e. one whose weights'
    neighbourhoods don't overlap) and for performing `cartesion product'
    operations (i.e. taking two distributions and obtaining the joint
    distribution for some combination of their parameters), including
    comparison. """

    def __init__(self, detail):
        self.__detail = detail

    def add(self, weights, scale=1, func=None, oneach=lambda x: (x, 1)):
        """Increment some of my keys.

        Arguments:

          weights -- a mapping (e.g. dictionary), for each key of which we'll be
          performing self[key] = self.get(key, 0) + weights[key], save that
          weights[key] may be scaled by scale and key may have been replaced
          with func(key) - see below.  If a sequence is given, it is read as a
          mapping with the sequence as .keys() and all values equal to 1; if
          anything other than a sequence or mapping is given, it is read as a
          single key with value 1.

          [scale=1] -- a scaling to apply to all values in weights

          [func=None] -- a callable which accepts keys of weights and returns
          keys for self.  Used to transform any keys of weights which aren't
          joinWeighted instances.

        If a key to be given to self (either a key of weights or func's output
        from such) is itself a joinWeighted, self.add recurses with
        self.add(key, weights[key] * scale, func).  If a key is a Sample, its
        .mirror is obtained: this is a joinWeighted and the same recursion is
        used.  Otherwise, (func's replacements for) keys should be scalars. """

        try: mites = weights.items()
        except AttributeError:
            try: mites = map(oneach, weights[:])
            except (TypeError, AttributeError): mites = [ (weights, 1) ]

        for key, val in mites:
            val = val * scale
            if val < 0: raise ValueError, ('Negative weight', val, key, scale)
            if isinstance(key, Sample): key = key.mirror

            if isinstance(key, joinWeighted): self.add(key, val, func)
            else:
                if func: key = func(key)
                self[key] = self[key] + val

    def cross(self, other):
        """Pointwise product of two distributions: `intersection'."""
        assert len(self) > 1 < len(other), "delta functions aren't nice here"
        return self._weighted_(self.interpolator.cross(other.interpolator),
                               detail=max(self.__detail, other.__detail))

    def decompose(self, new, mean=lambda a, b: (a+b)/2.):
        """Decomposes self.

        Argument, new, is a sorted sequence of keys to use.  Only keys in new
        which lie between self's bounds will actually be used. """

        if not self.sortedkeys: return self.copy() # trivial/bogus case
        # assert: new is sorted (but may have some repeated entries)

        # Flush out any repeats and out-of-range:
        lo, hi = self.bounds()
        run = []
        for k in new:
            if k not in run and hi > k > lo:
                run.append(k)

        # Get repWeighted to share self's weights out correctly between run's entries:
        result = {}
        if run:
            load = self.interpolator.weigh(map(mean, run[:-1], run[1:]))
            for k in run: result[k], load = load[0], load[1:]
        # I'm not convinced (2002/Feb) interpolator is the right solution here;
        # this method is used when self is a messy hodge-podge, so the weight
        # which straddles the mid-point between entries in new is haphazardly
        # weighted.  I may have been better off lumping each weight to nearest
        # in new or sharing each weight between the nearest entries in new in
        # proportion to how close each is.

        # Build a new distribution with this weighting:
        return self._weighted_(result, detail=len(result))

    def condense(self, count=None, middle=lambda i: i.median()):
        """Simplifies a messy distribution.

        Argument, count, is optional: it specifies the desired level of detail
        in the result (default: None).  None is taken to mean the level of
        detail specified for self when it was created.

        Returns a self._weighted_() whose keys are: the highest and lowest of
        self, and; count-1 points in between, roughly evenly-spaced as to self's
        weight between them.  The weight of each of these points is based on
        carving up self's weights according to who's nearest. """

        if count is None: count = self.__detail
        if len(self) <= count: return self

        # Carve self up into count-2 interior (count-1)-iles and two half-bands at
        # top and bottom.
        if count > 1: count = count - 1
        step, parts, last = self.total() * 1. / count, [ self._weighted_({}) ], None
        gap = .5 * step # total weight remaining in current band
        assert step > 0, 'Condensing degenerate weighting'

        for key in self.sortedkeys:
            # Weight provided by this key:
            val = self[key]

            # If that completes the band, split it suitably between bands:
            while gap < val: # well, OK, one big weight might span several bands
                # Fill up old band with its share of val:
                if gap > 0: parts[-1][key], val = gap, val - gap
                # Tack a new (empty) band onto parts:
                parts.append(self._weighted_({}))
                # Reset gap for this new band:
                gap = step
                # Note: final band wants gap = .5 * step, but won't get filled
                # in any case, so it doesn't care.

            # Add this key to the current band and decrement gap accordingly
            if val > 0:
                parts[-1][key] = val
                gap = gap - val

        # Use medians of resulting parts as sample points for condensed
        # distribution.  Work out how much weight to give to each of these
        # sample points:
        return self.decompose(map(middle, filter(None, parts)))

    def __combine(self, dict, func):
        ans = self._weighted_({})

        for key, val in self.items():
            ans.add(dict, val,
                    lambda j, k=key, f=func: f(k,j))

        try: return ans.normalise()
        except ZeroDivisionError: return ans

    def combine(self, dict, func, count=None):
        if count is None:
            try: det = dict.__detail
            except AttributeError: count = self.__detail
            else: count = max(det, self.__detail)

        # That's generated our coarse distribution: condense it.
        return self.__combine(dict, func).condense(count)

    # Comparison: which is probably greater ?
    def __cmp__(self, what):
        sign = self.__combine(what, cmp)
        # cmp coerces -ve values to -1, positives to +1; thank you Guido.
        # Thus sign.keys() is [-1, 0, 1] in some order.

        half = sign.total() / 2.
        # if either -1 or +1 has more than half the weight, it wins
        # otherwise, if one is less than half and the other equals half, the latter wins
        # otherwise, it's a draw.

        if sign[-1] < half: b = 0
        else: b = -1

        if sign[1] < half: return b
        return b + 1

# Various statistical computations.

class statWeighted (_baseWeighted):
    """Interface class providing statistical reading of weight dictionaries.

    Provides standard statistical functionality: presumes that instances behave
    as dictionaries, which must be arranged for by alloying this base with some
    other base-class providing that functionality (see _Weighted). """

    def median(self):
        """Takes the median of a distribution.

        Choses one of the keys of the distribution, with the aim that at most
        half of the total weight of the distribution lies on either side of the
        given key's neighbourhood.  If two equally good keys present themselves
        (two neighbourhoods abut at the `true' median), a choice is made between
        them: this choice might legitimately be arbitrary.

        I should really work out how to generalise this to the n-iles
        (i.e. those points in the distribution which are to n as pentiles are to
        5 and the median is to 2: there are n-1 n-iles (though one could bump
        that up to n+1 by regarding the top and bottom of the distribution as
        `boundary' n-iles for all n).

        See, for comparison, joinWeighted.condense() and repWeighted.niles(). """

        if not self: raise ValueError, 'Taking median of an empty population'
        if len(self) == 1: return self.keys()[0] # trivial case

        row = self.sortedkeys
        lo, hi = 0, len(row)
        top = bot = .5 * self.total()
        # I'll call this half-total simply `half', below.
        # half - top and half - bot are sum(self[row[hi:]]) and sum(self[row[:lo]])

        while bot > self[row[lo]]:
            bot = bot - self[row[lo]]
            lo = lo + 1
        # assert: self[row[lo]] >= bot > 0, so
        # infer: sum(self[row[:lo+1]]) >= half > sum(self[row[:lo]])
        # and, subtracting each from total,
        # infer: sum(self[row[lo+1:]]) <= half < sum(self[row[lo:]])

        while top > self[row[hi - 1]]:
            hi = hi - 1
            top = top - self[row[hi]]
        # assert: 0 < top <= self[row[hi-1]]
        # as above, 
        # sum(self[row[hi:]]) < half <= sum(self[row[hi-1:]])
        # sum(self[row[:hi]]) > half >= sum(self[row[:hi-1]])
        # comparison with the above tells us hi-1 >= lo, so hi > lo ;^>

        # Each end has been claiming territory up to, but never quite reaching,
        # the half-way mark on an imaginary line, which we can mark out into
        # contiguous chunks, each labelled with a key of self, with length that
        # key's value in self, ordering the chunks by the order of their labels
        # in row.  Any chunk wholly to one side or the other of the half-way
        # mark will be claimed by its end of the line.  So an unclaimed chunk
        # must either have the half-way mark as one of its end-points, or
        # actually stradle it.  Likewise, either the half-way mark lies on a
        # boundary between chunks or within a chunk.  We thus have two cases:

        # i) half-way line falls in a chunk, so it is the only one unclaimed:
        if lo + 1 == hi: return row[lo]     # the one entry in row[lo:hi]

        # ii) the half-way line fell at the boundary of two chunks.  This case
        # should be rare, so I don't mind it being computationally expensive.

        # Clearly, in this case, there's a risk that the loop tests might just
        # get arithmitis and find almost-equality to be >, so an end might claim
        # a piece which abuts the half-way line.  If that happens to one but not
        # the other, I'm happy that the i) case has succeeded here: if to both,
        # however, we should re-wind to what ii) expects.

        # On the other hand, hereafter we want hi-1 rather than hi, so ...
        if lo == hi: lo = lo - 1
        else: hi = hi - 1
        # assert: hi is lo + 1

        # Now, decide between lo and hi, or (rather) between
        low, hie = row[lo], row[hi]

        # low represents an interval whose
        # width is roughly (row[lo+1] - row[lo-1]) / 2
        # total integral of the density is self[low]
        # typical density is thus 2 * self[low] / (row[lo+1] - row[lo-1])
        # so try to pick the candidate who has higher typical density
        try:
            left = self[low] * (row[hi+1] - row[hi-1])
            rite = self[hie] * (row[lo+1] - row[lo-1])
            # so ratio left:rite is same as typical density in lo:hi intervals.
        except IndexError: pass
        else:
            if left < rite: return hie
            if rite < left: return low

        # well, if I can't compute typical densities, or if the densities agree,
        # I'll just have to make do with totals:

        if self[hie] < self[low]: return low
        # err high in the event of a tie.
        return hie

    def _moments(self, n):
        """Returns a tuple of moments of the given distribution.

        Argument, n, is the highest order for which moments are desired.
        Returns a tuple with 1+n entries: for i running from 0 to n, result[i]
        is the sum, over (key, val) in self.items(), of val * pow(key, i). """

        row = (n + 1) * [ 0 ]
        for key, val in self.items():
            for i in range(n):
                row[i] = val + row[i]
                val = val * key
            row[n] = val + row[n]

        return tuple(row)

    def normalise(self):
        """Returns variant on self normalised to have .total() equal to 1."""
        sum = self.total()
        if sum == 1: return self
        if not sum: raise ZeroDivisionError, (
            'Attempted to normalise zero-sum mapping', self)

        # NB: sum *might* itself be a weighted thing
        if 1./sum == 0:
            # cope with infinity by going via a half-way house:
            self = self.copy(scale = 1./max(self.values()))
            sum = self.total()

        elif 1./(1./sum) == 0:
            # infinitessimal - similar, but in two steps
            # since 1./max(self.values()) may be zero ...
            s = pow(max(self.values()), -.5)
            self = self.copy(scale=s).copy(scale=s)
            sum = self.total()

        return self.copy(scale = 1. / sum)

    def total(self, add=lambda x, y: x + y): return reduce(add, self.values(), 0)
    def _total(self): return self.total(), # analogue for _mean, _variance, ...

    def _mean(self):
        norm, sum = self._moments(1)
        return norm, sum * 1. / norm

    def mean(self): return self._mean()[1]

    def _variance(self):
        norm, sum, squares = self._moments(2)
        mean = sum * 1. / norm
        return norm, mean, squares * 1. / norm - mean * mean

    def variance(self): return self._variance()[2]

    def modes(self):  # in no particular order
        same = ()
        for key, val in self.items():
            if not same or val > most: most, same = val, [ key ]
            elif val == most: same.append(key)

        return tuple(same)

    def mode(self):
        row = self.modes()
        if not row: raise ValueError, 'empty population has no mode'
        if len(row) < 2: return row[0] # easy case
        row, n = list(row), len(row)
        row.sort()
        if n % 2: return row[n/2]

        # have to chose among middle pair: take closest to median or mean
        n = n / 2
        lo, hi = row[n-1], row[n]
        sum = lo + hi
        for mide in (self.median, self.mean):
            mid = mide() * 2
            if mid < sum: return lo
            if mid > sum: return hi

        # failing those, be arbitrary:
        return hi

from basEddy.value import Object

class _Weighted (Object, _baseWeighted):
    """Base-class providing a form of weight-dictionary. """

    # configure Object.copy()
    _borrowed_value_ = ('keys', 'values', 'items', 'has_key', 'get', 'update', 'clear'
                        ) + Object._borrowed_value_

    __upinit = Object.__init__
    def __init__(self, weights, scale=1, *args, **what):
        assert not what.has_key('values'), what['values']
        apply(self.__upinit, args, what)
        self.__weights = {}
        self.add(weights, scale)

        # borrow _borrowed_values_ from weights
        self.borrow(self.__weights)
        # but override copy (see below)

    def _lazy_get_sortedkeys_(self, ignored):
        row = self.keys()
        row.sort()
        return tuple(row)

    # dicts don't have their __*__ methods :-( grr - until python 2.2 :-) so we
    # can't borrow them (yet):
    def __repr__(self): return `self.__weights`
    def __str__(self): return str(self.__weights)
    def __len__(self): return len(self.__weights)

    # override the get/set/del methods (which, likewise, we couldn't borrow)
    def __getitem__(self, key):
        try: result = self.__weights[key]
        except KeyError: return 0
        else:
            assert result >= 0, ('Negative weight', key, val)
            return result

    def __setitem__(self, key, val):
        val = float(val)
        if val < 0: raise ValueError, ('Negative weight', key, val)

        if val > 0:
            self.__weights[key] = val
            self.__change_weights()

        else:
            # don't bother storing 0 values.
            try: del self[key]
            except KeyError: pass

    def __delitem__(self, key):
        del self.__weights[key]
        self.__change_weights()

    def __change_weights(self,
                         # lazy attributes derived from weights:
                         volatiles=('sortedkeys', 'interpolator')):

        for nom in volatiles:
            try: delattr(self, nom)
            except AttributeError: pass

    # Override a method of self.__weights
    __obcopy = Object.copy
    def copy(self, func=None, scale=None):
        """Copies a distribution.

        No arguments are required.  The new distribution is of the same class as
        self.  With no arguments, this distribution is identical to the
        original: the two optional arguments allow for its keys and values
        (respectively) to be different.

        Optional arguments:

          scale -- a scaling to apply to the values in the distribution.
          Default is 1./self.total(), to produce a unit-total result, unless
          self.total() is zero (when there's nothing to copy anyway).

          func -- a function to apply to the keys: must accept keys (sample
          points) of the existing distribution, yielding a key for the new
          distribution.  Default is, strictly, None: if func is None, the
          identity, lambda x: x, is (implicitly) used.

        see, e.g., negation and copying of Samples (below). """

        if scale is None:
            sum = self.total()
            if sum: scale = 1. / sum

        bok = {}
        if scale:
            if func:
                for k, v in self.items():
                    h = func(k)
                    # watch out - func might not be monic;
                    # several keys could map to a common value
                    bok[h] = bok.get(h, 0) + v * scale

            elif scale == 1: bok.update(self.__weights)
            else:
                for k, v in self.items():
                    bok[k] = v * scale

        return self.__obcopy(bok)

    # `make something like me'
    def _weighted_(self, *args, **what):
        """Method for overriding by derived classes.

        Generates a new weight-book from the same creation args as a _Weighted.
        Derived classes will typically want to generate instances of themselves,
        which is what this does: if their __init__ has signature incompatible
        with that of _Weighted, they can override this method with something
        which works round that. """
        return apply(self.__class__, args, what)

class Weighted(_Weighted, repWeighted, statWeighted, joinWeighted):
    __joinit = joinWeighted.__init__
    __weinit = _Weighted.__init__

    def __init__(self, weights, scale=1, detail=5, *args, **what):
        apply(self.__weinit, (weights, scale) + args, what)
        self.__joinit(detail)

# That's built Weighted; now to build Sample, its client.
# First, some random tools to deal with overflow and kindred hassles
# (these get deleted once Sample is defined) ...

def _power(this, what):
    try:
        try: return pow(this, what)
        except ValueError: return 1. / pow(this, -what)
    except OverflowError: return pow(long(this), what)

def _multiply(this, what):
    try: return this * what
    except OverflowError: return long(this) * what

def _divide(this, what):
    """Division straightener """
    ratio = this / what # let that decide whether to raise ZeroDivisionError
    # integer division gotcha: we don't want no rounded answers ...
    if -.9375 < ratio * what - this < .9375: return ratio
    # Note that when integer division is exact, that's accepted it.

    # I made the interval wide (and don't test on type) in case some
    # sample-shaped `number' has enough natural width to it that it scarcely
    # fits between -1 and 1 (or, even, between -.5 and .5, since the errors may
    # get doubled by the computation I just did).  Quite what to do if such a
    # number wants to be `integer-like' is another matter ...

    return this / float(what)

# should, in principle, also provide similar for +, - and %

class Sample (Object):
    """Models numeric values by distributions. """

    __alias = {'_str': '_repr'}
    _unborrowable_attributes_ = Object._unborrowable_attributes_ + ('best',)

    # Sub-classes can use bolt-in replacements for Weighted ...
    _weighted_ = Weighted

    __upinit = Object.__init__
    def __init__(self, weights=None, *args, **what):
        # augment lazy aliases:
        try: bok = what['lazy_aliases']
        except KeyError: what['lazy_aliases'] = self.__alias
        else:
            what['lazy_aliases'] = new = {}
            new.update(self.__alias)
            new.update(bok) # so derived classes over-ride Sample

        if isinstance(weights, Sample):
            args = (weights,) + args # i.e. borrow attributes off it
            weights = weights.__weigh

        # massage best estimate:
        try:
            try: best = what['best']
            except KeyError: best = apply(Object, args).best
            else: del what['best']

        except AttributeError:
            self.__best = []
            if not weights and what.get('low', None) is None and what.get('high', None) is None:
                raise TypeError, 'What kind of numeric Sample has no data at all ?'
        else:
            def flatten(b):
                """Coerce a Sample or Weighted to a scalar."""
                while isinstance(b, Sample): b = b.best
                while isinstance(b, statWeighted): b = b.median()
                return b

            try: best[:]
            except (TypeError, AttributeError):
                # presume best is a scalar
                if not weights: weights = { best: 1 }
                # ... using the  given value of best for this, though it may be fuzzy.
                # Now coerce best to a raw scalar and remember it:
                self.__best = [ flatten(best) ]

            else:
                if not weights: weights = best
                self.__best = map(flatten, best)

        # Finished massaging inputs: initialise self.
        apply(self.__upinit, args, what)
        self.__weigh = self._weighted_(weights)
        self.__weigh.reach(what.get('low', None), what.get('high', None))

    def __update(self, weights):
        """Take note of what looks like a distribution.

        Single argument should be a Weighted (or, at least, curveWeighted)
        object.  If this has more than one weight, it's interpreted as a
        distribution describing the value self is meant to represent; otherwise,
        its weight-points are merely used as candidate best estimate values.
        Return value is true precisely if the changes made by this call to
        __update invalidate self.best (and possibly other related values).

        When it *is* read as a distribution: if self has no distribution
        (e.g. because just removed by update()'s preamble for being merely best
        estimate), the new distribution is taken on board as self's
        distribution.  Otherwise, the two distributions are combined via
        point-wise product (see joinWeighted.cross): each is interpreted as
        giving someone's information about our value, and if their experiments
        rule out some values the other's data permitted, it should be ruled out;
        more generally, the probability that our quantity has some value is the
        product of the two opinions' probabilities, give-or-take some
        normalisation of the results. """

        if len(weights) < 2:
            return apply(self.__better, tuple(weights.keys()))
        else:
            try: w = self.__weigh
            except AttributeError: self.__weigh = weights
            else: self.__weigh = w.cross(weights)
            return 1

    def __better(self, *args):
        """Support for update(): notice some new best estimates.

        Each arg (if any) should be someone's best estimate for self.  Returns a
        true value if at least one of these values wasn't already in .__best; in
        that case, caller should del self.best (at some point, possibly after
        doing assorted other things that might also necessitate this).

        If two sources agree *exactly* on best estimate, sooner suspect that
        they're both quoting a common source than believe them independent;
        hence the return value. """

        hit, b = None, self.__best
        for it in args:
            if it not in b:
                b.append(it)
                hit = 1

        return hit

    def update(self, other):
        """Implements the .observe() functionality of quantity.Quantity (q.v.)"""
        # used to also accept: weight=1, func=None, **what
        # and perform: self.__weigh.add(other, weight, func)
        # and: self.__dict__.update(what)
        # but hopefully that's all redundant ...

        hit = None # have we invalidated stuff computed from __weights ?

        # Is __weigh bogus ?  If so, note anything of interest from it and forget it.
        if len(self.__weigh) < 2:
            for k in self.__weigh.keys():
                if k not in self.__best: self.__best.append(k)
            del self.__weigh
            hit = 1

        elif len(self.__weigh) <= len(self.__best):
            for k, v in self.__weigh.items():
                if not (k in self.__best and v is 1): break
            else:
                # weigh is just bests, as set below when no distribution available
                del self.__weigh
                hit = 1

        # Extract useful information from other:
        if isinstance(other, Sample):
            if self.__update(other.__weigh): hit = 1
            if self.__better(other.best): hit = 1

        elif isinstance(other, curveWeighted):
            if self.__update(other): hit = 1

        else:
            # theoretically, if other is a dict or seq, wrap it as a Weighted
            # and .__update() with it; but I don't think that happens !
            other / 2.3, other - 1.7 # if non-number, these will error for us
            if self.__better(other): hit = 1

        # If that's not given us a distribution, use fall-back bogus one:
        try: self.__weigh
        except AttributeError:
            assert hit
            self.__weigh = self._weighted_(self.__best)

        # If we changed anything, invalidate lazy attributes
        if hit:
            try: del self.best # which _lazy_reset_ might not delete
            except AttributeError: pass

            self.simplify()

    def simplify(self, count=None):
        """Simplifies the distribution describing self.

        Argument, count, is optional (default: None).  If count is omitted or
        None, the distribution's view of how many sample points it should be
        using is used as count.  Otherwise, it specifies the number of sample
        points to retain in the simplification; if it exceeds the number of
        sample points in use, nothing changes. """

        self.__weigh = self.__weigh.condense(count)
        self._lazy_reset_()

    def normalise(self): self.__weigh = self.__weigh.normalise()
    def disperse(self): self.__weigh = self.__weigh.disperse()

    def copy(self, func=None):
        """Copies a sample, optionally transforming it.

        Optional argument, func, is a function to apply to the sample-points.
        Default is None: if func is None, the identity is used. """

        bok = self.dir.copy()
        for key in bok.keys():
            if key[-1:] == '_' or self._lazy_ephemeral_(key):
                del bok[key]

        if func: bok['best'] = func(self.best)
        return apply(self._sampler_, (self.__weigh.copy(func),), bok)

    # Derived classes over-ride this to fiddle behaviour of sums, prods, etc.
    def _sampler_(self, *args, **what): return apply(self.__class__, args, what)

    # now define arithmetic, using join (see below)

    # Binary operators:
    def __add__(self, what, f=lambda x, w: x+w): return self.join(f, what)
    def __sub__(self, what, f=lambda x, w: x-w): return self.join(f, what)
    def __mod__(self, what, f=lambda x, w: x%w): return self.join(f, what)
    def __mul__(self, what, f=_multiply): return self.join(f, what)

    def __radd__(self, what, f=lambda x, w: w+x): return self.join(f, what)
    def __rsub__(self, what, f=lambda x, w: w-x): return self.join(f, what)
    def __rmod__(self, what, f=lambda x, w: w%x): return self.join(f, what)
    def __rmul__(self, what,
                 f=lambda x, w, m=_multiply: m(w, x)): return self.join(f, what)

    # division is slightly messier, thanks to ZeroDivisionError
    def __div__(self, what, f=_divide):
        try: lo, hi = cmp(what.low, 0), cmp(what.high, 0)
        except AttributeError:
            if not what:
                raise ZeroDivisionError, ('Dividing by zero', self, what)
        else:
            if lo == 0 == hi or lo * hi < 0:
                raise ZeroDivisionError, ('Dividing by interval about 0', self, what)

        return self.join(f, what)

    def __rdiv__(self, what, f=lambda x, w, d=_divide: d(w, x)):
        lo, hi = cmp(self.low, 0), cmp(self.high, 0)
        if lo == 0 == hi or lo * hi < 0:
            raise ZeroDivisionError, ('Dividing by interval about 0', what, self)

        return self.join(f, what)

    # For pow, expect simple argument:
    def __pow__(self, what, f=_power): return self.join(f, what)
    # officially: (self, what [, modulo ]) ... for ternary pow().
    def __abs__(self): return self.copy(abs)

    # Representation:
    def __repr__(self): return self._repr
    def __str__(self): return self._str
    def _lazy_get__repr_(self, ignored): return self.__weigh.round(self.best)
    _lazy_get__str_ = _lazy_get__repr_

    # Extractor function (not method) needed by join and __cmp__:
    def extract(what):
        try: bok = what.__weigh
        except AttributeError: return { what: 1 }, what
        return bok, what.best

    def join(self, func, what, grab=extract):
        """Combine with another Sample via a two-parameter function.

        First argument is the function, second is the other sample (or a plain
        number, which will be handled as if it were a single-point sample).  Do
        not pass more than two arguments.

        An intermediate distribution is built: for each point in each
        distribution (self and the other), the product of their weights gives
        the weight used for the result of applying the function to the two
        sample points.  The resulting distribution may then be somewhat
        simplified.  Its best estimate is obtained by applying the function to
        self's best estimate and that of the other sample. """

        bok, best = grab(what)
        return self._sampler_(self.__weigh.combine(bok, func),
                              best=func(self.best, best))

        # problems arise; a sample * quantity is a sample, not a quantity, so
        # loses its units !  the underlying __weigh has a quantity as a key
        # ... if that key has units, the product has no _repr, because
        # __weigh.round(best) has to compare `best < 0', violating
        # dimensionality.

        # Work-around: say Quantity(sample) * quantity ...

    # Comparison:
    def __cmp__(self, what, grab=extract):
        bok, best = grab(what)
        return cmp(self.__weigh, bok) or cmp(self.best, best)

    del extract

    # Hashing:
    __why_lazy_hash = """Lazy hash value for samples.  Sub-optimal.

    Inconveniently, IIRC, the relationship between hashing and comparison
    requires equal keys to hash to the same value, making it hard (given the
    oddities of Sample comparison) to see any sensible dependance of the hash on
    the data of a Sample's distribution, even if samples weren't mutable, so all
    samples must have the same hash - making mappings using them as keys behave
    as linked lists (with a large hash-table overhead).  The easiest way to
    ensure that all samples have the same hash is to have them inherit its value
    from the base-class, Sample ... albeit hooking inheritance of a value into
    python's __hash__() idiom is here implemented by exploiting Lazy's fall-back
    hashing mechanism.

    Ideally, I'd replace this constant with something of form:

        def _lazy_get__lazy_hash_(self, ignored, base=...):
            return hash(self.__weigh) ^ base

    with base's default being the shared value below.  The problem is in
    deciding what Weighted.__hash__() should do ... """

    # and that documentation'll get inherited alongside what it describes:
    _lazy_hash = hash('Sample') ^ hash(Weighted) ^ hash(Object) # (this is evil)

    # Other built-ins:
    def __nonzero__(self): return not(self.high == 0 == self.low)
    # NB: not(nonzero) is stricter than == 0.
    def __float__(self): return float(self.mean)
    def __long__(self): return long(self.median)
    def __int__(self): return int(self.median)
    def __neg__(self): return self._neg

    def _lazy_get__neg_(self, ignored, neg=lambda x: -x):
        result = self.copy(neg)
        result._neg = self
        return result

    def _lazy_get_best_(self, ignored):
        b = self.__best
        if b: b.sort()
        else: return self.mean

        mid, bit = divmod(len(b), 2)
        if bit: return b[mid]
        return (b[mid] + b[mid-1]) * .5

    def __call_or_best_(self, func):
        try: return func()
        except (ValueError, ZeroDivisionError): return self.best

    def _lazy_get_mean_(self, ignored): return self.__call_or_best_(self.__weigh.mean)
    def _lazy_get_mode_(self, ignored): return self.__call_or_best_(self.__weigh.mode)
    def _lazy_get_median_(self, ignored): return self.__call_or_best_(self.__weigh.median)
    def _lazy_get_modes_(self, ignored):
        try: return self.__weigh.modes()
        except (ValueError, ZeroDivisionError): return (self.best,)

    # lazy lookup of variance (and mean):
    def _lazy_get_variance_(self, ignored):
        try: total, self.mean, vary = self.__weigh._variance()
        except (ValueError, ZeroDivisionError):
            raise ValueError, ('Seeking variance of degenerate distribution',
                               self.__weigh)
        return vary

    def _lazy_get_width_(self, ignored): return self.span[1] - self.span[0]
    def _lazy_get_mirror_(self, ignored): return self.__weigh.condense()
    def _lazy_get_errors_(self, ignored): return self - self.best

    def _lazy_get_dispersal_(self, ignored): return self.__weigh.dispersal()
    # self / self.dispersor is dimensionless (and its `entropoid' is zero, FAPP).
    def _lazy_get_dispersor_(self, ignored): return self.__weigh.dispersor()

    def _lazy_get_low_(self, ignored): return self.__weigh.low()
    def _lazy_get_high_(self, ignored): return self.__weigh.high()
    # but contrast (low, high) with:
    def _lazy_get_span_(self, ignored): return self.bounds()
    def bounds(self, frac=1):
        """Returns upper and lower bounds on self's spread.

        Single argument, frac, is the fraction of self's distribution which
        should lie between the bounds returned; if greater than 1 or less than
        0, it is treated as its default, 1 - the upper and lower bounds of the
        distribution are returned.  Thus 95% confidence bounds for self's value
        are returned by self.bounds(0.95). """

        return self.__weigh.bounds(frac)

    def fractiles(self, n, mid=None):
        """Cuts self's distribution into n equal parts.

        First argument, n, is the number of parts into which to subdivide.
        Second argument, mid, controls whether you get n band-centres or 1+n
        band-ends.

        Returns a tuple of points in self's distribution; between any adjacent
        pair of these, 1/n of the distribution's weight lies.  If mid is true,
        there is weight 0.5/n to the left of the first entry in the tuple and
        the same to the right of the last, and there are n entries in the tuple.
        If mid is false (the default) the first and last entries are the nominal
        extremes of the distribution and there are 1+n entries in the tuple. """

        return self.__weigh.niles(n, mid)

del _power, _multiply, _divide
_surprise = """\
Note that one can do some surprising things with Sample()s; e.g.:
    >>> gr = (1 + Sample({5.**.5: 1, -(5.**.5): 1}))/2
    >>> gr
    0.
    >>> gr+1
    2.
    >>> gr**2
    0.
    >>> gr**2 > gr+1
    1

in which gr's weighs are the roots to x*x=x+1 (and its .best is .5).
Notice that gr.copy(lambda x: x**2-x-1) and gr**2-gr-1 will have quite
different weight dictionaries !
"""

Sample.tophat = Sample(Weighted.tophat, best=0,
                       __doc__="""Unit width zero-centred error bar.

Also known as 0 +/- .5, which can readily be used as
a simple way to implement a+/-b as a + 2*b*tophat.""")

_rcs_log_ = """
  $Log: sample.py,v $
  Revision 1.27  2004-12-30 14:42:55  eddy
  Corrected phrasing of interpolator._unit's scale invariance properties.

  Revision 1.26  2004/02/15 16:31:12  eddy
  Fix stupid bug in repWeighted.bounds() when frac between 0 and 1

  Revision 1.25  2004/02/15 16:16:00  eddy
  Got reach() wrong for case with prior data; fixed.

  Revision 1.24  2004/02/15 15:41:48  eddy
  Allow weights to be empty if reach()ing is going to give us a value.

  Revision 1.23  2004/02/14 20:41:50  eddy
  Added new method, reach, to curveWeighted, so Sample can ensure its
  distribution reaches any low or high bounds it's given.  Moved tophat
  onto Sample, as attribute (so Quantity's tophat doesn't hide it).
  Some tidying.

  Revision 1.22  2003/09/24 21:25:57  eddy
  Turned __extract_ into a function, not a method; tunnelled it into join and
  __cmp__, del after; shuffled these three together to limit extract's scope.

  Revision 1.21  2003/09/24 21:01:38  eddy
  Tweaks to cope with bigfloat.BigFloat() as sample points.
  Added what*0 to __unit()'s 10. to coerce its type.
  Try parsing str(unit) to compute exponent.

  Revision 1.20  2003/09/21 14:17:35  eddy
  Deal with infinity in repWeighted.round()

  Revision 1.19  2003/07/05 15:03:04  eddy
  Renamed Sample.__bundle_ to .join, so other code can use it.
  Documented it in the process.

  Revision 1.18  2003/04/21 19:55:25  eddy
  Handle case where a Sample is supplied as the weights when constructing
  a Sample (so quantity.qSample doesn't need a constructor).  Made best a
  non-borrowable attribute, but made what *would* be borrowed serve as a
  replacement for what['best'], when absent.  Ensured any Sample gets
  non-empty __weigh when initialised, but allow constructor to be given no
  weights - in which case a best estimate must be available !

  Revision 1.17  2003/04/21 11:49:19  eddy
  More change to Sample.update(); ensure single-point distributions are
  interpreted as mere best estimates, so that joinWeighted.cross can work
  sensibly.  Stopped struggling to tell lazy infrastructure to preserve
  .best's value - __best handles that for us, now.  Abandonned update's
  old **what, which I'm now sure was redundant and I don't want.  Renamed
  curveWeighted without leading _, sanitised _rcs_* variables.

  Revision 1.16  2003/04/20 23:54:04  eddy
  Changed Sample.update() to `intersect' distributions rather than
  `uniting' them; i.e. multiply pointwise, rather than adding.  Did this
  by adding a .cross method to joinWeighted, supported in turn by
  interpolator.cross (including a bodge to cope with combining
  incompatible distributions).

  Ripped interpolator out of repWeighted into a new base-class,
  _curveWeighted (along with the entropy-related toys that didn't belong
  in repWeighted), so that joinWeighted and repWeighted can both inherit
  from it.  May make replacement interpolators easier to do.

  Also, changed Sample.update() to only mess with distribution if offered
  new value is also a distribution, otherwise only add it to a list of
  `best' estimates whose median to use as actual .best attribute; this
  list also gets an entry from any distributions offered, if they're
  packaged as Sample() so have a .best to offer.  Stopped bothering with
  (hopefully) unused weight/func args specific to add(), which was the
  crux of the old implementation.  Skeptical about retaining **what.

  Revision 1.15  2003/04/20 14:53:35  eddy
  Fix to width-computation in repWeighted.__embrace, so as to ensure
  correct rounding when the half-width interval fails to embrace the
  target value; either because it's outside the distribution, or because
  enough weight was on the weight-point nearest it.  Fixes rounding bug
  first noticed in tophat**2's repr being "0".

  Revision 1.14  2003/01/26 16:03:25  eddy
  Bug-fix so .between() copes if input sequence includes some positions
  less than all cut-points of our distribution (i.e. our result needs to
  begin with some zeros).

  Revision 1.13  2002/10/07 17:53:42  eddy
  Fixed two bugs: normalise() would return None if total() was 1; copy()
  would discard some weights if func mapped several keys to equal outputs.
  Also made __combine() normalise its result if possible, added support
  for % to Sample and made its __rdiv__ complain if both bounds are zero
  (thus matching what __div__ does).

  Revision 1.12  2002/10/06 16:12:45  eddy
  Added note, _surprise, on how Sample()s can behave strangely.

  Revision 1.11  2002/10/06 15:41:52  eddy
  Lose leading hspace on tophat's doc string.

  Revision 1.10  2002/10/06 14:20:00  eddy
  Doc tweaks to *Weighted, more lambda tweaks, added tophat implementing +/-.5.
  Fixed bug in width induced by recent rehabilitation of high and low.
  Fixed Sample's .modes to always produce a tuple (unlike __call_or_best_).

  Revision 1.9  2002/10/05 13:06:45  eddy
  Use parameter defaults to build various lambda expressions only once.
  Replaced repWeighted's `internal use only' sorted keys params with
  sortedkeys lazy attribute on _Weighted.  Gave Sample a str() and
  partially rehabilitated low and high methods on _baseWeighted.  Added
  lots of commentary (much on possible difficulties) and untabified.

  Revision 1.8  2001/12/13 03:53:42  eddy
  Refined Weighted.__cmp__ and moved it from _Weighted to joinWeighted.
  Also moved obsolete low, high to baseWeighted.  May retire later.

  Revision 1.7  2001/12/12 16:56:27  eddy
  Reinstated _str but made it an alias for _repr.
  Shuffled Sample.__init__ to handle best via **what rather than overtly.
  untabified.

  Revision 1.6  2001/12/12 14:58:04  eddy
  shuffled parts of _Weighted for clarity of reading.
  Made Sample use _repr for __str__ rather than having a separate
  _str for derived classes to need to over-ride as well as _repr.

  Revision 1.5  2001/12/10 20:42:43  eddy
  Major re-write of repWeighted to use piecewise uniform interpolation
  mediated by an inner class as _lazy_get_interpolator_(), simplified
  bounds() and niles() to use interpolator.split().  Changed decompose to
  use repWeighted's notion of weights between end-points, tweaked
  condense.  Made normalisation a feature of statWeighted, fixed noddy
  bugs in mode and fractiles.  Added the means to compute dispersal
  (entropy) along with various uses of it.

  Revision 1.4  2001/11/30 17:54:51  eddy
  Much messing and rumination; about to do revolutionary change to repWeighted.
  Replaced internal-use optional row arguments with sortedkeys attribute.
  Moved cmp and hash together and documented irritation.  Value -> Object.
  Added bounds()/span(), fractiles()/niles()/share_cut() features,
  tweaked rounding, expanded docs.

  Revision 1.3  1999/12/31 18:33:46  eddy
  Much has changed ...

  Revision 1.2  1999/07/04 12:15:42  eddy
  First working version.

  Initial Revision 1.1  1999/06/01 21:17:47  eddy
"""
