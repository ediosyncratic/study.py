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

    @classmethod
    def __weighted__(cls, *args, **what):
        """Method to over-ride to match common constructor signature"""
        return cls(*args, **what)

# First layer of functionality: interpret self as describing a curve.
import math # del at end of this page
from study.snake.lazy import Lazy # likewise

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
    .weigh(), .split() with the same semantics as here and have a .cuts
    attribute with a suitable meaning.\n"""

    # Approximate a gaussian with mean 0 and standard deviation 1:
    gaussish = { 0: .382,
                 1: .242, -1: .242,
                 2: .0608, -2: .0608,
                 3: .0062, -3: .0062 }
    # Variance is 1.082, which is a fraction wide, but endurable.

    # Support tool: interpolator to interpret the dictionary as a curve.
    from study.maths.interpolator import PiecewiseConstant as Interpolator
    class Interpolator (Interpolator):
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
        with steps at the mid-points between adjacent weights.\n"""

        @classmethod
        def fromSample(cls, weigher):
            """Initialize from a weight-mapping.

            First argument is a mapping from values to weights.  (Second
            argument is ignored; it's part of the lazy-attribute protocol and
            should always have value 'interpolator'.)  Initializes self with
            attributes .cuts and .mass; cuts is a sorted list of bounds of
            intervals; in each interval from a cuts[i] to cuts[1+i], there is
            exactly one key of the mapping, whose value is used as size[i];
            this is the integral of the curve described by self over the
            interval.

            This only constrains cuts[0] to be less, and cuts[-1] to be
            greater, than all keys of the mapping; the actual values used span
            the range of keys.  When only one key is available, it is used for
            both; otherwise, these bounds are so placed that the intervals
            around the max and min keys stretch twice as far outside the key
            as inwards towards the interior of the distribution - except for
            one heuristic adjustment.  If all keys of the mapping are on one
            side of zero and the foregoing would place a bound on the other
            side, that bound is moved to zero.\n"""
            try: mids = weigher.sortedkeys
            except AttributeError:
                mids = weigher.keys()
                mids.sort()

            return cls.fromMidMass(mids, map(lambda k, w=weigher: w[k], mids))

        @classmethod
        def fromMidMass(cls, mids, mass):
            if not mass or not mids:
                raise ValueError('Too little data', mids, mass)
            return cls(cls.__cuts(mids), mass)

        @staticmethod
        def __cuts(row, mean=lambda a, b: .5*(a+b)):
            # computes cut points and ensures they're floats.
            if len(row) < 2:
                if not row: return ()
                x = 1. * row[0] # coerce it to real
                return (x, x)

            assert not filter(None,
                              map(lambda x, y: y < x, row[:-1], row[1:])), \
                   ('expected sorted data', row)

            # Between extreme key b and its neighbour i, we have a cut
            # at (b+i)/2; twice as far the other side of it is 2*b-i,
            # a.k.a. b +(b-i), or b -(i-b).
            top, bot = 2. * row[-1] - row[-2], 2. * row[0] - row[1]
            if top > 0 and row[-1] <= 0: top = 0.
            if bot < 0 and row[0] >= 0: bot = 0.

            return ( bot, ) + tuple(map(mean, row[:-1], row[1:])) + ( top, )

        def reach(self, low=None, high=None, share=1e-6):
            """Return self extended to span given values.

            Arguments:
               low -- None, or a lower bound that self should reach
               high -- None, or an upper bound that self should reach
               share -- fraction of self's total weight available to reach
                        bounds

            Note that if low or high is None, or is already within self's
            range, it will be ignored; if self is empty, share will be
            ignored.\n"""
            if self:
                cuts, mass, n = self.cuts, self.mass, 0
                if low is not None:
                    if low >= cuts[0]: low = None
                    else: n += 1
                if high is not None:
                    if high <= cuts[-1]: high = None
                    else: n += 1
                if n: way = self.total * share * 1. / n
                else: return self

                if low is not None: cuts, mass = (low,) + cuts, (way,) + mass
                if high is not None: cuts, mass = cuts + (high,), mass + (way,)
                return self.__interpolator__(cuts, mass)

            # else: ignore share - low and high shall be our only points !
            if high is None:
                if low is None: return self
                return self.__interpolator__((low, low), 1)

            if low is None: return self.__interpolator__((high, high), 1)
            return self.__interpolator__((low, high), 1)

        @classmethod
        def gaussian(cls, mean=0, variance=1, count=None, fudge=1.082):
            # Ignores count.
            bok = curveWeighted.gaussish
            mids = bok.keys()
            mids.sort()
            mass = map(lambda k, b=bok: b[k], mids)
            mids = map(lambda k, m=mean, s=variance**.5/fudge: m + k * s, mids)
            return cls.fromMidMass(mids, mass)

        def toWeights(self, mean=lambda x, y: .5 * (x + y)):
            # Turn a distribution into a weight dictionary:
            bok, cut = {}, self.cuts
            if len(self) == 1 and cut[0] < cut[1]:
                # Split simple interval evenly in two:
                cut = (cut[0], mean(cut[0], cut[1]), cut[1])
                mass = self.mass[0] * .5
                mass = (mass, mass)
            else: mass = self.mass

            if len(mass) > 1:
                seq = iter(mass)
                bok[(cut[0] + 2*cut[1])/3.] = seq.next()
                for k in map(mean, cut[1:-2], cut[2:-1]):
                    bok[k] = seq.next()
                bok[(2*cut[-2] + cut[-1])/3.] = seq.next()
                try: seq.next()
                except StopIteration: pass
                else: assert False, 'That should have exhausted the iterator !'
            else:
                assert len(cut) == 2
                assert cut[0] == cut[1] # conjecture !
                bok[mean(cut[0], cut[1])] = 1. # any value will do ...

            return bok
    # end of inner class Interpolator

    def _lazy_get_interpolator_(self, ignored, cls=Interpolator.fromSample):
        return cls(self)

    # Odd little fripperies that fascinate me, connected to entropoid:

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
            assert False, 'Does this ever even happen ?'
            # cope with infinity by going via a half-way house:
            self = self.copy(scale = 1./max(self.values()))
            i = self.interpolator
        elif 1./(1./i.total) == 0:
            assert False, 'Does this ever even happen ?'
            # cope with infinitessimals by going via two half-way houses:
            s = pow(max(self.values()), -.5)
            self = self.copy(scale=s).copy(scale=s)
            i = self.interpolator

        return self.copy(scale = exp( - i.entropoid / i.total))

del math, Lazy

# Integration over intervals, etc.; including rounding a `best estimate'.
# Makes heavy use of baseWeighted's interpolator.

class repWeighted (curveWeighted):
    """Base-class for rounding (whence representation) and integration.\n"""

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

        Optional argument, frac (default: 1), is the proportion of self's
        total weight which is to fall between the bounds; ignored
        (i.e. treated as 1) unless between 0 and 1.  Returns a twople (lo, hi)
        for which:

            self.between(*self.bounds(f)) == f * self.total()

        Thus, for instance, 95% of a distribution lies between the two entries
        in the .bounds(0.95) of the distribution; 2.5% lies below the first
        entry and 2.5% lies above the second.\n"""

        # One could arguably prefer: a,b = bounds(u, 1-frac, v)
        # with u+v=frac and a,b as close together as possible.

        if 0 <= frac < 1:
            gap = .5 * (1 - frac)
            return self.interpolator.split([ gap, frac, gap ])

        cut = self.interpolator.cuts
        return cut[0], cut[-1]

    def round(self, estim=None, group=3):
        """Returns a rounding-string for estim.

        Arguments are optional:
          estim -- value to be represented; or None, meaning the distribution's
                   median should be used.
          group -- if an exponent is included in the number, it should be a
                   multiple of this; defaults to 3.

        Result is a string representing estim to some accuracy, in %e-style
        (or %E-style) format.  This implicity represents an interval, given by
        `plus or minus a half in the last digit'.  This interval will contain
        estim.

        Normally, the interval denoted by the result string will contain less
        than half the weight of self's distribution and is the shortest such
        representation.  E.g. if self.between(3.05, 3.15) >= .5 >
        self.between(3.135, 3.145) then self.round(pi) will return '3.14'.

        That would yield an infinite string if half (or more) of self's weight
        sat at estim, i.e. self's half-width about estim were zero.  In (only)
        this case, the number of significant digits of estim given in the
        result string will be eight more than the number of sample-points of
        self (the speed of light, in m/s, is a nine-digit integer with one
        sample-point; eight is the offset needed for it to display neatly);
        and if the next five digits would all have been 0, any trailing zeros
        will be elided from the ones given [for various sanity
        reasons].  Sugar: in this `exact' case, any exponent used will employ
        E rather than e (thus 1.2E1 for 12); and if no digits appear after the
        '.'  in an exact representation, the '.' is omitted.\n"""

        estim, ent = self.interpolator.round(estim) # expon-ent
        if ent is None: return `estim`
        digits = '%.0f' % (estim / 10. ** ent)
        if digits.isdigit(): sign = ''
        else:
            sign, digits = digits[0], digits[1:]
            assert digits.isdigit()

        ent += len(digits)
        # so now 1 > estim / 10**ent >= .1;
        # thus sign + '.' + digits + 'e%d' % ent would be a valid answer

        q, r = divmod(ent, group) # ent = group*q +r
        if r * 3 > 2 * group: # i.e. r > group*2/3
            # Rather than many digits before the decimal point, have a few
            # zeros immediately after it:
            q, r = q + 1, group - r
            digits = '0' * r + digits
            r = 0

        if q:
            if self.interpolator.weigh((estim, estim), 2)[1] < 1: e = 'e'
            else: e = 'E'
            tail = e + '%d' % (group * q)
        else: tail = ''

        return sign + digits[:r] + '.' + digits[r:] + tail

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

    def add(self, weights, scale=1, func=None, smooth=None,
            oneach=lambda x: (x, 1)):
        """Increment some of my keys.

        Arguments:

          weights -- a mapping (e.g. dictionary), for each key of which we'll be
                     performing self[key] = self.get(key, 0) + weights[key],
                     save that weights[key] may be scaled by scale and key may
                     have been replaced with func(key) - see below.  If a
                     sequence is given, it is read as a mapping with the
                     sequence as .keys() and all values equal to 1; if anything
                     other than a sequence or mapping is given, it is read as a
                     single key with value 1.

          [scale=1] -- a scaling to apply to all values in weights

          [func=None] -- a callable which accepts keys of weights and returns
                         keys for self.  Used to transform any keys of weights
                         which aren't joinWeighted instances.

        If a key to be given to self (either a key of weights or func's output
        from such) is itself a joinWeighted, self.add recurses with
        self.add(key, weights[key] * scale, func).  If a key is a Sample, its
        .mirror is obtained: this is a joinWeighted and the same recursion is
        used.  Otherwise, (func's replacements for) keys should be scalars. """

        if smooth is not None and weights is None:
            weights = smooth.toWeights()

        try: mites = weights.items()
        except AttributeError:
            try: mites = map(oneach, weights[:])
            except (TypeError, AttributeError): mites = [ (weights, 1) ]

        if smooth is None:
            # Those filtered out need the recursive calls to .add():
            smooth = filter(lambda (k,v): not isinstance(
                    k, (Sample, joinWeighted)), mites)
            if smooth:
                if func is not None:
                    smooth = map(lambda (k, v), f=func: (f(k), v))
                smooth = self.Interpolator.fromSample(dict(smooth))

        if smooth: smooth = smooth.scale(by=scale)
        try: prior = self.interpolator
        except AttributeError: pass
        else:
            if smooth: smooth += prior
            else: smooth = prior

        for key, val in mites:
            val = val * scale
            if val < 0: raise ValueError('Negative weight', val, key, scale)
            if isinstance(key, Sample): key = key.mirror

            if isinstance(key, joinWeighted): self.add(key, val, func)
            else:
                if func is not None: key = func(key)
                self[key] = self[key] + val

        if smooth: self.interpolator = smooth

    def cross(self, other):
        """Pointwise product of two distributions.

        This models set intersection and the inference of a combined
        likelihood distribution from those due to two separate data-sets, with
        some gentle munging to cope with poorly-overlapped sources.

        If the distributions were gaussians, the new distribution would also
        be gaussian; its mean would be the weighted average of the old means,
        each weighted by the other's variance; and the new variance would be
        the inverse of the sum of inverses of variances.

        In practice, our piecewise constant distributions might not overlap,
        so it's expeditious to mix the straight piecewise product of the
        distributions with (an approximation to) the gaussian that we'd get if
        both source distributions had been gaussians.\n"""
        assert len(self) > 1 < len(other), "delta functions aren't nice here"

        me, yo = self.interpolator, other.interpolator
        prod = me * yo
        if prod.total < 1:
            # Model each as gaussian:
            (mym, mys), (yom, yos) = me.normal, yo.normal
            # Determine interpolator of product gaussian:
            v = 1/(1/mys +1/yos)
            gaus = self.interpolator.gaussian((mym / mys + yom / yos) * v, v)
            gaus = gaus.scale(to = 1 - prod.total)
            if prod.total > 0: prod += gaus
            else: prod = gaus

        return self.__weighted__(None, smooth=prod).condense(
            max(self.__detail, other.__detail))

    def condense(self, count=None, middle=lambda i: i.median()):
        """Simplifies a messy distribution.

        Argument, count, is optional: it specifies the desired level of detail
        in the result (default: None).  None is taken to mean the level of
        detail specified for self when it was created.

        Returns a self.__weighted__() whose keys are: the highest and lowest
        of self, and; count-1 points in between, roughly evenly-spaced as to
        self's weight between them.  The weight of each of these points is
        based on carving up self's weights according to who's nearest.\n"""

        if count is None: count = self.__detail
        if len(self) <= count: return self

        # Carve self up into count-2 interior (count-1)-iles and two
        # half-bands at top and bottom:
        if count < 3: count = 0
        else: count -= 2

        smooth = self.interpolator
        cuts = smooth.split([ 0, 1 ] + [ 2 ] * count + [ 1, 0 ])
        mass = smooth.weigh(cuts)
        assert 1e-6 * smooth.total > max(mass[0], mass[-1])
        return self.__weighted__(None,
                                 smooth=self.Interpolator(cuts, mass[1:-1]))

    def combine(self, other, func, count=None):
        """Generate a combined distribution.

        Required arguments:

          other -- a second numeric entity; typically a weight-dictionary, but
                   anything that Weighted's constructor accepts will do; will
                   by wrapped by Weighted if it lacks a .interpolator; this
                   method shall produce bogus results if it has .interpolator
                   but this doesn't behave like one from curveWeighted.

          func -- function to use to combine numbers; if x is a key of self
                  and y is a key of other, func(x, y) is a suitable value for
                  a key of the result; actual values for keys of result may be
                  computed by interpolation from calls to func on values
                  interpolated from keys of self and other.

        Optional third argument, count, is the target number of items to have
        in the result's weights dictionary; honoured only in so far as
        possible.  Defaults to None, in which case the result has whatever
        number of weights comes naturally.

        For each interval of self's interpolator and each interval of other's,
        example values of func, at extreme values in the respective intervals,
        are used to obtain a range of values of func over which to distribute
        the product of the weights from self and other in the source
        intervals.  This gives a mess of overlapping intervals; every
        end-point of any of these is then used as a cut-point, between which
        to sum the various intervals' contributions, to obtain weights to
        place at the centre-points between cut-points.  Hopefully this is
        somewhat more robust than placing the product of two weights at the
        centre of each rectangle.\n"""

        if not isinstance(other, _baseWeighted):
            other = self.__weighted__(other)

        mix = self.interpolator.combine(func, other.interpolator)
        if count is None or count > len(mix): pass
        else: mix = mix.simplify(count)

        try: mix = mix.scale() # normalise
        except ValueError: pass
        ans = self.__weighted__(None, smooth=mix)
        ans.interpolator = mix
        return ans

    # Comparison: which is probably greater ?
    def __cmp__(self, other):
        if not isinstance(other, _baseWeighted):
            other = self.__weighted__(other)

        return self.interpolator < other.interpolator

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

        See, for comparison, joinWeighted.condense() and Sample.fractiles(). """

        if not self: raise ValueError('Taking median of an empty population')

        smooth, row = self.interpolator, self.sortedkeys
        assert len(smooth) == len(row)
        assert not filter(
            None, smooth.map(lambda l, h, w, m: l > m or m > h, row))
        i, j = 0, len(smooth) - 1
        lo, hi = smooth.mass[i], smooth.mass[j]
        while i + 1 < j:
            up, dn = lo <= hi, hi <= lo
            if up:
                i += 1
                lo += smooth.mass[i]
            if dn:
                j -= 1
                hi += smooth.mass[j]
        if i == j or hi < lo: return row[i]
        if lo < hi: return row[j]
        cut = smooth.cuts[j]
        assert row[i] <= cut <= row[j]
        if cut - row[i] < row[j] - cut: return row[i]
        return row[j]

    def normalise(self):
        """Returns variant on self normalised to have .total() equal to 1."""
        sum = self.total()
        if sum == 1: return self
        if not sum: raise ZeroDivisionError(
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

    def total(self): return sum(self.values())

    def mean(self): return self.interpolator.normal[0]
    def variance(self): return self.interpolator.normal[1]

    def modes(self):  # in no particular order
        same = ()
        for key, val in self.items():
            if not same or val > most: most, same = val, [ key ]
            elif val == most: same.append(key)

        return tuple(same)

    def mode(self):
        row = self.modes()
        if not row: raise ValueError('empty population has no mode')
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

from object import Object

class _Weighted (Object, _baseWeighted):
    """Mixin class providing a form of weight-dictionary."""

    # configure Object.copy()
    _borrowed_value_ = ('keys', 'values', 'items', 'has_key', 'get', 'update', 'clear'
                        ) + Object._borrowed_value_

    __upinit = Object.__init__
    def __init__(self, weights=None, scale=1, smooth=None,
                 *args, **what):
        assert not what.has_key('values'), what['values']
        self.__upinit(*args, **what)
        self.__weights = {}
        self.add(weights, scale, smooth=smooth)
        if smooth is not None: self.interpolator = smooth

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
        if val < 0: raise ValueError('Negative weight', key, val)

        if val > 0:
            self.__weights[key] = val
        else:
            # don't bother storing 0 values.
            try: del self[key]
            except KeyError: pass
        self.__change_weights()

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
    def copy(self, scale=None, func=None):
        """Copies a distribution.

        No arguments are required.  The new distribution is of the same class
        as self.  With no arguments, this distribution is identical to the
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

        See, e.g., negation and copying of Samples (below).\n"""

        smooth = self.interpolator
        if scale is None:
            sum = self.total()
            if sum: scale = 1. / sum
            smooth = smooth.scale()
        else: smooth = smooth.scale(by=scale)

        bok = {}
        if scale: # (else: result is degenerate)
            if func:
                smooth = smooth.combine(func)
                for k, v in self.items():
                    h = func(k)
                    # watch out - func might not be monic;
                    # several keys could map to a common value
                    bok[h] = bok.get(h, 0) + v * scale

            elif scale == 1: bok.update(self.__weights)
            else:
                for k, v in self.items():
                    bok[k] = v * scale

        dir = self.dir.copy()
        dir['smooth'] = smooth
        return self.__class__(bok, **dir)

class Weighted(_Weighted, repWeighted, statWeighted, joinWeighted):
    __joinit = joinWeighted.__init__
    __weinit = _Weighted.__init__

    def __init__(self, weights=None, scale=1, detail=5, *args, **what):
        self.__weinit(weights, scale, *args, **what)
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
    def _weighted_(self, weights, scale=None, smooth=None, cls=Weighted):
        if scale is None:
            try: weights[:]
            except TypeError:
                try: tot = sum(weights.values())
                except AttributeError: tot = 1
            else: tot = len(weights)

            if tot < 0: y = -1.
            else: y = 1.
            if y * tot <= 1: scale = y
            else:
                while y * tot > 1: scale, y = y, y * .5

        if smooth is None: return cls(weights, scale)
        return cls(weights, scale, smooth=smooth)

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
            except KeyError: best = Object(*args).best
            else: del what['best']

            if not weights:
                # Maybe what['best'], or someone in args, is a Sample:
                try: weights = best.mirror
                except AttributeError: pass

        except AttributeError:
            self.__best = []
            if not weights and what.get('low', None) is None and what.get('high', None) is None:
                raise TypeError(
                    'What kind of numeric Sample has no data at all ?')
        else:
            def flatten(b):
                """Coerce a Sample or Weighted to a scalar."""
                while True:
                    if isinstance(b, Sample): b = b.best
                    elif isinstance(b, statWeighted): b = b.median()
                    else: return b

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
        self.__upinit(*args, **what)
        weights = self._weighted_(weights)
        if what.has_key('low') or what.has_key('high'):
            self.__weigh = self._weighted_(
                None,
                smooth=weights.interpolator.reach(
                    what.get('low', None), what.get('high', None)))
        else: self.__weigh = weights

    def __update(self, weights):
        """Take note of another witness of our value.

        Single argument should be a Weighted (or, at least, curveWeighted)
        object.  If this has more than one weight, it's interpreted as a
        distribution describing the value self is meant to represent; otherwise,
        its weight-point is merely used as candidate best estimate value.
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
            return self.__better(*weights.keys())
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

        hit = False # have we invalidated stuff computed from __weights ?

        # Is __weigh bogus ?  If so, note anything of interest from it and forget it.
        if len(self.__weigh) < 2:
            for k in self.__weigh.keys():
                if k not in self.__best: self.__best.append(k)
            del self.__weigh
            hit = True

        elif len(self.__weigh) <= len(self.__best):
            for k, v in self.__weigh.items():
                if not (k in self.__best and v is 1): break
            else:
                # weigh is just bests, as set below when no distribution available
                del self.__weigh
                hit = True

        # Extract useful information from other:
        if isinstance(other, Sample):
            if self.__update(other.__weigh): hit = True
            if self.__better(other.best): hit = True

        elif isinstance(other, curveWeighted):
            if self.__update(other): hit = True

        else:
            # theoretically, if other is a dict or seq, wrap it as a Weighted
            # and .__update() with it; but I don't think that happens !
            other / 2.3, other - 1.7 # if non-number, these will error for us
            if self.__better(other): hit = True

        # If that's not given us a distribution, use fall-back bogus one:
        try: self.__weigh
        except AttributeError:
            assert hit
            self.__weigh = self._weighted_(self.__best)

        # If we changed anything, invalidate lazy attributes
        if hit:
            try: del self.best # which _lazy_reset_ might not delete
            except AttributeError: pass

            self.simplify() # perhaps we should only _lazy_reset_ ?

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
        return self._sampler_(self.__weigh.copy(func=func), **bok)

    # Derived classes over-ride this to fiddle behaviour of sums, prods, etc.
    def _sampler_(self, *args, **what): return self.__class__(*args, **what)

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

    # Division is slightly messier, thanks to ZeroDivisionError
    def __div__(self, what, f=_divide):
        try:
            try: w = what.__weigh
            except AttributeError: lo, hi = what.low, what.high
            else: lo, hi = w.bounds()
            lo, hi = cmp(lo, 0), cmp(hi, 0)
        except AttributeError:
            if not what:
                raise ZeroDivisionError('Dividing by zero', self, what)
        else:
            if lo == 0 == hi or lo * hi < 0:
                raise ZeroDivisionError('Dividing by (interval about) 0',
                                        self, what)

        return self.join(f, what)
    __truediv__ = __div__

    def __rdiv__(self, what, f=lambda x, w, d=_divide: d(w, x)):
        lo, hi = self.__weigh.bounds()
        lo, hi = cmp(lo, 0), cmp(hi, 0)
        if lo == 0 == hi or lo * hi < 0:
            raise ZeroDivisionError('Dividing by interval about 0', what, self)

        return self.join(f, what)
    __rtruediv__ = __rdiv__

    # For pow, expect simple argument:
    def __pow__(self, what, mod=None, f=_power):
        assert mod is None, "Modular power only makes sense for whole things"
        # ... and Sample()s are implicitly real-blurry, not discrete
        return self.join(f, what)
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
        except AttributeError, prior:
            try: return { what: 1 }, what
            except AttributeError, given:
                raise apply(AttributeError,
                            (prior.args + given.args + (what,)))
        return bok, what.best

    def join(self, func, what, grab=extract):
        """Combine with another Sample via a two-parameter function.

        First argument is the function, second is the other sample (or a plain
        number, which will be handled as if it were a single-point sample).  Do
        not pass more than two arguments.

        An composite distribution is built, using products of weights from the
        two samples to provide weights to attach to values returned by the
        function when passed a value from each; self's value is always the
        first parameter to func, the second parameter comes from the other
        sample.  This distribution is combined with a best estimate, obtained
        by applying the function to self's best estimate and that of the other
        sample, to create a new Sample.\n"""

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
        # It might make sense to take the candidate nearest self's
        # distribution's median ... i.e. use the distribution rather than just
        # the ordering in self.__best; but I'm not doing this yet !
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

    def _lazy_get_variance_(self, ignored):
        return self.__weigh.variance()

    def _lazy_get_width_(self, ignored): return self.span[1] - self.span[0]
    def _lazy_get_mirror_(self, ignored): return self.__weigh.condense()
    def _lazy_get_errors_(self, ignored): return self - self.best

    def _lazy_get_dispersal_(self, ignored): return self.__weigh.dispersal()
    # self / self.dispersor is dimensionless (and its `entropoid' is zero, FAPP).
    def _lazy_get_dispersor_(self, ignored): return self.__weigh.dispersor()

    def _lazy_get_low_(self, ignored): return self.__weigh.bounds()[0]
    def _lazy_get_high_(self, ignored): return self.__weigh.bounds()[1]
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

    def fractiles(self, n, mid=False):
        """Cuts self's distribution into n equal parts.

        First argument, n, is the number of parts into which to
        subdivide.  Optional second argument, mid, controls whether you get n
        band-centres or 1+n band-ends.

        Returns a tuple of points in self's distribution; between any adjacent
        pair of these, 1/n of the distribution's weight lies.  If mid is true,
        there is weight 0.5/n to the left of the first entry in the tuple and
        the same to the right of the last, and there are n entries in the
        tuple. If mid is false (the default) the first and last entries are
        the nominal extremes of the distribution and there are 1+n entries in
        the tuple."""

        if n < 1: raise ValueError(
            'Can only subdivide range into positive number of parts', n)
        split = self.__weigh.interpolator.split
        if mid: return split([ 1 ] + [ 2 ] * n + [ 1 ])
        return split([ 0 ] + [ 1 ] * (1+n) + [ 0 ])

    @staticmethod
    def flat(lo, hi, best=None, *args, **what):

        if isinstance(hi, Sample):
            args = (hi,) + args
            weights = hi.__weigh.normalise()
            hi = what['high'] = hi.high
        else: weights = None

        if isinstance(lo, Sample):
            args = (lo,) + args
            if weights: weights.add(lo.__weigh.normalise())
            else: weights = lo.__weigh.normalise()
            lo = what['low'] = lo.low

        if isinstance(best, Sample):
            args = (best,) + args
            if weights: weights.add(best.__weigh.normalise())
            else: weights = best.__weigh.normalise()
            what['best'] = best.best
        elif best is not None: what['best'] = best

        if not weights:
            weights = {(2*lo + hi)/3.: 1, (lo + 2 * hi)/3.: 1}

        return Sample(weights, *args, **what)

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
    True

in which gr's weighs are the roots to x*x=x+1 (and its .best is .5).
Notice that gr.copy(lambda x: x**2-x-1) and gr**2-gr-1 will have quite
different weight dictionaries !
"""

Sample.gaussish = Sample(Weighted.gaussish, best=0,
                        __doc__="""Roughly normal distribution.

This (piecewise constantly) approximates a gaussian with mean zero and standard
deviation 1.  It is intended for use with data which have been given as mean and
standard deviation; multiply by the latter and add the former.
""")
