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

$Id: sample.py,v 1.8 2001-12-13 03:53:42 eddy Exp $
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
    the other parts, so only such alloys are viable: instances of the component
    classes won't work.

    Sub-classes:

      statWeighted -- provides for statistical computations;
      joinWeighted -- provides for combining distributions (and transforming them);
      repWeighted -- integrating and rounding;
      _Weighted -- Object packaging weights dictionary;

    These are all alloyed together to build the final class, Weighted, which is
    what Sample actually uses. """

    # these aren't ideal; nor do I still use them ...
    def high(self): return max(self.keys())
    def low(self): return min(self.keys())

class statWeighted(_baseWeighted):
    """Interface class providing statistical reading of weight dictionaries.

    Provides standard statistical functionality: presumes that instances behave
    as dictionaries, which must be arranged for by alloying this base with some
    other base-class providing that functionality.

    Also provides for comparison.
    """

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
        if sum == 1: return
        if sum == 0: raise ZeroDivisionError, (
            'Attempted to normalise zero-sum mapping', self)

        if 1./sum == 0:
            # cope with infinity or infinitessimal by going via a half-way house:
            self = self.copy(scale = 1./max(self.values()))
            sum = self.total()

        elif 1./(1./sum) == 0:
            # infinitessimal - similar, but in two steps
            # since 1./max(self.values()) may be zero ...
            s = pow(max(self.values()), -.5)
            self = self.copy(scale=s).copy(scale=s)
            sum = self.total()

        return self.copy(scale = 1. / sum)

    def total(self): return reduce(lambda x, y: x + y, self.values(), 0)
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

class joinWeighted(_baseWeighted):
    """Interface-class defining how to stick distributions together.

    Provides apparatus for adding data to a distribution, re-sampling a
    distribution, obtaining a canonical distribution (i.e. one whose weights'
    neighbourhoods don't overlap) and for performing `cartesion product'
    operations (i.e. taking two distributions and obtaining the joint
    distribution for some combinatin of their parameters). """

    def __init__(self, detail):
        self.__detail = detail

    def add(self, weights, scale=1, func=None):
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
            try: weights[:]
            except (TypeError, AttributeError): mites = [ (weights, 1) ]
            else: mites = map(lambda x: (x, 1), weights)

        for key, val in mites:
            val = val * scale
            if val < 0: raise ValueError, ('Negative weight', val, key, scale)
            if isinstance(key, Sample): key = key.mirror

            if isinstance(key, joinWeighted): self.add(key, val, func)
            else:
                if func: key = func(key)
                self[key] = self[key] + val

    def decompose(self, new):
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
            load = self.interpolator.weigh(map(lambda a, b: (a+b)/2, run[:-1], run[1:]))
            for k in run: result[k], load = load[0], load[1:]

        # Build a new distribution with this weighting:
        return self._weighted_(result, detail=len(result))

    def condense(self, count=None):
        """Simplifies a messy distribution.

        Argument, count, is optional: it specifies the desired level of detail
        in the result (default: None).  None is taken to mean the level of
        detail specified for self when it was created.

        Returns a self._weighted_() whose keys are: the highest and lowest of
        self, and; count-1 points in between, roughly evenly-spaced as to self's
        weight between them.  The weight of each of these points is based on
        carving up self's weights according to who's nearest. """

        if count is None: count = self.__detail
        if len(self) <= count: return self.copy()

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
        return self.decompose(map(lambda i: i.median(), filter(None, parts)))

    def __combine(self, dict, func):
        ans = self._weighted_({})

        for key, val in self.items():
            ans.add(dict, val,
                    lambda j, k=key, f=func: f(k,j))

	return ans

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

import math # del at end of this page
from basEddy import Lazy # likewise
# Next layer of functionality: integrating over intervals, etc.
# including rounding a `best estimate'.

class repWeighted(_baseWeighted):
    """Base-class for rounding (whence representation) and integration.

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

    More sophisticated replacements for repWeighted may be worth using in its
    place when alloying a modified Weighted for use by Sample.  The replacement
    should be sure to provide between(), weights(), carve() and round(). """

    # Support tool: interpolator to interpret the dictionary as a curve.
    class _lazy_get_interpolator_ (Lazy):
        """Integration of a curve interpolated from a weights-dictionary.

        What should be happening here ?
        repWeighted (or its s, e.g. a Bezier interpolator) provides:
          between([low, high]) -- defaults, None, mean relevant infinity; yeilds weight
          weights(row) -- map(self.between, [ None ] + row, row + [ None ])
          carve(weights) -- yields tuple for which map(self.between, [ None ] * len, yield) = weights.
          round([estim]) -- yields string describing estim to self's accuracy.

        The meaning of a distribution is:
        we have { position: weight, ... }
        and ks = sortedkeys lists the positions in increasing order.
        What an entry { ks[i]: w } in the mapping means is that the
        total weight between (ks[i-1]+ks[i])/2 and (ks[i]+ks[i+1])/2 is w.
        It doesn't say anything about mean or median.

        Doing it piecewise linearly looks a pig.  So do it piecewise constant, with a
        step at the mid-points between adjacent weights.
        """

        def __init__(self, weigher, ignored):
            self.cuts = self.__cuts(weigher.sortedkeys)
            self.size = tuple(map(lambda k, w=weigher: w[k], weigher.sortedkeys))
            # the curve described has integral load[i] between cut[i] and cut[1+i].

        def __cuts(self, row):
            # computes cut points and ensures they're floats.
            if not row: return ()
            if len(row) < 2:
                x = 1. * row[0]
                return (x, x)

            return ( 2. * row[0] - row[1], ) + \
                   tuple(map(lambda a,b: (a+b)/2., row[:-1], row[1:])) + \
                   ( 2. * row[-1] - row[-2], )

        def _lazy_get_total_(self, ignored): return reduce(lambda a,b: a+b, self.size, 0)

        def split(self, weights):
            """Cuts the distribution into pieces in the proportions requested.

            Required argument, weights, is a list of non-negative values, having
            positive sum.  A scaling is applied to all entries in the list to
            make its sum equal to self.total().

            Returns a list, result, one entry shorter than weights, for which
            self.weigh(result) equals the re-scaled weights-list. """

            assert not filter(lambda x: x < 0, weights), 'weights cannot be negative'
            scale = self.total / reduce(lambda a, b: a+b, weights, 0.)

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
            sequence, with:each being the integral over the distribution between
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

        # dispersal, a.k.a. entropy or information content, computations:

        def _lazy_get_entropoid_(self, ignored, log=math.log):
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

            return reduce(lambda a, b: a+b,
                          map(lambda l,h,w,g=log: w*g(w/(h-l)),
                              cut[:-1], cut[1:], siz))

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
            its.  See the doc of self._unit (i.e. _lazy_get__unit_.__doc__) for
            further discussion. """

            if len(self.size) < 2: return 0 # delta function - no dispersal ?

            a = self.total
            return log(a / self._unit) - self.entropoid / a

        def _lazy_get__unit_(self, ignored):
            """A `width of the distribution' unit for use in normalising dispersal.

            This is still exploratory.

            A suitable unit must be independent of applying an overall scaling
            to self's weights, or to self's cuts; it must also be unchanged by
            an overall translation of self's cuts.  Thus it must be a `width' of
            the distribution, such as the total span or standard deviation.

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

    def bounds(self, frac=1):
        """Bounds self's distribution.

        Optional argument, frac (default: 1), is the proportion of self's total
        weight which is to fall between the bounds; ignored (i.e. treated as 1)
        unless between 0 and 1.  Returns a 2-tuple (lo, hi) for which:

            apply(self.between, self.bounds(f)) == f * self.total()

        Thus, for instance, 95% of a distribution lies between the two entries
        in the .bounds(0.95) of the distribution; 2.5% lies below the first
        entry and 2.5% lies above the second. """

        if 0 <= frac < 1:
            return self.interpolator.split([ frac / 2., 1. - frac, frac / 2. ])
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
    def __embrace(self, total, about, row):
        """Finds a slice of self, returns its weight and width.

        Arguments:
          total -- lower-bound on total weight of the slice.
          about -- slice will span this value
          row -- self's keys, sorted.
        """

        hi = top = len(row)
        while hi > 0 and row[hi-1] > about: hi = hi - 1
        # assert hi is 0 or row[hi] > about >= row[hi-1]
        lo = hi

        # Expand [lo:hi] until it embraces more than half self's weight:
        weight = 0      # sum, for key in row[lo:hi], of self[key]
        while weight <= total:
            # Chose an end at which to expand [lo:hi]

            if lo <= 0: # we can only grow upwards
                if hi >= top: break
                sign = +1
            elif hi >= top: sign = -1 # we can only grow downwards
            else:
                # cmp(about, mid-point), but dodge int/2 gotcha
                sign = 2 * about - row[hi] - row[lo-1]

            if sign < 0:        # grow down
                lo = lo - 1
                weight = weight + self[row[lo]]
            else:               # grow up
                weight = weight + self[row[hi]]
                hi = hi + 1

        # assert weight > total or (lo, hi) == (0, top), 'while loop exit'

        # So, how wide is this range ?
        # Separation between end-points plus the tail of each end-band:
        width = row[hi-1] - row[lo]
        if hi < top: width = width + .5 * (row[hi] - row[hi-1])
        elif top > 1: width = width + row[-1] - row[-2]
        if lo > 0: width = width + .5 * (row[lo] - row[lo-1])
        elif top > 1: width = width + row[1] - row[0]

        return weight, width

    def __unit(self, what):
        """Returns a suitable power of 10 for examining what."""

        decade = 0
        while what >= 10: what, decade = what / 10., decade + 1
        while what < 1: what, decade = what * 10, decade - 1

        return pow(10., decade)

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

        # Deal with dumb case:
        if len(self) < 1: return `estim`

        # Compute half-weight:
        threshold = .5 * self.total()
        assert threshold > 0, ('Weights need to be positive for rounding algorithm',
                               threshold, self.values())

        # Find half-width of self:
        weight, width = self.__embrace(threshold, estim, self.sortedkeys)

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
        unit = self.__unit(max(abs(estim), width) or 1)

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

        if unit == 1: tail = ''
        else: tail = ('%.0e' % unit)[1:] # '1e93'[1:]
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

del math, Lazy

from basEddy.value import Object

class _Weighted(Object, _baseWeighted):
    """Base-class providing a form of weight-dictionary. """

    # configure Object.copy()
    _borrowed_value_ = ('keys', 'values', 'items', 'has_key', 'get', 'update', 'clear'
                        ) + Object._borrowed_value_

    def __init__(self, weights, scale=1, *args, **what):
        assert not what.has_key('values'), what['values']
        apply(Object.__init__, (self,) + args, what)
        self.__weights = {}
        self.add(weights, scale)

        # borrow _borrowed_values_ from weights
        self.borrow(self.__weights)
        # but override copy

    def _lazy_get_sortedkeys_(self, ignored):
        row = self.keys()
        row.sort()
        return tuple(row)

    # dicts don't have their __*__ methods (grr) so we can't borrow them:
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

        # else: don't bother storing 0 values.

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
    def copy(self, func=None, scale=None):
        """Copies a distribution.

        No arguments are required.  The new distribution is of the same class as
        self.  With no arguments, this distribution is identical to the
        original: the two optional arguments allow for its keys and values
        (respectively) to be different.

        Optional arguments:

          scale -- a scaling to apply to the values in the distribution.
          Default is 1./self.total(), to produce a unit-total result, provided
          self's total is positive: otherwise a default of 1 is used.

          func -- a function to apply to the keys: must accept keys (sample
          points) of the existing distribution, yielding a key for the new
          distribution.  Default is, strictly, None: if func is None, the
          identity, lambda x: x, is (implicitly) used.

        see, e.g., negation and copying of Samples (below). """

        if scale is None:
            sum = self.total()
            if sum > 0: scale = 1. / sum
            else: scale = 1

        bok = {}
        if scale:
            if func:
                for k, v in self.items():
                    bok[func(k)] = v * scale

            elif scale == 1: bok.update(self.__weights)
            else:
                for k, v in self.items():
                    bok[k] = v * scale

        return Object.copy(self, bok)

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
    def __init__(self, weights, scale=1, detail=5, *args, **what):
        apply(_Weighted.__init__, (self, weights, scale) + args, what)
        joinWeighted.__init__(self, detail)

# random tools to deal with overflow and kindred hassles ...

def _power(this, what):
    try:
        try: return pow(this, what)
        except ValueError: return 1. / pow(this, -what)
    except OverflowError: return pow(long(this), what)

def _multiply(this, what):
    try: return this * what
    except OverflowError: return long(this) * what

def _rmultiply(this, what): return _multiply(what, this)

def _divide(this, what):
    """Division straightener """
    ratio = this / what # let that decide whether to raise ZeroDivisionError
    # integer division gotcha: we don't want no rounded answers ...
    if -.9375 < ratio * what - this < .9375: return ratio
    # Note that when integer division is exact, that's accepted it.

    # I made the interval wide (and don't test on type) in case some
    # sample-shaped `number' has enough natural width to it that it scarcely
    # fits between -1 and 1 (or even, between -.5 and .5, since the errors may
    # get doubled by the computation I just did).  Quite what to do if such
    # a number wants to be `integer-like' is another matter ...

    return this / float(what)

def _rdivide(this, what): return _divide(what, this)

class Sample (Object):
    """Models numeric values by distributions. """

    try: _lazy_preserve_ = Object._lazy_preserve_
    except AttributeError: _lazy_preserve_ = ()
    _lazy_preserve_ = tuple(_lazy_preserve_) + ( 'best', )
    __alias = {'_str': '_repr'}

    # Sub-classes can use bolt-in replacements for Weighted ...
    _weighted_ = Weighted

    def __init__(self, weights, *args, **what):
        # augment lazy aliases:
        try: bok = what['lazy_aliases']
        except KeyError: what['lazy_aliases'] = self.__alias
        else:
            new = {}
            new.update(self.__alias)
            new.update(bok) # so derived classes over-ride Sample
            what['lazy_aliases'] = new

        # massage best estimate:
        try: best = what['best']
        except KeyError: pass
        else:
            if not weights: weights = { best: 1 }
            # ... using the  given value of best for this, though it may be fuzzy.
            # Now coerce best to a raw scalar ...

            while isinstance(best, Sample):
                best = best.best

            while isinstance(best, statWeighted):
                best = best.median()

            # ... and remember it.
            what['best'] = best

        # Finished massaging inputs: initialise self.
        apply(Object.__init__, (self,) + args, what)
        self.__weigh = self._weighted_(weights)

    def update(self, other, weight=1, func=None, **what):
        self.__weigh.add(other, weight, func)
        self.simplify()
        self.__dict__.update(what)

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

    def __extract_(self, what):
        try: bok = what.__weigh
        except AttributeError: return { what: 1 }, what
        return bok, what.best

    # now define arithmetic
    def __bundle_(self, func, what):
        bok, best = self.__extract_(what)
        return self._sampler_(self.__weigh.combine(bok, func),
                              best=func(self.best, best))

    # Binary operators:
    def __add__(self, what): return self.__bundle_(lambda x, w: x+w, what)
    def __sub__(self, what): return self.__bundle_(lambda x, w: x-w, what)
    def __mul__(self, what): return self.__bundle_(_multiply, what)

    def __radd__(self, what): return self.__bundle_(lambda x, w: w+x, what)
    def __rsub__(self, what): return self.__bundle_(lambda x, w: w-x, what)
    def __rmul__(self, what): return self.__bundle_(_rmultiply, what)

    def __div__(self, what):
        try: lo, hi = cmp(what.low, 0), cmp(what.high, 0)
        except AttributeError:
            if not what:
                raise ZeroDivisionError, ('Dividing by zero', self, what)
        else:
            if lo == 0 == hi or lo * hi < 0:
                raise ZeroDivisionError, ('Dividing by interval about 0', self, what)

        return self.__bundle_(_divide, what)

    def __rdiv__(self, what):
        if cmp(self.low, 0) * cmp(self.high, 0) < 0:
            raise ZeroDivisionError, ('Dividing by interval about 0', self)

        return self.__bundle_(_rdivide, what)

    # For pow, expect simple argument:
    def __pow__(self, what): return self.copy(lambda x, _w=what: _power(x, _w))
    def __abs__(self): return self.copy(abs)

    # Representation:
    def __repr__(self): return self._repr
    def __str__(self): return self._str
    def _lazy_get__repr_(self, ignored): return self.__weigh.round(self.best)

    # Comparison:
    def __cmp__(self, what):
        bok, best = self.__extract_(what)
        return cmp(self.__weigh, bok) or cmp(self.best, best)

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

    def _lazy_get__neg_(self, ignored):
        result = self.copy(lambda x: -x)
        result._neg = self
        return result

    def _lazy_get_best_(self, ignored): return self.mean
    def __call_or_best_(self, func):
        try: return func()
        except (ValueError, ZeroDivisionError): return self.best

    def _lazy_get_mean_(self, ignored): return self.__call_or_best_(self.__weigh.mean)
    def _lazy_get_mode_(self, ignored): return self.__call_or_best_(self.__weigh.mode)
    def _lazy_get_modes_(self, ignored): return self.__call_or_best_(self.__weigh.modes)
    def _lazy_get_median_(self, ignored): return self.__call_or_best_(self.__weigh.median)

    # lazy lookup of variance (and mean):
    def _lazy_get_variance_(self, ignored):
        try: total, self.mean, vary = self.__weigh._variance()
        except (ValueError, ZeroDivisionError):
            raise ValueError, ('Seeking variance of degenerate distribution',
                               self.__weigh)
        return vary

    def _lazy_get_width_(self, ignored): return self.high - self.low
    def _lazy_get_mirror_(self, ignored): return self.__weigh.condense()
    def _lazy_get_errors_(self, ignored): return self - self.best

    def _lazy_get_dispersal_(self, ignored): return self.__weigh.dispersal()
    # self / self.dispersor is dimensionless (and its `entropoid' is zero, FAPP).
    def _lazy_get_dispersor_(self, ignored): return self.__weigh.dispersor()
    def _lazy_get_span_(self, ignored): return self.bounds()

    def bounds(self, frac=1):
        """Returns upper and lower bounds on self's spread.

        Single argument, frac, is the fraction of self's distribution which
        should lie between the bounds returned; if greater than 1 or less than
        0, it is treated as its default, 1 - the upper and lower bounds of the
        distribution are returned.  Thus 95% confidence bounds for self's value
        are returned by self.bounds(0.95). """

        return self.__weigh.bounds(frac)

    def _lazy_get_low_(self, key):
        self.low, self.high = self.bounds()
        return { 'low': self.low, 'high': self.high }[key]

    _lazy_get_high_ = _lazy_get_low_

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


_rcs_id = """
  $Log: sample.py,v $
  Revision 1.8  2001-12-13 03:53:42  eddy
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
