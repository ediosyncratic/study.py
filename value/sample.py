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

$Id: sample.py,v 1.3 1999-12-31 18:33:46 eddy Exp $
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
    toolset. """

class statWeighted(_baseWeighted):
    """Interface class providing statistical reading of weight dictionaries.

    Provides standard statistical functionality: presumes that instances behave
    as dictionaries, which must be arranged for by alloying this base with some
    other base-class providing that functionality. """

    def high(self): return max(self.keys())
    def low(self): return min(self.keys())

    def median(self, row=None):
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

        See, for comparison, joinWeighted.condense(). """

        if not self: raise ValueError, 'Taking median of an empty population'
        if len(self) == 1: return self.keys()[0] # trivial case
        if row is None:
            row = self.keys()
            row.sort()

        lo, hi = 0, len(row)
        # half-total - sum(self[row[hi:]]) and half-total - sum(self[row[:lo]]):
        top = bot = .5 * reduce(lambda x, y: x + y, self.values(), 0.)
        # I'll call this half-total simply `half', below.

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
            right= self[hie] * (row[lo+1] - row[lo-1])
            # so ratio left:right is same as typical density in lo:hi intervals.
        except IndexError: pass
        else:
            if left < right: return hie
            if right < left: return low

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

    def total(self): return reduce(lambda x, y: x + y, self.values(), 0)
    def _total(self): return self.total(),

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

        return same

    def mode(self):
        row = modes(self)
        if not row: raise ValueError, 'Asking for mode of empty population'
        row.sort()
        n = len(row)
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
    operations (i.e.  taking two distributions and obtaining the joint
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
            except AttributeError: mites = [ (weights, 1) ]
            else: mites = map(lambda x: (x, 1), weights)

        for key, val in mites:
            val = val * scale
            if val < 0: raise ValueError, ('Negative weight', val, key, scale)
            if isinstance(key, Sample): key = key.mirror

            if isinstance(key, joinWeighted): self.add(key, val, func)
            else:
                if func: key = func(key)
                self[key] = self[key] + val

    def decompose(self, new, row=None):
        """Decomposes self.

        Argument, new, is a sorted sequence of keys to use.  The highest and
        lowest keys of self will also be used: and only keys in new which lie
        between these extremes will actually be used.  Optional second argument
        is private to this class and should not be given by other callers.

        Each weight in self contributes its all to whichever of the selected
        keys is closest to it. """

        if row is None:
            row = self.keys()
            row.sort()
        if not row: return self.copy()

        # assert: new is sorted (but may have some repeated entries)
        count = len(new)
        # flush out any repeats and out-of-range
        run = []
        for k in new:
            if k not in run and row[-1] > k > row[0]:
                run.append(k)
        if run: new = run
        else: new = [ row[-1] ] # happens to lead to sane results

        # Now flatten self's distribution by `moving' each key to the nearest
        # entry in new.  Do this by going through self.keys() in order, so we
        # always know which entries in new need considered as possible nearests.

        result, ind, run = {}, 0, 0
        # ind: which member of new was nearest the last key
        # run: a running total for result[new[ind]]
        mid, cut = row[0], row[0] + new[0]
        # mid: the key whose weight run is accumulating
        # cut: when 2 * key exceeds this, it's time to advance along new

        for key in row:
            while 2 * key > cut:
                result[mid] = run
                ind = ind + 1

                try: mid = new[ind]
                except IndexError:      # off new's end
                    cut, mid = 2 * row[-1] + 1, row[-1]
                else:
                    try: cut = mid + new[ind+1]
                    except IndexError: cut = mid + row[-1]      # final cut

                run = result.get(mid, 0)

            run = run + self[key]

        result[mid] = run
        assert mid is row[-1]

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
        row = self.keys()
        row.sort()

        # Carve self up into count-1 interior count-iles and two half-bands at
        # top and bottom.
        step, parts, last = self.total() / count, [ self._weighted_({}) ], None
        gap = .5 * step # total weight remaining in current band
        assert step > 0, 'Condensing degenerate weighting'

        for key in row:
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

        # Select new sample points from parts: outer ends of the half-bands,
        # medians of the interior count-iles.  Work out how much weight to give
        # to each of these sample points.
        return self.decompose(map(lambda i: i.median(),
                                  filter(None, parts[1:-1])),
                              row)

    def combine(self, dict, func, count=None):
        ans = self._weighted_({})
        for key, val in self.items():
            ans.add(dict, val,
                    lambda j, k=key, f=func: f(k,j))

        # that's generated our coarse distribution
        if count is None:
            try: det = dict.__detail
            except AttributeError: count = self.__detail
            else: count = max(det, self.__detail)

        return ans.condense(count)

# Support tool: interpolator to interpret the dictionary as a curve:
import string

class pwLinterp:
    """Interpolation handler.

    This is a piecewise-linear interpolator. """

    def tail_slice(self, total, width, near, far):
        """Computes the weight in a band in a tail of a distribution.

        Models this by the area of a slice of a right-angle triangle.  The RAT's
        right-angled corner is taken as origin, total is twice the area of the
        triangle, width is how long one of its sides is: this side is taken as
        the X-axis.  Two lines are drawn parallel to the Y axis, at X-ordinates
        near and far: the value returned is the area of the portion of the
        triangle lying between these two lines.

        The triangle's base is width, its height is total/width.  Its height at
        X-ordinate x (between 0 and width) is thus (1 - x/width) * total/width,
        giving an area .5 * (width - x) times this height to the right of x.
        This is .5 * total * pow(1 - x/width, 2).  We thus take the difference
        of two such quantities, with x as near and far (possibly clipped) to
        obtain our answer. """

        if near >= far: return 0
        assert 0 <= near < far, ('Bad args:', near, far)

        # Transform X-ordinates as x-> 1 - x/width: at the same time, clip the
        # interval to the triangle.
        rate = 1. / width

        if near < width: fat = 1 - rate * near
        else: return 0      # band and triangle did not overlap.

        if far > width: thin = 0
        else: thin = 1 - rate * far

        # expand fat * fat - thin * thin ...
        return .5 * total * (fat - thin) * (fat + thin)

    def body_slice(self, before, width, after, start, gap):
        """Computes the weight in a band between two weight-points.

        Two weights, before and after, are width apart: the band, of size gap,
        begins start along from the before weight.

        Constraint: body_slice(after, width, before, width - start - gap, gap)
        should be the same. """

        if gap == 0: return 0
        assert 0 <= start < start + gap <= width, ('Bad args', start, gap, width)

        return after * gap / width + self.tail_slice(
            before - after, width, start, start + gap)

# Next layer of functionality: probabilities in intervals,
# rounding a `best estimate'
class repWeighted(_baseWeighted):
    """Base-class for representation and integration.

    We now introduce an implicit curve described by the weight dictionary.
    This is arrived at by interpolation between keys.

    If there is only one key, no interpolation is possible: an exact `delta
    function' is assumed.  Otherwise, the gaps between adjacent keys are used
    for interpolation: and the boundary keys are extrapolated outwards to a
    width equal to that of the gap adjacent to the boundary.  Interpolation is
    controlled by an object's interpolator attribute: this base class provides
    an interpolator which performs `piecewise linear' interpolation.  This reads
    each { key: weight } as a pair of triangles, each of area half the weight,
    one spanning each of the gaps to either side of key.

    """

    interpolator = pwLinterp()
    def between(self, low=None, high=None, row=None):
        """Returns the weight associated with an interval.

        Arguments are the low and high bounds of the interval.  Either may be
        None, indicating an interval unbounded at that end.

        Treats each interval between weight-points as having half of the weight
        of each point inside it, uniformly distributed.  Treats each end as a
        triangular wedge whose width is the separation of the last two
        weight-points.  This is in suck-it-and-see mode: it may get `refined'
        later. """

        if low is None:
            if high is None: return self.total()
        elif high is not None and low >= high: return 0.
        if row is None:
            row = self.keys()
            row.sort()

        if not row: return 0.
        if len(row) is 1:
            # special case: delta function
            if low is None or low < row[0]:
                if high is None or high > row[0]:
                    return self.total()
                if high == row[0]: return .5 * self.total()
            elif low == row[0]:
                if high is None or high > row[0]:
                    return .5 * self.total()
            return 0

        lo, hi = 0, len(row) - 1
        if low is not None:
            while lo <= hi and row[lo] < low: lo = lo + 1
        if high is not None:
            while lo <= hi and row[hi] > high: hi = hi - 1
        # so row[lo:hi] lies between low and high - or is empty

        if lo > hi:
            # extreme case: no keys in the interval
            if lo is 0:
                # left tail: low < high <= row[0]
                return self.interpolator.tail_slice(
                    self[row[0]], row[1] - row[0],
                    row[0] - high, row[0] - low)

            elif lo is len(row):
                # right tail: row[-1] <= low < high
                return self.interpolator.tail_slice(
                    self[row[-1]], row[-1] - row[-2],
                    low - row[-1], high - row[-1])

            else:
                # interval between two weights:
                return self.interpolator.body_slice(
                    self[row[hi]], row[lo] - row[hi], self[row[lo]],
                    low - row[hi], high - low)

        assert low <= row[lo] <= row[hi] <= high, (low, high, lo, hi)
        # (the other cases have already returned).

        if lo < hi:
            # Compute the weight from row[lo] to row[hi]: this consists of half
            # the band at each end, plus all the bands in between (if any).
            mid = .5 * (self[row[hi]] + self[row[lo]])
            for key in row[lo+1:hi]: mid = mid + self[key]

        else: mid = 0.

        # Now deal with two tail-pieces ...
        if lo > 0:
            bot = self.interpolator.body_slice(
                self[row[lo]], row[lo] - row[lo-1], self[row[lo-1]],
                0, row[lo] - low)
        else:
            bot = self.interpolator.tail_slice(
                self[row[0]], row[1] - row[0],
                0, row[0] - low)

        if hi < len(row) - 1:
            top = self.interpolator.body_slice(
                self[row[hi]], row[hi+1] - row[hi], self[row[hi+1]],
                high - row[hi], 0)
        else:
            top = self.interpolator.tail_slice(
                self[row[-1]], row[-1] - row[-2],
                0, high - row[-1])

        return bot + mid + top

    def __unit(self, what):
        """Returns a suitable power of 10 for examining what."""

        decade = 0
        while what >= 10: what, decade = what / 10., decade + 1
        while what < 1: what, decade = what * 10, decade - 1

        return pow(10., decade)

    def round(self, estim=None, row=None):
        """Returns a rounding-string for estim.

        Argument, estim, is optional: if omitted, the distribution's median (if
        available) or mean (likewise) will be used.  Result is a string
        representing this value to some accuracy, in %e-style format.

        Result string implicity represents an interval, given by `plus or minus
        a half in the last digit'.  This interval will contain estim and will
        span less than half of self's total weight.  The result will be the
        shortest string which matches this constraint.

        E.g. if self.between(3.05, 3.15) >= .5 > self.between(3.135, 3.145) then
        self.round(pi) will return '3.14'. """

        # Handle argument defaults:
        if estim is None:
            try: estim = self.median()
            except (AttributeError, ValueError): estim = self.mean()

        if row is None:
            row = self.keys()
            row.sort()

        if not row: return `estim`

        threshold, stop = .5 * self.total(), 7 + len(row)
        assert threshold > 0, ('Weights need to be positive for rounding algorithm',
                               threshold, self.values())

        # Find half-width of self:
        hi = top = len(row)
        while hi > 0 and row[hi-1] > estim: hi = hi - 1
        # assert row[hi] > estim >= row[hi-1], subject to IndexError
        lo = hi

        weight = 0      # sum(row[lo:hi]: self :)
        while weight <= threshold:
            # sign = cmp(mid-point, estim), but adjust if mid-point ill-defined
            if lo <= 0:
                assert hi < top, 'self.total() lied to me'
                sign = -1
            elif hi >= top: sign = +1
            else: sign = row[hi] + row[lo-1] - 2 * estim

            if sign > 0:        # grow down
                lo = lo - 1
                weight = weight + self[row[lo]]
            else:               # grow up
                weight = weight + self[row[hi]]
                hi = hi + 1

        width = row[hi-1] - row[lo]     # plus some tails:
        if hi < top: width = width + .5 * (row[hi] - row[hi-1])
        elif top > 1: width = width + row[-1] - row[-2]
        if lo > 0: width = width + .5 * (row[lo] - row[lo-1])
        elif top > 1: width = width + row[1] - row[0]

        # Initialise:
        if estim < 0: head, sign = '-', -1
        else: head, sign = '', +1
        body, round, aim = '', 0, 0

        unit = self.__unit(max(abs(estim), width) or 1)
        if unit == 1: tail = ''
        else: tail = ('%.0e' % unit)[1:]

        # Loop until over-long string or as precise as we'll allow:
        while len(body) < stop and (unit > width or weight > threshold):
            # Compute unrounded digit for present:
            dig = int((estim - aim) / unit / sign)
            # Compute unrounded approximation thereby implied:
            aim = aim + sign * unit * dig
            body = body + `dig`

            # Determine whether to round:
            round = cmp((estim - aim) * sign, .5 * unit)
            # on exact half, round to even:
            if round == 0: round = { 1: +1, 0: -1 } [dig % 2]

            # If rounding, use next digit up as mid-point: otherwise, use unrounded.
            if round < 0: mid = aim
            else: mid = aim + sign * unit

            if width <= 0 and estim == mid:
                if tail:
                    # Use %E to flag exact match, %e otherwise ...
                    assert tail[0] == 'e', tail
                    tail = 'E' + tail[1:]
                break

            # Compute weight in interval implied by suitably-rounded value:
            weight = self.between(mid - .5 * unit, mid + .5 * unit)

            # Prepare for next digit:
            unit = .1 * unit

        # Propagate rounding as necessary:
        try:
            # (if round > 0: while round: contracted to ...)
            while round > 0:
                # Take last character off body, transfer to tail, rounding up.
                last = body[-1]         # may fall off left end here ...
                body = body[:-1]

                try: tail = { '0': '1', '1': '2', '3': '4', '4': '5',
                              '5': '6', '6': '7', '7': '8', '8': '9'
                              }[last] + tail
                except KeyError: tail = '0' + tail
                else: round = 0

        except IndexError: head = head + '1'          # ... fell off left end !

        # Join body to tail and punctuate:
        body = body + tail
        if body: body = body[:1] + '.' + body[1:]

        return head + body

from basEddy.value import Value

class _Weighted(Value, _baseWeighted):
    """Base-class providing a form of weight-dictionary. """

    # configure Value.copy()
    _borrowed_value_ = ('keys', 'values', 'items', 'has_key', 'get', 'update', 'clear'
                        ) + Value._borrowed_value_

    def __init__(self, weights, scale=1, *args, **what):
        assert not what.has_key('values'), what['values']
        apply(Value.__init__, (self,) + args, what)
        self.__weights = {}
        self.add(weights, scale)

        # borrow _borrowed_values_ from weights
        self.borrow(self.__weights)
        # but override copy

    # dicts don't have their __*__ methods (grr) so we can't borrow them:
    def __repr__(self): return `self.__weights`
    def __str__(self): return str(self.__weights)
    def __len__(self): return len(self.__weights)
    def __delitem__(self, key): del self.__weights[key]

    # override the get/set methods (which, likewise, we couldn't borrow)
    def __getitem__(self, key):
        try: result = self.__weights[key]
        except KeyError: return 0
        else:
            assert result >= 0, ('Negative weight', key, val)
            return result

    def __setitem__(self, key, val):
        val = float(val)
        if val > 0: self.__weights[key] = val
        elif val < 0: raise ValueError, ('Negative weight', key, val)
        # don't bother storing 0 values

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

        if scale == 0: bok = {}
        else:
            if func is None:
                if scale == 1: bok = self.__weights.copy()
                else:
                    bok = {}
                    for k, v in self.items():
                        bok[k] = v * scale
            else:
                bok = {}
                for k, v in self.items():
                    bok[func(k)] = v * scale

        return Value.copy(self, bok)

    # Comparison: which is probably greater ?
    def __cmp__(self, what):
        ans = self._weighted_({})
        for key, val in self.items():
            ans.add(what, val, lambda j, k=key: cmp(k, j))

        if abs(ans[1] - ans[-1]) < .5 * ans[0]: return 0
        return cmp(ans[1], ans[-1])

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

class Sample(Value):
    """Primitive distribution modeller. """

    try: _lazy_preserve_ = Value._lazy_preserve_
    except AttributeError: _lazy_preserve_ = ()
    _lazy_preserve_ = tuple(_lazy_preserve_) + ( 'best', )

    # Sub-classes can use bolt-in replacements for Weighted ...
    _weighted_ = Weighted

    def __init__(self, weights, best=None, *args, **what):
        apply(Value.__init__, (self,) + args, what)

        if best is not None:
            if not weights: weights = { best: 1 }
            # ... using the  given value of best for this, though it may be fuzzy.
            # Now coerce best to a raw scalar ...

            while isinstance(best, Sample):
                best = best.best

            while isinstance(best, statWeighted):
                best = best.median()

            # ... and remember it.
            self.best = best

        self.__weigh = self._weighted_(weights)

    def update(self, other, weight=1, func=None, **what):
        old = self.__weigh
        old.add(other, weight, func)
        self.__weigh = old.condense()
        self._lazy_reset_()
        self.__dict__.update(what)

    def normalise(self):
        sum = reduce(lambda x, y: x + y, self.__weigh.values(), 0.)
        if sum == 1: return
        if sum == 0: raise ZeroDivisionError, (
            'Attempted to normalise zero-sum mapping', self.__weigh)

        self.__weigh = self.__weigh.copy(scale = 1. / sum)

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
    def _sampler_(self, weights, best=None, *args, **what):
        return apply(self.__class__, (weights, best) + args, what)

    def __extract_(self, what):
        try: bok = what.__weigh
        except AttributeError: return { what: 1 }, what
        return bok, what.best

    # now define arithmetic
    def __bundle_(self, func, what):
        bok, best = self.__extract_(what)
	return self._sampler_(self.__weigh.combine(bok, func),
                              func(self.best, best))

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

    # Comparison:
    def __cmp__(self, what):
        bok, best = self.__extract_(what)
        return cmp(self.__weigh, bok) or cmp(self.best, best)

    # Representation:
    def __repr__(self): return self._repr
    def __str__(self): return self._str

    def _lazy_get__repr_(self, ignored):
        return self.__weigh.round(self.best)

    def _lazy_get__str_(self, key):
        return self._lazy_get__repr_(key)

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

    def _lazy_get_low_(self, ignored): return self.__call_or_best_(self.__weigh.low)
    def _lazy_get_high_(self, ignored): return self.__call_or_best_(self.__weigh.high)
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

    _lazy_hash = hash('Sample') ^ hash(Weighted) ^ hash(Value)

_rcs_id = """
  $Log: sample.py,v $
  Revision 1.3  1999-12-31 18:33:46  eddy
  Much has changed ...

  Revision 1.2  1999/07/04 12:15:42  eddy
  First working version.

  Initial Revision 1.1  1999/06/01 21:17:47  eddy
"""
