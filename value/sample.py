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
with each key of the other, giving the result (as its value) the product of the
two keys' values (added to the same for any other pairs of keys producing the
same result, of course, if they arise).  Then take the resulting big bok of
weighted values, compute its odd 2n-iles for some chosen n, use these as the
keys of a dictionary in which weight[key] is the sum of the big bok's values for
keys which are closer to this key of weight than to any other.

$Id: sample.py,v 1.1 1999-06-01 21:17:47 eddy Exp $
"""

# First, some generally useful tools for { example: weight } mappings.
def median(bok):
    """Takes the median of a distribution.

    Represents a distribution as a dictionary whose keys and values are
    notionally floats, the latter all being positive. """

    if not bok: raise ValueError, 'Taking median of an empty population'
    if len(bok) == 1: return bok.keys()[0] # trivial case

    row = bok.keys()
    row.sort()
    return _median(bok, row)

def _median(bok, sorted):
    lo, hi = 0, len(row)
    # half-total - sum(bok[row[hi:]]) and half-total - sum(bok[row[:lo]]):
    top = bot = .5 * reduce(lambda x, y: x + y, bok.values(), 0.)
    # I'll call this half-total simply `half', below.

    while bot > bok[row[lo]]:
        bot = bot - bok[row[lo]]
        lo = lo + 1
    # assert: bok[row[lo]] >= bot > 0, so
    # infer: sum(bok[row[:lo+1]]) >= half > sum(bok[row[:lo]])
    # and, subtracting each from total,
    # infer: sum(bok[row[lo+1:]]) <= half < sum(bok[row[lo:]])

    while top > bok[row[hi - 1]]:
        hi = hi - 1
        top = top - bok[row[hi]]
    # assert: 0 < top <= bok[row[hi-1]]
    # as above, 
    # sum(bok[row[hi:]]) < half <= sum(bok[row[hi-1:]])
    # sum(bok[row[:hi]]) > half >= sum(bok[row[:hi-1]])
    # comparison with the above tells us hi-1 >= lo, so hi > lo ;^>

    # Each end has been claiming territory up to, but never quite reaching, the
    # half-way mark on an imaginary line, which we can mark out into contiguous
    # chunks, each labelled with a key of bok, with length that key's value in
    # bok, ordering the chunks by the order of their labels in row.  Any chunk
    # wholly to one side or the other of the half-way mark will be claimed by
    # its end of the line.  So an unclaimed chunk must either have the half-way
    # mark as one of its end-points, or actually stradle it.  Likewise, either
    # the half-way mark lies on a boundary between chunks or within a chunk.  We
    # thus have two cases:

    # i) half-way line falls in a chunk, so it is the only one unclaimed:
    if lo + 1 == hi: return row[lo]     # the one entry in row[lo:hi]

    # ii) the half-way line fell at the boundary of two chunks.

    # Clearly, in this case, there's a risk that the loop tests might just get
    # arithmitis and find almost-equality to be >, so an end might claim a piece
    # # which abuts the half-way line.  If that happens to one but not the
    # other, # I'm happy to allow the i) case to succeed here: if to both,
    # however, we # should re-wind to what ii) expects.

    # On the other hand, hereafter we want hi-1 rather than hi, so ...
    if lo == hi: lo = lo - 1
    else: hi = hi - 1
    # assert: hi is lo + 1

    # Now, decide between lo and hi, or (rather) between
    low, hie = row[lo], row[hi]

    # low represents an interval whose
    # width is roughly (row[lo+1] - row[lo-1]) / 2
    # total integral of the density is bok[low]
    # typical density is thus 2 * bok[low] / (row[lo+1] - row[lo-1])
    # so try to pick the candidate who has higher typical density
    try:
        left = bok[low] * (row[hi+1] - row[hi-1])
        right= bok[hie] * (row[lo+1] - row[lo-1])
        # so ratio left:right is same as typical density in lo:hi intervals.
    except IndexError: pass
    else:
        if left < right: return hie
        if right < left: return low

    # well, if I can't compute typical densities, or if the densities agree,
    # I'll just have to make do with totals:

    if bok[hie] < bok[low]: return low
    # err high in the event of a tie.
    return hie

def layers(bok, count, halves=None):
    if len(bok) <= count: return bok

    # OK, so carve bok up
    step = reduce(lambda x, y: x + y, bok.values(), 0.) / count
    parts, last = [{}], None
    if halves: gap = .5 * step
    else: gap = 0

    row = bok.keys()
    row.sort()

    for key in row:
        val = bok[key]

        while gap < val:
            if gap > 0: parts[-1][key] = gap
            parts.append({})
            val, gap = val - gap, step

        if val > 0:
            parts[-1][key] = val
            gap = gap - val

    return parts

def total(bok): return reduce(lambda x, y: x + y, bok.values(), 0)
_total = total

def _mean(bok):
    norm = sum = 0.
    for key, val in bok.items():
        norm, sum = val + norm, key * val + sum

    return norm, sum / norm

def mean(bok): return _mean(bok)[1]

def _variance(bok):
    norm = sum = squares = 0.
    for key, val in bok.items():
        norm, sum, squares = val + norm, key * val + sum, key * key * val + squares

    mean = sum / norm
    return norm, mean, squares / norm - mean * mean

def variance(bok): return _variance(bok)[2]

def modes(bok):  # in no particular order
    same = ()
    for key, val in bok.items():
        if not same or val > most: most, same = val, [ key ]
        elif val == most: same.append(key)

    return same

def mode(bok):
    row = modes(bok)
    if not row: raise ValueError, 'Asking for mode of empty population'
    row.sort()
    n = len(row)
    if n % 2: return row[n/2]
    n = n / 2
    # have to chose among middle pair: take closest to median or mean
    lo, hi = row[n-1], row[n]
    sum = lo + hi
    for mide in (median, mean):
            mid = mide(bok) * 2
            if mid < sum: return lo
            if mid > sum: return hi

    # failing those, be arbitrary:
    return hi

from basEddy.value import Value

class Weighted(Value):
    def __init__(self, weights, scale=1, detail=5, *args, **what):
        apply(Value.__init__, (self,) + args, what)
        self.__weights, self.__detail = {}, detail
        self.add(weights, scale)

        # borrow clear, update, get, has_key, items, keys, values from weights
        self.borrow(lambda k, _w=self.__weights: getattr(_w, k))
        # but override copy

    def __delitem__(self, key): del self.__weights[key]
    def __getitem__(self, key):
        try: return self.__weights[key]
        except KeyError: return 0
    def __setitem__(self, key, val):
        if val > 0: self.__weights[key] = val
        elif val < 0: raise ValueError, ('Negative weight', key, val)
        # don't bother storing 0 values

    def __len__(self): return len(self.__weights)

    def copy(self, func=None, scale=1):
        if func is None: bok = self.__weights.copy()
        else:
            bok = {}
            for k, v in self.items():
                bok[func(k)] = v

        if scale != 1:
            for k, v in bok.items():
                bok[k] = v * scale

        return apply(self.__class__, (bok,))
        

    def add(self, weights, scale=1, func=None):
        """Increment some of my keys.

        Arguments:

          weights -- a mapping (e.g. dictionary), for each key of which we'll be
          performing self[key] = self.get(key, 0) + weights[key], save that
          weights[key] may be scaled by scale and key may have been replaced
          with func(key).  See below.

          [scale=1] -- a scaling to apply to all values in weights

          [func=None] -- a callable which accepts keys of weights and returns
          keys for self.  Used to transform any keys of weights which aren't
          Weighted instances.

        If a key of weights is itself a Weighted, self.add recurses with
        self.add(key, weights[key] * scale, func).  Otherwise, keys should be
        scalars (at least, by the time func has done with them, if supplied).
        """

        try: mites = weights.items()
        except AttributeError: mites = [ (weights, 1) ]
        for key, val in mites:
            val = scale * val
            if isinstance(key, Weighted): self.add(key, val, func)
            else:
                if func: key = func(key)
                self[key] = self[key] + val

    def high(self): return max(self.keys())
    def low(self): return min(self.keys())

    def median(self): return median(self.__weights)
    def mean(self): return mean(self.__weights)
    def variance(self): return variance(self.__weights)
    def modes(self): return modes(self.__weights)

    def condense(self, count=None, bok=None):
        if count is None: count = self.__detail
        if bok is None:
            if len(self) <= count: return self.copy()
            bok = self.__weights
        elif len(bok) <= count: return bok

        # OK, so carve bok up
        parts = layers(bok, count, halves=1)
        # that's partitioned it into count-ile bands,
        # except for half-bands at start and end (whose outer ends we now use).

        row = bok.keys()
        return self.decompose([ min(row) ] +
                              map(median, filter(None, parts[1:-1])) +
                              [ max(row) ])

    def decompose(self, new):
        # assert: new is sorted (but may have some repeated entries)
        count = len(new)
        for k in new: # flush out any repeats
            while new.count(k) > 1: new.remove(k)

        # Now flatten bok's distribution by `moving' each key to the nearest
        # entry in new.  Do this by going through bok.keys() in order, so we
        # always know which entries in new need considered as possible nearests:

        result, ind, run, cut = {}, 0, 0, new[0] + new[1]
        # ind: which member of new was nearest the last key
        # run: a running total for result[new[ind]]
        # cut: when 2 * key exceeds this, it's time to advance along new

        row = self.keys()
        row.sort()
        for key in row:
            while 2 * key > cut:
                result[new[ind]] = run
                run = result.get(new[ind], 0)
                try: cut = new[ind] + new[ind+1]
                except IndexError: cut = 3 * row[-1]        # ie huge

            run = run + bok[key]

        result[new[-1]] = run

        return Weighted(result, detail=count)

    def combine(self, dict, func, count=None):
        ans = Weighted({})
        for key, val in self.items():
            ans.add(dict, val,
                    lambda j, k=key, f=func: f(k,j))

        # that's generated our coarse distribution
        if count is None:
            try: det = dict.__detail
            except AttributeError: count = self.__detail
            else: count = max(det, self.__detail)
        return ans.condense(count)

def _defaulted(dict):
    """Fills in any None entries in a weight dictionary.

    Provided all positive values in the dictionary sum to less than 1, the
    difference between this sum and 1 will be shared out equally among the keys
    given with `weight' None. """

    
    try: vals = dict.values()
    except AttributeError:
        vals, rate, dict = dict, 1. / len(dict), {}
        for it in vals: dict[it] = rate
    if None not in vals: return dict

    spare, nuns = 1., 0
    for v in vals:
        if v is None: nuns = nuns + 1
        elif v > 0: spare = spare - v
        # leave the value < 0 case for during add(), when we know its key
    default = spare / nuns
    if default < 0:
        raise ValueError, 'Over-full dictionary with Nones needing filled in'

    ans = {}
    for k, v in dict.items():
        if v is None: ans[k] = default
        elif v: ans[k] = v

    return ans

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
    # fits between -1 and 1 (or even, between -.5 and .5, since the errors may
    # get doubled by the computation I just did).  Quite what to do if such
    # a number wants to be `integer-like' is another matter ...

    try: that = float(what)
    except AttributeError:

    return this / float(what)

class Sample(Value):
    """Primitive distribution modeller. """

    try: _lazy_preserve_ = Value._lazy_preserve_
    except AttributeError: _lazy_preserve_ = ()
    _lazy_preserve_ = tuple(_lazy_preserve_) + ( 'best', )

    def __init__(self, weights, best=None, *args, **what):
        apply(Value.__init__, (self,) + args, what)
        if best is not None: self.best = best

        self.__weigh = Weighted(_defaulted(weights))

    def copy(self):
        return apply(self.__class__, (self.__weigh.copy(),), self.__dict__)

    def update(self, other, weight=1, func=None):
        old = self.__weigh
        old.add(other, weight, func)
        old = old.condense()
        self.__weigh = old
        self._lazy_reset_()

    def normalise(self):
        sum = reduce(lambda x, y: x + y, self.__weigh.values(), 0.)
        if sum == 1: return
        if sum == 0: raise ZeroDivisionError, (
            'Attempted to normalise zero-sum mapping', self.__weigh)

        self.__weigh.scale(sum)

    # now define arithmetic
    def __bundle_(self, func, what):

	try: bok = what.__weigh
        except AttributeError: bok, best = { what: 1 }, what
	else: best = what.best

	return Sample(self.__weigh.combine(bok, func),
                      func(self.best, best))

    # Binary operators:
    def __add__(self, what): return self.__bundle_(lambda x, w: x+w, what)
    def __sub__(self, what): return self.__bundle_(lambda x, w: x-w, what)
    def __mul__(self, what): return self.__bundle_(lambda x, w: x*w, what)

    def __radd__(self, what): return self.__bundle_(lambda x, w: w+x, what)
    def __rsub__(self, what): return self.__bundle_(lambda x, w: w-x, what)
    def __rmul__(self, what): return self.__bundle_(lambda x, w: w*x, what)

    def __pow__(self, what): return self.__bundle_(_power, what)

    def __div__(self, what):
	try:
	    if what.low * what.high < 0:
		raise ZeroDivisionError, ('Dividing by interval about 0', self, what)
	except AttributeError:
	    if what == 0:
		raise ZeroDivisionError, ('Dividing by zero', self, what)

	return self.__bundle_(lambda x, w: x/w, what)

    def __rdiv__(self, what):
	if self.low * self.high < 0:
	    raise ZeroDivisionError, ('Dividing by interval about 0', self)

	return self.__bundle_(lambda x, w: w/x, what)

    # Other built-ins:
    def __nonzero__(self): return not(self.high == 0 == self.low)
    # NB: not(nonzero) is stricter than == 0.
    def __float__(self): return float(self.mean)
    def __long__(self): return long(self.median)
    def __int__(self): return int(self.median)
    def __neg__(self): return self._neg

    def _lazy_get__neg_(self, ignored):
	result = Sample(self.__weigh.map(lambda x: -x), -self.best)
	result._neg = self
	return result

    def _lazy_get_best_(self, ignored): return self.mean
    def _lazy_get_low_(self, ignored): return self.__weigh.low()
    def _lazy_get_high_(self, ignored): return self.__weigh.high()
    def _lazy_get_mean_(self, ignored): return self.__weigh.mean()
    def _lazy_get_mode_(self, ignored): return self.__weigh.mode()
    def _lazy_get_modes_(self, ignored): return self.__weigh.modes()
    def _lazy_get_median_(self, ignored): return self.__weigh.median()
    def _lazy_get_variance_(self, ignored): return self.__weigh.variance()


_rcs_id = """
  $Log: sample.py,v $
  Revision 1.1  1999-06-01 21:17:47  eddy
  Initial revision

"""
