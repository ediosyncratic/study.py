"""Objects to describe values with namespaces.

$Id: object.py,v 1.2 1999-02-21 01:31:57 eddy Exp $
"""

from types import FloatType
from basEddy.lazy import Lazy

class Value(Lazy):
    def copy(self): apply(self.__class__, (), self.__dict__)
    def also(self, **what): self.__dict__.update(what)
    def __init__(self, *lookups, **what):
	Lazy.__init__(self)
	self.__dict__.update(what)
	row = list(lookups) + [ self._lazy_lookup_ ]
	self.borrow = lambda where, _row = row: _row.append(where)

	def getit(key, _row = row):
	    for item in _row:
		try: return item(key)
		except AttributeError: pass
	    raise AttributeError, key
	self._lazy_lookup_ = getit


def _bchop(row, val):	# no longer needed ! (but pretty anyway ;^)
    """Uses binary chop to find where val belongs in the ordered row. """
    top, bot = len(row), 0
    while top > bot + 1:
	mid = (top + bot) / 2
	# assert: top > mid > bot
	if row[mid] <= val: bot = mid
	if row[mid] >= val: top = mid
	# assert: top - bot diminished

    return top	# a valid index at which to insert val.

def median(row, func=None):
    """Returns the medianof a sequence of values.

    Required argument, row, is a non-empty sequence of values supporting
    comparison.  A `middle member' of this will be returned.

    Optional argument, func, is a callable which, given a sorted copy of the
    row, returns a value of the same kind as the entries in the list.  It will
    only be called if (given and) the row is of even length with distinct middle
    members (once sorted).

    Strictly, a row of even length might have no median: it has two middle
    entries.  If these are equal, the row has a median: otherwise, we need to
    chose between them.  By default, somewhat arbitrarily, median() returns the
    higher of the two.  However, if func is provided, the middle entry closest
    to its output is used.  (Again somewhat arbitrarily, if they are equally
    close, the higher one is used.)

    For example, passing a func which will return the mean of row (say) will
    bias the median choice to whichever of the middle two entries is closer to
    the mean.  To override the default high-bias with a matching low-bias, use
    lambda r: r[0] as func: the default is effectively lambda r: r[-1].

    Note that the value returned is *always* an entry drawn from the list whose
    median was sought: if this list is empty, a ValueError is raised. """

    if not row: raise ValueError, 'Cannot take median of empty row'
    row, n = list(row), len(row)
    row.sort()

    if n % 2: return row[n/2]

    # Even length list: grab the middle pair.
    n = (n-1)/2
    lo, hi = row[n:n+2]
    # assert: lo <= hi

    if lo < hi and func and func(row) * 2 < lo + hi: return lo

    return hi

def _mean(row):
    if row: return reduce(lambda x,y: x+y, row, 0) / len(row)
    else: raise ValueError, 'Taking mean of empty sample'

def _medial(row):
    """Returns a short row with the same min, max and median as the given."""
    if len(row) < 4: return row
    # lists of length > 3: keep only three elements.
    return [ min(row), median(row), max(row) ]

def _power(this, what):
    try: return pow(this, what)
    except OverflowError: return pow(float(this), what)

def _common_substr(given):
    def common(s, r=given, n=len(given)):
	m = min(len(s), n)
	different = filter(lambda i, a=s,b=r: a[i] != b[i],
			   range(m))
	if different: return min(different)
	return m
    # So common takes a string and returns the first index at
    # which that string differs from given.
    return common

def _majority_length(given, jury):
    # how far down given do most of jury agree with it ?
    return median(map(_common_substr(given), jury))

class Sample(Value):
    def __init__(self, sample=(), mean=None, *args, **what):
	apply(Value.__init__, (self,) + args, what)

	if isinstance(sample, Sample): sample = sample.__known
	else:
	    try: sample[:]
	    except AttributeError: sample = [sample,]
	    else: sample = list(sample)

	self.__known = sample
	if mean: self.mean = mean

    # Internal utilities for the binary operators:
    def __row_(self, func=None, seq=None):
	if seq == None: seq = self.__known
	if func: return _medial(map(func, seq))
	return seq

    def __grid_(self, func, seq=None):
	if seq == None: seq = self.__known
	return _medial(reduce(lambda x,y: x+y, map(func, seq), []))

    def __bundle_(self, func, what):
	def rower(w, r=self.__row_, f=func):
	    return r(lambda x,m=w,g=f: g(x,m))

	try: row = self.__grid_(rower, what.__known)
	except AttributeError:
	    row = rower(what)
	    best = what
	else: best = what.mean

	return Sample(row, mean=func(self.mean, best))

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
    def __repr__(self): return self._repr
    def __str__(self): return self._str

    def __abs__(self, what):
	if self.median > 0: return self
	return self._neg

    def __cmp__(self, what):
	if self.high < what: return -1
	if self.low > what: return 1
	return 0

    # Internal lazies for neg/repr/str:
    def _lazy_get__neg_(self, ignored):
	result = Sample(map(lambda x: -x, self.__known), -self.mean)
	result._neg = self
	return result

    def _lazy_get__repr_(self, ignored):
	val = self.mean
	if type(val) != FloatType and self.spread < .1:
	    # have a go at rendering it as an integer
	    try: iscale = int(val)
	    except OverflowError: iscale = long(val)
	    if val == iscale: return `iscale`

	result = `val`

	if '.' not in result: return result
	import string

	if 'e' in result:
	    end = string.index(result, 'e')
	    result, tail = result[:end], result[end:]
	    if '.' not in result: return result + tail

	else: tail = ''

	sample = map(repr, self.__known)
	while result in sample: sample.remove(result)
	ind = _majority_length(result, sample)
	# So most sample strings agree with result at least as far as ind

	# Don't truncate sooner than the decimal point ...
	if '.' not in result[:ind]:
	    ind = string.index(result, '.')

	return result[:ind] + tail

    def _lazy_get__str_(self, ignored):
	if type(self.mean) != FloatType: return self._repr
	off = 3 * self.decade(1000, .1)	# self / pow(10, off) is between .1 and 100.
	scale = pow(.1, off)
	result = '%f' % (self.mean * scale)
	# Assert: '.' in result
	while result[-1] == '0': result = result[:-1]

	sample = map(lambda x, s=scale: '%f' % (x * s),
		     self.__known)
	ind = _majority_length(result, sample)

	if '.' not in result[:ind]:
	    import string
	    ind = string.index(result, '.')

	return result[:ind] + 'e%+d' % off

    # Lazy namespace: sample, width, high, mean, low, mid, spread, errors.
    def _lazy_get_sample_(self, ignored): return _medial(self.__known)
    def _lazy_get_width_(self, ignored): return self.high - self.low
    def _lazy_get_high_(self, ignored): return max(self.__known)
    def _lazy_get_mean_(self, ignored): return _mean(self.__known)
    def _lazy_get_low_(self, ignored): return min(self.__known)

    def _lazy_get_median_(self, ignored):
	return median(self.__known,
		      # offer self.mean as extra value, if needed.
		      lambda r, s=self: s.mean)	# (ignores r)

    def _lazy_get_spread_(self, ignored):
	return max(abs(self.errors.low), abs(self.errors.high))
    def _lazy_get_errors_(self, ignored):
	return Sample(map(lambda x, m=self.mean: x-m, self.__known))

    def observe(self, *vals, **stuff):
	"""Returns a fresh sample characterised by the given values and self.

	Thus, given self as a record of measurement of some physical quantity,
		new = self.observe(3.14, 3.14159, 3.1415927, mean=pi)
	will set new to a record of today's `more trusted' data, using today's
	average as best estimate, but including both today's data and all prior
	data from self. """

	return apply(Sample, self.__known + list(vals), stuff)

    def decade(self, stride=None, base=None, top=None):
	"""Returns the power of stride needed to bring self into some standard interval.

	Arguments are optional:

	  stride -- ratio of base to top.
	  base -- start-point of interval.
	  top -- end-point of interval.

	If the value returned is D, say, then self * power(D, scale) will be at
	least as big as the smaller of base and top; unless forced by that, it
	will also be no bigger than the bigger of the two. """

	hi, lo = map(abs, (self.high, self.low))

	if stride: it = abs(stride)
	elif base and top: it = abs(float(top) / base)
	else: it = 10

	if it < 1:
	    it = 1. / it	# if that overflows, stride is 0: dumb !
	    sign = -1
	elif it > 1: sign = 1
	else:
	    raise ValueError, 'Cannot change anything by scaling by 1'

	if top and top < 0: top = -top

	# Coerce range so 0 < base * it = top
	if base:
	    if base < 0: base = -base
	    if not top: top = base * it
	    elif top < base: top, base = base, top
	elif top: base = float(top) / it
	else: base, top = 1, it

	# Compute the power of stride by which to multiply self to fall between
	# base and top.
	count = 0
	while hi > top:
	    lo, hi, count = lo / it, hi / it,  count + 1
	while lo < base:
	    lo, count = lo * it, count - 1

	return sign * count

    # Things which compare equal must have equal hash values.
    # Consider Numeric(n, 10) for integer n: enough of these compare equal that
    # they must all have equal hash values ... and every Numeric is equal to at
    # least several of them.  So all Numerics must have the same hash value.

    _lazy_hash = hash('Sample') ^ hash(Value) ^ hash(__name__)

def Numeric(base, tolerance=0, sample=(), *args, **what):

    if tolerance < 0: tolerance = - tolerance
    if isinstance(tolerance, Sample):
	tolerance = tolerance.high

    if isinstance(base, Sample):
	tolerance = tolerance + base.spread
	err = base.errors
	base = base.mean
    else: err = 0

    if isinstance(sample, Sample): sample = (sample + err).sample
    else: sample = list(sample)

    if base not in sample: sample.append(base)
    if tolerance:
	for v in base - tolerance, base + tolerance:
	    if v not in sample: sample.append(v)

    sample.sort()
    what['sample'] = sample
    what['mean'] = base
    return apply(Sample, args, what)

"""
 $Log: object.py,v $
 Revision 1.2  1999-02-21 01:31:57  eddy
 Revolution.

 Initial Revision 1.1  1999/01/24 22:34:32  eddy
"""
