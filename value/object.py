"""Objects to describe values with namespaces.

$Id: object.py,v 1.1 1999-01-24 22:34:32 eddy Exp $
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

class Numeric(Value):
    def __init__(self, base, tolerance=0, *args, **what):
	apply(Value.__init__, (self,) + args, what)

	if tolerance < 0: tolerance = -tolerance

	if isinstance(base, Numeric):
	    tol = base.__tolerance
	    if tol > tolerance: tolerance = tol
	    base = base.__scale

	if isinstance(tolerance, Numeric):
	    tolerance = tolerance.__scale + tolerance.__tolerance

	self.__scale, self.__tolerance = base, tolerance

    def decade(self, stride, base=None, top=None):
	me, tol, it = abs(self.__scale), self.__tolerance, abs(stride)

	if stride < 0: stride = -stride
	if stride < 1:
	    stride = 1 / stride
	    sign = -1
	else: sign = 1

	if base:
	    if base < 0: base = -base
	    top = base * stride
	elif top: base = float(top) / stride
	else: base = 1

	count = 0

	while top <= me + tol:
	    me, tol, count = me / it, tol / it,  count + 1

	while base > me:
	    me, count = me * it, count - 1

	# me * pow(stride, count) == self,
	# base <= me < top

	return sign * count

    # Only an exact zero is truly zero:
    def __nonzero__(self): return not(self.__scale == 0 == self.__tolerance)
    def __float__(self): return float(self.__scale)
    def __long__(self): return long(self.__scale)
    def __int__(self): return int(self.__scale)
    def __neg__(self): return Numeric( - self.__scale, self.__tolerance)
    def __abs__(self):
	if self.__scale < 0: return - self
	return self

    def __unpack_(self, what):
	if isinstance(what, Numeric):
	    return what.__scale, what.__tolerance
	return what, 0

    def __cmp__(self, what):
	what, tol = self.__unpack_(what)
	tol = max(self.__tolerance, tol)
	diff = self.__scale - what
	if tol < diff: return -1
	if diff < -tol: return 1
	return 0

    def __add__(self, what):
	what, tol = self.__unpack_(what)
	tol = tol + self.__tolerance
	return Numeric(self.__scale + what, tol)
    __radd__ = __add__

    def __sub__(self, what):
	what, tol = self.__unpack_(what)
	tol = tol + self.__tolerance
	return Numeric(self.__scale - what, tol)

    def __rsub__(self, what):
	what, tol = self.__unpack_(what)
	tol = tol + self.__tolerance
	return Numeric(what - self.__scale, tol)

    def __row_(self, func):
	base, tol = self.__scale, self.__tolerance
	lo = hi = mid = func(base)
	if tol:
	    try: row = ( base - tol, base + tol )
	    except TypeError, what:
		raise TypeError, what.args + (base, tol)
	    for v in row:
		q = func(v)
		if q < lo: lo = q
		if q > hi: hi = q

	return lo, mid, hi

    def __grid_(self, func, base, tol):
	lo, mid, hi = func(base)
	if tol:
	    for m in base - tol, base + tol:
		row = func(m)
		for v in row:
		    if v < lo: lo = v
		    if v > hi: hi = v

	return lo, mid, hi

    def __mul__(self, what):
	what, tol = self.__unpack_(what)
	mine, mol = self.__scale, self.__tolerance
	lo, mid, hi = self.__grid_(lambda w, s=self: s.__row_(lambda v, x=w: v * x),
				   what, tol)
	return Numeric(mid, max(hi - mid, mid - lo))

    def __rmul__(self, what):
	what, tol = self.__unpack_(what)
	mine, mol = self.__scale, self.__tolerance
	lo, mid, hi = self.__grid_(lambda w, s=self: s.__row_(lambda v, x=w: x * v),
				   what, tol)
	return Numeric(mid, max(hi - mid, mid - lo))

    def __div__(self, what):
	what, tol = self.__unpack_(what)
	mine, mol = self.__scale, self.__tolerance
	lo, mid, hi = self.__grid_(lambda w, s=self: s.__row_(lambda v,x=w: v / x),
				   what, tol)
	return Numeric(mid, max(hi - mid, mid - lo))

    def __rdiv__(self, what):
	what, tol = self.__unpack_(what)
	mine, mol = self.__scale, self.__tolerance
	lo, mid, hi = self.__grid_(lambda w, s=self: s.__row_(lambda v,x=w: x / v),
				   what, tol)
	return Numeric(mid, max(hi - mid, mid - lo))

    def __pow__(self, what):
	try: scale = pow(self.__scale, what)
	except OverflowError: scale = pow(float(self._scale), what)
	return Numeric(scale, what * scale * self.__tolerance / self.__scale)

    def __repr__(self):
	return self._repr

    def __str__(self):
	return self._str

    def _lazy_get__repr_(self, ignored):
	val, tol = self.__scale, self.__tolerance
	if type(val) != FloatType and tol < .1:
	    # have a go at rendering it as an integer
	    try: iscale = int(val)
	    except OverflowError: iscale = long(val)
	    if val == iscale: return `iscale`

	result = `val`
	if tol:
	    lo, mi, hi = `val - tol`, result, `val + tol`
	    ind = 0
	    stop = max(len(lo), len(hi), len(mi))
	    while ind < stop and (lo[ind] == mi[ind] or mi[ind] == hi[ind]):
		ind = ind + 1

	    if ind + 1 < stop:
		ind = ind + 1
	    
		if 'e' in result:
		    end = string.index(result, 'e')
		    if ind > end: ind = end
		    tail = result[end:]
		    result = result[:end]
		else: tail = ''

		if '.' not in result[:ind] and '.' in result:
		    ind = string.index(result, '.')

		result = result[:ind] + tail

	return result

    def _lazy_get__str_(self, ignored):
	if type(self.__scale) != FloatType: return self._repr
	off = 3 * self.decade(1000, .1)	# self / pow(10, off) is between .1 and 100.
	val, tol = self.__scale, self.__tolerance

	low, mid, hie = map(lambda x, s=pow(.1, off): `x * s`,
			    (val - tol, val, val + tol))
	run = min(len(low), len(mid), len(hie))
	ind = 0
	while ind < run and (low[ind] == mid[ind] or mid[ind] == hie[ind]):
	    ind = ind + 1

	return mid[:ind] + 'e%+d' % off

    # Things which compare equal must have equal hash values.
    # Consider Numeric(n, 10) for integer n: enough of these compare equal
    # that they must all have equal hash values ... and every Numeric is equal
    # to at least several of them.
    _lazy_hash = hash('Numeric') ^ hash(Value)

"""
 $Log: object.py,v $
 Revision 1.1  1999-01-24 22:34:32  eddy
 Initial revision

"""
