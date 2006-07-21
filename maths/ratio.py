"""The rationals.

See also:
http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
expounding the virtues of continued fractions.
"""

def _asint(val):
    try: return val.asint()
    except AttributeError: pass

    try: return int(val)
    except OverflowError: pass

    return long(val)

def intsplitfrac(val):
    res = _asint(val)
    while val > res + 1: res = 1 + res
    while val < res: res = res - 1
    return res, val - res

from natural import hcf

class Rational:
    def __init__(self, numer, denom=1):
	try: val = intsplitfrac(denom)[0]
	except: pass
	else:
            if val == denom: denom = val
	if denom == 0: raise ZeroDivisionError(numer, denom)

        try: val = intsplitfrac(numer)[0]
	except: pass
	else:
            if val == numer: numer = val

	common = hcf(numer, denom)
	if denom < 0: common = -common
	self._rational = (numer / common, denom / common)

    def __add__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return Rational(num * q + p * den, den * q)
	    except OverflowError: num, den = long(num), long(den)

    __radd__ = __add__

    def __sub__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return Rational(num * q - p * den, den * q)
	    except OverflowError: den, num = long(den), long(num)

    def __rsub__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return Rational(den * p - q * num, q * den)
	    except OverflowError: den, num = long(den), long(num)

    def __mul__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return Rational(num * p, den * q)
	    except OverflowError: den, num = long(den), long(num)

    __rmul__ = __mul__

    def __div__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while True:
	    try: return Rational(num * q, den * p)
	    except OverflowError: den, num = long(den), long(num)
    __truediv__ = __div__

    def __rdiv__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return Rational(p * den, q * num)
	    except OverflowError: den, num = long(den), long(num)
    __rtruediv__ = __rdiv__

    def __divmod__(self, other):
	rat = self.__div__(other)
	return rat, self - rat * other

    def __mod__(self, other):
	return self.__divmod__(other)[1]

    def __pow__(self, count):
	num, den = self._rational
	while 1>0:
	    try: return Rational(num**count, den**count)
	    except OverflowError: den, num = long(den), long(num)

    def __nonzero__(self): return self._rational[0]
    def __cmp__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError: p, q = other, 1
	while 1>0:
	    try: return cmp(num * q, den * p)
	    except OverflowError: den, num = long(den), long(num)

    def __hash__(self):
	num, den = self._rational
	return hash(num) ^ hash(den)

    def __str__(self):
	num, den = self._rational
	if den == 1: return `num`
	return `num` + ' / ' + `den`

    def __repr__(self):
	row = str(self)
	if not '.' in row: row = '1. * ' + row
	return '(' + row + ')'

    def floor(self):
	num, den = self._rational
	rat = num / den
	try: rat = int(rat)
	except OverflowError: rat = long(rat)

	if num < rat * den: return rat - 1
	return rat

    asint = floor

    def ceil(self):
	num, den = self._rational
	rat = num / den
	try: rat = int(rat)
	except OverflowError: rat = long(rat)

	if num > rat * den: return rat + 1
	return rat

    def asfloat(self):
	num, den = self._rational

	try: top = num.asfloat()
	except AttributeError: top = float(num)

	return top / den

    def __getattr__(self, key):
	try: return self._rational[{'denominator': 1, 'numerator': 0}[key]]
	except KeyError: raise AttributeError(self, key)

_previous = {}
def approximate(val, toler=None, loquax=1>0, assess=None):
    # pi is very close to 355 / 113: within 3e-7
    print '%9s %9s\t' % ('score', 'error'), 'Approximation to', val
    floor, frac = intsplitfrac(val)
    if toler is None: toler = 1.e-12 * max(abs(val), 1)

    global _previous
    try: best, denom = _previous[val]
    except KeyError: best, denom = Rational(floor), 1
    numer = 1

    while 1>0:
	try:
	    err = abs(val - best.asfloat())
            if assess: weigh = assess(denom) + assess(numer)
            else: weigh = denom + numer
	    print '%9.3g %9.2e\t' % (err * denom * weigh, err), best
	    if err < toler: break

	    while 1>0:
		denom = 1 + denom
		numer, gap = intsplitfrac(frac * denom + .5)
		if abs(gap - .5) < err: break

	    best = Rational(numer, denom) + floor

	except OverflowError: denom = 0L + denom

	except Exception, what:
	    if what.__class__.__module__ == 'exceptions':
		klaz = what.__class__.__name__
	    else: klaz = str(what.__class__)
	    print 'Exception:', klaz + `what.args`

	    print 'Aborted at denominator', denom, 'using', best
	    break
    else:
        print   # to match printing the exception that might break.

    _previous[val] = best, denom
    return best

def refine(val, offer=None):
    floor, frac = intsplitfrac(val)

    global _previous
    if offer is None:
	try: best = _previous[val][0]
	except KeyError:
	    best = Rational(floor), 1
    else:
        best = offer

    numer = best.numerator
    denom = best.denominator

    err = val - best.asfloat()
    count = intsplitfrac(1 / abs(err) + .5)[0]

    # val = err + numer / denom = err + (numer * count) / (denom * count)
    # val = (err * count + (numer * count / denom)) / count
    # and err * count is pretty close to 1 or -1.

    while 1>0:
	try: new = Rational(intsplitfrac(frac * denom * count + .5)[0],
			    denom * count) + floor
	except OverflowError: count = long(count)
	else: break

    gap = val - new.asfloat()
    if abs(gap) < abs(err):
	try: best, denom = _previous[val]
	except KeyError: pass
	else:
	    if abs(err) * denom * denom > abs(gap) * new.denominator * new.denominator:
		_previous[val] = new, new.denominator
	return new

    print 'Not as good: %g error from' % gap, new
