"""The rationals. """

from natural import hcf

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
	except AttributeError:
	    return Rational(num + other * den, den)
	return Rational(num * q + p * den, den * q)

    __radd__ = __add__

    def __sub__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError:
	    return Rational(num - other * den, den)
	return Rational(num * q - p * den, den * q)

    def __mul__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError:
	    return Rational(num * other, den)
	return Rational(num * p, den * q)

    def __div__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError:
	    return Rational(num, den * other)
	return Rational(num * q, den * p)

    def __mod__(self, other):
	rat = self.__div__(other)
	return self - rat * other

    def __divmod__(self, other):
	rat = self.__div__(other)
	return rat, self - rat * other

    def __pow__(self, count):
	num, den = self._rational
	return Rational(pow(num, count), pow(den, count))

    def __nonzero__(self): return self._rational[0]
    def __cmp__(self, other):
	num, den = self._rational
	try: p, q = other._rational
	except AttributeError:
	    return cmp(num, den * other)
	return cmp(num * q, den * p)

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
	except: pass

	if num < rat * den: return rat - 1
	return rat

    asint = floor

    def ceil(self):
	num, den = self._rational
	rat = num / den
	try: rat = int(rat)
	except OverflowError: rat = long(rat)
	except: pass

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
def approximate(val, offer=None, toler=None, loquax=1>0):
    # pi is very close to 355 / 113: within 3e-7
    print '%9s %9s\t' % ('product', 'error'), 'Approximation'
    floor, frac = intsplitfrac(val)
    if toler == None: toler = 1.e-12 * max(abs(val), 1)

    if offer != None:
	best = offer
	denom = best.denominator
    else:
	try: best, denom = _previous[val]
	except KeyError:
	    best, denom = Rational(floor), 1

    while 1>0:
	try:
	    err = abs(val - best.asfloat())
	    print '%9.3g %8.2e\t' % (err * denom * denom, err), best
	    if err < toler: break

	    while 1>0:
		denom = 1 + denom
		numer, gap = intsplitfrac(frac * denom + .5)
		if abs(gap - .5) < err: break

	    best = Rational(numer, denom) + floor

	except OverflowError: denom = 0L + denom

	except Exception, what:
	    print what
	    print 'Aborted at denominator', denom, 'using', best
	    break

    global _previous
    _previous[val] = best, denom
    return best
