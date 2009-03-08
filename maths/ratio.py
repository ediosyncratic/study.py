"""The rationals.

See also:
http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
expounding the virtues of continued fractions.

$Id: ratio.py,v 1.11 2009-03-08 09:49:12 eddy Exp $
"""

def intsplitfrac(val):
    try:
	if val.imag == 0:
	    val = val.real
	    raise AttributeError
	v = val.real
    except AttributeError: v = val

    try: res = v.asint()
    except AttributeError: res = int(v)
    while v > res + 1: res = 1 + res
    while v < res: res = res - 1

    return res, val - res

from natural import hcf

class Rational:
    def __init__(self, numer, denom=1):
	try: val = intsplitfrac(denom)[0]
	except TypeError: pass
	else:
            if val == denom: denom = val
	if denom == 0: raise ZeroDivisionError(numer, denom)

        try: val = intsplitfrac(numer)[0]
	except TypeError: pass
	else:
            if val == numer: numer = val

	common = hcf(numer, denom)
	if denom < 0: common = -common
	self.__ratio = (numer / common, denom / common)

    def __add__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(num * q + p * den, den * q)

    __radd__ = __add__

    def __sub__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(num * q - p * den, den * q)

    def __rsub__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(den * p - q * num, q * den)

    def __mul__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(num * p, den * q)

    __rmul__ = __mul__

    def __div__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(num * q, den * p)
    __truediv__ = __div__

    def __rdiv__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return Rational(p * den, q * num)
    __rtruediv__ = __rdiv__

    def __divmod__(self, other):
	rat = self.__div__(other)
	return rat, self - rat * other

    def __mod__(self, other):
	return self.__divmod__(other)[1]

    def __pow__(self, count): # third arg, if given, would request modular arithmetic
	num, den = self.__ratio
        return Rational(num**count, den**count)

    def __nonzero__(self): return self.__ratio[0]
    def __cmp__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return cmp(num * q, den * p)

    def __hash__(self):
	num, den = self.__ratio
	return hash(num) ^ hash(den)

    def __str__(self):
	num, den = self.__ratio
	if den == 1: return str(num)
	return '%s / %s' % (num, den)

    def __repr__(self):
	num, den = self.__ratio
	if den == 1: return `num`
        if num == 1: return '1. / ' + `den`
        num, den = `num`, `den`
        if '.' in num or '.' in den: return num + ' / ' + den
        elif num[-1].upper() != 'L': return num + '. / ' + den
        elif den[-1].upper() != 'L': return num + ' / ' + den + '.'
        else: return num + ' * 1. / ' + den

    def floor(self):
	num, den = self.__ratio
	rat = int(num / den)

	if num < rat * den: return rat - 1
	return rat

    asint = floor

    def ceil(self):
	num, den = self.__ratio
	rat = int(num / den)

	if num > rat * den: return rat + 1
	return rat

    def asfloat(self):
	num, den = self.__ratio

	try: top = num.asfloat()
	except AttributeError: top = float(num)

	return top / den

    def __getattr__(self, key, bok={'denominator': 1, 'numerator': 0}):
	try: return self.__ratio[bok[key]]
	except KeyError:
            raise AttributeError(self, key)

def rationalize(x, tol=1e-7, depth=5):
    """Find rational approximation to a real.

    Required parameter, x, is a real number; complex is not handled and there is
    no point doing this to integers.  Optional arguments are:
      tol -- error tolerance (default: 1e-7)
      depth -- depth of search (default: 5), see below.
    Returns a twople n, d of integers for which n = d * x, give or take errors
    of order tol, raising ValueError if unable to find suitable n and d.

    Result is sought by using continued fractions; that is, by first trying to
    approximate x as s[0] + 1./(s[1] + 1./(s[2] + ...)) for some sequence s of
    integers, then unwinding this expression to obtain n and d.  The search
    aborts if it needs more than depth entries in s to get within tol of x.

    For more on the theory of continued fractions, see
    http://en.wikipedia.org/wiki/Continued_fraction\n"""

    seq, r = [], x
    while len(seq) < depth:
	q, r = divmod(r, 1)
	if r > .5: q, r = q+1, r-1
	seq.append(int(q))
	assert q == seq[-1], 'divmod(,1)[0] should be whole !'
	if abs(r) < tol: break
	tol *= (abs(r) + tol)**2
	r = 1. / r
    else:
	raise ValueError('Hard to approximate', x)

    # x == seq[0] + 1/(seq[1] + 1/(...))
    n, d = seq.pop(), 1
    while seq: n, d = d + n * seq.pop(), n
    return n, d

# TODO: re-work the following to exploit rationalize().
prior = {}
def approximate(val, toler=None, assess=None, old=prior):
    # pi is very close to 355 / 113: within 3e-7
    print '%9s %9s\t' % ('score', 'error'), 'Approximation to', val
    floor, frac = intsplitfrac(val)
    if toler is None: toler = 1.e-12 * max(abs(val), 1)

    try: best, denom = old[val]
    except KeyError: best, denom = Rational(floor), 1
    numer = 1

    try:
        while True:
	    err = abs(val - best.asfloat())
            if assess: weigh = assess(denom) + assess(numer)
            else: weigh = denom + numer
	    print '%9.3g %9.2e\t' % (err * denom * weigh, err), best
	    if err < toler: break

	    while True:
		denom = 1 + denom
		numer, gap = intsplitfrac(frac * denom + .5)
		if abs(gap - .5) < err: break

	    best = Rational(numer, denom) + floor

        else: print   # to match printing the exception that might break.
    except Exception, what:
        if what.__class__.__module__ == 'exceptions':
            klaz = what.__class__.__name__
        else: klaz = str(what.__class__)
        print 'Exception:', klaz + `what.args`

        print 'Aborted at denominator', denom, 'using', best

    old[val] = best, denom
    return best

def refine(val, best=None, old=prior):
    floor, frac = intsplitfrac(val)

    if best is None:
	try: best = old[val][0]
	except KeyError:
	    best = Rational(floor)

    numer = best.numerator
    denom = best.denominator

    err = val - best.asfloat()
    count = intsplitfrac(1 / abs(err) + .5)[0]

    # val = err + numer / denom = err + (numer * count) / (denom * count)
    # val = (err * count + (numer * count / denom)) / count
    # and err * count is pretty close to 1 or -1.

    new = Rational(intsplitfrac(frac * denom * count + .5)[0],
                   denom * count) + floor

    gap = val - new.asfloat()
    if abs(gap) < abs(err):
	try: best, denom = old[val]
	except KeyError: pass
	else:
	    if abs(err) * denom * denom > abs(gap) * new.denominator * new.denominator:
		old[val] = new, new.denominator
	return new

    print 'Not as good: %g error from' % gap, new

del prior
