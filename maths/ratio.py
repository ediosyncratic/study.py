"""The rationals.

Exports:
  Rational(n, d) -- represents the ratio n / d without rounding artefacts
  approximate(val [, tol, chatty [, assess]]) -- approximate val with a Rational
  refine(val [, best]) -- improve on an earlier approximation to val

See also: study.maths.continued, compared to which this is crude and ugly.

$Id: ratio.py,v 1.16 2009-12-24 12:27:54 eddy Exp $
"""

def intsplitfrac(val):
    try:
	if val.imag == 0:
	    val = val.real
	    raise AttributeError
	v = val.real
    except AttributeError: v = val

    try: res = v.nearint
    except AttributeError:
	res = int(v)
	if res != v:
	    res = divmod(int(2 * v + 1), 2)[0]
	    while 2*(v - res) > 1: res += 1
	    while 2*(v - res) < -1: res -= 1
	    if res % 2 and 2 * (v - res) in (1, -1): # prefer even if at exact mid-point:
		res += cmp(v, res)

    return res, val - res

from study.cache.property import Cached, lazyprop

class Rational (Cached):
    def __init__(self, numer, denom=1):
        if isinstance(numer, Rational):
            numer, denom = numer.numerator, denom * numer.denominator
        if isinstance(denom, Rational):
            numer, denom = numer * denom.denominator, denom.numerator
	self.__ratio = self.__coprime(numer, denom)
	assert self.__ratio[1] > 0

    from natural import hcf
    def asint(v, isf=intsplitfrac): # tool function, not method
	try: a = isf(v)[0]
	except TypeError: pass
	else:
	    if a == v: return a
	return v

    @staticmethod
    def __coprime(n, d, clean=asint, gcd=hcf):
	n, d = clean(n), clean(d)
	i = gcd(n, d)
	if i * d < 0: i = -i
	return n/i, d/i

    del asint, hcf
    from continued import rationalize, real_continued
    __continue = rationalize, real_continued
    del rationalize, real_continued

    @classmethod
    def from_float(mode, val, tol=1e-9, depth=12):
	"""Approximates a real number.

	Required argument, val, is the real number to be approximated. Optional
	arguments tol and depth are as for rationalize(), q.v., but with
	defaults changed to 1e-9 and 12, respectively.  Raises ValueError
	precisely if rationalize() does, due to being unable to find an adequate
	approximation; otherwise, returns a Rational object approximating
	val.\n"""

	n, d = mode.__continue[0](val, tol, depth)
	return mode(n, d)

    @classmethod
    def approach(mode, val):
        """Returns an interator over Rational approximations to val.

        Single argument, val, is a number, presumed real.  At each point in its
        continued fraction, the remainder is discarded and the truncated form is
        used to produce a Rational approximation to val.  The error in this
        approximation is saved as a .error attribute on each value yielded.\n"""

        seq, ctd = [], mode.__continue[1](val)
        while True:
            seq.append(ctd.next()) # We're done if it raises StopIteration :-)
            i, n, d = len(seq), 1, 0
            while i > 0:
                i -= 1
                # replace n/d with seq[i] + d / n = (n*seq[i] + d) / n
                n, d = n * seq[i] + d, n
            ans = mode(n, d)
            ans.error = float(ans) - val
            yield ans

    @property
    def denominator(self, ig=None): return self.__ratio[1]
    @property
    def numerator(self, ig=None): return self.__ratio[0]

    @lazyprop
    def floor(self, ig=None): # round down (towards -infinity)
	num, den = self.__ratio
	rat = int(num // den)
	assert num >= rat * den
	return rat

    @lazyprop
    def ceil(self, ig=None): # round up (towards +infinity)
	num, den = self.__ratio
	rat = int(num / den)
	if num > rat * den: return rat + 1
	return rat

    @lazyprop
    def nearint(self, ig=None): # round to nearest int, preferring even when ambiguous
	num, den = self.__ratio
	q = int(divmod(2 * num + den, 2 * den)[0])
	while 2 * (num - q * den) >  den: q += 1
	while 2 * (num - q * den) < -den: q -= 1
	r = num - q * den
	if q % 2 and 2 * r in (den, -den): q += cmp(r, 0)
	return q

    @lazyprop
    def truncate(self, ig=None): # round towards zero
	num, den = self.__ratio
	if num > 0: return int(num / den)
	return -int(-num / den)

    @lazyprop
    def real(self, ig=None):
	num, den = self.__ratio
	return float(num) / den

    @classmethod
    def __rational__(mode, num, den):
        return mode(num, den)

    def __nonzero__(self): return self.__ratio[0] != 0
    def __pos__(self): return self
    def __neg__(self):
	num, den = self.__ratio
	return self.__rational__(-num, den)
    def __abs__(self):
	num, den = self.__ratio
	return self.__rational__(abs(num), den)

    def __long__(self):    return long(self.truncate)
    def __int__(self):     return self.truncate
    def __complex__(self): return self.real + 0j
    def __float__(self):   return self.real

    def __add__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(num * q + p * den, den * q)

    __radd__ = __add__

    def __sub__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(num * q - p * den, den * q)

    def __rsub__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(den * p - q * num, q * den)

    def __mul__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(num * p, den * q)

    __rmul__ = __mul__

    def __truediv__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(num * q, den * p)
    __div__ = __truediv__

    def __rtruediv__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
	return self.__rational__(p * den, q * num)
    __rdiv__ = __rtruediv__

    def __floordiv__(self, other): return self.__truediv(other).floor
    def __mod__(self, other): self - self.__floordiv__(other) * other
    def __divmod__(self, other):
	rat = self.__floordiv__(other)
	return rat, self - rat * other

    def __pow__(self, count, mod=None):
	num, den = self.__ratio
        ans = self.__rational__(num**count, den**count)
	if mod is None: return ans
	return ans % mod

    def __cmp__(self, other):
	num, den = self.__ratio
	try: p, q = other.__ratio
	except AttributeError: p, q = other, 1
        assert den > 0 and q > 0, 'else sort order messed up'
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

del Cached, lazyprop

# TODO: re-work the following to exploit continued.rationalize().
prior = {}
def approximate(val, tol=None, chatty=False, assess=abs, old=prior, isf=intsplitfrac):
    """Brute-force approximation.

    Required argument, val, is a numeric value to be approximated by a
    rational.  Complex values probably won't work.  Optional arguments (details
    below):
      tol -- None (default) or an error tolerance.
      chatty -- whether to report findings (default: False)
      assess -- cost function (default: abs), ignored unless chatty
    Do not pass any further arguments.  When tol is None (or unspecified) the
    computed value 1e-12 * max(abs(val), 1) is used for it.

    The return from this function is always recorded in a cache (shared with
    refine(), q.v.); if val has been studied previously, we pick up where we
    left off.  Otherwise, we start with the nearest whole number to val, using
    denominator 1.  We then search, with progressively higher denominators, for
    rational approximations to val.  When an improved approximation is found it
    is optionally reported.  If an approximation is found that comes within tol
    of val, it is returned.  If any exception (notably including interrupt,
    since this function may run indefinitely) is caught, it is reported
    (regardless of chatty) and the current best approximation is returned.

    If chatty is true, this function also produces output describing the
    approximations found.  The output may be partly customized using asses: this
    function is called on the numerator and denominator; the results are added;
    this sum is scaled by denominator times the absolute error and the product
    is used as a 'score' for this approximation, which is output along with the
    error itself and the approximation.\n"""

    # pi is very close to 355 / 113: within 3e-7
    if chatty: print '%9s %9s\t' % ('score', 'error'), 'Approximation to', val
    floor, frac = isf(val)
    if tol is None: tol = 1e-12 * max(abs(val), 1)

    try: best, denom = old[val]
    except KeyError: best, denom = Rational(floor), 1
    numer = 1

    try:
        while True:
	    err = abs(val - float(best))
            weigh = assess(denom) + assess(numer)
	    if chatty: print '%9.3g %9.2e\t' % (err * denom * weigh, err), best
	    if err < tol: break

	    while True:
		denom = 1 + denom
		numer, gap = isf(frac * denom)
		if abs(gap) < err: break

	    best = Rational(numer, denom) + floor

    except Exception, what:
        if what.__class__.__module__ == 'exceptions':
            klaz = what.__class__.__name__
        else: klaz = str(what.__class__)
        print 'Caught:', klaz + `what.args`, 'in study.maths.ratio.approximate()'

	if chatty: print 'Aborted at denominator', denom, 'using', best

    old[val] = best, denom
    return best

def refine(val, best=None, old=prior, isf=intsplitfrac):
    floor, frac = isf(val)

    if best is None:
	try: best = old[val][0]
	except KeyError:
	    best = Rational(floor)

    numer = best.numerator
    denom = best.denominator

    err = val - float(best)
    count = isf(1 / abs(err))[0]

    # val = err + numer / denom = err + (numer * count) / (denom * count)
    # val = (err * count + (numer * count / denom)) / count
    # and err * count is pretty close to 1 or -1.

    new = Rational(isf(frac * denom * count)[0],
                   denom * count) + floor

    gap = val - float(new)
    if abs(gap) < abs(err):
	try: best, denom = old[val]
	except KeyError: pass
	else:
	    if abs(err) * denom * denom > abs(gap) * new.denominator * new.denominator:
		old[val] = new, new.denominator
	return new

    print 'Not as good: %g error from' % gap, new

del prior, intsplitfrac

from study.value.quantity import Quantity

golden = Quantity((1 + 5.**.5) / 2,
                  doc = """The golden ratio.

This is the positive solution to the quadratic equation x*x = x+1; divide -1 by
it to get the negative solution.  One can re-write the equation as (2*x-1)**2 =
4*x*x -4*x +1 = 4*(x*x-x) + 1 = 5, whence the solutions are (1 +/- 5**.5)/2.
""")

assert golden**2 == golden+1

del Quantity
