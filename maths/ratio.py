"""The rationals.

Exports:
  Rational(n, d) -- represents the ratio n / d without rounding artefacts
  approximate(val [, tol, chatty [, assess]]) -- approximate val with a Rational
  refine(val [, best]) -- improve on an earlier approximation to val

See also: study.maths.continued, compared to which this is crude and ugly.
See study.LICENSE for copyright and license information.
"""

def intsplitfrac(val): # tool function, not exported
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
    def from_float(cls, val, tol=1e-9, depth=12):
        """Approximates a real number.

        Required argument, val, is the real number to be approximated. Optional
        arguments tol and depth are as for continued.rationalize(), q.v., but
        with defaults changed to 1e-9 and 12, respectively.  Raises ValueError
        precisely if rationalize() does, due to being unable to find an adequate
        approximation; otherwise, returns a Rational object approximating val.
        See also: approach().\n"""

        n, d = cls.__continue[0](val, tol, depth)
        return cls(n, d)

    @classmethod
    def approach(cls, val):
        """Returns an interator over Rational approximations to val.

        Single argument, val, is a number, presumed real.  At each point in its
        continued fraction, the remainder is discarded and the truncated form is
        used to produce a Rational approximation to val.  The error in this
        approximation is saved as a .error attribute on each value yielded.\n"""

        seq, ctd = [], cls.__continue[1](val)
        while True:
            seq.append(ctd.next()) # We're done if it raises StopIteration :-)
            i, n, d = len(seq), 1, 0
            while i > 0:
                i -= 1
                # replace n/d with seq[i] + d / n = (n*seq[i] + d) / n
                n, d = n * seq[i] + d, n
            yield cls.__approximator(n, d, val)

    @classmethod
    def Farey(cls, val):
        """Use Farey's algorithm to approximate a value.

        Given d, e > 0, let ~ denote the ordering relation between n/d
        and m/e, as in: n/d ~ m/e.  This implies e.n ~ d.m which, in
        turn, implies (adding d.n to both sides in one case, e.m in
        the other):

          * (d +e).n ~ d.(n +m) and e.(n +m) ~ (d +e).m

        which together imply n/d ~ (n +m)/(d +e) ~ m/e.
        Thus (n +m) / (d +e) lies between n/d and m/e.

        So we start with val bounded between two whole numbers and repeatedly
        replace one of the bounds with Farey's mid-point between our bounds,
        stopping if we ever hit val exactly.  At each step, this yields the
        bound closest to val, with a .error attribute set to the difference
        between it and val.  In the event of a tie (val is exactly half way
        between the bounds), we take a short-cut and use their mid-point.\n"""

        v = int(val)
        lo = cls.__approximator(v, 1, val)
        hi = cls.__approximator(v + 1, 1, val)
        while True:
            assert lo <= val <= hi and hi.error >= 0 and lo.error <= 0
            y, mid = cls.__stepFarey(val, lo, hi)
            if y is not None: # The short-cut.
                yield y

            if mid.error > 0: hi = mid
            elif mid.error < 0: lo = mid
            else: # error is either NaN (give up) or 0 (done)
                yield mid
                break

            if y is None:
                # We took the short-cut and it wasn't exact after all:
                yield mid

    @classmethod
    def __stepFarey(cls, val, lo, hi):
        # See Farey()
        if hi.error > -lo.error:
            y = lo
        elif hi.error < -lo.error:
            y = hi
        else:
            mid = (lo + hi) / 2
            mid.error = float(mid) - val
            return None, mid

        n, d = lo.__ratio
        m, e = hi.__ratio
        return y, cls.__approximator(n + m, d + e, val)

    @classmethod
    def __approximator(cls, num, den, val):
        ans = cls(num, den)
        ans.error = float(ans) - val
        return ans

    @property
    def denominator(self): return self.__ratio[1]
    @property
    def numerator(self): return self.__ratio[0]

    def hypot(self, *args):
        """Returns sqare-rooted sum of squares.

        Takes arbitrarily many arguments, assumed Rational.  Adds
        self's square to the sum of their squares and returns the
        sum's square root.  Unlike math.hypot(), this does sum the
        actual squares, since doing that with Rational instances
        suffers no loss of precision; so passing any values, such as
        float, where loss of precision may be an issue, is apt to
        produce poor results.  The return is a Rational if possible,
        otherwise a float.\n"""
        total = self * self
        for arg in args:
            total += arg * arg
        assert total >= 0
        try:
            if isinstance(total, Rational):
                return total.__sqrt(10**17)
        except ValueError as what:
            pass # Make do with float's approximation
        return float(total) ** .5

    @property
    def sqrt(self):
        return self.__sqrt()

    from study.maths.natural import desquare
    from study.maths.primes import factorise
    def __sqrt(self, invTol=None, root = desquare, factor = factorise):
        """The square root of self.

        Optional parameter invTol defaults to None; otherwise, it should be a
        (typically large) positive number.  When invTol is None, this raises
        ValueError if self is not the square of a rational.  Otherwise, it
        returns a rational whose square differs from self by at most
        self/invTol; it will be exact if this is possible.\n"""
        if self < 0:
            raise ValueError("mathematical domain error", self)
        num, den = self.__ratio
        d = 1
        for k, v in factor(den).items():
            q, r = divmod(v, 2)
            if r: num *= k
            d *= k**(q +r)
        assert self * d**2 == num, (self, num, d)
        n, m = root(num, True)
        # so self * d**2 = n**2 +m, n >= 0, -n < m < n.
        if not m: return Rational(n, d)
        if invTol:
            raise ValueError("Is sadly not the square of a rational", self, n, m, d)

        assert -n < m <= n
        assert 0 < self * d**2 == n**2 +m
        # = (n +m/n/2)**2 -(m/n/2)**2
        # = (n +m/n/2 -m*m/n/4/(2*n*n +m))**2
        #   -(m*m/n/4/(2*n*n +m))**2
        # with m no bigger than n so the last term is < (1/n/8)**2; hopefully
        # good enough.  This is equivalent to two steps of Newton-Raphson,
        # which we can continue until we attain the requested precision.
        rough = (Rational(n, d) +Rational(m, 2 * n * d)
                 -Rational(m * m, 4 * n * d * (2 * n * n +m)))
        while True:
            gap = rough**2 -self
            assert gap >= 0, (gap, self, rough)
            # Make the adjustment even if we're already within tolerance:
            rough -= gap / rough / 2
            if gap * invTol < self: break
        return rough
    del desquare, factorise

    @lazyprop
    def floor(self): # round down (towards -infinity)
        num, den = self.__ratio
        rat = int(num // den)
        assert num >= rat * den
        return rat

    @lazyprop
    def ceil(self): # round up (towards +infinity)
        num, den = self.__ratio
        rat = int(num / den)
        if num > rat * den: return rat + 1
        return rat

    @lazyprop
    def nearint(self): # round to nearest int, preferring even when ambiguous
        num, den = self.__ratio
        q = int(divmod(2 * num + den, 2 * den)[0])
        while 2 * (num - q * den) >  den: q += 1
        while 2 * (num - q * den) < -den: q -= 1
        r = num - q * den
        if q % 2 and 2 * r in (den, -den): q += cmp(r, 0)
        return q

    @lazyprop
    def truncate(self): # round towards zero
        num, den = self.__ratio
        assert den > 0
        if num > 0: return int(num / den)
        return -int(-num / den)

    def continual(num, den):
        # iterates continued-fraction form of num / den
        while num:
            a, b = divmod(num, den) # num = a * den + b
            yield a
            num, den = den, b

    def discontinue(seq):
        # real representation from continued fraction
        val, i = 0., len(seq)
        while i > 0:
            i -= 1
            if val: val = 1. / val
            val = seq[i] + val
        return val

    @lazyprop
    def real(self, ingest=continual, digest=discontinue, tol=1e-6, count=4):
        num, den = self.__ratio
        try: return float(num) / den
        except OverflowError: pass

        # Fall back to calculation via continued fraction:
        seq, last = [], 0.0
        for it in ingest(num, den):
            seq.append(it)
            val = digest(seq)
            if len(seq) > count and last - tol < val < last + tol:
                return val
            last = val
        return digest(seq)
    del continual, discontinue

    @classmethod
    def _rational_(cls, num, den):
        return cls(num, den)

    def __nonzero__(self): return self.__ratio[0] != 0
    def __pos__(self): return self
    def __neg__(self):
        num, den = self.__ratio
        return self._rational_(-num, den)
    def __abs__(self):
        num, den = self.__ratio
        return self._rational_(abs(num), den)

    def __long__(self):    return long(self.truncate)
    def __int__(self):     return self.truncate
    def __complex__(self): return self.real + 0j
    def __float__(self):   return self.real
    def __format__(self, fmt=None):
        if not fmt:
            return str(self)
        if fmt[-1] in "eEfFgG%":
            return format(self.real, fmt)
        if fmt[-1] in "bcdoxX":
            return format(self.truncate, fmt)
        return format(str(self), fmt)

    def __add__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(num * q + p * den, den * q)

    __radd__ = __add__

    def __sub__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(num * q - p * den, den * q)

    def __rsub__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(den * p - q * num, q * den)

    def __mul__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(num * p, den * q)

    __rmul__ = __mul__

    def __truediv__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(num * q, den * p)
    __div__ = __truediv__

    def __rtruediv__(self, other):
        num, den = self.__ratio
        try: p, q = other.__ratio
        except AttributeError: p, q = other, 1
        return self._rational_(p * den, q * num)
    __rdiv__ = __rtruediv__

    def __floordiv__(self, other): return self.__truediv(other).floor
    def __mod__(self, other): self - self.__floordiv__(other) * other
    def __divmod__(self, other):
        rat = self.__floordiv__(other)
        return rat, self - rat * other

    def __pow__(self, count, mod=None):
        num, den = self.__ratio
        ans = self._rational_(num**count, den**count)
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
            cls = what.__class__.__name__
        else: cls = str(what.__class__)
        print 'Caught:', cls + `what.args`, 'in study.maths.ratio.approximate()'

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
