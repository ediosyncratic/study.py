"""Representing *very* big floating-point numbers.

No attempt is made to increase the *precision* above that of the native float
type; only the *range* of values (both towards zero and towards infinity).  Thus
BigFloat.logeps is the largest number of decimal digits worth attending to in
any float or BigFloat; but 10**BigFloat.loginf is distinguished from infinity
and 10**BigFloat.logzero is distinguished from zero.  Much the same applies to
the BigComplex type; note that (analogous to builtin complex) it holds real and
imaginary parts as separate BigFloat()s, so doesn't lose a tiny value of one
when added to a huge value of the other.

This approach (using a separate arbitrary-sized int as exponent) begs an
implementation float which uses an entire word to hold a bit-pattern denoting a
number between 1 and 2, with the leading '1.' elided (because implicit); then
all floating point instructions yield their answer in this form along with an
int register holding the power of two the answer should be scaled by; this int
is thus made available to the infrastructure tracking the overall powers of two
encoded by the arbitrary-sized int (i.e. python's long or an equivalent)
accompanying the fractional value.
"""
from study.snake.lazy import Lazy

class BigFloat (Lazy):
    def __init__(self, val=1, century=0):
        if century != long(century):
            raise ValueError, 'century must be whole'

        if isinstance(val, float):
            if val in (BigFloat.infinity, -BigFloat.infinity):
                raise ValueError, 'infinite'
            if 1 == val == 0:
                raise ValueError, 'not a number'
        # </checks>

        if isinstance(val, BigFloat):
            val, century = val.__scale, century + val.__century

        if val:
            while abs(val) > BigFloat.huge: val, century = val / BigFloat.step, century + 1L
            while abs(val) > 1e50: val, century = val * 1e-100, century + 1L
            while abs(val) < 1e-50: val, century = val * 1e100, century - 1L
        else: century = 0

        self.__century, self.__scale = long(century), float(val)

    def __str__(self): return self.str
    __repr__ = __str__
    # def __repr__(self): return 'BigFloat(%g, %dL)' % (self.__scale, self.__century)

    def _lazy_get_str_(self, ignored):
        ans = '%e' % self.__scale
        siz, dec = ans.split('e') # raise exception unless exactly one 'e'
        dec = int(dec) + 100 * self.__century
        return '%se%d' % (siz, dec)

    def __nonzero__(self): return self.__scale != 0
    def _lazy_get__lazy_hash_(self, ignored): return hash(self.__scale) ^ hash(self.__century)
    def __neg__(self): return BigFloat(-self.__scale, self.__century)
    def __abs__(self): return BigFloat(abs(self.__scale), self.__century)
    def __int__(self): return int(float(self))
    def __long__(self): return self.long
    def __float__(self): return self.float

    def _lazy_get_float_(self, ignored):
        val, cen = self.__scale, self.__century

        while cen > 0:
            val, cen = val * 1e100, cen - 1
            if val == val / 10: return val # infinite

        while cen < 0:
            val, cen = val * 1e-100, cen + 1
            if val == 0: return val

        return val

    def __divmod__(self, other):
        # Need to solve: self = quot * other + rem;
        # quot must be whole, rem must be between 0 (included) and other (excluded).
        if 0 <= self < other or 0 >= self > other: return 0L, self

        # determine order of magnitude of quotient:
        q = 10L ** long(str(self / other).split('e')[1])

        # get on with refining a value ...
        quot, rem = 0L, self
        while q:
            d = int(rem / other / q)
            if d: quot, rem = quot + d * q, rem - d * q * other
            assert int(rem / other / q) == 0
            q = q / 10
        
        return quot, rem

    def extract(other): # local function, not method
        try: return other.__scale, other.__century
        except AttributeError: pass
        try: other = BigFloat(other)
        except ValueError: return other, 0L # leave problem for caller !
        return other.__scale, other.__century

    def __mul__(self, other, get=extract):
        val, cen = get(other)
        return BigFloat(self.__scale * val, self.__century + cen)

    def __div__(self, other, get=extract):
        val, cen = get(other)
        return BigFloat(self.__scale / val, self.__century - cen)
    __truediv__ = __div__

    def __rdiv__(self, other, get=extract):
        val, cen = get(other)
        return BigFloat(val / self.__scale, cen - self.__century)
    __rtruediv__ = __rdiv__

    def __cmp__(self, other, get=extract):
        val, cen = get(other)
        if cen == self.__century or val == 0 or self.__scale == 0:
            return cmp(self.__scale, val)

        if val < 0 and self.__scale > 0: return 1
        if val > 0 and self.__scale < 0: return -1
        # same sign, different century

        if val < 0: return cmp(cen, self.__century)
        return cmp(self.__century, cen)

    def decade(val): # local function, not method
        if val == 0: return BigFloat.logzero
        if val < 0: val = -val
        i = 0
        while val < .32: val, i = val * 10, i - 1
        while val > 3.2: val, i = val * .1, i + 1
        return i

    def __add__(self, other, get=extract, log=decade):
        if self.__scale == 0: return BigFloat(other)
        val, cen = get(other)
        if val == 0: return self
        if cen == self.__century: return BigFloat(self.__scale + val, cen)

        # ah !  messier ... one is huge compared to the other.
        this, that = self.__century * 100 + log(self.__scale), cen * 100  + log(val)
        # Is one number ignorable compared to the other ?
        if this + 1 < BigFloat.logeps + that: return BigFloat(val, cen)
        if this + BigFloat.logeps > that + 1: return self

        # No, we really have to do this the hard way ...
        era, mid = divmod((this + that) / 2, 100L)
        val = self.__scale * 1e100 ** (self.__century - era) + val * 1e100 ** (cen - era)
        return BigFloat(val, era)

    del extract

    def _lazy_get_long_(self, ignored, log=decade):
        val, cen = self.__scale, self.__century
        if cen < 0: return 0L
        if cen == 0:
            if abs(val) < 1: return 0L
            return long(val)

        dec = log(val) # >= -50
        gap = BigFloat.loginf + BigFloat.logeps - 50 - dec # < loginf + logeps
        if gap > 100L * cen: return long(val * 10. ** (100L * cen))
        return long(val * 10. ** gap) * 10L**(100L * cen - gap)

    def __pow__(self, other, mod=None, log=decade):
        assert mod is None
        if self.__scale == 0:
            if other > 0: return self
            if other < 0: return BigFloat.infinity
            return BigFloat.infinity * 0

        whole, frac = divmod(other, 1)
        dec = log(self.__scale)
        val, dec = self.__scale * .1**dec, dec + self.__century * 100

        if not frac: ans, wer = 1, 0
        else:
            wer, ans = divmod(dec * frac, 1)
            wer, ans = long(wer), 10.**ans * val**frac
        # self**frac is ans * 10**wer

        if whole < 0: val, dec, whole = 1./val, -dec, -whole
        whole = long(whole)
        while whole:
            if whole % 2:
                ans, wer = ans * val, wer + dec
                while ans >  4: ans, wer = ans * .1, wer + 1
                while ans < .3: ans, wer = ans * 10, wer - 1

            val, dec, whole = val**2, dec * 2, whole / 2
            while val >  4: val, dec = val * .1, dec + 1
            while val < .3: val, dec = val * 10, dec - 1

        cen, wer = divmod(wer, 100)
        if wer < -50: cen, wer = cen - 1, wer + 100
        if wer >  50: cen, wer = cen + 1, wer - 100

        return BigFloat(ans * 10.**wer, cen)

    del decade
    __rmul__, __radd__ = __mul__, __add__
    def __rcmp__(self, other): return - cmp(self, other)
    def __sub__(self, other): return self + (-other)
    def __rsub__(self, other): return (-self) + other

    import math
    def _lazy_get_exp_(self, ignored, exp=math.exp):
        # Obviously, this will get you into trouble if self is big -
        # exp(1e308) is OK, but exp(1e308 * 10) will make you wait !
        # exp(self) = expval * 1e100**expcen
        # self = log(expval) + expcen*log(1e100)
        c, v = divmod(self, BigFloat.logcentury)
        return BigFloat(exp(v), c)

    def _lazy_get_log_(self, ignored, log=math.log, pi=math.pi):
        if self > 0:
            return log(self.__scale) + BigFloat.logcentury * BigFloat(self.__century)
        if self < 0:
            return BigComplex((-self).log, pi)
        return log(self.__scale) # raises deserved OverflowError: log(0)

    def arctan(self, other=1, atan=math.atan2):
        """Return an angle (in radians) whose tan() is self / other."""
        ans = atan(self.float, float(other)) # between -pi and +pi
        if ans == 0: return self / other
        return ans

    logcentury = 100 * math.log(10)
    del math

    i, x = 0, 1.
    while x > 0: i, x = i - 1, x * .1
    logzero = i

    i, x = 0, 1.
    while x != x / 10.: i, x = i + 1, x * 10.
    loginf, infinity = i, x

    i, x = 0, 1.
    while x + 1 != 1: i, x = i-1, x * .1
    logeps = i # loginf + logzero - 1

    huge, step = 10L ** (loginf + logeps), 10L ** 100

    del i, x

class BigComplex (Lazy):
    def __init__(self, r, i, century=0):
        self.real, self.imag = BigFloat(r, century), BigFloat(i, century)

    def __str__(self): return self.str
    __repr__ = __str__
    # def __repr__(self): return 'BigComplex(%s, %s)' % (`self.real`, `self.imag`)
    def _lazy_get_str_(self, ignored):
        if self.imag == 0: return str(self.real)
        if self.real == 0: return str(self.imag) + 'j'
        return '(%s+%sj)' % (self.real, self.imag)

    def _lazy_get_conjugate_(self, ignored):
        return BigComplex(self.real, -self.imag)

    def _lazy_get_abs_(self, ignored): return self.__absq()**.5
    def _lazy_get_phase_(self, ignored): return self.imag.arctan(self.real)
    def __nonzero__(self): return self.real != 0 or self.imag != 0
    def _lazy_get__lazy_hash_(self, ignored): return hash(self.real) ^ hash(self.imag)
    def __neg__(self): return BigComplex(-self.real, -self.imag)
    def __abs__(self): return self.abs

    def _lazy_get_log_(self, ignored): return BigComplex(self.abs.log, self.phase)

    def __pow__(self, other): # third arg, if given, is to be reduced modulo.
        return (self.log * other).exp

    def __rpow__(self, other):
        try: return (self * other.log).exp
        except AttributeError: pass
        try: r, i = other.real, other.imag
        except AttributeError: return (self * BigFloat(other).log).exp
        return (self * Bigcomplex(r, i).log).exp

    import cmath
    def expi(phi, qi=cmath.pi/2, exp=cmath.exp): # an attempt to evade tiny errors
        i, phase = divmod(phi, qi)
        return exp(1j * float(phase)) * 1j**(long(i) % 4)
    del cmath

    def _lazy_get_exp_(self, ignored, expi=expi):
        phase = expi(self.imag)
        return BigComplex(phase.real, phase.imag) * self.real.exp
    del expi

    def __absq(self):
        try: ans = self.__abssquare
        except AttributeError:
            self.__abssquare = ans = self.real**2 + self.imag**2
        return ans

    def extract(other): # local function, not method
        try: return other.real, other.imag
        except AttributeError: return other, 0

    def __eq__(self, other, get=extract):
        r, i = get(other)
        return r == self.real and i == self.imag

    def __add__(self, other, get=extract):
        r, i = get(other)
        return BigComplex(self.real + r, self.imag + i)

    def __sub__(self, other, get=extract):
        r, i = get(other)
        return BigComplex(self.real - r, self.imag - i)

    def __rsub__(self, other, get=extract):
        r, i = get(other)
        return BigComplex(r - self.real, i - self.imag)

    def __mul__(self, other, get=extract):
        r, i = get(other)
        return BigComplex(self.real * r - self.imag * i,
                          self.real * i + self.imag * r)

    def __div__(self, other, get=extract):
        r, i = get(other)
        a = BigFloat(r)**2 + BigFloat(i)**2
        return BigComplex((self.real * r + self.imag * i) / a,
                          (self.imag * r - self.real * i) / a)

    def __rdiv__(self, other, get=extract):
        r, i = get(other)
        a = self.__absq()
        return BigComplex((self.real * r + self.imag * i) / a,
                          (self.real * i - self.imag * r) / a)

    del extract
    __rmul__, __radd__ = __mul__, __add__

del Lazy
