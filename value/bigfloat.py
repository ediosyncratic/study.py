"""Representing *very* big floating-point numbers.

ToDo:
  * make it work with Quantity, i.e. with Sample, i.e. with Weighted
  * make it work with complex
"""

_rcs_id_ = """
$Id: bigfloat.py,v 1.5 2003-09-22 23:19:35 eddy Exp $
"""

from basEddy.lazy import Lazy
from types import FloatType

class BigFloat (Lazy):

    def __init__(self, val=1, century=0):
        if century != long(century):
            raise ValueError, 'century must be whole'

        if type(val) == FloatType:
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

    def __rdiv__(self, other, get=extract):
        val, cen = get(other)
        return BigFloat(val / self.__scale, cen - self.__century)

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

    def __pow__(self, other, log=decade):
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

    def _lazy_get_log_(self, ignored, log=math.log):
        return log(self.__scale) + BigFloat.logcentury * BigFloat(self.__century)

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

_rcs_log_ = """
$Log: bigfloat.py,v $
Revision 1.5  2003-09-22 23:19:35  eddy
Fixed bugs in long, add.  Made test against inf/nan only apply to
floats; a long can appear to be inf otherwise ...

Revision 1.4  2003/09/22 22:15:48  eddy
Fixed bug in add (forgot factors of 100 on centuries !), made lazy;
implemented float, int, long, divmod, exp, log, hash.

Revision 1.3  2003/09/22 07:20:32  eddy
Added support for abs, long, int, float

Revision 1.2  2003/09/21 17:29:23  eddy
everything == nan, so test for it differently !
pow algorithm depends on whole being of an integral type; make it so.
make decade more symmetric

Revision 1.1  2003/09/21 17:10:50  eddy
Initial revision
"""
