"""Representing *very* big floating-point numbers.
"""

_rcs_id_ = """
$Id: bigfloat.py,v 1.3 2003-09-22 07:20:32 eddy Exp $
"""

class BigFloat:

    def __init__(self, val=1, century=0):
        if century != long(century): raise ValueError, 'century must be whole'
        if val in (BigFloat.infinity, -BigFloat.infinity): raise ValueError, 'infinite'
        if 1 == val == 0: raise ValueError, 'not a number'

        if isinstance(val, BigFloat):
            val, century = val.__scale, century + val.__century

        if val:
            while abs(val) > 1e50: val, century = val * 1e-100, century + 1L
            while abs(val) < 1e-50: val, century = val * 1e100, century - 1L
        else: century = 0

        self.__century, self.__scale = long(century), float(val)

    def __str__(self):
        ans = '%e' % self.__scale
        siz, dec = ans.split('e') # raise exception unless exactly one 'e'
        dec = int(dec) + 100 * self.__century
        return '%se%d' % (siz, dec)

    __repr__ = __str__
    # def __repr__(self): return 'BigFloat(%g, %dL)' % (self.__scale, self.__century)

    def __nonzero__(self): return self.__scale != 0
    def __neg__(self): return BigFloat(-self.__scale, self.__century)
    def __abs__(self): return BigFloat(abs(self.__scale), self.__century)
    def __long__(self): return long(float(self))
    def __int__(self): return int(float(self))

    def __float__(self):
        val, cen = self.__scale, self.__century
        while cen > 0:
            val, cen = val * 1e100, cen - 1
            if val == val / 10: return val

        while cen < 0:
            val, cen = val * 1e-100, cen + 1
            if val == 0: return val

        return val

    def extract(other): # local function, not method
        try: return other.__scale, other.__century
        except AttributeError: return other, 0L

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

        way = cmp(cen, self.__century)
        if way == 0: return BigFloat(self.__scale + val, cen)

        # ah !  messier ... one is huge compared to the other.
        this, that = self.__century + log(self.__scale), cen  + log(val)
        # is one of the numbers ignorable compared to the other ?
        if this < BigFloat.logeps + that: return BigFloat(other)
        if this + BigFloat.logeps > that: return self
        mid = (this + that) / 2
        val = self.__scale * 10.**(this - mid) + val * 10.**(that - mid)
        # answer is now val * 10.**mid
        that = decade(val) # within one of either this or prior that.
        cen, mid = divmod(mid, 100L)
        if mid + that < -50: mid, cen = mid + 100, cen - 1 # won't happen
        if mid + that >  50: mid, cen = mid - 100, cen + 1 # will happen

        return BigFloat(val * 10.**mid, cen)

    del extract

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

    i, x = 0, 1.
    while x > 0: i, x = i - 1, x * .1
    logzero = i

    i, x = 0, 1.
    while x != x / 10.: i, x = i + 1, x * 10.
    loginf, infinity = i, x

    i, x = 0, 1.
    while x + 1 != 1: i, x = i-1, x * .1
    logeps = i
    del i, x

_rcs_log_ = """
$Log: bigfloat.py,v $
Revision 1.3  2003-09-22 07:20:32  eddy
Added support for abs, long, int, float

Revision 1.2  2003/09/21 17:29:23  eddy
everything == nan, so test for it differently !
pow algorithm depends on whole being of an integral type; make it so.
make decade more symmetric

Revision 1.1  2003/09/21 17:10:50  eddy
Initial revision
"""
