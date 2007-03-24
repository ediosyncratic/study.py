"""Multinomials - polynomials in several free variables.

c.f. polynomial, using only one free variable.

$Id: multinomial.py,v 1.3 2007-03-24 15:23:09 eddy Exp $
"""
from value.lazy import Lazy
from polynomial import unNaturalPower

class Multinomial (Lazy):
    def __init__(self, bok):
        for k, v in bok.items():
            if v:
                v *.4 + v * .6 # raise error if no arithmetic
                for it in k: # raise error if k not a sequence
                    if it != long(it): raise unNaturalPower

                if k and k[-1] == 0:
                    q = k[:-1]
                    while q and q[-1] == 0: q = q[:-1]
                    del bok[k]
                    bok[q] = v

            else: del bok[k]

        # canonical form for keys: last entry non-zero
        self.__coefs = bok # dictionary { (int, ...): scalars }

    def __add__(self, whom):
        try: sum = whom.__coefs.copy()
        except AttributeError: sum = {(): whom}

        zero = self._zero
        for k, v in self.__coefs.items():
            sum[k] = sum.get(k, zero) + v

        return Multinomial(sum)

    __radd__ = __add__

    def __sub__(self, whom):
        sum, zero = self.__coefs.copy(), self._zero

        try: bok = whom.__coefs
        except AttributeError: sum[()] = sum.get((), zero) - whom
        else:
            for k, v in bok.items():
                sum[k] = sum.get(k, zero) - v

        return Multinomial(sum)

    def __rsub__(self, whom):
        try: sum = whom.__coefs.copy()
        except AttributeError: sum = {(): whom}

        zero = self._zero
        for k, v in self.__coefs.items():
            sum[k] = sum.get(k, zero) - v

        return Multinomial(sum)

    def addboks(key, cle): # tool func for __mul__
        sum = [0] * max(len(key), len(cle))

        i = len(key)
        while i > 0:
            i = i - 1
            sum[i] = key[i]

        i = len(cle)
        while i > 0:
            i = i - 1
            sum[i] = sum[i] + cle[i]

        while sum and sum[-1] == 0: sum = sum[:-1]
        return tuple(sum)

    def __mul__(self, whom, add=addboks):
	term = {}
        try: bok = whom.__coefs
        except AttributeError:
	    for key, val in self.__coefs.items():
		term[key] = val * whom
	else:
            zero = self._zero * whom._zero
	    for key, val in self.__coefs.items():
		for cle, lue in bok.items():
                    sum = add(key, cle)
                    term[sum] = term.get(sum, zero) + val * lue

        return Multinomial(term)

    del addboks
    __rmul__ = __mul__

    def __pow__(self, whom):
        result = 1

        while whom >= 1:
            if whom % 2: result = result * self
            self, whom = self * self, whom / 2

        return result

    def __divmod__(self, whom):
        # solve self = q * whom + r with r `suitably less than (?)' whom
        raise NotImplementedError

    def subboks(this, that): # tool func for derivative (and divmod ?)
        row, scale = [0] * max(len(this), len(that)), 1

        i = len(this)
        while i > 0:
            i = i - 1
            row[i] = this[i]

        i = len(that)
        while i > 0:
            i = i - 1
            if that[i] > 0:
                v = row[i] - that[i]
                if v < 0: return None, 1
                scale = reduce(lambda x, y: x * y, range(row[i], v, -1), scale)
                row[i] = v

        while row and row[-1] == 0: row = row[:-1]
        return tuple(row), scale

    def derivative(self, key, sub=subboks):
        """Differentiate; key says how often in which variables. """
        bok = {}
        while key and key[-1] == 0: key = key[:-1]

        for k, v in self.__coefs.items():
            q, s = subboks(k, key)
            if q is not None: bok[q] = s * v

        return Multinomial(bok)

    del subboks

    def __call__(self, *args):
        if len(args) != len(self.profile):
            raise TypeError('Multinomial in n variables needs n values', len(self.profile))

        result = self._zero
        for key in self._powers:
            val, i = self.__coefs[key], len(args)
            while i > 0:
                i = i - 1
                try: p = key[i]
                except IndexError: pass
                else: val = val * args[i]**p

            result = result + val

        return result

    def __nonzero__(self): return self.rank >= 0
    def __eq__(self, whom): return (self - whom).rank < 0
    def __neg__(self): return 0 - self

    def __repr__(self): return self._repr
    __str__ = __repr__
    variablenames = 'zyxwvutsrqpnmlkhgfdcba' # skip o,i,j,e [0, sqrt(-1), exp]

    def _lazy_get_rank_(self, ig):
        if not filter(None, self.__coefs.values()): return -1
        return max(self._ranks)

    def _lazy_get_uniform_(self, ig):
        return not filter(lambda x, r=self.rank: x != r, self._ranks)

    def _lazy_get_profile_(self, ig):
        return tuple(apply(map, [lambda *x: max((0,) + filter(None, x))] + self.__coefs.keys()))

    # support ...

    def _lazy_get__zero_(self, ig):
        try: return self.__coefs.values()[0] * 0
        except IndexError: return 0

    def format(num):
        # it might be nice to also cope with non-scalar coefficients ...
        try:
            if num.imag == 0:
                num = num.real
                raise AttributeError
        except AttributeError:
            try:
                if num == int(num): num = int(num)
            except OverflowError: pass

        ans = str(num)
        if ans[0] != '-': return ' +' + ans
        else: return ' ' + ans

    def powname(seq, noms):
        i, row = len(seq), []
        while i > 0:
            i = i - 1
            if seq[i] == 1: row.append(noms[i])
            elif seq[i]: row.append('%s**%d' % (noms[i], seq[i]))

        return '*'.join(row)

    def _lazy_get__repr_(self, ig, fmt=format, nom=powname):
        result, names, keys = '', self.variablenames, self._powers

        for key in keys:
            val = self.__coefs[key]
            if key:
                if val == 1: result = result + ' +'
                elif val == -1: result = result + ' -'
                else: result = result + fmt(val) + '*'
                result = result + nom(key, names)

            else: result = result + fmt(val)

        lamb = 'lambda ' + ', '.join(names[:len(self.profile)]) + ':'
        if not result: return lamb + ' 0'
        if result[:2] == ' +': return lamb + ' ' + result[2:]
        return lamb + result

    del format, powname

    def sum(seq): return reduce(lambda a, b: a+b, seq, 0)

    def _lazy_get__ranks_(self, ign, sum=sum):
        return tuple(map(sum, self.__coefs.keys()))

    def keyorder(that, this, sum=sum):
        sign = cmp(sum(this), sum(that)) or cmp(len(this), len(that))
        if sign: return sign

        i = len(this) # == len(that)
        while i > 0:
            i = i - 1
            sign = cmp(this[i], that[i])
            if sign: return sign

        return 0

    del sum

    def _lazy_get__powers_(self, id, order=keyorder):
        row = self.__coefs.keys()
        row.sort(order)
        return tuple(row)

    del keyorder
