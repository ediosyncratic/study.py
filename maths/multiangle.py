"""Sinusoids of multiples of angles, via polynomials.

See S and C for details, or
http://www.chaos.org.uk/~eddy/math/multiangle.html
for theory.
"""

_rcs_id_ = """
$Id: multiangle.py,v 1.4 2003-08-10 12:45:42 eddy Exp $
"""

from polynomial import Polynomial

class LazySeq:
    def __init__(self, first=Polynomial(1)): self.__seq = [ first ]

    def __growto(self, key):
        val = self.growto(key)

        if __debug__:
            i = val.rank
            while i >= 0:
                assert self.check(val.coefficient(i))
                i = i-1

        if key >= len(self.__seq):
            self.__seq = self.__seq + [ None ] * (1 + key - len(self.__seq))

        self.__seq[key] = val

        return val

    def check(self, val): return val == long(val)

    def __getitem__(self, key):
        try: ans = self.__seq[key]
        except IndexError: return self.__growto(key)
        else:
            if ans is None:
                return self.__growto(key)

        return ans

    def __getslice__(self, lo, hi, step=1):
        ans = []
        if step > 0:
            while lo < hi:
                ans.append(self[lo])
                lo = lo + step
        elif step < 0:
            while lo > hi:
                ans.append(self[lo])
                lo = lo + step
        elif lo != hi:
            raise ValueError("Sequence with zero step is a very bad idea !")

        return ans

z = Polynomial(0, 1)

class LazyPos (LazySeq):
    __check = LazySeq.check
    def check(self, val): return val > 0 and self.__check(val)

class EvenCos (LazyPos):
    """Sequence of polynomials giving cos of even multiples of angles

    C[2*n](u) = A[n](-2*u*u) * (-1)**n; see C for details.\n"""

    def growto(self, key, factor=Polynomial(0,2,1)):
        assert A is self
        if key == 1: return Polynomial(1, 1)
        n = key / 2
        m = key - n
        assert n and m
        return self[n] * self[m] + factor * R[n-1] * R[m-1]

class OddCos (LazyPos):
    """Sequence of polynomials giving cos of odd multiples of angles

    C[2*n+1](u) = u*B[n](-2*u*u) * (-1)**n; see C for details.\n"""

    def growto(self, key, factor=Polynomial(2, 1)):
        assert B is self
        if key == 1: return Polynomial(3, 2)
        n = key / 2
        m = key - n
        assert n and m
        return self[n] * A[m] + factor * Q[n] * R[m-1]

class OddSin (LazyPos):
    """Sequence of polynomials giving sin of odd multiples of angles

    S[2*n+1](u) = Q[n](-2*u*u) * (-1)**n; see S for details.\n"""

    def growto(self, key, factor=z):
        assert Q is self
        if key == 1: return Polynomial(1, 2)
        n = key / 2
        m = key - n
        assert n and m
        return self[n] * A[m] + factor * B[n] * R[m-1]

class EvenSin (LazyPos):
    """Sequence of polynomials giving sin of even multiples of angles

    S[2*(n+1)](u) = 2*u*R[n](-2*u*u) * (-1)**n; see S for details.\n"""

    # Could sensibly be given a [-1] equal to Polynomial(0).
    def growto(self, key):
        assert R is self
        if key == 1: return Polynomial(2, 2)
        n = key / 2
        m = key - n
        assert n and m
        return self[n] * A[m] + A[n+1] * self[m-1]

# [n] of each has n roots between -2 and 0
A, B, Q, R = EvenCos(), OddCos(), OddSin(), EvenSin()
del EvenCos, OddCos, EvenSin, OddSin

term = Polynomial(0, 0, -2)

class seqCos (LazySeq):
    """Sequence of polynomials describing cos(n.t) in terms of cos(t)

    C[n](cos(t)) = cos(n.t)\n"""
    def growto(self, key, factor=z):
        n, r = divmod(key, 2)
        if r: ans = factor * B[n](term)
        else: ans = A[n](term)
        assert ans.rank == key
        if n % 2: return -ans
        return ans

class seqSin (LazySeq):
    """Sequence of polynomials describing sin(n.t)/sin(t) in terms of cos(t)

    sin(t)*S[n](cos(t)) == sin(n.t)\n"""

    def growto(self, key, factor=z):
        n, r = divmod(key, 2)
        if r: ans = 2 * factor * R[n](term)
        else: ans = Q[n](term)
        assert ans.rank == key
        if n % 2: return -ans
        return ans

# C[n] and S[n+1] each have n roots between -1 and +1, symmetrically placed about 0
C, S = seqCos(), seqSin()
del seqCos, seqSin, z

_rcs_log_ = """
$Log: multiangle.py,v $
Revision 1.4  2003-08-10 12:45:42  eddy
Renumbered S down by 1, so S[n].rank is n.
Added assorted assertions.

Revision 1.3  2003/08/10 11:47:33  eddy
Fixed the S[1] tweak so it's lambda x: 1, not lambda x: x - d'oh !

Revision 1.2  2003/08/05 21:42:34  eddy
Tweaked so S[1] is a polynomial, not 1.0; added comments.

Initial Revision 1.1  2003/08/03 23:21:14  eddy
"""
