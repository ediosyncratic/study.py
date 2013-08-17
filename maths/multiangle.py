"""Sinusoids of multiples of angles, via polynomials.

See S and C for details; or, for theory,
http://www.chaos.org.uk/~eddy/math/multiangle.html
See study.LICENSE for copyright and license information.
"""

from polynomial import Polynomial

class LazySeq:
    def __init__(self, first=Polynomial.power(0)): self.__seq = [ first ]

    def iswhole(val): return val == long(val)
    def __getitem__(self, key, check=iswhole):
        try:
            ans = self.__seq[key]
            if ans is None: raise IndexError
        except IndexError:
            ans = self.growto(key)
            assert all(check(ans.coefficient(i)) for i in range(1 + ans.rank))

            if key >= len(self.__seq):
                self.__seq += [ None ] * (1 + key - len(self.__seq))

            self.__seq[key] = ans

        return ans
    del iswhole

    def __getslice__(self, lo, hi, step=1):
        ans = []
        if step > 0:
            while lo < hi:
                ans.append(self[lo])
                lo += step
        elif step < 0:
            while lo > hi:
                ans.append(self[lo])
                lo += step
        elif lo != hi:
            raise ValueError("Sequence with zero step is a very bad idea !")

        return ans

z = Polynomial.power(1)

class Middle (LazySeq):
    def growto(self, key, u=z,
               squp = lambda p, u=z: (2 + u) * p**2,
               flap = lambda p, u=z: p + p(-u)):
        assert H is self # singleton class
        assert key > 0
        m, r = divmod(key - 1, 3)
        n = m + r
        assert key == n + 2 * m + 1 and (n + m) % 2 == r % 2
        N, M = self[n], self[m]
        head = (u + 2) * N * M
        tail = head(-u) # now subtract (-1) ** (n + m) of that from head:
        if r % 2: head += tail
        else: head -= tail
        ans = M * head / 2 - N
        assert ans.rank == key
        assert flap(squp(ans)) == 4
        return ans

H = Middle()
del Middle

term = 2 * z * z - 1

class seqCos (LazySeq):
    """Sequence of polynomials describing cos(n.t) in terms of cos(t)

    C[n](cos(t)) = cos(n.t)\n"""
    def growto(self, key, term=term, x=z):
        assert C is self # singleton class
        n, r = divmod(key, 2)
        if r: ans = x * H[n](2 * term)
        else: ans = term(self[n])

        assert ans.rank == key
        return ans

class seqSin (LazySeq):
    """Sequence of polynomials describing sin(n.t)/sin(t) in terms of cos(t)

    sin(t)*S[n-1](cos(t)) == sin(n.t)\n"""

    def growto(self, key, mert=-2 * term):
        assert S is self # singleton class
        n, r = divmod(key, 2)
        if r: ans = 2 * self[n] * C[1 + n]
        else:
            ans = H[n](mert)
            if n % 2: ans = -ans

        assert ans.rank == key
        return ans

del Polynomial, term, z, LazySeq

# C[n] and S[n] each have n roots between -1 and +1, symmetrically placed about 0
C, S = seqCos(), seqSin()
del seqCos, seqSin
