"""Sinusoids of multiples of angles, via polynomials.

See S and C for details, or
http://www.chaos.org.uk/~eddy/math/multiangle.html
for theory.
"""

_rcs_id_ = """
$Id: multiangle.py,v 1.7 2003-08-13 22:15:12 eddy Exp $
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

class Middle (LazySeq):
    def growto(self, key, down={0: 1-z, 1: z-1}, up=z+1, two=-z-2):
        assert K is self
        prior = self[key-1]
        return (prior * up + down[key%2] * prior(two)) * .5

K = Middle()
del Middle

term = Polynomial(1, 0, -2)

class seqCos (LazySeq):
    """Sequence of polynomials describing cos(n.t) in terms of cos(t)

    C[n](cos(t)) = cos(n.t)\n"""
    def growto(self, key, kate=4*z*z-3, term=-term, z=z):
        n, r = divmod(key, 2)
        if r: ans = z * K[n](kate)
        else: ans = self[n](term)

        assert ans.rank == key
        return ans

class seqSin (LazySeq):
    """Sequence of polynomials describing sin(n.t)/sin(t) in terms of cos(t)

    sin(t)*S[n](cos(t)) == sin(n.t)\n"""

    def growto(self, key, stem=1-4*z*z, term=term, z=z):
        n, r = divmod(key, 2)
        if r: ans = 2 * z * S[n](-term)
        else: ans = K[n](stem) * {0: 1, 1: -1}[n%2]

        assert ans.rank == key
        return ans

# C[n] and S[n+1] each have n roots between -1 and +1, symmetrically placed about 0
C, S = seqCos(), seqSin()
del seqCos, seqSin
del term, z, Polynomial

_rcs_log_ = """
$Log: multiangle.py,v $
Revision 1.7  2003-08-13 22:15:12  eddy
Fixed d'oh; wrong sign for the term used by Cos.

Revision 1.6  2003/08/12 22:57:43  eddy
Eliminated A, B, Q, R in favour of K and halvings through S and C;
i.e. I have now proved B-Q unification (and renamed T and D to K), along
with eliminating A and R as mere aspects of C and S (respectively).

Revision 1.5  2003/08/10 20:45:02  eddy
Added implementation of the polynomial that unifies Q and B, albeit I haven't
yet *proved* it does so.  The assertions make me confident, though ;^>

Revision 1.4  2003/08/10 12:45:42  eddy
Renumbered S down by 1, so S[n].rank is n.
Added assorted assertions.

Revision 1.3  2003/08/10 11:47:33  eddy
Fixed the S[1] tweak so it's lambda x: 1, not lambda x: x - d'oh !

Revision 1.2  2003/08/05 21:42:34  eddy
Tweaked so S[1] is a polynomial, not 1.0; added comments.

Initial Revision 1.1  2003/08/03 23:21:14  eddy
"""
