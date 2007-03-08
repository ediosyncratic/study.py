"""Legendre polynomials and spherical harmonics.

See http://www.chaos.org.uk/~eddy/math/smooth/harmony.html
"""

_rcs_id_ = """
$Id: Legendre.py,v 1.2 2007-03-08 23:40:31 eddy Exp $
"""
from polynomial import Polynomial

class Legendre (Polynomial):
    __upinit = Polynomial.__init__
    from natural import hcf, factorial

    def __init__(self, b, q, hcf=hcf, pling=factorial):
        if b < 0 or q > b or q < -b or b != int(b) or q != int(q):
            raise ValueError("Legendre(b, q) is defined for natural b and integers q between -b and +b", b, q)
        if q < 0: q = -q
        self.__q = q

        # solve k(j+2).(j+2).(j+1) = k(j).((q+j).(q+j+1) -b.(1+b)) for q+j = b-2, b-4, ...
        # k(b-q-2.j) is non-zero for b-q >= 2.j > 0
        j, L = b - q, b * (1+b)
        k, last, j = [ 0 ] * j, pling(j), j % 2
        if ((b-q-j)/2) % 2: last = -last
        while q + j < b:
            k[j] = last
            last, j = last * ((q + j) * (q + j + 1) - L), j + 2
            last = last // ((j - 1) * j)

        assert q + j == b
        k.append(last)

        self.__upinit(map(lambda x, n=apply(hcf, tuple(k)): x // n, k))

    del hcf, factorial

    # The factor of sqrt(2.pi) actually belongs to the longitude.
    def _lazy_get_scale_(self, ig, cosp=Polynomial(1,0,-1)):
        return (self**2 * cosp**self.__q).integral(-1, 0)(1) ** .5

del Polynomial

class Spherical:
    """A spherical harmonic.

    Attributes l and j correspond to total spin and its component parallel to
    the co-ordinate axis.  Supports being called as a function of two
    parameters, longitude and latitude, both of which must be Quantity()s with
    units of angle."""

    def azimuth(b, q, cache={}): # local tool function to cache Legendre results
        try: ans = cache[(b,q)]
        except KeyError: cache[(b,q)] = ans = Legendre(b, q)
        return ans

    def __init__(self, l=0, j=0, Legendre=azimuth):
        self.l, self.j = l, j
        if j < 0: j = -j
        self.__poly, self.__q = Legendre(l, j), j

    del azimuth
    from math import pi

    def __call__(self, phi, theta, tp=(2*pi)**.5):
        return (self.j * phi).iExp * self.__poly(theta.Sin) * \
               theta.Cos**self.__q / self.__poly.scale / tp

    del pi

_rcs_log_ = """
$Log: Legendre.py,v $
Revision 1.2  2007-03-08 23:40:31  eddy
Extensive changes (2005-09-18).

Initial Revision 1.1  2005/09/08 21:37:29  eddy
"""
