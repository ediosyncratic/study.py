"""Legendre polynomials and spherical harmonics.

See http://www.chaos.org.uk/~eddy/math/smooth/harmony.html
"""

_rcs_id_ = """
$Id: Legendre.py,v 1.1 2005-09-08 21:37:29 eddy Exp $
"""
from polynomial import Polynomial

class Legendre (Polynomial):
    __upinit = Polynomial.__init__
    from natural import hcf

    def __init__(self, b, q, hcf=hcf):
        if b < 0 or q > b or q < -b or b != int(b) or q != int(q):
            raise ValueError("Legendre(b, q) is defined for natural b and integers q between -b and +b", b, q)
        if q < 0: q = -q
        self.__q = q

        # solve k(j+2).(j+2).(j+1) = k(j).(j.(j+1) -b.(1+b)) for j = b-2, b-4, ...
        k, j, L = [ 0 ] * b, b-q, b * (1+b)
        last = 1
        while j > 1:
            j = j - 2
            last = last * ((q+j) * (1+q+j) - L)
        if last < 0: last = -last

        j = b-q
        k.append(last)
        while j > 1:
            last = k[j] * j * (j - 1)
            j = j - 2
            k[j] = last // ((q+j) * (1+q+j) - L)

        self.__upinit(map(lambda x, n=apply(hcf, tuple(k)): x // n, k))

    del hcf
    from math import pi

    # The factor of sqrt(2.pi) actually belongs to the longitude.
    def _lazy_get_scale_(self, ig, cosp=Polynomial(1,0,-1), tp=2*pi):
        return (tp * (self**2 * cosp**self.__q).integral(-1, 0)(1)) ** .5

    del pi

del Polynomial

class Spherical:
    """A spherical harmonic.

    Attributes l and j correspond to total spin and its component parallel to
    the co-ordinate axis.  Supports being called as a function of two
    parameters, longitude and latitude, both of which must be Quantity()s with
    units of angle."""

    def azimuth(b, q, cache={}):
        try: ans = cache[(b,q)]
        except KeyError: cache[(b,q)] = ans = Legendre(b, q)
        return ans

    def __init__(self, l=0, j=0, Legendre=azimuth):
        self.l, self.j = l, j
        if j < 0: j = -j
        self.__poly, self.__q = Legendre(l, j), j

    del azimuth

    def __call__(self, phi, theta):
        return phi.iExp * self.__poly(theta.Sin) * theta.Cos**self.__q / self.__poly.scale

_rcs_log_ = """
$Log: Legendre.py,v $
Revision 1.1  2005-09-08 21:37:29  eddy
Initial revision

"""
