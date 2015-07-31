"""Legendre polynomials and spherical harmonics.

See http://www.chaos.org.uk/~eddy/math/smooth/harmony.html
See study.LICENSE for copyright and license information.
"""
from polynomial import Polynomial

class Legendre (Polynomial):
    __upinit = Polynomial.__init__
    from natural import hcf
    from Pascal import factorial

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

        e = hcf(*k)
        self.__upinit(enumerate(x // e for x in k))

    del hcf, factorial

    # The factor of sqrt(2.pi) actually belongs to the longitude.
    def _lazy_get_scale_(self, ig, cosp=Polynomial.fromSeq((1,0,-1))):
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
