"""Atomic energy levels.

$Id: atomic.py,v 1.7 2009-03-05 07:16:32 eddy Exp $
"""
from study.maths.polynomial import Polynomial

class Laguerre (Polynomial):
    __upinit = Polynomial.__init__
    from study.maths.natural import hcf
    from study.maths.Pascal import factorial

    def __init__(self, n, b, hcf=hcf, pling=factorial):
        if b < 0 or n <= b or b != int(b) or n != int(n):
            raise ValueError("Laguerre(n, b) is defined for natural n, b with n > b", n, b)

        f = [ 0 ] * n
        # (j*(j+1) -b*(b+1))*f[j] = f[j-1]*(j-n) for n > j >= b
        j, last, L = n, pling(n-1-b), b * (b+1)
        while j > b:
            j = j - 1
            f[j] = last
            last = last * (j * (j+1) - L) / (j - n)

        assert last == 0
        self.__upinit(map(lambda x, e=hcf(*f): x // e, f))

    del hcf, factorial

    def _lazy_get_scale_(self, ig, linear=Polynomial((0,1))):
        return ((self * linear)**2).Gamma ** .5

del Polynomial

from study.snake.lazy import Lazy

class Radial (Lazy):
    def __init__(self, n, b, Z):
        self.__poly, self.__frac = Laguerre(n, b), Z * 1. / n
        self.degeneracy = 2 * b + 1

    def _lazy_get_scale_(self, ig):
        return self.__poly.scale / (2 * self.__frac) ** 1.5

    def _lazy_get_Energy_(self, ig):
        return - self.__frac**2

    def __call__(self, r):
        # r is measured in Bohr radii, and must be a Quantity.
        r = r * self.__frac
        # maybe use .evaluate() ...
        return self.__poly(2*r) / r.exp / self.scale

from study.maths.Legendre import Spherical

class Orbit (Lazy):
    def __init__(self, n, b, j, Z=1):

        """Initialize a dimensionless quantum orbit in a Coulomb field.

        Required parameters:
          n - shell number; natural
          b - total angular momentum number; natural < n
          j - angular momentum component about coordinate axis; integer between
              minus b and b.
        Optional argument:
          Z - atomic number of atom (number of protons in the nucleus); positive
              integer.

        Describes a potential orbit of a lone electron about a nucleus of atomic
        number Z.\n"""

        if Z <= 0 or Z != int(Z):
            raise ValueError("Atomic number should be a positive integer", Z)

        # Let Laguerre and Legendre do all the asserting we need about n, b, j
        self.__radial = Radial(n, b, Z)
        self.__angular = Spherical(b, j)
        self.n, self.l, self.j, self.Z = n, b, j, Z

    def _lazy_get_Energy_(self, ig):
        """In units of particle.Rydberg.energy (q.v.)"""
        return self.__radial.Energy

    def __call__(self, r, phi, theta):
        """Returns dimensionless field probability at specified location.

        The radial co-ordinate, r, should be a dimensionless Quantity() obtained
        by dividing true radius by the Bohr radius, hbar / (mass * alpha * c)
        where the mass should really be a reduced mass, given by 1/mass =
        1/electron.mass + 1/nucleus.mass; but this is almost exactly the
        electron mass anyway.  The returned field value should be divided by a
        factor of this Bohr radius to the power 1.5 to get a quantity whose
        squared modulus, when integrated over a volume, yields the probability
        of finding the electron in that volume.  Alternatively, skip that
        scaling and do your integration in co-ordinates which use the Bohr
        radius as unit of length.\n"""

        return self.__angular(phi, theta) * self.__radial(r)

del Lazy
