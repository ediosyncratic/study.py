"""Atomic energy levels.

A good reference for these is Leonard I. Schiff's 'quantum mechanics'; page
numbers quoted here are for the third edition.
"""
from study.maths.polynomial import Polynomial
from study.maths.natural import hcf

class Hermite (Polynomial):
    __upinit = Polynomial.__init__
    def __init__(self, n):
        """Polynomial for quantum simple harmonic oscillators.

        Single argument is the order of the polynomial.  The constructed
        polynomial has positive leading order term with integers for all
        coefficients, sharing no common factor among them.

        See: http://www.chaos.org.uk/~eddy/physics/harmonic.xhtml
        or Schiff p. 69.\n"""

        self.__upinit(self.__coefs(n))

    @staticmethod
    def __coefs(n, gcd=hcf):
        bok, e = {n: 1}, n
        while n > 1:
            n -= 2
            v = -(n + 1) * (n + 2) * bok[n + 2] // 2
            a = gcd(v, e - n)
            # Nominally, we add bok[n] = v/(e-n) but then rescale all
            # co-efficients to make that whole; really, scale all others
            # by (e-n)/a and set bok[n] = v/a.
            assert a > 0 and (e - n) % a == 0 == v % a, 'Euclid failed !'
            b = (e - n) // a
            for k in bok.iterkeys(): bok[k] *= b
            bok[n] = v // a

        return bok

    def _lazy_get_scale_(self, ig, square=Polynomial((0,0,1))):
        grand = (self * self).unafter(square)
        n, ans = grand.rank, 0
        while n > -1:
            ans *= n + .5
            ans += grand.coefficient(n)
            n -= 1
        assert ans * 2**(self.rank % 2) == reduce(lambda x, y: x * (y+1), range(self.rank), 1)
        return ans # * (h/k/m/2)**.5

class Laguerre (Polynomial):
    """The Laguerre polynomials.

    See http://www.chaos.org.uk/~eddy/physics/atom.xhtml
    or Schiff p. 92.\n"""
    __upinit = Polynomial.__init__
    def __init__(self, n, b):
        if b < 0 or n <= b or b != int(b) or n != int(n):
            raise ValueError("Laguerre(n, b) is defined for natural n, b with n > b", n, b)
        self.__upinit(self.__coefs(n, b))

    from study.maths.Pascal import factorial
    @staticmethod
    def __coefs(n, b, gcd=hcf, pling=factorial):
        f = [ 0 ] * n
        # (j*(j+1) -b*(b+1))*f[j] = f[j-1]*(j-n) for n > j >= b
        j, last, L = n, pling(n-1-b), b * (b+1)
        while j > b:
            j = j - 1
            f[j] = last
            last = last * (j * (j+1) - L) / (j - n)

        assert last == 0
        return map(lambda x, e=gcd(*f): x // e, f)
    del factorial

    def _lazy_get_scale_(self, ig, linear=Polynomial((0,1))):
        return ((self * linear)**2).Gamma ** .5

del Polynomial, hcf

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
          j - angular momentum component about coordinate axis; integer
              between minus b and b.
        Optional argument:
          Z - atomic number of atom (number of protons in the nucleus);
              positive integer (default: 1).

        Describes a potential orbit of a lone electron about a nucleus
        of atomic number Z.\n"""

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

        The radial co-ordinate, r, should be a dimensionless Quantity()
        obtained by dividing true radius by the Bohr radius, hbar /
        (mass * alpha * c) where the mass should really be a reduced
        mass, given by 1/mass = 1/electron.mass + 1/nucleus.mass; but
        this is almost exactly the electron mass anyway.  The returned
        field value should be divided by a factor of this Bohr radius to
        the power 1.5 to get a quantity whose squared modulus, when
        integrated over a volume, yields the probability of finding the
        electron in that volume.  Alternatively, skip that scaling and
        do your integration in co-ordinates which use the Bohr radius as
        unit of length.\n"""

        return self.__angular(phi, theta) * self.__radial(r)

del Lazy
