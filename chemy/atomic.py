"""Atomic energy levels.

A good reference for these is Leonard I. Schiff's 'quantum mechanics';
page numbers quoted here are for the third edition.

Classes, each describing the spatial variation of a quantum system's
wave-function:

  Hermite -- simple harmonic oscillator
  Laguerre -- underlies Radial
  Radial -- radial variation for the Coulomb potential
  Orbit -- spatial variation for the Coulomb potential

See study.LICENSE for copyright and license information.
"""
from study.maths.polynomial import Polynomial
from study.maths.natural import hcf

class Hermite (Polynomial):
    """The (physicists') Hermite polynomials.

    Property scale (is lazily computed and) is the result of integrating the
    polynomial's square, times twice exp&on;negate&on;square / &radic;pi.
    Pseudo-constructor normalised(n) returns the instance of order n normalised
    to make scale 1.\n"""
    __upinit = Polynomial.__init__
    def __init__(self, n, denom = None):
        """Polynomial for quantum simple harmonic oscillators.

        Required argument is the order of the polynomial.  By default, the
        constructed polynomial has positive leading order term with integers
        for all coefficients.  Optional second argument, denom, is a number by
        which to divide the canonical form of the polynomial, or None (its
        default) to use the highest common factor of its coefficients, which is
        always a multiple of 2**((n +1)//2); pass 1 to get the canonical form
        of the function.  See .normalised() for the unit-scaled version.

        See: http://www.chaos.org.uk/~eddy/physics/harmonic.xhtml
        or Schiff p. 69.\n"""
        self.__upinit(enumerate(self.__coefs(n)),
                      self.__coefs(n, True) if denom is None else denom)
        assert self.rank == n

    @classmethod
    def __coefs(cls, n, justGcd = False, cache=[(1,)], gcd = hcf):
        """If H(n) is the one of these of degree n, then

          H(n)*G = repeat(n, lambda f: -f', G)

        in which G(x) = exp(-x*x), -G' = 2*power(1)*G, so

          H(n+1)*G = -(H(n)*G)' = 2*power(1)*H(n)*G -H(n)'*G

        which enables us to iteratively determine these polynomials as

          H(n+1) = 2*power(1)*H(n) -H(n)'

        which we can cache as we compute higher order cases.\n"""
        if n < 0 or not isinstance(n, int):
            raise ValueError("Hermite polynomials have natural order", n)

        while n >= len(cache):
            h = cache[-1]
            # Derivative, with zero coefficients for two more powers:
            hd = tuple(i * c for i, c in enumerate(h[1:], 1)) + (0, 0)
            # 2.power(1).h:
            zh = (0,) + tuple(2 * x for x in h)
            cache.append(tuple(z -d for z, d in zip(zh, hd)))

        assert not divmod(gcd(*cache[n]), 2 ** divmod(n +1, 2)[0])[1], cache[n]
        return gcd(*cache[n]) if justGcd else cache[n]

    def _lazy_get_scale_(self, ig, square=Polynomial.power(2)):
        """Scaling needed to normalise.

        Since self is a sum either of odd powers or of even powers, its square
        is a sum of even powers; when that square is written as a polynomial P
        in the square of self's parameter, P(x*x) = self(x)*self(x), P has the
        same rank as self.  Each g(i).power(i) term in P contributes
        g(i).(2.i)!/i!/power(i, 4) = g(i).(i -1/2).(i -3/2)...(3/2).(1/2) to
        the sum computed here.

        With n = self.rank, this makes the sum

          ((...(g(n)*(2*n -1)/2 +g(n-1))*(2*n -3)/2 +... +g(2))*3/2 +g(1))*1/2 +g(0)

        which is computed iteratively here.\n"""
        # Either all terms in self have odd order or all have even order, so
        # self's square's terms are all of even order:
        grand = (self * self).unafter(square)
        n = grand.rank
        assert n == self.rank, grand
        ans = grand.coefficient(n)
        while n > 0:
            n -= 1
            if isinstance(ans, int):
                ans *= 2 * n + 1
                ans, r = divmod(ans, 2)
                if r: ans += .5
            else:
                ans *= n + .5
            ans += grand.coefficient(n)

        return ans

    @classmethod
    def normalised(cls, n):
        h = cls(n, 1)
        return cls(n, h.scale**.5)

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
        e = gcd(*f)
        return enumerate(x // e for x in f)
    del factorial

    def _lazy_get_scale_(self, ig, linear=Polynomial.power(1)):
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
