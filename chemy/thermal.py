"""Description of thermal radiation.

See study.LICENSE for copyright and license information.
"""
from physics import Thermal, Quantum, Vacuum
from study.value.object import Object

import math # will del later

# Experiment - due to move once a better home for it materialises
class Radiator (Object):
    """Descriptor for a black-body radiator.

    Properties:
      total -- total power output per unit surface area
      frequency -- frequency of maximal radiance
      wavelength -- wavelength of maximal radiance

    Method:
      spectral() -- spectral radiance at a given wavelength or frequency

    Note that, because the latter is a density with respect to the
    parameter given (wavelength or (possibly angular) frequency), the
    wavelength and frequency at which it is maximal correspond to
    different points in the spectrum; their product is about c/1.76
    instead of c.\n"""
    __upinit = Object.__init__
    def __init__(self, temperature, *args, **what):
        self.__upinit(*args, **what)
        self.__temperature = temperature

    def _lazy_get_total_(self, ignored, S=Thermal.Stefan):
        """Computes the power output per unit area of exposed surface.

        To compute total power output, allowing (if the body isn't convex) for
        re-absorption of some of what it emits, I suspect you should multiply
        radiance by the surface area of the body's convex hull, rather than its
        actual surface area.  The product is then the rate of loss of energy,
        due to thermal radiation, of the body. """

        # Stefan-Boltzmann law:
        return self.__temperature**4 * S

    def _lazy_get__hoverkT_(self, ignored, hoverk=Quantum.h / Thermal.k):
        """Constant needed by spectral: h / (k.T)."""

        return hoverk / self.__temperature

    def spectral(self, nu=None, frequency=None, wavelength=None,
                 exp=math.exp, h=Quantum.h, c=Vacuum.c, pi=math.pi):
        """Returns spectral radiance.

        Takes any one of three arguments, in the order nu, frequency,
        wavelength, the former being the `angular' frequency of the radiation,
        frequency/(2*pi).  If more than one is given, the first in the above
        ordering (which is their positional order) is used, any others are
        ignored.  Returns the radiance (power output per unit area) of the body,
        per unit variation in the given argument, produced in a narrow portion
        of the spectrum about the given position in the spectrum.

        Since this is `per unit', the answer depends on which argument you
        supplied not only for how the argument is read but also in how the power
        density varies with that argument. """

        if nu: return self.spectral(frequency = nu * 2 * pi) / 2 / pi
        elif frequency: f = frequency
        elif wavelength: f = c / wavelength
        else: raise ValueError, 'Either zero input or none given: unphysical.'

        base = 2 * h / (exp(self._hoverkT * f) - 1)
        if frequency: return base * f**3 / c**2
        else: return base * c**2 / wavelength**5

    def splitexp(scale, exp = math.exp, tol = 1e-9): # Temporary tool
        """Solve exp(-x) +x/scale = 1 to within 1e-9.

        This has a fatuous solution at x = 0.  We're looking for
        another zero of f(x) = x/scale +exp(-x) -1 with f'(x) =
        1/scale -exp(-x) and f''(x) = exp(-x) always positive.

        For scale > 1, f is decreasing at its fatuous zero, has a
        minimum at x = -log(1/scale) and is necessarily positive for x
        >= scale, so must have a second zero somewhere between
        -log(1/scale) and scale.

        Solved using Newton-Raphson, with initial value obtained by
        hard-coding what a step from x = scale would produce; it
        converges in two or three iterations, for the scales we care
        about (each of which has splitexp(scale) slightly less than
        scale)."""
        assert scale > 1
        x, r = scale * (1 -1 / (exp(scale) - scale)), 1. / scale
        emx = exp(-x)
        error = x * r +emx -1
        while abs(error) > tol:
            x -= error / (r -emx)
            emx = exp(-x)
            error = x * r +emx -1
        return x

    def _lazy_get_frequency_(self, ignored, scale = splitexp(3)):
        """The frequency at which self.spectral() peaks.

        This is proportional to temperature.

        As spectral(frequency=f) is proportional to f**3 / (exp(s*f)
        -1), where s = h/k/T is a time, its derivative varies as

            3*f**2/(exp(s*f) -1) -s*exp(s*f)*f**3/(exp(s*f) -1)**2

        at whose roots exp(-s*f) +s*f/3 = 1.
        """
        return scale / self._hoverkT

    def _lazy_get_wavelength_(self, ignored, scale = Vacuum.c / splitexp(5)):
        """The wavelength at which self.spectral() peaks.

        This is inversely proportional to temperature.

        As spectral(wavelength=w) is inversely proportional to w**5 *
        (exp(s/w) -1) where s = h*c/k/T is a length, its derivative
        varies as

        s * exp(s/w) / w**7 / (exp(s/w) -1)**2 -5 / w**6 / (exp(s/w) -1)

        at whose roots exp(-s/w) +s/w/5 = 1.
        """
        return self._hoverkT * scale

    del splitexp

del math, Object, Quantum, Vacuum
from study.value.units import Kelvin, Centigrade

def radiator(temperature, *args, **what):
    """Wrap Radiator with provision for -ve temperatures (in Centigrade).

    This isn't the right way to wrap it, but if any wrapping's to be done, it
    should be done here, not by bodging Radiator's __init__ method !\n"""

    try:
        if temperature > 0: T = temperature * Kelvin
        else: T = Centigrade(temperature)
    except TypeError: # temperature has units, so > 0 check bombed.
        if temperature / Kelvin > 0: T = temperature # TypeError if wrong units
        else: T = temperature + Centigrade(0)

    if T / Kelvin < 0:
        raise ValueError, 'Negative temperature, even after Centigrade coercion'

    return Radiator(T, *args, **what)

from study.value.quantity import Quantity
Human = Radiator(Quantity.fromDecimal(309.5, 1, None, Kelvin),
                 oral=Centigrade(Quantity.gaussian(36.8, .5)),
                 axillary=Centigrade(Quantity.flat(36, 36.9, 36.6)),
                 __doc__="""Human body as a radiator.

The human body maintains a roughly constant temperature, so naturally radiates
as a body of that temperature, in so far as it's exposed.  Skin temperature is
doubtless less than one would measure in an arm-pit (an 'axillary' measurement,
commonly used in Russia and Poland), which is about .2 K lower than oral
measurement (common in the anglophone world); which, in turn, is about .5 K
below anal measurements; while core temperatures are presumed to be higher yet
than this.
""")
del Quantity, Centigrade
