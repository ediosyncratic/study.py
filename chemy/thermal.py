"""Description of thermal radiation.

$Id: thermal.py,v 1.5 2008-04-03 07:22:46 eddy Exp $
"""
from physics import Thermal, Quantum, Vacuum
from study.value.object import Object

import math # will del later

# Experiment - due to move once a better home for it materialises
class Radiator (Object):
    """Descriptor for a black-body radiator. """
    __upinit = Object.__init__
    def __init__(self, temperature, *args, **what):
        apply(self.__upinit, args, what)
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

del math, Object, Quantum
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

    return apply(Radiator, (T,) + args, what)

from study.value.sample import Sample
from study.value.quantity import tophat, gausish
Human = Radiator(Kelvin * (309.5 + tophat),
                 oral=Centigrade(gausish * .5 + 36.8),
                 axillary=Centigrade(Sample(Sample.tophat * .9 + 36.45, best=36.6)),
                 __doc__="""Human body as a radiator.

The human body maintains a roughly constant temperature, so naturally radiates
as a body of that temperature, in so far as it's exposed.  Skin temperature is
doubtless less than one would measure in an arm-pit (an 'axillary' measurement,
commonly used in Russia and Poland), which is about .2 K lower than oral
measurement (common in the anglophone world); which, in turn, is about .5 K
below anal measurements; while core temperatures are presumed to be higher yet
than this.
""")
