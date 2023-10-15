"""Description of thermal radiation.

See study.LICENSE for copyright and license information.
"""
from physics import Thermal, Quantum, Vacuum
from study.value.object import Object
from study.value.units import \
    turn, radian, Kelvin, metre, second, Hertz, Joule, Newton

import math # will del later

# Experiment - due to move once a better home for it materialises
class Radiator (Object):
    """Descriptor for a black-body radiator.

    Pseudo-contructor:

      fromPeak() -- Radiator with maximal radiance at specified value

    Properties:

      frequency -- frequency of maximal radiance
      temperature -- the body's temperature
      total -- total radiance (power output per unit surface area)
      wavelength -- wavelength of maximal radiance

    Method:

      spectral() -- spectral radiance at a given wavelength or frequency

    Note that, because the latter is a density with respect to the parameter
    given (wavelength or (possibly angular) frequency), the wavelength and
    frequency at which it is maximal correspond to different points in the
    spectrum; their product is about c/1.76 instead of c.  This likewise affects
    the frequency and wavelength at which it's maximal, hence also fromPeak()'s
    interpretation of its parameter.

    == Spectral parameters ==

    Positions in the electromagnetic spectrum are parameterised by

      * frequency (cycles/time), wave covector (cycles/length), energy, momentum or
      * wavelength (length/cycle), period (time/cycle),

    any of which implies all the others.  The energy and momentum are related to
    frequency and wavelength by Planck's and de Broglie's laws, respectively;
    period is the inverse of frequency, wavelength the inverse (of the
    magnitude) of the wave covector and the product of frequency and wavelength
    is the speed of light.  Within each list, the quantities are proportional to
    one another and thus inversely proportional to those in the other list.

    Orthodoxy specifies frequencies and wavelengths in inverse time and length
    units, respectively, treating cycle as dimensionless; however, since both
    'whole cycle' and radian-based 'angular' versions are used (the latter
    tacitly interpreting cycle as turn, so scaling up or down by 2*pi), this
    leads to ambiguity.  The methods of this class that identify a position in
    the spectrum thus do so using frequency and wavelength with units that
    include angle, treating turn as cycle.

    However, if these methods are passed a simple inverse time, this is
    interpreted as a full-cycle frequency, so multiplied by turn; likewise, a
    simple length is interpreted as a length per full cycle, so divided by turn.
    Likewise, orthodoxy specifies 'wavenumber' (magnitude of wave covector) as a
    simple inverse length and period as a simple time; these, too, are accepted
    and scaled by turn, if used to identify a spectral position.  If 'angular'
    versions of frequency, period, wavelength or wavenumber are to be used, the
    caller can scale by radian (in place of turn) or its inverse to get them
    into the form assumed by this class.

    Where spectral() scales its parameter by a turn to map orthodox quantities
    to the angle-aware ones it uses, it does suitably undo that scaling for its
    return value, to ensure this actually is radiance per unit variation in the
    passed parameter (interpreted as turn-based, rather than radian-based),
    rather than in the angle-aware equivalent.  Callers doing their own
    radian-scaling to use angular forms of orthodox units shall get back a
    result with a matching angular unit, that they can remove by scaling by
    radian or its inverse to get the radiance per unit variation in the angular
    form of the orthodox unit.\n"""
    __upinit = Object.__init__
    def __init__(self, temperature, *args, **what):
        """Sets up a thermal black-body Radiator object.

        First argument is the temperature of the body.  Any further arguments
        are forwarded to study.value.object.Object's constructor.\n"""
        self.__upinit(*args, **what)
        self.__temperature = temperature

    @classmethod
    def fromPeak(cls, value):
        """Construct instance with maximal radiance at the given input.

        Single parameter should identify a position in the electromagnetic
        spectrum; see the class doc's section on spectral parameters.  This
        value must be positive (of the relevant kind).

        Note that the spectral radiance's peak depends on the type of the
        quantity given.  If it is specified by wavelength, the result's
        .wavelength will be (within arithmetic roundings) the given wavelength,
        but the frequency of light with this wavelength is not its .frequency;
        see these properties where specified."""
        return cls(cls.__to_temperature(value))

    @property
    def temperature(self):
        return self.__temperature

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
        """Constant needed by spectral et al.: h / (k*T)."""

        return hoverk / self.__temperature

    def spectral(self, value, exp=math.exp, pi=math.pi, h=Quantum.h, c=Vacuum.c):
        """Returns spectral radiance.

        Single parameter should identify a position in the electromagnetic
        spectrum; see the class doc's section on spectral parameters.  This
        value must be positive (of the relevant kind).

        Returns the radiance (power output per unit area) of the body, per unit
        variation in the given argument, produced in a narrow portion of the
        spectrum about the given position in the spectrum.

        Since this is `per unit', the answer depends on which argument you
        supplied not only for how the argument is read but also in how the power
        density varies with that argument.\n"""

        f, scale, index = self.__ingest(value)
        base = 2 * h / (exp(self._hoverkT * f) -1) / c**2
        return base * f**(3 + 2 * index) / scale

    def _lazy_get_frequency_(self, ignored, cycle = turn):
        """The frequency at which self.spectral() peaks.

        This is proportional to temperature.  To get the peak of the energy or
        angular frequency distribution, just multiply this by Planck's constant
        or two pi.

        As spectral(frequency=f) is proportional to f**3 / (exp(s*f)
        -1), where s = h/k/T is a time, its derivative varies as

            3*f**2/(exp(s*f) -1) -s*exp(s*f)*f**3/(exp(s*f) -1)**2

        at whose roots exp(-s*f) +s*f/3 = 1.
        """
        return cycle * self.__splitexp[0] / self._hoverkT

    def _lazy_get_wavelength_(self, ignored, c = Vacuum.c, cycle = turn):
        """The wavelength at which self.spectral() peaks.

        This is inversely proportional to temperature.

        As spectral(wavelength=w) is inversely proportional to w**5 *
        (exp(s/w) -1) where s = h*c/k/T is a length, its derivative
        varies as

        s * exp(s/w) / w**7 / (exp(s/w) -1)**2 -5 / w**6 / (exp(s/w) -1)

        at whose roots exp(-s/w) +s/w/5 = 1.
        """
        return c * self._hoverkT / self.__splitexp[1] / cycle

    # Grungy implementation details:
    def __splitexp(scale, exp = math.exp, tol = 1e-9): # Temporary tool, over-written
        """Solve exp(-x) +x/scale = 1 to within 1e-9.

        This has a fatuous solution at x = 0.  We're looking for another zero of
        f(x) = x/scale +exp(-x) -1 with f'(x) = 1/scale -exp(-x) and f''(x) =
        exp(-x) always positive.

        For scale > 1, f is decreasing at its fatuous zero, has a minimum at x =
        log(scale) and is necessarily positive for x >= scale, so must have a
        second zero somewhere between log(scale) and scale.

        Solved using Newton-Raphson, with initial value obtained by hard-coding
        what a first step from x = scale would produce; it converges in two or
        three iterations, for the scales we care about (each of which has
        splitexp(scale) slightly less than scale)."""
        assert scale > 1
        x, r = scale * (1 -1 / (exp(scale) - scale)), 1. / scale
        emx = exp(-x)
        error = x * r +emx -1
        while abs(error) > tol:
            x -= error / (r -emx)
            emx = exp(-x)
            error = x * r +emx -1
        return x

    __splitexp = (__splitexp(3), __splitexp(5))
    @classmethod
    def __to_temperature(cls, value, hoverk = Quantum.h / Thermal.k):
        """Temperature at which the radiance peaks at the given value.

        This is radiance per variation in the given value's quantity.
        """
        # If value is wavelength, this gives the frequency of that wavelength,
        # of peak spectral radiance per unit wavelength, not the frequency of
        # peak spectral radiance per unit frequency.  That wavelength is
        # c * h / k / T / s[1], where s = self.__splitexp, so the frequency is
        # s[1] * k * T / h; similar for the others proportional to wavelength.
        # Otherwise, the frequency is s[0] * k * T / h
        f, ignored, index = cls.__ingest(value)
        return f * hoverk / cls.__splitexp[index]

    @staticmethod
    def __ingest(value, freq = Hertz * turn, Hz = Hertz, J = Joule,
                 wavlen = metre / turn, length = metre, Ns = Newton * second,
                 cycle = turn, h = Quantum.h, c = Vacuum.c):
        """Identify parameter by its value type.

        Input value is as for spectral().  Returns a triple of cycle-frequency
        (inverse time), a scaling and an index.

        The last is 1 if the parameter was interpreted as a wavelength, 0 if as
        an energy or frequency (it is an index into self.__splitexp).  When that
        is 1, the scaling is a constant by which the value was divided to get
        the frequency; otherwise, it is the constant the value was divided by to
        get the frequency.

        When frequency is value / scale, radiance per value is radiance per
        (freqency * scale), which is (radiance per frequency) divided by scale.
        When frequency is scale / value, an interval of frequency is
        scale/value**2 times an interval of value, making an interval of value
        scale / frequency**2 times the interval of frequency.  Thus a radiance
        per value is a (radiance per frequency) times squared frequency, divided
        by scale.  Thus, in both cases, spectral() divides the radiance per
        frequency by scale, with the index = 1 case adding two to the power of
        frequency.\n"""

        # Monstrous trial and error:
        try:
            if value > 0 * freq:
                return value / cycle, cycle, 0
        except TypeError:
            try:
                if value * freq > 0:
                    return 1 / cycle / value, 1 / cycle, 1
            except TypeError:
                try:
                    if value > 0 * J:
                        return value / h, h, 0
                except TypeError:
                    try:
                        if value > 0 * Ns:
                            # p = E/c = f*h/c
                            scale = h / c
                            return value / scale, scale, 0
                    except TypeError:
                        try:
                            if value > 0 * wavlen:
                                scale = c / cycle
                                return scale / value, scale, 1
                        except TypeError:
                            try: # wave covector
                                if value * wavlen > 0:
                                    scale = cycle / c
                                    return value / scale, scale, 0
                            except TypeError:
                                # Presume cycle if unit of angle missing:
                                try:
                                    if value > 0 * Hz:
                                        return value, 1, 0
                                except TypeError:
                                    try:
                                        if value * Hz > 0:
                                            return 1. / value, 1, 1
                                    except TypeError:
                                        try:
                                            if value > 0 * metre:
                                                return c / value, c, 1
                                        except TypeError:
                                            try:
                                                if value * metre > 0:
                                                    return value * c, 1 / c, 0
                                            except TypeError:
                                                raise TypeError(
                 "Should be frequency, wave covector, energy, momentum, "
                 "wavelength or period", value)
        raise ValueError("Should be positive", value)

del math, Object, Quantum, Vacuum
from study.value.units import Centigrade

def radiator(temperature, *args, **what):
    """Wrap Radiator with provision for -ve temperatures (in Centigrade).

    This isn't the right way to wrap it, but if any wrapping's to be done, it
    should be done here, not by bodging Radiator.__init__() !\n"""

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
