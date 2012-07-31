"""Relativistic constant acceleration.

Theory: see http://www.chaos.org.uk/~eddy/physics/Zoom.html
"""

from study.value.units import Object, year, kg

class Zoom (Object):
    """Relativistic constant accelerator.

    Each method parameter described as a time or length should be a quantity
    with appropriate units - i.e. seconds for time or metres for length, though
    suitable alternative units (e.g. hours and miles, years and parsecs) will do
    fine.  Likewise, the initialiser's acceleration should have appropriate
    units.

    The relationships between proper time, s, and - relative to the initial rest
    frame - the time, t, instantaneous velocity v and distance, x, travelled are
    given in terms of a hyperbolic angle b by:

        b = a.s/c
        sinh(b) = a.t/c
        cosh(b) = 1 +a.x/c/c
        tanh(b) = v/c

    For derivation, see http://www.chaos.org.uk/~eddy/physics/Zoom.html\n"""

    __obinit = Object.__init__
    __c = year.light / year
    def __init__(self, a=kg.force / kg, *args, **what):
	"""Initialises a Zoom object.

	Takes one optional argument, the body's acceleration; default is
	`standard gravity'.  Must be a quantity with suitable units.  Further
	arguments are as for an Object. """

	self.__obinit(*args, **what)
	self.acceleration = a
	self.__rate = a / self.__c

    def times(self, distance):
	"""Returns (proper, external) time twople for given distance."""

        b = (1 +self.__rate * distance / self.__c).arccosh
        return b / self.__rate, b.sinh / self.__rate

    def saturation(self, time):
	"""Returns speed, as fraction of light, at given proper time."""
	return self.hyperbole(time).tanh

    def distance(self, time):
	"""Returns distance travelled in a given proper time."""

        return (self.hyperbole(time).cosh -1) * self.__c / self.__rate

    def hyperbole(self, time):
        """Computes the hyperbolic angle b as a function of proper time."""
        return time * self.__rate

del Object, year, kg
