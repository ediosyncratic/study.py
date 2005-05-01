"""Relativistic constant acceleration.

Theory: see http://www.chaos.org.uk/~eddy/physics/Lorentz.html#Zoom

$Id: zoom.py,v 1.6 2005-05-01 04:25:29 eddy Exp $
"""

from basEddy.units import Object, year, pound
import math

# the following is generally wrong !
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

        sinh(b) = a.t/c
        cosh(b) = 1 +a.x/c/c
        sinh(2.b) +2.b = 4.a.s/c
        tanh(b) = v/c

    For derivation, see http://www.chaos.org.uk/~eddy/physics/Lorenz.html#Zoom\n"""

    __obinit = Object.__init__
    __c = year.light / year
    def __init__(self, a=pound.force / pound, *args, **what):
	"""Initialises a Zoom object.

	Takes one optional argument, the body's acceleration; default is
	`standard gravity'.  Must be a quantity with suitable units.  Further
	arguments are as for an Object. """

	apply(self.__obinit, args, what)
	self.acceleration = a
	self.__rate = a / self.__c

    def times(self, distance):
	"""Returns (proper, external) time twople for given distance."""

        b = (1 +self.__rate * distance / self.__c).arccosh
        return ((2*b).sinh +2*b) * .25 / self.__rate, b.sinh / self.__rate

    def saturation(self, time):
	"""Returns speed, as fraction of light, at given proper time."""
	return self.hyperbole(time).tanh

    def distance(self, time):
	"""Returns distance travelled in a given proper time."""

        return (self.hyperbole(time).cosh -1) * self.__c / self.__rate

    def hyperbole(self, time):
        """Computes the hyperbolic angle b as a function of proper time."""

        # First approximation: sinh(2.b) = 2.b = 2.a.s/c
        b2 = 2 * time * self.__rate
        # The value sinh(b2) +b2 nees to equal, and a dummy last change:
        rough, last = 2 * b2, 0
        # Now apply Newton-Raphson
        while True:
            err = b2.sinh +b2 - rough
            if b2.width == 0:
                if err * 1e6 < last: break
                last = err
            elif err.evaluate(abs) < b2.width: break
            b2 = b2 - err / (b2.cosh +1)

        return .5 * b2

del Object, year, pound

_rcs_id = """
 $Log: zoom.py,v $
 Revision 1.6  2005-05-01 04:25:29  eddy
 Replaced methods with correct version, now that I've noticed the mistake I made !

 Revision 1.5  2005/04/25 07:38:26  eddy
 Removed theory section to web page.

 Revision 1.4  2003/07/06 17:48:53  eddy
 typo fix + minor presentation tweak

 Revision 1.3  2003/02/08 12:01:00  eddy
 Added some sub-section headings to the theory doc

 Revision 1.2  2002/02/18 17:57:55  eddy
 Tidy-up.  Removed gamma and asymptote remarks; but made .times() support
 asymptotes.  Added v as function of s and discussion of implied unit of
 time; changed saturation to deliver v as function of s.  Import stuff
 from units, rather than const, and del it once finished with it.

 Initial Revision 1.1  2002/02/18 17:02:23  eddy
"""
