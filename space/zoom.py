"""Relativistic constant acceleration.

Theory: see http://www.chaos.org.uk/~eddy/physics/Lorentz.html

$Id: zoom.py,v 1.5 2005-04-25 07:38:26 eddy Exp $
"""

from basEddy.units import Object, year, pound
import math

class Zoom (Object):
    """Relativistic constant accelerator.

    Each method parameter described as a time or length should be a quantity
    with appropriate units - i.e. seconds for time or metres for length, though
    suitable alternative units (e.g. hours and miles, years and parsecs) will do
    fine.  Likewise, the initialiser's acceleration should have appropriate
    units.

    The relationship between proper time, s, and - relative to the initial rest
    frame - the time, t, and distance, x, travelled is:

        exp(a.x/c/c) = cosh(a.t/c) = 1/cos(a.s/c).

    For derivation, see file documentation. """

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
	"""Returns (proper, external) time twople for given distance.

	Note on solving cosh(p) = z, needed in solving for t given cosh(a.t/c):
	decompose this into solving exp(p) = u, u+1/u = 2.z; the latter is just

            0 = u.u -2.z.u +1 = (u-z)**2 +1 -z.z

	whence u-z = sqrt(z.z -1) and p = log(u) = log(z +sqrt(z.z -1)). """

	try:
	    z = math.exp(distance * self.__rate / self.__c)
	    return (math.acos(1/z) / self.__rate,
		    math.log(z + (z-1)**.5 * (z+1)**.5) / self.__rate)
	except OverflowError:
	    # limit as distance tends to infinity:
	    return (math.pi / 2 / self.__rate,
		    distance / self.__c + math.log(2) / self.__rate)

    def saturation(self, time):
	"""Returns speed, as fraction of light, at given proper time."""
	return math.sin(time * self.__rate)

    def distance(self, time):
	"""Returns distance travelled in a given proper time."""

	assert abs(time * self.__rate) <= math.pi / 2, \
	       'Proper time for uniformly accelerating body exceeds infinite limit !'

	return -(self.__c / self.__rate) * math.log(math.cos(time * self.__rate))

del Object, year, pound

_rcs_id = """
 $Log: zoom.py,v $
 Revision 1.5  2005-04-25 07:38:26  eddy
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
