"""Relativistic constant acceleration.

Theory
======

Proper time experienced by an observer moving along a trajectory is given by:

  ds**2 = dt**2 -(dr/c)**2

where s is the observer's proper time, in units of time, and the trajectory has
been described with reference to some orthonormal co-ordinates comprising a
time-like t (measured in units of time) and a space-like 3-vector r (measured in
units of length); dr**2 is implicitly a 3-vector (positive-definite) inner
product dr&middot;dr; c is the speed of light, in units of length/time.

Addition law for velocities
---------------------------

For alternative co-ordinates T time-like and R space-like, let v.c be the
velocity wrt of (t,r) of an object at rest wrt (T,R); decompose r and R into
components parallel and perpendicular to v, r = w+x, R=W+X; w and W are
perpendicular to v; x and X are parallel to v; then we obtain

   dW = dw,			dw = dW,
   dX = k.(dx -v.c.dt),		dx = k.(dX +v.c.dT),
   dT = k.(dt -v.dx/c),		dt = k.(dT +v.dX/c),

with 1/k**2 = 1 -v**2.

A third co-ordinate system, with (S,Y,Z) corresponding to (t,x,w) or (T,X,W),
for which an object at rest wrt (S,Y,Z) moves at speed u.c wrt (T,X,W), with u
parallel to v, gives

   dZ = dW = dw
   dY = h.(dX +u.c.dT) = h.k.(dx +v.c.dt +u.c.dt +u.v.dx)
      = h.k.(1+u.v).(dx +dt.(u+v).c/(1+u.v))
   dS = h.(dT +u.dX/c) = h.k.(dt +v.dx/c +u.(dx +v.c.dt)/c)
      = h.k.(1+u.v).(dt +dx.(u+v)/(1+u.v)/c)

with 1/h/h = 1 -u.u.  Now, this puts c.(u+v)/(1+u.v) in the role of the
velocity, wrt (t,x,w), of an object at rest wrt (S,Y,Z).  Note that

  1 -(u+v)**2/(1+u.v)**2
    = 1 - (u.u +2.u.v +v.v)/(1 +2.u.v +u.v.u.v)
    = (1 -u.u -v.v +u.u.v.v)/(1 +2.u.v +u.v.u.v)
    = (1-u.u).(1-v.v)/(1+u.v)**2
    = 1/(h.k.(1+u.v))**2

so (S,Y,Z) are, indeed, just related to (t,x,w) in the manner appropriate for
velocity c.(u+v)/(1+u.v).  Thus the `sum' of two velocities, c.u and c.v, is
c.(u+v)/(1+u.v).

Constant acceleration
---------------------

Now consider an object accelerating at constant rate a, parallel to v.  That its
acceleration is `constant' says that, over any short enough time interval q, its
velocity at the end of the interval, in the frame of reference with respect to
which it was at rest at the interval's start, is a.q plus tiny terms quadratic in
q.  Let c.v(t) be its velocity as a function of t; then, replacing u with a.q/c,
v(t) changes by

 ((a.q/c+v)/(1+a.q.v/c) -v) = (a.q/c +v -v -a.q.v.v/c)/(1+a.q.v/c)
   = a.q.(1-v.v)/(c+a.q.v)

in time interval q.  The denominator, c+a.q.v, tends to c as q tends to zero;
consequently, we have

   dv/dt = a.(1-v.v)/c, or
   a.dt/c = dv/(1-v.v)

which we can integrate: writing v = tanh(m), we get dv = (1-v.v).dm whence dm =
a.dt/c, so v = tanh(constant + a.t/c).  Shift the origin of our (t,x,w)
co-ordinate system (without changing which objects it deems to be at rest) so
that our object is at rest at t = x = w = 0.  Thus dx/dt = v.c = c.tanh(a.t/c);
since d(log(cosh(z))) = tanh(z).dz, we obtain

   dx = c.(c/a).d(log(cosh(a.t/c))), whence
   exp(a.x/c/c) = cosh(a.t/c)

Along our uniformly accelerating trajectory, we have

  ds**2 = dt**2 -(dx/c)**2 = dt**2.(1 -v(t)**2)
	= (1 -tanh(a.t/c)**2).dt**2
	= (dt/cosh(a.t/c))**2

whence ds = dt/cosh(a.t/c).  By considering a variable p defined by
tan(p) = sinh(a.t/c), we obtain

  dp.(1+tan(p)**2) = a.dt.cosh(a.t/c)/c, whence
  c.dp/a = dt.cosh(a.t/c)/(1+sinh(a.t/c)**2) = dt/cosh(a.t/c) = ds

and infer: tan(a.s/c) = sinh(a.t/c), measuring s from s=0 at t=x=w=0.  In
particular, this implies that, as t and x tend to infinity, a.s/c tends to pi/2;
i.e. s tends to c.pi/2/a.  Thus, at Earth standard gravity, just over c/year,
you can go as far as you like in 1.52 years, just under 556 days.  If you want
to stop at your destination, you'll be turning round half way, so taking 3 years
and (almost) 16 days.

Thus exp(2.a.x/c/c) = 1+tan(a.s/c)**2 = 1/cos(a.s/c)**2 and

   exp(a.x/c/c) = cosh(a.t/c) = 1/cos(a.s/c).

Note that v = tanh(a.t/c) = tan(a.s/c).cos(a.s/c) = sin(a.s/c).

From -pi/2 < a.s/c < pi/2 and standard gravity we can infer a unit of time,
pi*c/g = 3.04337 years, the time it takes to travel arbitrarily far and stop.

$Id: zoom.py,v 1.4 2003-07-06 17:48:53 eddy Exp $
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
 Revision 1.4  2003-07-06 17:48:53  eddy
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
