"""Description of space elevators.

See http://www.chaos.org.uk/~eddy/project/space/ladder.html
See study.LICENSE for copyright and license information.
"""
from math import exp
from home import Earth

from study.value.units import kg, m, mega, Pascal, Quantity, Sample, Object
def V(S, D, MPa=mega * Pascal, rho=kg/m**3, Q=Quantity, W=Sample):
    v = (Q(W(S), MPa) / Q(W(D), rho)) ** .5
    return v.low, v.high
del kg, m, mega, Pascal, Sample

class Ladder (Object):
    """A ladder out of a gravitational well.

    A Ladder object describes an idealised structure which stretches from the
    surface of a planet out to beyond its synchronous orbit (i.e. the orbit in
    which a freely falling body remains directly above a fixed point on the
    planet's surface).  The bottom will be clamped to the planet's surface; the
    top will have a counter-weight attached to it somewhere beyond synchronous
    orbit.

    The ladder is here presumed to be made of a single material, all parts of
    which are under equal stress.  This stress level and the density of the
    material will be the ladder's .stress and .density attributes.

    Where a parameter to a method, below, is described as a `radial coordinate'
    it is presumed dimensionless and measured in units of the synchronous
    orbital radius of the planet.  Anything described as `at orbit' thus happens
    where the radial coordinate is 1; the ladder's bottom will be less than one,
    and usually correspond to the planet's surface, its top should really be
    greater than 1 but will default to 1 unless specified.

    Print Ladder.theory for more details. """

    theory = """Theory of space escalators.

    This fills in details not covered by
    http://www.chaos.org.uk/~eddy/project/space/ladder.html
    and presumes the notation and language thereof.

    Formulae:

      G*M = w**2 * R**3
      g = G*M/(R*b)**2 = (w/b)**2 * R
      v*v = (w*R)**2 * D / S is dimensionless
      A(u)/A(1) = exp(v*v*(1-u)*((1+u)/2 -1/u))
      volume = R*integral(: A(u)&larr;u; b<u<B :)

    Relationship to attributes of a Ladder():
      stress, density, v, R = S, D, v, R
      thin = (: A(u)/A(1) &larr;u; b&lt;u&lt;B :)
      shrink = volume / R / A(1)
      load = S.A(b)/g/D/volume = (b/v)**2 * thin(b) / shrink
    This last is called Q in ladder.html\n"""

    __obinit = Object.__init__

    def __init__(self, S, D, top=1, planet=Earth, bot=None, Q=Quantity, **what):
	"""Initialises a ladder object.

        Required arguments, describing mechanical properties of the material of
	which the ladder is to be built (to be spacified as Quantity objects,
	see study.value.quantity):
          S -- ultimate tensile stress
          D -- density

        Optional arguments:
          top -- the outer radial coordinate of your ladder; default is 1 but
	         values nearer 1.3 would be more realistic.
          planet -- the Planet for which you are building a ladder; default is Earth.
          bot -- the inner radial coordinate of the ladder; default uses the
	         planet's surface radius.

        Both top and bot; may be given either as dimensionless radial
        coordinates or as lengths (that is, Quantity objects with dimensions of
        length), in which case they're divided by the synchronous orbital
        radius.\n"""

	self.__obinit(**what)
	self.stress, self.density = S, D
	sync, surf, densile = planet.synchronous, planet.surface, S / D
	self.spin, self.R = surf.spin, sync.radius
	self.__kk = Q(sync.speed**2 / densile)
	# __kk *must* be a Quantity for thin's use of evaluate ...

	if bot is None: bot = surf.radius / self.R
	self.__ends = (self.__radial(bot), self.__radial(top))

    def __radial(self, value):
        try: value + 1
        except TypeError:
            try: value + self.R
            except TypeError: pass
            else: return value / self.R
        else: return value

        raise TypeError("Ladder() top or bot radial parameter",
                        value,
 "length or the dimensionless result of dividing a length by synchronous orbital radius")

    def thin(self, u, e=exp):
	"""Area of ladder at radial coordinate u, in units of that at orbit. """
	return (self.__kk * (1-u) * (.5 +u/2 -1/u)).evaluate(e)

    def _lazy_get_v_(self, ignored): return self.__kk ** .5
    def _lazy_get_length_(self, ignored):
	b, t = self.__ends
	return self.R * (t-b)

    def _lazy_get__integrator_(self, ignored, tool=[]):
        try: return tool[0]
        except IndexError: pass # 1st call; initialise tool
        from study.maths import integrate
        I = integrate.Integrator
        tool.append(I)
        return I

    def _lazy_get_shrink_(self, ignored):
	"""The integral defining volume of a ladder to the stars.

	The volume of ladder is the synchronous orbital radius times the
	cross-sectional area at orbit times the integral of self.thin(u) for u
	between self.__ends.  This function computes that integral, which is
	dimensionless.  Multiply it by self.R to get the ratio of volume to
	orbital area. """

	b, t = self.__ends
	return self._integrator(self.thin).between(b, t)

    def _lazy_get_moment_(self, ignored):
        """Relative moment of inertia.

        This is the ratio of the actual moment of inertia - of the cable,
        without its counter-weight - to the moment of inertia it *would* have if
        its entire mass were at geostationary orbit. """

        b, t = self.__ends
        return self._integrator(lambda u, f=self.thin: f(u) * u**2
                                ).between(b, t) / self.shrink

    def _lazy_get_ends_(self, ignored):
	bot, top = self.__ends
	return self.thin(bot), self.thin(top)

    def area(self, ciel):
	"""Tells you how fat a ladder you can make given how much of your material.

	Single required argument is how much of the building material your
	budget will let you buy, expressed as a volume.  Returns the
	cross-sectional area, at orbit.  Scale by self.ends to get the areas at
	bottom and top; scale by self.thin(u) to get area at radial coordinate
	u. """

	return ciel / self.R / self.shrink

    def _lazy_get_burden_(self, ignored):
	"""Total mass of cable per unit orbital cross-section."""
	return self.shrink * self.R * self.density

    def _lazy_get_total_(self, ignored):
	return self.burden + self.bauble + self.lift

    def _lazy_get_parts_(self, ignored):
	"""3-tuple of (cable, counter, payload) fractions of total mass."""

	s, w, p = self.shrink, self._counter, self._load
	t = s + w + p
	return s/t, w/t, p/t

    def __pull(self, u):
	"""Mass-equivalent of tension, as fraction of total mass.

	Single argument is a radial coordinate.

	The (gravity - centrifugal) force, a.k.a. weight, per unit mass at given
	radial co-ordinate is w.w.(R.u -R/u/u) outwards (so it'll be negative
	for u<1).  The tension in the cable is S.A(u) = S.thin(u).A(1), equal to
	the weight of a mass S.A(1).thin(u)/(w.w.R)/(u-1/u/u).  The total mass
	of our cable is D.A(1).R.shrink; dividing the former by the latter gives:

	    T(u)/M = (S/D/(w.R)**2).thin(u)/(u-1/u/u)/shrink

	This function returns the result of leaving out shrink from this, which
	happens to be useful for various purposes (without introducing the
	inaccuracy usually present in shrink). """

	return self.thin(u) / (u -1/u/u) / self.__kk

    # unshrunk values
    def _lazy_get__load_(self, ignored): return -self.__pull(self.__ends[0])
    def _lazy_get__counter_(self, ignored): return self.__pull(self.__ends[1])

    # masses as fractions of total mass of cable
    def _lazy_get_load_(self, ignored): return self._load / self.shrink # bot
    def _lazy_get_countermass_(self, ignored): return self._counter / self.shrink # top

    # masses per unit `area at orbit'
    def _lazy_get_lift_(self, ignored): return self._load * self.density * self.R # bot
    def _lazy_get_bauble_(self, ignored): return self._counter * self.density * self.R # top

del exp, Earth, Object, Quantity
