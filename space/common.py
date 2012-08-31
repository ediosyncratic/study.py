# -*- coding: iso-8859-1 -*-
"""Base classes and common types for astronomical data.

See study.LICENSE for copyright and license information.
"""
from study.value.units import Quantity, arc, pi, Object, second

class Discovery (Object):
    __upinit = Object.__init__
    def __init__(self, who, year, **what):
        what.update(year=year, who=who)
        self.__upinit(**what)

    def __repr__(self): return '%s (%d)' % (self.who, self.year)

class Spin (Object):
    __upinit = Object.__init__
    def __init__(self, period, tilt=Quantity.below(90), **what):
        """Initialises a Spin object.

        Takes two positional arguments:

          period -- a Quantity with dimensions of time; the period of the
          rotation described; stored as self.period.

          tilt -- angle, in degrees, between the axis of spin and some fixed
          direction provided by your context; a.k.a. inclination.  Should lie
          between 0 and 90.  Default is an error bar covering this whole range,
          meaning `unspecified'.  Is stored as self.tilt; can be accessed as an
          angle value (i.e. multiplied by arc.degree) as self.inclination.

        Takes arbitrary keyword arguments after the manner of Object (q.v.).
        """
        self.__upinit(**what)
        self.period, self.tilt = self.__time(period), tilt

    def __time(self, t, zero=0 * second): return t + zero
    # need to do arithmetic on period to:
    #  * check it really is a time and
    #  * ensure new object, since we'll do .observe() to it (see Orbit).
    # Creating Earth.orbiter(day) shouldn't give day an error-bar !

    def _lazy_get_inclination_(self, ignored, unit=arc.degree): return self.tilt * unit
    def _lazy_get_omega_(self, ignored, unit=2*pi): return unit / self.period

class Round (Object): # handy base-class for round things
    __upinit = Object.__init__
    def __init__(self, radius, spin, **what):
        self.__upinit(**what)
        self.radius, self.spin = radius, spin

    def _lazy_get_circumference_(self, ignored, twopi=2*pi): return twopi * self.radius
    def _lazy_get_diameter_(self, ignored): return 2 * self.radius
    def _lazy_get_speed_(self, ignored): return self.radius * self.spin.omega
    def _lazy_get_acceleration_(self, ignored): return self.spin.omega**2 * self.radius
    _lazy_get_velocity_ = _lazy_get_speed_
    def _lazy_get_potential_(self, ignored): return - self._GM / self.radius

class Orbit (Round):
    __upinit = Round.__init__
    def __init__(self, centre, radius, spin, eccentricity=None, tilt=None, **what):
        """Constructor.

        Required arguments:
          centre -- a body.Body instance at a focus of the orbit
          radius -- semi-major axis of the orbit
          spin -- a Spin object describing the rotation; or None to force
                  computation from other data (you'll get a ValueError if you
                  haven't supplied enough other data).

        Optional arguments:
          eccentricity -- pure number or default, None, provoking an abuse of
                          radius; its high and low shall be interpreted as
                          semi-major and semi-minor axes, from which to infer
                          eccentricity.
          tilt -- when spin is given as None, and this is supplied, this shall
                  be forwarded to the constructor for the inferred Spin object.

        Note that semi-major axis is the radius to use here: this is [1] the
        time-average of the radius of the orbit; it is also [2] the
        eccentricity-adjusted radius whose cube is proportional to the square of
        the orbit's period.

        Some day I should go over to using a vector to encode the eccentricity -
        see [2]'s Q, whose magnitude is the scalar eccentricity.  The vector to
        use is obtained as follows: multiply the velocity of the orbiting body
        (relative to the body it orbits) by the semi-latus recum; divide by J,
        the total angular momentum times by the sum of the inverses of the
        masses; and subtract a unit vector perpendicular to the radius, in the
        forward direction of the orbit.  This vector is constant and in the
        direction of the orbit's velocity at closest approach.

        [1] http://csep10.phys.utk.edu/astr161/lect/history/kepler.html
        [2] http://www.chaos.org.uk/~eddy/physics/cocentric.html\n"""
        ws = []
        try: ws.append(what['speed'] / radius)
        except KeyError: pass

        try: GM = centre._lazy_get_GM_('GM') # compute but don't store ...
        except AttributeError: pass
        else: ws.append((GM / radius**3)**.5)

        if spin is None and not ws: # needed by __spin
            raise ValueError('No spin specified or computeble for orbit', centre, radius, what)

        if eccentricity is None:
            # Invent one based on blur in radius: b = (1+e)/(1-e)
            # => b*(1-e) = 1+e => b-1 = e*(1+b) => e = (b-1)/(1+b)
            b = radius.high / radius.low
            eccentricity = (b - 1) / (1 + b)
            # ideally invent a suitable error bar ...
            eccentricity.document("""Eccentricity guessed from error bar on radius.  Not to be trusted !""")

        self.__upinit(radius, self.__spin(spin, ws, tilt), **what)
        self.centre, self.eccentricity = centre, eccentricity

    def __spin(self, maybe, ws, tilt, squish=lambda x, tp=2*pi: tp/x):
        periods = map(squish, ws)
        if maybe is None:
            assert periods # tested by constructor
            if tilt is None: maybe = Spin(periods[0]) # use Spin's default tilt
            else: maybe = Spin(periods[0], tilt)
            periods = periods[1:]

        for it in periods:
            maybe.period.observe(it)

        return maybe

    def _lazy_get_iBode_(self, ignored):
        """Generalized Titius-Bode index.

        For the Solar System, the Titius-Bode law says that planet i+2 has
        orbital radius roughly k0 + k1 * b**i where k0 is about .4 AU, k1 is
        about .3 AU and b is 2; except that it's so inaccurate for Mercury that
        we special-case it by using 0 in place of b**-1.  However, planet 5
        would then fall roughly where the asteroid belt is and the planets from
        Jupiter outwards thus have values of i that are one higher than one
        might have expected.  We thus need to introduce an artificial 'Bode
        index' for each planet that's the i we need to feed to the above formula
        to get the right radius (for Mercury, it's minus infinity ...).

        We can generalize this for any set of orbits around a common centre and
        ask what values of k0, k1 and b will give the extant satellites' orbital
        radii as k0 + k1 * b**i for various values of i; and each satellite then
        has the relevant i as its 'Bode index'.

        Doing this in general depends on auto-detecting the values of k0, k1 and
        b from the available orbital radii; see Planetoid._lazy_get_Bode_ for
        on-going attempts to solve this problem ...\n"""

        return self.centre.Bode.index(self.radius)

    def _lazy_get_center_(self, ignored): return self.centre # map to US spelling ...
    def _lazy_get_tidal_(self, ignored): return 2 * self.spin.omega**2
    def _lazy_get__GM_(self, ignored): return self.spin.omega**2 * self.radius**3
    # Strictly, these two are specific to when centre is Sun (distances from Helios):
    def _lazy_get_perihelion_(self, ignored): return self.radius.low
    def _lazy_get_aphelion_(self, ignored): return self.radius.high
    # (and I hope I got them the right way round !)
    def _lazy_get_energy_(self, ignored): # per unit mass in this orbit
        return self.potential + .5 * self.speed**2

class Spheroid (Object):
    # a bit like Round (q.v.) but lumpier ...
    __upinit = Object.__init__
    def __init__(self, major, minor=None, minim=None, **what):
        self.__upinit(**what)
        if minor is None: minor = major
        if minim is None: minim = minor
        self.__semis = major, minor, minim

    def _lazy_get_volume_(self, ignored, ftp=4*pi/3):
        return ftp * self._cuboid

    def _lazy_get_radius_(self, ignored, third=1./3):
        """Geometric mean radius."""
        # should give it an error bar reflecting __semis
        return self._cuboid ** third

    def _lazy_get__cuboid_(self, ignored):
        maj, nor, nim = self.__semis
        return maj * nor * nim

    # could, in principle, estimate area, too ...

class SurfacePart (Object):
    __upinit = Object.__init__
    def __init__(self, *parts, **what):
        self.__parts = parts
        for part in parts:
            what[part.name], part.__parent = part, self
            part.borrow(self)

        self.__upinit(**what)

    pro_rata = ( 'rainfall', ) # anything else roughly proportional to area ...
    def proportion(self, prop):
        all = getattr(self.__parent, prop) # can't guess from parent unless it has it, too
        # could subtract off contributions from parent's parts with known prop ...
        return self.area * all / self.__parent.area

    for nom in pro_rata:
        exec '_lazy_get_%s_ = proportion' % nom
    del proportion

    # Fails to deliver Earth.surface.Ocean.volume :-(
    extensives = ( 'area', 'mass', 'volume', 'population' )
    def extensive(self, prop):
        ans = None
        for part in self.__parts:

            try: this = getattr(part, prop)
            except AttributeError: continue

            if ans is None: ans = this
            else: ans = ans + this

        return ans

    for nom in extensives:
        exec '_lazy_get_%s_ = extensive' % nom

    _unborrowable_attributes_ = extensives + pro_rata
    del extensive, nom, pro_rata, extensives

class Surface (Spheroid, Round, SurfacePart):
    # Spheroid before SurfacePart, since we prefer its lazy volume ...
    __upinit = Round.__init__
    __spinit = SurfacePart.__init__
    __blinit = Spheroid.__init__
    def __init__(self, radius, gravity, spin, *parts, **what):
        try: nim = (1 - what['flattening']) * radius
        except KeyError: nim = None
        self.__blinit(radius, None, nim)
        self.__spinit(*parts)
        self.__upinit(radius, spin, **what)
        self.gravity = self.g = gravity

    def _lazy_get_area_(self, ignored, fp=4*pi): return fp *  self.radius ** 2
    def _lazy_get_potential_(self, ignored): return - self.radius * self.g
    def _lazy_get_escape_(self, ignored): return (-2 * self.potential)**.5
    def _lazy_get__GM_(self, ignored): return self.gravity * self.radius**2

    def _lazy_get__sync_(self, ignored, third=1./3):
        """Radius for synchronous orbit."""
        return (self.gravity * (self.radius / self.spin.omega) ** 2)**third

class Ocean (SurfacePart):
    __upvol = SurfacePart._lazy_get_volume_
    def _lazy_get_volume_(self, ig):
        # guesstimate approximately "conical":
        try: return self.depth * self.area / 3
        except AttributeError: return self.__upvol(ig)

class LandMass (SurfacePart): pass
class Continent (LandMass): pass
class Island (LandMass): pass # also used for groups of islands

del Quantity, arc, pi, Object, second
