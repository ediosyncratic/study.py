# -*- coding: iso-8859-1 -*-
"""The various types of heavenly body.

See study.LICENSE for copyright and license information.
"""

class Satellites:
    """Carrier-object for satellites of a heavenly body."""

    def __init__(self, callback, fill):
        """Construct a new satellite-array.

Arguments:

  callback -- function to call when adding a new entry; should clear any
              attributes (of the centre this object serves) that depend on what
              satellites it has.

  fill -- function to call once when it's a good moment to load all members; is
          actually called the first time any indexing is attempted.

In practice, the central body's lazy reset method is sensible for the former
(provided we configure it to preserve its lazy satellites attribute, of
course !), though ideally a more selective purge could be used.\n"""

        # Use Ordered (see study.snake.sequence) for __carry ?
        self.__cb, self.__carry = callback, []
        if fill is not None: self.__fill = fill

    def GMs(self):
        row = []
        for sat in self.__carry:
            if isinstance(sat, Body):
                try: GM = sat.orbit._GM
                except AttributeError: pass
                else: row.append(GM)

        return row

    def insert(self, ind, what):
        """Inserts what into self: ind is a hint at where to put it.

        Arguments:

          ind -- suggested position for new entry, or None.
          what -- new entry

        Actual position used will ensure that self's entries are ordered by increasing
        .orbit.radius; using an ind of None will skip trying to use it as a hint and get
        straight on with looking for the right place to put it."""

        # Look up much-used stuff once:
        row, rad = self.__carry, what.orbit.radius

        if not row or row[0].orbit.radius > rad:
            lo = hi = 0
        else:
            # Prepare for binary chop:
            hi, lo = len(row), 0
            # what belongs at an index i with lo < i <= hi

            if ind is not None:
                # Use ind as a hint:
                if hi > ind > 1 and row[ind-1].orbit.radius < rad: lo = ind - 1
                if hi > ind >= 0 and row[ind].orbit.radius > rad: hi = ind

        # Use binary chop between positions hi and lo in row:
        while lo + 1 < hi:
            mid = (lo + hi) / 2
            if row[mid].orbit.radius > rad: hi = mid
            else: lo = mid

        row.insert(hi, what)
        if isinstance(what, Body):
            self.__cb()

    def append(self, what):
        """Inserts what in self, taking hint that it belongs at end."""
        self.insert(len(self.__carry), what)

    # Dealing with things for which we should first call .__fill():
    def __all(self):
        try: fill = self.__fill
        except AttributeError: pass
        else:
            del self.__fill
            fill()

        self.__all = lambda c=self.__carry: c
        return self.__carry

    def __getitem__(self, ind):
        if isinstance(ind, slice): pass
        elif ind < 0 or ind >= len(self.__all()): raise IndexError, ind
        return self.__all()[ind]

    def __repr__(self): return `self.__all()`
    def __str__(self): return str(self.__all())
    def __len__(self): return len(self.__all())

from study.value import object
from study.value.units import Quantity

class Object (object.Object):
    """Generic object class for astronomical things.

    Each derived class should have its own instance dictionary, unless its
    instances should be carried by a base class.\n"""

    __space_upinit = object.Object.__init__
    def __init__(self, name, satelload=None, __doc__=None, **what):
        self.__space_upinit(**what)
        self.name = name
        self.__load_satellites = satelload
        try: what['orbit'].centre.satellites.insert(None, self)
        except (KeyError, AttributeError):
            pass # print 'Failed to insert', name, "in satellite list of its orbit's centre"

        self._name_as_(name)
        if __doc__ is None:
            __doc__ = "%s named %s" % (self.__class__.__name__, name)
        self.__doc__ = __doc__

    instance = {}
    _lazy_preserve_ = object.Object._lazy_preserve_ + ('satellites',)

    def _name_as_(self, nom, cls=None):
        if cls is None: cls = self.__class__

        try: was = cls.instance[nom]
        except KeyError: pass
        else:
            if was is not self:
                print "Name collision for", cls.__name__, "name:", nom

        cls.instance[nom] = self

    def augment(self, what, Q=Quantity):
        # TODO: richer handling of aliases
        try: your = what['aliases']
        except KeyError: pass
        else:
            try: mine = self.aliases
            except AttributeError: mine = ()
            mine = tuple(mine)
            for nom in your:
                if not nom in mine:
                    self._name_as_(nom)
                    mine = mine + (nom,)

            what['aliases'] = mine

        try: your, mine = what['type'], self.type
        except (KeyError, AttributeError): pass
        else:
            if mine[:len(your)] == your:
                del what['type']

        for k, your in what.items():
            try: mine = getattr(self, k)
            except AttributeError: pass
            else:
                if isinstance(mine, Q):
                    mine.observe(your)
                    del what[k]
                elif isinstance(your, Q):
                    your.observe(mine)
                elif k not in ('aliases', 'type') and mine != your:
                    print "Data loss: collision for", name, \
                          "attibute", k, "replacing", mine, "with", your

        # TODO: something more sophisticated, to cope if we already know a datum !
        self.__dict__.update(what)

    def __str__(self): return self.name
    __repr__ = __str__

    def _lazy_get_Bode_(self, ignored, cache=[]):
        try: gen = cache[0]
        except IndexError:
            from Bode import Bodalizer
            gen = Bodalizer
            cache.append(gen)

        if self.__load_satellites is not None:
            self.__load_satellites()

        return gen(self.satellites)

    def _lazy_get_satellites_(self, ig, S=Satellites):
        return S(self._lazy_reset_, self.__load_satellites)

    from study.chemy.physics import Cosmos
    def _lazy_get_GM_(self, ig, G=Cosmos.G):
        try: mass = self.mass
        except AttributeError: # e.g. thanks to Lazy's recursion detection ...
            mass = self.surface.volume * self.density
            # raising AttributeError if we don't even have those.
        return mass * G

    def _lazy_get_Schwarzschild_(self, ignored, S=Cosmos.Schwarzschild):
        return self.mass * S

    def _lazy_get_mass_(self, ignored, G=Cosmos.G):
        return self.GM / G

    del Cosmos

del object, Satellites

from study.value.units import second, metre, turn, pi, day, year
from common import Spin, Orbit

class Body (Object):
    instance = {}
    __upinit = Object.__init__
    def __init__(self, *args, **what):
        self.__upinit(*args, **what)

        try: surface = self.surface
        except AttributeError: pass
        else: # give surface.gravity an error bar (where feasible):
            try: r, m = surface.radius, self.GM
            except AttributeError: pass
            else:
                myg = m/r**2
                try: g = surface.gravity
                except AttributeError: surface.gravity = myg
                else: g.observe(myg)

    def _lazy_get_tidal_(self, ignored, zero = 0 / second**2, Q=Quantity):
        """Returns strength of tidal stresses near self.

        Made up of three terms:
          * that due to the body's own satellites (if any),
          * that due to whatever the body orbits (if any) combined with the
            contributions from other bodies orbiting the same centre, and
          * an ambient term, taken as .orbit.centre.tidal without its
            contributions due to .orbit.centre's satellites.

        The last two are provided (as .orbital and .ambient) as attributes of
        the composite; which also has, for each body that contributed, an
        attribute for that body's effect, with that body's name.  The
        difference between .orbital and the attribute named for the centre of
        orbit is the contribution of the other bodies orbiting the same
        centre.

        Each term is taken to imply a tidal compression with half the strength
        of its stretch in the two directions perpendicular to its stretching,
        so we have to consider the directions of terms.  The orbital term
        always stretches along the ray from self to its .orbit.centre; the
        ambient term and the terms due to satellites vary their direction
        uniformly relative to this ray, with no correlation between relative
        direction and strength.  Terms due to other bodies orbiting the same
        centre, however, vary strength and direction in ways notably
        correlated with their direction relative to the ray from self to its
        .orbit.centre; see theory below.  These terms are thus combined into
        .orbital and deemed part of it, effectively expanding its range of
        variation.

        The resulting orbital term, ambient term and terms due to satellites
        are presumed uncorrelated in direction; the largest of them is taken
        as basis for the returned value, with each of the others contributing
        an error bar ranging from -1/2 to +1 of times its full value.

        Theory
        ======

        The basic effect is easy; the gravitational field at self due to a
        body of mass M at distance R from self's centre pulls self towards the
        other body hard enough to produce acceleration G*M/R**2, but self's
        surface isn't all at distance R from the other body, so each point
        experiences (relative to self's freely-falling frame) forces due to
        the difference between this acceleration and the field, at the given
        point, due to the other body.  For positions along the line of
        centres, this is just their distance from self's centre times the
        derivative of the field strength, 2*G*M/R**3 away from self's centre;
        this is the primary tidal stress.  In directions at right angles to
        the line of centres, the direction to the other body varies, producing
        a discrepancy between the attraction to the other body (along the line
        towards it) and self's acceleration (along the line from self's centre
        towards the other body); this discrepancy's magnitude is G*M/R**2
        times the sin() of the angle, which is the distance from self's centre
        divided by R; and it effecitvely pulls towards self's centre.  We thus
        have tidal stretching by 2*G*M/R**3 times displacement from self's
        centre along the line of centres, with compression half as strong in
        each direction perpendicular to this.

        It thus remains to consider the variation in R between self and
        another body orbiting the same centre, as the angle t between their
        lines of centres varies.  Let S and B be the orbital radii of two
        bodies, presumed constant (since other variations shall dwarf theirs)
        with S > B; then R varies between S-B and S+B as R**2 = S**2 +B**2
        -2*S*B*cos(t). The directions of the line joining S to B, relative to
        their respective rays to their common centre of orbit, also vary with
        t.  For S, being further out, the angle s between B and centre is
        always less than a quarter turn, with sin(s) = B*sin(t)/R.  For B, the
        angle b between S and centre varies over the full cycle, with sin(b) =
        S*sin(t)/R.

        At right angles to the plane of their orbits (presumed coplanar), each
        body always experiences compression due to the other, varying in
        magnitude as R varies from S-B to S+B.  Aside from a G*M/R**3 factor,
        each body's radial stretching gets a contribution 2*abs(cos())
        -abs(sin()) of its relevant angle, while its tangential compression
        (due to orbit-centre) is offset by a matching 2*abs(sin()) -abs(cos())
        term.

        We know s is less than a quarter turn in magnitude, so presume that it
        is between zero and a quarter turn, so sin() and cos() are positive;
        and we're only interested in where the tangential stress is largest;
        for the inner body at radius B there shall be two values of b for
        which this arises, one between a quarter turn and a half turn, the
        other between half turn and three quarters; select the latter, where
        both sin() and cos() are negative.

        The tangential term for the inner body is then (2*R*sin(b)
        -R*cos(b))*G*M/R**4 with 0 < R*sin(b) = S*sin(t) and 0 < R*cos(b) = B
        -S*cos(t), so we have (2*S*sin(t) -B +S*cos(t))*G*M/(S**2
        -2*S*B*cos(t) +B**2)**2 with t between minus a quarter turn and
        zero.  Setting G*M aside, differentiating and multiplying by R**6, we
        get
          d((2*S*sin(t) -B +S*cos(t))/R**4)/dt*R**6
          = (2*S*cos(t) -S*sin(t))*R**2 -2*(2*S*sin(t) -B +S*cos(t))*2*S*B*sin(t)
          = 2*S*S*S*cos(t) -4*S*S*B*cos(t)*cos(t) +2*S*B*B*cos(t) -S*S*S*sin(t)
           +2*S*S*B*cos(t)*sin(t) -S*B*B*sin(t) -8*S*sin(t)*S*B*sin(t)
           +4*B*S*B*sin(t) -4*S*cos(t)*S*B*sin(t)
          = 2*S*(S*S +B*B)*cos(t) +S*(3*B*B -S*S)*sin(t) -8*S*S*B*sin(t)*sin(t)
           -2*S*S*B*sin(t)*cos(t) -4*S*S*B*cos(t)*cos(t)
          = S*( (S*S +B*B)*(2*cos(t) +sin(t)) -2*(S*S -B*B)*sin(t)
               -2*S*B*(4*sin(t)*sin(t) +sin(t)*cos(t) +2*cos(t)*cos(t)) )

        For the outer body, the analysis just swaps S and B while taking t to
        be between zero and a quarter turn.\n"""

        pole = zero # sum of all compression terms
        spin = [] # in which to collect up angularly uncorrelated terms
        radi = [] # in which to collect up maximal radial contributions
        tang = [] # in which to collect up maximal tangential contributions
        bok = {} # in which to name contributions per body

        # TODO: work out separate radial (+ve) and circumferential (-ve)
        # components.  Ambient and satellites take all directions relative to
        # radial, so are simply (-1, +2) times G.m/r^3; orbital is always
        # radial; but peers are where it gets interesting.  Max radial
        # component is easy, 2.G.m/abs(r-R)^3, but circumferential is
        # fiddlier.

        ambient = peer = zero
        try: mid = self.orbit.centre
        except AttributeError: pass
        else:
            try: ambient = mid.tidal
            except AttributeError: pass
            # but separate out the satellite contribution to this ...
            else: ambient = ambient.orbital + ambient.ambient
            row.append(ambient)

            try: r = self.orbit.radius
            except AttributeError: pass
            else:
                for p in mid.satellites:
                    if p is self: continue
                    try: gm, s = p.GM, p.orbit.radius
                    except AttributeError: pass
                    else:
                        best, hi, lo = max(s, r)**-3, abs(s - r)**-3, (s + r)**-3
                        q = bok[p.name] = 2 * gm * Q.flat(lo, hi, best)
                        peer += q

        try: bok[mid.name] = self.orbit.tidal
        except AttributeError: orbital = peer
        else: orbital = self.orbit.tidal + peer
        row.append(orbital)

        for moon in self.satellites:
            try: mine = bok[moon.name] = moon.orbit.tidal * moon.mass / self.mass
            except AttributeError: pass
            else: row.append(mine)

        # That's gathered together all contributions.
        if row:
            row.sort()
            row, big = row[:-1], row[-1]
        else: big = zero

        # TODO: this assumes tidal stretches along the line of centres
        # (correct) and compresses 1/2 as hard on each direction perpendicular
        # to that line (because I think the trace of a relevant tensor is
        # zero); what's the correct value ?

        tot = sum(row, zero)
        bok['squash'] = -(tot + big) / 2
        return Q(big + Q.flat(-tot/2, tot),
                 doc = self._lazy_get_tidal_.__doc__,
                 ambient = ambient, orbital = orbital, **bok)

    def _lazy_get_tide_(self, ignored):
        """Returns strength of tidal forces across self.

        This is just the tidal stress times the body-radius.
        """
        return self.tidal * self.surface.radius

    def _lazy_get_day_(self, ignored, unit=2*pi):
        """Returns the standard day-length of the body.

        This is the spin-period of the body adjusted for the amount of
        movement round its centre of orbit.  The computation here assumes that
        the surface spins in the same sense as the body follows its orbit.\n"""
        return unit / (self.surface.spin.omega - self.orbit.spin.omega)

    def _lazy_get_density(self, ignored):
        return self.mass / self.surface.volume

    def _lazy_get_binding_(self, ignored):
        """Gravitational binding energy

        To remove a shell of thickness dr from the surface of a body of radius r
        and density rho takes energy dE = 4*dr*pi*r*r*rho*G*M/r where M is
        (4*pi/3)*rho*r**3, so dE = (4*rho*pi)**2 * G/3 * r**4 * dr so shredding
        the body will take the integral of this from 0 to radius,
        i.e. (4*rho*pi)**2 * G/15 * r**5 plus any energy needed to break the
        mass up into the dust assumed by this.

        Substituting back to use M rather than rho we have 3*G*M*M/r/5\n"""

        return .6 * self.mass * self.GM / self.surface.radius

    def _lazy_get_centralpressure_(self, ig):
        """Pressure at centre of body.

        Supposing the body to have uniform density rho and spherical symmetry,
        the mass inside radius r is (4*pi/3)*rho*r**3 so that the weight per
        unit area (i.e. contribution to pressure) of a shell of thickness dr at
        radius r is rho*G*dr/r/r times this, or (4*pi*G/3)* rho**2 * r*dr which
        we can integrate up from pressure 0 at r=R to a central pressure, at
        r=0, of (4*pi*G/3)* rho**2 * R*R/2; substituting in for the body's mass,
        this is G*M*rho/R/2."""

        return .5 * self.GM * self.density / self.surface.radius

    __GM = Object._lazy_get_GM_
    def _lazy_get_GM_(self, ignored, Q=Quantity):
        row = self.satellites.GMs() # assorted estimates

        try: row.append(self.surface._GM)
        except AttributeError: pass

        try: best = self.__GM(ignored)
        except AttributeError:
            from study.value.sample import Weighted
            if row: best = Weighted(row).median()
            else: raise AttributeError(ignored, 'no data available from which to infer')
            row.remove(best)

        return Q(best, sample=row)

    def _lazy_get_Stefan_(self, ignored):
        """Data from which to compute self's temperature.

        When self's heat derives from some other body, S, let r*radian be the
        angle subtended, at self, by S's radius; then .Stefan is the twople of S
        and the square root of r/2.  The fraction of self's celestial sphere
        covered by S is just the square of r/2, so the number in the twople
        ratio is the fourth root of this.  (That square of r/2 is the solid
        angle of S's circle on the celestial sphere, pi.r.r, divided by the
        total solid angle of the celestial sphere, 4.pi.)

        If R is the distance from self to S, then r.R is the radius of S; if S
        has temperature T, it radiates away power at sigma * T**4 * 4*pi *
        (r*R)**2, where sigma is study.chemy.physics.Thermal.Stefan; at distance
        R, this is spread evenly over an area 4*pi * R**2, so we get sigma *
        T**4 * r**2 as the arriving power per unit area.  Let self's radius be
        h; it covers an area pi * h**2 of that shall at distance R, so collects
        sigma * T**4 * pi * (h*r)**2.  If self's temperature is t, then it
        radiates away energy at sigma * t**4 * 4*pi * h**2; unless self is
        generating energy from some other source, or saving it away in some kind
        of a reservoir, these two power rates must be equal.  Cancelling the
        common factor of sigma * pi * h**2, we're left with T**4 * r**2 = t**4 *
        4 so that T/t is the fourth root of r*r/4, so the square root of r/2.

        Ideally, we'd return S's temperature times the scale factor given here,
        but there is no assurance that S has a temperature attribute (and it
        might have it as .surface.temperature or simply as .temperature), so I
        leave this for the client to sort out.\n"""

        if isinstance(self, Star):
            raise AttributeError("Generates its own heat", self)

        try: S, R = self.centred(Star)
        except ValueError as what:
            raise AttributeError(*what.args)

        r = S.surface.radius / R / 2
        return S, r ** .5

    def centred(self, cls=None, zero=0 * metre, about=Quantity.encircle):
        """Finds what self orbits of a given kind, possibly via intermediaries.

        Optional argument, cls, defaults to None which serves as surrogate for
        Star (not defined when we declare this, but available for use by the
        time it's run).  Chases .orbit.centre until it finds an instance of the
        given type, raising ValueError if it never does.  (If self is of the
        given type, it's found immediately and we stop on it.)  Returns the
        given central body paired with self's distance from it, the last
        .orbit.radius plus an error bar made of all earlier .orbit.radius
        values.\n"""
        if cls is None: cls = Star

        S, R = self, zero
        try:
            while not isinstance(S, cls):
                O = S.orbit
                R = O.radius + about(R)
                S = O.centre
        except AttributeError:
            raise ValueError("Doesn't orbit [a satellite of ...] one of those",
                             self, S, cls)

        return S, R

    def orbital(self, radius, ecce=0, tp=2*pi, S=Spin):
        """The spin of the relevant orbit.

        Required first argument is the radius of the orbit; optional second
        argument is its eccentricity, defaulting to 0.  Result is a Spin object
        describing the angular velocity of the orbit.

        Ignoring eccentricity, we get: GM/R/R = w.w.R so w = sqrt(GM/R/R/R) and
        T = 2.pi/w.  This actually holds true, for non-zero eccentricity, if R
        is the semi-major axis of the orbit: but if R is the semi-latus rectum,
        you should specify a non-zero eccentricity and this will adjust it by
        the correct factor.\n"""

        if ecce: radius = radius / (1 - ecce**2)
        return S(tp * (radius**3 / self.GM)**.5)

    def __radius(self, given, time=second, length=metre, angle=turn, tp=2*pi):
        try: given + length # radius
        except TypeError: pass
        else: return given, None

        try: given + angle/time # frequency; convert to period
        except TypeError: pass
        else: given = angle / given

        try: given + time / angle
        except TypeError: pass
        else: given = given * angle

        try: given + time # period
        except TypeError: pass
        else: return (self.GM * (given / tp)**2)**(1./3), given
        # should take account of eccentricity !

        raise ValueError('how does that specify an orbit ?', given)

    def orbiter(self, given, O=Orbit, S=Spin, **what):
        """The orbit with the given parameter.

        Single positional parameter, given, may be an orbital radius, period
        (either as time to complete one orbit or as time divided by angle
        moved) or frequency (as angle traversed divided by time to traverse
        it).  Returns an Orbit object describing an orbit around self with the
        given parameter.  Keyword parameters, if supplied, are forwarded to
        the Orbit's constructor (except for the names 'given', 'S' and 'O',
        which should be avoided).\n"""
        radius, period = self.__radius(given)
        if period is None:
            return O(self, radius, None, **what) # accept Orbit's guestimate of spin

        return O(self, radius, S(period), **what)

    def _lazy_get_synchronous_(self, ignored, path=Orbit):
        """Synchronous orbit. """
        name = "Synchronous orbit about %s" % self.name
        return path(self, self.surface._sync, self.surface.spin, 0,
                    name = name, __doc__ = """%s

This is the orbit in which a satellite remains permanently above the same point
on the surface of %s as it rotates. """ % (name, self.name))

    def _lazy_get_Roche_(self, ignored, factor=2.46):
        """The Roche limit

        This is the minimum distance at which a fluid satellite, influenced by
        its own gravitation and that of the central mass, can be in equilibrium.
        For a satellite of zero tensile stress and the same mean density as its
        primary, in a circular orbit around its primary, this critical distance
        is 2.46 times the radius of the primary. [NASA]\n"""

        return self.surface.radius * factor

    class rowbok:
        def __init__(self): self.__bok = {}
        def __getitem__(self, key):
            assert key in "+-"
            try: return self.__bok[key]
            except KeyError: pass
            ans = self.__bok[key] = []
            return ans

    def _lazy_get_Lagrange_(self, ignored, klz = rowbok):
        """Inhabitants of Lagrangian / Trojan points.

        I'm presently using self.Lagrange as a dictionary recording the bodies
        in the Lagrangian / Trojan positions on self's orbit (see NASAtrojan(),
        below).  This may change ...

        Presently, the only keys of self.Lagrange will be '+' and '-' denoting
        the leading and following stable points, respectively.\n"""

        return klz()

    del rowbok

from common import Round, Spheroid
class Star (Body, Round):
    instance = {}

    def __sol(self):
        from home import Sun
        Star.__sol = lambda x: Sun
        return Sun

    def Solstation(self, type, dist, locn, mass, size, lum, spin=None,
                   Sp=Spin, Sf=Spheroid, ly=year.light, dy=day,
                   **what):
        """Initialize a Star based (mostly) on solstation.com's data."""

        Sol = self.__sol()
        if spin is None: skin = Sf(Sol.surface.radius * size)
        else: skin = Sf(Sol.surface.radius * size, spin=Sp(dy * spin))

        what.update(type=type, distance=dist * ly,
                    ICRS2000_0=locn, # 'ICRS 2000.0' data
                    mass=mass * Sol.mass,
                    surface=skin,
                    luminosity=lum * Sol.luminosity)

        self.augment(what)

    def NeighbourTable(self, away, (x, y, z), type=None, lum=None, mass=None, radius=None,
                       ly = year.light, Sf=Spheroid):
        """Data from a table of near neighbour stars.

        I'm not sure where I found this.  Typos suggest I transcribed the data
        by hand.  Its columns were: Star, R(Sol)/light-year, (X,Y,Z)/light-year,
        Type, Luminosity/Sun.lum, Mass/Sun.mass and Radius/Sun.R, and it noted
        that Types are: O,B,A,F,G,K,M; O0 brightest, M9 dimmest; but white
        dwarfs have type D*\n"""

        bok, Sol = {}, self.__sol()
        bok['position'] = (x * ly, y * ly, z * ly)
        bok['distance'] = away * ly
        if type is not None: bok['type'] = type
        if lum is not None: bok['luminosity'] = lum * Sol.luminosity
        if mass is not None: bok['mass'] = mass * Sol.mass

        if radius is not None:
            radius *= Sol.surface.radius
            try: skin = self.surface
            except AttributeError: self.surface = Sf(radius)
            else: skin.radius.observe(radius)

        self.augment(bok)

del Spin, year, second, metre, turn, pi

class Galaxy (Body, Round):
    instance = {}

    def AtlasOfUniverseLG(self, name, (l, b), distance, diameter, type, alias, year):
        pass # TODO !

# need a plural class to collect members together in a .parts thing.
class Group (Object): instance = {} # of galaxies
class Cluster (Object): instance = {} # lots of stars
class Constellation (Object): instance = {} # of (apparent) stars
class System (Object): instance = {} # solar system of possibly several stars

class Hoop (Object, Round):
    instance = {}
    # used for gaps and ring arcs, and as base for Ring.
    __upinit = Object.__init__
    def __init__(self, name, centre, radius, tilt=0, eccentricity=0, **what):
        # Assume rings move circularly, since Saturn's rings look like they do ...
        o = what['orbit'] = self.__orbit(centre, radius, eccentricity, tilt)
        # It's important that we pass orbit via what ...
        self.__upinit(name, **what)
        self.borrow(o) # for Round

    @staticmethod
    def __orbit(centre, radius, eccentricity, tilt, O=Orbit, Q=Quantity.flat):
        if tilt: tilt = Q(-tilt, tilt)
        return O(centre, radius, None, eccentricity, tilt)

class Shell (Object, Spheroid):
    __obinit, __spinit = Object.__init__, Spheroid.__init__
    def __init__(self, name, centre, *radii, **what):
        self.__spinit(*radii)
        what['centre'] = centre
        # what has to come this way so satelload reaches Object
        self.__obinit(name, **what)

class Ring (Hoop):
    # Share instance dictionary with other Hoop-based classes.
    __upinit = Hoop.__init__
    def __init__(self, name, centre, inner, outer, tilt=0, eccentricity=0, **what):
        self.__upinit(name, centre, self.__radii(inner, outer), tilt, eccentricity, **what)

    @staticmethod
    def __radii(inner, outer, Q=Quantity.flat):
        return Q(inner.low, outer.high, (inner.best + outer.best) * .5)

del Quantity, Spheroid, Orbit, Round

class Planetoid (Body):
    # Any vaguely spherical object that orbits a star.
    # Share instance dictionary with Body.

    def _lazy_get_iBode_(self, ignored):
        return self.orbit.iBode

class MinorPlanet (Planetoid): instance = {}
class DwarfPlanet (MinorPlanet): instance = {}

class Asteroid (MinorPlanet):
    instance = {}
    __upinit = MinorPlanet.__init__
    def __init__(self, name, orbit, mass, **what):
        what.update(orbit=orbit, mass=mass)
        self.__upinit(name, **what)

class DwarfAster (Asteroid, DwarfPlanet):
    __upname = Asteroid._name_as_
    assert __upname.im_func is DwarfPlanet._name_as_.im_func, "else need to call each"
    def _name_as_(self, nom):
        self.__upname(nom, Asteroid)
        self.__upname(nom, DwarfPlanet)

class Planet (Planetoid):
    """A major satellite of the sun.

    There is some debate about where we should draw the boundary between planets
    and other detritus in the solar system.  Sedna's discoverers advocate that,
    to qualify as a planet, a body should have to have more mass than all the
    other bodies in orbits of kindred radius.  This would knock Pluto off the
    list and leave us with only the eight planets out to Neptune.  They regard
    Pluto, like Quaoar, as a Kuiper belt object.\n"""

    instance = {}

    __upinit = Planetoid.__init__
    def __init__(self, name, surface, orbit, **what):
        """Initialises a Planet object during creation.

        Required arguments:
            name -- name of the planet
            surface -- a Surface object describing the planet's surface
            orbit -- an Orbit object describing its orbit

        Further data may be supplied in name=value form, to taste; these
        will be added to the Planet's namespace.  Likewise, name, surface
        and orbit will be stored as self.name, self.surface and self.orbit.

        Initialisation ensures that orbit.centre notices that the new Planet
        is one of its satellites. """

        what.update(orbit=orbit, surface=surface)
        self.__upinit(name, **what)
