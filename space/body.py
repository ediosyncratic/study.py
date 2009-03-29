# -*- coding: iso-8859-1 -*-
"""The various types of heavenly body.

$Id: body.py,v 1.28 2009-03-29 11:26:24 eddy Exp $
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

    def _name_as_(self, nom, klaz=None):
        if klaz is None: klaz = self.__class__

        try: was = klaz.instance[nom]
        except KeyError: pass
        else:
            if was is not self:
                print "Name collision for", klaz.__name__, "name:", nom

        klaz.instance[nom] = self

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
                    self._name_as(nom)
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

from study.value.units import second, metre, turn, pi, tophat, day, year
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
          * tidal stresses due to the body's own satellites (if any).
          * tidal stresses due to whatever the body orbits (if any),
          * ambient tidal stresses, taken as the orbit.centre.tidal

        The last two are provided (as .ambient and .orbital) as attributes of
        the composite, which is obtained by presuming that the various
        contributions are all uncorrelated in phase - i.e. sometimes they
        interfere with one another constructively, sometimes destructively - and
        computed as the largest term plus a `fuzzy' term resembling `+/- each of
        the remaining terms'. """

        row = [] # in which to collect up the contributions
        bok = {} # in which to name contributions per body

        ambient = zero
        try: mid = self.orbit.centre
        except AttributeError: pass
        else:
            try: ambient = mid.tidal
            except AttributeError: pass
            # but separate out the satellite contribution to this ...
            else: ambient = ambient.orbital + ambient.ambient
            try: r = self.orbit.radius
            except AttributeError: pass
            else:
                peer = zero
                for p in mid.satellites:
                    if p is self: continue
                    try: gm, s = p.GM, p.orbit.radius
                    except AttributeError: pass
                    else:
                        best, hi, lo = (s ** 2 + r **2)**-1.5, abs(s - r)**-3, (s + r)**-3
                        q = bok[p.name] = 2 * gm * Q.flat(lo, hi, best)
                        peer += q

                if peer is not zero: ambient += peer
                ambient.peer = peer
            row.append(ambient)

        try: orbital = bok[mid.name] = self.orbit.tidal
        except AttributeError: orbital = zero
        else: row.append(orbital)

        for moon in self.satellites:
            try: mine = bok[moon.name] = moon.orbit.tidal * moon.mass / self.mass
            except AttributeError: pass
            else: row.append(mine)

        # That's gathered together all contributions.
        if row:
            row.sort()
            row, big = row[:-1], row[-1]
        else: big = zero

        tot = sum(row, zero)
        return Q(big + Q.flat(-tot, tot),
                 ambient = ambient, orbital = orbital, **bok)

    def _lazy_get_tide_(self, ignored):
        """Returns strength of tidal forces across self.

        This is just the tidal stress times the body-radius.
        """
        return self.tidal * self.surface.radius

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

        try: given + time/angle # frequency; convert to period
        except TypeError: pass
        else: given = angle / given

        try: given + time # period
        except TypeError: pass
        else: return (self.GM * (given / tp)**2)**(1./3), given
        # should take account of eccentricity !

        raise ValueError('how does that specify an orbit ?', given)

    def orbiter(self, given, O=Orbit, S=Spin, **what):
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

del Spin, year, Quantity, second, metre, turn, pi

class Galaxy (Body, Round):
    instance = {}

    def AtlasOfUniverseLG(self, name, (l, b), distance, diameter, type, alias, year):
        pass

# need a plural class to collect members together in a .parts thing.
class Group (Object): instance = {} # of galaxies
class Cluster (Object): instance = {} # lots of stars
class Constellation (Object): instance = {} # of (apparent) stars
class System (Object): instance = {} # solar system of possibly several stars

class Hoop (Object, Round):
    instance = {}
    # used for gaps and ring arcs, and as base for Ring.
    __upinit = Object.__init__
    def __init__(self, name, centre, radius, tilt=0, eccentricity=0, O=Orbit, **what):
        # Assume rings move circularly, since Saturn's rings look like they do ...
        if tilt: tilt = tilt * tophat
        o = what['orbit'] = O(centre, radius, None, eccentricity, tilt)
        # It's important that we pass orbit via what ...
        self.__upinit(name, **what)
        self.borrow(o) # for Round

del Orbit, Round

class Shell (Object, Spheroid):
    __obinit, __spinit = Object.__init__, Spheroid.__init__
    def __init__(self, name, centre, *radii, **what):
        self.__spinit(*radii)
        what['centre'] = centre
        # what has to come this way so satelload reaches Object
        self.__obinit(name, **what)

del Spheroid

class Ring (Hoop):
    # Share instance dictionary with other Hoop-based classes.
    __upinit = Hoop.__init__
    def __init__(self, name, centre, inner, outer, tilt=0, eccentricity=0, **what):
        self.__upinit(name, centre,
                      (inner.best + outer.best) * .5 + (outer.high - inner.low) * tophat,
                      tilt, eccentricity,
                      **what)


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
