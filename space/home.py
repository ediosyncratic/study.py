# -*- coding: iso-8859-1 -*-
"""Where I come from.

$Id: home.py,v 1.29 2008-04-20 14:16:26 eddy Exp $
"""

from study.value.units import Sample, qSample, Quantity, Object, tophat, \
     micro, milli, kilo, mega, giga, tera, peta, arc, radian, pi, \
     year, day, hour, minute, second, kg, metre, km, \
     litre, bar, Watt, Tesla, Ampere, Gauss, Kelvin
from study.value.archaea import mile
import body
from common import Orbit, Spin, Discovery, Surface, \
     SurfacePart, Ocean, Island, Continent, LandMass

from study.chemy.physics import Cosmos
# some rough data from my Nuffield data book:
Universe = body.Object('Observable Universe', mass=1e52 * kg, radius=3e26 * metre,
                       # some slightly more definite data:
                       age = Quantity(13.7 * (1 + .02 * tophat), giga * year,
                                      """The age of our universe.

This is only about 60 mega years short of 2**201 Planck times.  Compare with
Hubble's constant (q.v.) and note that multiplying the two together gives an
answer just a little bit less than 1.\n"""),
                       temperature = Cosmos.temperature,
                       composition = { 'dark energy': .73,
                                       'cold dark matter': .23,
                                       'atoms': .04 },
                       # microwave background: mass density and number density:
                       photon = Object(mass = 4.68e-31 * kg / metre**3,
                                       number = 413e6 / metre**3))
del Cosmos

# See also: http://www.daviddarling.info/encyclopedia/L/LocalGroup.html
# Data from apod:
LocalGroup = body.Group("The Local Group",
                        # Error bar is a wild guess, based on MilkyWay.orbit.speed
                        velocity = Quantity(627 + 44 * tophat, km / second,
                                            """Speed of Local Group

Our local group of galaxies is moving at somewhere between 600 and 650 km/s
relative to the cosmic microwave background, in a direction described by
astronomers as toward a position (somewhere in the Virgo Cluster) given in terms
of co-ordinates (l,b) whose values are given as attributes here.

Most of our uncertainty about this arises from our ignorance of our motion
relative to the Local Group: we know our own velocity relative to the background
radiation far more accurately.

See also: http://antwrp.gsfc.nasa.gov/apod/ap030209.html
""",
                                  l = (273 + 6 * tophat) * arc.degree,
                                  b = (30 + 6 * tophat) * arc.degree),
                        # Guess based on: Andromeda being a bit bigger than the
                        # Milky Way; and the rest of the group not adding up to
                        # much by comparison:
                        mass = (4+tophat) * 1e41 * kg)

MilkyWay = body.Galaxy('Milky Way', mass=1.79e41 * kg,
                       orbit = Orbit(LocalGroup,
                                     # guess radius: over half way to Andromeda
                                     Quantity(1.5 + tophat, mega * year.light),
                                     None))
# Just the part inwards from the Sun (about 90 giga Sun), given here so that
# adding the Sun as a satellite of it goes smoothly.  See below for a .also()
# with more details.

# <bootstrap> some of Sun's data are given in units of Earth's ... but Earth's
# orbit can't be specified until Sun has been created.
def load_rubble(): # lazy satellite loader for Sun
    import inner, outer, Kuiper, asteroid

Sun = body.Star(
    'Sun', load_rubble, type='G2 V',
    orbit = Orbit(MilkyWay,
		  Quantity(27 + 4 * tophat, kilo * year.light),
                  # I've seen figures of 28 k ly, 8 k parsec; I used to quote 3e4 ly
		  Spin(Quantity(230 + 20 * tophat, mega * year)),
                  # I've seen period figures from 220 to 240 My.
                  None, # I don't know eccentricity, let Orbit guess for us ...
                  # I've seen speed quoted as 215 and 220 km/s
		  speed=Quantity(218 + 4 * tophat, km / second)),
    # for mass and surface, see below: Sun.also(...)
    __doc__ = """The Sun: the star at the heart of the solar system.""",

    velocity = Quantity(371 + tophat, kilo * metre / second,
                        """Speed of Solar System.

The solar system's motion relative to Cosmic Microwave Background shows up as a
dipole anisotropy, which we can measure fairly accurately.  Its direction is
given both as (l,b) co-ordinates and as (alpha,delta) which appear to be
synonyms for (RA,Dec), the first of which is measured in hours and minutes.  All
four data are here given, as attributes l, b, alpha and delta.
""",
                        l = (264.31 + tophat * .34) * arc.degree,
                        b = (48.05 + tophat * .1) * arc.degree,
                        alpha = 11.2 + .02 * tophat, # unit is hours ...
                        delta = (tophat * .16 - 7.22) * arc.degree),

    bright = Quantity(1.4 + Sample.tophat * .06,
                      kilo * Watt / metre / metre,
                      """How brightly shines the sun ?

Maximum solar total radiant power outside atmosphere at a distance of 1
Astronomical Unit from the Sun.  Natural variability is roughly 30 Watt /
metre**2, or about 2%.  This radiant power corresponds to an astronomical
apparent magnitude of -26.73; it should be possible to work out the zero-point
of the astronomical magnitude scale from this ...

The Earth's cross-section, the area relevant to this power supply, is a quarter
of its surface area; multiplying that by the solar constant, Earth is receiving
energy at 179 peta-Watts (albeit reflecting 39% of this, about 70 peta-Watts);
dividing by the square of the speed of light, that's the energy-equivalent of 2
kilogrammes per second.  Naively interpreting the energy-generation process as
consisting of the direct conversion of Hydrogen(1) to Iron(56), with a mass
defecit of half a gram per mole of Iron produced, this implies the production of
4000 moles of Iron (from 224000 of Hydrogen) per second: thus Earth is the
beneficiary of about a quarter tonne per second of the Sun's fuel being used up.

The total surface area of the sphere of radius one AU is 28e22 square metres
(22e8 times Earth's cross-section, above), implying that the Sun's total power
output is 390 yotta-Watts, the mass-equivalent of 4.4 million tonnes per second.
This implies, assuming total conversion to Iron again, that the Sun consumes
49e10 kg/s of its fuel - in the space of five millennia, this adds up to a
little over the Moon's mass - and that, in the four giga-years it's been shining
thus far, the Sun has consumed about 3 percent of its fuel, reducing its mass by
about one part in three thousand.  The amount of iron produced (in this, I
stress again, naive model) is nearly twelve thousand times the mass of the
Earth; or over 25 times the mass of all the planets put together.

The gravitational potential energy stored in a uniform spherical body (due to
gravitation attraction among its parts) is 3/5 of the obvious -G.M.M/R, with R
its surface radius and M its mass; so that of the Sun is of order 2e41 Joules,
equal to about 18 million years' worth of solar constant power output.  I
compute 2.53 zetta tonnes mass-equivalent for this naive model, equivalent to
about one thousandth of the Sun's total mass, about three times the reduction in
mass computed above from fusion.  In the course of pulling the Sun together, we
should take account of the work done increasing its temperature and compressing
it; but my crude sums indicate these are much smaller (of order exa tonnes).
""",
                      fullname = "Solar Constant"),

    discovery=Discovery("the earliest life-forms", -3e9), # etym ?

    density = 1.409 * kg / litre,
    age = Quantity(4.6e9, year, # Peter Francis, age of solar system [missing error bar]
                   lifespan = 1e18 * second, # Nuffield, order of magnitude
                   remain = (5 + tophat) * giga * year), # plus c. 2e9 years as a white dwarf
    magnitude = qSample({}, low=4.79, high=4.83), # K&L, Moore
    aliases = ('Sol',))

del load_rubble

ua = AU = AstronomicalUnit = Quantity(
    93, mega * mile,
    nom = 'ua', # unite astronomique ?
    fullname = 'Astronomical Unit',
    doc = """The typical distance between Earth and Sun.

This is the standard unit of length used, by astronomers, for measuring other
distances within the solar system.  Determining the ratio between Earth's
orbital radius and those of other planets is relatively easily performed to
fairly high accuracy; before the advent of radar, however, determining
astronomical distances in terrestrial units was rather hard and significantly
less precise.  Thus using Earth's `unmeasured' orbital radius as unit of length
was practical and sensible.

To understand the imprecision of the AU in terms of miles or metres, consider
the problem of measuring it.  To do this, you need two observatories a known
distance apart to compare the position of some other planet as seen at some
exact moment.  This is essentially the same mechanism surveyors use to measure
the heights of mountains, only on a grander scale.  For the distance to the
moon, this can be done with relatively good precision; however, this isn't much
help in determining the AU, at least for an observer sat in the middle of the
moon's orbit.  (For contrast, relating the orbital radii of satellites of other
planets to the AU is feasible, if perhaps fiddly.)

Using a base-line of order a thousand miles long, two observatories' observed
directions to an object of order a light year away should differ by a few arc
seconds, so the measurement is feasible *in principle*, albeit to only as good
an accuracy as one can measure angles of order a few arc seconds (not brilliant,
though photography improved it a fair bit in the nineteenth century).  However,
Earth travels in its orbit at about 30 km/second (67 thousand miles/hour) and
its spin contributes about another half km/second (one thousand miles/hour);
thus any imprecision in the simultaneity of the two measurements will introduce
errors dwarfing the answer.  While the moons of Jupiter provided a clock the
astronomers could share, the precision with which they could read it ensured
only very rough estimates.  In the mid-18th century, Venus passed across the
face of the Sun a couple of times, enabling astronomers to (with huge care and
much co-operation around the world) perform parallax measurements with decisive
precision of timing, thereby obtaining a *much* more accurate estimate of the
Astronomical Unit than had previously been available.

The modern definition of the astronomical unit is given by
http://physics.nist.gov/cuu/Units/outside.html
as:

  The astronomical unit is a unit of length. Its value is such that, when used
  to describe the motion of bodies in the solar system, the heliocentric
  gravitation constant is (0.017 202 098 95)**2 ua**3 * day**-2.  The value must
  be obtained by experiment, and is therefore not known exactly.

i.e. it's definition specifies the square root of the numeric value of Sun.GM
when expressed using the day as unit of time and the ua as unit of length.

See also: parsec.\n""")

parsec = Quantity(radian / arc.second, AU,
                  doc="""The parsec - an astronomical unit of length.

The parsec is defined to be the distance at which one Astronomical Unit (q.v.)
subtends an angle of one second of arc (of which 3600 make one degree); this
makes it about a fifth of a million AU, which is about 3.26 light years.

This choice of unit arises naturally from determining the distance from Earth to
a star by measuring parallax - the direction to the star, as the Earth goes
around its orbit, will vary by the angle subtended by two AU (the diameter of
Earth's orbit) at the distance between Earth and the star; and actual stars vary
in direction by up to of order one arc second.  Dividing two arc seconds by a
star's observed angle of variation yields the number of parsecs to the star; the
nearest stars are modest multiples of a parsec away.

See the doc string of AU for further details.
""")

parsec.observe(30.857 * peta * metre) # Kaye & Laby
# some other source alleged 3.26 * year.light, which is about the same.

Sun.luminosity = Sun.bright * 4 * pi * AU**2

# surface part data taken from Asimov:
_square_kilo_mile = (kilo * mile)**2
def IAcontinent(name, area=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    what['name'] = name
    return apply(Continent, parts, what)

def IAisland(name, area=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    what['name'] = name
    return apply(Island, parts, what)

def IAocean(name, area=None, depth=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    if depth is not None: what['depth'] = depth * mile
    what['name'] = name
    return apply(Ocean, parts, what)

# My home planet:
Earth = body.Planet(
    'Earth',
    Surface(Quantity(qSample(6367650 + 21476 * Sample.tophat,
                             best=6371020, low=6352400, high=6384100),
                     metre, """Radius of the Earth's surface.

This is how far mean sea level is from the Earth's centre; which varies
significantly between the poles and equator, thanks to centrifugal force.  I've
included, as .low and .high, the extremes of the radius at the Earth's solid
surface, 4.5 km below sea level in the Arctic and 5.89 km above sea level at the
top of Mount Kilimanjaro, in Africa.  See also: altitude.\n"""),
            # radius should also be nauticalMile * 60 * 180 / math.pi

            kg.force / kg, # by definition, from special property of masses

            Spin(Quantity(day * (1 - day / year.sidereal),
                          doc="""Rotational period of Earth wrt the fixed stars""",
                          sample = (24 * hour - 4 * minute + 4 * second,),
                          fullname="Sidereal Day"),
                 # Tilt varies between 22.1 and 24.5 degrees in 41 k yr cycle,
                 # due to reach a minimum in c. 1000 AD; (eh ? ten thousand ?)
                 23.44, # currently decreasing
                 # its direction varies on a 26 k yr cycle (precession).
                 axis = 'Polaris'),
            # need to also describe precession; see doc of year.sidereal

            # Ocean
            SurfacePart(IAocean('Pacific', 68, 2.6),
                        IAocean('Atlantic', 41.5, 2.1),
                        IAocean('Indian', 30, 2.4),
                        name = 'Ocean',
                        fresh = Object(ice = 23.67e18 * kg,
                                       water = .5e18 * kg,
                                       steam = 14.17e15 * kg)),

            # Land
            SurfacePart(IAcontinent('Africa', 11.5),
                        LandMass(IAcontinent('Asia', 16.5),
                                 IAcontinent('Europe', 3.8),
                                 name = 'Eurasia'),
                        LandMass(IAcontinent('South America', 7.035),
                                 IAcontinent('North America', 9.385),
                                 name = 'America'),
                        IAcontinent('Antarctica', 5.1),
                                   IAcontinent('Australia', 2.971021),
                                   IAcontinent('Greenland', .84),
                                   IAisland('islands', 2.5,
                                            IAisland('New Guinea', .312329),
                                            IAisland('Borneo', .290285),
                                            IAisland('Sumatra', .163145),
                                            # The above are mostly in Indonesia ...
                                            IAisland('Madagascar', .230035), # aka Malagasy
                                            IAisland('Baffin', .2016),
                                            IAisland('Japan', .142726,
                                                     IAisland('Honshu', .091278),
                                                     IAisland('Kyushu', .014791)),
                                            IAisland('Java', .048504),
                                            IAisland('Britain', .09422,
                                                     IAisland('Great Britain', .088133)),
                                            IAisland('Ireland', .027135),
                                            IAisland('Philipines', .115707),
                                            IAisland('Cuba', .044218),
                                            IAisland('Ceylon', .025332),
                                            IAisland('New Zealand', .103736),
                                            IAisland('Iceland', .039768),
                                            IAisland('Formosa', .013855)),
                                   name = 'Land',
                                   height = Quantity(qSample({}, mean = 840,
                                                             low = 0, high = 8840),
                                                     metre)),

            # misc other data:
            rainfall = .125e18 * kg / year,
            flattening = 1 / 298.25,
            albedo = .39, # so 61% of incident radiation is absorbed
            # but total solar power available at surface is 1/8 of that in space:
            # http://www.physorg.com/news117649731.html
            # This may be about which frequencies solar panels can use.
            nature = { 'Land': .292, 'Ocean': .708 },
            material = "basalt, granite, water",
            magnetism = Object(
    sampled = 'London, 1960',
    horizontal = 1.87e-5 * Tesla,
    vertical = 4.36e-5 * Tesla),

            altitude = Quantity(qSample({-4000: 3, 2000: 1},
                                        low = -10915, high = 8882),
                                metre, """\
Variation in Earth's surface altitude, relative to sea level.

I know the extremes, but have only a vague knowledge of the distribution in
between.  Note that the 19.8 km range is comparable with the variation in
Earth's radius due to the equatorial bulge, 21.4 km.\n""")),

    Orbit(Sun,
          Quantity(AU,
                   sample = [ 8.3 * minute.light,
                              #' Somewhere, I've also seen .155 * tera * metre
                              .149600 * tera * metre ]), # K&L, NASA and some other source ...
          # Perihelion (currently) happens at the beginning of January;
          # varies with Milankovitch cycle, on periods O(10 k yr).
          Spin(year.sidereal, 0,
               __doc__ = """The angular velocity of the Earth's orbit.

I've used the inclination of this as my presumed origin for directions of
orbital tilts and the Earth's axial spin; I should probably define orbital tilts
relative to the invariable plane, perpendicular to the total angular momentum of
the solar system, so roughly Jupiter's orbital plane.  Spin of each body could
be relative to its orbit's tilt, but perhaps the invariable plane would be
better even for these.  Earth crosses the invariable plane on January 9th and
July 9th.

Earth's orbital inclination varies on a 70 k yr cycle; but a 100 k yr cycle
relative to the invariable plane (?).\n""",
               plane='Ecliptic'),
          Quantity(16.7, milli,
                   """Eccentricity of Earth's orbit.

This varies between .005 and .058, with a mean of .028; about .012 of this
variation happens on a period of 413 k yr, but there are other terms varying on
other time-scales.  Perihelion happens on about January 3rd, apehelion about
July 4th.\n"""),
          __doc__ = """The Earth's orbit about the Sun.

The plane of this orbit is known as the ecliptic: all other orbits' inclinations
are given relative to this.  The mean radius of this orbit is known as the
astronomical unit; see ua.__doc__ for details.  The axis of the orbit's ellipse
precesses at a rate of about 0.3 degrees per century, due to tidal effects of
other planets (notably Jupiter)."""),

    discovery=Discovery("the earliest life-forms", -3e9,
                        etymology="""Earth

From ancient Indo-European 'er', whence sprang lots of words for soil, land,
ground ... earth.
"""),

    magnetic = Object(dipole = Quantity(8.1e22, Ampere * metre**2,
                                        doc="""Earth's magnetic dipole.

The magnetic field of the Earth is dominated by its dipole term.  This yields a
magnetic field strength of order one Gauss (1e-4 Tesla) at the Earth's surface.
For contrast, magnetars have magnetic fields of order peta-Gauss - see:
http://antwrp.gsfc.nasa.gov/apod/ap010901.html

The strength of Earth's magnetic field appears to be decreasing at about 5% per
century at present, though its average over the last 2500 years is more like
1.6% per century: http://www.spectrum.ieee.org/nov06/4708
""")),
    mass = 5.976e24 * kg,
    density = 5.518 * kg / litre,
    age = 1e17 * second, # Nuffield, approx
    Atmosphere = Object(mass = 5.27e18 * kg, pressure = bar,
                        composition = { "N2": .78, "O2": .21 }),

    Core = Object(name = "Earth's core",
                  surface = Object(radius = 3.488e6 * metre,
                                   flattening = 1/390.),
                  # but http://www.physorg.com/news65368833.html
                  # says the Core-Mantle boundary is at 1800 miles, 2.9e6 m;
                  # also indicates temperature of 2000K and pressure of 170 G Pa
                  mass = 1.88e24 * metre,
                  density = 10.72 * kg / litre),
    aliases = ('Terra', 'Gaia'))

Earth.surface.radius.observe(6.37814 * mega * metre) # NASA
Earth.mass.observe(5.9742e24 * kg)
Earth.orbit.radius.observe(Quantity(qSample({},
                                            low = 147.1e9, # perihelion
                                            high = 152.1e9, # aphelion
                                            best = 1000001017.8 * 149.597871), # mean
                                    metre))

del IAcontinent, IAisland, IAocean

def KLsurface(radius, gravity, spin, S=Surface, G=Earth.surface, **what):
    """As Surface, but with radius and gravity scaled to Earth = 1."""
    return apply(S, (radius * G.radius, gravity * G.gravity, spin), what)

# Sol: reprise
Sun.also(
    mass = 332946 * Earth.mass,
    surface = KLsurface(109.12,
                        27.96,
                        Spin(25 * day + 9 * hour, 7.2),
                        flattening = 0.0,
                        radiation = 63.3 * mega * Watt / metre**2,
                        temperature = 5800 * Kelvin))
Sun.surface.radius.observe(6.96e8 * metre)
Sun.mass.observe(1.9891e30 * kg) # over 700 times the sum of all the planets' masses
# </bootstrap>

# Milky way: reprise
MilkyWay.also(
    __doc__="""Our home Galaxy.

Visible in the sky as The Milky Way, our home Galaxy has a long central bar,
which points roughly at us, of length 27 k ly.  Its disk spans about 100 k ly
and is of order 20 k ly thick on average.  Orbital speeds of stars drop off on a
Keplerian curve out to around 10 k ly but then recover and remain roughly
constant, generally between about 150 and 200 km/s, out to 300 k ly (in so far
as there are any stars that far out to measure).  This implies far more mass at
large radii than would be suggested by the visible stars and clouds.
""",
    # Span given as 1e21 metre by Nuffield, 100,000 light years by
    # /apod/ap030103.html and assorted other sources; but it's blurry.
    radius=(51.5 + 5 * tophat) * kilo * year.light,
    # same apod gave "200 billion"; David Darling (see URL below) gives 200 to
    # 400 billion, not counting brown dwarves.
    starcount=(3 + 2*tophat) * 100 * giga,
    # and from an article in el Reg:
    age=(13.6 + 0.8 * tophat) * giga * year,
    # http://www.daviddarling.info/encyclopedia/G/Galaxy.html offers:
    density=Quantity(0.1, Sun.mass / parsec**3,
                     """Mean density of our Galaxy.

About 5 to 10 % of the Galaxy's mass is in gas and dust; its total mass is
between .2 and .4 tera Sun.mass (the figure given above is its mass closer in
than the Sun, so that adding the Sun as a satellite goes smoothly).
"""),
    mass=Quantity(.3 + .2 * tophat, tera * Sun.mass),
    thick=Quantity(2.45 + .3 * tophat, kilo * year.light,
                   "Thickness of the Galactic disk"),
    bulge=Quantity(16, kilo * year.light,
                   "Thickness of our Galaxy's central bulge"),
    luminosity=Quantity(1 + tophat, 10e36 * Watt),
    magnetic=Quantity(4 + 2 * tophat, micro * Gauss,
                      """Background magnetic field strength of our Galaxy.

The central bulge has much stronger magnetic fields.
"""))

def KLplanet(name, surface, orbit, mass, density, P=body.Planet, d=kg/litre, **what):
    """As Planet, but with mass scaled by that of the earth and density in g/cc"""
    what.update({'mass': Earth.mass * mass,
                 'density': density * d,
                 'surface': surface,
                 'orbit': orbit})
    return apply(P, (name,), what)

# Kaye & Laby also give, for orbits, synodic periods and longitudes of node and
# perihelion (closest approach).  Several of the following also get .observe
# applied to some of their data, giving the /usr/share/misc/units value.

Moon = KLplanet('Moon',
                KLsurface(.272, .17, Spin(29.4 * day, 5.2), material="silicates"),
                Orbit(Earth,
                      Quantity(.238855, mega * mile,
                               doc = """Distance from Moon to Earth""",
                               sample = (385 * mega * metre,
                                         .239 * mega * mile),
                               derivative = +.038 * metre / year),
                      Spin(27 * day + 7.719864 * hour, 1.5),
                      .055),
                .0123, 3.34,      # mass, density
                body.Planetoid, # over-ride Planet !
                discovery = Discovery("early life", -1e9,
                                      etymology = "English: Moon <- Germanic: Mond/Mand"),
                aliases = ('Luna', 'Selene'))
# Moon.orbit.radius' derivative and sample are taken from
# http://news.bbc.co.uk/hi/english/sci/tech/newsid_399000/399468.stm
Moon.mass.observe(7.3483e22 * kg)
Moon.mass.observe(7.349e22 * kg) # NASA
Moon.surface.radius.observe(1.738e6 * metre)
Moon.surface.radius.observe(1.7374e6 * metre) # NASA
Moon.surface.gravity.observe(1.62 * metre / second**2)
Moon.orbit.radius.observe(3.844e8 * metre) # agrees with NASA
Moon.orbit.spin.period.observe(27.32 * day) # NASA

Month = 1/(1/Moon.orbit.spin.period - 1/Earth.orbit.spin.period)
Moon.surface.spin.period.observe(Month) # tidally locked

del body, Orbit, Spin, Discovery, Surface, SurfacePart, Ocean, Island, Continent, LandMass, \
    Sample, qSample, Quantity, Object, tophat, micro, milli, kilo, mega, giga, tera, peta, \
    kg, metre, mile, arc, radian, pi, Kelvin, year, day, hour, minute, second, \
    litre, bar, Watt, Tesla, Ampere, Gauss
