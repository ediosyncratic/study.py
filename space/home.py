# -*- coding: iso-8859-1 -*-
"""Where I come from.

$Id: home.py,v 1.1 2005-03-12 15:05:44 eddy Exp $
"""

from basEddy.units import *
from space.body import Body, discreteBody, Planet

# some rough data from my Nuffield data book:
MilkyWay = Body('Milky Way', mass=1e41 * kg,
                        # given as 1e21 metre by Nuffield,
                        # 100,000 light years by /apod/ap030103.html
                        radius=(.97 + .06 * tophat) * zetta * metre,
                        starcount=200 * giga, # same apod, assuming billion is giga
                        # and from an article in el Reg:
                        age=(13.6 + 0.8 * tophat) * giga * year)
# which is part of a local group of galaxies, which moves at about 600
# kilometers per second relative to the primordial (cosmic background)
# radiation: see http://antwrp.gsfc.nasa.gov/apod/ap030209.html
Universe = Body('Observable Universe', mass=1e52 * kg, radius=3e26 * metre,
                        temperature = 4 * Kelvin)

# <bootstrap> some of Sun's data are given in units of Earth's ... but Earth's orbit
# can't be specified until Sun has been created.
Sun = discreteBody(
    'Sun',
    orbit = Orbit(MilkyWay,
		  3e4 * year.light,
		  Spin(225e6 * year), # period agrees with that from GM/r**3
                  None, # I don't know eccentricity, let Orbit guess for us ...
                  # Random factor of 10 to remove discrepancy between period and (2*pi*radius)/speed
                  # but - for all I know - it may be period or radius that needs corrected, not speed.
                  # After all, the reason folk believe in Dark Matter is that the radius and period *don't* match ...
		  speed=2.15 * mega * m/s / 10),
    # for mass and surface, see below: Sun.also(...)
    __doc__ = """The Sun: the star at the heart of the solar system.""",

    bright = Quantity(1.4 + Sample.tophat * .06,
                      kilo * Watt / metre / metre,
                      """How brightly shines the sun ?

Maximum solar total radiant power outside atmosphere at a distance of 1
Astronomical Unit from the Sun.  Natural variability is roughly 30 Watt /
metre**2, or about 2%.

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

    density = 1.409 * kilogramme / litre,
    age = Quantity(4.6e9, year, # Peter Francis, age of solar system [missing error bar]
                   lifespan = 1e18 * second, # Nuffield, order of magnitude
                   remain = (5 + tophat) * giga * year), # plus c. 2e9 years as a white dwarf
    type = 'G2 V',
    magnitude = qSample({}, low=4.79, high=4.83), # K&L, Moore
    aliases = ('Sol',))

del Body, discreteBody
SolarConstant = Sun.bright

AU = AstronomicalUnit = Quantity(
    93, mega * mile,
    nom = 'AU',
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

See also: parsec.\n""")

parsec = Quantity(radian / arc.second, AU,
                  doc="""The parsec - an astronomical unit of length.

The parsec is defined to be the distance at which one Astronomical Unit (q.v.)
subtends an angle of one second of arc (of which 3600 make one degree); this
makes it about a fifth of a million AU, which is about 3.26 light years.  This
choice of unit arises naturally from determining the distance from Earth to a
star by measuring parallax - the direction to the star, as the Earth goes around
its orbit, will vary by the angle subtended by two AU (the diameter of Earth's
orbit) at the distance between Earth and the star; and actual stars vary in
direction by up to of order one arc second.  Dividing two arc seconds by a
star's observed angle of variation yields the number of parsecs to the star; the
nearest stars are modest multiples of a parsec away.

See the doc string of AU for further details. """)

parsec.observe(30.857 * peta * metre) # Kaye & Laby
# some other source alleged 3.26 * year.light, which is about the same.

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
Earth = Planet('Earth',
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

                       Spin(Quantity(day * (1 - day / year),
                                     doc="""Rotational period of Earth wrt the fixed stars""",
                                     sample = (23 * hour + 56 * minute + 4 * second,),
                                     fullname="Sidereal Day"),
                            23.4, axis = 'Polaris'),

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
                     Quantity(
                         AU,
                         sample = [
                             8.3 * minute.light,
                             # Somewhere, I've also seen .155 * tera * metre #'
                             .149600 * tera * metre ]), # K&L, NASA and some other source ...
                     Spin(year, 0, plane='Ecliptic'),
                     .0167, # eccentricity; c.f. perihelion & apehelion, below.
                     __doc__ = """The Earth's orbit about the Sun.

The plane of this orbit is known as the ecliptic: all other orbits' inclinations
are given relative to this.  The mean radius of this orbit is known as the
astronomical unit; see AU.__doc__ for details. """),

               discovery=Discovery("the earliest life-forms", -3e9,
                                   etymology="""Earth

From ancient Indo-European 'er', whence sprang lots of words for soil, land,
ground ... earth.
"""),

               magnetic = Object(dipole = 8.1e22 * Ampere * metre * metre),
               mass = 5.976e24 * kilogramme,
               density = 5.518 * kilogramme / litre,
	       age = 1e17 * second, # Nuffield, approx
               Atmosphere = Object(mass = 5.27e18 * kilogramme, pressure = bar,
                                   composition = { "N2": .78, "O2": .21 }),

               Core = Object(name = "Earth's core",
                             surface = Object(radius = 3.488e6 * metre,
                                              flattening = 1/390.),
                             mass = 1.88e24 * metre,
                             density = 10.72 * kilogramme / litre),
               aliases = ('Terra', 'Gaia'))

Earth.surface.radius.observe(6.37814 * mega * metre) # NASA
Earth.mass.observe(5.9742e24 * kilogramme)
Earth.orbit.radius.observe(Quantity(qSample({},
                                            low = 147.1e9, # perihelion
                                            high = 152.1e9, # aphelion
                                            best = 1000001017.8 * 149.597871), # mean
                                    metre))

def KLsurface(radius, gravity, spin, **what):
    """As Surface, but with radius and gravity scaled to Earth = 1."""
    return apply(Surface,
                 (radius * Earth.surface.radius,
                  gravity * Earth.surface.gravity,
                  spin),
                 what)

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
Sun.mass.observe(1.9891e30 * kilogramme) # over 700 times the sum of all the planets' masses
# </bootstrap>

def KLplanet(name, surface, orbit, mass, density, **what):
    """As Planet, but with mass scaled by that of the earth and density in g/cc"""
    what['mass'] = Earth.mass * mass
    what['density'] = density * kilogramme / litre
    return apply(Planet, (name, surface, orbit), what)

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
                discovery = Discovery("early life", -1e9,
                                      etymology = "English: Moon <- Germanic: Mond/Mand"),
                aliases = ('Luna', 'Selene'))
# Moon.orbit.radius' derivative and sample are taken from
# http://news.bbc.co.uk/hi/english/sci/tech/newsid_399000/399468.stm
Moon.mass.observe(7.3483e22 * kilogramme)
Moon.mass.observe(7.349e22 * kilogramme) # NASA
Moon.surface.radius.observe(1.738e6 * metre)
Moon.surface.radius.observe(1.7374e6 * metre) # NASA
Moon.surface.gravity.observe(1.62 * metre / s**2)
Moon.orbit.radius.observe(3.844e8 * metre) # agrees with NASA
Moon.orbit.spin.period.observe(27.32 * day) # NASA

Month = 1/(1/Moon.orbit.spin.period - 1/Earth.orbit.spin.period)
Moon.surface.spin.period.observe(Month) # tidally locked

_rcs_log = """
$Log: home.py,v $
Revision 1.1  2005-03-12 15:05:44  eddy
Initial revision

"""
