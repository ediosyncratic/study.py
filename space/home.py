# -*- coding: iso-8859-1 -*-
"""Where I come from.

See study.LICENSE for copyright and license information.
"""

from study.value.units import Sample, qSample, Quantity, Object, \
     micro, milli, kilo, mega, giga, tera, peta, exa, zetta, \
     arc, turn, radian, pi, au, year, day, hour, minute, second, \
     kg, metre, km, litre, bar, Watt, Tesla, Gauss, Ampere, Kelvin
from study.value.archaea import mile
import body
from common import Orbit, Spin, Discovery, Surface, \
     SurfacePart, Ocean, Island, Continent, LandMass

from study.chemy.physics import Cosmos
# some rough data from my Nuffield data book:
Universe = body.Object('Observable Universe', None,
                       """The observable universe

See also chemy.physics.Cosmos
""",
                       mass=1e52 * kg, radius=3e26 * metre,
                       # some slightly more definite data:
                       age = Quantity.within(1, .01, 13.7 * giga * year,
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
                        velocity = Quantity.within(
        627, 22, km / second,
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
        l = Quantity.within(273, 3, arc.degree),
        b = Quantity.within(30, 3, arc.degree)),
                        mass = Quantity.fromDecimal(
        2, 0, 43, kg,
        """Estimated mass of the Local Group

The mass inwards from radius R within each galaxy grows in proportion to R,
which might suggest that galactic masses vary in proportion to radii; a simple
uniform area density model in each galactic disk would predict masses
proportional to squares of radii; and a simple spherical model at fixed density
would use the cubes.  When I summed R**P for radii R and power P, dividing the
total by the Milky Way's contribution, the ratio has a minimum of about 4.16 at
about P = 1.9, falling from just under 6 at P = 1 and rising to just over 5 at P
= 3 (however, these results are quite sensitive to variations in the value used
for the radius of the Milky Way, shifting significantly even between 90 and 100
M ly).  It thus seems reasonable to estimate that the mass of the local group is
between four and six times that of the Milky Way.
"""))

# NB: MilkyWay.mass figure used is *not* the temporary one given immediately
# below; see later correction once Sun has been successfully added as a
# satelite.

MilkyWay = body.Galaxy('Milky Way', mass=1.79e41 * kg,
                       # NB: actual mass is O(1e42) - but see below !
                       orbit = Orbit(LocalGroup,
                                     # guess radius: over half way to Andromeda
                                     Quantity.fromDecimal(1.5, 0, 6, year.light),
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
                  Quantity.within(26, 1.4, kilo * year.light),
                  # I've seen figures of 28 k ly, 8 k parsec; I used to quote 3e4 ly
                  # and Wikipedia gives both 26 +/- 1.4 and 25 +/- 1 k ly
                  Spin(Quantity.within(230, 10, mega * year)),
                  # I've seen period figures from 220 to 240 My.
                  None, # I don't know eccentricity, let Orbit guess for us ...
                  # I've seen speed quoted as 215 and 220 km/s
                  speed=Quantity.within(218, 2, km / second)),
    # for mass and surface, see below: Sun.also(...)
    __doc__ = """The Sun: the star at the heart of the solar system.""",
    velocity = Quantity.fromDecimal(
        371, 0, 3, metre / second,
        """Speed of Solar System.

The solar system's motion relative to Cosmic Microwave Background shows up as a
dipole anisotropy, which we can measure fairly accurately.  Its direction is
given both as (l,b) co-ordinates and as (alpha,delta) which appear to be
synonyms for (RA,Dec), the first of which is measured in hours and minutes.  All
four data are here given, as attributes l, b, alpha and delta.
""",
        l = Quantity.within(264.31, .17, arc.degree),
        b = Quantity.within(48.05, .05, arc.degree),
        alpha = Quantity.within(11.2, .01), # unit is hours ...
        delta = Quantity.within(-7.22, 0.8, arc.degree)),

    bright = Quantity.within(1360.8, 1.5,
                             Watt / metre / metre,
                             """How brightly shines the sun ?

Maximum solar total radiant power outside atmosphere at a distance of
1 Astronomical Unit from the Sun.  Natural variability is roughly 1.5
Watt / metre**2, or about 0.1%.  It varies cyclically with periods of
about 11, 88, 208 and about 1000 years (the last are called, to my
delight, Eddy cycles).  The Earth's orbit has variable distance from
the Sun, leading to a 6.9% variation in how much solar radiation
reaches Earth, independent of any variation in Sun.bright (whose
distance from the Sun is fixed).  Earth's atmosphere and clouds
further interfere so that only about 550 to 1025 W/m^2 actually
reaches the surface.

This radiant power corresponds to an astronomical apparent magnitude
of -26.73 (it should be possible to work out the zero-point of the
astronomical magnitude scale from this ...).  For the photon pressure
due to this radiant power, see Sun.bright.pressure.  For the
corresponding neutrino flux, see Sun.bright.neutrino.  Both .bright
and .bright.neutrino deserve to be multiplied by AU**2/steradian to
get the more natural solar output per solid angle - regarless of
distance - of each kind.

The Earth's cross-section, the area relevant to this power supply, is a quarter
of its surface area; multiplying that by the solar constant, Earth is receiving
energy at 179 peta-Watts (albeit reflecting 39% of this, about 70 peta-Watts);
dividing by the square of the speed of light, that's the energy-equivalent of 2
kilogrammes per second.  Naively interpreting the energy-generation process as
consisting of the direct conversion of Hydrogen(1) to Iron(56), with a mass
defecit of half a gram per mole of Iron produced, this implies the production of
4 kilo moles of Iron (from 224 k mol of Hydrogen) per second: thus Earth is the
beneficiary of about a quarter tonne per second of the Sun's fuel being used
up.  (The real figure shall be higher: the sun isn't presently burning all the
way to iron, so take more raw materials only part way.)

The total surface area of the sphere of radius one AU is 28e22 square metres
(22e8 times Earth's cross-section, above), implying that the Sun's total power
output is 390 yotta-Watts, the mass-equivalent of 4.4 million tonnes per
second. This implies, assuming total conversion to Iron again, that the Sun
consumes 49e10 kg/s of its fuel - in the space of five millennia, this adds up
to a little over the Moon's mass - and that, in the four giga-years it's been
shining thus far, the Sun has consumed about 3 percent of its fuel, reducing its
mass by about one part in three thousand.  The amount of iron produced (in this,
I stress again, naive model) is nearly twelve thousand times the mass of the
Earth; or over 25 times the mass of all the planets put together.  (A more
realistic model turns Hydrogen into Helium, Carbon, Nitrogen and Oxygen, in
larger quantities.)

The gravitational potential energy stored in a uniform spherical body (due to
gravitational attraction among its parts) is 3/5 of the obvious -G.M.M/R, with R
its surface radius and M its mass; so that of the Sun is of order 2e41 Joules,
equal to about 18 million years' worth of solar constant power output.  I
compute 2.53 zetta tonnes mass-equivalent for this naive model, equivalent to
about one thousandth of the Sun's total mass, about three times the reduction in
mass computed above from fusion.  In the course of pulling the Sun together, we
should take account of the work done increasing its temperature and compressing
it; but my crude sums indicate these are much smaller (of order exa tonnes).
""",
                             fullname = "Solar Constant",
                             neutrino=Quantity.fromDecimal(
            65, units=10 * tera / metre**2, doc="""Solar neutrino flux at Earth.

This just counts how many neutrinos cross unit area, facing the sun, at 1 AU,
per unit time.  No account is taken of the energy or momentum distributions
among these neutrinos.  They are roughly one third each of the three types of
antineutrino.  See Sun.bright for related observations.
""")),

    reactions = """The chain of fusion reactions in The Sun.

We start with protons (p); reactions produce neutrons (n), light
(&gamma;), neutrinos (&nu;), elecrtons (e-) and positrons (e+).  I
don't distinguish neutrinos from their antiparticles, here.  I'll use
^{number} to indicate a superscript, place on the left of an element's
symbol to indicate an isotope's atomic weight.  The three principal
reactions are:

 * p +p -> D +e+ +&nu; (D is the deuteron, p+n)
 * p +D -> ^{3}He +&gamma;
 * ^{3}He +^{3}He -> ^{4}He +p +p

That process turns six protons into an alpha particle (^{4}He) and two
protons.  By these three reactions, our Sun turns 6e11 kg/s of protons
to alpha particles; 0.4% of the mass of that is converted to energy in
the process, due to ^{4}He's mass being less than that of four
protons.

These processes, at O(10 MK), convert some Hydrogen to Helium; towards
the end of The Sun's life, it's core shall get hot enough, O(100 MK),
to fuse some Helium to make Carbon; and then some Helium and Carbon to
make Oxygen.  I guess, during that process, some Hydrogen and
Deuterium shall get involved in the hotter reactions to make some
Beryllium, Boron and Nitrogen.  All of which shall remain locked up in
its white dwarf corpse, even though the radiation it produces as a
by-product of those reactions shall drive off an outer layer of
Hydrogen and Helium.

Another source [*] gives the early universe's reactions as:

 * p +n -> D
 * p +D -> ^{3}He
 * D +D -> ^{3}He +n
 * D +D -> ^{3}H +p
 * D +^{3}He -> ^{4}He +p
 * D +^{3}H -> ^{4}He +n

(and a little Li) based on the early quark-gluon plasma having
significantly more protons than neutrons (and this is the first
quarter hour after the big bang, so only about half the neutrons had
time to decay).
[*] https://www.youtube.com/watch?v=9sn9eBL6cFk
""",

    progenitors = """The origin of our solar system.

The fact that the solar system contains notable amounts of elements
other than Hydrogen and Helium implies the system condensed out of a
galactic cloud that contained the heavier elements, notably including
the Oxygen, Carbon, Nitrogen, Phosphorous and Sulphur crucial to life,
as well as the Silicon, Iron and other (relatively) heavy elements
that make up rocks.

Small stars don't release much of the light elements, beyond Helium,
that they do create; those light elements, up to Oxygen, remain in the
white dwarf remnant that is the star's corpose.  Heavier elements are
formed in larger stars.  These, once their cores run low on Hydrogen,
fuse Helium with, initially, itself to make Carbon, then with that to
make Oxygen and with that to make Neon; they can then fuse Carbon with
Carbon to make Magnesium and Oxygen with Oxygen, with alpha particle
byproduct, to make Silicon.  All of this is built from Helium, so
even-numbered elements tend to be more abundant than the odd-numbered
ones of similar mass, typically by a factor of ten (albeit with
odd-numbered Hydrogen is the obvious outlier).  This continues, Helium
fusing with Silicon to make Sulphur, with that to make Argon and then,
successively, Calcium, Titanium, Chromium, ^{52}Iron and ^{56}Nickel,
which finally beta-decays to ^{56}Iron.  There's also a channel for
two ^{28}Si to combine directly into one ^{56}Ni, before that last
decay.

Along the way, some stray H-fusion can produce the intervening
odd-numbered elements.  Each stage of that fusion chain takes a
significantly higher temperature and pressure, so a given star spends
less of its life (by a large factor, at each step) pushing up each
successive step.

When a large enough star reaches the end of its life, the core of Iron
(and Nickel, about to decay back into Iron), crushed by gravity,
collapses in on itself; this is the only situation in which any
element heavier than Nickel is synthesised, in the space of a couple
of seconds at the end of a big star's life.  The collapse heats the
core so hot that the outer layers, including even much of the core, in
which Iron and all heavier elements are made, get blown off in a
superhova, that outshines a galaxy for a few days.  The majority of
the mass of such a star is lost in the explosion.

As smaller stars only blow off their mostly-Hydrogen outer layers, all
heavier elements, even Carbon and upwards, found anywhere but the core
of a star, were supernova ejecta at some point.  (We are star-dust.)
Everything heavier than Iron is rare (yet Iron is abundant).
""",

    wind=Object(speed=Quantity.flat(.3, .8, .4, mega * metre / second),
                # I suppose that's speed at the Sun or Earth and it slows on its
                # way out (c.f. study.space.Kuiper.Heliosphere).
                # TODO: check back on unfinished page, for composition:
                # http://solarscience.msfc.nasa.gov/feature4.shtml
                source="Corona"),

    discovery=Discovery("the earliest life-forms", -3e9), # etym ?

    density = 1.409 * kg / litre,
    age = Quantity(4.6e9, year, # Peter Francis, age of solar system [missing error bar]
                   lifespan = 1e18 * second, # Nuffield, order of magnitude
                   # remain could include a further c. 2e9 years as a white dwarf
                   remain = Quantity.fromDecimal(5, 0, 9, year)),
    magnitude = Quantity.flat(4.79, 4.83), # K&L, Moore
    aliases = ('Sol',))

del load_rubble

# Common aliases for the atronomical unit (unit astronomique):
ua = AstronomicalUnit = AU = au
# Important derived unit:
parsec = Quantity(radian / arc.second, au,
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
Sun.bright.also(pressure = Quantity(2 * second / second.light, Sun.bright,
                                    """Photon pressure from the Sun at Earth.

The light from the sun carries momentum as well as energy.  If we put a mirror
in the path of the sun-light to reflect all of the light back to the sun, all
of that momentum gets reversed, so the mirror receives twice the momentum of
the light, thereby exerting a pressure equal to 2/c times the incident power,
Sun.bright.  This works out at about five micro-Pascal at Earth's orbit.
"""))

# surface part data taken from Asimov:
_square_kilo_mile = (kilo * mile)**2
def IAcontinent(name, area=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    what['name'] = name
    return Continent(*parts, **what)

def IAisland(name, area=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    what['name'] = name
    return Island(*parts, **what)

def IAocean(name, area=None, depth=None, *parts, **what):
    if area is not None: what['area'] = area * _square_kilo_mile
    if depth is not None: what['depth'] = depth * mile
    what['name'] = name
    return Ocean(*parts, **what)

# Equation of Time, for use as Earth.EoT:
def equationoftime(when = None, within = Quantity.within, rad = radian, unit = minute,
                   # Maps angle-multiplier to (scale, offset) pair:
                   terms = {1: (7.353, 6.209), 2: (9.927, 0.37),
                            3: (0.337, 0.304), 4: (0.232, 0.715)}):
    """Actual solar time time minus mean solar time.

    Single argument, when, is optional (defaults to now); if given
    (and not None), it should be a datetime.datetime instance
    representing, as a local time, the moment at which the discrepancy
    is desired.  Pass no further arguments.  The result is a quantity
    of kind time.
    """
    if when is None:
        from datetime import datetime
        gap = datetime.now() - datetime(2000, 1, 1, 12)
    else:
        gap = when - when.__class__(2000, 1, 1, 12)
    # See Fourier Method on https://equation-of-time.info/calculating-the-equation-of-time
    cycle = within((gap.total_seconds() / 3600 / 6) % 1461, 1e-5 / 216)
    theta = cycle * 0.004301 * rad
    mins = 0.019 + sum(p[0] * (k * theta + p[1] * rad).Sin for k, p in terms.items())
    return - mins * unit

# My home planet:
Earth = body.Planet(
    'Earth',
    Surface(Quantity(Sample.flat(6356912, 6378388, 6371020,
                                 low=6352400, high=6384100),
                     metre, """Radius of the Earth's surface.

This is how far mean sea level is from the Earth's centre; which varies
significantly between the poles and equator, thanks to centrifugal force.  I've
included, as .low and .high, the extremes of the radius at the Earth's solid
surface, 4.5 km below sea level in the Arctic and 5.89 km above sea level at the
top of Mount Kilimanjaro, in Africa.  See also: altitude.\n"""),
            # radius should also be nauticalMile * 60 * 180 / math.pi

            Quantity.flat(9.78 * metre / second**2, # equatorial
                          9.83 * metre / second**2, # polar
                          # Definition, from special property of masses:
                          kg.force / kg, {},
                          """Earth's surface gravity

This is the rate at which an unsupported compact body accelerates
downward while near sea level.  It is equally the ratio of the force
one must use to support a massive body, so that it stays still rather
than falling, to the mass of that body.

It varies with location around the world, due to the centrifugal
effect of the Earth's spin (pulling away from the spin axis, so only
parallel to gravity at the equator) and the consequent variation in
distance of sea level from Earth's centre of mass (reducing the field
strength according to the inverse square law).  This results in low
values near the equator and high values near the poles.  There is also
some local variation due to the non-uniformity of Earth's mass
distribution.

The measured downward acceleration of an unsupported body, or weight
to mass ratio of a body, above the solid surface of the Earth will
also depend on how far above sea level it is, diminishing as altitude
increases.

The usual standard gravity value of 9.81 m/s/s is representative of
values measured around 45 degrees either side of the equator,
reflecting the fact that Europeans set the standard; most of the
Earth's surface (and population) experiences a lower value.  The
median (by surface area) is probably closer to 9.80 m/s/s (the value
at roughly 30 degrees either side of the equator, e.g. the northern
and southern extremities of Africa).

Source:
https://www.mezzacotta.net/100proofs/archives/358
"""),

            Spin(day.sidereal, # day -4 min +4 seconds
                 # 4.5 Gyr ago, period was 5 hours; and
                 # Moon.orbit.radius has since *= 10.
                 # Tilt varies between 22.1 and 24.5 degrees in 41 k yr cycle,
                 # due to reach a minimum in c. 1000 AD; (eh ? ten thousand ?)
                 23.44, # currently decreasing at 47 * arc.second / century
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
                                       steam = 14.17e15 * kg),
                        area = 362 * tera * metre**2, # 0.708 of Earth.surface.area
                        rainfall = 399 * peta * kg / year,
                        material = "water"),

            # Land
            SurfacePart(IAcontinent('Africa', 11.5),
                        # arguably Africa is part of the same land-mass as Eurasia !
                        LandMass(IAcontinent('Asia', 16.5),
                                 IAcontinent('Europe', 3.8),
                                 name = 'Eurasia'),
                        LandMass(IAcontinent('South America', 7.035),
                                 IAcontinent('North America', 9.385),
                                 name = 'America'),
                        IAcontinent('Antarctica', 5.1),
                        IAcontinent('Australia', 2.971021),
                        IAcontinent('Greenland', .84),
                        # TODO: split between their respective continents:
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
                        height = Quantity.below(8840, metre, mean=840),
                        area = 149.3 * tera * metre**2, # 0.292 of Earth.surface.area
                        rainfall = 106.7 * peta * kg / year,
                        material = "basalt, granite"),

            # misc other data:
            rainfall = Quantity(506 * peta, kg / year, """Earth's total rainfall.

Wikipedia's page [0] on rain gives 0.99 metres / year averaged over
the surface.  The average over land, from the same source, is only
0.715 metres / year; this implies the average over seas and oceans is
about 1.095 metres / year.  Thus about 78% of Earth's precipitation
lands on salt water.

The Wikipedia data implies c. half a million km**3/year, consistent
also with data I've seen on [1] (at a page now long gone).  The
earliest version of this module gave about an eighth of that, but I've
no idea where I got that figure.

[0] https://en.wikipedia.org/wiki/Rain
[1] http://www.globalchange.umich.edu/
"""), # Ocean and Land shares of that given above.
            flattening = 1 / 298.25,
            albedo = .39, # so 61% of incident radiation is absorbed
            # but total solar power available at surface is 1/8 of that in space:
            # http://www.physorg.com/news117649731.html
            # This may be about which frequencies solar panels can use.
            magnetic = Object(
                field = Quantity(31.2 * micro, Tesla,
                                 doc = """Earth's magnetic field.

See body.Body.magnetic; Earth's B_0 is given here, implying a magnetic
dipole of about 81e21 A.m^2, equal to the field of a 634 MA current
circling the equator.  The actual magnetic field of the Earth is much
more complex than a dipole, though, with strength varying all over the
range from 20 to 80 micro Tesla.

The strength of Earth's magnetic field appears to be decreasing at
about 5% per century at present, though its average over the last 2500
years is more like 1.6% per century:
http://www.spectrum.ieee.org/nov06/4708
"""),
                London = Object( # Latitude 51 degree, 30 minute, 26 second
                    sampled = '1960',
                    horizontal = 1.87e-5 * Tesla,
                    vertical = 4.36e-5 * Tesla)),
            EoT = equationoftime,

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
          # See below for more {peri,ap}helion data.
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
          Quantity(16.7 * milli, {},
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
    mass = 5.976e24 * kg,
    density = 5.518 * kg / litre,
    age = 1e17 * second, # Nuffield, approx
    Atmosphere = Object(mass = 5.27e18 * kg, pressure = bar,
                        composition = { "N2": .7809, "O2": .2095, 'Ar': .0093,
                                        # partial volumes of constituent gasses in dry air
                                        # there's also usually some 'H2O' in it !
                                        'CO2': 3e-4, 'Ne': 1.8e-5, 'He': 5e-6,
                                        'CH4': 2e-6, 'Kr': 1e-6, 'Xe': 1e-7 }),

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
# DR Becky (2024) supplied {peri,ap}helion values and dates.
# These vary with Milankovitch cycle, on periods O(10 k yr).
# No idea where I got the rather eccentric best estimate.
Earth.orbit.radius.observe(Quantity.flat(146.915696e9, # perihelion, Jan 2nd
                                         152.458496e9, # aphelion, Jul 5th
                                         1000001017.8 * 149.597871) * metre)

Earth.magnetic.dipole.also(
    # N pole on Ellesmere Island, c. 11 degrees off spin axis
    latitude = 79 * arc.degree,
    longitude = -78 * arc.degree) # West is -ve
# https://en.wikipedia.org/wiki/Magnetopause#cite_note-13
Earth.magnetic.also(moment = Quantity(7.90627, Tesla / metre**3))

del IAcontinent, IAisland, IAocean, equationoftime

def KLsurface(radius, gravity, spin, S=Surface, G=Earth.surface, **what):
    """As Surface, but with radius and gravity scaled to Earth = 1."""
    return S(radius * G.radius, gravity * G.gravity, spin, **what)

# Sol: reprise
Sun.also(
    mass = 332946 * Earth.mass,
    surface = KLsurface(109.12,
                        27.96,
                        Spin(25 * day + 9 * hour, 7.2),
                        flattening = 0.0,
                        radiation = 63.3 * mega * Watt / metre**2,
                        magnetic = Object(
            field = Quantity(Quantity.flat(1, 2) / 1e4, Tesla,
                             doc = """Solar magnetic field strengh.

The solar magnetic field varies significantly on various time-scales,
longer than its spin period (25 days), including the 11-year cycle of
sunspot activity.  The field is of order 1 to 2 Gauss (= Tesla/1e4) at
the surface generally, but rises to significant fractions of a Tesla
in sunspots.

For contrast, magnetars have magnetic surface field strengths of order
peta-Gauss (100 GT) - http://antwrp.gsfc.nasa.gov/apod/ap010901.html
""")),
                        temperature = 5800 * Kelvin))
Sun.orbit.altitude = Quantity.flat(5, 30, None, parsec,
                                   "Distance from galactic disk-plane")
Sun.surface.radius.observe(6.96e8 * metre)
Sun.mass.observe(1.9891e30 * kg) # over 700 times the sum of all the planets' masses
# </bootstrap>

# Milky way: reprise
# http://en.wikipedia.org/wiki/Milky_Way among other sources
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
    radius=Quantity.within(51.5, 2.5, kilo * year.light,
                           "Radius of the Milky Way's visible thin disk",
                           # bulge = ... ?
                           bar = Quantity.fromDecimal(
            13.5, 0, 3, year.light, "Radius of the Milky Way's central bar"),
                           halo = Quantity.within(
            100, 15, kilo * year.light, "Radius of the Milky Way's spherical stellar halo")),
    # same apod gave "200 billion"; David Darling (see URL below) gives 200 to
    # 400 billion, not counting brown dwarves.
    starcount=Quantity.within(3, 1, rescale=100 * giga),
    age=Quantity.within(8.3, 1.9, giga * year,
                        "Age of the Milky Way's galactic thin disk",
                        # and from an article in el Reg:
                        oldest=Quantity.within(
            13.6, .4, giga * year, "Age of the oldest stars in the Mikly Way")),
    spin=Spin(Quantity.within(50, 5, mega * year),
              "Rotation rate of the Milky Way's Spiral pattern",
              bar=Spin(Quantity.within(17.5, 1.5, mega * year),
                       "Rotation rate of the Milky Way's central bar"),
              speed=Quantity.within(225, 7.5, kilo * metre / second,
                                    """Typical stellar orbital speed.

Stars outside the central bulge of the Milky Way but not so far out as the outer
rim all move at roughly the same speed (instead of exhibiting a Keplerian
drop-off of speed with distance from the centre.  This implies that the total
mass closer to the galactic centre than any given radius R is proportional to R,
for R larger than the radius of the central bulge but smaller than the
rim-radius.  The discrepancy between this and the visible matter density is
evidence for dark matter.\n""")),
    # Correct mass value (previous figure served to make adding Sun go smoothly):
    mass=Quantity.within(2, 1, tera * Sun.mass,
                         """Mass of our Galaxy

About 5 to 10 % of the Galaxy's mass is in gas and dust; its total mass, in tera
Sun.mass, has been given as 1 to 2 (revised upwards from around a fifth of that
since the 1990s) but estimates of around 3 are becoming (2009) more usual.  This
puts the Milky Way on a par with (rather than smaller than, as previously
supposed) Andromeda.\n"""),
    # http://www.daviddarling.info/encyclopedia/G/Galaxy.html
    density=Quantity(0.1, Sun.mass / parsec**3,
                     "Mean density of our Galaxy."),
    thick=Quantity.within(2.45, .15, kilo * year.light,
                          "Thickness of the stellar disk of the Milky Way",
                          bulge=Quantity(16, kilo * year.light,
                                         "Thickness of our Galaxy's central bulge"),
                          gas=Quantity(
            12, kilo * year.light, "Thickness of gas disk enclosing the Milky Way's disk")),
    luminosity=Quantity.fromDecimal(1, 0, 36, Watt),
    magnetic = Object(field = Quantity.within(
            4, 1, micro * Gauss,
            """Background magnetic field strength of our Galaxy.

The central bulge has much stronger magnetic fields.
""")))

def KLplanet(name, surface, orbit, mass, density, P=body.Planet, d=kg/litre, **what):
    """As Planet, but with mass scaled by that of the earth and density in g/cc"""
    what.update({'mass': Earth.mass * mass,
                 'density': density * d,
                 'surface': surface,
                 'orbit': orbit})
    return P(name, **what)

# Kaye & Laby also give, for orbits, synodic periods and longitudes of node and
# perihelion (closest approach).  Several of the following also get .observe
# applied to some of their data, giving the /usr/share/misc/units value.

Moon = KLplanet('Moon',
                KLsurface(.272, .17, Spin(29.4 * day, 5.2), material="silicates"),
                Orbit(Earth,
                      # Apogee: 405.5 Mm, Perigee: 363.3 Mm
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
                aliases = ('Luna', 'Selene'),
                doc = """Earth's principal satellite

The moon formed from the coallescing of the debris knocked loose when
the two planet-scale bodies sharing an orbit collided and coalesced to
form The Earth.  By the time they collided, they had settled down
enough to separate out by density so that the matter that comprises
the crusts of such planets was mostly near the surface.  Consequently
such matter, of the kinds that make up the Earth's crust today, made
up most of what was shed in the collision.  The resulting Earth thus
has a relatively thin crust compared to the scale of its core and
mantle, compared to comparable-sized planets formed only by gradual
accretion, without any cataclysmic collision of large equals.  The
Moon, as a result, consists mostly of crustal material, so its density
is considerably lower than that of most rocky bodies of comparable
mass.

The Moon's orbit wobbles on an 18.61 year cycle known as the 'lunar
nodal cycle'.  This affects the lunar tides enough to influence the
lives of coastal ecosystems.
""")
# Moon.orbit.radius' derivative and sample are taken from
# http://news.bbc.co.uk/hi/english/sci/tech/newsid_399000/399468.stm
Moon.mass.observe(7.3483e22 * kg)
Moon.mass.observe(7.349e22 * kg) # NASA
Moon.surface.radius.observe(1.738e6 * metre) # APOD 2008/Aug/1
Moon.surface.radius.observe(1.7374e6 * metre) # NASA
Moon.surface.gravity.observe(1.62 * metre / second**2)
Moon.orbit.radius.observe(3.844e8 * metre) # agrees with NASA
Moon.orbit.spin.period.observe(27.32 * day) # NASA

Month = 1/(1/Moon.orbit.spin.period - 1/Earth.orbit.spin.period)
Moon.surface.spin.period.observe(Month) # tidally locked
Month.document("""The mean period of lunar phases, as seen from Earth.

Hipparchus (confirmed and) recorded what earlier sources had shown,
that 4267 times this period is a pretty good approximation to various
whole number multiples of several other astronomical periods.  See his
Wikipedia page for details, but two such approximately equal periods
are are 345 years and 12,607 days.
""")

del body, Orbit, Spin, Discovery, Surface, SurfacePart, Ocean, Island, Continent, LandMass, \
    Sample, qSample, Quantity, Object, micro, milli, kilo, mega, giga, tera, peta, \
    kg, metre, mile, arc, radian, pi, Kelvin, year, day, hour, minute, second, \
    litre, bar, Watt, Tesla, Ampere, Gauss
