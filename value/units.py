# -*- coding: iso-8859-1 -*-
"""Assorted units of measurement.

See SI.py for base units and archaea.py for more arcane units (and references).

$Id: units.py,v 1.30 2008-04-03 06:45:20 eddy Exp $
"""
from SI import *

# Logarithmic units:
from math import log
bel = Quantity(log(10), {},
               """Bel

The Bel is a logarithmic unit, originally the reduction in audio level over one
mile of standard telephone cable but now formalized as a scaling by a factor of
ten.  This is quite a large ratio, so the deci Bel, dB, is more commonly used.
Two Bel equals five astronomical magnitudes.\n""")
magnitude = Quantity(log(.01) / 5, {},
                     """Astromonical apparent magnitude.

The observed brightness of astronomical objects is described on a logarithmic
scale, effectively taking logarithms to base .01**.2 ~= 0.398; if one object's
brightness is one hundred times that of another, then the former's magnitude is
five lower than that of the latter.  This is a tidied-up form of the ancient
Hellenistic astronomers' scheme (popularized by Ptolemy, probably originated by
Hipparchus) for classifying visible stars into six magnitudes; the brightest
stars were first magnitude and the limit of human vision were sixth magnitude.
In the tidied-up scheme, Sirius has magnitude -1.46; the Sun and Moon are even
more negative.

Note that astronomical objects are also assigned 'absolute' magnitudes, which
relates to its luminosity (of which the lumen is unit).  For solar system
objects, this is defined as the apparent magnitude the object would have if it
were 1 AU from each of Earth and Sun (i.e. at one of Earth's stable
Lagrangians); for a star or galaxy, however, the absolute magnitude is defined
to be the apparent magnitude it would have at a distance of 10 parsecs.\n""")

del log

# Angles:
from math import pi
turn = cycle = revolution = 2 * pi * radian
grad = turn / 400 # a unit used by gunners, I believe.
arc = Object(degree = turn / 360,
             __doc__="""The arc-units of angle.

An angle can be expressed by the portion of a circle, centred on the point at
which two lines meet in the given angle, that falls between the two lines.  Such
a portion is known as an 'arc' of the circle.  Its length depends on the radius
of the circle, but its more important characteristic is the angle it 'subtends'
at its centre - that is, the angle we first started with.  Thus arcs are
characterised by angles and a standard family of units of angle is characterised
by the subdivision of a turn into arcs subtending assorted fractions of the
whole.

The classical unit of arc, the degree, is one 360th of a turn.
This arises from the Babylonian approximation of the year as being 360 days
long - nice numerology but bad astronomy.

The separation of two distant objects, as seen in the sky, may sensibly be
described by the angle between the rays from the observer to the objects.  If
we're fussy, we might prefer to replace the observer with the centre of the
Earth, or of its orbit (i.e. the Sun); but, for sufficiently distant objects,
this won't make significant difference.  If you can only measure angles to an
accuracy comparable with the degree, any object outside the solar system and
visible to the unaided human eye is sufficiently distant, for these purposes.

Once astronomers had half-way-decent telescopes, they could resolve angles
(between objects they could see) on much finer scales than the degree (and could
see much fainter objects), so it became meaningful to sub-divide the degree.
For one reason or another, one sixtieth of a degree was chosen as the next
smaller unit.  Presumably by some kind of analogy treating the degree as an
'hour', this unit is known as the 'minute' of arc.

Once telescopes became sufficiently better to enable astronomers to resolve
angles significantly smaller than the minute of arc, it became necessary to
sub-divide the minute.  Having once used a factor of sixty to obtain a minute,
it was natural to further sub-divide the minute by a factor of sixty to obtain a
second of arc.

If we back-derive the 'hour' of arc, it would be one degree of arc.  The next
factor of sixty up from the degree is, conveniently, turn/6 - which is the
internal angle in each corner of an equilateral triangle (i.e. one whose three
internal angles are equal).  This is the angle a twenty-four hour clock's hour
hand sweeps across in four hours or a twelve-hour clock's hour hand sweeps
across in two hours.  If we added, to our clock, a hand which swept one degree
per hour of time, it would complete a whole turn in 360 hours, which is 360 / 24
= 15 days; just one day more than a fortnight and almost half an average month.
One year would then be just slightly over twenty-four and a third turns of this
hand of our clock.\n""",
             point = turn / 32) # there are 32 points on a ship's compass rose
arc.minute = arc.degree / 60
arc.second = second.arc = arc.minute / 60

# Degrees of angle (for temperature, see below):
degree = Object(arc.degree, arc = arc.degree)

# Time
minute = Quantity(60, second, arc=arc.minute)
# bell = 30 * minute # nautical - need to check correctness !
# Not to be confused with the Bel, a tenth of which is the dB.
hour = 60 * minute # should this also be the degree of time ?
day = 24 * hour
week = 7 * day
fortnight, year = 2 * week, 27 * 773 * week / 400 # the Gregorian approximation
month = year / 12 # on average, at least; c.f. planets.Month, the lunar month
# factors of 6**3 seconds abound ...

year.also(Gregorian = year,
          tropical = 365 * day + 5 * hour + 48 * minute + 46 * second,
          sidereal = 365 * day + 6 * hour + 9 * minute + 9.5 * second)
year.tropical.document("""The tropical year.

This is the time between successive vernal equinoxes.  It differs from the
sidereal year because of the precession of the equinoxes: the equinoctial points
move 50.27 seconds of arc per year westwards round the plane of the ecliptic.
""") # so a period of c. 25.78 millennia

year.sidereal.document("""The sidereal year: Earth's orbital period.

This is the period of Earth's orbit relative to the fixed stars.  Contrast the
tropical year (disturbed by the precession of the equinoxes) and the Gregorian
year (an approximation).
""")

# Miscelaneous SI-compatible units (c.f. SI.py), notably cm,g,s ones:
gram, tonne = milli * kilogramme, kilo * kilogramme
km, cm = kilo * metre, centi * metre
cc = cm ** 3
tex = gram / km # fineness of textiles
dtex, denier = deci * tex, deci * tex / .9

St = Stokes = cm**2 / second # kinematic viscosity
Angstrom = .1 * nano * metre    # Ångstrøm or Ångström, aka Å
# (but there's a separate Unicode code-point for the unit ...).
micron, fermi = micro * metre, femto * metre
litre = milli * stere
hectare = hecto * are
barn = (10 * femto * metre) ** 2

stilb = candela / cm**2
phot = 10 * kilo * lux

Gs = Gauss = .1 * milli * Tesla
gamma = nano * Tesla
Mx = Maxwell = 10 * nano * Weber

erg = .1 * micro * Joule
dyn = 10 * micro * Newton

bar = .1 * mega * Pascal
def Centigrade(number): return Kelvin * (number + 273.16)

# Radiation and its effects:
Bq = Becquerel = Hz             # Activity of a radionuclide (events / s)
Gy = Gray = Joule / kilogramme  # Absorbed dose of radiation
Sv = sievert = Gy               # Dose equivalent
# rad = Gy/100 # but conflicts with common abbreviation of radian
rem = 10 * milli * Sv
# 10 milli Gray is also called a rad (but its name conflicts with radian)

Ci = Curie = 37 * giga * Becquerel
R = Roentgen = Quantity(.258, milli * Coulomb / kilogramme,
                       fullname='Röentgen') # also Rontgen ?
Oe = Oersted = kilo * Ampere / metre / 4 / pi # should that be Örsted ?
# see also particle.py for the electron-Volt, eV
Rydberg = 2.17977 * atto * Joule # 13.605698 eV

# Non-SI but (relatively) scientific:
atm = Atmosphere = Quantity(101325, Pascal,
                            """Standard Atmospheric Pressure""",
                            'atm', 'Atmoshpere',
                            technical = kg.weight / cm**2)

torr = mmHg = Quantity(133.322, Pascal,
                       """Torr, the common laboratory unit of pressure.

Named after Evangelista Torricelli (1608-1647), who succeeded Galileo as court
mathematician to Grand Duke Ferdinando II of Tuscany.

The unit is defined as the pressure exerted by a depth of one millimetre of
mercury at zero Celsius under standard gravity.  Actual laboratory observations
should be corrected for the local ambient gravitational field strength and
mercury's thermal expansion.""")

mach = Quantity(331.46, metre / second,
                doc = """The speed of sound in dry air.
\n(at standard temperature and pressure).\n""")

Rankine = Kelvin / 1.8
degree.also(Centigrade = Kelvin, Celsius = Kelvin, C = Kelvin,
            Fahrenheit = Rankine, F = Rankine,
            Reaumur = .8 * Kelvin,
            __doc__ = """The degree.

Various quantities are measured in 'degrees': the name comes from the Latin for
a step (de gradus, I think), which makes it sort-of synonymous with 'unit'.
See arc.__doc__ for the unit of angle with this name.

Several units of temperature share this name, qualified by the originators of
the respective units.  A Swede called Celsius invented a unit which France (and
hence SI) adopted; a Frenchman called Réaumur invented one which the Germans
adopted (until they switched over to SI); and a German called Fahrenheit
invented (before these others, the thermometer and) various units, one of which
remains in use in some backwards parts of the anglophone world.\n""")

degree.__dict__['Réaumur'] = degree.Reaumur
def Fahrenheit(number): return Centigrade((number - 32) / 1.8)
def Reaumur(number): return Centigrade(number * .8)

calorie = Object(international = Quantity(4.1868, Joule, # 3.088 * lb.weight * foot
                                          doc="The international calorie."),
                 thermochemical = Quantity(4.184, Joule,
                                           doc="The thermodynamic calorie."),
                 doc="""The calorie.

This unit is implicated in assorted confusions.  First, there's both an
international one and a thermochemical one, and I've seen the international one
described as the 'thermodynamic calorie (IT)'.  Second, I've heard that some
folk, notably dieticians (allegedly) use 'calorie' to mean kilo calorie.

Values used here are taken from python's Scientific.Physics.PhysicalQuantities
module.\n""")

calorie.borrow((calorie.international + calorie.thermochemical) * .5 +
               (calorie.international - calorie.thermochemical) * tophat)

clausius = kilo * calorie / Kelvin

limit = Object(__doc__ = "Various limiting values, usually for humans",
               # vision ?
               hearing = Quantity(pico, Watt / m / m,
                                  """Threshold of human hearing.

This is the sound intensity conventionally used as base-value when describing
sound intensities in decibels: divide a sound intensity by this and take its log
to base ten and multiply by ten to get the dB (SIL) value for it.  A sound
intensity of one Watt per square metre is thus 12 Bel or 120 dB.\n"""))
