# -*- coding: iso-8859-1 -*-
"""Assorted units of measurement.

See SI.py for base units and archaea.py for more arcane units (and references).
"""
from SI import *

# Logarithmic units:
from math import log

bel = Quantity(log(10), {},
               """Bel

The Bel is a logarithmic unit, originally the reduction in audio level over
one mile of standard telephone cable but now formalized as a scaling by a
factor of ten.  This is quite a large ratio, so the deci Bel, dB, is more
commonly used. Two Bel equals five astronomical magnitudes.\n""")

magnitude = Quantity(log(.01) / 5, {},
                     """Astromonical apparent magnitude.

The observed brightness of astronomical objects is described on a logarithmic
scale, effectively taking logarithms to base .01**.2 ~= 0.398; if one object's
brightness is one hundred times that of another, then the former's magnitude
is five lower than that of the latter.  This is a tidied-up form of the
ancient Hellenistic astronomers' scheme (popularized by Ptolemy, probably
originated by Hipparchus) for classifying visible stars into six magnitudes;
the brightest stars were first magnitude and the limit of human vision were
sixth magnitude. In the tidied-up scheme, Sirius has magnitude -1.46; the Sun
and Moon are even more negative.

Note that astronomical objects are also assigned 'absolute' magnitudes, which
relates to its luminosity (of which the lumen is unit).  For solar system
objects, this is defined as the apparent magnitude the object would have if it
were 1 AU from each of Earth and Sun (i.e. at one of Earth's stable
Lagrangians); for a star or galaxy, however, the absolute magnitude is defined
to be the apparent magnitude it would have at a distance of 10 parsecs.\n""")

del log

# Time
minute = Quantity(60, second)
# bell = 30 * minute # nautical - need to check correctness !
# Not to be confused with the Bel, a tenth of which is the dB.
hour = 60 * minute # should this also be the degree of time ?
day = 24 * hour
week = 7 * day
fortnight = 2 * week

year = Quantity.flat(365.242, 365.25636, 365.24225,
                     day,
                     """The period of relative motion of Earth and Sun.

The year originated in each human culture as the period over which the cycle
of the Earthly seasons repeats itself.  The seasons have fuzzy boundaries,
which, historically, made it difficult to measure this period with any
precision; but also makes it possible to be fairly forgiving of minor
discrepancies in one's estimate of it.

Each day the Sun moves from East to West across the sky (due to the Earth's
spinning, rather than actual relative motion of Earth and Sun) but, from day
to day; its path moves North for many days, then slows, stops and travels
South for about as long before slowing, stopping and turning North once more;
as it goes about that cycle, the length of time the Sun takes to cross the sky
lengthens and shortens.  These variations affect the weather and living
creatures respond in diverse ways; the cycles in the Sun's variations incline
these consequences to be likewise cyclic; but the atmosphere and life are
complex, so their responses to the solar cycle are at best roughly
cyclic.  The fuzzy notion of season is a characterisation, of the agregate of
all the diverse cycles of life and weather that roughly follow the Sun's
cycle, that helps one to anticipate the weather, the availability of diverse
foods in diverse places and the movements of dangerous beasts; anticipating
these improves one's chances of prosperity and safety.  Cultures that provide
a straightforward framework for keeping track of the cycle of the seasons have
thus also prospered, to the point where each culture has some such framework;
and each such framework effectively embeds an estimate of the length of the
underlying period of the solar cycle - this period is idealised by the
tropical year (see year.tropical for details).

Once folk got moderately skilled at astronomy, it became evident that there is
another cycle with almost the same period: folk first noticed it as the period
of the Sun's movement relative to the pattern of fixed stars (where the
tropical cycle is relative to the world around us - its horizon and such
land-marks as folk may use as references when observing the Sun's movement);
later, folk learned to characterise this movement as a parallax effect caused
by the Earth's orbit around the Sun.  This cycle's period is called the
sidereal year (see year.sidereal) and is longer than the tropical year by
twenty and a bit minutes.  The tropical year is the natural one to use for
Earthly activity (which is all any calendar has ever mattered for thus far),
so it's what a calendar aims to match; but the sidereal one is easier to
measure precisely, once one has the necessary astronomical skills.

Being a day or several short would lead to appreciable drift between the
observed seasons and the calendar's labelling of dates: a culture would need
to adapt to it, each generation sowing its crops a modest way later in the
'year' than the previous used to.  Using a raw 360-day calendar (with no
intercallary days - apparently some Zoroastrian sects use this) would lead to
a full season's drift within eighteen years, so each generation would grow up
with different associations of calendar dates with seasons from those their
parents grew up with.  Approximating the year as 52 weeks (364 days), though
convenient, would still leave enough drift to be significant across a
life-time (at least for those who live to be old); the calendar would drift by
a season within 74 years.

Using 365 days (the 'vague year') as approximation (almost a quarter day off)
would reduce the drift to a week or three in each lifetime; even a
pre-literate culture might well notice that (e.g. a feast associated with a
particular calendar date might be associated, in old folk tales, with a
different season than one is used to), but one could readily enough adapt to
it.  See year.Julian, .Gregorian and .Herschel for better approximations; each
of which comes with a .leap(yrno) method that returns true if the year with
sequence number yrno is a leap year (of 366 days) in the given calendar;
otherwise, it's a normal year (of 365 days).

Meanwhile, it should be noted that the Earth's spin is slowing, causing the
length of the day to increase by 1.70 +/- .05 milliseconds per century; the
length of the year is relatively stable, so the number of days in a year is
decreasing at one per 13.9 million years; the 7.5 milli-days of difference
between Gregorian and Julian years corresponds to about 104 millennia of this
variation.  However, this variation is itself varying; although a major factor
in it is the tidal slowing of the Earth (which isn't varying much), this is
complicated by the Earth having changed shape over the last ten millennia due
to ice sheets (that used to press down on the poles) having melted, leading to
the equatorial radius decreasing while the polar radius increases, thereby
reducing Earth's moment of inertia without affecting its angular momentum, so
causing it to spin faster than it would have, partially countering the tidal
drag.  As Earth's shape stabilises, the day length increase shall increase
towards c. 2.3 ms/century.
""",

                     Julian = Quantity(3 * 487, day / 4,
                                       """The Julian year.

This is the nominal average duration of one year in the calendar that Rome
switched to using under Julius Caesar.  The extra day once every four years
reduces the error to the point where, to drift a week, the calendar must be in
use for nearly nine centures; and, indeed, it remained in use in Rome for over
1500 years (see year.Gregorian) and elsewhere until the 20th century.
""",
                                       leap=lambda yrno: yrno % 4 == 0),

                     Gregorian = Quantity(27 * 773, week / 400,
                                          """The Gregorian year.

This is the nominal average duration of one year in the Gregorian calendar,
introduced by Pope Gregory XIII in 1582.  See the documentation of year for
general discussion of available approximations to it.

The Gregorian calendar uses a cycle of 400 years, mostly of 365 days but with
97 'leap years' of 366 days (namely, those years whose sequence number is a
multiple of four, excluding the ones whose number is a multiple of 100 but not
of 400).  The average for this is 365 +97/400, which works out at
        365 * day + 5 * hour + 49 * minute + 12 * second

This is a mere 26 seconds longer than the tropical year, so the seasons drift
so slowly we don't notice; in ten millennia (longer than documented history)
they'll only drift by about three days, which is still small enough to be lost
in the fuzziness of the seasons' boundaries.
""",
                                          leap=lambda yrno: (
            yrno % 4 == 0 and (yrno % 100 != 0 or yrno % 400 == 0))),

                     Herschel = Quantity(365.24225, day,
                                         """The Herschel year.

Sir John Herschel advocated, as a refinement to the Gregorian calendar's
approximation to the year (see year.Gregorian), letting years whose number is
a multiple of 4000 be normal, rather than leap; this would not affect any date
for which the Gregorian calendar has been in use (so it's backwards
compatible) and would improve the precision of the calendar's estimate of the
year length to 365 * day + 5 * hour + 48 * minute + 50.4 * second, which is
less than five seconds above the tropical year.  We don't have to make up our
minds about that for a millennium or two, though ;-)
""",
                                         leap=lambda yrno: (
            yrno % 4 == 0 and (yrno % 100 != 0 or (
                    yrno % 400 == 0 and yrno % 4000 != 0)))),

                     tropical = Quantity(1, 365 * day +
                                         5 * hour + 48 * minute + 46 * second,
                                         """The tropical year.

This is the time between successive vernal equinoxes.  It differs from the
sidereal year because of the precession of the equinoxes: the equinoctial
points move 50.27 seconds of arc per year westwards round the plane of the
ecliptic.
"""), # so a period of c. 25.78 millennia

                     sidereal = Quantity(1, 365 * day +
                                         6 * hour + 9 * minute + 9.5 * second,
                                         """The sidereal year.

This is the period of Earth's orbit about the Sun relative to the fixed
stars.  Contrast the tropical year (disturbed by the precession of the
equinoxes) and the Gregorian year (an approximation).
"""),

                     anomalistic = Quantity(1, 365 * day + 6 * hour +
                                            13 * minute + 52.6 * second,
                                            """The anomalistic year.

This is the interval between successive perihelions of Earth's orbit.  Since
the major axis of the orbit isn't exactly fixed, this isn't quite the same as
the sidereal year ...
"""))
month = year / 12 # on average, at least; c.f. planets.Month, the lunar month
# factors of 6**3 seconds abound ...

# Angles:
from math import pi
turn = cycle = revolution = 2 * pi * radian
arc = Object(degree = turn / 360,
             __doc__="""The arc-units of angle.

An angle can be expressed by the portion of a circle, centred on the point at
which two lines meet in the given angle, that falls between the two
lines.  Such a portion is known as an 'arc' of the circle.  Its length depends
on the radius of the circle, but its more important characteristic is the
angle it 'subtends' at its centre - that is, the angle we first started
with.  Thus arcs are characterised by angles and a standard family of units of
angle is characterised by the subdivision of a turn into arcs subtending
assorted fractions of the whole.

The classical unit of arc, the degree, is one 360th of a turn.  This dates
back to the Babylonians, who (probably didn't actually suffer the delusion
that the year was tidily 360 days long, whatever popular myth may say, but)
liked factors of 60 and 6.

The separation of two distant objects, as seen in the sky, may sensibly be
described by the angle between the rays from the observer to the objects.  If
we're fussy, we might prefer to replace the observer with the centre of the
Earth, or of its orbit (i.e. the Sun); but, for sufficiently distant objects,
this won't make significant difference.  If you can only measure angles to an
accuracy comparable with the degree, any object outside the solar system and
visible to the unaided human eye is sufficiently distant, for these purposes.

Once astronomers had half-way-decent telescopes, they could resolve angles
(between objects they could see) on much finer scales than the degree (and
could see much fainter objects), so it became meaningful to sub-divide the
degree.  A first subdivision by a factor of sixty giave a 'minute' (synonym of
tiny) unit, which became known as a 'minute' (pronounced differently).

Once telescopes became sufficiently better to enable astronomers to resolve
angles significantly smaller than the minute of arc, it became necessary to
sub-divide the minute.  Subdividing again by a factor of sixty was a 'second'
(minute, in the 'tiny' sense) sub-division so it's known as the second of
arc.  (The exact same operation applied to an hour subdivided it into a minute
sub-division and a second minute sub-division to produce the units of time
with the same names.)

If we back-derive the 'hour' of arc, it would be one degree of arc.  The next
factor of sixty up from the degree is, conveniently, turn/6 - which is the
internal angle in each corner of an equilateral triangle (i.e. one whose three
internal angles are equal).  This is the angle a twenty-four hour clock's hour
hand sweeps across in four hours or a twelve-hour clock's hour hand sweeps
across in two hours.  If we added, to our clock, a hand which swept one degree
per hour of time, it would complete a whole turn in 360 hours, which is 360 /
24 = 15 days; just one day more than a fortnight and almost half an average
month.  One year would then be just slightly over twenty-four and a third
turns of this hand of our clock.  If, instead, we introduced the unit of time
that's sixty hours, this anti-minute (huge ?) is two and a half days; six of
them make up the fore-going 15-day unit.\n""",
         point = turn / 32) # there are 32 points on a ship's compass rose

minute.arc = arc.minute = arc.degree / 60
second.arc = arc.second = arc.minute / 60

# Miscelaneous SI-compatible units (c.f. SI.py), notably cm,g,s ones:
gram, tonne = milli * kilogramme, kilo * kilogramme
km, cm = kilo * metre, centi * metre
cc = cm ** 3
tex = gram / km # fineness of textiles
dtex, denier = deci * tex, deci * tex / .9

Angstrom = .1 * nano * metre    # Ångstrøm or Ångström, aka Å
# (but there's a separate Unicode code-point for the unit ...).
micron, fermi = micro * metre, femto * metre
litre = milli * stere
hectare = hecto * are
barn = (10 * femto * metre) ** 2
bar = .1 * mega * Pascal
def Centigrade(number): return Kelvin * (number + 273.16)

stilb = candela / cm**2
phot = 10 * kilo * lux

St = Stokes = cm**2 / second # kinematic viscosity
poise = Pascal * second / 10 # dynamic viscosity = kinematic * density
Gs = Gauss = 1e-4 * Tesla
gamma = nano * Tesla
Mx = Maxwell = 10 * nano * Weber
erg = .1 * micro * Joule
dyn = 10 * micro * Newton

# NB: A/m = C.(m/s)/m^2, charge flux density.
Oe = Oersted = Quantity(kilo / 4 / pi, Ampere / metre,
                        """Magnetising field strength in the cgs system.

Named after Hans Christian Ørsted.

Measures H-field strength, in contrast to B-field strength (in Tesla or
Gauss).  Multiplying H by permeability of the medium gives B.
""")

# Radiation and its effects:
Bq = Becquerel = Hz             # Activity of a radionuclide (events / s)
Ci = Curie = 37 * giga * Becquerel

R = Roentgen = Quantity(258, micro * Coulomb / kilogramme,
                        """The Röentgen, a unit of ionising radiation.

This measures the intensity of of ionising radiation in terms of amount of
charge separated per unit mass of exposed material.  Its definition is: the
amount of radiation required to liberate 1 esu of charge of each polarity in 1
cubic centimeter of dry air.  Here, the esu - electro-static unit,
a.k.a. franklin (Fr) or statcoulomb (statC) - is (in some sense) 1/2997924580
Coulombs (but normally expressed in the Coulomb-cgs system, which takes 4*pi
times the permittivity of free space as a dimensionless unit, so charge has
units length*sqrt(force)).
""",
                        fullname='Röentgen')

Gy = Gray = Quantity(1, Joule / kilogramme,
                     """Absorbed dose of radiation.

This measures the amount of energy absorbed, from radiation, by a subject's
tissues, during exposure to radiation, per unit mass of tissue.  It is a raw
physical unit and takes no account of how much damage is caused per unit of
energy.  Contrast the Sievert and its derivatives.

The centi-Gray is also called a rad; but its name conflicts with the standard
abbreviation for radian, so I don't include it in this units collection.
""")

Sv = Sievert = Quantity(1, Gray,
                        """Dose equivalent of radiation.

Although dimensionally the same unit as the Gray, the Sievert is an adjusted
'dose-equivalent' unit which aims to take into account the differences in how
much damage different kinds and intensities of radiation do.  The rem is a
more commonly used unit, equal to one centi-Sievert.

Note that these are units of total dose and the effects of irradiation are not
simply linear; a given dose in a short period of time may do significantly
more harm than the same dose spread over a longer time.  The Sievert is a
'big' unit, in the sense that someone exposed to several Sievert within a few
days can be expected to die within a few months; exposure levels between 2.5
and 4 Sv in a few days kill half of those exposed to them; exposure to half a
Sievert within a few days typically produces radiation sickness, albeit
usually healable if treated promptly and properly.  Yet there are places on
Earth where the natural background exposes inhabitants to a quarter Sievert
(somewhat more than the planet-wide average lifetime dose) per year with no
sign of ill effects.

In contrast to the 'big' Sievert, the rem is a 'small' unit: there is no
direct evidence of doses less than one rem causing health effects in humans.
""")
rem = centi * Sv
radiation = Object(
    background = Quantity.flat(1, 13, 2.4, milli * Sv / year,
                               """Background radiation level.

The given range, based on a table in UNSCEAR's FAQ, ignores some outliers; the
highest of which is around Talesh-Mahalleh in Iran, whose hot springs - used
as a health spa - exhibit levels up to two hundred times as high as the global
average used as best estimate here.  As seen in the error-bar used here, there
is substantial variation across the globe; for individual countries, narrower
ranges with significantly different averages apply, e.g. the USA's average is
generally reckoned to be 3.6 mSv/yr.

Food contributes around 0.4 mSv/yr to the total.  Flight crews on air-craft
typically experience an extra 2.2 mSv/yr from increased exposure to cosmic
rays, effectively doubling their exposure; passengers take a proportionate
increase dependent on how often they fly.  Those who live in airtight homes in
regions of high Radon-prudiction (notably granite hills) can be subject to as
much as 10 mSv/yr extra from Radon; and can endu up suffocating, regardless of
the radioactivity - ventilate your cellar !

In contrast, nuclear bomb testing contributed 0.15 mSv/yr at its peak, in 1963
(when I was born); this contribution has since decreased
thirty-fold.  Releases from coal-fired power stations have been found to
account for significantly more than those from nuclear energy and weapons
combined; and the harm caused by these releases is dwarfed by that from the
accompanying greenhouse emissions and acid rain.

Sources:
 * FAQ at http://www.unscear.org/
 * http://en.wikipedia.org/wiki/Background_radiation
 * http://en.wikipedia.org/wiki/Ionizing_radiation#Sources
"""),
    EPA = Object(
        airborne = Quantity(10 * milli, rem / year,
                            """Exposure limit from airborne emissions.

Applies to 'operations of nuclear fuel cycle facilities', including power
plant, uranium mines and mills.
"""),
        emergency = Quantity(25, rem,
                             """Guideline for non-livesaver volunteers.

During an emergency, for work not directed towards saving lives, volunteers
should not be exposed to more than a quarter Sievert.
"""),
        lifesaver = Quantity(75, rem,
                             """Guideline for life-saving volunteers.

During an emergency, those volunteering to do work that shall save lives
should not normally be exposed to more than three quarters of a Sievert.
"""),
        __doc__="""Various EPA guidelines and limits on radiation exposure.

The USA's Environmental Protection Agency sets limits on how much radioactive
material may be released into the environment.  See also radiation.DOE.
"""),
    DOE = Object(
        public = Quantity(milli, Sievert / year,
                          """DOE limit on public exposure.

Members of the public, who are not radiation workers, should not be exposed -
by the combined effect of all DOE facilities - to more than one mSv each year
(less than half the normal background level).
"""),
        worker = Quantity(5, rem / year,
                          """DOE limit on radiation worker exposure.

The US NRC and DOE limit occupational exposure of radiation workers to a
twentieth of a Sievert per year, fifty times the permitted exposure of the
general public (see radiation.DOE.public).
"""),
        __doc__ = """Various DOE limits on radiation exposure.

The US department of energy and nuclear regulatory commission set limits on
radiation exposure from the DOE's operations.  See also radiation.EPA.
"""),
    EU = Object(
        aircrew = Quantity(20 * milli, Sievert / year,
                           "EU limit for annual exposure of airline crew."),
        __doc__="EU limits on radiation exposure."),
    danger = Quantity.flat(25, 75, 50, rem,
                           """Threshold beyond which acute doses are harmful.

It is generally reckoned that subjects exposed to more than about half a
Sievert in a short space of time can be expected to suffer some degree of
radiation sickness, albeit proper medical attention should suffice to ensure
full recovery unless the dose is significantly above this threshold.  Those
exposed to several Sievert, in a short space of time, can be expected to die
within a few months.
"""),
    __doc__="""A collection of doses and dose-rates for ionizing radiation.

Note that dose and dose-rate limits tend to be set conservatively; the fact
that authorities impose such limits is not tied to any concrete evidence that
these limits are at all close to levels at which subjects would experience
harm.  The limits were initially set cautiously partly because even such
cautious limits do appear achievable, partly because the ethics of conducting
experiments (either in the laboratory or by allowing higher dose rates and
thus effectively using the general public as test subjects) preclude discovery
of actual thresholds at which harm becomes significant.  The accident at
Chernobyl did produce significant experimental data on the subject (thanks to
dosimeters on the fire-fighters who risked - and in many cases lost - their
lives to control the damage), which could be used to revise limits: but
political factors make it unlikely that this will happen.

See also the relevant units: Sievert, Gray and rem for doses by energy
delivered; Röentgen and Oersted for doses by charge; Becquerel and Curie for
hit-frequency.  Contrast study.chemy.physics's Cosmos.temperature (of the
(non-ionizing) cosmic microwave background).
""")

# see also study.chemy.particle for the electron-Volt, eV, and Rydberg's
# constant.

# Non-SI but (relatively) scientific:
atm = Atmosphere = Quantity(101325, Pascal,
                            """Standard Atmospheric Pressure""",
                            'atm', 'Atmoshpere',
                            technical = kg.weight / cm**2)

# Standard temperature and pressure:
mol.STP = Quantity(Centigrade(0) / atm, {},
                   """Standard temperature / pressure

Various thermodynamic quantities are defined in terms a standard temperature
of zero Celsius and pressure of one (standard) atmosphere; these are supplied
as .temperature and .pressure attributes of mol.STP.  In many cases, notably
whenever the ideal gas law is involved, it is actually only the ratio of these
two that matters, so mol.STP is in fact the .temperature / .pressure ratio.
""",
                   temperature=Centigrade(0),
                   pressure=atm)

torr = mmHg = Quantity(133.322, Pascal,
                       """Torr, the common laboratory unit of pressure.

Named after Evangelista Torricelli (1608-1647), who succeeded Galileo as court
mathematician to Grand Duke Ferdinando II of Tuscany.

The unit is defined as the pressure exerted by a depth of one millimetre of
mercury at zero Celsius under standard gravity.  Actual laboratory
observations should be corrected for the local ambient gravitational field
strength and mercury's thermal expansion.
""")

mach = Quantity(331.46, metre / second,
                doc = """The speed of sound in dry air.

(at standard temperature and pressure; see mol.STP).
""")

Rankine = Kelvin / 1.8
def Fahrenheit(number): return Centigrade((number - 32) / 1.8)
def Reaumur(number): return Centigrade(number * .8)

limit = Object(__doc__ = "Various limiting values, usually for humans",
               # vision ?
               hearing = Quantity(pico, Watt / m / m,
                                  """Threshold of human hearing.

This is the sound intensity conventionally used as base-value when describing
sound intensities in decibels: divide a sound intensity by this and take its
log to base ten and multiply by ten to get the dB (SIL) value for it.  A sound
intensity of one Watt per square metre is thus 12 Bel or 120 dB.\n"""))
