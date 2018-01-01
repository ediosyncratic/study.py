# -*- coding: iso-8859-1 -*-
"""Archaic and idiosyncratic units of measurement.

See SI.py for base units and units.py for various others.

This file documents lots of obscure and/or silly units, many of them derived
from the /usr/share/misc/units repository of knowledge on the subject (see
gnu.org's units.html, in URLs attribute of this module, for details).  This
file aims to be all-inclusive, rather than sensible; however, there are
`issues', since some of the units (especially ones relevant to trade in the
anglophone world) have several variants - where possible, I have tried to find
namespace-based ways to manage this mess (e.g.: print bushel.US.__doc__), but
sometimes (e.g. the chain) I just gave up and left a comment here indicating
the part of the story that I've left out !

I should probably replace most of this file with a family of XML or RDF
documents describing all the units, coupled to a deployment of some standard
parsing tool-kit to access the data they provide.

There are enough units of measurement here to provide for some cutely specific
units of measurements in all sorts of odd domains.

  For example, consider the fuel efficiency of transport systems fueled by
  petrol; this is commonly measured in miles / gallon in the anglophone world
  (with consequent confusion between the new imperialists and the old; while
  the two agree on what a mile is, they disagree on what a gallon is; five UK
  gallons roughly equal six US gallons) or kilometres / litre in the civilised
  world (48 miles / UK gallon is roughly equal to 40 miles / US gallon and to
  17 kilometres / litre).  This last is, itself, one million times the natural
  SI unit, metres per cubic metre.

  Superficially, this is a 1/area unit, though the implicit `of petrol' clause
  in it does subvert that a little; 17 km/litre is officially 17 million /
  square metre, a.k.a. 17 per square millimetre; which is meaningless drivel
  until I point out that it means that each square millimetre of
  cross-sectional area of your fuel tank contributes (at this fuel efficiency)
  17 to the ratio between the speed of your vehicle and the rate at which the
  fuel level in the tank is dropping; if your fuel tank's cross-section is one
  square foot (92,903 square mm), and your vehicle is managing 17 km/litre,
  then you're travelling a little over one and a half million (i.e. 17 *
  92,903) times as fast as the level in the fuel tank is dropping.

  In reality, fuel efficiencies of real road vehicles seem to fall in the
  range from around 8 to around 80 mpg (UK), so that a unit of order 20 mpg
  would be quite handy; as it happens, one furlong / UK floz is exactly 20
  miles per UK gallon, suggesting it as the ideal imperial unit for the task
  (i.e. it's the right size and marvelously perverse).  A slightly saner unit
  would clearly be the mile per UK pint - the given range runs from one to 10
  of these, and that's a nice range of numbers to work with - which is 2.83 km
  / litre.  By a weird twist of fate, 2.83 is almost exactly the square root
  of 8; so that the civilised unit, one km/litre, is 2.83 miles per UK gallon;
  and using this unit makes the range of real-world values run from 2.8 to 28,
  with 10 km/litre (roughly 28 miles per UK gallon or 23 per US gallon)
  presenting itself as a fairly good cut-off between `inefficient' and `not so
  bad, all things considered'.

  Note that one might equally measure the same phenomenon as a fuel
  consumption rate, in gallons per mile or litres per kilometre, which would
  encourage trying to find a unit of order one UK gallon per 80 miles,
  a.k.a. one UK pint per ten miles or 2 UK floz per mile.  (The pint per mile
  would also make quite a good unit of measurement for pub-crawls, albeit with
  very different semantics.)  A vehicle consuming a small number of floz per
  mile (up to five or six, to match the cut-off above) would then be
  considered frugal, while ones beyond that would be considered wasteful.

Chosing the right unit, and the right way up (c.f. the contrast between fuel
efficiency and consumption rate), is important to how measurements get
interpreted - for example, although the gradient of a sloping road may
formally be a dimensionless quantity, it makes sense to measure `slope' in
such units as metre (of ascent or descent) per kilometre (of travel) or, in a
culture which has different units for vertical and horizontal lengths, fathoms
per furlong. Even when using the official SI unit, different ways of
expressing a unit can change perceptions of its meaning - for example, (metre
/ second)**2 means the same as Joule / kilogramme, but expresses a different
perspective on it.

See study.LICENSE for copyright and license information.
"""

URLs = """Interesting URLs:

http://www.ukmetrication.com/history2.htm
http://www.sylvaefa.com/svf1.htm
http://www.maritimt.net/arkforsk/svenskem.htm
http://en.wikipedia.org/wiki/Cgs
http://home.clara.net/brianp/
http://www.gnu.org/software/units/units.html
http://www.unc.edu/~rowlett/units/
http://www.bipm.org/en/si/si_brochure/chapter4/4-1.html
http://www.easyunitconverter.com/
http://www.chaos.org.uk/~eddy/physics/units.html

Wikipedia:
http://en.wikipedia.org/wiki/Category:Obsolete_units_of_measure
http://en.wikipedia.org/wiki/Category:Units_of_measure
https://en.wikipedia.org/wiki/Ancient_Hebrew_units_of_measurement
https://en.wikipedia.org/wiki/Ancient_Greek_units_of_measurement
https://en.wikipedia.org/wiki/Ancient_Roman_units_of_measurement
https://en.wikipedia.org/wiki/Template:Hand_measurements

Nautical mile and its kin
http://www.gwydir.demon.co.uk/jo/units/sea.htm
http://www.explore-dictionary.com/weights_and_measures/M/Megalithic_yard.html

German archaic units of measurement:
http://de.wikipedia.org/wiki/Alte_Maße_und_Gewichte
should move the relevant section of units.py to
archaea.py or even archaea/*.py !

Dutch archaic units:
http://home.hetnet.nl/~vanadovv/Lengte.html

Fathom = vadem in dutch:
<URL: http://home.hetnet.nl/~vanadovv/Lengte.html >
http://gwydir.demon.co.uk/jo/units/oldunits/sea.htm

Sources begging to be plundered:
 * http://en.wikipedia.org/wiki/Metrication#Chronology_and_status_of_conversion_by_country
   links to old systems from several nations ;-)
 * More fun energy units: http://en.wikipedia.org/wiki/Tonne_of_oil_equivalent
   (also, diverse other quantities are implied).
"""

_notes_ = """\
Many units are or have been subject to what amounts to dialect variation, some
have undergone significant changes of definition over time (as compared to the
fine-tuning of, for example, the metre in 1975).  This is only to be expected:
language works like that and it's words we're discussing here.

Comments in the following indicate the views of various sources. (K&L = Kaye
and Laby, NHD = New Hackers' Dictionary, KDWB = Kim's dad's 1936 white
booklet, EB = 11th edition Encyclopaedia Britannica, 1911, article on Weights
and Measures.)  The source 'poxy' is my 1973 reprint of the fifth edition of
the Pocket Oxford Dictionary.  Where source is not given in a comment (either
on the line or at start of section) the calculation of the line reflects a
consensus.  Either that or it's some random detail I tripped over somewhere
and jotted down but forgot to record the source ...

Units I describe as `anglophone' are, to the best of my knowledge, common to
all the `English-speaking' peoples (they also typically have equivalents in
archaic units of Scandinavia and sometimes elsewhere): by `imperial' I mean
those in which the US and UK have diverged.  The US versions of these are
collected together in the namespace of an object US; these units also appear
as .US attributes of the (un-namespaced) UK variants.  Various other nations'
arcane units are likewise collected in name-space objects (whose names are
English words for the national adjectives); where cognate with an anglophone
unit, they also appear in that unit's name-space (using the national adjective
in its native tongue, except for French - an attribute name can't have a
cedilla in it).  For these purposes, Troy is treated as a nation - but doesn't
actually mean the ancient city-state of that name !  I should probably do the
same to the UK versions of anglophone units - if only to make this module's
name-space less cluttered !  For similar reasons, I should break up this
module into a sub-package comprising several modules (and probably write some
machinery classes to facilitate cross-linking unit.nation and nation.unit,
among other complications).
"""

from units import *

# some non-SI base units ...
Lenat = Quantity.base_unit('L', 'Lenat',
                           """The standard unit of bogosity

See the New Hackers' Dictionary, under microLenat.  Also known as the Reid.
""")

Helen = Quantity.base_unit('Helen', 'Helen',
                           """The standard unit of beauty (trad).

Definitively `beautiful enough to launch a thousand ships', so that launching a
single ship gains credit for a single milli-Helen.  The origin of this is the
story of the Trojan war, in which a Greek fleet of a thousand ships, carrying a
great army, went to retrieve Helen from Troy, to which Paris had taken her.
""")

Scoville = Quantity.base_unit('Scoville', 'Scoville',
                              """Standard unit of pungency.

Dilution to one part in N (with sugar water) makes the taste undetectable for
an N Scoville pungency.  Conventional wisdom classes 0 to 500 as mild, 500 to
999 as medium, 1k to 4.999 k as hot and 5k or above super-hot.  Chile sauces
scoring 93 k Scoville are insane, but some exist as high as 577 k Scoville;
Jalapeno extract scores about 4.5 k; pure Capsaicin rates over 15,000,000
Scoville Units.
""")

# Dimensionless:
dozen = Quantity(12, {}, baker = 13)
pair, half, quarter = 2, .5, .25
# few, some, several (.best=7), many ? Mainly of value for different-shaped distributions.
percent = .01
prial = nest = 3
dickers = 10 # *must* be a `corruption' of dix, arranging to *not* sound like `dicks' !
score, shock, gross = 20, 60, 144 # c.f. Danish.{snes, skok, gros}
timer = flock = 40
greatgross = gross * dozen
myriad = 1e4
paper = Object(
    quire = Quantity(25, {}, short = 24), # baker's two-dozen ?
    ream = Quantity(500, {}, short = 480, perfect = 516)) # 43 * dozen
paper.also(bundle = 2 * paper.ream, bale = 10 * paper.ream,
           short = Object(quire = paper.quire.short, ream = paper.ream.short))

Geldof = Quantity(3e4, 1/day,
                  """The pre-Geldof poverty-induced infant mortality rate.

As at 2005, the Live8Live organizers report that 30,000 children die every
day, needlessly, as a result of extreme poverty.  This seemed a reasonable
basis on which to name a rate-of-death unit.  Since Sir Bob Geldof stands as
front-man for the Live8Live organization, and has done sterling work in
previous kindred organizations, he seemed a natural person after whom to name
the unit.  It should be noted that the 3e4/day figure is merely the infant
mortality rate; we could as readily use 5e4 if we include adults.
""")

# The degree and some additions to arc
arc.also(grad = turn / 400, # a unit of elevation used by gunners, IIRC.
         mil = Quantity(1e-3, radian,
                        """The milliradian (a gunnery unit).

http://gwydir.demon.co.uk/jo/units/oldunits/sea.htm#angles
""",
                        NATO=turn/6400,
                        Warsaw=turn/6000))

degree = Object(arc.degree, arc = arc.degree,
                Centigrade = Kelvin, Celsius = Kelvin, C = Kelvin,
                Fahrenheit = Rankine, F = Rankine,
                Reaumur = .8 * Kelvin,
                __doc__ = """The degree.

Various quantities are measured in 'degrees': the name comes from the Latin
for a step (de gradus, I think), which makes it sort-of synonymous with
'unit'. See arc.__doc__ for the unit of angle with this name.

Several units of temperature share this name, qualified by the originators of
the respective units.  A Swede called Celsius invented a unit which France
(and hence SI) adopted; a Frenchman called Réaumur invented one which the
Germans adopted (until they switched over to SI); and a German called
Fahrenheit invented (before these others, the thermometer and) various units,
one of which remains in use in some backwards parts of the anglophone world.
""")

degree.__dict__['Réaumur'] = degree.Reaumur

champagne = Object(
    split = .2 * litre,
    magnum = 1.5 * litre)
champagne.also(
    jeroboam = 2 * champagne.magnum,
    rehoboam = 3 * champagne.magnum,
    methuselah = 4 * champagne.magnum,
    salmanazar = 6 * champagne.magnum,
    balthazar = 8 * champagne.magnum,
    nebuchadnezzar = 10 * champagne.magnum)

# Anglophone units of length:
inch = 2.54e-2 * metre # from Latin, uncia, via OE ynce
caliber = inch / 100
barleycorn = inch / 3
pica = barleycorn / 2 # CSS 2.1 spec, the DTP point according to Wikipedia
# printer's pica variants:
# http://fontforge.github.io/en-US/documentation/reference/glossary/
pica.US = 7 * cm / 16.6
pica.didot = milli * metre / 2.66
line = pica / 2 # inch / 12, c.f. French.line, Norse.linje, Swedish.linje
palm = Quantity(7, inch / 2,
                """Palm: width of a hand's palm without thumb.

Known as palm or handsbreadth and generally reckoned at four fingers, making it
three inches or three and a half inches, depending on whether you use
finger.breadth (at knuckle, used here) or digit (just short of finger tip).  In
ancient Egypt, palm = 4 digits.
""",
                length = Quantity(8, inch))
hand = Quantity(4, inch,
                """Hand: the width of a hand, including thumb lying alongside it.

In ancient Egypt, hand = 5 digits.
""",
                breadth = palm)
shaftment = Quantity(6, inch,
                     """Shaftment: width of hand plus extended thumb.

Used for measuring poles: grip the pole with one hand,thumb at pole's end; pass
to the other hand, thumb just touching the edge of the hand before; repeat,
counting, until end of pole; you've counted how many shaftments long the pole
is.  The length was generally reckoned to be about twice the width of a palm;
later usage presumed it be half a foot, as given here.
""")
digit = Quantity(3, inch / 4,
                 """Digit: width of a finger, just short of its tip.

In ancient Egypt, a digit was a quarter palm, hence the value given here.  See
also fingerbreadth, which is sometimes used as a synonym, although I've had some
source give me a slightly larger value for that.  Contrast the astronomical
unit, digit.astronomy (an angle); and finger.breadth, for the width at a
knuckle.  I find that my index finger's width, just short of its tip, is fairly
accurately a sixth of the same finger's length; which is close to half my span;
reckoning back from the last as nominally 9 inches, that makes the index finger
definitive for the digit as a twelfth of a span.
""",
                 astronomy=Quantity(8, arc.minute / 3,
                                    """On twelfth the diameter of Sun or Moon.

Astronomers (until at least the eighteenth century) used a twelfth of the
diameter of The Sun (roughly the same as, but more constant than, that of The
Moon) as a unit of angle, known as a digit.  I find that The Sun's diameter
subtends 32 arc minutes, as seen from Earth, giving 8' / 3 as a twelfth of
that.  (By my calculations, The Moon's diameter subtends a slightly smaller
angle even at the point on Earth closest to The Moon; the difference is two
fifths of an arc minute (24 arc seconds) there but larger elsewhere on
Earth.  However, this is when the moon is at its mean distance from Earth; its
orbit is eccentric enough to bring it close enough to fully cover The Sun during
eclipses.)
"""))
span = Quantity(9, inch,
                """Separation between outstretched tips of thumb and little finger.

This is as far apart as one hand can touch two things.
""")
finger = Object(doc="""Various units of length, implying a unit of drink.

The lengths and widths of fingers are variable - from person to person, from
finger to finger within a hand and, for width, along the length of each finger,
particularly between at knuckles and between them.  See digit for breadth just
short of the tip, finger.breadth for the knuckle (and more discussion); and
length for the length.

The common unit of (usually) distilled liquor in drinking games is a finger's
breadth as a depth within a glass; the volume this implies, naturally, depends
on the cross-sectional area of the glass at the relevant depth, so is wildly
variable - entirely in keeping with the spirit of such games ;^>
""",
                length = Quantity(6, digit,
                                  """The length of a finger or width of a fist.

As finger length, this used to be used as a unit of measurement of cloth.

Fingers have diverse lengths, even on a single hand: when I bend one hand with
straight fingers perpendicular to palm and measure the lengths of the backs of
that hand's finger with the widths of the other's matching finger (see digit;
contrast finger.breadth), just short of its tip, I do indeed find most are
pretty close to six times their own length (my ring finger is a little more than
six times its width; my thumb closer to three and a half times its width).  See
note on digit for a reason to treat the index finger's length as definitive; for
me, at least, it's a half span.

Ancient Egypt had a fist as unit of length, equal to six digits, one and a half
palms.  I find my clenched fist's width, from unenclosed thumb-knuckle across
the back of my hand, does roughly match the lengths of the opposite hand's
fingers.
"""),
                breadth = Quantity(7, inch / 8,
                                   """The breadth of a finger at a knuckle.

When fingers are held together curved, the knuckles line up, making for
collective widths measured in terms of finger.breadth, the knuckle width; I find
that four together match the opposite hand's palm; hence my choice to take a
palm as four of this rather than four digits.  When fingers are held together,
straight, the knuckles of each interleave with the gaps between those of the
next, making the collective width less than the relevant multiple of
finger.breadth but more than that of digit.

Contrast digit and the Swedish fingerbredd = fot / 15 = tv&auml;rhand / 5 (from
Norwegian, I read tverrhand as hand-width, like hand or palm here).  The
Akkadian finger (bredth) was a thirtieth of a cubit (so a twentieth of a foot,
three fifths of an inch).
"""))
fist = finger.length
ell = Quantity(5, span, flemish = Quantity(27, inch, "The Flemish ell"))
cubit = Quantity(2, span,
                 """Cubit: length of bent arm from elbow to finger-tip.

The ancient egyptian cubit was seven palms (21 inches or more), rather than the
two spans (18 inches) given here.  The name cubit is also used as a synonym for
ell.

Ancient Akkadia's cubit was a foot and a half; ancient Egypt's was seven palms.
""")
ft = foot = Quantity(3, hand,
                     """The English foot.

See namespace for other nation's variants on this unit.  All are fairly close to
the distance (given here as foot.SI) that light travels in a nano second: those
closest to it are the old Swedish foot, just under 1% below, and the English
foot, just over 1/60 above.  Ancient Akkadia's foot was two thirds of a cubit,
thus twenty fingers.
""",
                     SI=Quantity(nano, second.light,
                                 """The SI foot, or light nanosecond.

In one nanosecond, light travels a distance firmly within the range of lengths
known, among various nations' diverse archaic units, as a foot.  The Swedish
foot (or 'fot' in Swedish) is less than 1% shorter; most others are slightly
longer.  Given that the metre is now defined in terms of the light second, it
violates the spirit of SI to retain it; it should be replaced by the light
second and units derived therefrom.  The light nanosecond thus presents itself
naturally as a replacement unit, which would naturally be called the 'SI
foot'.
"""),
                     survey=Quantity(1.2e3 / 3937, metre,
                                     """The (geodetic) survey foot.

In the US the Metric Act of 1866 defined the foot to equal exactly 1200/3937m,
or approximately 30.48006096 cm.  This unit, still used for geodetic surveying
in the United States, is now called the survey foot.
"""))

yard = Quantity(3, foot,
                """The Modern Yard.

The yard (0.9144 metres) is a modern survivor of a family of roughly
stride-sized units of length dating back - if the excellent Monsieur Thom is
to be believed - to prehistory.  (Thom measured the lengths in stone circles
all over Europe and found correlations that lead him to conclude that they
were measured to a common unit, approximately equal to the yard.)  It takes
its name from a Germanic word, 'gyrd'.  Its metric replacement, the metre, is
just slightly bigger than the anglophone yard, or indeed than three times the
French pied du roi (the longest foot I know of).
""")

nail = yard / 16
pace = Quantity(5, foot, US = 30 * inch)
rope = 4 * pace
fathom = Quantity(2, yard, """The Fathom.

The fathom (also called 'mark') is cognate with the Swedish famn and Danish
favn (q.v.) and has principally survived in use as a maritime measure of
vertical distances - notably the depths of bodies of water.
""")
shackle = 15 * fathom

chain = 22 * yard 	# but engineers (and Ramsden) use a 100ft chain !
chain.engineer = 100 * foot # rather than 100 links; c.f. Swedish.ref
rod = pole = perch = chain / 4 # rod from German
link = pole / 25
furlong = 10 * chain	# from German, `furrow long'
mile = Quantity(8, furlong,
                """The Statute Mile.

The British mile (also used in the U.S.A.) is just over 1600 metres; for a
long time it was used as a standard distance for races, fitting nicely with
the pattern of doubling lengths upwards from 100m.  Like all Imperial units,
it has a long and contorted history.

Its nominal origin is in the Imperial Roman 'mille passus' - a thousand paces,
though the Roman passus was the distance a soldier's foot travels in each
stride, roughly double the separation of the feet when both are on the
ground.  Compare this to the British 'pace', of 5 feet, which is quite close
to one thousandth of a mile, while the US 'pace' is exactly half as long.

Some backward countries seem likely to continue using this unit to measure
distances - along with the mile per hour as a unit of speed - for some time to
come.  Contrast mile.nautical, the Scandinavian mil and, generally, the other
things you'll find listed by dir(mile).

Scandinavia, North Germany and various parts of Eastern Europe used units with
cognate names but based on the rather longer Roman 'millum spatium' - a
thousand 'intervals'.  Such units are typically subdivided into 24000 (Danish,
North German, Austrian) or 36000 (Swedish, Croatian) 'feet' that do, at least
roughly, match other so-named units.

See also: http://en.wikipedia.org/wiki/Mile
""")

league = Quantity(3, mile,
                  """The league

A varying measure of road distance, usu. about three miles (poxy).
""")
marathon = Quantity(1, 26 * mile + 385 * yard,
                    """The length of a marathon race.

At the first Olympic games, hosted by Athens in 1896, a race was run from the
town of Marathon to Athens, following a 40 km route, commemorating a confused
legend about a messenger bearing news of the end of the battle of Marathon in
490 BC.  (Prior to the battle, Pheidippides allegedly ran, in two days, from
Athens to Sparta, a distance of over 140 miles, to request aid.  After the
battle, the Athenian army marched home in a hurry, lest the Persians sail up
the coast to attack Athens directly.  These two stories ended up getting
conflated.)

Since then, races over about 40 km have tended to be called marathons.  Until
1921, there was no standardised distance for the race; thereafter, a standard
distance of 42.195 km was specified for Olympic marathons and has been widely
adopted elsewhere.  The given distance is based on the 26 miles and 385 yards
of the 1908 Olympic marathon, which had happened to have a particularly
memorable ending; this course's length, in turn, had been arrived at by a
(surprisingly well-documented) comedy of happenstances in the preparations for
the race.
""")

point = Quantity(.25 / 3, pica, "The printer's point (a length).",
                  # http://fontforge.github.io/en-US/faq/
                  # claims "pica point" = inch / 72.27 (used in Anglophone fonts)
                  # and "didot point" = inch / 67.54 (used in European fonts)
                  # also "metric didot point" = .4 mm
                  silversmith=Quantity(.25e-3, inch,
                                        """The silversmith's point (a length).

Contrast: point.jeweller - a mass.
"""),
                  US = pica.US / 12,
                  arc = arc.point)
shoe = Object(
    __doc__= "Units of thickness of leather in shoes.",
    iron = inch / 48, # soles
    ounce = inch / 64)# elsewhere
railgauge = 4 * foot + 8.5 * inch # 1435 mm according to Anders at Tandberg
mile.also(sea = Quantity(2000, yard,
                         """A 'sea mile'.

Apparently an informal approximation to the nautical mile; see comments on
mile.nautical.  The various Scandinavian nations also had kindred units, see
note on mile.Scandinavian, although I presently only have data for the Swedish
sjoemil.
"""),
          # geographical = 7420 * metre, # whence got I this ? seems to match Scandinavian
          geographical = Quantity.flat(1855.3248, 1855.4, 1855.3257, metre,
                                  """The geographical mile

This is one minute of arc along the Earth's equator.  Compare mile.nautical,
which was once defined as a minute of arc along a great circle, without
specifying which.  Even with the equator specified, the value depends on
choice of geoid (formal world-wide model of nominal sea level); the estimated
.best used here is taken from the IAU-2000 standard, while the range is taken
from WGS-84 (.low) and the 1924 International Spheroid (.high).

Scandinavia and Germany used equivalent names for four minutes of arc,
sometimes named as a 'sea mile' (suitably translated),
"""),
          # some uses of nautical mile, below, should perhaps use geographical
          nautical = Quantity(1852, metre,
                              # K&L, given as the definition of this unit.
                              """The nautical mile

I've met assertions that the nautical mile is 2000 yards (here given as
mile.sea since I've also seen it called a sea mile).  Alternatively, that it's
one minute of arc - see mile.geographical.  My available figures for the
Earth's radius yield figures ranging from 1853 to 1855 metres, aka 2026 to
2029 yards: so Kaye & Laby fits with the minute of arc view (to reasonable
accuracy) and I take the 2000 yard figure as being a widely used
approximation.  See also notes on mile.nautical.Admiralty.  Apparently, the US
used some other unit until 1954, the UK until 1970; both catching up with a
1929 international standard, which set it to 1852 metres, the value used here.
""",
                 Admiralty = Quantity(6080, foot, """Admiralty Nautical mile

An archaic UK variant on the nautical mile, replaced in 1970 with the
international standard.

<quote src='http://gwydir.demon.co.uk/jo/units/oldunits/sea.htm'>
Nautical miles measure distance. 1 nautical mile is the angular distance of 1
minute of arc on the earth's surface. As these differ slightly (6108' at pole
and 6046' at equator) 6080 was adopted (this being it's approximate value in
the English Channel). ... Now the International nautical mile is used
throughout the UK, except in the Statutory Instrument 1995 No. 1804 which
defines it as 1853 metres!
</quote>

Some authors (notably including Joseph Conrad) use knot (q.v.) as a synonym
for the nautical mile (as well as the speed that covers one per hour, for
which I used the name knot in this module).
""")),
          Roman = Quantity.within(1480, 1, metre,
                                  """The Roman mile

The Roman mile, mille passus (1000 paces, or 5000 Roman feet), was used (with
local adaptations) throughout the Roman empire.  Its precise length is not
known with certainty: [0] gives 1479 metres as modern estimate but [1] gives
1481 metres, possibly (but not clearly) as an English traditional estimate of
1620 yards (1481.328 metres).

[0] http://en.wikipedia.org/wiki/Mile#Roman_mile
[1] http://en.wiktionary.org/wiki/Roman_mile
"""),
          Scots = Quantity(1807, metre, """The Scots mile.

Scots ratios:
mile = 8 furlongs
furlong = 40 fall
fall = 6 ell
ell = 3 ft + 1 inch = 37 inch
"""),
          Arabic = Quantity.within(1925, 5, metre),
          Hungarian = Quantity.flat(8937.4, 8379, 8353.6, metre),
          Prussian = 7532 * metre, # KDWB
          # Wikipedia gives North German: 7532.5
          Portugese = 2087.3 * metre,
          Croatian = 11130 * metre, # hrvatska milja
          Austrian = 7586 * metre, # Croatian banska milja
          Russian = 7468 * metre)
cable = Quantity(0.1, mile.nautical,
                 US = Quantity(100, fathom,
                               navy = 720 * foot.survey))
league.marine = 3 * mile.nautical
knot = mile.nautical / hour

Scots = Object(__doc__="""Scots units of measure.

Used until 1824, when an Act of Parliament forbade use of these units in
favour of the Imperial standard units, which had technically been introduced
earlier by the 
""",
    mile = mile.Scots)

Roman = Object(mile = mile.Roman,
               passus = Quantity(1e-3, mile.Roman, """The Roman passus (pace).

This is the distance a legionnaire's foot moved in each stride, half the
forward distance between his feet at a moment when both were on the ground
(but a little less than half the separation of the feet at this moment, which
also includes the sideways separation of his feet).
"""),
               foot = Quantity(2e-4, mile.Roman, "The Roman foot"))
foot.Roman = Roman.foot

foot.French = 4500 * metre / 13853	# pied de roi, French foot
inch.French = foot.French / 12
point.French = inch.French / 144        # hmm ... not inch / 72 ?
# https://en.wikipedia.org/wiki/Pied_du_Roi
French = Object(pied = foot.French,
                # what're the right French names for inch, line, point ?
                pouce = inch.French,
                ligne = inch.French / 12,
                point = point.French,
                toise = 6 * foot.French,
                perche = 22 * foot.French, # Paris
                arpent = 220 * foot.French, # or 180 in Quebec
                amphora = foot.French ** 3,
                acre = Quantity(1, (180 * foot.French)**2,
                                """The arpent carr&eacute;

The 180 foot side to a square of this area was called arpent in Quebec, at odds
with 220 foot used as arpent in France.  This area also shows up in the US as a
unit called 'arpent', just to confuse matters.
"""))
French.also(lieue = Object(
        ancienne = myriad * foot.French,
        Paris = 2000 * French.toise,
        postes = 2200 * French.toise,
        tarifaire = 2400 * French.toise),
            pinte = French.amphora / 36)
French.also(quade = 2 * French.pinte,
            chopine = French.pinte / 2)
French.also(velte = 4 * French.quade,
            demiard = French.chopine / 2)
French.also(quartot = 9 * French.velte,
            posson = French.demiard / 2)
French.also(feuillette = 2 * French.quartot,
            roquille = French.posson / 4)
French.muid = 2 * French.feuillette
league.French = French.lieue
fathom.French = French.toise

# Archaic units of mass:
grain = Quantity(64.79891e-6, kilogramme, # K&L
                 """The mass of one barleycorn.

The value given here is modern - as used in anglic jurisdictions, that still
entertain various archaic units, to relate these to saner ones.

The name grain also shows up as a unit for other quantities: the ancient
Akkadians used it for a length, cubit / 80, for example.
""",
                 French=Quantity.fromDecimal(53.11, 2, unit=gram * milli))
French.also(denier=24 * grain.French,
            prime=grain.French / 24)
mite = grain / 20
droit = mite / 24 # a cube of water half a mm on each side
periot = droit / 20 # an eighth of the Planck mass; a.k.a. perrot
blanc = periot / 24 # did I mention that some of these units are silly ?
scruple = 20 * grain
lb = pound = 350 * scruple # from latin: libra and pondus
oz = ounce = pound / 16
dram = ounce / 16
clove = 7 * pound
stone = 2 * clove
cental = 100 * pound # cental is a UK name for the US cwt
quintal = 76 * pound # standard (iron) "flask" of Mercury (originally a Spanish unit)
cwt = hundredweight = Quantity(8, stone, US = cental)
ton = Quantity(20, cwt, US = 20 * cwt.US, metric = tonne)

# Cologne mark and derivatives (c/o Wikipedia):
Cologne = Object(mark = Quantity(233.856, gram,
                                 """The Cologne Mark

Introduced by King Hans of Denmark in the late 1400s as a standard of weight,
later used in the definition of various (mainly Germanic) currency standards,
notably including (in 1754) the Holy Roman Empire's conventionsthaler, one
tenth of a Cologne mark of silver, superseded in the early 1800s by the
(Prussian) Thaler, containing one fourteenth of a Cologne mark of silver.

See: http://en.wikipedia.org/wiki/Cologne_Mark
"""))
pound.Cologne = 2 * Cologne.mark
ounce.Cologne = Cologne.mark / 8
Cologne.also(pfund = pound.Cologne,
             unze = ounce.Cologne,
             lot = ounce.Cologne / 2,
             quentchen = ounce.Cologne / 8,
             pfennig = ounce.Cologne / 32,
             gran = ounce.Cologne / 36)

# Other units of mass: KDWB
stone.butcher = 8 * pound
dram.apothecary = 3 * scruple
denier.Troy = 24 * grain # = ounce.Troy / 20, the denier (a frankish coin) or pennyweight
ounce.Troy = 8 * dram.apothecary
pound.Troy = 12 * ounce.Troy
carat = Quantity(.2, gram, # metric variant, from /usr/share/misc/units
                 """The Carat, a unit of mass used by jewelers.

Apparently originally the mass of a carob seed.
""",
                 Troy=3.17 * grain) # or 3.163 grain according to u.s.m.u
Troy = Object(drachm = dram.apothecary, denier = denier.Troy,
              carat = carat.Troy, ounce = ounce.Troy, pound = pound.Troy)
point.jeweller = carat / 100
pound.mercantile = 15 * Troy.ounce

# Imperial force, power, etc.:
psi = pound.weight / inch**2
horsepower = Quantity(
    550, foot * pound.weight / second,
    metric = 75 * kilogram.weight * metre / second,
    electric = 746 * Watt,
    boiler = 9809.50 * Watt,
    water = 746.043 * Watt,
    donkey = 250 * Watt) 
chevalVapeur = horsepower.metric
celo = foot / second**2
jerk = celo / second
poundal = pound * celo
reyn = psi * second
slug = pound.weight / celo
slinch = 12 * slug
duty = foot * pound.weight
pond = gram.weight

# Miscellaneous units
jiffy = second / 60 # but also s/100 and ms ...
Atmosphere.technical = kg.weight / cm**2

TNT = Quantity(4184, Joule / gram, # (2.045 km/s)**2
               """The conventional unit of power of explosions

The energy released by each gram of TNT (trinitrotoluene) upon detonation is
between 4.1 and 4.6 kJ; however, for the purposes of standardized definition, a
value of 4184 Joule is used - this is the thermochemical kilo-Calorie (see
calorie.thermochemical).

It is worth noting that food carbohydrate has energy content per unit mass
roughly four times as high as this.  The energy equivalent of matter (via the
square of the speed of light) is about 21.5e9 times as high.  The square root of
this ratio (analogous to the speed of light in the last) is just over 2 km/s, or
two thirds of geosynchronous orbital speed.

Allegedly, when people talk about megaton or kiloton nukes, the 'ton' involved
is in fact the metric tonne, not (as I had guessed, or read somewhere now
forgotten, in the past) the US ton.

c.f.: http://en.wikipedia.org/wiki/TNT_equivalent
""")

Krakatoa = Quantity.parseDecimal('84e16', None, Joule,
                                 """The Krakatoa explosion's (approximate) energy output.

Explosions of this order (around 200 Mt, equivalent to detonating a fifth of a
giga-tonne of TNT) are apt to mess with Earth's climate, based on concrete
observations of how the actual Krakatoa explosion did just that.  The details of
an explosion doubtless make a difference - maybe by a factor of ten or three,
one way or another - in how its energy output relates to its impact: but, for
that, we only have the estimates our models have produced.  For this particular
case, we have experimental data.
""")

class Magnitude (Object):
    """Seismic magnitudes.

    There are various scales for seismic events, such as earthquakes, based on
    various measurements that can be recorded in practice: unfortunately,
    journalists tend to call them all Richter, which is just one of them.  See
    Wikipedia or a real seismology text-book for the actual details of the
    particular scales; support for them here is mainly to provide a rough idea
    of the values, with the hope of doing the right thing for those who (unlike
    me) know what they're doing.

    The attributes of this object are mappings from magnitude numbers to amounts
    of energy associated, in one way or another (depending on the scale in
    question), with seismic events of the given magnitude. The amount of energy
    increases, in each case, by a factor of a thousand for every increase by two
    in magnitude; so the magnitudes associated with a given energy differ by
    fixed offsets.

    See also the .seismic attribute of any Quantity with units of energy or
    torque; its attributes map energies back to magnitudes, inverting the
    functions here with the same names.  Thus Magnitude.moment(e.seismic.moment)
    should equal e, for any energy e, while Magnitude.moment(m).seismic.moment
    should equal m, for any number m; and likewise for .energy or .Richter in
    place of .moment throughout.\n"""

    @staticmethod
    def __raise(val, f=lambda v: 10 ** v, Q=Quantity):
        if isinstance(val, Q):
            return val.evaluate(f)
        return f(val)

    @classmethod
    def moment(cls, m): return cls.__raise(1.5 * m + 9.1) * Joule
    @classmethod
    def energy(cls, m): return cls.__raise(1.5 * m + 2.9) * Joule
    @classmethod
    def Richter(cls, m): return cls.__raise(1.5 * m + 6) * 4.2 * Joule

Magnitude = Magnitude()

# Speed of sound in dry air at 0 centigrade
sound = 331.3 * metre / second # * (temperature / 273.15 / K)**.5
# so km / 3 / sec at 3.35 centigrade; 343 m/s at 20 centigrade

# Archaic energy units based on the heat capacity of water.

# temporary name, to avoid calorie.short = calorie (rude to GC)
_short = Quantity.flat(4.182, 4.204, None, Joule,
                      """The short calorie.

The short calorie (or gram-calorie) is defined to be the amount of heat energy
needed to warm one gram of water by one degree Celsius at standard atmospheric
pressure.  Since the heat capacity of water varies with temperature, this
definition leads to various actual values for the unit, depending on the
chosen temperature at which to measure, or range of temperatures over which to
average.
""")

calorie = Object(_short,
                 __doc__="""The calorie.

This was originally a unit of chemical and thermal energy, defined by Nicolas
Clément in 1824 as the amount of energy required to warm one kg of water by
one degree Celsius.  This original 'kg-calorie' definition was superceded just
over a century later when a British Association of Science committee proposed
the 'gram calorie'.  The former is now called the long calorie or Calorie
(capitalised) while the latter is called the short calorie or calorie
(lower-case; try to avoid starting sentences with this word).  Nutritional
information is in many contexts given using the older long Calorie while
modern standards bodies specify calorie to mean diverse variations on the
short calorie; in the resulting confusion, the sanest course of action is to
avoid using these units entirely, especially as they're deprecated by SI
(since they take the heat capacity of water as a unit, without specifying the
temperature at which it's to be taken, despite its variation with
temperature).

Water is at its densest at 4 Celsius and also has its highest heat capacity
there, 4.204 J/K/kg, giving rise to the largest of the range of units known as
the calorie, the 4-degree calorie, measured for a rise from 3.5 to 4.5
Centigrade.  The heat capacity of water decreases, via (4.1855 +/- .0005)
J/K/kg between 14.5 and 15.5 Centigrade, reaching 4.182 J/K/kg between 19.5
and 20.5 Centigrade

See calorie.nutritional, .thermochemical and .international for particular
values selected, from among the available candidates, by standards bodies.
In contrast, calorie.short and .long represent the range of values seen
between 4 and 20 centigrade.

Note that using the Celsius degree (defined as one percent of the interval
between the boiling and freezing temperatures of water) and asking for the
specific heat capacity of water to be 1 (at some suitably specific temperature
and pressure) leads to 64.8 * metre/second as unit of speed; this holds true
regardless of unit of mass, as long as the same unit is used both in building
one's unit of energy and in defining heat capacity (as for the long calorie;
the short calorie uses the kg in defining the Joule but the gram in defining
heat capacity).  Using 64.8 metre (2550 inches, 0.2161 light microseconds) as
unit of length, in conjunction with the second, would suffice, yielding
c. 2720000 cubic metres as unit of volume; a body of water with volume one
millionth of this would thus have mass around 272 kg, which we could use to
complete a system of units based on the second and properties of water.

Some values used here are taken from python's
Scientific.Physics.PhysicalQuantities module.
""",
                 mean = Quantity(4.19, Joule,
                                        """The mean calorie.

This is one percent of the energy needed to warm one gram of air-free pure
water at atmospheric pressure from its freezing point to its boiling point.
See calorie's documentation for further details.
"""),
                 international = Quantity(4.1868, Joule,
                                          # 3.088 * lb.weight * foot
                                          """The international calorie.

This is the calorie adopted by the Fifth International Conference on
Properties of Steam (London, 1956), formalising a decimal truncation of an
earlier (1929) figure of 180/43.
"""),
                 thermochemical = Quantity(4.184, Joule,
                                         """The thermochemical calorie.

This is ISO's standard calorie.
"""),
                 nutritional = Quantity(4.182, Joule,
                                        """The nutritional calorie.

This is the standard calorie adopted by the International Union of Nutritional
Sciences.
"""),
                 short=_short,
                 long = Quantity(kilo, _short,
                                 """The long Calorie.

The long Calorie (it is conventional to capitalise it, to limit confusion with
the short calorie; I prefer to simply *not use* either) is defined as the
amount of heat energy needed to warm one kilogramme of water by one degree
Celsius; one Calorie is consequently one kilocalorie.  It is what's usually
meant when anything to do with nutritional energy uses the term 'Calorie'
(usually without bothering to capitalise).  It is subject to the same
variation with base temperature as the short calorie; see documentation of
calorie.short for further details, or that of calorie for background.
"""))
del _short

Calorie = calorie.long
Clausius = Quantity.unit(1, Calorie / Kelvin, # the heat capacity of a kg of water.
                         "Cl", "Clausius",
                         "Clausius's original unit of entropy.")
frigorie = calorie.short / hour # Rate of transfer of heat (in refrigeration).

# Anglophone units of energy:
CHU = calorie.short * pound / gram # don't know what CHU stands for ! (KDWB)
BTU = Quantity(1, CHU * Rankine / Kelvin,
               """British Thermal Unit

This is the energy required (at standard atmospheric pressure) to heat one
pound of water by one degree Fahrenheit.  (The heat capacity of water varies
with temperature, hence variants on the unit arise by chosing a different
reference temperature at which to measure; see documentation of calorie for
details.)  It is not an SI unit; if a prefix M is prepended to it, it's from
the latin numeral M and so means a thousand - not (mega) a million !
""")
# = Clausius * Rankine * pound / kg
therm = Quantity(.1 * mega, BTU, US = 1.054804e8 * Joule)

# Anglophone units of area (KDWB):
acre = chain * furlong # consensus; mile**2/640.  From German/Norse: field
acre.French = French.acre
rood = acre / 4
rod.also(building = 33 * yard * yard, bricklayer = rod * rod)
# US names for certain areas:
section = mile**2
township = 36 * section
homestead = 160 * acre # aka section / 4

# `cubic or solid measure' (KDWB):
foot.timber = foot**3
stack = Object(wood = 108 * foot.timber) # yard * fathom * fathom
cord = 128 * foot.timber
cord.house = cord / 3
ton.ship = 40 * foot.timber
ton.ship.timber = 42 * foot.timber
ton.also(displacement = 35 * foot.timber,
         freight = ton.ship,
         register = 100 * foot.timber)	# internal capacity of ships

# Imperial units of volume (part 1):

# `fluid measure'
gallon = Quantity(4.54609, litre, # 10 * pound / water.density
                  French = French.velte,
                  wine = 3 * 11 * 7 * inch**3, # (aka US gallon) ancient encyclopaedia, K&L
                  beer = 2 * 3 * 47 * inch**3)
quart = Quantity(1, gallon / 4,
                 wine = gallon.wine / 4,
                 beer = gallon.beer / 4)
pint = Quantity(1, quart / 2,
                French = French.pinte,
                wine = quart.wine / 2, # 16.65 UK floz; 16 US ones.
                beer = quart.beer / 2)
gallon.US, quart.US, pint.US = gallon.wine, quart.wine, pint.wine

bucket = 4 * gallon
ton.water = 8 * 7 * bucket
tierce = Quantity(42, gallon,
                  wine = gallon.wine * 42)
puncheon = Quantity(21, bucket,		# or 18 buckets
                    wine = tierce.wine * 2)
pottle = gallon / 2             # and a piggin is about 2 gallons
gill = noggin = Quantity(1, pint / 4, # gill confirmed by Nick
                         US = pint.US / 4)
# but gill = pint / 2 in North England, where noggin is used (KDWB)
# Wikipedia's article on the gallon gives gill = gallon/32
cup = pint / 2
floz = ounce.fluid = Quantity(1, gill / 5, US = gill.US / 4)
teacup = pint / 3
spoon = Object(table = Quantity(1, floz / 2, metric = 20 * cc))
spoon.desert = spoon.table / 2
dram.fluid = spoon.tea = spoon.desert / 2
dram.US = floz.US / 8
minim = drop = Quantity(1, dram / 60, US = dram.US / 60)
drachm = Object(Troy = Troy.drachm, fluid = dram.fluid)
fluid = Object(ounce = ounce.fluid, dram = dram.fluid)

# The volumes following are from a publican's handbook Nick (landlord of the
# Cambridge Blue) showed me.  These names are particularly subject to
# differences of meaning, both geographic and temporal.
firkin = Quantity(9, gallon, US = 9 * gallon.US)
pin = firkin / 2
# I've even met these last two names with swapped meanings.
kilderkin = firkin * 2  # aka kil, possibly kill.
barrel = Quantity(2, kilderkin,  # NHD agrees on barrel = 4 firkin.
                  wine = 63 * gallon.wine / 2,
                  beer = 36 * gallon.beer,
                  ale = 34 * gallon.beer)
hogshead = Quantity(3, kilderkin,
                    US = 7 * firkin.US,
                    wine = 2 * barrel.wine,
                    beer = 3 * barrel.beer / 2,
                    ale = 51 * gallon.beer)
# Oxford handy dictionary and other sources support a hogshead of 54 gallons.
# KDWB gives it as 63 gallons and I've met other figures, including a `wine
# hogshead' of 56 gallons; see also the US variants.
pipe = Quantity(2, hogshead, wine = 2 * hogshead.wine)
# = butt for wine, but a beer.butt might be 3 * beer.hogshead ...
tun = Quantity(2, pipe, # 216 gallons, but I've had 72 suggested.
               wine = 2 * pipe.wine)
wine = Object(doc = """Winchester measures, for wine.

(Due to Queen Anne's regime, 1707)
""",
              gallon = gallon.wine, quart = quart.wine, pint = pint.wine,
              barrel = barrel.wine, rundlet = 18 * gallon.wine, tierce = tierce.wine,
              hogshead = hogshead.wine, puncheon = puncheon.wine,
              butt = pipe.wine, pipe = pipe.wine, tun = tun.wine)
beer = Object(doc = """Archaic measures for beer, pre-1688 and 1803 to 1824.""",
              gallon = gallon.beer, quart = quart.beer, pint = pint.beer,
              barrel = barrel.beer, hogshead = hogshead.beer)
ale = Object(beer,
             doc = """Archaic measures for ale, 1688 to 1803.""",
             barrel = barrel.ale, hogshead = hogshead.ale)
# and so on *ad nauseam* !

# A few other nations' contributions:

# Swedish stuff from http://www.maritimt.net/arkforsk/svenskem.htm
Swedish = Object(
    doc = "Old Swedish units, as used since 1863",
    aln = Quantity(.59372, metre, old=.5938097 * metre),
    kvartmil = mile.nautical)
Swedish.also(fot = Quantity(1, Swedish.aln/2, old=Swedish.aln.old/2), # foot
             # The fot.old is .9904 nano light seconds
             famn = Quantity(3, Swedish.aln,
                             old = 3 * Swedish.aln.old), # fathom (Norsk: favn)
             # "sea mile"; c.f. mile.geographical * 4
             sjoemil = Quantity(4, Swedish.kvartmil, doc="Svensk sjømil"))
mile.sea.Svensk = Swedish.sjoemil
# Joachim also reports favn as a unit of volume ~ 2.4 metre**3 (so ~ 2 * housecord)
Swedish.also(mil = Quantity(6000, Swedish.famn, old=6000 * Swedish.famn.old),
             # ... but Wikipedia gives 6 to 14.485 for mil.old/km (compare
             # 10.7ish here), varying by district, until 1649.
             tum = Quantity(1, Swedish.fot / 10, # thumb, i.e. inch
                            old = Swedish.fot.old / 12))
Swedish.also(linje = Quantity(1, Swedish.tum / 10, # line (pica)
                              old = Swedish.tum.old / 12),
             rode = Quantity(8, Swedish.aln),
             staang = Quantity(5, Swedish.aln, doc="Svensk stäng"))
# also: fingerbredd = fot / 15, tv?rhand = fot / 3 (hand-width); see finger.breadth
Swedish.ref = Quantity(10, Swedish.staang) # 100 fot, c.f. chain.engineer
Swedish.old = Object(doc = "Swedish units in use prior to 1863",
                     fot = Swedish.fot.old, aln = Swedish.aln.old,
                     tum = Swedish.tum.old, famn = Swedish.famn.old,
                     mil = Swedish.mil.old)
foot.Svensk, inch.Svensk = Swedish.fot, Swedish.tum
ell.Svensk, fathom.Svensk = Swedish.aln, Swedish.famn
# rod.Svensk, line.Svensk = Swedish.rode, Swedish.linje # not in right ratios to foot, &c.
# and there's also weights, volumes, etc. on that page ....

# Dansk stuff from http://www.sylvaefa.com/svf1.htm
Danish = Object(alen = .6277 * metre, pot = .9661 * litre,
                # also, a sjømil = mile.nautical ...
                paegl = Quantity(.2242, litre, "Dansk pægl"),
                tyle=dozen, dusin=dozen, gros=gross, snes=20)
Danish.also(fod = Danish.alen / 2, favn = 3 * Danish.alen,
            kande = 2 * Danish.pot, # 3.4 pints
            skok = 3 * Danish.snes, ol = 4 * Danish.snes)
Danish.also(tomme = Danish.fod / 12,
            mil = 4000 * Danish.favn, # 12000 alen, 7532 m == mile.Prussian.
            anker = 20 * Danish.kande) # plural is ankre; 68 pints; c.f. firkin
# pl. = oksehoveder, clealy means Ox-head, .944 hogsheads:
Danish.oksehoved = 6 * Danish.anker
Danish.fad = 4 * Danish.oksehoved # c.f. tun
foot.Dansk, inch.Dansk = Danish.fod, Danish.tomme
ell.Dansk, fathom.Dansk = Danish.alen, Danish.favn

Norse = Object(fot = .31374 * metre,
               favn = 1.88245 * metre, # 6 * fot
               mil = 11294.6 * metre) # 600 * favn; Wikipedia gives 11298 m
Norse.also(alen = 2 * Norse.fot,
           tom = Norse.fot / 12)
line.Norsk = Norse.linje = Norse.tom / 12
foot.Norsk, inch.Norsk = Norse.fot, Norse.tom
ell.Norsk, fathom.Norsk = Norse.alen, Norse.favn
# unse ~ ounce

mile.Scandinavian = mil = Quantity(10, kilo * metre,
                                   """The Norwegian mil, 10 km.

In Norway, 10 km is known as 'en mil'.  This is clearly a metricised
replacement for an archaic Norwegian unit of distance, presumably close to the
Danish and Swedish variants which I've found documented on the web.  The name
is doubtless cognate with the Anglic 'mile' (q.v.), but the distance is
significantly larger (but compare the Prussion mile).  The related sjømil
units of the Scandinavian tradition literally translate as 'sea mile'; see
Swedish.sjoemil, for example, and mile.sea.  However, the Scandinavian
countries have embraced international standards, so now use the 1929 nautical
mile and the metric system of units, rather than clinging to archaic units
like some less civilized countries.
""")
mil.also(Norsk = Norse.mil, Dansk = Danish.mil, Svensk = Swedish.mil)

# Obscure stuff from Kim's dad's 1936 white booklet ...
Swiss = Object(lien = 5249 * yard) # c. (land) league
Dutch = Object(oncen = kilogramme / 10)
Turk = Object(oke = 2.8342 * pound, berri = 1828 * yard)
Russia = Object(verst = 1167 * yard, pood = 36.11 * pound)

# Imperial units of volume (part 2):

# `dry measure'
peck = Quantity(2, gallon, """The peck: a unit of (dry) volume

Proverbially, in English, you'll eat a peck of dirt before you die;
this is rather more than the mental image of a bird pecking at its
food might suggest !
""")
bushel = peck * 4       # but Nick thought a peck was half a bushel ...
# average weights of bushels:
# barley = 47 lb, oats = 38 lb, wheat = 60 lb; c.f. USbushel
strike = 2 * bushel
coomb = bag = 2 * strike
# Quarter = 2 * coomb = 64 * gallon
seam = 2 * bag
wey = load = 5 * seam 	# a `last' is 1 or 2 of these ...
sack = 3 * bushel
firlot = sack / 2
boll = 2 * sack
chaldron = 12 * sack # but OED (etym) gives 32 * bushel (vs. 36 bushel here)
cran = 75 * gallon / 2	# measures herring - c. 750 fish (i.e. 1 fish = 8 floz)

# More US units of volume:
barrel.US = hogshead.US / 2
barrel.US.oil = 42 * gallon.US
barrel.US.dry = 7056 * inch**3 # (7 * 3 * 4)**2 = 7056
bushel.US = Quantity(2150.42, inch**3,
                    doc="""The US bushel.

This is the volume of an 8 inch cylinder with 18.5 inch diameter.
However, the US bushel is also a unit of mass for various types of grain.
See bushel.US.dir for details.
""",
    wheat = 60 * pound,
    soybean = 60 * pound,
    corn = 56 * pound, # of course, this means maize, not wheat
    rye = 56 * pound,
    barley = 48 * pound,
    oat = 32 * pound, # Canada uses 34lb
    rice = 45 * pound)
peck.US = bushel.US / 4
gallon.US.dry = peck.US / 2
# Wikipedia gives the US dry gallon as 1/8 US Winchester bushel
quart.US.dry = gallon.US.dry / 4
pint.US.dry = quart.US.dry / 2
US = Object(gallon = gallon.US, quart = quart.US, pint = pint.US,
            gill = gill.US, dram = dram.US, minim = minim.US,
            floz = floz.US, pony = floz.US, jigger = 3 * floz.US / 2,
            firkin = firkin.US, hogshead = hogshead.US, barrel = barrel.US,
            bushel = bushel.US, peck = peck.US, pace = pace.US, cable = cable.US,
            pica = pica.US, point = point.US,
            cwt = cwt.US, hundredweight = cwt.US, ton = ton.US, therm = therm.US)

# Now, imagine being expected, in school, to memorise the UK share of that ...
