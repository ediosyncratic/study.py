# -*- coding: iso-8859-1 -*-
"""Archaic and idiosyncratic units of measurement.

See SI.py for base units and units.py for various others.

This file documents lots of obscure and/or silly units, may of them derived from
the /usr/share/misc/units repository of knowledge on the subject (see
units.html, in URLs attribute of this module, for details).  This file aims to
be all-inclusive, rather than sensible; however, there are `issues', since some
of the units (especially ones relevant to trade in the anglophone world) have
several variants - where possible, I have tried to find namespace-based ways to
manage this mess (e.g.: print bushel.US.__doc__), but sometimes (e.g. the chain)
I just gave up and left a comment here indicating the part of the story that
I've left out !

I should probably replace most of this file with a family of XML or RDF
documents describing all the units, coupled to a deployment of some standard
parsing tool-kit to access the data they provide.

There are enough units of measurement here to provide for some cutely specific
units of measurements in all sorts of odd domains.

  For example, consider the fuel efficiency of transport systems fueled by
  petrol; this is commonly measured in miles / gallon in the anglophone world
  (with consequent confusion between the new imperialists and the old; while the
  two agree on what a mile is, they disagree on what a gallon is; five UK
  gallons roughly equal six US gallons) or kilometres / litre in the civilised
  world (48 miles / UK gallon is roughly equal to 40 miles / US gallon and to 17
  kilometres / litre).  This last is, itself, one million times the natural SI
  unit, metres per cubic metre.

  Superficially, this is a 1/area unit, though the implicit `of petrol' clause
  in it does subvert that a little; 17 km/litre is officially 17 million /
  square metre, a.k.a. 17 per square millimetre; which is meaningless drivel
  until I point out that it means that each square millimetre of cross-sectional
  area of your fuel tank contributes (at this fuel efficiency) 17 to the ratio
  between the speed of your vehicle and the rate at which the fuel level in the
  tank is dropping; if your fuel tank's cross-section is one square foot (92,903
  square mm), and your vehicle is managing 17 km/litre, then you're travelling a
  little over one and a half million (i.e. 17 * 92,903) times as fast as the
  level in the fuel tank is dropping.

  In reality, fuel efficiencies of real road vehicles seem to fall in the range
  from around 8 to around 80 mpg (UK), so that a unit of order 20 mpg would be
  quite handy; as it happens, one furlong / UK floz is exactly 20 miles per UK
  gallon, suggesting it as the ideal imperial unit for the task (i.e. it's the
  right size and marvelously perverse).  A slightly saner unit would clearly be
  the mile per UK pint - the given range runs from one to 10 of these, and
  that's a nice range of numbers to work with - which is 2.83 km / litre.  By a
  weird twist of fate, 2.83 is almost exactly the square root of 8; so that the
  civilised unit, one km/litre, is 2.83 miles per UK gallon; and using this unit
  makes the range of real-world values run from 2.8 to 28, with 10 km/litre
  (roughly 28 miles per UK gallon or 23 per US gallon) presenting itself as a
  fairly good cut-off between `inefficient' and `not so bad, all things
  considered'.

  Note that one might equally measure the same phenomenon as a fuel consumption
  rate, in gallons per mile or litres per kilometre, which would encourage
  trying to find a unit of order one UK gallon per 80 miles, a.k.a. one UK pint
  per ten miles or 2 UK floz per mile.  (The pint per mile would also make quite
  a good unit of measurement for pub-crawls, albeit with very different
  semantics.)  A vehicle consuming a small number of floz per mile (up to five
  or six, to match the cut-off above) would then be considered frugal, while
  ones beyond that would be considered wasteful.

Chosing the right unit, and the right way up (c.f. the contrast between fuel
efficiency and consumption rate), is important to how measurements get
interpreted - for example, although the gradient of a sloping road may formally
be a dimensionless quantity, it makes sense to measure `slope' in such units as
metre (of ascent or descent) per kilometre (of travel) or, in a culture which
has different units for vertical and horizontal lengths, fathoms per furlong.
Even when using the official SI unit, different ways of expressing a unit can
change perceptions of its meaning - for example, (metre / second)**2 means the
same as Joule / kilogramme, but expresses a different perspective on it.

$Id: archaea.py,v 1.7 2008-04-21 23:41:24 eddy Exp $

  You, in this country, are subjected to the British insularity in weights and
  measures; you use the foot, inch and yard. I am obliged to use that system,
  but must apologize to you for doing so, because it is so inconvenient, and I
  hope Americans will do everything in their power to introduce the French
  metrical system. ... I look upon our English system as a wickedly,
  brain-destroying system of bondage under which we suffer. The reason why we
  continue to use it, is the imaginary difficulty of making a change, and
  nothing else; but I do not think in America that any such difficulty should
  stand in the way of adopting so splendidly useful a reform. -- Lord Kelvin.
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
http://en.wikipedia.org/wiki/Category:Obsolete_units_of_measure

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
"""

from units import *

# some non-SI base units ...
Lenat = base_unit('L', 'Lenat',
		  """The standard unit of bogosity

See the New Hackers' Dictionary, under microLenat.  Also known as the Reid. """)

Helen = base_unit('Helen', 'Helen',
		  """The standard unit of beauty (trad).

Definitively `beautiful enough to launch a thousand ships', so that launching a
single ship gains credit for a single milli-Helen.  The origin of this is the
story of the Trojan war, in which a Greek fleet of a thousand ships, carrying a
great army, went to retrieve Helen from Troy, to which Paris had taken her. """)

Scoville = base_unit('Scoville', 'Scoville',
		     """Standard unit of pungency.

Dilution to one part in N (with sugar water) makes the taste undetectable for an
N Scoville pungency.  Conventional wisdom classes 0 to 500 as mild, 500 to 999
as medium, 1k to 4.999 k as hot and 5k or above super-hot.  Chile sauces scoring
93 k Scoville are insane, but some exist as high as 577 k Scoville; Jalapeno
extract scores about 4.5 k; pure Capsaicin rates over 15,000,000 Scoville Units.
""")

# Dimensionless:
dozen = Quantity(12, {}, baker = 13)
pair, half, quarter = 2, .5, .25
# few, some, several (.best=7), many ? Mainly of value for different-shaped distributions.
percent = .01
prial = nest = 3
dickers = 10 # *must* be a `corruption' of dix, arranging to *not* sound like `dicks'
score, shock, gross = 20, 60, 144 # c.f. Danish.{snes, skok, gros}
timer = flock = 40
greatgross = gross * dozen
paper = Object(
    quire = Quantity(25, {}, short = 24), # baker's two-dozen ?
    ream = Quantity(500, {}, short = 480, perfect = 516)) # 43 * dozen
paper.also(bundle = 2 * paper.ream, bale = 10 * paper.ream,
           short = Object(quire = paper.quire.short, ream = paper.ream.short))

Geldof = Quantity(3e4, 1/day,
                  """The pre-Geldof poverty-induced infant mortality rate.

As at 2005, the Live8Live organizers report that 30,000 children die every day,
needlessly, as a result of extreme poverty.  This seemed a reasonable basis on
which to name a rate-of-death unit.  Since Sir Bob Geldof stands as front-man
for the Live8Live organization, and has done sterling work in previous kindred
organizations, he seemed a natural person after whom to name the unit.  It
should be noted that the 3e4/day figure is merely the infant mortality rate; we
could as readily use 5e4 if we include adults.\n""")


# Many units are or have been subject to what amounts to dialect variation, some
# have undergone significant changes of definition over time (as compared to the
# fine-tuning of, for example, the metre in 1975).  This is only to be expected:
# language works like that and it's words we're discussing here.

# Comments in the following indicate the views of various sources. (K&L = Kaye
# and Laby, NHD = New Hackers' Dictionary, KDWB = Kim's dad's 1936 white
# booklet, EB = 11th edition Encyclopaedia Britannica, 1911, article on Weights
# and Measures.)  Where source is not given in a comment (either on the line or
# at start of section) the calculation of the line reflects a consensus.  Either
# that or it's some random detail I tripped over somewhere and jotted down ...

# Units I describe as `anglophone' are, to the best of my knowledge, common to
# all the `English-speaking' peoples (they also typically have equivalents in
# archaic units of Scandinavia): by `imperial' I mean those in which the US and
# UK have diverged.  The US versions of these are collected together in the
# namespace of an object US; these units also appear as .US attributes of the UK
# variants.  Various other nations' arcane units are likewise collected in
# name-space objects (whose names are English words for the national
# adjectives); where cognate with an anglophone unit, they also appear in that
# unit's name-space (using the national adjective in its native tongue, except
# for French - an attribute name can't have a cedilla in it).  For these
# purposes, Troy is treated as a nation - but doesn't actually mean the ancient
# city-state of that name !  I should probably do the same to the UK versions of
# anglophone units - if only to make this module's name-space less cluttered !

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

jiffy = second / 60 # but also s/100 and ms ...

# Anglophone units of length:
inch = 2.54e-2 * metre # from Latin, uncia, via OE ynce
caliber = inch / 100
barleycorn = inch / 3
pica = barleycorn / 2 # CSS 2.1 spec
line = pica / 2 # inch / 12, c.f. French.line, Norse.linje, Swedish.linje
hand = 4 * inch
palmlength = 8 * inch
span = 9 * inch
fingerlength = span / 2
fingerbreadth = 7 * inch / 8
ell = Quantity(5, span, flemish = Quantity(27, inch, doc="The Flemish ell"))
cubit = span * 2 # but also used as synonym for ell
ft = foot = Quantity(3, hand,
                     """The English foot.

See namespace for other nation's variants on this unit.  All are fairly close to
the distance (given here as foot.astronomical) that light travels in a nano second:
those closest to it are the old Swedish foot, just under 1% below, and the
English foot, just over 1/60 above.
""",
                     astronomical = nano * second.light,
                     survey = Quantity(1.2e3 / 3937, metre,
                                       """The (geodetic) survey foot.

In the US the Metric Act of 1866 defined the foot to equal exactly
1200/3937m, or approximately 30.48006096cm.  This unit, still used for
geodetic surveying in the United States, is now called the survey
foot.\n"""))

yard = Quantity(3, foot,
                """The Modern Yard.

The yard (0.9144 metres) is a modern survivor of a family of roughly
stride-sized units of length dating back - if the excellent Monsieur Thom is to
be believed - to prehistory (Thom measured the lengths in stone circles all over
Europe and found evidence that they were measured to a common unit,
approximately equal to the yard).  It takes its name from a Germanic word,
'gyrd'.  Its metric replacement, the metre, is just slightly bigger.\n""")

nail = yard / 16
pace = Quantity(5, foot, US = 30 * inch)
rope = 4 * pace
fathom = Quantity(2, yard, """The Fathom.

The fathom is cognate with the Swedish famn and Danish favn (q.v.) and has
principally survived in use as a maritime measure of vertical distances -
notably the depths of bodies of water.\n""")

chain = 22 * yard 	# but engineers (and Ramsden) use a 100ft chain !
chain.engineer = 100 * foot # rather than 100 links; c.f. Swedish.ref
rod = pole = perch = chain / 4 # rod from German
link = pole / 25
furlong = 10 * chain	# from German, `furrow long'
mile = Quantity(8, furlong,
                """The Statute Mile.

The British mile (also used in the U.S.A.) is just over 1600 metres; for a long
time it was used as a standard distance for races, fitting nicely with the
pattern of doubling lengths upwards from 100m.  Like all Imperial units, it has
a long and contorted history.

Its nominal origin is in the Imperial Roman 'millum pes' - a thousand paces,
though the Roman pes was the distance a soldier's foot travels in each stride,
roughly double the separation of the feet when both are on the ground.  Compare
this to the British 'pace', of 5 feet, which is quite close to one thousandth of
a mile, while the US 'pace' is exactly half as long.

Some backward countries seem likely to continue using this unit to measure
distances - along with the mile per hour as a unit of speed - for some time to
come.  Contrast mile.nautical and the Scandinavian mil.\n""")

league = Quantity(3, mile,
                  """The league

A varying measure of road distance, usu. about three miles (poxy).
""")
marathon = 26 * mile + 385 * yard
point = pica / 12        # the printer's point
point.silversmith = inch / 4000 # the silversmith's point (contrast: point.jeweller - a mass)
point.arc = arc.point
shoe = Object( # units of thickness of leather in shoes
    iron = inch / 48, # soles
    ounce = inch / 64)# elsewhere
railgauge = 4 * foot + 8.5 * inch
mile.also(sea = 2000 * yard,
          geographical = 7420 * metre,
          Prussian = 7532 * metre,
          nautical = Quantity(1852, metre, # K&L, given as the definition of this unit.
                              """The nautical mile

I've met assertions that the nautical mile is 2000 yards (here given as mile.sea
since I've seen it called a sea mile).  Alternatively, that it's one minute of
arc - i.e. Earth.surface.radius * pi / 180 / 60.  My available figures for the
Earth's radius yield figures ranging from 1853 to 1855 metres, aka 2026 to 2029
yards: so Kaye & Laby fits with the minute of arc view (to reasonable accuracy)
and I take the 2000 yard figure as being a widely used approximation.
Apparently, the US used some other unit until 1954, the UK until 1970; both
catching up with a 1929 international standard.
""",
                              UK = 6080 * foot)) # until 1970
cable = Quantity(0.1, mile.nautical,
                 US = Quantity(100, fathom,
                               navy = 720 * foot.survey))
league.marine = 3 * mile.nautical
knot = mile.nautical / hour

foot.French = 4500 * metre / 13853	# pied de roi, French foot
inch.French = foot.French / 12
point.French = inch.French / 144
French = Object(pied = foot.French,
                # what're the right French names for inch, line, point ?
                inch = inch.French,
                line = inch.French / 12,
                point = point.French,
                toise = 6 * foot.French,
                arpent = (180 * foot.French)**2)

foot.SI = Quantity(nano, second.light, """The SI foot, or light nanosecond.

In one nanosecond, light travels a distance firmly within the range of lengths
known, among various nations' diverse archaic units, as a foot.  The Swedish
foot (or 'fot' in Swedish) is less than 1% shorter; most others are slightly
longer.  Given that the metre is now defined in terms of the light second, it
violates the spirit of SI to retain it; it should be replaced by the light
second and units derived therefrom.  The light nanosecond thus presents itself
naturally as a replacement unit, which would naturally be called the 'SI foot'.
""")

# Archaic units of mass:
grain = 64.79891e-6 * kilogramme        # K&L; one barleycorn's mass
mite = grain / 20
droit = mite / 24 # a cube of water half a mm on each side
periot = droit / 20 # an eighth of the Planck mass
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
TNT = Quantity(4.184 + .001 * tophat, giga * Joule) / ton.US # (2.15 km/s)**2

# Anglophone units of energy:
CHU = calorie * pound / gram # whatever CHU is ! (KDWB)
BTU = Btu = BritishThermalUnit = CHU * Rankine / Kelvin
therm = Quantity(.1 * mega, BTU, US = 1.054804e8 * Joule)

# Anglophone units of area (KDWB):
acre = chain * furlong # consensus; mile**2/640.  From German/Norse: field
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

# Other units of mass: KDWB
stone.butcher = 8 * pound
dram.apothecary = 3 * scruple
denier.Troy = 24 * grain # = ounce.Troy / 20, the denier (a frankish coin) or pennyweight
ounce.Troy = 8 * dram.apothecary
pound.Troy = 12 * ounce.Troy
carat = Quantity(.2, gram, # metric variant, from /usr/share/misc/units
                 """The Carat, a unit of mass used by jewelers.

Apparently originally the mass of a carob seed.\n""",
                 Troy=3.17 * grain) # or 3.163 grain according to u.s.m.u
Troy = Object(drachm = dram.apothecary, denier = denier.Troy, carat = carat.Troy,
              ounce = ounce.Troy, pound = pound.Troy)
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


# Imperial units of volume (part 1):

# `fluid measure'
gallon = Quantity(4.54609, litre, # 10 * pound / water.density
                  wine = 3 * 11 * 7 * inch**3, # (aka US gallon) ancient encyclopaedia, K&L
                  beer = 2 * 3 * 47 * inch**3)
quart = Quantity(1, gallon / 4,
                 wine = gallon.wine / 4,
                 beer = gallon.beer / 4)
pint = Quantity(1, quart / 2,
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
tun = Quantity(2, pipe, # 216 gallons, but I've had 72 suggested.
               wine = 2 * pipe.wine)
wine = Object(doc = """Winchester measures, for wine.

(Due to Queen Anne's regime, 1707)\n""",
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
             sjoemil = Quantity(4, Swedish.kvartmil, doc="Svensk sjømil")) # "sea mile"
# Joachim also reports favn as a unit of volume ~ 2.4 metre**3 (so ~ 2 * housecord)
Swedish.also(mil = Quantity(6000, Swedish.famn, old=6000 * Swedish.famn.old),
             tum = Quantity(1, Swedish.fot / 10, # thumb, i.e. inch
                            old = Swedish.fot.old / 12))
Swedish.also(linje = Quantity(1, Swedish.tum / 10, # line (pica)
                              old = Swedish.tum.old / 12),
             rode = Quantity(8, Swedish.aln),
             staang = Quantity(5, Swedish.aln, doc="Svensk stäng"))
# also: fingerbredd = fot / 15, tv?rhand = fot / 3 (hand-width)
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
            mil = 4000 * Danish.favn, # 12000 alen, 7.532 km
            anker = 20 * Danish.kande) # plural is ankre; 68 pints; c.f. firkin
Danish.oksehoved = 6 * Danish.anker # pl. = oksehoveder, clealy means Ox-head, .944 hogsheads ...
Danish.fad = 4 * Danish.oksehoved # c.f. tun
foot.Dansk, inch.Dansk = Danish.fod, Danish.tomme
ell.Dansk, fathom.Dansk = Danish.alen, Danish.favn

Norse = Object(fot = .31374 * metre,
               favn = 1.88245 * metre, # 6 * fot
               mil = 11294.6 * metre) # 600 * favn
Norse.also(alen = 2 * Norse.fot,
           tom = Norse.fot / 12)
line.Norsk = Norse.linje = Norse.tom / 12
foot.Norsk, inch.Norsk = Norse.fot, Norse.tom
ell.Norsk, fathom.Norsk = Norse.alen, Norse.favn
# unse ~ ounce

mil = Quantity(10, kilo * metre,
               """The Norwegian mil, 10 km.

In Norway, 10 km is known as 'en mil'.  This is clearly a metricised replacement
for an archaic Norwegian unit of distance, presumably close to the Danish and
Swedish variants which I've found documented on the web.  The name is doubtless
cognate with the Anglic 'mile' (q.v.), but the distance is significantly larger
(but compare the Prussion mile).  The related sjømil units of the Scandinavian
tradition literally translate as 'sea mile'; see Swedish.sjoemil, for example.
However, the Scandinavian countries have embraced international standards, so
now use the 1929 nautical mile and the metric system of units, rather than
clinging to archaic units like some less civilized countries.\n""",
               Dansk = Danish.mil,
               Svensk = Swedish.mil)

# Obscure stuff from Kim's dad's 1936 white booklet ...
Swiss = Object(lien = 5249 * yard) # c. (land) league
Dutch = Object(oncen = kilogramme / 10)
Turk = Object(oke = 2.8342 * pound, berri = 1828 * yard)
Russia = Object(verst = 1167 * yard, pood = 36.11 * pound)

# Imperial units of volume (part 2):

# `dry measure'
peck = gallon * 2
bushel = peck * 4       # but Nick thought a peck was half a bushel ...
# average weights of bushels: barley = 47 lb, oats = 38 lb, wheat = 60 lb; c.f. USbushel
strike = 2 * bushel
coomb = bag = 2 * strike
# Quarter = 2 * coomb = 64 * gallon
seam = 2 * bag
wey = load = 5 * seam 	# a `last' is 1 or 2 of these ...
sack = 3 * bushel
firlot = sack / 2
boll = 2 * sack
chaldron = 12 * sack
cran = 75 * gallon / 2	# measures herring - c. 750 fish (i.e. 1 fish = 8 floz)

# More US units of volume:
barrel.US = hogshead.US / 2
barrel.US.oil = 42 * gallon.US
barrel.US.dry = 7056 * inch**3 # (7 * 3 * 4)**2 = 7056
bushel.US = Quantity(2150.42, inch**3,
                    doc="""The US bushel.

This is the volume of an 8 inch cylinder with 18.5 inch diameter.
However, the US bushel is also a unit of mass for various types of grain.
See dir(bushel.US) for details.
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
            cwt = cwt.US, hundredweight = cwt.US, ton = ton.US, therm = therm.US)

# Now, imagine being expected, in school, to memorise the UK share of that ...
