# -*- coding: iso-8859-1 -*-
"""Assorted units of measurement.

See SI.py for base units.

Interesting URLs:
http://www.ukmetrication.com/history2.htm
http://www.sylvaefa.com/svf1.htm
http://www.maritimt.net/arkforsk/svenskem.htm
http://en.wikipedia.org/wiki/Cgs
http://home.clara.net/brianp/
http://www.gnu.org/software/units/units.html

This file documents lots of obscure and/or silly units, may of them derived from
the /usr/share/misc/units repository of knowledge on the subject (see
units.html, above, for details).  This file aims to be all-inclusive, rather
than sensible; however, there are `issues', since some of the units (especially
ones relevant to trade in the anglophone world) have several variants - where
possible, I have tried to find namespace-based ways to manage this mess (e.g.:
print bushel.US.__doc__), but sometimes (e.g. the chain) I just gave up and left
a comment here indicating the part of the story that I've left out !

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

$Id: units.py,v 1.18 2005-03-20 17:45:11 eddy Exp $
"""
from SI import *

# some non-SI base units ...
bit = base_unit('bit', 'bit',
		"""The definitive unit of binary data.

A single binary digit is capable of exactly two states, known as 0 and 1.
A sequence of n binary digits thus has pow(2, n) possible states. """)

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

quid = base_unit('£', 'Pound Sterling',
		 """The base unit of British currency.

Used to be 20 shillings (21 shillings made a Guinea); each shilling was 12
pence, each penny was four farthings.  A florin was two shillings; a crown was
five.  Apparently a pound was also called a sovereign.  HTML supports character
entity &sterling; for the Pound Sterling.

To properly handle money within a system of units, I need support for variation
in time and space (conversion factors between different currencies vary with
time; and you'll get different exchange rates from different trading partners).
Then again, conversion factors between systems of units also show similar
variation - contrast the different nations' archaic units of length, and notice
how units of volume got re-defined by assorted legislative acts over the years.
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

# Angles:
from math import pi
turn = cycle = revolution = 2 * pi * radian
grad = turn / 400 # a unit used by gunners, I believe.
arc = Object(degree = turn / 360)
arc.minute = arc.degree / 60
arc.second = second.arc = arc.minute / 60

# Degrees (of angle and temperature):
degree = Object(arc.degree, arc = arc.degree,
                Centigrade = Kelvin, Celsius = Kelvin, C = Kelvin,
                Fahrenheit = Kelvin / 1.8)
degree.also(F = degree.Fahrenheit)

# Time
minute = Quantity(60, second, arc=arc.minute)
hour = 60 * minute # should this also be the degree of time ?
day = 24 * hour
week = 7 * day
fortnight, year = 2 * week, 27 * 773 * week / 400 # the Gregorian approximation
month = year / 12 # on average, at least; c.f. planets.Month, the lunar month
# factors of 6**3 seconds abound ...

# Miscelaneous SI-compatible units (c.f. SI.py), notably cm,g,s ones:
gram, tonne = milli * kilogramme, kilo * kilogramme
km, cm = kilo * metre, centi * metre
cc = pow(cm, 3)
tex = gram / km # fineness of textiles
dtex, denier = deci * tex, deci * tex / .9

St = Stokes = pow(cm, 2) / second # kinematic viscosity
Angstrom = .1 * nano * metre    # Ångstrøm, aka Å
# (but there's a separate Unicode code-point for the unit ...).
micron, fermi = micro * metre, femto * metre
litre = milli * stere
hectare = hecto * are
barn = femto * hectare

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
rem = 10 * milli * Sv
# 10 milli Gray is also called a rad (but its name conflicts with radian)

Ci = Curie = 37 * giga * Becquerel
R = Rontgen = Quantity(.258, milli * Coulomb / kilogramme,
                       fullname='Röntgen') # also Roentgen ?
Oe = Oersted = kilo * Ampere / metre / 4 / pi # should that be Örsted ?
# see also particle.py for the electron-Volt, eV

# Non-SI but (relatively) scientific:
atm = Atmosphere = Quantity(1.01325, bar,
                            """Standard Atmospheric Pressure""",
                            'atm', 'Atmoshpere')

mach = Quantity(331.46, metre / second,
                doc = """The speed of sound in dry air.
\n(at standard temperature and pressure).\n""")

torr = mmHg = 133.322 * Pascal

Rankine = degree.Fahrenheit
def Fahrenheit(number): return Centigrade((number - 32) / 1.8)

calorie = Object(international = Quantity(4.1868, Joule, # 3.088 * lb.force * foot
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

from lazy import Lazy

# temp class; one instance, working - see below, describes time and pay
class Job (Lazy):
    def __init__(self, pay, period, leave, dailyhours=8, public=8, weeklydays=5):
	"""Sets up job-related data.

	Required arguments:
	  pay -- amount of money you get paid
	  period -- time period between payments
	  leave -- with how many days you get to chose to take off per year
	  
	Optional arguments:
	  dailyhours=7.5 -- hours worked per day (must be < 24)
	  public=8 -- number of public holidays per year
	  weeklydays=5 -- number of working days per week

	Only pay and period should have units; the rest are numbers.  The job
	allows public+leave holidays per year, plus 7-weeklydays per week.

	For pay and period, you can give an annual salary and year even if you
	get paid monthly; or an hourly rate and `hour'.  If period is less than
	a day, pay is presumed to be the actual pay per that much time;
	otherwise, it is presumed to be how much you are paid per that long when
	working the hours you would `normally' work during that period. """

	assert dailyhours * hour <= day
	assert weeklydays * day <= week
	assert leave + public <= year * weeklydays / week # i.e. yearlydays

	self.__pay, self.__period = pay, period
	# The remainder are numbers and may be irritatingly integer:
	self.__daily, self.__weekly = dailyhours, weeklydays
	self.__hols = public + leave
	self.leave = leave

    # time spent working per ...:
    def _lazy_get_day_(self, ignored): return self.__daily * hour
    def _lazy_get_week_(self, ignored): return self.__weekly * self.day
    def _lazy_get_year_(self, ignored):
	return self.day * (year * self.__weekly / week - self.__hols)
    def _lazy_get_month_(self, ignored): return self.year / 12
    def _lazy_get_quarter_(self, ignored): return self.year / 4

    # Rates of pay

    # relative to actual time spent working:
    def _lazy_get_rate_(self, ignored):
	time = self.__period
	rate = self.__pay / time # naive estimate
	if time < day: return rate
	# otherwise, make allowance for time off ...
	if time < week: return rate * day / self.day # eat, drink, sleep
	if 4 * time < year: return rate * week / self.week # week-end
	# more than a season; assume inputs took account of holidays
	return rate * year / self.year

    # amount of pay received per ...
    def _lazy_get_hourly_(self, ignored): return self.rate * hour
    def _lazy_get_daily_(self,  ignored): return self.rate * self.day
    def _lazy_get_weekly_(self, ignored): return self.rate * self.week
    def _lazy_get_annual_(self, ignored): return self.rate * self.year
    def _lazy_get_monthly_(self,   ignored): return self.annual / 12
    def _lazy_get_quarterly_(self, ignored): return self.annual / 4

working = Job(28000 * quid, year, 25)
del Job

import string
# Description of bytes, including the kilo = 1024 twist ...
# but note kibi, mibi etc. should obsolete these; see quantity.py

class bQuantity(Quantity):
    def _quantity(self, what, units):
        try: order = units['bit']
        except KeyError: order = 0
        if order: return self.__class__(what, units)
        return Quantity(what, units)

    def _lazy_get__unit_str_(self, key):
        order = self._unit_order('bit')
        factor, mul = self._quantade_split_(order)

        if factor < 1: factor, mul = 1, ''
        tail = self / pow(factor * byte, order)
        tail = tail._primitive()
        num, uni = tail._number_str, tail._unit_str

        if order > 0:
            if order == 1: here = mul + 'byte'
            else: here = '%sbyte^%d^' % (mul, order)

            if uni:
                ind = string.find(uni, '/')
                if ind < 0: uni = uni + '.' + here
                elif ind > 0: uni = uni[:ind] + '.' + here + uni[ind:]
                else: uni = here + uni

            else: uni = here

        elif order < 0:
            if order == -1: uni = '%s/%sbyte' % (uni, mul)
            else: uni = '%s/%sbyte^%d^' % (uni, mul, -order)

        self._number_str, self._unit_str = num, uni

        if key == '_unit_str': return uni
        elif key == '_number_str': return num
        else: raise ValueError, key

    _lazy_get__number_str_ = _lazy_get__unit_str_

_name = 'byte'
byte = bQuantity(qSample(best=8), # but no actual sample
                bit,
                """The standard unit of memory on a computer.

Whereas the bit is the *natural* unit of binary data, in practice one normally
manipulates data in larger chunks.  These may vary from machine to machine, but
one mostly deals with multiples of 8 bits (these days) - these suffice for 256
distinct values.  An 8-bit byte is also known as an `octet'.

Groups of (typically 2, 4, 8 and sometimes higher powers of two) bytes tend to
have special significance to the machine, and form the building blocks out of
which one builds data structures.

On the large scale, one tends to measure amounts of data in units of kilobytes,
megabytes, gigabytes, ... in which the factor by which each is a multiple of its
predecessor is 1024, rather than the 1000 normally used in kilo, mega, giga ...
So I also define Kb, Mb, Gb, along with the full swath of positive-exponent
quantifiers applied to byte, as kilobyte etc. above.  Indeed, it is mainly in
order to do this that I bother defining the byte ... """,
                _name)

from quantity import _quantifier_dictionary
_row = filter(lambda x: x > 0, _quantifier_dictionary.keys())
_row.sort()
for _key in _row:
    if _key % 3: continue # skip deka, hecto
    _nom = '%sbyte' % _quantifier_dictionary[_key]
    try: exec '%s = Quantity(1024, %s, nom="%s")' % (_nom, _name, _nom)
    except OverflowError:
        # assert: _nom is terabyte
        exec '%s = Quantity(1024L, %s, nom="%s")' % (_nom, _name, _nom)
    _name = _nom

del _nom, _name, _key, _row, _quantifier_dictionary
Kb, Mb, Gb = kilobyte, megabyte, gigabyte

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
# all the `English-speaking' peoples: by `imperial' I mean those in which the US
# and UK have diverged.  The US versions of these are collected together in the
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

# Anglophone units of length:
inch = 2.54e-2 * metre # from Latin, uncia, via OE ynce
caliber = inch / 100
barleycorn = inch / 3
pica = line = barleycorn / 4 # inch / 12, c.f. French.line, Norse.linje, Swedish.linje
hand = 4 * inch
palmlength = 8 * inch
span = 9 * inch
fingerlength = span / 2
fingerbreadth = 7 * inch / 8
ell = cubit = Quantity(5, span, flemish = Quantity(27, inch, doc="The Flemish ell"))
# but also: cubit = yard / 2, span = cubit / 2 (but ell = 45 in, as here)
ft = foot = Quantity(3, hand,
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

league = 3 * mile
league.document("""league: a varying measure of road distance, usu. about three miles (poxy).""")
marathon = 26 * mile + 385 * yard
point = pica / 6        # the printer's point
point.silversmith = inch / 4000 # the silversmith's point (contrast: point.jeweller, below - under mass !)
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
                               navy = 720 * 1200 / 3937 * metre))
league.marine = 3 * mile.nautical
knot = mile.nautical * 1. / hour

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
stack = Object(wood = 108 * foot.timber)
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
psi = pound.force / inch**2
horsepower = Quantity(
    550, foot * pound.force / second,
    metric = 75 * kilogram.force * metre / second,
    electric = 746 * Watt,
    boiler = 9809.50 * Watt,
    water = 746.043 * Watt,
    donkey = 250 * Watt) 
chevalVapeur = horsepower.metric
celo = foot / second**2
jerk = celo / second
poundal = pound * celo
reyn = psi * second
slug = pound.force / celo
slinch = 12 * slug
duty = foot * pound.force

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
cup = pint / 2
floz = ounce.fluid = Quantity(1, gill / 5, US = gill.US / 4)
teacup = pint / 3
spoon = Object(table = floz / 2) # but 20 cc in metric versions
spoon.desert = spoon.table / 2
dram.fluid = spoon.tea = Quantity(1, spoon.desert / 2)
dram.US = floz.US / 8
minim = drop = Quantity(1, dram / 60, US = dram.US / 60)
drachm = Object(Troy = Troy.drachm, fluid = dram.fluid)
fluid = Object(ounce = ounce.fluid, dram = dram.fluid)

# The volumes following are from a publican's handbook Nick (landlord of the
# Cambridge Blue) showed me.  These names are particularly subject to
# differences of meaning, both geographic and temporal.
firkin = Quantity(9, gallon, US = 9 * gallon.US)
pin = firkin / 2
# but I've met these two names with swapped meanings.
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

# Swedish stuff from http://www.maritimt.net/arkforsk/svenskem.htm
Swedish = Object(
    doc = "Old Swedish units, as used since 1863",
    aln = Quantity(.59372, metre, old=.5938097 * metre),
    kvartmil = mile.nautical)
Swedish.also(fot = Quantity(1, Swedish.aln/2, old=Swedish.aln.old/2), # foot
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

mil = Quantity(10, kilo * metre,
               """The Norwegian mil, 10 km.

In Norway, 10 km is known as 'en mil'.  This is clearly a metricised replacement
for an archaic Norwegian unit of distance, presumably close to the Danish and
Swedish variants which I've found documented on the web.  The name is doubtless
cognate with the Anglic 'mile' (q.v.), but the distance is significantly larger.
The related sjømil units of the Scandinavian tradition literally translate as
'sea mile'; see Swedish.sjoemil, for example.  However, the Scandinavian
countries have embraced international standards, so now use the 1929 nautical
mile and the metric system of units, rather than clinging to archaic units like
some less civilized countries.\n""",
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
barrel.US.dry = 7056 * pow(inch, 3) # (7 * 3 * 4)**2 = 7056
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
quart.US.dry = gallon.US.dry / 4
pint.US.dry = quart.US.dry / 2
US = Object(gallon = gallon.US, quart = quart.US, pint = pint.US,
            gill = gill.US, dram = dram.US, minim = minim.US,
            floz = floz.US, pony = floz.US, jigger = 3 * floz.US / 2,
            firkin = firkin.US, hogshead = hogshead.US, barrel = barrel.US,
            bushel = bushel.US, peck = peck.US, pace = pace.US, cable = cable.US,
            cwt = cwt.US, hundredweight = cwt.US, ton = ton.US, therm = therm.US)

_rcs_log = """
 $Log: units.py,v $
 Revision 1.18  2005-03-20 17:45:11  eddy
 Scientific python module contradicted my datum on calorie; adjusted accordingly.

 Revision 1.17  2005/02/13 20:47:09  eddy
 Suggest several et al.
 Added to comments, shuffled order of bits, tidied up a bit.

 Revision 1.16  2005/02/06 10:46:56  eddy
 Conformed French units to name-spacery used for Scandic ones.
 Noted similarity of Swiss lien to the league.
 Guessed error bar on TNT from number of sig. figs in given datum.

 Revision 1.15  2005/01/17 23:57:38  eddy
 Added degree Object to carry the various kinds thereof.
 Added gunner's grad = turn / 400.
 Noted similarities between Danish volumes and UK ones.
 Noted Danish and Norse members of foot &c. families.
 Assorted minor tidy-up.

 Revision 1.14  2005/01/16 19:28:09  eddy
 Merged in lots of data I'd gathered into my copy of this at work.
 Added some documentation.

 Revision 1.13  2005/01/16 16:40:47  eddy
 Made to{Centigrade,Fahrenheit} redundant - temperatures now have an attribute for this.
 Fixed iso-latin-1 mangled chars

 Revision 1.12  2004/04/03 18:09:26  eddy
 Mass/Time bodge now redundant thanks to kind-specific _lazy_late_ in Quantity.

 Revision 1.11  2003/09/05 00:22:00  eddy
 Packed various things into namespaces, rather than jamming several words
 together to make their names.  Lots of tidy-up in the process.

 Revision 1.10  2003/04/21 20:15:45  eddy
 Use best, not mean, in qSample constructor (now delegates directly to Sample).

 Revision 1.9  2003/04/17 22:46:16  eddy
 Put non-SI units of angle back in units.py

 Revision 1.8  2003/04/17 22:42:02  eddy
 moved various SI-compatibles in from SI; moved angles to SI; some comments

 Revision 1.7  2003/04/17 22:19:20  eddy
 Fixed stupid typo in toCentrigrade; enhanced several docs; wrote, into
 doc string, a minor essay on the relevance of choice of unit.

 Revision 1.6  2002/10/06 18:04:58  eddy
 Removed use of the Quantity.name() - bad choice of method name !
 Also, eV is now elsewhere.

 Revision 1.5  2002/09/23 18:52:44  eddy
 Expanded sterling's doc.  Added Centigrade and Fahrenheit handlers.

 Revision 1.4  2002/03/21 03:06:18  eddy
 Added Scoville unit of pungency.

 Revision 1.3  2002/02/15 16:03:51  eddy
 Moved Mass/Time bodge to SI, various minor tweaks.

 Revision 1.2  2002/02/11 01:37:00  eddy
 Added Mass and Time to implement .force and .light attributes (respectively).
 Added quid and some comments.  Replaced workyear mess with Job etc.

 Initial Revision 1.1  2001/03/21 20:04:06  eddy
"""
