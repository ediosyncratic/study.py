"""Assorted units of measurement.

See SI.py for base units.

$Id: units.py,v 1.3 2002-02-15 16:03:51 eddy Exp $
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

quid = base_unit('&sterling;', 'Pound Sterling',
		 """The base unit of British currency.""")

# dimensionless:
from math import pi
dozen = 12
bakersDozen = 13
half = .5
quarter = .25
pair = 2
nest = 3
dickers = 10
score = 20
timer = flock = 40
shock = 60
gross = 144
greatgross = gross * dozen
paper = Object(
    shortquire = 24,
    quire = 25,
    shortream = 480,
    ream = 500,
    perfectream = 516) # 43 * dozen
paper.also(bundle = 2 * paper.ream, bale = 10 * paper.ream)

# time
minute = Time(60 * second)
hour = Time(60 * minute)
day = Time(24 * hour)
week = Time(7 * day)
fortnight = Time(2 * week)
year = Time(27 * 773 * week / 400)	# the Gregorian approximation
month = Time(year / 12) # on average, at least; c.f. planets.Month, the lunar month
# factors of 216 seconds abound ...

# etc.
turn = cycle = revolution = 2 * pi * radian
arc = Object(degree = turn / 360)
arc.minute = arc.degree / 60
arc.second = arc.minute / 60
erg = .1 * micro * Joule
dyn = 10 * micro * Newton

Ci = Curie = 37 * giga * Becquerel
R = Rontgen = .258 * milli * Coulomb / kilogramme
R.name('R&ouml;ntgen') # also Roentgen ?
Oe = Oersted = kilo * Ampere / metre / 4 / pi
eV = 160.21e-21 * Coulomb * Volt # electron-Volt, .16 atto Joules

Rankine = Kelvin / 1.8          # steps in the Fahrenheit scale
tonne = Mass(kilo, kilogramme)
calorie = 4.1868 * Joule	# the thermodynamic calorie (IT), not any other sort.
# food talks about `calorie' but means kilo calorie
BTU = Btu = BritishThermalUnit = 1.05506 * kilo * Joule
# calorie * pound * Farenheit / gram / Kelvin
CHU = 1.8 * BTU         # whatever CHU is ! (KDWB)
therm = 1.05506e8 * Joule # US uses 1.054804e8
clausius = kilo * calorie / Kelvin

bar = .1 * mega * Pascal
atm = Atmosphere = Quantity(1.01325, bar,
                            """Standard Atmospheric Pressure""",
                            'atm', 'Atmoshpere')
sound = Object(speed = Quantity(331.46, metre / second,
                                doc = """The speed of sound in dry air
                                (at standard temperature and pressure)"""))
mach = sound.speed
torr = mmHg = 133.322 * Pascal
denier = deci * tex / .9

from lazy import Lazy

# temp class; one instance, working - see below, describes time and pay
class Job (Lazy):
    def __init__(self, pay, period, leave, dailyhours=7.5, bank=8, weeklydays=5):
	"""Sets up job-related data.

	Required arguments:
	  pay -- amount of money you get paid
	  period -- time period between payments
	  leave -- with how many days you get to chose to take off per year
	  
	Optional arguments:
	  dailyhours=7.5 -- hours worked per day (must be < 24)
	  bank=8 -- number of bank holidays per year
	  weeklydays=5 -- number of working days per week

	Only pay and period should have units; the rest are numbers.  The job
	allows bank+leave holidays per year, plus 7-weeklydays per week.

	For pay and period, you can give an annual salary and year even if you
	get paid monthly; or an hourly rate and `hour'.  If period is less than
	a day, pay is presumed to be the actual pay per that much time;
	otherwise, it is presumed to be how much you are paid per that long when
	working the hours you would `normally' work during that period. """

	assert dailyhours * hour <= day
	assert weeklydays * day <= week
	assert leave + bank <= year * weeklydays / week # i.e. yearlydays

	self.__pay, self.__period = pay, period
	# The remainder are numbers and may be irritatingly integer:
	self.__daily, self.__weekly = dailyhours, weeklydays
	self.__hols = bank + leave
	self.leave = leave

    # time spent working per ...:
    def _lazy_get_day_(self, ignored): return Time(self.__daily * hour)
    def _lazy_get_week_(self, ignored): return Time(self.__weekly * self.day)
    def _lazy_get_year_(self, ignored):
	return Time(self.day * (year * self.__weekly / week - self.__hols))
    def _lazy_get_month_(self, ignored): return Time(self.year / 12)
    def _lazy_get_quarter_(self, ignored): return Time(self.year / 4)

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
byte = bQuantity(qSample(mean=8), # but no actual sample
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
# at start of section) the calculation of the line reflects a consensus.

# Units I describe as `anglophone' are, to the best of my knowledge, common to
# all the `English-speaking' peoples: by `imperial' I mean those in which the US
# and UK have diverged.

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

nauticalMile = 1852 * metre     # K&L, given as the definition of this unit.
# I've met assertions that the nautical mile is 2000 yards.  Alternatively, that
# it's one minute of arc - i.e. Earth.surface.radius * pi / 180 / 60.  My
# available figures for the Earth's radius yield figures ranging from 1853 to
# 1855 metres, aka 2026 to 2029 yards: so Kaye & Laby fits with the minute of
# arc view (to reasonable accuracy) and I take the 2000 yard figure as being a
# widely used approximation.  Apparently, the US used some other unit until
# 1954, the UK until 1970; both catching up with a 1929 international standard.
USnavyCable = 720 * 1200 / 3937 * metre
cable = nauticalMile / 10
marineLeague = 3 * nauticalMile
knot = nauticalMile * 1. / hour

# Anglophone units of length:
inch = 2.54e-2 * metre
caliber = inch / 100
barleycorn = inch / 3
hand = 4 * inch
palmlength = 8 * inch
span = 9 * inch
fingerlength = span / 2
fingerbreadth = 7 * inch / 8
ell = cubit = 5 * span
flemishell = 27 * inch
ft = foot = 3 * hand
yard = 3 * foot
nail = yard / 16
pace = 5 * foot
USpace = 30 * inch
rope = 4 * pace
fathom = 6 * foot
UScable = 100 * fathom
chain = 22 * yard 	# but engineers (and Ramsden) use a 100ft chain !
rod = pole = perch = chain / 4
link = pole / 25
furlong = 10 * chain	# `furrow long'
mile = 8 * furlong
league = 3 * mile
league.document("""league: a varying measure of road distance, usu. about three miles (poxy).""")
marathon = 26 * mile + 385 * yard
ppoint = inch / 72      # the printer's point
spoint = inch / 4000    # the silversmith's point (see also jeweler's, under mass !)
shoe = Object( # units of thickness of leather in shoes
    iron = inch / 48, # soles
    ounce = inch / 64)# elsewhere
railgauge = 4 * foot + 8.5 * inch
UKnm = 6080 * foot		# until 1970
seamile = 6000 * foot

pied = 4500 * metre / 13853	# pied de roi, French foot
French = Object(foot = pied,
                inch = pied / 12,
                line = pied / 144,
                point = pied / 12**3,
                toise = 6 * pied,
                arpent = (180 * pied)**2)

# Anglophone units of mass:
grain = Mass(64.79891e-6, kilogramme)        # K&L
mite = Mass(1. / 20, grain)
droit = Mass(1. / 24, mite)
periot = Mass(1. / 20, droit)
blanc = Mass(1. / 24, periot)
scruple = Mass(20, grain)
lb = pound = Mass(350, scruple)
oz = ounce = Mass(1. / 16, pound)
dram = Mass(1. / 16, ounce)
clove = Mass(7, pound)
stone = Mass(2, clove)
cwt = hundredweight = Mass(8, stone)
ton = Mass(20, cwt)
UScwt = cental = Mass(100, pound)
USton = Mass(20, UScwt)
TNT = 4.184e9 * Joule / USton

# Anglophone units of area (KDWB):
acre = chain * furlong          # consensus.
rood = acre / 4
buildingRod = 33 * yard * yard
bricklayerRod = rod * rod
# US names for certain areas:
section = mile**2
township = 36 * section
homestead = 160 * acre # aka section / 4

# `cubic or solid measure' (KDWB):
timberfoot = foot**3
woodStack = 108 * timberfoot
cord = 128 * timberfoot
housecord = cord / 3
shipTon = 40 * timberfoot		# aka freight ton
shipTimberTon = 42 * timberfoot
displacementTon = 35 * timberfoot
registerTon = 100 * timberfoot		# internal capacity of ships

# Other units of mass: KDWB
butcherStone = Mass(8, pound)
drachmTroy = Mass(3, scruple)	# aka apothecary's dram
ounceTroy = Mass(8, drachmTroy)
denierTroy = Mass(24, grain) # = ounceTroy / 20, the denier (a frankish coin) or pennyweight
poundTroy = Mass(12, ounceTroy)
caratTroy = Mass(3.17, grain)	# or 3.163 grain according to u.s.m.u
carat = Mass(.2, gram)		# metric variant, from /usr/share/misc/units
jpoint = Mass(.01, carat)	# jeweler's point
mercantilePound = Mass(15, ounceTroy)

# Imperial force, power, etc.:
psi = pound.force / inch**2
horsepower = Object(
    550 * foot * pound.force / second,
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
slug = Mass(1, pound.force * second**2 / foot)
slinch = Mass(12, slug)
duty = foot * pound.force

# US units of volume:
USgallon = 3 * 11 * 7 * pow(inch, 3)   # (aka wine gallon) ancient encyclopaedia, K&L
USquart = USgallon / 4
USpint = USquart / 2            # 16.65 UK floz; ? 12, 15, 16 or 20 US ones ?
USgill = USpint / 4
USfloz = USgill / 4		# aka a pony; 1.5 USfloz is aka a jigger
USdram = USfloz / 8
USminim = USdram / 60
USbarrelOil = 42 * USgallon
USfirkin = 9 * USgallon
UShogshead = 7 * USfirkin
USbarrel = UShogshead / 2
USbarrelDry = 7056 * pow(inch, 3) # (7 * 3 * 4)**2 = 7056
USbushel = Object(
    2150.42 * pow(inch, 3), # volume of an 8 inch cylinder with 18.5 inch diameter
    # North Americans also use `bushel' as a unit of mass for grains:
    wheat = Mass(60, pound),
    soybean = Mass(60, pound),
    corn = Mass(56, pound), # of course, this means maize, not wheat
    rye = Mass(56, pound),
    barley = Mass(48, pound),
    oat = Mass(32, pound), # Canada uses 34lb
    rice = Mass(45, pound))
USpeck = USbushel / 4
USgallonDry = USpeck / 2
USquartDry = USgallonDry / 4
USpintDry = USquartDry / 2

# Imperial units of volume (part 1):
gallon = 4.54609 * litre        # 10 * pound / water.density
pint = gallon / 8
floz = fluidOunce = pint / 20
waterTon = 224 * gallon

# The volumes following are from a publican's handbook Nick (landlord of the
# Cambridge Blue) showed me.  These names are particularly subject to
# differences of meaning, both geographic and temporal.
firkin = 9 * gallon
pin = firkin / 2
# but I've met these two names with swapped meanings.
kilderkin = firkin * 2  # aka kil, possibly kill.
barrel = kilderkin * 2  # NHD agrees on barrel = 4 firkin.
hogshead = kilderkin * 3
# Oxford handy dictionary and other sources support a hogshead of 54 gallons.
# KDWB gives it as 63 gallons and I've met other figures, including a `wine
# hogshead' of 56 gallons; see also the US variants.
wine = Object(
    doc = """Winchester measures, for wine.
    (Due to Queen Anne, 1707)""",
    gallon = 231 * inch**3)
wine.also(quart = wine.gallon / 4, pint = wine.gallon / 8,
          rundlet = 18 * wine.gallon, barrel = 63 * wine.gallon / 2,
          tierce = 42 * wine.gallon)
wine.also(hogshead = 2 * wine.barrel, puncheon = 2 * wine.tierce)
wine.also(butt = 2 * wine.hogshead)
wine.also(pipe = wine.butt, tun = 2 * wine.butt)
beer = Object(
    doc = """Archaic measures for beer, pre-1688 and 1803 to 1824.""",
    gallon = 282 * inch**3)
beer.also(quart = beer.gallon / 4, barrel = 36 * beer.gallon)
beer.also(pint = beer.quart / 2, hogshead = 3 * beer.barrel / 2)
ale = Object(
    beer,
    doc = """Archaic measures for ale, 1688 to 1803.""",
    barrel = 34 * beer.gallon,
    hogshead = 51 * beer.gallon)
# and so on ad nauseam !

# Obscure stuff from Kim's dad's 1936 white booklet ...
lienSwiss = 5249 * yard
oncenDutch = Mass(.1, kilogramme)
okeTurk = Mass(2.8342, pound)
berriTurk = 1828 * yard
milDane = 8238 * yard
verstRussia = 1167 * yard
poodRussia = Mass(36.11, pound)

# Imperial units of volume (part 2):

# `fluid measure'
bucket = 4 * gallon
puncheon = 21 * bucket		# or 18 buckets
pipe = 2 * hogshead
tun = 2 * pipe                  # 216 gallons, but I've had 72 suggested.
pottle = gallon / 2             # and a piggin is about 2 gallons
gill = noggin = pint / 4        # gill confirmed by Nick
# but gill = pint / 2 in North England, where noggin is used (KDWB)
cup = pint / 2
teacup = pint / 3
tablespoon = floz / 2 # but 20 cc in metric versions
desertspoon = tablespoon / 2
dram = teaspoon = fluidDrachm = desertspoon / 2
minim = drop = dram / 60

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

_rcs_log = """
 $Log: units.py,v $
 Revision 1.3  2002-02-15 16:03:51  eddy
 Moved Mass/Time bodge to SI, various minor tweaks.

 Revision 1.2  2002/02/11 01:37:00  eddy
 Added Mass and Time to implement .force and .light attributes (respectively).
 Added quid and some comments.  Replaced workyear mess with Job etc.

 Initial Revision 1.1  2001/03/21 20:04:06  eddy
"""
