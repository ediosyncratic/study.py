"""Assorted units of measurement.

$Id: units.py,v 1.1 2001-03-21 20:04:06 eddy Exp $
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
    perfectream = 516)
paper.also(bundle = 2 * paper.ream, bale = 10 * paper.ream)

minute = 60 * second
hour = 60 * minute
day = 24 * hour
week = 7 * day
fortnight = 2 * week
year = 27 * 773 * (week / 400)   	# the Gregorian approximation
# factors of 216 seconds abound ...
# My working year has 8 bank holidays + 25 my holidays and
# I work 5-day weeks, nominally 7.5 hours per day.
workyear = (year * 5. / week - 33) * 7.5 * hour
# how about lunar months ? how do they relate to all this ? e.g. 216 secs ?

turn = revolution = 2 * pi * radian
arc = Object(degree = turn / 360)
arc.minute = arc.degree / 60
arc.second = arc.minute / 60
erg = .1 * micro * Joule
dyn = 10 * micro * Newton

Ci = Curie = 37 * giga * Becquerel
R = Rontgen = .258 * milli * Coulomb / kilogramme       # R&ouml;ntgen
Oe = Oersted = kilo * Ampere / metre / 4 / pi
eV = 160.21e-21 * Coulomb * Volt # electron-Volt, .16 atto Joules

Rankine = Kelvin / 1.8          # steps in the Fahrenheit scale
tonne = kilo * kilogramme
slug = 14.5939 * kilogramme
kilogramforce = 9.80665 * Newton
calorie = 4.1868 * Joule	# the thermodynamic calorie (IT), not any other sort.
# food talks about `calorie' but means kilo calorie
BTU = Btu = BritishThermalUnit = 1.05506 * kilo * Joule
# calorie * pound * Farenheit / gram / Kelvin
CHU = 1.8 * BTU         # whatever CHU is ! (KDWB)
chevalVapeur = 75 * kilogramforce * metre / second
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

import string
# Description of bytes, including the kilo = 1024 twist ...
from quantity import _quantifier_dictionary
# strictly, I should filter keys() for multiples of 3, or scale its keys by 3 ...
_biggest = max(_quantifier_dictionary.keys()) / 3

class bSample(qSample):
    def _sample_decade_split(self, power=1):
        raise AssertionError, 'I thought this was defunct !'
        off = self.decade(pow(1024., power), 100 * pow(8., power))
        # problem - e.g. range for 1/byte is 8/10.24 to 800
        if 1 > off: return 1, ''
        # don't use an unfamiliar prefix unless it'll work ...
        if off > _biggest: off = 4

        try: mul = _quantifier_dictionary[3 * off]
        except KeyError: return 1, ''

        try: return pow(1024, off), mul
        except OverflowError: return pow(1024L, off), mul

class bQuantity(Quantity):
    def _quantity(self, what, units):
        try: order = units['bit']
        except KeyError: order = 0
        if order: return self.__class__(what, units)
        # if we have no byte-ness left, shed sophistication ...
        if isinstance(what, bSample): what = qSample(what)
        return Quantity(what, units)

    def _lazy_get__unit_str_(self, key):
        order = self._unit_order('bit')
        factor, mul = self._quantade_split_(order)

        if factor < 1: factor, mul = 1, ''
        tail = self / pow(factor * byte, order)
        tail = tail._primitive()
        # assert: tail isn't a bQuantity and its scale isn't a bSample.
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
byte = bQuantity(bSample(mean=8), # but no actual sample
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

_row = filter(lambda x: x > 0, _quantifier_dictionary.keys())
_row.sort()
for _key in _row:
    _nom = '%sbyte' % _quantifier_dictionary[_key]
    try: exec '%s = Quantity(1024, %s, nom="%s")' % (_nom, _name, _nom)
    except OverflowError:
        # assert: _nom is terabyte
        exec '%s = Quantity(1024L, %s, nom="%s")' % (_nom, _name, _nom)
    _name = _nom

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
grain = 64.79891e-6 * kilogramme        # K&L
mite = grain / 20
droit = mite / 24
periot = droit / 20
blanc = periot / 24
scruple = 20 * grain
lb = pound = 350 * scruple
oz = ounce = pound / 16
dram = ounce / 16
clove = 7 * pound
stone = 2 * clove
cwt = hundredweight = 8 * stone
ton = 20 * cwt
UScwt = cental = 100 * pound
USton = 20 * UScwt
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
butcherStone = 8 * pound
drachmTroy = 3 * scruple	# aka apothecary's dram
ounceTroy = 8 * drachmTroy
denierTroy = 24 * grain # = ounceTroy / 20, the denier (a frankish coin) or pennyweight
poundTroy = 12 * ounceTroy
caratTroy = 3.17 * grain	# or 3.163 grain according to u.s.m.u
carat = .2 * gram		# metric variant, from /usr/share/misc/units
jpoint = carat / 100		# jeweler's point
mercantilePound = 15 * ounceTroy

# Imperial force and power:
poundforce = kilogramforce * pound / kilogramme
horsepower = Object(
    550 * foot * poundforce / second,
    metric = 75 * kilogramforce * metre / second,
    electric = 746 * Watt,
    boiler = 9809.50 * Watt,
    water = 746.043 * Watt,
    donkey = 250 * Watt) 
celo = foot / second**2
jerk = celo / second
poundal = pound * celo
reyn = poundforce * second / inch**2
slug = poundforce * second**2 / foot
slinch = slug * 12
duty = foot * poundforce

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
    wheat = 60 * pound,
    soybean = 60 * pound,
    corn = 56 * pound, # of course, this means maize, not wheat
    rye = 56 * pound,
    barley = 48 * pound,
    oat = 32 * pound, # Canada uses 34lb
    rice = 45 * pound)
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
oncenDutch = kilogramme / 10
okeTurk = 2.8342 * pound
berriTurk = 1828 * yard
milDane = 8238 * yard
verstRussia = 1167 * yard
poodRussia = 36.11 * pound

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
tablespoon = floz / 2
desertspoon = tablespoon / 2
dram = teaspoon = fluidDrachm = desertspoon / 2
minim = drop = dram / 60

# `dry measure'
peck = gallon * 2
bushel = peck * 4       # but Nick thought a peck was half a bushel ...
# average weights of bushels: barley = 47 lb, oats = 38 lb, wheat = 60 lb; c.f. USbushel
strike = 2 * bushel
coomb = bag = 2 * strike
seam = 2 * bag
wey = load = 5 * seam 	# a `last' is 1 or 2 of these ...
sack = 3 * bushel
firlot = sack / 2
boll = 2 * sack
# Quarter = 2 * coomb = 64 * gallon
chaldron = 12 * sack
cran = 75 * gallon / 2	# measures herring - c. 750 fish

"""
  $Log: units.py,v $
  Revision 1.1  2001-03-21 20:04:06  eddy
  Initial revision

"""
