"""Objects to describe real quantities (with units of measurement).

$Id: quantity.py,v 1.1 1999-01-24 15:04:14 eddy Exp $
"""

def adddict(this, that):
    cop = this.copy()
    cop.update(that)
    result = {}

    for key in cop.keys():
	try: it = this[key]
	except KeyError: it = 0
	try: at = that[key]
	except KeyError: at = 0
	sum = it + at
	# leave out zero entries.
	if sum: result[key] = sum

    return result

def subdict(this, that):
    cop = this.copy()
    cop.update(that)
    result = {}

    for key in cop.keys():
	try: it = this[key]
	except KeyError: it = 0
	try: at = that[key]
	except KeyError: at = 0
	sum = it - at
	# leave out zero entries.
	if sum: result[key] = sum

    return result

def scaledict(dict, scale):
    result = {}

    if scale:
	for key, val in dict.items():
	    if val:
		result[key] = scale * val

    return result

# The multipliers (these are dimensionless)
muldict = {
    # from http://www.weburbia.demon.co.uk/physics/notation.html
    24: 'yotta',
    21: 'zetta',
    18: 'exa',
    15: 'peta',
    12: 'tera',
    9: 'giga',
    6: 'mega',
    3: 'kilo',
    -3: 'milli',
    -6: 'micro',
    -9: 'nano',
    -12: 'pico',
    -15: 'femto',
    -18: 'atto',
    -21: 'zepto',
    -24: 'yocto'
    }
for key, val in muldict.items():
    exec '%s = 1e%d' % (val, key)
hecto= 100
deka = 10
deca = 10
deci = .1
centi= .01


from types import FloatType, IntType
from basEddy.lazy import Lazy

class Value(Lazy):
    def copy(self): apply(self.__class__, (), self.__dict__)
    def also(self, **what): self.__dict__.update(what)
    def __init__(self, *lookups, **what):
	Lazy.__init__(self)
	self.__dict__.update(what)
	row = list(lookups) + [ self._lazy_lookup_ ]
	self.borrow = lambda where, _row = row: _row.append(where)

	def getit(key, _row = row):
	    for item in _row:
		try: return item(key)
		except AttributeError: pass
	    raise AttributeError, key
	self._lazy_lookup_ = getit

import string
_terse_dict = {}

class Quantity(Value):
    def __init__(self, scale, units, doc=None, nom=None, fullname=None, *args, **what):
	"""Initialises an object representing a quantity.

	Arguments:

	  scale -- a scalar, e.g. integer, long or float: it must support
	  addition with and multiplication by at least these types.

	  units -- a dictionary with string keys and integer values, or a
	  Quantity.  Each key names a unit of measurement: the whole dictionary
	  represents the product of pow(key, units[key]) over all keys, so
	  {'kg':1, 'm':2, 's':-2} denotes kg.m.m/s/s, aka the Joule.

	  [doc] -- a documentation string for the quantity (default None).
	  This may alternatively be set by the .document(doc) method.

	  [nom] -- a short name by which to refer to the quantity.
	  This may alternatively be set by the .name(short=nom) method.

	  [fullname] -- a long name (capitalised, if appropriate) for the
	  quantity: as for nom, .name(long=fullname) can be used.

	The first two arguments, scale and units, may be Quantity instances: in
	which case each contributes its scale and units to the new Quantity,
	effectively multiplicatively. """

	if isinstance(units, Quantity):
	    scale = scale * units._scale
	    units = units._units

	if isinstance(scale, Quantity):
	    units = addict(units, scale._units)
	    scale = scale._scale

	apply(Value.__init__, (self,) + args, what)
	# tidy around to what used to be:

	self._scale, self._units = scale, units
	self.__doc__, self._short_name_, self._long_name_ = doc, nom, fullname

    def document(self, doc): self.__doc__ = doc
    def name(self, nom=None, fullname=None):
	if nom: self._short_name_ = nom
	if fullname: self._long_name_ = fullname

    def __cmp__(self, other):
	try:
	    if self._units != other._units:
		raise TypeError, ('comparing quantities of different dimensions',
				  self._unit_repr, other._unit_repr)
	    return cmp(self._scale, other._scale)

	except AttributeError:
	    # other is scalar => dimensionless
	    if self._units:
		raise TypeError, ('comparing scalar to dimensioned quantity',
				  other, self._unit_repr)
	    return cmp(self._scale, other)

    def __nonzero__(self): return 0 != self._scale
    def __float__(self): return float(self._scale)
    def __long__(self): return long(self._scale)
    def __int__(self): return int(self._scale)

    def __neg__(self):
	return Quantity( - self._scale, self._units)

    # Addition, subtraction and their reverses.

    def __add__(self, other):
	try:
	    if self._units != other._units:
		raise TypeError, ('differing dimensions',
				  self._unit_repr, other._unit_repr)

	    return Quantity(self._scale + other._scale,
			    self._units)

	except AttributeError:
	    if self._units:
		raise TypeError, ('adding scalar to dimensioned quantity',
				  other, self._unit_repr)

	    return Quantity(self._scale + other,
			    self._units)

    def __sub__(self, other):
	try:
	    if self._units != other._units:
		raise TypeError, ('differing dimensions',
				  self._unit_repr, other._unit_repr)

	    return Quantity(self._scale - other._scale,
			    self._units)

	except AttributeError:
	    if self._units:
		raise TypeError, ('subtracting scalar from dimensioned quantity',
				  other, self._unit_repr)

	    return Quantity(self._scale - other,
			    self._units)

    def __radd__(self, other):
	try:
	    if self._units != other._units:
		raise TypeError, ('differing dimensions',
				  self._unit_repr, other._unit_repr)

	    return Quantity(other._scale + self._scale,
			    self._units)

	except AttributeError:
	    if self._units:
		raise TypeError, ('adding dimensioned quantity to scalar',
				  other, self._unit_repr)

	    return Quantity(other + self._scale,
			    self._units)

    def __rsub__(self, other):
	try:
	    if self._units != other._units:
		raise TypeError, ('differing dimensions',
				  self._unit_repr, other._unit_repr)

	    return Quantity(other._scale - self._scale,
			    self._units)

	except AttributeError:
	    if self._units:
		raise TypeError, ('subtracting dimensioned quantity from scalar',
				  other, self._unit_repr)

	    return Quantity(other - self._scale,
			    self._units)

    # multiplicative stuff is easier than additive stuff !

    def __mul__(self, other):
	try: return Quantity(self._scale * other._scale,
			     adddict(self._units, other._units))
	except AttributeError:
	    return Quantity(self._scale * other, self._units)
	except TypeError:
	    print 'what is the matter ?', self._units, other._units

    def __rmul__(self, other):
	try: return Quantity(other._scale * self._scale,
			     adddict(other._units, self._units))
	except AttributeError:
	    return Quantity(other * self._scale, self._units)

    def __div__(self, other):
	try: return Quantity(self._scale / other._scale,
			     subdict(self._units, other._units))
	except AttributeError:
	    return Quantity(self._scale / other, self._units)

    def __rdiv__(self, other):
	try: return Quantity(other._scale / self._scale,
			     subdict(other._units, self._units))
	except AttributeError:
	    return Quantity(other / self._scale,
			    subdict({}, self._units))

    def __pow__(self, what):
	try: scale = pow(self._scale, what)
	except OverflowError: scale = pow(float(self._scale), what)
	return Quantity(scale, scaledict(self._units, what))

    def __repr__(self):
	return self._full_repr

    def __str__(self):
	return self._full_str

    # lazy attribute lookups:
    def _lazy_get__full_str_(self, ignored):
	def power(whom, what, many):
	    if many: return '(%s)^%s^' % (whom, what)
	    return '%s^%s^' % (whom, what)
	num = self._number_str
	uni = self.unit_string(exp=power)
	if num and uni: return num + ' ' + uni
	return num or uni or '1'

    def _lazy_get__full_repr_(self, ignored):

	def lookup(row, l=_terse_dict):
	    out = []
	    for nom in row:
		try: out.append(l[nom]._long_name_)
		except KeyError: out.append(nom)
	    return out

	def power(whom, what, many):
	    return 'pow(%s, %s)' % (whom, what)

	return self.unit_string(scale=self._number_repr,
				times='*', Times=' * ',
				divide='/', Divide=' / ',
				lookemup=lookup, exp=power)

    def unit_string(self, scale='',
		    times='.', divide='/',
		    Times=None, Divide=None,
		    lookemup=None, exp=None):
	# Do something smarter when you can see when to say J rather than kg.m.m/s/s
	pows = {}
	for key, val in self._units.items():
	    try: pows[val].append(key)
	    except KeyError: pows[val] = [ key ]
	vals = pows.keys()
	vals.sort()
	vals.reverse()
	# strip down for an optimisation
	for val in vals[:]:
	    if val < 0: break
	    if -val in vals: vals.remove(-val)

	# allow Times='' even with times='.'
	if Times == None: Times = times
	if not Divide: Divide = divide

	if not lookemup:
	    def lookemup(row): return row

	try: result = scale + ''
	except TypeError: result = `scale`

	for p in vals:
	    # punctuate
	    if p > 0:
		if result: result = result + Times
	    elif p < 0: result = result + Divide
	    else: continue

	    row = lookemup(pows[p])
	    lang = len(row)

	    top = string.joinfields(row, times)
	    # do the bits just stripped
	    try: wor = lookemup(pows[-p])
	    except KeyError: bot = ''
	    else:
		if wor:
		    bot = divide + string.joinfields(wor, divide)
		    lang = lang + len(wor)

	    # only invoke pow if abs(power) > 1.
	    p = abs(p)
	    if p == 1: more = top + bot
	    elif exp: more = exp(top + bot, p, lang)
	    else: more = string.joinfields([top] * p, times) + bot * p
	    result = result + more

	return result

    def _lazy_get__lazy_hash_(self, ignored):
	h = hash(self._scale)
	for k, v in self._units.items():
	    h = h ^ v ^ hash(k)

	return h

    def _lazy_get__number_repr_(self, ignored):
	tip = type(self._scale)
	if tip == IntType and self._scale == 1: return ''
	if tip != FloatType:
	    # have a go at rendering it as an integer
	    try: iscale = int(self._scale)
	    except OverflowError: iscale = long(self._scale)
	    if self._scale == iscale: return `iscale`

	return `self._scale`

    def _lazy_get__number_str_(self, ignored):
	"""Coerce the number to my preferred form.

	This has the bit before the e as a number between .1 and 100.

	"""
	if type(self._scale) != FloatType: return self._number_repr

	mant, expo = string.splitfields('%e' % self._scale, 'e')
	hol, par = string.splitfields(mant, '.')
	pone = string.atoi(expo)

	# make the exponent a multiple of 3:
	if pone % 3 == 1:
	    if len(hol) < 3: hol, par, pone = hol + par[:1], par[1:], pone - 1
	    else: hol, par, pone = hol[:-2], hol[-2:] + par, pone + 2
	elif pone % 3 == 2:
	    if len(hol) > 0: hol, par, pone = hol[:-1], hol[-1:] + par, pone + 1
	    else: hol, par, pone = hol + par[:2], par[2:], pone - 2
	elif pone % 3:
	    raise ArithmeticError, 'python remainder glitch: %d %% 3 is %d' % (
		pone, pone % 3)

	while par[-1:] == '0': par = par[:-1]
	mant = hol + '.' + par
	if pone == 0: return mant
	try: mul = muldict[pone]
	except KeyError: return mant + 'e%+d' % pone
	return mant + ' ' + mul

# Some handy constants in SI units

def _base_unit(nom, fullname, doc):
    result = Quantity(1, {nom:1}, doc, nom, fullname)
    _terse_dict[nom] = result
    return result

# The base units
m = metre = _base_unit('m', 'metre',
		     """The SI unit of length.

1650763.73 wavelengths in vacuum of the radiation corresponding to the
transition 2p_10_-5d_s_ of the krypton-86 atom (in the spectroscopists' notation
for spectral lines).""")

s = second = _base_unit('s', 'second',
		      """The SI unit of time.

9192631770 periods of the radiation corresponding to the transition between
the two hyperfine levels of the ground state of  the caesium-133 atom.""")

kg = kilogramme = _base_unit('kg', 'kilogramme',
			   """The SI unit of mass.

The mass of the <EM>International Prototype</EM> kilogramme (a platinum-iridium
cylinder) kept in the Bureau International des Poids et Mesures (BIPM), S&egrave;vres,
Paris.""")

A = Ampere = _base_unit('A', 'Ampere',
		      """The SI unit of electric current.

That constant current which, if maintained in two parallel rectilinear
conductors, of immense length and negligible circular cross-section, placed 1
metre apart in a vacuum, would produce a force between these conductors equal to
2e-7 newton per meter of length.""")

K = Kelvin = _base_unit('K', 'Kelvin',
		      """The SI unit of temperature.

The fraction 1/273.16 (exactly) of the thermodynamic temperature at the triple
point of water. """)

mol = _base_unit('mol', 'Mole',
	       """The SI unit of `amount of substance'.

The amount of substance which contains as many elementary units as there are
atoms in 12e-3 kilogrammes (exactly) of pure carbon-12.  The elementary unit
must be specified and may be atom, molecule, ion, radical, electron, photon
<I>etc</I>. or collection of elementary units. """)

cd = candela = _base_unit('cd', 'Candela',
			"""The SI unit of luminous intensity.

The luminous intensity, in the perpendicular direction, of a surface of 1/60
square centimetere of a black body at the freezing temperature of platinum under
a pressure of 101325 Pascal (1 atmosphere).""")

rad = radian = _base_unit('rad', 'Radian',
			"""The SI unit of angle.

The angle subtended at the centre of a circle by an arc of the circumference
equal in length to the radius of the circle.""")

sr = steradian = _base_unit('sr', 'Steradian',
			  """The SI unit of solid angle.

The unit of solid angle is the solid angle subtended at the center of a sphere
of radius r by a portion of the surface of the sphere having area r*r.""")

_dimension_dict = _terse_dict.copy()
# Composite SI units
N = Newton = kilogramme * metre / second / second	# Force
Hz = Hertz = 1 / second		# Frequency
C = Coulomb = Ampere * second	# Charge
J = Joule = Newton * metre	# Energy
W = Watt = Joule / second	# Power
Pa = Pascal = Newton / metre / metre	# Pressure
lm = lumen = candela * steradian	# Luminous flux
lx = lux = lumen / metre / metre	# Illumination
litre = milli * pow(metre, 3)

V = Volt = Joule / Coulomb	# Electromagnetic potential
Wb = Weber = Joule / Ampere	# Magnetic flux
Ohm = Volt / Ampere		# Electrical resistance
S = Siemens = 1 / Ohm		# Conductance
F = Farad = second / Ohm	# Capacitance
H = Henry = Weber / Ampere	# Inductance
T = Tesla = Weber / metre / metre	# Magnetic flux density

# Derived units
minute = 60 * second
hour = 60 * minute
day = 24 * hour
week = 7 * day
year = (365 + 1/ (4 + 1/(25 + 1/4.))) * day	# Gregorian

cm = centi * metre
# Do not define g = gram: reserve g for EarthSurfaceGravity
gram = milli * kilogramme
cc = pow(cm, 3)


# (UK) Imperial units converted to SI
inch = 2.54e-2 * metre
ft = foot = 12 * inch
yard = 3 * foot
# chain
# furlong 
# 1760 = 20 * 88 = 22 * 80 = 32 * 5 * 11
mile = 1760 * yard
nauticalMile = 2000 * yard
# or a minute of arc.
# EarthRadius * pi / mile / 180 / 60 is a little over one mile:
# 2026 yards, to the precision (such as it is) that I know.

# NB: I don't know if the US oz is the same as the UK one
lb = pound = .45359237 * kilogramme
oz = ounce = pound / 16
stone = 14 * pound

# NB: the US floz et al. are different from the UK ones
gallon = 4.54609 * litre
pint = gallon / 8
floz = pint / 20

calorie = 4.184 * Joule	# the thermodynamic calorie, not any other sort.
BTU = BritishThermalUnit = 1.05506 * kilo * Joule
horsepower = 745.7 * Watt

Atmosphere = Quantity(.101325, mega * Pascal,
		      """Standard Atmospheric Pressure""")

_rcs_log = """
$Log: quantity.py,v $
Revision 1.1  1999-01-24 15:04:14  eddy
Initial revision

"""
