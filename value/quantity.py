"""Objects to describe real quantities (with units of measurement).

$Id: quantity.py,v 1.5 1999-07-19 21:17:32 eddy Exp $
"""

# The multipliers (these are dimensionless)
_quantifier_dictionary = {
    # see quantifiers in the New Hackers' Dictionary
    30: 'grouchi',
    27: 'harpi',
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
    -24: 'yocto',
    # back to quantifiers (suggested by Morgan Burke, probably unratified ;^)
    -27: 'harpo',
    -30: 'groucho',
    # also: zeppo, gummo, chico ?
    }

_exponent_to_quantifier = {}
for _key, _val in _quantifier_dictionary.items():
    exec '%s = 1e%d' % (_val, _key)
    _exponent_to_quantifier['e%+d' % _key] = ' ' + _val
hecto= 100
deca = deka = 10
deci = .1
centi= .01

from basEddy.sample import Sample
import string
class qSample(Sample):
    def __init__(self, sample=(), *args, **what):
        if isinstance(sample, Sample):
            what.update(sample.dir)
            sample = sample.mirror

        apply(Sample.__init__, (self, sample, best) + args, what)

    def _lazy_get__str_(self, ignored):
        ans = Sample._lazy_get__str_(self, ignored)
        if ans[:1] in '-+': sign, ans = ans[:1], ans[1:]
        else: sign = ''

        try: head, tail = string.split(ans, 'e')
        except ValueError:
            head, tail = string.split(ans, 'E')
            glue = 'E'
        else: glue = 'e'

        exponent = string.atoi(tail)
        try: up, down = string.split(head, '.')
        except ValueError: up, down = head, ''

        if exponent % 3 in (-2, 1):
            exponent = exponent - 1
            if down: head = up + down[:1] + '.' + down[1:]
            else: head = up + '0'
        elif exponent % 3 in (-1, 2):
            exponent = exponent + 1
            if up: head = up[:-1] + '.' + up[-1:] + down
            else: head = '.0' + down

        if exponent:
            tail = 'e' + `exponent`
            try: mul = _exponent_to_quantifier[tail]
            except KeyError: tail = glue + `exponent`
            else: tail = mul
        else:
            tail = ''

        return sign + head + tail

def adddict(this, that):
    cop = this.copy()
    cop.update(that)
    cop = cop.keys()
    result = {}

    for key in cop:
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
    cop = cop.keys()
    result = {}

    for key in cop:
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

    # Raising to power zero yields dimensionless.
    if scale:
        # Try to avoid fancy numbers as exponents for units ...
        try: scale = scale.median
        except AttributeError: pass

	for key, val in dict.items():
	    if val:
		result[key] = scale * val

    return result

from basEddy.value import Value
_terse_dict = {}

class Quantity(Value):
    def __init__(self, scale, units={},
		 doc=None, nom=None, fullname=None,
		 sample=None,
		 *args, **what):
	"""Initialises an object representing a quantity.

	Arguments:

	  scale -- a scalar, e.g. integer, long or float: it must support
	  addition with and multiplication by at least these types.

	  [units] -- a dictionary with string keys and integer values, or a
	  Quantity.  Each key names a unit of measurement: the whole dictionary
	  represents the product of pow(key, units[key]) over all keys, so
	  {'kg':1, 'm':2, 's':-2} denotes kg.m.m/s/s, aka the Joule.  If
	  omitted, an empty dictionary is used (indicating a dimensionless
	  quantity).

	  [doc] -- a documentation string for the quantity (default None).
	  This may alternatively be set by the .document(doc) method.

	  [nom] -- a short name by which to refer to the quantity.
	  This may alternatively be set by the .name(short=nom) method.

	  [fullname] -- a long name (capitalised, if appropriate) for the
	  quantity: as for nom, .name(long=fullname) can be used. [These two
	  arguments act as fall-backs for one another: if you give either of
	  them, it serves as the default for the other.]

	  [sample] -- a sequence of quantities which should be equal to this
	  one.

	The first two arguments, scale and units, may be Quantity instances: in
	which case each contributes its scale and units to the new Quantity,
	effectively multiplicatively. """

        # Initialise self as a Value:
	apply(Value.__init__, (self,) + args, what)

        # massage the arguments: first mix scale and units
	if isinstance(units, Quantity):
	    scale, units = scale * units.__scale, units.__units

	if isinstance(scale, Quantity):
	    units, scale = adddict(units, scale.__units), scale.__scale

        # massaging scale as a sample (so we can trust its str() to work).
        if not isinstance(scale, Sample): scale = qSample(best=scale)

        # then (check and) massage sample:
	if sample:
	    row = []

	    for val in sample:
		if val.__units != units:
		    raise TypeError, ('Sample of wrong dimensions', val, units)
		else: row.append(val.__scale)

	    if row:
		try: new = apply(scale.update, row)
		except (TypeError, AttributeError):
                    new = qSample(row + [ scale ])

		scale = new
            # else assert oops !

        # initialise self as a Quantity with the thus-massaged arguments
	self.__scale, self.__units, self.__doc__ = scale, units, doc
        self.name(nom or fullname, fullname or nom)

    def _primitive(self):
        """Returns a quantity in a primitive form.

        The primitive form doesn't try to use quantifiers when printing itself,
        or other fancy stuff.  This is intended for use by derived classes when
        sorting out their representations ... """

        what, units = self.__scale, self.__units
        if isinstance(what, qSample):
            what = apply(Sample, (what.mirror,), what.dir)
        return Quantity(what, units)

    def document(self, doc): self.__doc__ = doc
    def name(self, nom=None, fullname=None):
	if nom: self._short_name_ = nom
	if fullname: self._long_name_ = fullname

    def observe(self, what):
        self.__scale = self.__scale.update(self.__addcheck_(what, 'observe'))

    def __cmp__(self, other):
	return cmp(self.__scale, self.__addcheck_(other, 'compare'))

    def __nonzero__(self): return 0 != self.__scale
    def __float__(self): return float(self.__scale)
    def __long__(self): return long(self.__scale)
    def __int__(self): return int(self.__scale)

    def __repr__(self):return self._full_repr
    def __str__(self): return self._full_str

    def __neg__(self):
	return self._quantity( - self.__scale, self.__units)

    # Support for additive functionality:
    def __addcheck_(self, other, why):
        """Checks for additive compatibility and unpacks.

        Arguments:

          other -- another quantity, which should have the same dimensions as
          self; may be a scalar iff self is dimensionless.

          why -- string, e.g. '+' or 'compare', describing what caller is doing:
          used in raising errors.

        Returns other's scalar aspect or raises a TypeError. """

	try:
	    if self.__units != other.__units:
		raise TypeError, (why + ' with differing dimensions',
				  self._unit_str, other._unit_str)

	    return other.__scale

	except AttributeError:
	    if self.__units:
		raise TypeError, (why + ' between scalar and dimensioned quantity',
				  other, self._unit_str)

	    return other

    def __kin(self, scale):
        return self._quantity(scale, self.__units)

    # Addition, subtraction and their reverses.
    def __add__(self, other):
	return self.__kin(self.__scale + self.__addcheck_(other, '+'))

    def __radd__(self, other):
	return self.__kin(self.__addcheck_(other, '+') + self.__scale)

    def __sub__(self, other):
	return self.__kin(self.__scale - self.__addcheck_(other, '-'))

    def __rsub__(self, other):
	return self.__kin(self.__addcheck_(other, '-') - self.__scale)

    # multiplicative stuff is easier than additive stuff !
    def __unpack_(self, other):
	if isinstance(other, Quantity):
	    return other.__scale, other.__units
	return other, {}

    def __mul__(self, other):
	ot, her = self.__unpack_(other)
	return self._quantity(self.__scale * ot, adddict(self.__units, her))

    def __rmul__(self, other):
	ot, her = self.__unpack_(other)
	return self._quantity(ot * self.__scale, adddict(her, self.__units))

    def __div__(self, other): 
	ot, her = self.__unpack_(other)
	return self._quantity(self.__scale / ot, subdict(self.__units, her))

    def __rdiv__(self, other):
	ot, her = self.__unpack_(other)
	return self._quantity(ot / self.__scale, subdict(her, self.__units))

    def __pow__(self, what):
	wh, at = self.__unpack_(what)
	if at: raise TypeError, ('raising to a dimensioned power', what)

	return self._quantity(pow(self.__scale, wh),
                              scaledict(self.__units, wh))

    # lazy attribute lookups:
    def _lazy_get_accuracy_(self, ignored):
        return self.__scale.spread / self.__scale.mean

    # lazy string and representation lookups:
    def _lazy_get__unit_str_(self, ignored):
	def power(whom, what, many):
	    if many: whom = '(%s)' % whom
	    return '%s^%s^' % (whom, what)
	return self.unit_string(exp=power)

    def _lazy_get__number_str_(self, ignored):
        return str(self.__scale) or '?'

    def _lazy_get__full_str_(self, ignored):
        uni, num = self._unit_str, self._number_str
        if uni[:1] == '/': return num + uni
        if not uni: return num

        try: num = { '1': '', '-1': '-' }[num]

        except KeyError:
            if num[-1] in '. \t\n\f': pad = ''
            elif '.' in num: pad = ' '
            elif num[-1] in '0123456789': pad = '.'
            else: pad = ' '

        else: pad = ''

        return num + pad + uni

    def _lazy_get__number_repr_(self, ignored):
	return `self.__scale`

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
	"""Generates representations of a quantity.

	All arguments are optional and should be given by name when given.

	  scale -- a prefix string to which to join the unit representation
	  built by this routine: in particular, if given, it will be joined to
	  this unit representation by a suitable times or divide operator.  If a
	  non-string is given for scale, its repr() is used.

	  times, divide -- strings, default '.' and '/', to be used to denote
	  multiplication and division in compact texts, e.g. kg.m/s.

	  Times, Divide -- strings, defaulting to times and divide, to be used
	  to denote multiplication and division in spread-out texts,
	  e.g. kg.m / s^2^.

	  lookemup -- a function taking a sequence of strings and returning a
	  similar sequence in which each string may have been replaced with an
	  alternative: typically, the strings in the input list will be short
	  names of units, to be converted to long names where known,
	  e.g. [ kg ] -> [ kilogramme ].

	  exp -- a function to denote exponentiation, takes arguments (whom,
	  what, many), where whom is a unit-string, what is the exponent, exp's
	  result must denote pow(whom, what) and many is a flag indicating
	  whether whom is a single term or a compound: eg exp('m/s', 2, true) ->
	  (m/s)^2^ but exp('m', 2, false) -> m^2^.  If omitted, repetition will
	  be used (so, where exp('m/s', 2, true) would have been called, m.m/s/s
	  gets used).  This function is never called with what in (-1, 0, 1).

	I hope to be able to do something smarter when I can see when to say J
	rather than kg.m.m/s/s, and etc.  But that will probably involve
	creating a units base-class to replace the present __units dictionary.
	"""
	pows = {}
	for key, val in self.__units.items():
	    try: pows[val].append(key)
	    except KeyError: pows[val] = [ key ]

	# Prepare the list of powers we'll be using:
	vals = pows.keys()
	vals.sort()
	vals.reverse()
	if 0 in vals: vals.remove(0)
	for val in vals[:]:
	    if val < 0: break
	    # We'll be folding x^-i^ terms in with y^i^ as (y/x)^i^ ...
	    if -val in vals: vals.remove(-val)

	# allow Times='' even with times='.'
	if Times is None: Times = times
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
	    else: continue	# never happens - 0 got stripped

	    row = lookemup(pows[p])
	    lang, top, bot = len(row), string.joinfields(row, times), ''

	    # ... do the promised folding:
	    try: wor = lookemup(pows[-p])
	    except KeyError: pass
	    else:
		if wor:
		    bot = divide + string.joinfields(wor, divide)
		    lang = lang + len(wor)

	    # only invoke pow if abs(power) > 1.
	    p = abs(p)
            try:
                # if we can represent p as an integer ...
                try: ip = int(p)
                except OverflowError: ip = long(p)
            except (AttributeError, ValueError): pass
            else:
                # ... use that by preference !
                if ip == p: p = ip

	    if p == 1: more = top + bot
	    elif exp: more = exp(top + bot, p, lang > 1)
	    else: more = string.joinfields([top] * p, times) + bot * p
	    result = result + more

	return result

    def _lazy_get__lazy_hash_(self, ignored):
	h = hash(self.__scale)
	for k, v in self.__units.items():
	    h = h ^ v ^ hash(k)

	return h

    # Methods for use by derived classes and kindred allies:
    def _quantade_split_(self, power=1):
        raise AssertionError, 'retired functionality'

    def _unit_order(self, unit):
        try: return self.__units[unit]
        except KeyError: return 0

    # override this if needed in derived classes ...
    def _quantity(self, what, units): return self.__class__(what, units)

def base_unit(nom, fullname, doc):
    result = Quantity(1, {nom:1}, doc, nom, fullname)
    _terse_dict[nom] = result
    return result

_rcs_log = """
 $Log: quantity.py,v $
 Revision 1.5  1999-07-19 21:17:32  eddy
 Extracted units into other files (units, SI, unit).
 Switched to new Sample class, in peer module sample.
 Mended file after disk-mess.

 Revision 1.4  1999/05/07 17:22:13  eddy
 Several things inspired by the New Hackers' Dictionary, some other bits
 and pieces: now I'm going to lug data out of Kaye & Laby ...

 Revision 1.3  1999/02/21 13:41:54  eddy
 Ditched tolerance, moved to sample-form, simplified numeric side.
 Tidied up __*__ routines, maintenance.

 Revision 1.2  1999/02/21 01:30:23  eddy
 Evolution.

 Initial Revision 1.1  1999/01/24 15:04:14  eddy
"""
