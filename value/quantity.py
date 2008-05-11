"""Objects to describe real quantities (with units of measurement).

$Id: quantity.py,v 1.49 2008-05-11 15:39:45 eddy Exp $
"""

# The multipliers (these are dimensionless) - also used by units.py
# http://www.bipm.fr/enus/3_SI/si-prefixes.html
# http://physics.nist.gov/cuu/Units/prefixes.html
_quantifier_dictionary = {
    # <q src="New Hackers' Dictionary">
    30: 'grouchi',
    27: 'harpi',
    # <q src="http://www.weburbia.demon.co.uk/physics/notation.html">
    24: 'yotta',
    21: 'zetta',
    # <q src="Kaye & Laby">
    18: 'exa',
    15: 'peta',
    12: 'tera',
    9: 'giga',
    6: 'mega',
    3: 'kilo', # k
    2: 'hecto', # h
    1: 'deca', # a.k.a. deka; see below
    -1: 'deci',
    -2: 'centi',
    -3: 'milli',
    -6: 'micro',
    -9: 'nano',
    -12: 'pico',
    -15: 'femto',
    -18: 'atto', # </q>
    -21: 'zepto',
    -24: 'yocto', # </q>; last two suggested by Morgan Burke (probably unratified ;^)
    -27: 'harpo',
    -30: 'groucho' # also: zeppo, gummo, chico ? </q>
    }
# Aside from kilo and hecto, each positive-power quantifier abbreviates to its
# upper-case first letter; each negative entry abbreviates to its lower-case
# first letter.  This rule clearly can't be extended to harpo/harpi and
# groucho/grouchi, which might militate against their formal adoption !

_exponent_to_quantifier = {} # needed for qSample._repr
for _key, _val in _quantifier_dictionary.items():
    exec '%s = 1e%d' % (_val, _key)
    _exponent_to_quantifier['e%+d' % _key] = _val

deka = deca

# IEC 60027-2
# http://physics.nist.gov/cuu/Units/binary.html
_pow1024_dictionary = {
    1: 'kibi',
    2: 'mebi',
    3: 'gibi',
    4: 'tebi',
    5: 'pebi',
    6: 'exbi',
    7: 'zebi',
    8: 'yobi',
    # postulated: 9: 'habi', 10: 'grubi'
    }
for _key, _val in _pow1024_dictionary.items():
    exec '%s = 1024 ** %d' % (_val, _key)

Ki, Mi, Gi = kibi, mebi, gibi

# Note: a gram comes out as a milli * kilogramme, which is only fair, all things
# considered.  Maybe I'll fix it some day ... but will someone object to mega *
# gramme not using an SI base unit ?  I seem to be obliged to work in kg as base
# unit but use gramme as base for naming of masses below 1e-3 kg and tonne for
# naming of masses above 1e3 kg.  How silly is that ?

def massage_text(text, times,
                 _e2q=_exponent_to_quantifier):
    """Computes the representation of qSample.

    Arguments (both required, none more allowed):
      text -- preliminary representation of a number
      times -- the text to put between a number and a quantifier to serve as
               `multiplication' in the resulting string.

    Result has form [sign]digits[.digits][quantifier] where: quantifier may be,
    for instance, times + 'mega', meaning the appropriate power of 10, else it
    is explicitly [e[sign]digits], e.g. 'e6', an exponent; in which case the
    exponent is a multiple of three.  The mantissa, digits[.digits], is always
    in the range .1 to 1000 and only exceeds 100 if at least three significant
    digits are available and, by so doing, we can avoid the need for a
    [quantifier].  If the exponent is given as [e[sign]digits] and the
    representation is exact, the e will be an E, e.g. the integer 4000 is 4E3.
    Exact numbers will also elide their decimal point if it is the last
    character of the mantissa, rough ones are less likely to.\n"""

    # Extract the sign, if present, and set it aside; we'll put it back as we return
    if text[:1] in '-+': sign, text = text[:1], text[1:]
    else: sign = ''

    # Snip apart mantissa (head) and exponent (tail):
    glue = 'e'
    try: head, tail = text.split('e')
    except ValueError:
        try: head, tail = text.split('E')
        except ValueError: head, tail = text, '0'
        else: glue = 'E' # value is exact

    # Decode (string) tail as an (integer) exponent:
    exponent = int(tail)
    # Snip apart the mantissa (head) at the dot (if any):
    try: up, down = head.split('.')
    except ValueError: up, down = head, ''
    # up is the whole part, down the fractional part

    # Roll the dot left or right to make exponent a multiple of 3:
    if exponent % 3 in (-2, 1):
        exponent = exponent - 1
        if up == '0': up = ''
        if down: head = up + down[:1] + '.' + down[1:]
        else: head = up + '0'
    elif exponent % 3 in (-1, 2):
        exponent = exponent + 1
        if up: head = up[:-1] + '.' + up[-1:] + down
        else: head = '.0' + down

    # Now, about the exception for 400 rather than .400e3 (but .40e3 is .40 * kilo)
    if exponent == 3 and head[0] == '.':
        if len(head) > 3:
            # head is '.ddd' or longer with 'e3' to follow
            head = head[1:4] + '.' + head[4:]
            exponent = 0
        # if value is exact, we can treat implicit trailing zeros as significant ...
        elif glue == 'E':
            # head is '.dd' or shorter with 'E3' to follow
            head = head[1:] + '0' * (4 - len(head)) # + '.' elided; about to be ditched.
            exponent = 0

    # Ditch trailing '.' if value is exact:
    if head[-1] == '.' and glue == 'E': head = head[:-1]

    # Finally, transform 'e[sign]prial' into a name, if we have one for it:
    # e.g. 'e6' -> ' mega'
    if exponent:
        tail = 'e%+d' % exponent
        try: mul = _e2q[tail]
        except KeyError: tail = glue + `exponent`
        else: tail = times + mul
    else:
        tail = ''

    return sign + head + tail

del _exponent_to_quantifier
from sample import Sample

class qSample (Sample):
    # Massage Sample's answers, which use any integer for the exponent
    # and put digits.[digits] between 1 and 10.
    _lazy_get__sample_repr_ = Sample._lazy_get__repr_
    def _lazy_get__repr_(self, ignored, mash=massage_text):
        return mash(self._sample_repr, ' * ')
    def _lazy_get__str_(self, ignored, mash=massage_text):
        return mash(self._sample_repr, ' ')

    # Sample's .low and .high are boundary weights, decidedly *inside* true bounds.
    def _lazy_get_high_(self, ignored): return self.span[1]
    def _lazy_get_low_(self, ignored): return self.span[0]

del massage_text

def indent(line, tabwidth=8):
    dent = i = 0
    while line[i].isspace():
        if line[i] == '\t': dent = (1 + dent // tabwidth) * tabwidth
        elif line[i] == ' ': dent += 1
        # else: erm ... try not to think about it.
        i += 1
    return dent

def _cleandoc(text, dent=indent):
    if text:
        text = text.strip()

        if '\n' in text:
            # Remove any ubiquitous common indent (and any dangling hspace):
            lines = map(lambda x: x.rstrip(), text.split('\n'))
            ind, i = min(map(dent, filter(None, lines[1:]))), 1
            while i < len(lines):
                # Canonicalize surviving indentation to spaces while we're at it.
                if lines[i]: lines[i] = ' ' * (dent(lines[i]) - ind) + lines[i].lstrip()
                i += 1
            text = '\n'.join(lines)

        if text: return text + '\n'
    # else: implicitly return None.

del indent

# lazy-evaluators for special attributes of quantities of specific types
def scalar():
    import cmath, math
    from SI import radian, second

    def chose(val, s, c):
        try: return s(val)
        except (ValueError, TypeError):
            return c(val)

    def exp(val, xp=math.exp, px=cmath.exp, s=chose): return s(val, xp, px)
    def ln(val, lg=math.log, gl=cmath.log, s=chose): return s(val, lg, gl)
    def arccos(val, ac=math.acos, ca=cmath.acos, s=chose): return s(val, ac, ca)
    def arcsin(val, as=math.asin, sa=cmath.asin, s=chose): return s(val, as, sa)
    def arcsec(val, ac=arccos): return ac(1./val)
    def arccosec(val, as=arcsin): return as(1./val)

    def sinc(val, s=math.sin, x=cmath.sin, c=chose):
        try: return c(val, s, c) / val
        except ZeroDivisionError: return 1 # sinc(0) = 1
        # and there's a reason why I'm not trying to offer asinc !
        # min sinc is c. -.217 at pi * 1.43 ~ x = tan(x) ~ 4.5

    def Shannon(val, ln=math.log, ln2=math.log(2)):
        if val > 0: return val * ln(val) / ln2
        return 0
    # NB: for a distribution among N things, information content is sum(Shannon)
    # and redundancy is 1 - sum(Shannon)/N.

    def simple(val):
        if val.imag + 1 == 1: return val.real
        return val

    def arccosh(val, ac=cmath.acosh, s=simple): return s(ac(val))
    def arcsinh(val, as=cmath.asinh, s=simple): return s(as(val))
    def arctanh(val, at=cmath.atanh, s=simple): return s(at(val))

    return { 'arcCos': lambda v, a=arccos, r=radian: r * v.evaluate(a),
             'arcSin': lambda v, a=arcsin, r=radian: r * v.evaluate(a),
             'arcTan': lambda v, a=math.atan, r=radian: r * v.evaluate(a),
             'arcCoTan': lambda v, a=math.atan, r=radian, q=math.pi / 4: r * (q - v.evaluate(a)),
             'arcSec': lambda v, a=arccos, r=radian: r * v.evaluate(lambda x, f=a: f(1./x)),
             'arcCoSec': lambda v, a=math.asin, r=radian: r * v.evaluate(lambda x, f=a: f(1./x)),
             'exp': lambda v, e=exp: v.evaluate(e),
             'log': lambda v, l=ln: v.evaluate(l),
             'Shannon': lambda v, s=Shannon: v.evaluate(s),
             'arccosh': lambda v, a=arccosh: v.evaluate(a),
             'arcsinh': lambda v, a=arcsinh: v.evaluate(a),
             'arctanh': lambda v, a=arctanh: v.evaluate(a),
             'cosh': lambda v, c=math.cosh: v.evaluate(c),
             'sinh': lambda v, s=math.sinh: v.evaluate(s),
             'tanh': lambda v, t=math.tanh: v.evaluate(t),
             'sinc': lambda v, s=sinc: v.evaluate(s),
             # See http://chaos.org.uk/~eddy/physics/Lorentz.html
             'Lorentz': lambda v, c=second.light / second: c * v.tanh,
             'Doppler': lambda v: v.log.Lorentz }

def angle():
    from math import cos, sin, tan, pi
    from SI import radian
    def angeval(q, f, r=radian): return (q/r).evaluate(f)
    def sinc(val, r=radian, s=sin):
        try: return (val/r).evaluate(s) / val
        except ZeroDivisionError: return 1 / r

    return { 'Sin': lambda v, s=sin, a=angeval: a(v, s),
             'Cos': lambda v, c=cos, a=angeval: a(v, c),
             'Tan': lambda v, t=tan, a=angeval: a(v, t),
             'Sec': lambda v: 1. / v.Cos,
             'CoSec': lambda v: 1. / v.Sin,
             'SinC': lambda v, s=sinc: s(v),
             'CoTan': lambda v, q = radian * pi / 4: (q - v).Tan,
             'iExp': lambda v: v.Cos + 1j * v.Sin }

def speed():
    from SI import second, radian
    # See http://chaos.org.uk/~eddy/physics/Lorentz.html
    return { 'Lorentz': lambda v, c=second.light / second: (v / c).arctanh,
             'Doppler': lambda v: v.Lorentz.exp }

def mass():
    from SI import second, metre
    def weigh(v, g = 9.80665 * metre / second**2): return v * g
    def energy(v, cc = (second.light / second)**2): return v * cc
    return { 'weight': weigh, 'force': weigh, 'energy': energy }

def energy():
    from SI import second
    def mass(v, cc = (second.light / second)**2): return v / cc
    return { 'mass': mass }

def time():
    from SI import second, metre
    return { 'light': lambda v, c=299792458 * metre / second: v * c }
# It would also be nice to give time a print-format that breaks it up into
# years, days, hours, minutes, seconds.

def thermal():
    from SI import Kelvin
    def C(v, K=Kelvin): return v/K - 273.16
    def F(v): return v.Celsius * 1.8 + 32
    return { 'Centigrade': C, 'C': C, 'Celsius': C,
             'Fahrenheit': F, 'F': F  }

kind_prop_lookup = {
    '': scalar, 'rad': angle, 'm/s': speed,
    'kg': mass, 's': time, 'K': thermal,
    '(m/s)**2.kg' : energy }
del scalar, angle, speed, mass, time, thermal

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

from object import Object
_terse_dict = {}

class Quantity (Object):

    __obinit = Object.__init__
    def __init__(self, scale, units={},
                 doc=None, nom=None, fullname=None,
                 sample=None,
                 *args, **what):
        """Initialises an object representing a quantity.

        Arguments:

          scale -- (a Quantity or) a scalar, e.g. integer, long or float: it
          must support addition with and multiplication by at least these types.

          [units] -- (a Quantity or) a dictionary with string keys and integer
          values, or a Quantity.  Each key names a unit of measurement: the
          whole dictionary represents the product of pow(key, units[key]) over
          all keys, so {'kg':1, 'm':2, 's':-2} denotes kg.m.m/s/s, aka the
          Joule.  If omitted, an empty dictionary is used (indicating a
          dimensionless quantity).

          [doc] -- a documentation string for the quantity (default None).
          This may alternatively be set by the .document(doc) method.

          [nom] -- a short name by which to refer to the quantity.

          [fullname] -- a long name (capitalised, if appropriate) for the
          quantity. [This and nom act as fall-backs for one another: if you give
          either of them, it serves as the default for the other.]

          [sample] -- a sequence of quantities which should be equal to this
          one.

        The first two arguments, scale and units, may be Quantity instances: in
        which case each contributes its scale and units to the new Quantity,
        effectively multiplicatively. """

        # Initialise self as an Object:
        self.__obinit(*args, **what)

        # massage the arguments: first mix scale and units
        if isinstance(units, Quantity):
            if units.__scale is 1: units = units.__units
            else: scale, units = scale * units.__scale, units.__units
        if isinstance(scale, Quantity):
            units, scale = adddict(units, scale.__units), scale.__scale

        # massaging scale as a sample (so we can trust its str() to work).
        if not isinstance(scale, Sample):
            scale = qSample(best=scale)
        elif not isinstance(scale, qSample):
            scale = qSample(scale)

        # then (check and) massage sample:
        if sample:
            try: new, row = scale.copy(), ()
            except (TypeError, AttributeError): row = [ scale ]

            for val in sample:
                if val.__units != units:
                    raise TypeError('Sample of wrong dimensions', val, units)

                if row: row.append(val.__scale)
                else: new.update(val.__scale)

            if row: scale = qSample(row)
            else: scale = new

        if doc is not None: doc = _cleandoc(doc)

        # initialise self as a Quantity with the thus-massaged arguments
        self.__scale, self.__units, self.__doc__ = scale, units, doc
        self.__name(nom or fullname, fullname or nom)
        # Should __addcheck_() what['best'], what['low'] ... if given.

    def _primitive(self):
        """Returns a quantity in a primitive form.

        The primitive form doesn't try to use quantifiers when printing itself,
        or other fancy stuff.  This is intended for use by derived classes when
        sorting out their representations ... """

        what, units = self.__scale, self.__units
        if isinstance(what, qSample):
            what = Sample(what.mirror, **what.dir)
        return Quantity(what, units)

    def __name(self, nom=None, fullname=None):
        if nom: self._short_name_ = nom
        if fullname: self._long_name_ = fullname

    def document(self, doc):
        doc = _cleandoc(doc)
        if not doc: return

        try: old = self.__dict__['__doc__']
        except AttributeError: old = None
        if old: self.__doc__ = old + '\n' + doc
        else:   self.__doc__ = doc

    # TODO: need (?) a .merge(self, other) yielding result of merging two Quantities.
    def observe(self, what, doc=None):
        """Incorporate information from a measurement of this quantity.

        Required argument, what, is a Quantity or, of self is dimensionless, a
        Sample, sample.Weighted or simple numeric value.  A simple numeric value
        is used as a candidate best estimate.  Otherwise, the spread of the
        supplied value is suitably combined with self's prior spread and any
        best estimate it supplies is used as a candidate best estimate.

        Optional argument, doc, is a documentation string to be added to self's;
        any documentation string on what is also added, after doc.  For example,
        the passed doc value might indicate why the observation is relevant to
        the quantity self represents, while what.__doc__ indicates how that
        value was obtained.\n"""

        self.__scale.update(self.__addcheck_(what, 'observe'))
        if doc is not None: self.document(doc)
        # NB: don't use inherited what.__doc__, it may come from class, albeit not Quantity.
        try: self.document(what.__dict__['__doc__'])
        except KeyError: pass

    __obcopy = Object.copy
    def copy(self, func=None):
	return self.__obcopy(self.__scale.copy(func), self.__units.copy())

    def __cmp__(self, other): return cmp(self.__scale, self.__addcheck_(other, 'compare'))
    def _lazy_get__lazy_hash_(self, ignored):
        return reduce(lambda p, (k, v): p ^ v ^ hash(k),
                      self.__units.items(), hash(self.__scale))

    __lazy_late_ = Object._lazy_late_
    def _lazy_late_(self, key):
        # fall-back lookup of kind-specific attributes:
        bok = self._kind_lazy_props
        try: f = bok[key]
        except KeyError: return self.__lazy_late_(key)
        else: return f(self)

    def _lazy_get__kind_lazy_props_(self, ig, k=kind_prop_lookup, cache={}, empty={}):
        key = self._unit_str
        try: return cache[key]
        except KeyError: pass

        try: f = k[key]
        except KeyError: bok = empty
        else: bok = f()

        cache[key] = bok
        return bok

    def evaluate(self, f):
	"""Return result of passing self to the given scalar function.

	Takes a single argument, a callable, typically a function such as
	math.exp which can only handle inputs of type scalar; returns the
	appropriate Quantity obtained by supplying self to this function; but
	raises TypeError unless self is dimensionless.  Result is always
	dimensionless.

	Note that self.copy(f) will do the corresponding thing but giving the
	result the same units as self, whatever these may be; .copy() makes no
	attempt to check whether what you asked for makes sense ... """

	return self._quantity(self._scalar.copy(f), {})

    def __float__(self): return float(self._scalar)
    def __long__(self): return long(self._scalar)
    def __int__(self): return int(self._scalar)

    def _lazy_get__scalar_(self, ignored):
	# in later equivalents, self.__units may use quantities as units; in
	# which case, one should `flatten' self to check whether non-empty
	# really means dimensioned ...
        if self.__units: raise TypeError('not dimensionless', self._unit_str)
        return self.__scale

    def __nonzero__(self): return 0 != self.__scale
    def __neg__(self):
        try: return self.__neg
        except AttributeError: pass
        ans = self.__neg = self.copy(lambda x: -x)
        ans.__neg = self
        return ans

    def __abs__(self):
        try: return self.__abs
        except AttributeError: pass
        ans = self.__abs = self.copy(abs)
        return ans

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
                raise TypeError(why + ' with differing dimensions',
				self._unit_str, other._unit_str)

            return other.__scale

        except AttributeError:
            if self.__units:
                raise TypeError(why + ' between scalar and dimensioned quantity',
				other, self._unit_str)

            return other

    import math

    def arcTan2(self, what, units=[], atan=math.atan2):
        """Returns the angle whose Cos and Sin are in the same ratio as self and other.

        Combines self, as the adjacent side, with its one argument, as the
        opposite side, of a right-angle triangle; they must have the same units,
        but we're not fussy what those are.  Returns the angle (as a Quantity,
        with units of angle) between the hypotenuse and self.\n"""

        try: radian = units[0]
        except IndexError:
            from SI import radian
            units.append(radian)

        return self._quantity(self.__scale.join(atan, self.__addcheck_(what, 'arcTan2')), radian)

    def __hypot(self, other, h=math.hypot):
        return self.__kin(self.__scale.join(h, self.__addcheck_(other, 'Hypotenuse')))

    del math

    def Hypotenuse(self, *others):
        val = self
        for it in others:
            val = val.__hypot(it)
        return val

    # Addition, subtraction and their reverses.
    def __kin(self,    scale): return self._quantity(scale, self.__units)

    def __add__(self,  other): return self.__kin(self.__scale + self.__addcheck_(other, '+'))
    def __sub__(self,  other): return self.__kin(self.__scale - self.__addcheck_(other, '-'))
    def __mod__(self,  other): return self.__kin(self.__scale % self.__addcheck_(other, '%'))

    def __radd__(self, other): return self.__kin(self.__addcheck_(other, '+') + self.__scale)
    def __rsub__(self, other): return self.__kin(self.__addcheck_(other, '-') - self.__scale)
    def __rmod__(self, other): return self.__kin(self.__addcheck_(other, '%') % self.__scale)

    # multiplicative stuff is easier than additive stuff !
    def unpack(other):
        if isinstance(other, Quantity):
            return other.__scale, other.__units
        return other, {}

    def __mul__(self, other, grab=unpack):
        ot, her = grab(other)
        return self._quantity(self.__scale * ot, adddict(self.__units, her))

    def __rmul__(self, other, grab=unpack):
        ot, her = grab(other)
        return self._quantity(ot * self.__scale, adddict(her, self.__units))

    def __div__(self, other, grab=unpack): 
        ot, her = grab(other)
        if not ot: raise ZeroDivisionError, other
        return self._quantity(self.__scale / ot, subdict(self.__units, her))
    __truediv__ = __div__

    def __rdiv__(self, other, grab=unpack):
        ot, her = grab(other)
        return self._quantity(ot / self.__scale, subdict(her, self.__units))
    __rtruediv__ = __rdiv__

    def __pow__(self, what, mod=None, grab=unpack):
        assert mod is None
        wh, at = grab(what)
        if at: raise TypeError('raising to a dimensioned power', what)

        return self._quantity(pow(self.__scale, wh), scaledict(self.__units, wh))

    del unpack

    # lazy attribute lookups:
    def _lazy_get_accuracy_(self, ignored):
        s = self.__scale
        # maybe use s.bounds ? or span
        return (s.high - s.low) / s.best

    def _lazy_get_span_(self, ig):
        lo, hi = self.__scale.span
        return Quantity(lo, self.__units), Quantity(hi, self.__units)

    def _lazy_get_best_(self, which):
        """generic method for statistics, packaging those for __scale with __units """
        stat = getattr(self.__scale, which) # the statistic (e.g. best estimate) of scale
        return Quantity(stat, self.__units) # with the same units as self.

    _lazy_get_low_ = _lazy_get_high_ = _lazy_get_width_ = _lazy_get_errors_ \
                   = _lazy_get_median_ = _lazy_get_mean_ = _lazy_get_mode_ \
                   = _lazy_get_dispersor_ = _lazy_get_best_

    def _lazy_get_dispersal_(self, ignored): return self.__scale.dispersal
    def _lazy_get_variance_(self, ignored):
        return self._quantity(self.__scale.variance, scaledict(self.__units, 2))

    # lazy string and representation lookups:

    def __str__(self): return self._full_str
    # short-form, e.g. 9.81 micro m.kg/s**2
    def _lazy_get__unit_str_(self, ignored): return self.unit_string()
    def _lazy_get__number_str_(self, ignored): return str(self.__scale) or '?'

    def __repr__(self): return self._full_repr
    # valid python expression, full names: e.g. 9.81 * micro * metre*kilogramme / second**2
    def _lazy_get__number_repr_(self, ignored): return `self.__scale`

    # __scale isn't mentioned after this
    def _lazy_get__quantity_stinu_bok_(self, ignored):
        # Build reverse-lookup for self.__units
        pows = {} # ! { power: list of units appearing with this power }
        for key, val in self.__units.items():
            try: pows[val].append(key)
            except KeyError: pows[val] = [ key ]
        return pows
    # nor __units in the string infrastructure borrowed by eddy.science.quantity

    def _lazy_get__full_str_(self, ignored):
        # Gather components
        uni, num = self._unit_str, self._number_str
        # Deal with easy cases:
        if uni[:1] == '/': return num + uni
        if not uni: return num

        # Chuck 1 if that's all the number is:
        try: num = { '-1': '-', '-1.': '-', '1': '', '1.': ''}[num]
        except KeyError:
            # Otherwise, work out how to separate it from units:
            if num[-1] in '. \t\n\f': pad = ''
            elif '.' in num or num[-1] not in '0123456789': pad = ' '
            else: pad = '.'

        else: pad = ''

        # Stick the pieces together:
        return num + pad + uni

    def _lazy_get__full_repr_(self, ignored):

        def lookup(row, l=_terse_dict):
            out = []
            for nom in row:
                try: out.append(l[nom]._long_name_)
                except KeyError: out.append(nom)
            return out

        return self.unit_string(scale=self._number_repr,
                                times='*', Times=' * ',
                                divide='/', Divide=' / ',
                                lookemup=lookup)

    def _lazy_get__quantity_stinu_skey_(self, ignored):
        # Prepare the list of powers we'll be using (in descending order):
        vals = self._quantity_stinu_bok.keys()
        vals.sort()
        vals.reverse()
        while 0 in vals: vals.remove(0)
        # We'll be folding x^-i^ terms in with y^i^ as (y/x)^i^ ...
        # so eliminate -i from vals if i appears in it.
        for val in vals[:]:
            if val < 0: break
            while -val in vals: vals.remove(-val)

        return tuple(vals)

    def unit_string(self, scale='',
                    times='.', divide='/',
                    Times=None, Divide=None,
                    lookemup=lambda r: r):
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
          e.g. [ kg ] -> [ kilogramme ], or vice versa.

        I hope to be able to do something smarter when I can see when to say J
        rather than kg.m.m/s/s, and etc.  But that will probably involve
        creating a units base-class to replace the present _quantity_unit_bok
        dictionary. """


        # Honour Times='' even with times='.'
        if Times is None: Times = times
        # but not so for division ...
        if not Divide: Divide = divide

        head, tail = str(scale), ''
        pows = self._quantity_stinu_bok 

        for p in self._quantity_stinu_skey: # sorted keys of pows
            # punctuate
            if p > 0:
                if head or tail: tail = tail + Times
            elif p < 0: tail = tail + Divide
            else: continue      # never happens - 0 got stripped

            # This might be a better place for the milli * kilogramme bodge ...
            row = lookemup(pows[p])
            lang, top, bot = len(row), times.join(row), ''

            # ... do the promised folding:
            try: wor = lookemup(pows[-p])
            except KeyError: pass
            else:
                if wor:
                    bot = divide + divide.join(wor)
                    lang = lang + len(wor)

            if lang > 1 and p != 1: more = '(%s%s)' % (top, bot)
            else: more = top + bot

            p = abs(p)
            if p != 1:
                try:
                    # if we can represent p as an integer ...
                    try: ip = int(p)
                    except OverflowError: ip = long(p)
                except (AttributeError, ValueError): pass
                else:
                    # ... use that by preference !
                    if ip == p: p = ip

                more = '%s**%s' % (more, p)

            tail = tail + more

        return head + tail

    # Methods for use by derived classes and kindred allies:
    def _unit_order(self, unit):
        try: return self.__units[unit]
        except KeyError: return 0

    # Method to override, if needed, in derived classes ...
    def _quantity(self, what, units): return self.__class__(what, units)

del kind_prop_lookup

def base_unit(nom, fullname, doc, **what):
    result = Quantity(1, {nom:1}, doc, nom, fullname, **what)
    _terse_dict[nom] = result
    return result

gaussish = Quantity(Sample.gaussish, doc=Sample.gaussish.__doc__)
tophat = Quantity(Sample.tophat, doc=Sample.tophat.__doc__)
upward = Quantity(Sample.upward)
# 0 +/- .5: scale and add offset to taste, e.g.:
def sample(mid, tol, flat=2*tophat): return mid + tol * flat
