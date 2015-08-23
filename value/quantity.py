"""Objects to describe real quantities (with units of measurement).

TODO: Invent a quantity-type that identifies a value in principle, either by
definition (e.g. it's a primitive quantity such as a unit, physical constant or
property of a particular entity) or by formula in terms of others, supports
being told what values have experimentally been found for it, but doesn't need
any of these concrete values in the creation of the object.  Change data-supply
to consult data-sources (e.g. DBpedia and other Open Data, along perhaps with
some local files) and load data into the quantities for which they supply
values.  In particular, each particle, planet, element etc. comes into being
with such quantities as the values for its mass, charge, etc., with no value
given; but (for example) an element's atomic number, or an isotope's number of
neutrons, shall be an actual value ab initio.  This may involve different ways
of extracting data from each source: custom parsers for specific files,
appropriate protocol and semantics-groking code for Open Data sources.  Probably
prudent to support local caching of any data fetched from remote
sources.  Prototype by converting chemy.element first; see its TODO.

TODO: FIXME: support complex numbers.  Presently, things like .iExp (on an
angle) think they do, but produce Sample objects with complex keys; and the
interpolators assume ordering on their ordinate values.  Probably best handled
as part and parcel of integrating support for vector quantities.  Until this is
sorted out, various things that should work don't; e.g. chemy.atomic's Orbit and
maths.Legendre's Spherical think they're callable but actually involve doing
complex arithmetic on Quantity objects.

Probably want a different class for each quantity-kind, so that we can load it
up with the needed kind-specific properties, e.g. by providing a second base as
mix-in to define the extra lazyprops.

See study.LICENSE for copyright and license information.
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
    _exponent_to_quantifier['e%d' % _key] = _val

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
    representation is exact, the e will be an E, e.g. the integer 4000 is
    4E3. Exact numbers will also elide their decimal point if it is the last
    character of the mantissa, rough ones are less likely to.\n"""

    # Extract any sign and set it aside; we'll put it back as we return:
    if text[:1] in '-+': sign, text = text[:1], text[1:]
    else: sign = ''

    # Snip apart mantissa (head) and exponent (tail):
    glue = 'e'
    try: head, tail = text.split('e', 1)
    except ValueError:
        try: head, tail = text.split('E', 1)
        except ValueError: head, tail = text, ''
        else: glue = 'E' # value is exact

    # Ditch trailing '.' if value is exact:
    if head[-1] == '.' and glue == 'E': head = head[:-1]

    # Finally, transform 'e[sign]prial' into a name, if we have one for it:
    # e.g. 'e6' -> ' mega'
    if tail:
        try: mul = _e2q['e' + tail]
        except KeyError: tail = glue + tail
        else: tail = times + mul

    return sign + head + tail

del _exponent_to_quantifier
from study.value.sample import Sample

class qSample (Sample):
    # Massage Sample's answers, which use any integer for the exponent
    # and put digits.[digits] between 1 and 10.
    _lazy_get__sample_repr_ = Sample._lazy_get__repr_
    def _lazy_get__repr_(self, ignored, mash=massage_text):
        return mash(self._sample_repr, ' * ')
    def _lazy_get__str_(self, ignored, mash=massage_text):
        return mash(self._sample_repr, ' ')

del massage_text

# Lazy-evaluators for special attributes of quantities of specific types; see
# Quantity._lazy_get__kind_lazy_props_() for details.
def scalar():
    # Note that Sample doesn't cope well with complex values, but we can
    # sometimes get away with manipulating them anyway.
    import cmath, math
    from SI import radian, second

    def pick(m, c):
        def ok(v, m=m, c=c):
            try: return m(v)
            except (ValueError, TypeError):
                ans = c(v)
                if ans.imag + 1 != 1 or 1 + ans.real == 1: return ans
                # Imaginary part is ignorable, real part isn't:
                return ans.real
        return ok

    ln = pick(math.log, cmath.log)
    arccos = pick(math.acos, cmath.acos)
    arcsin = pick(math.asin, cmath.asin)
    arctan = pick(math.atan, cmath.atan)

    def arcsec(val, ac=arccos): return ac(1./val)
    def arccosec(val, ar=arcsin): return ar(1./val)
    def arccot(val, a=arctan, q=math.pi/2): return q - a(val)

    def Shannon(val, ln=ln, ln2=math.log(2)):
        if 1 + val == val: return 0
        return val * ln(val) / ln2
    # NB: for a distribution among N things, information content is
    # sum(Shannon) and redundancy is 1 - sum(Shannon)/N.

    def wrap(m, c, p=pick): return lambda v, f=p(m, c): v.evaluate(f)
    def angular(func, r=radian):
        return lambda v, f=func, r=r: r * v.evaluate(f)

    return { 'arcCos': angular(arccos), 'arcSin': angular(arcsin),
             'arcTan': angular(arctan), 'arcCoTan': angular(arccot),
             'arcSec': angular(arcsec), 'arcCoSec': angular(arccosec),
             'exp': wrap(math.exp, cmath.exp),
             'log': lambda v, l=ln: v.evaluate(l),
             'Shannon': lambda v, s=Shannon: v.evaluate(s),
             'arccosh': wrap(math.acosh, cmath.acosh),
             'arcsinh': wrap(math.asinh, cmath.asinh),
             'arctanh': wrap(math.atanh, cmath.atanh),
             'cosh': wrap(math.cosh, cmath.cosh),
             'sinh': wrap(math.sinh, cmath.sinh),
             'tanh': wrap(math.tanh, cmath.tanh),
             # See http://chaos.org.uk/~eddy/physics/Lorentz.html
             'Lorentz': lambda v, c=second.light / second: c * v.tanh,
             'Doppler': lambda v: v.log.Lorentz }

def angle():
    from math import cos, sin, tan, pi
    from SI import radian

    # min(sinc) is c. -.217 at pi * 1.43 ~ x = tan(x) ~ 4.5
    def sinc(val, sin=sin):
        """sin(x)/x &larr;x

        Used here to implement ({reals}: Sin(x).radian/x <- x :{angles}),
        which may or may not be The Right Thing to do.  Requires further
        research !\n"""
        if val + 1 == 1: return 1 # sinc(0) = 1
        return sin(val) / val
    # ... and there's a reason why I'm not trying to offer arcsinc !

    def wrap(func, r=radian):
        return lambda q, f=func, r=r: (q/r).evaluate(f)

    return { 'Sin': wrap(sin), 'Cos': wrap(cos), 'Tan': wrap(tan),
             'Sec': lambda v: 1. / v.Cos,
             'CoSec': lambda v: 1. / v.Sin,
             'CoTan': lambda v, q = radian * pi / 2: (q - v).Tan,
             'SinC': lambda v, s=sinc, r=radian: (v/r).evaluate(sinc),
             'iExp': lambda v: v.Cos + 1j * v.Sin }

def speed():
    from SI import second
    # See http://chaos.org.uk/~eddy/physics/Lorentz.html
    return { 'Lorentz': lambda v, c=second.light / second: (v / c).arctanh,
             'Doppler': lambda v: v.Lorentz.exp }

def acceleration():
    from study.chemy.physics import Thermal
    return { 'Unruh': lambda a, U=Thermal.Unruh: a / U }

def mass():
    from SI import second, metre
    from study.chemy.physics import Cosmos, Thermal
    def weigh(v, g = 9.80665 * metre / second**2): return v * g
    return { 'weight': weigh, 'force': weigh,
             'energy': lambda v, cc = (second.light / second)**2: v * cc,
             'wavelength': lambda m: m.energy.frequency.wavelength,
             'Schwarzschild': lambda m, S=Cosmos.Schwarzschild: S * m,
             'Hawking': lambda m, H=Thermal.Hawking: H / m }

def energy():
    from SI import second, Joule
    def mass(v, cc = (second.light / second)**2): return v / cc

    class seismic (object):
        """Seismic magnitudes.

        The lovely thing about standards is the number of them to chose
        amongs.  So this property of an energy (or, more precisely, a Quantity
        with units of torque, which are the same as of energy) has three
        attributes: the .Richter, .moment and .energy magnitudes associated with
        the the given amount of energy.  Note that the three different scales
        have quite separate meanings (i.e. the measure different energies for
        any given seismic event).  See also: study.value.archaea.Magnitude.\n"""

        from math import log
        base = log(10) * 1.5 # two steps are equivalent to a power of one thousand
        def __init__(self, lne, b=base): self.__e = lne / b
        from study.cache.property import lazyprop
        @lazyprop
        def Richter(self, off=log(4.2) / base + 4): return self.__e - off
        @lazyprop
        def energy(self, off=2.9 / 1.5): return self.__e - off
        @lazyprop
        def moment(self, off=9.1 / 1.5): return self.__e - off
        del lazyprop, log, base

    def seismic(v, J=Joule, S=seismic): return S((v / J).log)

    from study.chemy.physics import Thermal, Quantum
    return { 'frequency': lambda e, h=Quantum.h: e / h,
             'temperature': lambda e, k=Thermal.k: e / k,
             'mass': mass, 'seismic': seismic }

def frequency():
    from study.chemy.physics import Quantum
    return { 'period': lambda f: 1/f, 'wavelength': lambda f: (1/f).light,
             'energy': lambda f, h=Quantum.h: f * h }

def length():
    from study.chemy.physics import Quantum, Cosmos, Vacuum
    def hole(r, k=Cosmos.Schwarzschild): return k * r
    return { 'momentum': lambda d, h=Quantum.h: h / d,
             'mass': lambda d, h=Quantum.h / Vacuum.c: h / d,
             'frequency': lambda d, c=Vacuum.c: c / d,
             'time': lambda d, c=Vacuum.c: d / c,
             'Schwarzschild': hole, 'hole': hole }

def time():
    # NB: must control imports - broken if they evaluate .light !
    from SI import second, metre
    from units import year
    def split(t, y=year.sidereal, zero=0*second, s=second):
        """Broken-down time, in years, days, hours, minutes and seconds.

        Returns a tuple: first member is '+' or '-' according as t is positive
        or negative; the rest describe its value, giving whole years, days,
        hours and minutes, ending with the remainder in seconds, which may have
        a fractional part.  The year used is sidereal (the period of the Earth's
        orbit around The Sun), since I have no idea what calender was in force
        at either end of the interval that the time in question implicitly is.\n"""
        if t < zero: sign, t = '-', -t
        else: sign = '+'
        ny, t = divmod(t, y)
        t, ns = divmod(t, 60 * s)
        assert t == int(t)
        t, nm = divmod(t, 60)
        t, nh = divmod(t, 24)
        assert 0 <= t < 366
        return (sign, ny, t, nh, nm, ns)

    return { 'light': lambda v, c=299792458 * metre / second: v * c,
             'frequency': lambda t: 1/t, 'parts': split }

def thermal(): # temperature
    from SI import Kelvin, mol
    # See Centigrade() and Fahrenheit() in units.py for inverses of these:
    def C(v, K=Kelvin): return v/K - 273.16
    def F(v): return v.Celsius * 1.8 + 32

    from study.chemy.physics import Thermal
    def k(v, B=Thermal.k): return v * B # Energy

    return { 'R': lambda v, R=mol.R: v * R,
             'Unruh': lambda v, U=Thermal.Unruh: v * U,  # accelleration
             'hole': lambda v, H=Thermal.Hawking: H / v, # mass of black hole
             'Boltzmann':  k, 'k': k, 'energy':  k,
             'Centigrade': C, 'C': C, 'Celsius': C,
             'Fahrenheit': F, 'F': F  }

kind_prop_lookup = { # { ._unit_str: function }
    '': scalar, 'rad': angle, 'm/s': speed,
    'kg': mass, 's': time, 'm': length, 'K': thermal,
    '/s': frequency, 'm/s**2': acceleration, '(m/s)**2.kg': energy }
del scalar, angle, speed, acceleration, mass, energy, frequency, length, time, thermal

def tonumber(value): # tool function
    while True:
        for k in ( 'best', 'median' ):
            try: value = getattr(value, k)
            except AttributeError: pass
            else: break
        else: break
    return value

from study.snake import prodict
class Prodict (prodict.Prodict):
    def __pow__(self, n, mod=None, up=prodict.Prodict.__pow__, tonum=tonumber):
        assert mod is None
        # Prefer simple numbers as exponents for units ...
        if n: n = tonum(n)
        return up(self, n)
del prodict

from object import Object

class Quantity (Object):

    __obinit = Object.__init__
    def __init__(self, scale, units=Prodict(), doc=None, sample=None, *args, **what):
        """Initialises an object representing a quantity.

        Arguments:
          scale -- (a Quantity or) a scalar, e.g. integer, long or float: it
                   must support addition with and multiplication by at least
                   these types.
          [units] -- (a Quantity or) a dictionary with string keys and integer
                     values, or a Quantity.  Each key names a unit of
                     measurement: the whole dictionary represents the product of
                     pow(key, units[key]) over all keys, so {'kg':1, 'm':2,
                     's':-2} denotes kg.m.m/s/s, aka the Joule.  If omitted, an
                     empty dictionary is used (indicating a dimensionless
                     quantity).
          [doc] -- a documentation string for the quantity (default None).  This
                   may alternatively be set by the .document(doc) method.
          [sample] -- a sequence of quantities which should be equal to this
                      one.

        The first two arguments, scale and units, may be Quantity instances: in
        which case each contributes its scale and units to the new Quantity,
        effectively multiplicatively.\n"""

        # Initialise self as an Object:
        self.__obinit(*args, **what)

        # Massage the arguments: first mix scale and units.
        scale, units = self.__clean_scale_units(scale, units)
        # then (check and) massage sample (if any):
        if sample:
            try: new, row = scale.copy(), () # TODO: check this is always OK
            except (TypeError, AttributeError): row = [ scale ]

            for val in sample:
                sc, un = val._scale_units_()
                if un != units:
                    raise TypeError('Sample of wrong dimensions', val, units)

                if row: row.append(sc)
                else: new.update(sc)

            if row: scale = qSample(row)
            else: scale = new

        if doc is not None: doc = self.__cleandoc(doc)

        # Initialise self as a Quantity with the thus-massaged arguments:
        self.__scale, self.__units, self.__doc__ = scale, units, doc
        # Should __addcheck() what['best'], what['low'] ... if given.

    @staticmethod
    def __clean_scale_units(scale, units,
                            scalartypes=(int, long, float, complex),
                            Bok=Prodict, Spread=Sample, Nice=qSample):
        """Tidy up scale and units passed to constructor.

        Each is allowed to be a Quantity, scale may be a qSample, Sample or
        native scalar, units may be a simple dictionary or a Prodict.  This
        method puts all of the scalar-ness into scale and all the units-ness
        into units.  (It mainly exists so that scalartypes can be computed just
        once, rather than on every run of the test that uses it; but it may also
        help make it possible to del qSample and Prodict from this module's
        name-space, some day; and it also makes __init__ easier to read.)\n"""

        # Using try allows Object()s that borrow() from Quantity()s work.
        try: s, u = units._scale_units_()
        except AttributeError: pass
        else:
            if isinstance(s, scalartypes) and s == 1: units = u
            else: scale, units = scale * s, u

        try: s, u = scale._scale_units_()
        except AttributeError:
            if not isinstance(units, Bok): units = Bok(units)
        else: units, scale = u * units, s

        # Massaging scale as a sample (so we can trust its str() to work).
        if not isinstance(scale, Spread):
            scale = Nice(best=scale)
        elif not isinstance(scale, Nice):
            scale = Nice(scale)

        assert isinstance(scale, Nice) and isinstance(units, Bok)
        return scale, units

    @staticmethod
    def __cleandoc(text):
        if text:
            text = text.strip()

            if text.find('\n ') >= 0 or text.find('\n\t') >= 0:
                # Remove any ubiquitous common indent (and any dangling hspace
                # or final blank lines - expanding tabs as spaces, as we go):
                lines = [ x.rstrip().expandtabs() for x in text.rstrip().split('\n') ]
                # First line typically lacks the common indent.
                text, lines = lines[0], lines[1:]
                if lines:
                    ind = min(len(x) - len(x.lstrip()) for x in lines if x)
                    assert not ind or all(not x or x[:ind].isspace() for x in lines)
                    # Strip indent from first line, if it has it, though:
                    if text[:ind].isspace(): text = text[ind:]
                    else: text = text.lstrip()
                    text += '\n' + '\n'.join(x[ind:] for x in lines)

            if text: return text + '\n'
        # else: implicitly return None.

    def _str_frags(self):
        """Returns a quantity's raw strings for scale and units.

        The primitive form doesn't try to use quantifiers when printing itself,
        or other fancy stuff.  This is intended for use by derived classes when
        sorting out their representations ...\n"""

        what, unitstr = self.__scale, self._unit_str
        assert isinstance(what, qSample)
        what = str(Sample(what.mirror, **what.dir))
        return what, unitstr

    def document(self, doc):
        doc = self.__cleandoc(doc)
        if not doc: return

        try: old = self.__dict__['__doc__']
        except AttributeError: old = None
        if old: self.__doc__ = old + '\n' + doc
        else:   self.__doc__ = doc

    # TODO: need (?) a .merge(self, other) yielding result of merging two Quantities.
    def observe(self, what, doc=None):
        """Incorporate information from a measurement of this quantity.

        Required argument, what, is a Quantity or, if self is dimensionless, a
        Sample, sample.Weighted or simple numeric value.  A simple numeric value
        is used as a candidate best estimate.  Otherwise, the spread of the
        supplied value is suitably combined with self's prior spread and any
        best estimate it supplies is used as a candidate best estimate.

        Optional argument, doc, is a documentation string to be added to self's;
        any documentation string on what is also added, after doc.  For example,
        the passed doc value might indicate why the observation is relevant to
        the quantity self represents, while what.__doc__ indicates how that
        value was obtained.\n"""

        self.__scale.update(self.__addcheck(what, 'observe'))
        if doc is not None: self.document(doc)
        # NB: don't use inherited what.__doc__, it may come from class, albeit not Quantity.
        try: self.document(what.__dict__['__doc__'])
        except KeyError: pass

    __obcopy = Object.copy
    def copy(self, func=None):
        return self.__obcopy(self.__scale.copy(func), self.__units.copy())

    def __cmp__(self, other): return cmp(self.__scale, self.__addcheck(other, 'compare'))
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

    def _lazy_get__kind_lazy_props_(self, ig,
                                    k=kind_prop_lookup, cache={}, empty={}):
        """Quantity-kind-specific attribute lookup.

        For each kind of quantity (length, speed, etc.) this provides a mapping
        from names of attributes, relevant only to that kind of quantity, to
        functions that convert a value of that kind to its relevant
        attribute.  That values of this mapping are functions is required to
        ensure the attribute depends on the quantity of which it is an
        attribute.

        The actual mapping returned is obtained from an internal mapping which
        takes ._unit_str as key (identifying the kind) and provides a function
        as value.  The return from calling the provided function (with no
        arguments) is the mapping we return (and it's cached, so we won't need
        to call that function next time this method is called with the given
        ._unit_str).  This second layer of function-via-mapping makes it
        possible for the functions (that build the mappings whose values are the
        functions that compute kind-specific attributes) to import modules that
        may depend on this one - typically to obtain units, or the values of
        physical constants, that are expressed as Quantity objects - without
        creating a cyclic dependency.\n"""

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
        attempt to check whether what you asked for makes sense ...\n"""

        return self._quantity_(self._scalar.copy(f), {})

    def __float__(self): return float(self._scalar)
    def __long__(self): return long(self._scalar)
    def __int__(self): return int(self._scalar)

    def _lazy_get__scalar_(self, ignored):
        try: return self.__get_scale(self, {})
        except TypeError, what:
            assert len(what.args) == 1
            raise TypeError('not dimensionless', what.args[0])

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

    @staticmethod
    def __get_scale(value, units):
        """Checks value the given units and returns its scalar.

        Arguments:
          value -- the value to be checked and extracted
          units -- the expected units dictionary

        If the value doesn't match the units, a type-error is raised, whose
        .args[0] is value._unit_str if it had one; otherwise, .args is empty.\n"""

        # In later equivalents, units may use Quantity()s as units; in which
        # case, one should `flatten' it to check whether non-empty really
        # means dimensioned - and similar is needed for equality comparisons.

        try:
            scale, un = value._scale_units_()
            if un != units:
                raise TypeError(value._unit_str)
            return scale
        except AttributeError:
            if units: raise TypeError()
            return value

    # Support for additive functionality:
    def __addcheck(self, other, why):
        """Checks for additive compatibility and unpacks.

        Arguments:
          other -- another quantity, which should have the same dimensions as
                   self; may be a scalar iff self is dimensionless.
          why -- string, e.g. '+' or 'compare', describing what caller is doing:
                 used as prefix in error messages.

        Returns other's scalar aspect or raises a TypeError.\n"""

        try: return self.__get_scale(other, self.__units)
        except TypeError, what: pass
        if what.args:
            raise TypeError(why + ' with differing dimensions',
                            what.args[0], self._unit_str)
        raise TypeError(why + ' between scalar and dimensioned quantity',
                        other, self._unit_str)

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

        return self._quantity_(self.__scale.join(atan, self.__addcheck(what, 'arcTan2')), radian)

    def __hypot(self, other, h=math.hypot):
        """Pythagorean sum.

        Adds the squares of self and other, returns the sum's square root.\n"""
        return self.__kin(self.__scale.join(h, self.__addcheck(other, 'Hypotenuse')))

    del math

    def Hypotenuse(self, *others):
        """Pythagorean sum.

        Sums the squares of self and arbitrarily many others (all of the same
        kind); returns the square root of the sum.  Meaningful for many (albeit
        not all) kinds of quantities; enough that it makes more sense to provide
        it universally - and ignore it when inappropriate - than to try to
        exhausitively provide it kind-specifically for every kind of quantity to
        which it *is* relevant.\n"""
        val = self
        for it in others:
            val = val.__hypot(it)
        return val

    # Addition, subtraction and their reverses.
    def __kin(self,    scale): return self._quantity_(scale, self.__units)

    def __add__(self,  other): return self.__kin(self.__scale + self.__addcheck(other, '+'))
    def __radd__(self, other): return self.__kin(self.__addcheck(other, '+') + self.__scale)
    def __sub__(self,  other): return self.__kin(self.__scale - self.__addcheck(other, '-'))
    def __rsub__(self, other): return self.__kin(self.__addcheck(other, '-') - self.__scale)

    # multiplicative stuff is easier than additive stuff !
    def unpack(other, one=Prodict()):
        # Using try lets an Object that borrow()s from a Quantity work
        try: return other._scale_units_()
        except AttributeError: pass # not a Quantity
        return other, one

    def _scale_units_(self):
        """Provide borrowable access to privates.

        Object restricts borrowing to public attributes; but this prevents an
        Object from behaving numerically like a Quantity from which it borrows;
        so work around that by providing this method, to tunnel between unpack
        and the Quantity from which its argument is borrowing.\n"""
        return self.__scale, self.__units

    def __mul__(self, other, grab=unpack):
        if isinstance(other, tuple): # assume study.maths.vector.Vector
            return other * self

        ot, her = grab(other)
        return self._quantity_(self.__scale * ot, self.__units * her)

    def __rmul__(self, other, grab=unpack):
        ot, her = grab(other)
        return self._quantity_(ot * self.__scale, her * self.__units)

    def __div__(self, other, grab=unpack): 
        ot, her = grab(other)
        if not ot: raise ZeroDivisionError, other
        return self._quantity_(self.__scale / ot, self.__units / her)
    __truediv__ = __div__

    def __rdiv__(self, other, grab=unpack, one=Prodict()):
        if isinstance(other, tuple): # assume study.maths.vector.Vector
            return other * self._quantity_(1. / self.__scale, one / self.__units)

        ot, her = grab(other)
        return self._quantity_(ot / self.__scale, her / self.__units)
    __rtruediv__ = __rdiv__

    # Whole-quotient division and modulus:
    @staticmethod
    def __floor(rat):
        low = rat._scalar.low # we're rounding down
        try: n = int(low)
        except TypeError:
            raise TypeError('Whole division requires scalar ratio', rat)
        # int rounds towards zero, we want to round down
        if low < n: n -= 1
        assert n <= low < n + 1
        # but note that original rat.high might be > n+1
        return n

    def __floordiv__(self, other): return self.__floor(self / other)
    def __rfloordiv__(self, other): return self.__floor(self.__rdiv__(other))

    # These should give 0 <= .low < 1, but with no upper bound on .high
    def __mod__(self, other): return self - self.__floordiv__(other) * other
    def __rmod__(self, other): return other - self.__rfloordiv__(other) * self

    def __divmod__(self, other):
        rat = self.__floordiv__(other)
        return rat, self - rat * other

    def __rdivmod__(self, other):
        rat = self.__rfloordiv__(other)
        return rat, other - rat * self

    def __pow__(self, what, mod=None, grab=unpack):
        if mod is not None:
            # Only makes sense if self, what and mod are whole, which isn't
            # what Quantity is designed for (so Sample doesn't support it).
            return NotImplementedError('modular power not supported for Quantity()s', mod)

        wh, at = grab(what)
        if at: raise TypeError('raising to a dimensioned power', what)

        return self._quantity_(pow(self.__scale, wh), self.__units ** wh)

    def __rpow__(self, what, mod=None, grab=unpack):
        assert mod is None, "Ternary pow isn't meant to call __rpow__ !"
        if self.__units: raise TypeError('raising to a dimensioned power', self)

        wh, at = grab(what)
        return self._quantity_(pow(wh, self.__scale), at ** self.__scale)

    del unpack

    # lazy attribute lookups:
    def _lazy_get_accuracy_(self, ignored):
        s = self.__scale
        # maybe use s.bounds ? or span
        return (s.high - s.low) / s.best

    def _lazy_get_span_(self, ig):
        lo, hi = self.__scale.span
        return self.__kin(lo), self.__kin(hi)

    def _lazy_get_best_(self, which):
        """generic method for statistics, packaging those for __scale with __units"""
        stat = getattr(self.__scale, which) # the statistic (e.g. best estimate) of scale
        return self.__kin(stat) # with the same units as self.

    _lazy_get_low_ = _lazy_get_high_ = _lazy_get_width_ = _lazy_get_errors_ \
                   = _lazy_get_median_ = _lazy_get_mean_ = _lazy_get_mode_ \
                   = _lazy_get_dispersor_ = _lazy_get_best_

    def _lazy_get_dispersal_(self, ignored): return self.__scale.dispersal
    def _lazy_get_variance_(self, ignored):
        return self._quantity_(self.__scale.variance, self.__units ** 2)

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

    __terse_dict = {}
    def _lazy_get__full_repr_(self, ignored):

        def lookup(row, l=self.__terse_dict):
            out = []
            for nom in row:
                try: out.append(l[nom].__long_name)
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
                   built by this routine: in particular, if given, it will be
                   joined to this unit representation by a suitable times or
                   divide operator.  If a non-string is given for scale, its
                   repr() is used.
          times, divide -- strings, default '.' and '/', to be used to denote
                           multiplication and division in compact texts,
                           e.g. kg.m/s.
          Times, Divide -- strings, defaulting to times and divide, to be used
                           to denote multiplication and division in spread-out
                           texts, e.g. kg.m / s^2^.
          lookemup -- a function taking a sequence of strings and returning a
                      similar sequence in which each string may have been
                      replaced with an alternative: typically, the strings in
                      the input list will be short names of units, to be
                      converted to long names where known, e.g. [ kg ] -> [
                      kilogramme ], or vice versa.

        I hope to be able to do something smarter when I can see when to say J
        rather than kg.m.m/s/s, and etc.  But that will probably involve
        creating a units base-class to replace the present __terse_dict
        dictionary, which only knows about base units.\n"""


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
                # if we can represent p as an integer ...
                try: ip = int(p)
                except (AttributeError, ValueError): pass
                else:
                    # ... use that by preference !
                    if ip == p: p = ip

                more = '%s**%s' % (more, p)

            tail = tail + more

        return head + tail

    # Methods for use by derived classes and kindred allies:
    def _unit_order(self, unit): return self.__units[unit]

    # Method to override, if needed, in derived classes ...
    def _quantity_(self, what, units): return self.__class__(what, units)

    # Be sure to keep argument defaults in sync with __init__():
    @classmethod
    def flat(cls, lo, hi, best=None,
             units=Prodict(), doc=None, rescale=None,
             *args, **what):
        """Describe a value with a flat distribution.

        Required arguments:
          lo -- lower bound on value
          hi -- upper bound on value

        Optional arguments:
          best -- best estimate value in the interval, or None (the default) if
                  no best estimate is available
          units -- as for Quantity.
          doc -- as for Quantity.
          rescale -- scaling to apply to all of lo, hi and (if given) best; or
                     None to leave them alone.

        Values of lo, hi and (when given) best must all be of the same kind
        (i.e. have the same units), although mixing dimensionless Quantity()s
        with plain scalars (or Sample()s) is allowed.  If they have units, these
        are combined with the supplied units (if any); thus, for example, lo and
        hi could be times and units could be the speed of light, to produce a
        range of distances (that light travels between the given periods of
        time).\n"""

        try: lo, un = lo._scale_units_()
        except AttributeError: un = {}
        else: units = un * units

        hi = cls.__get_scale(hi, un)
        if best is not None:
            best = cls.__get_scale(best, un)

        scale = Sample.flat(lo, hi, best)
        if rescale:
            if isinstance(rescale, Quantity): scale = rescale * scale
            else: scale *= rescale

        return cls(scale, units, doc, *args, **what)

    @classmethod
    def within(cls, best, tol, *args, **what):
        """Convenience constructor for mid-point and half-width.

        Required arguments:
          best -- mid-point of the range of values, also used as best estimate
          tol -- half-width of the interval of values

        If either has units, both must and their units must agree; also, tol
        should be positive.  The range best +/- tol is what these values
        describe.

        All other arguments (positional, starting with units, or keyword) are
        forwarded to .flat(), hence possibly to Quantity(); if units are
        specified, they are effecitvely multiplied with any units of best and
        tol.\n"""
        return cls.flat(best - tol, best + tol, best, *args, **what)

    @classmethod
    def below(cls, top, units=Prodict(), *args, **what):
        """A quantity known to be nearer to zero than some given value.

        Required value, top, is the biggest value the quantity could have, with
        the sign that the quantity is known to have.  Result is bounded away
        from zero and at most the given value.  Optional argument units is as
        for Quantity.

        All other arguments (positional, starting with doc, or keyword) are
        forwarded to .flat(), hence possibly to Quantity().\n"""

        try: top, un = top._scale_units_()
        except AttributeError: pass
        else: units = un * units

        try: best = what['best'] # unnatural, but don't error on it !
        except KeyError: best = None
        else: del what['best']

        # TODO: pick a better distribution
        return cls.flat(top * femto, top, best, units, *args, **what)

    @classmethod
    def fromSpread(cls, best, down, up, *args, **what):
        """Convenience constructor for value with specified error-bar.

        Required arguments:
          best -- the best estimate, not necessarily central in the range
          down -- difference between best and lower bound
          up -- difference between best and upper bound

        If any has units, all must have the same units.  Both down and up should
        be positive; the range of values specified is from best-down to best+up.

        All other arguments (positional, starting with units, or keyword) are
        forwarded to .flat(), hence possibly to Quantity().\n"""
        return cls.flat(best - down, best + up, best, *args, **what)

    @classmethod
    def fromDecimal(cls, best, decimals=0, exponent=None, *args, **what):
        """Convenience constructor for value given number of decimal places.

        Supports a value recorded to some specific number of digits past the
        decimal point.  Required arguments, best, is the best estimate of the
        value, i.e. the value published.  Next positional argument, decimals, is
        the number of digits specified after the decimal point: it defaults to
        zero, for the common case where a value has been rounded to the nearest
        whole number.  Assigning whole = Quantity.fromDecimal may thus be useful
        in an interactive session.

        Best is expected to be dimensionless (in fact, a simple number);
        decimals should be an integer.  (Technically, even a negative value for
        decimals is meaningful; passing -2 as decimals means +/- 50 on best; the
        two digits to the left of the decimal point aren't known.  Such
        situations may, however, be better expressed by use of exponent, see
        below.)  Note that leading zeros after the decimal point in best should
        be included in decimals; if best is 0.00314 then decimals should be
        5.  Likewise, trailing zeros should normally be understood to be
        significant; if 0.003140 was the published value, then decimals should
        be 6.

        Note that some publications typeset a number such that the 'confidently
        correct' digits are distinct from subsequent digits indicating a best
        estimate; in such a case, the number of confidently correct digits after
        the decimal point is what you should pass as decimal (and it's perfectly
        fine to include the others in the value given for best).  For example,
        3.141(59) might be used to indicate the 3.141 part is confidently known
        and 59 is the best estimate at the next two digits; passing
        best=3.14159, decimals=3 represents this faithfully.  Note, however,
        that similar typesetting might indicate an error bar on the last few
        digits, e.g. 3.141(59) might mean 3.141 +/- 0.059; be sure to actually
        know what notation is in use.

        Optional argument exponent defaults to None; otherwise, it should be an
        integer and the values of best and the bounds implied by decimals are
        scaled by ten**exponent (after the bounds have been computed).  Thus
        .fromDecimal(b, d, n) has the same meaning as .fromDecimal(b * 10**i,
        d-i, n-i) for any integer i <= d.

        All other arguments (positional, starting with units, or keyword) are
        forwarded to .flat(), hence possibly to Quantity().  To read a text
        string suitably, see .parseDecimal().\n"""
        if exponent: what['rescale'] = what.get('rescale', 1) * 10 ** exponent
        return cls.within(best, 0.5 / 10 ** decimals, *args, **what)

    @classmethod
    def parseDecimal(cls, text, fuzzy=None, *args, **what):
        """Convenience constructor for use with a decimal string.

        Required argument, text, is a decimal string that float() can parse; the
        value float() gives for it is used as best, while its exponent and
        number of digits are parsed directly.  Optional argument, fuzzy, says
        how many of the last digits of the mantissa, given in text, are not
        confidently believed correct; a negative value implicitly pads the
        mantissa with zeros.  Further arguments (positional, starting at units,
        or keyword) are forwarded to .fromDecimal(), hence onwards via .flat().

        If fuzzy is supplied and not None (its default), it should be a whole
        number (integer).  Otherwise (omitted or None), if text is simply a
        sequence of digits, fuzzy defaults to its number of trailing zeros; if
        text includes a decimal point or exponent, fuzzy defaults to
        zero.  Thus, by default, '3400' only has two significant digits (the
        zeros aren't significant), whereas '3400.' or '3400e0' has four.

        Note that the standard print format for a Quantity's scale corresponds
        to the case fuzzy=1; the last digit it displays is always uncertain.\n"""

        best, cut, special = float(text), text.find('e'), True # fail if float() does
        if cut < 0: cut = text.find('E')
        if cut < 0: mant, exp = text, 0
        else: mant, exp, special = text[:cut], int(text[cut+1:]), False
        cut = mant.find('.')
        if cut < 0: whole, frac = mant, ''
        else: whole, frac, special = mant[:cut], mant[cut+1:], False
        # text is equivalent to whole + '.' + frac + 'e%d' % exp
        # NB: frac can be empty even without cut < 0; e.g. '100.'

        if fuzzy is None: # handle the special case
            if special:
                assert text is whole
                deci = -len(whole.rstrip('0'))
            else: deci = len(frac)
        else: deci = len(frac) - fuzzy

        return cls.fromDecimal(best, deci-exp, None, *args, **what)

    @classmethod
    def fromSigFigs(cls, best, sigfig, base=10, units=Prodict(), *args, **what):
        """Convenience constructor in terms of number of significant figures.

        Required arguments:
          best -- the given value, used as best estimate
          sigfig -- number of significant digits of best that are confidently
                    known; this counts digits from the left-most non-zero digit
                    to the right-most digit (including zeros) given in the
                    published value of best - give or take a few at the end if
                    the publication indicates these are not confidently known;
                    and ignoring any exponent.

        For example, in 03.140e27, the leading 0 of 03 is ignored, as is the
        exponent e27, but the 0 following 4 is included, so we have four
        significant figures, 3140 (we also ignore the decimal point).  If the
        publication uses 3.141(59) to mean 3.14159 is their best estimate but
        the last two digits are uncertain, only the four digits in 3.141 are
        confidently known, so sigfig is 4, but it is perfectly fine to pass
        3.14159 as the value for best.  (Note, however, that you should check
        the notation actually in use; 3.141(59) may mean other things.)  See
        also the discussion in fromDecimal().

        Optional arguments (may be provided positionally, in this order, or as
        keywords):
          base -- number base used in the representation in which sigfig was
                  counted; defaults to ten; must be > 1.
          units -- as for Quantity.

        All other arguments (positional, starting with doc, or keyword) are
        forwarded to .flat(), hence possibly to Quantity().\n"""

        try: scale, un = best._scale_units_()
        except AttributeError: scale = best
        else: units, best = un * units, scale

        assert base > 1, 'Silly number base'
        tol = cls.__magnitude(scale, base) * base ** sigfig

        return cls.within(best, 0.5 * tol, units, *args, **what)

    @staticmethod
    def __magnitude(scale, base, tonum=tonumber):
        tol, scale = 1, abs(tonum(scale))
        while tol < scale: tol *= base
        while tol > scale: tol /= base
        return tol

    @classmethod
    def gaussian(cls, best, sigma, units=Prodict(), *args, **what):
        """Convenience constructor in terms of mean and standard deviation.

        Required arguments:
          best -- best estimate at value (mean of distribution)
          sigma -- standard deviation of error distribution

        If either has units, both must (counting dimensionless as not having
        units); and their units must agree.  After these, optional argument
        units may be supplied, with the same meaning and default as for
        Quantity; if given, it may be a Quantity or a mapping from (short) names
        of units to powers of each; in such a case, it shall be multiplied by
        the units of best (or equally of sigma), if any.

        All other arguments (positional, starting with doc, or keyword) are
        forwarded to Quantity().\n"""

        try: mid, un = best._scale_units_()
        except AttributeError: un, mid = {}, best
        else: units = un * units
        sigma = cls.__get_scale(sigma, un)

        return cls(Sample.gaussish * sigma + mid, units, *args, **what)

    @classmethod
    def encircle(cls, scale=1, units=Prodict(), *args, **what):
        """Returns a zero-centred interval with weight derived from a circle.

        For the simple unit-scaled version of this, the range of values is from
        -1 to +1, with the weight in each interval between cos(a) and cos(b)
        being b-a.  This is useful when scale is the radius of a circle on whose
        circumference a value is uniformly distributed and we know our distance
        from the circle's centre (assumed large compared to the radius); this is
        the error bar that represents our distance from the point on the circle.

        Useful in orbital mechanics for getting the variability of a satellite's
        distance from the star orbitted by the planet the satellite orbits.\n"""
        try: mid, un = scale._scale_units_()
        except AttributeError: mid = scale
        else: units = un * units
        return cls(Sample.encircle * mid, units, *args, **what)

    @classmethod
    def unit(cls, scale, units, nom, fullname, doc, **what):
        result = cls(scale, units, doc, **what)
        if nom: result.__short_name = nom # nowhere used (for now)
        if fullname: result.__long_name = fullname
        return result

    @classmethod
    def base_unit(cls, nom, fullname, doc, **what):
        """Constructor for a unit not derived from others.

        Required arguments:
          nom -- short name (symbol) for the unit
          fullname -- full name (with capitalisation and non-ASCII content when
                      appropriate)
          doc -- documentation, explaining the unit

        Arbitrary keyword arguments may be passed and shall be forwarded to
        Quantity on construction.  Note, however, that values are already
        supplied for scale, units, doc, nom and fullname as positional
        parameters (so passing any of these as keyword shall fail).\n"""
        result = cls.unit(1, { nom: 1 }, nom, fullname, doc, **what)
        cls.__terse_dict[nom] = result
        return result

del kind_prop_lookup, tonumber, Prodict
