"""Objects to describe real quantities (with units of measurement).

$Id: quantity.py,v 1.14 2001-12-12 17:10:01 eddy Exp $
"""

# The multipliers (these are dimensionless) - also used by units.py
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
    3: 'kilo',
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

_exponent_to_quantifier = {} # needed for qSample._repr
for _key, _val in _quantifier_dictionary.items():
    exec '%s = 1e%d' % (_val, _key)
    _exponent_to_quantifier['e%+d' % _key] = _val

hecto= 100
deca = deka = 10
deci = .1
centi= .01

# Note: a gramme comes out as a milli * kilogramme, which is only fair, all
# things considered.  Maybe I'll fix it some day ... but will someone object to
# mega * gramme not using an SI base unit ?

import string
from basEddy.sample import Sample

class qSample (Sample):
    def __init__(self, sample=(), *args, **what):
        if isinstance(sample, Sample):
            what.update(sample.dir)
            sample = sample.mirror

        apply(Sample.__init__, (self, sample) + args, what)

    def __massage_text_(self, times, _e2q=_exponent_to_quantifier):
        """Computes the representation of qSample.

        Single argument, times, is the text to put between a number and a
        quantifier to serve as `multiplication' in the resulting string.

        Result has form [sign]digits[.digits][quantifier] where: quantifier may
        be, for instance, times + 'mega', meaning the appropriate power of 10,
        else it is explicitly [e[sign]digits], e.g. 'e6', an exponent; in which
        case the exponent is a multiple of three.  The mantissa,
        digits[.digits], is always in the range .1 to 1000 and only exceeds 100
        if at least three significant digits are available and, by so doing, we
        can avoid the need for a [quantifier].  If the exponent is given as
        [e[sign]digits] and the representation is exact, the e will be an E,
        e.g. the integer 4000 is 4E3.  Exact numbers will also elide their
        decimal point if it is the last character of the mantissa, rough ones
        are less likely to. """

        text = Sample._lazy_get__repr_(self, '_repr')
        # Extract the sign, if present, and set it aside; we'll put it back as we return
        if text[:1] in '-+': sign, text = text[:1], text[1:]
        else: sign = ''

        # Snip apart mantissa (head) and exponent (tail):
        glue = 'e'
        try: head, tail = string.split(text, 'e')
        except ValueError:
            try: head, tail = string.split(text, 'E')
            except ValueError: head, tail = text, '0'
            else: glue = 'E' # value is exact

        # Decode (string) tail as an (integer) exponent:
        exponent = string.atoi(tail)
        # Snip apart the mantissa (head) at the dot (if any):
        try: up, down = string.split(head, '.')
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

    # Massage Sample's answers, which use any integer for the exponent
    # and put digits.[digits] between 1 and 10.
    def _lazy_get__repr_(self, ignored): return self.__massage_text_(' * ')
    def _lazy_get__str_(self, ignored): return self.__massage_text_(' ')

del _exponent_to_quantifier

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

from basEddy.value import Object
_terse_dict = {}

class Quantity(Object):
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

        # Initialise self as a Object:
        apply(Object.__init__, (self,) + args, what)

        # massage the arguments: first mix scale and units
        if isinstance(units, Quantity):
            scale, units = scale * units.__scale, units.__units

        if isinstance(scale, Quantity):
            units, scale = adddict(units, scale.__units), scale.__scale

        # massaging scale as a sample (so we can trust its str() to work).
        if not isinstance(scale, Sample):
            scale = qSample(best=scale)
        elif not isinstance(scale, qSample):
            scale = qSample(scale)

        # then (check and) massage sample:
        if sample:
            row = [] # sequence of __scale attributes

            for val in sample:
                if val.__units != units:
                    raise TypeError, ('Sample of wrong dimensions', val, units)

                row.append(val.__scale)

            assert row, ('non-empty but no entries !', sample)

            try:
                new = scale.copy()
                apply(new.update, (row,))
            except (TypeError, AttributeError):
                new = qSample(row + [ scale ])

            scale = new

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

    def name(self, nom=None, fullname=None):
        if nom: self._short_name_ = nom
        if fullname: self._long_name_ = fullname

    def document(self, doc): self.__doc__ = doc
    def observe(self, what): self.__scale.update(self.__addcheck_(what, 'observe'))
    def copy(self): return Object.copy(self, self.__scale, self.__units)

    def __cmp__(self, other): return cmp(self.__scale, self.__addcheck_(other, 'compare'))
    def _lazy_get__lazy_hash_(self, ignored):
        h = hash(self.__scale)
        for k, v in self.__units.items():
            h = h ^ v ^ hash(k)

        return h

    def __float__(self): return float(self._scalar)
    def __long__(self): return long(self._scalar)
    def __int__(self): return int(self._scalar)

    def _lazy_get__scalar_(self, ignored):
        if self.__units: raise TypeError('not dimensionless', self._unit_str)
        return self.__scale

    def __nonzero__(self): return 0 != self.__scale
    def __neg__(self): return self._neg
    def __abs__(self):
        if self.__scale < 0: return self._neg
        return self

    def _lazy_get__neg_(self, ignored):
        result = self._quantity( - self.__scale, self.__units)
        result._neg = self
        return result

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

    # Addition, subtraction and their reverses.
    def __kin(self,    scale): return self._quantity(scale, self.__units)
    def __add__(self,  other): return self.__kin(self.__scale + self.__addcheck_(other, '+'))
    def __radd__(self, other): return self.__kin(self.__addcheck_(other, '+') + self.__scale)
    def __sub__(self,  other): return self.__kin(self.__scale - self.__addcheck_(other, '-'))
    def __rsub__(self, other): return self.__kin(self.__addcheck_(other, '-') - self.__scale)

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
        if not ot: raise ZeroDivisionError, other
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
        s = self.__scale
        # maybe use s.bounds ?
        return (s.high - s.low) / s.best

    def _lazy_get_best_(self, which):
        """generic method for statistics, packaging those for __scale with __units """
        stat = getattr(self.__scale, which) # the statistic (e.g. best estimate) of scale
        return Quantity(stat, self.__units) # with the same units as self.

    _lazy_get_low_ = _lazy_get_high_ = _lazy_get_mean_ = _lazy_get_mode_ \
                   = _lazy_get_median_ = _lazy_get_width_ = _lazy_get_errors_ \
                   = _lazy_get_dispersor_ = _lazy_get_best_

    def _lazy_get_dispersal_(self, ignored): return self.__scale.dispersal
    def _lazy_get_variance_(self, ignored):
        return Quantity(self.__scale.variance, scaledict(self.__units, 2))

    # lazy string and representation lookups:

    def __str__(self): return self._full_str
    # short-form, e.g. 9.81 micro m.kg/s**2
    def _lazy_get__unit_str_(self, ignored): return self.unit_string()
    def _lazy_get__number_str_(self, ignored): return str(self.__scale) or '?'

    def __repr__(self): return self._full_repr
    # valid python expression, full names: e.g. 9.81 * micro * metre*kilogramme / second**2
    def _lazy_get__number_repr_(self, ignored): return `self.__scale`

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

    def unit_string(self, scale='',
                    times='.', divide='/',
                    Times=None, Divide=None,
                    lookemup=None):
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

        I hope to be able to do something smarter when I can see when to say J
        rather than kg.m.m/s/s, and etc.  But that will probably involve
        creating a units base-class to replace the present __units dictionary.
        """

        # Build reverse-lookup for self.__units:
        pows = {} # ! { power: list of units appearing with this power }
        for key, val in self.__units.items():
            try: pows[val].append(key)
            except KeyError: pows[val] = [ key ]

        # Prepare the list of powers we'll be using (in descending order):
        vals = pows.keys()
        vals.sort()
        vals.reverse()
        while 0 in vals: vals.remove(0)
        # We'll be folding x^-i^ terms in with y^i^ as (y/x)^i^ ...
        # so eliminate -i from vals if i appears in it.
        for val in vals[:]:
            if val < 0: break
            while -val in vals: vals.remove(-val)

        # Honour Times='' even with times='.'
        if Times is None: Times = times
        # but not so for division ...
        if not Divide: Divide = divide

        # Default lookup-each-item operation
        if not lookemup:
            def lookemup(row): return row

        head, tail = str(scale), ''

        for p in vals:
            # punctuate
            if p > 0:
                if head or tail: tail = tail + Times
            elif p < 0: tail = tail + Divide
            else: continue      # never happens - 0 got stripped

            # This might be a better place for the milli * kilogramme bodge ...
            row = lookemup(pows[p])
            lang, top, bot = len(row), string.joinfields(row, times), ''

            # ... do the promised folding:
            try: wor = lookemup(pows[-p])
            except KeyError: pass
            else:
                if wor:
                    bot = divide + string.joinfields(wor, divide)
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

def base_unit(nom, fullname, doc, **what):
    result = apply(Quantity, (1, {nom:1}, doc, nom, fullname), what)
    _terse_dict[nom] = result
    return result

_rcs_log = """
 $Log: quantity.py,v $
 Revision 1.14  2001-12-12 17:10:01  eddy
 untabified

 Revision 1.13  2001/12/12 17:09:23  eddy
 Pulled back qSample's _str and _repr to work via a shared method with
 alternate multiplier.  Refined the e3 exception to work with exact
 values (which are precise enough).

 Revision 1.12  2001/12/12 15:23:34  eddy
 buried _exponent_to_quantifier in argument tunnel so I can del it.
 Removed gratuitous punctuation from '2.54 * centi' as _number_str.
 Some minor tidy-up.

 Revision 1.11  2001/12/10 21:58:03  eddy
 Made abs work even when non-dimensional, lots of layout changes on
 trivial methods, fully retired _quantade_split.

 Revision 1.10  2001/12/10 21:28:11  eddy
 Added abs, dispersor, dispersal to Quantity.  Shuffled hash, nonzero.
 Enhanced various docs.  Removed bogus debug.  Fixed noddy bugs:
 punctuating str((m*s)**2*kg); _lazy_get__scalar_'s ignored arg.

 Revision 1.9  2001/10/12 16:02:06  eddy
 Removed milli*kilogramme bodge - was wrong on, e.g., 1e3 *kg*kg = 1 *gram**2
 May try again later ...

 Revision 1.8  2001/10/12 15:43:52  eddy
 fixed powered units bug omitting () from, e.g., m**2/(s*kg).

 Revision 1.7  2001/09/27 16:12:38  eddy
 Two years of evolution.

 Revision 1.6  1999/07/19 21:21:09  eddy
 Dumb typo.

 Revision 1.5  1999/07/19 21:17:32  eddy
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
