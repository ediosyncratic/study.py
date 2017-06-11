"""Polynomials.  Coefficients are assumed numeric.  Only natural powers are considered.

See study.LICENSE for copyright and license information.
"""
from study.snake.lazy import Lazy

class invalidCoefficient (TypeError): "Invalid coefficient for polynomial"
class unNaturalPower (TypeError):
    """Power of variable in polynomial is not a natural number.

    Raised if such a power is fed to the constructor or if a Polynomial is
    raised to a non-whole power (although, if it's raised to power n/d, for some
    whole n and d, and the result of raising to power n happens to be p**d for
    some p, then p is used and there's no error).\n"""

class Polynomial (Lazy):
    """Model of the mathematical ring of polynomials (in one free variable).

    Supports arithmetic (+, -, *, /, %, divmod, power, negation), comparison
    (but cmp() will yield zero in some cases where == will say no),
    representation (as a python lambda expression), evaluation (i.e. calling as
    a function), repeated integration (as <<) and differentiation (as >>), use
    as boolean (only the zero polynomial is false) and (lazy) hashing.

    Display (with str and repr) is in the form of a lambda expression; the
    variable name to use in this display is controlled by .variablename, which
    is inherited (as 'z') from the class unless over-ridden on the instance.

    Lazy attributes:
    ===============

      rank -- highest power of the free variable with non-zero coefficient
      normalised -- same polynomial scaled so rank-power's coefficient is 1
      derivative -- result of differentiating the polynomial
      assquares -- decompose as sum of scaled squares and a remainder
      sign -- like cmp(self, 0) but None when ill-defined or variable
      isreal -- are all coefficients real ?
      factors -- tuple of normalised irreducible factors and an optional scalar
      roots -- inputs at which the polynomial is zero (or very nearly so)

    The value of p.assquares is of form (bok, rem) with rem either zero or of
    odd rank and p-rem equal to sum(x*x*v for (x, v) in bok.items()); each v is
    a scalar.

    The value of sign may presently sometimes be None when it needn't be; I've
    yet to find an example, but the computation in use only deals with `easy
    enough' cases.  Note that cmp() returns zero when the difference's sign is
    None or zero; but == is true only when sign is zero (actual equality).

    Alternate constructors:
    =======================

    A few class methods are provided to return particular polynomials (rather
    than complicate the constructor with hairy special cases):

      power(n [, s=1, d=None]) -- x**n &larr;x, optionally scaled by s / d.
      Chose(gap) -- x!/(x-gap)!/gap! &larr;x
      PowerSum(k) -- sum(: i**k &larr;i |n) &larr;n
      interpolate(data) -- fit a given mapping of inputs to outputs

    See the doc-string of each for details (especially the last, for limitations
    of the present implementation).  Various other modules in the study package
    sub-class Polynomial to provide further special cases; see
    study.chemy.atomic, study.maths.Legendre and study.maths.multiangle for
    examples.

    Note that setting z = Polynomial.power(1) provides a `free variable' that
    can then be used to generate polynomials the way they are commonly written;
    Polynomial({0:1, 1:2, 3:4, 7:8}.iteritems()) can then simply be written
    8*z**7 +4*z**3 +2*z +1.  Though less computationally efficient, this may be
    easier to read.

    A few pseudo-constructor static and class methods are provided:

      _polynomial_(cs [, d, v]) -- instantiator, for derived classes to over-ride
      fromSeq(seq [, d, v]) -- Polynomial(enumerate(seq) [, d, v])
      fromMap(map [, d, v]) -- Polynomial(map.iteritems() [, d, v])

   The first of these (possibly via other class methods) is used for all new
   instances, producing raw Polynomials; if a derived class wants actions on it
   and its instances to produce its own instances, instead of raw Polynomials,
   it can over-ride to achieve that.  The other two replace type-detection
   kludges in the primary constructor (which are slated for demolition).

    Methods:
    ========

      copy([d, v]) -- a copy of self; can over-ride denominator and variablename
      coefficient(n) -- coefficient of (: x**n &larr;x :) in polynomial
      hcf([poly, ...]) -- yields highest common factor of arbitrarily many
      integral([start=0, base=0]) -- integration
      unafter(poly) -- input to poly yielding self as output
      Weierstrass([tol=1e-6]) -- finds roots
      resultant(other) -- determinant of Sylvester(other)
      Sylvester(other) -- Sylvester matrix of self and another polynomial
      Bezout(other) -- B&eacute;zout matrix of self and another polynomial

    See individual methods' docs for details.\n"""

    __private_member_doc = """Private members: __coefs and __denom


    The polynomial represented is

        lambda x: sum(c * x**n for n, c in .__coefs.items()) / .__denom

    except that a value of None for .__denom is used to encode a value of 1,
    skipping the need for the division.  I presently entertain the delusion that
    the code avoids some unnecessary work when .__denom is None, but it is
    possible that using 1 would be just as efficient while saving a bunch of
    mess implied in checking for None and treating it specially.

    Separating out the denominator makes it possible to retain the full
    precision of integer arithmetic, without any floating point rounding
    artefacts.  This matters for the many polynomials that, although they have
    non-whole rational coefficients, none the less map whole number inputs to
    whole number outputs.  The returns from .Chose() and .PowerSum() illustrate
    this.  Deferring division by the denominator until all other computation is
    finished ensures that we get whole number answers when we should.  It
    incidentally also ensures that we retain as much precision as possible even
    when the answer has a fractional part.

    While it is usual for .__denom to be a whole number, it is perfectly
    reasonable for it to be a real or complex value: this lets us retain whole
    number coefficients (and operate precisely with them) while deferring the
    floating point computation to the last possible moment.  However, if two
    polynomials with non-whole denominators are added or subtracted (so this
    also applies to divmod), the case where their denonimators are rationally
    commensurate produces (for the same reason that non-whole real coefficients
    aren't coerced to rational form - see constructor) a result with non-whole
    coefficients and denominator None.
"""

    def __init__(self, coeffs, denominator=None, variate=None):
        """Constructor.

        Required argument, coeffs, specifies the coefficient of each power.  The
        preferred form for this is an iterator (it shall be iterated once; fully
        unless construction takes exception to some key or value) yielding key,
        value pairs; the constructed Polynomial shall then represent lambda z:
        sum(c * z**n for n, c in coeffs).  Each key must be a natural
        (non-negative int or long); otherwise, you'll get an unNaturalPower
        exception (which isa TypeError).  Each value should be of some suitably
        number-like type to be used as a coefficient (supporting .__add__() and
        .__mul__() suffices); if it's not number-like enough, you'll get an
        invalidCoefficient exception (which isa TypeError).

        For the sake of backwards compatibility, coeffs may also be a mapping or
        a sequence.  If coeffs is iter(coeffs) it is treated as an interator
        (the preferred form); otherwise, if it supports a [:] slice it is
        presumed to be a sequence (enumerate(coeffs) is used); failing that it
        is presumed to be a mapping (coeffs.iteritems() is used); if these
        type-detection heuristics misinterpret what you passed you get to fix
        the resulting mess yourself; caveat invokator.  Reliance on this
        type-detection is deprecated in favour of the .fromSeq() and .fromMap()
        pseudo-constructors; callers should use these or pass an actual iterator
        rather than relying on the present kludgery.

        Optional arguments (with defaults):
          denominator (None): specifies an over-all scaling
          variate (None): specifies the variable name to use in representation

        If denominator is specified, and not None, the resulting Polynomial is
        effectively 1/denominator times what it would have been without;
        however, instead of actually dividing coefficients by denominator, which
        may involve rounding errors, the denominator is tracked separately from
        the coefficients, if any of them are whole numbers (or, if complex, have
        whole number real or imaginary part).  Any common factor shared with all
        such whole numbers is eliminated.  If no coefficients are whole numbers,
        all are simply divided by the given denominator and the behaviour is as
        if no denominator were specified.

        This last is defective for the case where coefficients represent linear
        maps with whole-number co-ordinates, polynomials with whole-number
        coefficients or rationals; these and kindred numeric types in
        whole-number forms, optionally with whole-number denominators, should
        also be treated as whole-numbered in some way, but this would involve
        designing some non-trivial APIs (with the right level of generality,
        without going overboard) by which they can hook into such
        support.  Aside from this, reasonable care is taken to handle anything,
        as coefficients, that supports the usual arithmetic operations; so
        representations of endomorphisms, polynomials and more exotic things
        should work, as far as is sensible for their types; however, this has
        not been extensively tested, so you may need to fix implementations if
        your coefficients are more interesting than plain numeric types !

        If variate is specified, and has a true value, it is used as the name of
        the variable in the polynomial's representation as a string: it should
        be a non-empty string and is used as the new instance's .variablename;
        the same effect can be achieved by setting .variablename after
        instantiation (which permanently over-rides any variate passed to the
        constructor).  By default, str() and repr() use 'z' as the variable, but
        specifying an alternate name, e.g. 'x', by either of these means shall
        substitute that name for the default.

        See also: the alternate constructors listed in the class doc.\n"""

        self.__coefs = {}       # dictionary with natural keys
        try:
            if coeffs is iter(coeffs): ps = coeffs
            else:
                coeffs[:] # is it a sequence ?
                ps = enumerate(coeffs) # apparently so.
        except (AttributeError, TypeError, KeyError): # non-sequence arg
            try: coeffs.items, coeffs.get(0, None) # is it a mapping ?
            except AttributeError:
                raise TypeError('First argument should be iterable', coeffs)
            else: ps = coeffs.iteritems()

        v = 1. # fall-back if ps empty, for use when testing denominator
        for k, v in ps: self.__store(k, v)

        if denominator is None or denominator == 1: self.__denom = None
        else:
            v / denominator # raise suitable error if unsuitable as denominator !
            self.__descale(denominator) # normalise and set .__denom

        # self.__coefs and self.__denom should now be construed as immutable.

        if variate: self.variablename = variate

    def copy(self, denominator=None, variate=None):
        """Copy constructor (mostly for internal use).

        Takes optional arguments, denominator and variate, as for the
        constructor; these over-ride self's denominator and variablename in the
        copy.  Note that over-riding denominator uses self's (invisible to
        anything outside this class) internal coefficients, ignoring the (also
        invisible from outside) denominator that scales them, so anything but
        this class doesn't know what it's scaling by if it does so !\n"""
        if denom is None: denom = self.__denom
        if variate is None:
            # Consult self.__dict__, to avoid inheriting from class:
            try: variate = self.__dict__['variablename']
            except KeyError: pass

        return self.fromMap(self.__coefs, denom, variate)

    def ifint(v): # tool for __wholes
        try: i = int(v)
        except (TypeError, ValueError, ArithmeticError): pass
        else:
            if i and i == v: yield i

    @staticmethod
    def __wholes(cs, maybe=ifint):
        for c in cs:
            try: im, re = c.imag, c.real
            except AttributeError:
                for i in maybe(c): yield i
            else:
                for i in maybe(im): yield i
                for i in maybe(re): yield i
    del ifint

    from natural import gcd
    def __descale(self, denom, hcf=gcd):
        """Eliminate common factor from denominator and whole coefficients.

        Called after setting all coefficients from inputs, if there is a
        denominator by which to divide them; single argument is that
        denominator.  After this call, the instance should be deemed immutable:
        nothing should modify .__coeffs or .__denom.

        This method considers denom in combination with each whole number that
        appears as a real coefficient or as the real or imaginary part of a
        complex one.  If none of these are whole, tracking a denominator shall
        do us no good, so .__denom is set to None and all coefficients are
        divided by denom.  Otherwise, we compute the highest common factor of
        denom (in so far as it's whole) and of the whole values found; .__denom
        is set to denom divided by this factor and all coefficients are divided
        by it; this should not lead to any rounding errors.

        Note that no attempt is made to detect coefficients which might be
        rationals being poorly represented by reals; doing so would cause bugs
        when we come to look at polynomials which deliberately have coefficients
        slightly different from actual rationals.  If reals are to be replaced
        by rationals, that's a job for the caller of the constructor.\n"""

        d = 0 # its hcf with any whole number is that number
        for c in self.__wholes((denom,)):
            d = hcf(c, d)

        # If denom isn't whole (or a complex with some non-zero part whole) it's
        # still desirable to track it *if* some coefficients have whole parts.

        w = False # Do any coefficients have whole parts ?
        for c in self.__wholes(self.__coefs.itervalues()):
            d, w = hcf(c, d), True
            if w and d in (1, -1): break # pointless to continue

        # TODO: we might have a complex with whole real and imaginary parts that
        # we can divide all coefficients by without losing any wholeness; we've
        # reduced d to the hcf of that factor's real and imaginary parts, where
        # it would have been better to track that factor, at least if dividing
        # denom by it wouldn't lose any wholeness.  Note that some whole
        # multiples of such a complex whole value may be real whole values.

        self.__denom = None # We'll either divide out denom or revise this.
        if w:
            assert d
            try: # to end up with +ve .__denom if we can:
                if d * denom < 0: d = -d
            except TypeError: pass # complex
            if d != denom:  self.__denom = denom / d
        else: d = denom # no wholeness among coefficients; just divide denom out

        if d != 1: # we've already taken it into account in .__denom
            self.__coefs = dict((k, v / d) for k, v in self.__coefs.iteritems())

        # Ideally we would now "freeze" .__coefs

    # Coefficients only require the ability to add, multiply and divmod.
    def get_coeff(val, oktypes=(int, long, float, complex)): # transient tool
        if isinstance(val, oktypes):
            try:
                if val.imag: return val

                val = val.real # complex, but real none the less
            except AttributeError: pass # not complex
            v = int(val)
            return v if v == val else val

        elif hasattr(val, '__add__') and hasattr(val, '__mul__'):
            # This means we can use polynomials as coefficients ...
            # likewise, linear maps, &c.
            return val

        else: raise invalidCoefficient

    def __store(self, power, coeff, g=get_coeff):
        if not self.__iswhole(power) or power < 0:
            raise unNaturalPower(power)

        if coeff: self.__coefs[power] = g(coeff)
        else:
            try: self._zero = g(coeff)
            except invalidCoefficient: pass
    del get_coeff

    @staticmethod
    def __iswhole(k, i=(int, long)): return isinstance(k, i)

    def __len__(self): return len(self.__coefs)
    def _lazy_get_rank_(self, ignored):
        """The highest power present in self.

        This is the largest natural n for which self(x) includes a term v * x**n
        with v non-zero; if there is no such n (i.e. self is zero) the formal
        rank -1 is used.  This ensures that each non-zero polynomial's
        .derivative.rank is one less than it own .rank.\n"""
        top = -1
        for k, v in self.__coefs.items():
            if not v:
                assert False, 'How did that get past __store ?'
                del self.__coefs[k]
            elif k > top: top = k

        return top

    def coefficient(self, key):
        """Coefficient of a given power.

        The polynomial self represents is lambda x: sum(self.coefficient(k) *
        x**k for k in range(self.rank+1)).\n"""
        return self.__numerator(key, self.__denom)

    def __numerator(self, key, om=None):
        """Raw coefficient of power(key), preferrably as an int.

        Required argument, key, is a natural number.  Optional second argument
        is a scaling by which to divide the coefficient.  Returns the (suitably
        scaled) raw coefficient of power(key), without taking into account
        self.__denom (see .coefficient(key) for that).  If the result is a whole
        number, it's returned as an int; otherwise, as whatever it is (float,
        complex, whatever).\n"""
        val = self.__coefs.get(key, self._zero)
        if om is not None: val *= 1. / om
        try:
            i = int(val)
            if val == i: return i
        except (TypeError, AttributeError): pass
        return val

    def hcf(self, *others):
        for other in others:
            # Euclid's algorithm
            while other: self, other = other, self % other
        return self

    def coprime(self, *others):
        # coprime <=> hcf is a non-zero scalar
        return self.hcf(others).rank == 0

    def _lazy_get_normalised_(self, ignored):
        if self.rank < 0: raise ValueError("Can't normalise zero")
        scale = self.__coefs[self.rank]
        if scale == (self.__denom or 1): return self
        return self.copy(scale)

    variablename = 'z' # over-ride to taste, on each instance
    def __repr__(self):
        try: return self.__repr
        except AttributeError: pass

        names = [ self.variablename ]
        text = self.__represent(names, 0) # may grow names !

        lamb = 'lambda %s: ' % ', '.join(names)
        if not text: ans = lamb + '0'
        else: ans = lamb + text

        self.__repr = ans
        return ans

    def format(num, names, depth): # tool function for __represent
        try:
            if num.imag == 0: num = num.real
        except AttributeError: pass

        try: return num.__represent(names, 1+depth)
        except AttributeError: return str(num)

    def __represent(self, names, depth, fmt=format,
                    ones=('1', '1.0', '(1+0j)'), mons=('-1', '-1.0', '(-1+0j)')):
        if depth > 52: raise ValueError("We're going to run out of names !")
        while depth >= len(names):
            if names[-1][0] == 'a': names.append('Z')
            elif names[-1][0] == 'A': names.append('z')
            else: names.append(chr(ord(names[-1][0]) - 1))

        result, name = '', names[depth]
        for key in self._powers:
            val = self.__numerator(key)
            frag = fmt(val, names, depth)
            if key:
                if frag in ones: frag = ''
                elif frag in mons: frag = '-'
                else:
                    # TODO: avoid (...) if already present, but not e.g. (...)+(...)
                    if ' ' in frag or '+' in frag[1:] or '-' in frag[1:]:
                        frag = '(' + frag + ')*'
                    else: frag += '*'

                if key == 1: frag += name
                else: frag += '%s**%d' % (name, key)

            if not result: result = frag
            elif frag[:1] in ('-', '+'): result += ' ' + frag
            else: result += ' +' + frag
        assert not result.startswith(' +') and not result[:1].isspace()

        om = self.__denom
        if om is not None and om != 1:
            # TODO: avoid (...) when redundant, as above
            if ' ' in result: result = '(' + result + ')/'
            else: result += '/'
            try:
                if om.imag == 0: om = om.real
            except AttributeError: pass
            frag = str(om)
            # TODO: avoid (...) when redundant, as above
            if ' ' in frag or '+' in frag[1:] or '-' in frag[1:]:
                result += '(' + frag + ')'
            else: result += frag

        return result

    del format

    def __eachattr(self, each):
        bok = {}
        for k, v in self.__coefs.items(): bok[k] = each(v)
        return self.fromMap(bok, self.__denom)

    def toreal(val):
        try: return val.real
        except AttributeError: return val

    def _lazy_get_real_(self, ignored, fn=toreal):
        return self.__eachattr(fn)

    del toreal
    def toimag(val):
        try: return val.imag
        except AttributeError: return 0

    def _lazy_get_imag_(self, ignored, fn=toimag):
        return self.__eachattr(fn)

    del toimag
    def conjug8(val):
        try: return val.conjugate
        except AttributeError: return val

    def _lazy_get_conjugate_(self, ignored, fn=conjug8):
        return self.__eachattr(fn)

    del conjug8

    def _lazy_get__powers_(self, ignored):
        keys = self.__coefs.keys()
        keys.sort()
        keys.reverse()
        return tuple(keys)

    def __add__(self, other):
        try: tot, den = other.__coefs.copy(), other.__denom
        except AttributeError: tot, den = {0: other}, None
        om = self.__denom
        if om is None: denom = den
        else:
            if den is None: denom = om
            else: denom = den * om
            for k in tot.keys(): tot[k] *= om
        if den is None: den = 1

        for k, v in self.__coefs.items():
            prod = v * den
            try: was = tot[k]
            except KeyError: tot[k] = prod
            else: tot[k] = was + prod

        return self.fromMap(tot, denom)

    __radd__ = __add__
    def __pos__(self): return self
    def __neg__(self, neg=lambda v: -v): return self.__eachattr(neg)
    def __sub__(self, other): return self + (- other)
    def __rsub__(self, other): return other + (- self)

    def __mul__(self, other):
        term = {}
        try: bok, den = other.__coefs, other.__denom
        except AttributeError:
            return self._polynomial_(((key, val * other)
                                      for key, val in self.__coefs.iteritems()),
                                     self.__denom)

        for key, val in self.__coefs.items():
            for cle, lue in bok.items():
                tot, prod = key + cle, val * lue
                try: was = term[tot]
                except KeyError: term[tot] = prod
                else: term[tot] = was + prod

        om = self.__denom
        if den is None: denom = om
        elif om is None: denom = den
        else: denom = om * den

        return self.fromMap(term, denom)

    __rmul__ = __mul__ # abelian multiplication
    def __div__(self, other):
        q, r = self.__divmod__(other)
        try: n = other.rank - 1
        except AttributeError: n = self.rank
        else: n = max(n, self.rank)
        if r and not r.__istiny(self._bigcoef, max(0, n)):
            raise ValueError(self, 'not a multiple of', other, r)
        return q
    __floordiv__ = __div__

    def __mod__(self, other): return self.__divmod__(other)[1]

    def __divmod__(self, other, hcf=gcd):
        """Solves self = q.other + r for r of rank < other.rank: returns (q, r)

        This depends on our coefficients forming a field.
        """
        try: top, den = other.rank, other.__denom
        except AttributeError:
            if other: other, top, den = self.power(0, other), 0, 1
            else: raise ZeroDivisionError(other)
        else:
            if top < 0: raise ZeroDivisionError(other)
            if den is None: den = 1

        o = other.__numerator(top)
        q = self.power(0, self._zero)
        if top == 0:
            return self._polynomial_(((k, den * v) for k, v in self.__coefs.items()),
                                     o * (self.__denom or 1)), q

        r, got = self, self.rank

        # We now reduce the rank of r (by at least 1) at each iteration, by
        # shifting other*x**(got-top) times a scalar from r to q*other; thus, as
        # r.rank is finite, it must eventually descend below top > 0.
        while got >= top:
            m, om = r.__numerator(got), r.__denom
            while m: # take several slices off, in case of arithmetic blur ...
                scale = self.power(got - top, m * den, o * (om or 1))
                q, r = q + scale, r - scale * other
                n, om = r.__numerator(got), r.__denom
                if abs(n) * 10 > abs(m): # drowning in rounding errors :-(
                    print "Wiping %g x**%d in Polynomial.__divmod__" % (
                        r.__coefs[got], got )
                    del r.__coefs[got]  # => forcefully set to zero
                    break
                m = n
            assert r.rank < got
            got = r.rank

        assert self.__close_enough(r + q * other)
        return q, r

    from continued import rationalize
    def ratcom(num, rat=rationalize, hcf=gcd):
        """Expresses a (possibly complex) number in rational form.

        Single argument is the number.  If it can be approximated by n / d, with
        d natural and n either an integer or a complex number with at least one
        of its real and imaginary parts a whole number, then the pair n, d is
        returned.  Otherwise, a ValueError is raised.\n"""

        try:
            try: q, r = rat(num.imag)
            except ValueError:
                if num.real == 0: raise
                q = 0 # sentinel
            else:
                if q == 0:
                    num = num.real
                    raise AttributeError # to handle as the simple real it is
                q *= 1j
                if num.real == 0: return q, r
            # so now, if q == 0, r is unset; otherwise q/r approximates 1j * num.imag

            try: n, d = rat(num.real)
            except ValueError:
                if q == 0: raise # neither part is well-approximated
                n, d = num.real * r + q, r
            else:
                if q == 0: n += 1j * num.imag * d
                else:
                    i = hcf(d, r)
                    n = n * r / i + 1j * q * d / i
                    d *= r / i
        except AttributeError:
            n, d = rat(num)

        if d < 0: return -n, -d
        return n, d

    from cardan import cubic
    def _lazy_get_roots_(self, ignored, cub=cubic, rat=ratcom):
        if self.rank < 1: return ()
        if self.rank < 3:
            # for quadratics, we know how to be exact ...
            return cub(*[self.__numerator(i) for i in (3, 2, 1, 0)])
        elif self.rank == 3 and self.__pure_real():
            # and, for cubics, pretty accurate:
            rough, tol = cub(*[self.__numerator(i) for i in (3, 2, 1, 0)]), 1e-9
        else:
            rough, tol = self.Weierstrass(1e-9), 1e-7

        # Now try to refine our rough calculation; if any of them is almost an
        # exact rational, check to see if that rational yields a factor; if so,
        # record the rational and divide out the factor.
        ans = []
        # First, deal with any obvious approximations to rationals:
        for v in rough:
            # Try to approximate v as n / d, with n and d whole:
            try: n, d = rat(v)
            except ValueError: continue # give up on this one
            q, r = divmod(self, self.fromSeq((-n, d)))
            if r.rank < 0:
                if d == 1: ans.append(n)
                else: ans.append(n * 1. / d)
                self = q

        # TODO: see if we have any conjugate pairs we can resolve ?

        # If we refined any rough root, we also refined self:
        if ans:
            ans += list(self.roots) # recurse
            # sort - but complex has no order, so order by real part; but, to be
            # able to select .real, we have to treat all entries as complex:
            ans.sort(key = lambda x: (x + 0j).real)
            return tuple(ans)
        return tuple(rough)
    del cubic

    def scalarroot(val, exp, odd):
        assert val
        try:
            if val > 0: pass
            elif odd: return - ((-val)**exp)
            else: val = val + 0j # even root of -ve; coerce to complex
        except TypeError: pass # complex

        return val ** exp

    def __root(self, num, mod=None, root=scalarroot, rat=ratcom):
        """Solves self = ans ** num for a num-th root of self.

        Bug: presently ignores its second parameter, mod; should
        implement solving its problem modulo this, if non-None; but
        that's non-trivial.  Needs ans**num == self + p * mod for some
        p (reducing ans %= mod, which just frobs p).
        """
        if num < 0:
            if self.rank < 0:
                raise ValueError("Zero polynomial isn't an inverse power", num, self)

            if self.rank > 0:
                raise ValueError("Non-constant polynomial isn't an inverse power", num, self)

            # OK, self is just a number, we can inverse-power it:
            n, d, p = self.__coefs[0], self.__denom, -1. / num
            d = 1 if d is None else d ** p
            # Express result as a poly, even though it's really just a number:
            return self.power(0, d, n ** p)

        if not num: # The only self == ans ** 0 is self == 1:
            d = self.__denom
            if not self.rank and self.__coefs[0] == (1 if d is None else d):
                return self.power(0) # any non-zero poly will do
            raise ValueError('Only 1 is the 0-th power of anything', num, self)

        top, res = divmod(self.rank, num)
        if res: raise ValueError(num, mod, self)

        res = root(self.coefficient(self.rank), 1. / num, num % 2)
        try:
            n, d = rat(res)
            if n**num * (self.__denom or 1) != d**num * self.__numerator(self.rank):
                raise ValueError # heigh ho, not good enough, fall back on real:
        except ValueError: ans = self.power(top, res)
        else: ans = self.power(top, n, d)

        res = self - ans ** num
        while top > 0 and res:
            next = res.rank - (num - 1) * ans.rank
            if next < top: top = next
            else: raise ValueError
            ans += self.power(top, res.coefficient(res.rank),
                              num * ans.coefficient(ans.rank)**(num-1))
            res = self - ans ** num

        if res: raise ValueError
        return ans
    del scalarroot, ratcom

    @classmethod
    def __pow_factor(cls, exponent, rank,
                     rat=rationalize, hcf=gcd):
        """Deal with non-whole exponents passed to power.

        Returns a pair n, d for which n/d is (at least a respactably good
        approximation to) exponent.  Favours factors of rank as factors of d,
        since these are easier to cope with in .__root().  This gives us as good
        a chance as we can hope for at getting suitable fractional powers of
        polynomials.\n"""
        if cls.__iswhole(exponent): return exponent, 1 # easy :-)
        n, d = rat(exponent * rank) # ValueError if not possible
        i = hcf(n, rank) # preparing to take the factor of rank back out again
        if d * i < 0: i = -i
        return n // i, d * rank // i

    del rationalize, gcd

    def __pow__(self, other, mod=None):
        """Raising to a power, optionally modulo some given polynomial.

        Includes partial support for fractional powers"""
        if mod is None:
            wer, result = self, 1
            def step(b, x, r):
                if b: r *= x
                return x * x, r
        else:
            wer, result = self % mod, 1 % mod
            def step(b, x, r, m=mod):
                if b: r = (r * x) % mod
                return (x * x) % mod, r

        # Undo .__coerce__()'s wrapping (and allow other to be a Polynomial, as
        # long as it's constant):
        try:
            if other.rank < 1: other = other.coefficient(0)
        except AttributeError: pass
        # Require other to be natural
        try:
            if other < 0:
                if wer.rank != 0: raise TypeError
                return self.fromSeq((pow(wer.__denom, other),),
                                    pow(wer.__numerator(0), other))
            if wer.rank < 0: return self # zero**non-negative is zero
            other, d = self.__pow_factor(other, self.rank)
            # so other == n / d, give or take rounding.

        except (AttributeError, TypeError, ValueError):
            raise unNaturalPower(other)

        while other >= 1:
            other, b = divmod(other, 2)
            wer, result = step(b, wer, result)
        if d > 1: result = result.__root(d, mod) # ValueError if not possible

        return result

    # use << as repeated integration, >> as repeated differentiation
    def __lshift__(self, other):
        """f << n -> nth integral of f"""
        1L << other # evaluate to raise suitable error if any
        while other > 0:
            self, other = self.integral(), other - 1
        return self

    def __rshift__(self, other):
        """f >> n -> nth derivative of f"""
        1L >> other # evaluate in order to raise suitable error if any
        while other > 0:
            self, other = self.derivative, other - 1
        return self

    def _lazy_get_derivative_(self, ignored):
        """Differentiate a polynomial. """
        return self._polynomial_(((k - 1, k * v) for k, v in self.__coefs.items() if k),
                                 self.__denom)

    def integral(self, start=0, base=0):
        """Integrate a polynomial.

        Optional arguments, start and base, specify the constant of integration;
        the integral's value at start (whose default is 0) will be base (whose
        default is also zero).  Note that, if f = self.integral(), then
        self.integral(s, b) is equivalent to f + b - f(s); and
        self.integral(start)(end) is the integral of self from start to end.  If
        either start or base is (when considered as a boolean) false it is
        ignored; so the defaults won't present a problem if self's coefficients
        are of some more interesting type than simple numbers.\n"""

        fac = reduce(lambda x, y: x * y, (1 + k for k in self.__coefs.keys()), 1)
        den = self.__denom
        if den is None: den = fac
        else: den *= fac
        ans = self._polynomial_(((1 + k, v * fac / (1 + k))
                                 for k, v in self.__coefs.iteritems()),
                                den)
        # assert: ans(0) == 0
        if start: ans -= ans(start)
        # assert: ans(start) == 0
        if base: ans += base
        # assert: ans(start) == base
        return ans

    # assert: self.integral(s, b).derivative == self, for all s, b
    # assert: self.derivative.integral(x, self(x)) == self, for any x

    def sum(self, start=0, base=0):
        """Returns lambda n: sum(self(i) for i in range(n))

        This is the discrete equivalent of integral.  Also accepts optional
        arguments, start and base, as for integral(): each defaults to zero and,
        if f is the result without them, when they're specified the result is f
        - f(start) + base.  See delta for the inverse operation.\n"""

        ps = (v * self.PowerSum(k) for k, v in self.__coefs.iteritems())
        try: ans = ps.next()
        except StopIteration: ans = self.power(0, self._zero)
        else: ans = sum(ps, ans)

        om = self.__denom
        if om is not None: ans /= om

        if start: ans -= ans(start)
        if base: ans += base
        return ans

    def _lazy_get_delta_(self, ignored, cache=[]):
        """Returns lambda n: self(1+n) - self(n)

        This is the discrete equivalent of differentiation, the taking of
        'finite differences'.  Note that, for any s and b:
        self.sum(s, b).delta == self == self.delta.sum(0, self(0)).\n"""
        try: offset = cache[0]
        except IndexError:
            offset = self.fromSeq((1, 1))
            cache.append(offset)
        return self(offset) - self

    # non-zero: safe and unambiguous
    def __nonzero__(self): return self.rank >= 0
    # Comparison: of debatable value
    def __cmp__(self, other): return (self - other).sign or 0
    # *Stronger* test for equality ...
    def __eq__(self, other): return (self - other).rank < 0

    def __coerce__(self, other):
        try:
            if isinstance(other, Polynomial): return self, other
        except AttributeError: pass

        try:
            # TODO: when we kill the heuristics in the constructor, move them here !
            try: return self, self._polynomial_(other)
            except ValueError: return self, self.fromSeq((other,))
        except (unNaturalPower, invalidCoefficient): return None

    # Only useful if we want polys as keys in dictionaries ... but see .assquares
    def _lazy_get__lazy_hash_(self, ignored):
        if self.__denom is None: om = 1
        else: om = self.__denom
        result = 0
        for key, val in self.__coefs.items():
            r = val / om
            if r * om == val: val = r
            else: val *= 1. / om
            result = result ^ hash(key) ^ hash(val)
        return result

    # How to evaluate a polynomial ...
    # For any T supporting
    # ({(T: :T)}: * :T), ({(V::T)}: * :{coefficients}) and ({(V: :V)}: + :V)
    # evaluation is ({(V: :T)}: :{polynomials})
    # but the following should also suffice ...
    def __call__(self, arg):
        """Evaluate a polynomial as a function

        For a polynomial, p, with coefficients in some domain V, and a value t
        in some domain T supporting *: T-> V-> V and +: V-> V-> V, we can
        evaluate p(t) by substituting t in as the value of p's formal
        parameter.\n"""

        keys = iter(self._powers) # highest first
        try: top = keys.next()
        except StopIteration: return self._zero
        result, om = self.__numerator(top), self.__denom

        for key in keys:
            assert top > key
            result *= pow(arg, top - key)
            top = key
            result += self.__numerator(key)
        if top: result *= pow(arg, top)

        if om is not None:
            r = result / om
            if r * om == result: return r
            result *= 1. / om

        return result

    # Calling one polynomial with another as input yields the composite of the two;
    # the following explores undoing that:

    def unafter(self, other):
        """Returns a polynomial which, if fed other, will yield self.

        The nature of polynomial arithmetic is such that, if f and g are
        polynomials, their composite (: g(f(x)) &larr;x :) is simply g(f).  This
        method seeks to express self as g(other) for some g.  Requires rank of
        self to be a multiple of rank of other, among other things; raises
        ValueError if the goal can't be met.\n"""

        residue, result = self, {}

        while residue:
            if residue.rank % other.rank:
                raise ValueError('Cannot factorise', residue, 'via', other)
            p = residue.rank / other.rank
            q, residue = divmod(residue, other ** p)
            assert q.rank == 0
            assert residue.rank < p * other.rank
            result[p] = q.coefficient(0)

        return Polynomial(result)

    # unbefore would seem equivalent to arbitrary root-finding !

    def _lazy_get_Gamma_(self, ignored):
        """Integrates self * (: exp(-t) &larr;t :{positives}).

        When self is power(n) the result is Gamma(n+1) = n!, so the result is
        just the sum of self's coefficients, each multiplied by the factorial of
        its order.  The name is slighly misguided but not unreasonable.  See
        atomic.py for a sub-class using this.\n"""

        n, om = self.rank, self.__denom
        ans = self.__numerator(n)
        while n > 0:
            ans, n = ans * n, n - 1
            ans = ans + self.__numerator(n)

        if om is not None:
            r = ans / om
            if r * om == ans: return r
            ans *= 1. / om

        return ans

    from natural import sqrt
    def intassquare(n, s=sqrt):
        d, m, i = n, n, s(n)
        # invariant: d**2 = m * n and i**2 <= m
        while i > 1 < m:
            q, r = divmod(m, i**2)
            if r == 0:
                m = q
                q, r = divmod(d, i)
                assert r == 0
                d = q
                if i**2 > m > 1: i = s(m)
                else: i -= 1
            else: i -= 1

        assert d**2 == m * n
        return d, m
    del sqrt

    def _lazy_get_assquares_(self, ignored, squint=intassquare):
        """Decompose self as a sum of scaled squares, as far as possible.

        Returns a twople, (bok, poly) in which: poly is zero or a polynomial of
        odd rank; bok is a mapping from polynomials to scalars; if each key of
        bok is squared and multiplied by the corresponding value, summing the
        results and adding poly will yield self.

        Unfortunately, by the experiment of adding a sum of multiples of squares
        of polynomials and asking the result for its .assquares, I find the this
        algorithm is apt to leave a linear residue when an exact decomposition
        into squares is actually possible.\n"""

        bok, rem, i = {}, self, self.rank
        while i % 2 == 0:
            k, i = rem.__numerator(i), i / 2
            assert k != 0
            if rem.__denom is None: d = 1
            else:
                d, n = squint(rem.__denom) # denom * n = d**2
                k *= n
            z = self.power(i, 1, d)

            while i > 0:
                i = i - 1
                q, ign = divmod(rem - k*z*z, 2*z*k)
                assert q.rank <= i
                z = z + self.power(i, q.__numerator(i), q.__denom)

            bok[z], rem = k, rem - k * z * z
            i = rem.rank

        assert self == rem + sum(x*x*k for (x, k) in bok.items())
        return bok, rem

    del intassquare

    def __pure_real(self):
        for v in self.__coefs.values():
            try:
                if v.imag: return False
            except AttributeError: pass

        return True

    def _lazy_get_isreal_(self, ignored): return self.__pure_real()
    def _lazy_get__zero_(self, ignored):
        try: return self.__coefs.values()[0] * 0
        except IndexError: return 0

    def _lazy_get_sign_(self, ignored):
        if self.rank < 0: return 0 # definitely everywhere zero
        if self.rank % 1 or not self.isreal: return None
        if self.rank == 0: return cmp(self.coefficient(0), 0)
        b, r = self.assquares
        if r.rank > 0: return None
        row = b.values()
        if r.rank == 0: row.append(r.coefficient(0))
        lo, hi = min(row), max(row)
        if lo > 0: return +1 # definitely everywhere positive
        if hi < 0: return -1 # definitely everywhere negative

        return None # this is over-cautious

    def Weierstrass(self, tol=1e-6):
        """Seeks roots by the method of Weierstrass, a.k.a. Durand-Kerner method.

        Single optional argument, tol, is a tolerance for determining
        convergence, with default one part in a million: the iteration converges
        when the largest change in any estimated root, divided by the largest
        modulus root estimate, is less than tol.  Returns a list of length
        self.rank, each entry in which is a root.

        See [[Wikipedia:Durand-Kerner method]].  We know, aside from an over-all
        scaling, self(x) = product(: x-r[i] &larr;i :) for some list r of self's
        roots, albethey unknown; we can re-arrange this, for any j in len(r),
        to: r[j] = x -self(x)/product(: x-r[i] &larr;i, i != j :) for all x.
        Furthermore, if we take this expression as a function in x computed
        using only approximations to the r[i], i != j, we still find the
        function evaluates, near r[j], to r[j].  We can thus make up some stray
        numbers to use as an initial list r, then compute revised values for
        each r[j] from these and iterate until the values are stable.  The
        iteration step has to be modified for repeated roots, of course.\n"""

        # Initialize r arbitrarily but reasonably diversely:
        k = (2j -.1) ** (2./self.rank)
        r = [ k**(2 * i + 1) for k in range(self.rank) ]
        lead = 1. / self.__coefs[self.rank] # Scales self so leading coefficient is 1

        # Ensure ensible tol, initialize k so first iteration goes ahead
        tol = abs(tol)
        k = 1 + tol
        while k > tol:
            p, d, m, i = [], 0, 0, 0

            while i < self.rank:
                x, e, n, j = r[i], self(r[i]) * lead, 0, 0
                while j < self.rank:
                    if j == i or r[j] == x: n += 1
                    else: e /= x - r[j]
                    j += 1

                if n > 1: e = e ** (1./n)
                else: assert n == 1

                a = abs(e)
                if a > d: d = a

                p.append(x - e)
                a = abs(p[i])
                if a > m: m = a

                i += 1

            if m == 0: m = 1
            r, k = p, d * 1. / m

        return tuple(r)

    def _lazy_get__bigcoef_(self, ignored):
        scale, big = self.__denom, max(abs(i) for i in self.__coefs.values())
        if scale is not None: big *= 1. / scale
        return big

    def __close_enough(self, other, tol=1e-6):
        """Tests whether self and other are equal to within plausible rounding."""
        try: tot, den = other.__coefs, other.__denom
        except AttributeError: tot, den = {0: other}, None
        bok, siz = self.__coefs, self.__denom
        return (set(tot) == set(bok) and
                all(abs(me - yo) < tol * (abs(me) + abs(yo))
                    for me, yo in ((bok[k] * den, tot[k] * siz)
                                   for k in bok)))

    def __istiny(self, scale=1, maxrank=0):
        if self.rank > maxrank: return False
        return self.rank < 0 or self._bigcoef < scale * 1e-6

    def _lazy_get_factors_(self, ignored):
        """self == product(self.factors)"""
        ans = [self.fromSeq((-r, 1)) for r in self.roots]
        lead, den = self.__coefs[self.rank], self.__denom
        if den is None:
            if lead != 1: ans.append(lead)
        elif lead != den: ans.append(lead * 1. / den)
        return tuple(ans)

    def resultant(self, other):
        """See .Sylvester(other); this is its determinant.\n"""
        return reduce(lambda x, y: x * y, (other(z) for z in self.roots),
                      self.coefficient(self.rank) ** other.rank)

    def __Sylvester(self, i):
        ans = []
        while i > 0:
            i -= 1
            if ans: row = row[-1:] + row[:-1]
            else: row = [self.coefficient(j) for j in range(self.rank, -1, -1)] + [ 0 ] * i
            ans.append(tuple(row))

        return tuple(ans)

    def Sylvester(self, other):
        """The Sylvester matrix associated with self and other.

        See http://en.wikipedia.org/wiki/Sylvester_matrix - the metrix is
        singular if the two polynomials have a non-constant common factor; its
        determinant is self.resultant(other).\n"""

        return self.__Sylvester(other.rank) + other.__Sylvester(self.rank)

    # assert: self.resultant(other) == determinant(self.Sylvester(other))

    def Bezout(self, other):
        """The B&eacute;zout matrix of two polynomials.

        See http://en.wikipedia.org/wiki/Bezout_matrix\n"""
        n, i, ans = max(self.rank, other.rank), 0, []
        while i < n:
            j, row = 0, []
            while j < n:
                s, k = 0, 0

                while k <= i and j + k < n:
                    s += self.coefficient(j + k + 1) * other.coefficient(i - k)
                    s -= other.coefficient(j + k + 1) * self.coefficient(i - k)
                    k += 1

                row.append(s)
                j += 1

            ans.append(tuple(row))
            i += 1

        # assert: determinant(ans) == self.resultant
        return tuple(ans)

    @staticmethod
    def _polynomial_(coeffs, denom=None, variate=None):
        """Meta-constructor that derived classes may want to over-ride.

        Where this class instantiates itself, it does so via this method, with
        the same signature as the Polynomial constructor; this base
        implementation uses a naked Polynomial regardless of the class doing the
        instantiating.  (This happens to be what all presently extant derived
        classes in this study package want.)  A derived class with a suitable
        way to instantiate itself from Polynomial's constructor arguments can
        over-ride this method have operations on it and its instances produce
        instances of it, instead of raw Polynomials.\n"""
        # Avoid duplicating in the instance's dict what it inherits from class:
        if variate is Polynomial.variablename: variate = None
        return Polynomial(coeffs, denom, variate)

    @classmethod
    def power(cls, n, scale=1, denom=None): return cls.fromMap({n: scale}, denom)

    @staticmethod
    def __solve(data, tool=[]):
        # Package lazy access to System().obtain(), for interpolate().
        try: System = tool[0]
        except IndexError: # first time:
            from study.maths.reduce import System
            tool.append(System)

        n = len(data)
        return System(n, [[k**i for i in range(n)] for k in data.keys()]
                      ).obtain(data.values())

    @classmethod
    def interpolate(cls, data):
        """Construct a Polynomial through desired points.

        Single argument, data, is a mapping from inputs to the polynomial to
        their desired outputs.  Current implementation only supports integer
        (including long) keys; I should ultimately make it more liberal, at
        least as to outputs.  Returned polynomial's order is equal to the number
        of entries in the supplied dictionary.

        We're solving for a list c of coefficients given:
            value = sum(: c[i] * power(i, key) &larr;i :)
        for each key, value in our dictionary.  This is a simple matrix
        problem :-)\n"""

        ouch = [x for x in data.keys() if not cls.__iswhole(x)]
        if ouch:
            raise NotImplementedError(
                'Only integral arguments supported for now - sorry', ouch)

        coeffs, n = self.__solve(data), len(data)
        if len(coeffs) > n:
            assert len(coeffs) == n + 1
            coeffs, scale = coeffs[:n], coeffs[n]
            if scale != 1:
                return cls.fromSeq(coeffs, scale)

        return cls.fromSeq(coeffs)

    @classmethod
    def Chose(cls, gap):
        """Returns lambda x: x!/(x-gap)!/gap!

        The return is a polynomial, using gap! as denominator so as to keep all
        coefficients whole, so that evaluation on any natural x shall return a
        natural, as it should - Chose is formally the transpose of Pascal.chose
        (q.v.) in the sense Pascal.chose(n, g) = Chose(g)(n), so Chose(gap) maps
        every natural to a natural, for any natural gap.

        Single argument, gap, should be a natural number, although other numeric
        values are handled gracefully: if negative, the constant polynomial
        lambda x: 1 is returned, as for Chose(0); otherwise, the resulting
        polynomial has gap.(gap-1)... as factors of its denominator, ending with
        the fractional part of gap (i.e. gap-floor(gap)) and, as numerator
        factors, x+1 minus each of these.

        For the closely-related lambda x: (x+gap)!/x!/gap!, you can use
        Polynomial.Chose(gap)(Polynomial.fromSeq((gap, 1))

        Note that, when gap is natural, sum(Chose(gap)(i) for i in range(n)) ==
        Chose(1+gap)(n); see http://www.chaos.org.uk/~eddy/math/sumplex.html -
        this is exploited by the implementation of PowerSum (q.v.).\n"""

        num, den, x = cls.power(0), 1, cls.power(1)
        while gap > 0:
            den *= gap
            gap -= 1
            num *= x - gap

        assert num.__coefs[num.rank] == 1
        return num.copy(den)

    @classmethod
    def PowerSum(cls, k):
        """That p for which, for natural n, p(n) = sum(i**k for i in range(n)).

        Single input, k, must be a natural number.  Returns a Polynomial p; for
        every natural i, p(i) shall be a natural; i.e. p stores its coefficients
        as whole numbers, along with a suitable denominator.\n"""

        cache = cls.__power_sum
        if __debug__: check = lambda n, j, z=cls.power(1): n(1+z) -n(z) -z**j
        while len(cache) <= k:
            if cache:
                # Compute cache[len(cache)];
                i = len(cache)
                j = 1 + i
                term = cls.Chose(j)
                assert term.__coefs[j] == 1
                num, den = term.copy(j), term.__denom
                term = cls.Chose(i)
                tum, ten = term.__coefs, term.__denom
                assert i == max(tum.keys()) and tum[i] == 1
                # the lambda h: h**len(cache) we want to sum is the leading term
                # in term(h), while num(n) is sum(term(i) for i in range(n)) since:
                assert den == ten * j
                # Now, from num, subtract contributions from term's non-leading powers:
                while i > 0:
                    i -= 1
                    try: c = tum[i]
                    except KeyError: pass # implicit zero
                    else:
                        assert c, 'Zero entry in .__coefs'
                        p, d = cache[i]
                        num -= c * p / d

                assert check(num, len(cache)).rank == -1 and num(0) == 0, (len(cache), num)
                # i.e. num(1+n)-num(n) == n**j for all n
                cache.append((num.copy(1), num.__denom))

            else: cache.append((cls.power(1), 1))

        return cache[k][0].copy(cache[k][1])
    __power_sum = []

    @classmethod
    def fromSeq(cls, seq, denom=None, variate=None):
        """Pseudo-constructor using enumerate(seq)."""
        return cls._polynomial_(enumerate(seq), denom, variate)
    @classmethod
    def fromMap(bok, denom=None, variate=None):
        """Pseudo-constructor using bok.iteritems()."""
        return cls._polynomial_(bok.iteritems(), denom, variate)

del Lazy
