"""Polynomials.  Coefficients are assumed numeric.  Only natural powers are considered.

$Id: polynomial.py,v 1.39 2010-06-27 12:52:07 eddy Exp $
"""
import types
from study.snake.lazy import Lazy

class invalidCoefficient (TypeError): "Invalid coefficient for polynomial"
class unNaturalPower (TypeError): "Power of variable in polynomial is not a natural number"

class Polynomial (Lazy):
    """Model of the mathematical ring of polynomials (in one free variable).

    Supports arithmetic (+, -, *, /, %, divmod, power, negation), comparison
    (but cmp() will yield zero in some cases where == will say no),
    representation (as a python lambda expression), evaluation (i.e. calling as
    a function), repeated integration (as <<) and differentiation (as >>), use
    as boolean (only the zero polynomial is false) and (lazy) hashing.

    Lazy attributes:
    ===============

      rank -- highest power of the free variable with non-zero coefficient
      normalised -- same polynomial scaled so rank's coefficient is 1
      derivative -- result of differentiating the polynomial
      assquares -- decompose as sum of scaled squares and a remainder
      sign -- like cmp(self, 0) but None when ill-defined or variable
      isreal -- are all coefficients real ?
      factors -- tuple of normalised irreducible factors and an optional scalar
      roots -- inputs at which the polynomial is zero (or very nearly so)

    The value of p.assquares is of form (bok, rem) with rem either zero or of
    odd degree and p-rem equal to reduce(lambda y, (x, v): y + x*x*v,
    bok.items(), 0).

    The value of sign may presently sometimes be None when it needn't be; I've
    yet to find an example, but the computation in use only deals with `easy
    enough' cases.  Note that cmp() yields zero when the difference's sign is
    None or zero; but == is true only when sign is zero (actual equality).

    Methods:
    ========

      coefficient(n) -- coefficient of (: x**n &larr;x :) in polynomial
      hcf([poly, ...]) -- yields highest common factor of arbitrarily many
      integral([start=0, base=0]) -- integration
      unafter(poly) -- input to poly yielding self as output
      Weierstrass([tol=1e-6]) -- finds roots
      resultant(self, other) -- 
      Sylvester(other) -- Sylvester matrix of self and another polynomial
      Bezout(other) -- B&eacute;zout matrix of self and another polynomial

    An alternate constructor is also provided via the static method Chose(gap),
    which returns the polynomial lambda x: x!/(x-gap)!/gap!, which has some
    convenient properties under summation.

    See individual methods' docs for details.\n"""

    def __init__(self, coeffs, denominator=None, variate=None):
        """Constructor.

	Required argument, coeffs, specifies the coefficient of each power.  A
	mapping {n: c, ...} specifies a term lambda x: c * x**n in the
	polynomial, for each key, value pair n, c; this is particularly
	well-suited to sparse polynomials, with rank much greater than number of
	non-zero coefficients.  A sequence is construed as a mapping with its
	indices as keys and entries as values, so (a, b, c) is construed as {0:
	a, 1: b, 2: c}, so specifies a quadratic lambda x: a +b*x +c*x**2; the
	sequence form is particularly apt for low rank.  Any other value for
	coeffs shall provoke a TypeError.

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

	If variate is specified, and has a true value, it is used as the name of
	the variable in the polynomial's representations as a string: is should
	be a non-empty string and is used as the new instance's .variablename;
	the same effect can be achieved by setting .variablename after
	instantiation.  By default, str() and repr() use 'z' as the variable,
	but specifying an alternate name, e.g. 'x', by either of these means
	shall substitute that name for the default.

        Note that setting z = Polynomial((0, 1)) provides a `free variable' that
        can then be used to generate polynomials the way many folk prefer; with
        this, Polynomial((1,2,3)) can simply be written 3*z*z +2*z +1.

        See also the pseudo-constructors .Chose(n) and .PowerSum(n).\n"""

	if denominator is not None:
	    1. / denominator # raise suitable error if unsuitable as denominator !
	self.__denom = denominator

        self.__coefs = {}       # dictionary with natural keys
        try:
            coeffs[:] # is it a sequence ?
            self.__fromseq(coeffs)
        except (AttributeError, TypeError, KeyError): # non-sequence arg
            try: coeffs.items, coeffs.get(0, None) # is it a mapping ?
            except AttributeError:
		raise TypeError('First argument must be mapping or sequence', coeffs)
            else: self.__frombok(coeffs) # mapping

        # self.__coefs and self.__denom should now be construed as immutable.

	if variate: self.variablename = variate

    # Coefficients only require the ability to add, multiply and divmod.
    def _get_coeff(val, oktypes=(types.IntType, types.LongType,
                                 types.ComplexType, types.FloatType)):
        if type(val) in oktypes:
            try:
                if val.imag: return val
                val = val.real
            except AttributeError: pass # not complex:
            if val == int(val): return int(val)
            return val
        elif hasattr(val, '__add__') and hasattr(val, '__mul__'):
            # This means we can use polynomials as coefficients ...
            # likewise, linear maps, &c.
            return val
        else: raise invalidCoefficient

    from natural import gcd
    def __descale(self, cs, hcf=gcd, get=_get_coeff):
        """Eliminate common factor from denominator and coefficients.

        Single argument, cs, is a sequence of all the coefficients to be used
        for this polynomial; should be called after self.__denom is set but
        before these coefficients are __store()d; each non-zero coefficient,
        before it is passed to __store(), should be divided by the return from
        this function, which is 1 if self.__denom is None (or 1).

        When self.__denom is not None, this method considers it in combination
        with each whole number that appear as a real coefficient or as the real
        or imaginary part of a complex ones.  If none of these are whole,
        tracking a denominator shall do us no good, so self.__denom is set to
        None and its prior value is returned.  Otherwise, we compute the highest
        common factor, n, of self.__denom and of the whole values found;
        self.__denom is scaled down by n and n is returned; this should not lead
        to any rounding errors.

        Note that no attempt is made to detect coefficients which might be
        rationals being poorly represented by reals; doing so would cause bugs
        when we come to look at polynomials which deliberately have coefficients
        slightly different from actual rationals.  If reals are to be replaced
        by rationals, that's a job for the caller of the constructor.\n"""

        d, w = self.__denom, False
        if d is None: return 1
        for c in cs:
            c = get(c) # ensure sane value
            try: im, re = c.imag, c.real
            except AttributeError:
                if c == int(c): d, w = hcf(c, d), True
            else:
                if im == int(im): d, w = hcf(im, d), True
                if re == int(re): d, w = hcf(re, d), True
            if w and d in (1, -1): break # pointless to continue
        if w:
            if d * self.__denom < 0: d = -d
            if d != 1: self.__denom /= d
        else: # nothing is whole
            assert d is self.__denom
            self.__denom = None

        return d

    def __store(self, power, coeff, g=_get_coeff):
        if coeff: self.__coefs[power] = g(coeff)
        else: self._zero = g(coeff)
    del _get_coeff

    def __fromseq(self, seq):
        d = self.__descale(seq)
        i = 0
        for v in seq:
            self.__store(i, v / d)
            i = 1 + i

    def __frombok(self, bok, ok=lambda k, i=(types.IntType, types.LongType): type(k) in i):
        d = self.__descale(bok.values())
        for key, val in bok.items():
            if not ok(key) or key < 0: raise unNaturalPower
            else: self.__store(key, val / d)

    def _lazy_get_rank_(self, ignored):
        for k, v in self.__coefs.items():
            if not v:
                assert False, 'How did that get past __store ?'
                del self.__coefs[k]
        try: return max(self.__coefs.keys())
        except ValueError: return -1

    def coefficient(self, key):
        val, om = self.__coefs.get(key, self._zero), self.__denom
        if om is not None: val *= 1. / om
        try:
            i = int(val)
            if val == i: return i
        except (TypeError, AttributeError): pass
        return val

    def __numerator(self, key):
        # as for coefficient, but ignoring .__denom
        val = self.__coefs.get(key, self._zero)
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
        if self.rank < 0: raise ValueError, "Can't normalise zero"
        scale, om = self.__coefs[self.rank], self.__denom
        if om is None: om = 1
        if scale == om: return self
        return om * self / scale

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

    def format(num, names, depth):
        try:
            if num.imag == 0: num = num.real
        except AttributeError: pass

        try: return num.__represent(names, 1+depth)
        except AttributeError: return str(num)

    def __represent(self, names, depth, fmt=format):
        if depth > 52: raise ValueError, "We're going to run out of names !"
        while depth >= len(names):
            if names[-1][0] == 'a': names.append('Z')
            elif names[-1][0] == 'A': names.append('z')
            else: names.append(chr(ord(names[-1][0]) - 1))

        result, name = '', names[depth]
        for key in self._powers:
            val = self.__numerator(key)
            if key:
                if val == 1: frag = ''
                elif val == -1: frag = '-'
                else:
                    frag = fmt(val, names, depth)
                    if ' ' in frag or '+' in frag[1:] or '-' in frag[1:]:
                        frag = '(' + frag + ')*'
                    else: frag += '*'

                if key == 1: frag += name
                else: frag += '%s**%d' % (name, key)
            else: frag = fmt(val, names, depth)

            if not result: result = frag
            elif frag[:1] == '-': result += ' ' + frag
            else: result += ' +' + frag
        assert result[:2] != ' +' and result[:1] != ' '

        om = self.__denom
        if om is not None and om != 1:
            if ' ' in result: result = '(' + result + ')/'
            else: result += '/'
            try:
                if om.imag == 0: om = om.real
            except AttributeError: pass
            frag = str(om)
            if ' ' in frag or '+' in frag[1:] or '-' in frag[1:]:
                result += '(' + frag + ')'
            else: result += frag

        return result

    del format

    def __eachattr(self, each):
        bok = {}
        for k, v in self.__coefs.items(): bok[k] = each(v)
        return Polynomial(bok, self.__denom)

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
        try: sum, den = other.__coefs.copy(), other.__denom
        except AttributeError: sum, den = {0: other}, None
        om = self.__denom
        if om is None: denom = den
        else:
            if den is None: denom = om
            else: denom = den * om
            for k in sum.keys(): sum[k] *= om
        if den is None: den = 1

        zero = self._zero
        for k, v in self.__coefs.items():
            sum[k] = sum.get(k, zero) + v * den

        return Polynomial(sum, denom)

    __radd__ = __add__
    def __pos__(self): return self
    def __neg__(self, neg=lambda v: -v): return self.__eachattr(neg)
    def __sub__(self, other): return self + (- other)
    def __rsub__(self, other): return other + (- self)

    def __mul__(self, other):
        term = {}
        try: bok, den = other.__coefs, other.__denom
        except AttributeError:
            denom = self.__denom
            for key, val in self.__coefs.items():
                term[key] = val * other
        else:
            zero = self._zero * other._zero
            for key, val in self.__coefs.items():
                for cle, lue in bok.items():
                    sum = key + cle
                    term[sum] = term.get(sum, zero) + val * lue

            om = self.__denom
            if den is None: denom = om
            elif om is None: denom = den
            else: denom = om * den

        return Polynomial(term, denom)

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
	    if other: other, top, den = Polynomial({0: other}), 0, 1
	    else: raise ZeroDivisionError, other
        else:
            if top < 0: raise ZeroDivisionError, other
            if den is None: den = 1

        o = other.__numerator(top)
        if top == 0:
	    bok = {}
            for k, v in self.__coefs.items(): bok[k] = den * v
            return Polynomial(bok, o * (self.__denom or 1)), Polynomial((0,))

        q, r = Polynomial((self._zero,)), self
        got = self.rank

        # We now reduce the rank of r (by at least 1) at each iteration, by
        # shifting other*x**(got-top) times a scalar from r to q*other; thus, as
        # rank r is finite, it must eventually descend to 0.
        while got >= top:
            m, om = r.__numerator(got), r.__denom
            while m: # take several slices off, in case of arithmetic error ...
                scale = Polynomial({ got - top: m * den }, o * (om or 1))
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

        # assert r == self - q * other # tends to fail on small errors
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
            return cub(*map(self.__numerator, (3, 2, 1, 0)))
        elif self.rank == 3 and self.__pure_real():
            # and, for cubics, pretty accurate:
            rough, tol = cub(*map(self.__numerator, (3, 2, 1, 0))), 1e-9
        else:
            rough, tol = self.Weierstrass(1e-9), 1e-7

        # Not try to refine our rough calculation; if any of them is almost an
        # exact rational, check to see if that rational yields a factor; if so,
        # record the rational and divide out the factor.
        ans = []
        # First, deal with any obvious approximations to rationals:
        for v in rough:
            # Try to approximate v as n / d, with n and d whole:
	    try: n, d = rat(v)
	    except ValueError: continue # give up on this one
            q, r = divmod(self, Polynomial((-n, d)))
            if r.rank == -1:
                if d == 1: ans.append(n)
                else: ans.append(n * 1. / d)
                self = q

	# TODO: see if we have any conjugate pairs we can resolve ?

        # If we refined any rough root, we also refined self:
        if ans:
	    # sort - but complex has no order, so order by real part; but, to be
	    # able to select .real, we have to treat all entries as complex:
	    ans.sort(lambda x, y: cmp((x + 0j).real, (y + 0j).real))
            return tuple(ans) + self.roots # recurse
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
        """Solves self = ans ** num for a num-th root of self."""
        # TODO: support equality modulo mod
        assert num > 0 == self.rank % num

        top, res = self.rank / num, root(self.coefficient(self.rank), 1./num, num%2)
        try:
	    n, d = rat(res)
            if n**num * (self.__denom or 1) != d**num * self.__numerator(self.rank):
		raise ValueError # heigh ho, not good enough, fall back on real:
        except ValueError: ans = Polynomial({ top: res })
        else: ans = Polynomial({ top: n }, d)

        res = self - ans ** num
        while top > 0 and res:
            next = res.rank - (num-1)*ans.rank
            if next < top: top = next
            else: raise ValueError
            ans = ans + Polynomial({ top: res.coefficient(res.rank)},
                                   num * ans.coefficient(ans.rank)**(num-1))
            res = self - ans ** num

        if res: raise ValueError
        return ans
    del scalarroot, ratcom

    def __pow__(self, other, mod=None,
                ok=lambda i, t=(types.IntType, types.LongType): type(i) in t,
                rat=rationalize, hcf=gcd):
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

        # Require other to be natural
        try:
            if other.rank < 1: other = other.coefficient(0)
            # For some bizarre reason, x**0 isn't evaluated as x.__pow__(0) !
            # When evaluating x ** 0 I find other == Polynomial((0,)) instead.
        except AttributeError: pass
        try:
            if other < 0:
                if wer.rank != 0: raise TypeError
                return Polynomial((power(wer.__denom, other),),
                                  power(wer.__numerator(0), other))
            if wer.rank < 0: return self # zero**+ve is zero
            if not ok(other):
                # We *may* be able to cope with a fractional power, if rational:
                n, d = rat(other * self.rank) # ValueError if not possible
                # The factor of self.rank is there to favour factors of it in
                # the eventual denominator, as these are most readilly endured;
                # now take this factor back out:
                i = hcf(n, self.rank)
                if d * i < 0: i = -i
                d *= self.rank / i
                n /= i
                # so other == n / d, give or take rounding.
                if d > 1: wer = wer.__root(d, mod) # ValueError if not possible
                other = n

        except (AttributeError, TypeError, ValueError):
            raise unNaturalPower, other

        while other >= 1:
            other, b = divmod(other, 2)
            wer, result = step(b, wer, result)

        return result
    del rationalize, gcd

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
        bok = {}
        for k, v in self.__coefs.items():
            if k: bok[k-1] = k * v
        return Polynomial(bok, self.__denom)

    def integral(self, start=0, base=0):
        """Integrate a polynomial.

        Optional arguments, start and base, specify the constant of integration;
        the integral's value at start (whose default is 0) will be base (whose
        default is also zero).  Note that self.integral(base=h) is equivalent to
        self.integral()+h; and self.integral(start, 0)(end) is the integral of
        self from start to end."""

        bok = {}
        for k, v in self.__coefs.items():
            bok[k+1] = v / (1.+k)
        ans = Polynomial(bok, self.__denom)
        # assert: ans(0) == 0
        if start: ans = ans - ans(start)
        if base: ans = ans + base
        return ans

    # assert: self.integral().derivative == self
    # assert: self.derivative.integral(x, self(x)) == self, for any x

    def sum(self, start=0, base=0):
	"""Returns lambda n: sum(map(self, range(n)))

	This is the discrete equivalent of integral.  Also accepts optional
	arguments, start and base, as for integral(): each defaults to zero and,
	if f is the result without them, when they're specified the result is f
	- f(start) + base.  See delta for the inverse operation.\n"""

	ans, om = Polynomial((self._zero,)), self.__denom
	for k, v in self.__coefs.items():
	    ans += v * Polynomial.PowerSum(k)
	if om is not None: ans /= om
	if start: ans -= ans(start)
	if base: ans += base
	return ans

    def _lazy_get_delta_(self, ignored):
	"""Returns lambda n: self(1+n) - self(n)

	This is the discrete equivalent of differentiation, the taking of
	'finite differences'.  Note that, for any s and b:
	self.sum(s, b).delta == self == self.delta.sum(0, self(0)).\n"""
	n = Polynomial((0, 1))
	return self(1+n) - self(n)

    # non-zero: safe and unambiguous
    def __nonzero__(self): return self.rank >= 0
    # Comparison: of debatable value
    def __cmp__(self, other): return (self - other).sign or 0
    # *Stronger* test for equality ...
    def __eq__(self, other): return (self - other).rank < 0

    def __coerce__(self, other, boktyp=types.DictionaryType):
        try:
            if type(other.__coefs) == boktyp: return self, other
        except AttributeError: pass

        try:
	    try: return self, Polynomial(other)
	    except ValueError: return self, Polynomial((other,))
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

        keys = self._powers
        try: top, keys = keys[0], keys[1:]
        except IndexError: return self._zero
        result, om = self.__numerator(top), self.__denom

        for key in keys:        # highest first
            while top > key:
                result, top = result * arg, top - 1

            result = result + self.__numerator(key)

        while top > 0:
            result, top = result * arg, top - 1

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
        # invariant: d**2 = m * n and i**2 < m
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
            z = Polynomial({i: 1}, d)

            while i > 0:
                i = i - 1
                q, ign = divmod(rem - k*z*z, 2*z*k)
                assert q.rank <= i
                z = z + Polynomial({i: q.__numerator(i)}, q.__denom)

            bok[z], rem = k, rem - k * z * z
            i = rem.rank

        assert self == rem + reduce(lambda y, (x, k): y + x*x*k, bok.items(), 0)
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
        k = (2j -.1) ** (1./self.rank)
        r = map(lambda i, k=k**2: k**(2 * i + 1), range(self.rank))
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
        if self.__denom is None: scale = 1
        else: scale = 1. / abs(self.__denom)
        return max(map(abs, self.__coefs.values())) * scale

    def __istiny(self, scale=1, maxrank=0):
        if self.rank > maxrank: return None
        if self.rank < 0 or self._bigcoef < scale * 1e-6: return 1
        return None

    def _lazy_get_factors_(self, ignored):
        ans = map(lambda r: Polynomial((-r, 1)), self.roots)
        lead = self.__coefs[self.rank]
        if self.__denom is not None: lead *= 1. / self.__denom
        if lead != 1: ans.append(lead)
        return ans

    def resultant(self, other):
        """See .Sylvester(other); this is its determinant.\n"""
        return reduce(lambda x, y: x * y, map(other, self.roots),
                      self.coefficient(self.rank) ** other.rank)

    def __Sylvester(self, i):
        ans = []
        while i > 0:
            i -= 1
            if ans: row = row[:-1] + row[-1:]
            else: row = map(self.coefficient, range(self.rank, -1, -1)) + [ 0 ] * i
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
    def Chose(gap):
        """Returns lambda x: x!/(x-gap)!/gap!

        The return is a polynomial, using gap! as denominator so as to keep all
	coefficients whole, so that evaluation on any natural x shall return a
	natural, as it should - Chose is formally the transpose of Pascal.chose
	(q.v.) in the sense Pascal.chose(n, g) = Chose(g)(n), so Chose(gap) maps
	every natural to a natural.

        Single argument, gap, should be a natural number, although other numeric
        values are handled gracefully: if negative, the constant polynomial
        lambda x: 1 is returned, as for Chose(0); otherwise, the resulting
        polynomial has gap.(gap-1)... as factors of its denominator, ending with
        the fractional part of gap (i.e. gap-floor(gap)) and, as numerator
        factors, x+1 minus each of these.

	For the closely-related lambda x: (x+gap)!/x!/gap!, you can use
	Polynomial.Chose(gap)(Polynomial((gap, 1)))

        Note that, when gap is natural, sum(map(Chose(gap), range(n))) ==
        Chose(1+gap)(n); see http://www.chaos.org.uk/~eddy/math/sumplex.html -
        this is exploited by the implementation of PowerSum (q.v.).\n"""

        num, den, x = Polynomial((1,)), 1, Polynomial((0, 1))
        while gap > 0:
            den *= gap
            gap -= 1
            num *= x - gap

        assert num.__coefs[num.rank] == 1
        return Polynomial(num.__coefs, den)

    @staticmethod
    def PowerSum(k):
        """That p for which, for natural n, p(n) = sum(map(lambda i: i**k, range(n))).

        Single input, k, must be a natural number.  Returns a Polynomial p; for
        every natural i, num(i) shall be a natural; i.e. p stores its
        coefficients as whole numbers, along with a suitable denominator.\n"""

        cache = Polynomial.__power_sum
	if __debug__: check = lambda n, j, z=Polynomial((0, 1)): n(1+z) -n(z) -z**j
        while len(cache) <= k:
            if cache:
                # Compute cache[len(cache)];
		i = len(cache)
		j = 1 + i
                term = Polynomial.Chose(j)
		assert term.__coefs[j] == 1
                num, den = Polynomial(term.__coefs, j), term.__denom
                term = Polynomial.Chose(i)
                tum, ten = term.__coefs, term.__denom
		assert i == max(tum.keys()) and tum[i] == 1
                # the lambda h: h**len(cache) we want to sum is the leading term
                # in term(h), while num(n) is sum(map(term, range(n))) since:
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
                cache.append((Polynomial(num.__coefs), num.__denom))

            else: cache.append((Polynomial((0, 1)), 1))

        return Polynomial(cache[k][0].__coefs, cache[k][1])
    __power_sum = []

del types, Lazy
