"""Bezier splines.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop
from study.snake.sequence import Tuple

def Bezier(t, first, *rest):
    """The general Bezier interpolator.

    First parameter, t, is the scalar (presumed to behave like a real
    value in the range from 0 to 1); it suffices that it can be
    subtracted from 1 (without raising an exception) and that t, 1 -t
    and any product of powers of these can be multiplied together to
    give scalars (see below).

    All other parameters, of which there must be at least one, are
    control points, defining the Bezier hull of this curve.  All must
    be vectors in the same space.  It suffices that each of them can
    be multiplied by scalars (t, 1 -t and products of their powers) to
    give vectors that can be summed.  In particular, the vectors used
    may perfectly well be Curve objects, to represent
    multi-dimensional Bezier surfaces.  If only one control-point is
    passed, the result is degenerate, t is ignored and the result is
    the single control point.

    The return value shall be a weighted average of the control
    points.  Specifically, if ps = (first,) + rest = (p[0], ...,
    p[n]), the return is

	sum(chose(n, i) * t**i * (1 -t)**(n -i) * p
            for i, p in enumerate(ps))

    However, it isn't computed that way.  Instead a recursive descent
    (to O(log(len(rest) +1)) depth) employs simple linear
    interplations at its leaves.
    """
    if t == 0 or not rest:
        return first
    if t == 1:
        return rest[-1]
    if len(rest) == 1:
        return first * (1 - t) + rest[0] * t
    ps = (first,) + rest
    m = len(ps) // 2 + 1
    assert m < len(ps) # i.e. len(ps) > 2
    return Bezier(t, *(Bezier(t, *ps[n:][:m]) for n in range(len(ps) + 1 -m)))

class Curve (object):
    """A Bezier curve.

    This is a callable, representing a function of one parameter, that
    returns a weighted average of the control points passed to the
    constructor.  It maps 0 to its first control-point and 1 to its
    last.  Any in between control the shape of the path from one to
    the other.

    Supports arithmetic, shifting and representation.  The
    representation is as code to reproduce the object.  Left and right
    shifting - as for study.maths.polynomial.Polynomial - are
    integration (with zero constant) and differentiation,
    respectively.

    Arithmetic includes addition and subtraction - either with another
    Curve or with a displacement to control points - negation, scaling
    (i.e. mutiplication by scalars; for division, multiply by an
    inverse)

    Properties:
      order -- rank of the equivalent polynomial
      reverse -- same but with control-points reversed
      derivative -- rate of varriation of output with parameter
      asPolynomial -- the equivalent polynomial

    Methods:
      integral([base]) -- reverses derivative, starts at base (or zero)

    Pseudo-constructors:
      fromPolynomial(poly) -- reverse of asPolynomial
    """
    def __init__(self, first, *rest):
        """Constructs a Bezier Curve.

        Parameters are control points: there must be at least one.
        All must support scaling and the results of such scaling must
        support being added together.

        Note that, when their control points are arithmetically
        compatible, Curves are arithmetically compatible, hence can be
        used as control points to higher-level Curves that implement
        surfaces.  A Curve whose control points are vectors in some
        space represents a line, with one-parameter, while a Curve
        whose control points are of that form represents a surface,
        with two parameters; using these, in turn, as control points
        gives a parameterised three-dimensional surface (or a
        parameterisation of a three-dimensional space).  See
        __call__() for how you can supply the several parameters to
        such a higher-dimensional surface.
        """
        self.__ps = Tuple((first,) + rest)

    def __call__(self, t, *rest):
        """Auto-curried evaluation.

        If only one parameter is passed, it is used to compute a
        weighted average of self's control points.  If more parameters
        are passed, they (excluding the first) are forwarded to each
        control point and the weighted average of the results is
        returned instead.
        """
        if rest:
            return Bezier(t, *(p(*rest) for p in self.__ps))
        return Bezier(t, *self.__ps)

    @classmethod
    def _curve_(cls, *ps):
        """Contstructor-wrapper for derived classes to override.

        When methods of Curve create instances they do so via this
        wrapper.  If a derived class's constructor's signature differs
        from that of Curve, it needs to override this to accept the
        same arguments as Curve's constructor.  Note that it is called
        from class methods, so must be callable on the class itself.
        """
        return cls(*ps)

    @property
    def order(self):
        """The order of the Bezier curve.

        This is the rank of the equivalent polynomial, one less than
        the number of control points passed to the constructor.  The
        degenerate case, with only one control point, is constant and
        (even if its constant value is zero) has order zero.  Every
        term in the Bezier representation of the polynomial has the
        Curve's order as the sum of the powers to which its factors of
        t and 1 -t are raised.  This is also the len() of self.

        Contrast with the polynomial representation of self, which
        only counts powers with non-zero coefficient towards its len()
        and has rank -1 if it is constant with zero value.
        """
        return len(self.__ps) - 1

    @property
    def reverse(self):
        """The reverse of this Bezier curve.

        The result, called at t, gives the result self gives at 1 -t.
        """
        return self._curve_(*reversed(self.__ps))

    @lazyprop
    def derivative(self):
        """Derivative of a Bezier curve, as a Bezier curve.

        If self has order n, its derivative naturally has order n-1.
        A single t**i * (1 -t)**(n -i) term's derivative with respect
        to t is

            i*t**(i-1)*(1-t)**(n-i) -(n-i)*t**i*(1-t)**(n-i-1)

        The first term in that is 0 for i = 0, the second for i = n;
        otherwise, scaling them by the constant chose(n, i) * p[i]
        gives contributions n*p[i] to the derivative's [i-1] control
        point (with its chose(n-1, i-1) scaling) and -n*p[i] to the
        derivative's [i] control point (with its chose(n-1, i)
        scaling), skipping the former for i = 0 and latter for i = n.
        The result's control points are thus just n times the
        differences between self's.
        """
        ps = self.__ps
        n = len(ps) - 1
        if n == 0: # Constant.
            return self._curve_(self.__zero)
        return self._curve_(*ps.pairs(lambda u, v: (v - u) * n))

    def integral(self, base=None):
        """Integral of a Bezier curve, as a Bezier curve.

        This is the reverse of derivative, with the result's value at
        t = 0 taken to be base, or the zero of self's control points'
        space if base is omitted (or None, its default).
        """
        if base is None:
            base = self.__zero
        return self._curve_(*self.__integral(base, self.__ps))

    @property
    def asPolynomial(self):
        """Convert self to a Polynomial.

        Only viable if multiplication is supported betwen Polynomial
        (as left operand) and the type of vector passed as control
        points to self's constructor (as right operand).  The result's
        coefficients shall be vectors of the same kind as these
        control points.  Calling the result may be somewhat less
        efficient than calling self with the same parameter, but it
        may be amenable to some operations that self may not support.

        The result's .rank shall normally be equal to self.order, but
        may be -1 when self.order is 0, i.e. self is degenerate, if
        its constant value is zero.

        Calling fromPolynomial() on the result should get back a Curve
        that differs neglibly from self.
        """
        t, s = self.__ts
        ps = self.__ps
        n = len(ps)
        if n == 1: # Degenerate case.
            # Need to make it a Polynomial, so scale by s +t = 1.
            return (s +t) * ps[0]
        while n > 1:
            ps = ps.pairs(lambda u, v: s * u + t * v)
            n -= 1 # n = len(tuple(ps))
        return ps.next()

    @staticmethod
    def __fromPoly(poly, t, s): # s = 1 -t
        """Convert a polynomial to a Bezier.

        We're given a polynomial, poly, plus the polynomials t and s =
        1 -t (both functions of t) and we want to rewrite poly in the
        form

            poly(t) = sum(t**i * s**(n-i) * p / i! / (n -i)!
                          for i, p in enumerate(ps)) * n!

        with n = len(ps) -1.  This function yields ps[0] through ps[n]
        for which that is the case (to within the usual tolerances).

        We can get ps[0] easily, as poly(0); if we then subtract ps[0]
        * s**n, we're left with the remaining terms in the sum, all of
        which have at least one factor of t.  Dividing that out
        reduces us to a polynomial that we want to express in similar
        form, but with n * ps[j+1] / (j+1) in place of each ps[j] and
        (after that) n reduced by 1.  Evaluating this at 0 thus gets n
        * ps[1], which we need to divide by n.  We can thus iterate
        this to reduce the polynomial's rank, getting chose(n, i) *
        ps[i] at each step.

        For the special case where poly is constant, the degenerate
        Bezier 'curve' with just one point, that ignores its t
        parameter when called, we simply want poly's constant value
        (which is poly(x) for any x, including 0).  Otherwise, we
        could run the iteration down to rank zero and handle it the
        same, but we can finesse away some work in the last iteration,
        when poly is linear, as follows.

        When poly is linear, we can rewrite poly(t) = a +b*t = a*(1-t)
        +(a+b)*t = poly(0)*s +poly(1)*t to get the (last) two entries
        in ps as poly(0) and poly(1), without actually doing the
        reduction of the linear polynomial to a constant one.

        Aside from that finesse's final poly(1), we only ever need
        poly(0), which is conveniently poly.coefficient(0), saving
        some polynomial evaluation.
        """
        n, j, scale = poly.rank, 0, 1 # n+j is an invariant.
        while n > 1:
            tail = poly.coefficient(0) # == poly(0)
            yield tail * scale
            # poly(t) = ps[j] * s**n +... +ps[j+i] * t**i * s**(n-i) +...
            if tail: # remove the ps[j] * s**n term:
                poly -= tail * s**n
            poly, r = divmod(poly, t)
            assert r.rank < 1 # and the constant is negligible
            assert poly.rank < n
            scale *= 1. / n
            n -= 1
            j += 1
            scale *= j
        assert poly.rank <= n
        # For n in (0, -1) we started with constant poly, didn't go
        # round the loop, still have j = 0 and only need ps[j];
        # otherwise, we exited the loop with poly linear and can
        # finesse away the polynomial arithmetic of the iteration that
        # would otherwise reduce it to a constant.

        yield poly.coefficient(0) * scale # ps[j], in either case.
        if n > 0: # ps[j+1], when original poly wasn't constant.
            assert n == 1 # so j+1 is the original poly's rank.
            yield poly(1) * scale

    @classmethod
    def fromPolynomial(cls, poly):
        """Decompose a polynomial as a Bezier curve.

        Pseudo-contstructor, taking a polynomial whose coefficients
        are vectors; the control points of the result shal be of the
        same vector type.  The result's asPolynomial will equal the
        given polynomial, give or take any rounding errors.
        """
        return cls._curve_(*cls.__fromPoly(poly, *cls.__ts))

    def translate(self, vector):
        """Translates self through the given vector.

        This adds vector to each control-point of self.  While
        addition could be overloaded to do this, it would get
        problematic to do that for Bezier surfaces, described as Curve
        objects with Curve control-points.
        """
        return self._curve_(*(p + other for p in self.__ps))

    # Python's magic methods:
    def __add__(self, other):
        """Addition.

        Curve instances can be added to one another, to produce
        another.  When they have the same order, this just amounts to
        adding the corresponding control-points.  Otherwise, each can
        be expressed as a polynomial and the sum of these can be
        converted to a Curve.

        To add a fixed displacement to all control-points of a Curve,
        see translate().
        """
        if not isinstance(other, Curve):
            raise TypeError("Incompatible addition", other, self)

        if other.order == self.order:
            return self._curve_(*(p + q
                                  for p, q in zip(self.__ps, other.__ps)))
        # Otherwise, the best we can do is addition as polynomials,
        # converting the result back to a Bezier curve:
        return self.fromPolynomial(self.asPolynomial +
                                   other.asPolynomial)

    __radd__ = __add__
    def __sub__(self, other): return self + (-other)
    def __rsub__(self, other): return (-self) + other
    def __neg__(self): return self._curve_(*(-p for p in self.__ps))
    def __eq__(self, other): return self.__ps == other.__ps
    def __hash__(self): return hash(self.__ps) ^ hash(type(self))
    def __len__(self): return self.order

    def __repr__(self):
        return "Bezier.Curve(%s)" % ', '.join(repr(p) for p in self.__ps)
    def __mul__(self, other):
        return self._curve_(*(p * other for p in self.__ps))
    def __rmul__(self, other):
        return self._curve_(*(other * p for p in self.__ps))

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

    # Internal tools:
    @staticmethod
    def __integral(base, ps):
        scale = 1. / len(ps)
        yield base
        for p in ps:
            base += p * scale
            yield base

    @lazyprop
    def __zero(self):
        val = self.__ps[0]
        return (val - val) * 0

    # Internal tools depending on imports.
    from study.maths.polynomial import Polynomial as poly
    __ts = poly((0, 1)), poly((1, -1)) # t, s = 1 -t
    del poly
