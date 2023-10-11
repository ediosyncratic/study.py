"""Computing norms to describe distances

Exports:
 * norm(n, *args) - the L_n norm of args
 * hypot(*args) - the usual (L_2 norm) root sum square of args
 * normiter(seq, n, scale, count) - L_n norm of a sequence
 * hypotiter(seq, scale, count) - L_2 norm of a sequence

The L_n norm (for positive real n) of a sequence s of numbers is
defined to be

   sum(abs(x)**n for x in s)**(1./n)

However, the range of values that the python float type can represent
is limited and many of the values it can represent are large enough
that their **n is too big to represent; while similarly many of them
are small enough that their **n gets approximated by zero.  If we deem
the former to be 'huge' values and the latter to be 'tiny', even one
huge value in s will cause the definition's formula to overflow;
while, if all entries in s are tiny, or even almost tiny, the
definition's formula will produce an inaccurate result.

This can be mitigated by computing

    abs(S) * sum(abs(x/S)**n for x in s)**(1./n)

instead, for some value S with an order of magnitude close to that of
the correct result (or for which the code doing this doesn't care
about the difference between zero and the result, when small compared
to S).  The scale parameter to normiter() and hypotiter() is used as
such an S, but any much larger entry in the sequence displaces it.

== Floating-point details ==

The range of positive values representable (at full precision; some
smaller values are represented at reduced precision) by Python's float
type is bounded by sys.float_info's max and min attributes.  Any value
bigger than sys.float_info.max**(1./n) is huge, for the purposes above
(will cause overflow); and any value smaller than
sys.float_info.min**(1./n) is tiny (will lose precision, may lead to a
misleading zero result).

On a system where float is implemented as [0] IEEE 754's binary64
representation of real numbers (the 'double precision floating point'
numbers supported by most modern processors), max is just under
1.8e+308 while min is just over 2.225e-308.  While values down to
almost as small as 4.9e-324 can be represented, values between there
and sys.float_info.min are represented with reduced precision (IEEE
754's denormal form).

* [0] https://en.wikipedia.org/wiki/IEEE_754

=== Handling of NaNs ===

Usually, the presence of a NaN infects all arithmetic involving it, so
one might expect the L_n norm of a vector with a NaN entry to be a
NaN.  When all other components are finite, this is indeed the case.

A NaN (not a number) arises from invalid arithmetic operations such as
multiplying zero by infinity or subtracting infinity from infinity.
When such computations arise, they typically mean any number,
including an infinity of either sign, could be a reasonable candidate
to consider as the result.  When it comes to adding up a sum of
squares, however, if one of the values in the sequence is infinite,
the sum of squares is definitely infinite, even if one of the other
values was NaN; no matter what value we might select to use in place
of the NaN, finite or infinite, the sum of squares remains infinite.

As a result, the implementations in this module (aside from L_0, see
below) all return infinity if any value in the sequence is infinite;
only when there are no infinite values in the sequence will a NaN in
the sequence lead to NaN as the result.  This is consistent with the
behaviour of built-in max() and math.hypot().

== Special cases for n ==

For very large n, the largest entry in the sequence always dominates
the sum, with all smaller entries making negligible contributions, so
that the final **(1./n) gets you back to that largest entry.  (If the
maximal element is repeated, the factor of how many times it is
repeated makes little difference as count**(1./n) tends to 1 for
sufficiently large n and any finite count.)  So the case of infinite
positive n [1] is handled specially, to give max(abs(x) for x in s).

* [1] https://mathworld.wolfram.com/L-Infinity-Norm.html

The special case n = 0 is handled, as well, even though taking
sum(...)**(1./n) makes no sense for it; in this case, the L_0 'norm'
of a sequence of numbers counts how many of those numbers have
non-zero abs().  This may also be called the cardinality of the
support; it is [2] a conventional exception to the general definition;
and is technically not a norm since scaling every entry in the
sequence by some positive real doesn't scale its L_0 'norm' by the
same factor.  Note that NaN and infinite entries in the sequence are
treated just the same as all other non-zero entries.  As a
side-effect, the L_0 norm of (0*x for x in seq) is the number of
infinite and NaN entries in seq.

* [2] https://en.wikipedia.org/wiki/Lp_space#When_p_=_0

See study.LICENSE for copyright and license information.
"""

import cmath

# Both isinf and isnan are true for complex(inf, nan) and complex(nan,
# inf).  These two values both have inf as abs() and abs(complex(nan,
# nan)) is nan, so these functions do just what we want here.
# Warning: 1j * nan is nan +nanj, so inf +1j * nan is (entirely
# logically) complex(nan, nan) not complex(inf, nan) !
from sys import float_info

def _iter_norm(seq, n, scale, count):
    """The arithmetic details of norm computation."""
    invn, vast = 1. / n, None
    if scale and (not count or count < float_info.max):
        # Avoid revising scale unless failure to do so would risk overflow:
        big = float_info.max / max(count or 0, 100)
        if n > 1:
            big, vast = big ** invn, big
    else:
        # Use adaptive scale based on largest entry seen
        big = 1

    seq, total = iter(seq), 0
    # Use first non-zero entry in seq (if any) to ensure scale
    try:
        while not scale:
            scale = abs(seq.next())
            total = 1
    except StopIteration:
        pass
    if cmath.isinf(scale): return scale
    scale *= 1. # Avoid integer-division problems below.

    for arg in seq:
        if cmath.isinf(arg): return abs(arg)
        if cmath.isnan(scale) or not arg: continue
        if cmath.isnan(arg):
            scale = abs(arg)
            continue

        r = abs(arg / scale)
        if r > big:
            scale = abs(arg) * 1.
            # total = total / r**n + 1
            # but we can't safely compute r**n to divide by it, so:
            if vast:
                assert n > 1
                # ... and we were passed scale, so big is indeed big.
                # assert vast == big**n, modulo floating-point imprecision
                while r > big:
                    total /= vast
                    r /= big
                # (Albeit, if we go round that loop more than once, we'll
                # be left with total = 0.)
                # Now r <= big, so we can safely compute r**n.
            total = total / r**n + 1
        else:
            total += r**n

    if cmath.isnan(scale): return scale
    assert not cmath.isnan(total)
    if scale and total != 1: scale *= total ** invn
    return int(scale) if scale.is_integer() else scale

def hypot(*args):
    """Return the Pythagorean norm of the given arguments.

    Receives arbitrarily many numbers, real or complex, and returns the square
    root of their sum of squared abs() values.  As for ECMAScript's hypot(), if
    any value is infinite, you should get +Infinity, otherwise if any is a NaN
    you'll get NaN; you should only get 0 if all inputs are 0; otherwise, the
    result is a positive real.

    Note that python's built-in module cmath has no hypot(), while the one
    provided by the math module can't cope with complex parameters, and can
    only cope with two real parameters.\n"""
    scale = 0.
    if len(args) == 1: scale = abs(args[0]) * 1.
    elif args: return _iter_norm(args, 2, scale, len(args))
    return int(scale) if scale.is_integer() else scale

def hypotiter(seq, scale=0, count=None):
    """Compute the L_2 norm of a sequence of numbers.

    Required first argument, seq, must be an iterable whose entries are
    numeric.  Optional second and third arguments scale and count are as for
    normiter.\n"""
    return _iter_norm(seq, 2, scale, count)

def norm(n, *args):
    """Returns the L_n norm of the given numeric values.

    See normiter() for details.
    """
    return normiter(args, n)

def normiter(seq, n, scale=0, count=None):
    """The L_n norm of a sequence of numeric values.

    First argument, seq, is an iterable whose entries are numeric.
    Second argument, n, is the order of the norm desired, which must
    behave as a non-negative real number for purposes of taking powers
    (a non-negative integer will do).  As an exception, n = +Infinity
    is an allowed value, returning max(abs(x) for x in seq).

    Optional third argument, scale (default 0.), gives a magnitude at
    which the caller cares about imprecision, but is willing to ignore
    relatively tiny values. If you're testing whether a vector is
    within 1e-6 of the origin, passing scale=1e-6 will save the
    algorithm some wasted effort tracking precision of contributions
    from components tiny compared to 1e-6.

    Optional fourth argument, count, defaults to None. Ignored unless
    scale is positive, in which case passing the length of seq (or an
    upper bound estimate on its length) as count can help the
    numerical stability of the algorithm.

    For n = 0, this returns the number of entries in seq for which
    abs() is non-zero.  Otherwise, it returns a value equivalent to

        sum(abs(x)**n for x in seq)**(1./n)

    See module comment for handling of non-finite values, for details
    of why it isn't actually computed naively, for why the n = 0 and
    and n = +Infinity cases are handled specially.\n"""

    if cmath.isnan(n):
        raise ValueError("Order of norm must be a number", n)
    try:
        if n < 0:
            raise ValueError("Order of norm must be >= 0", n)
    except TypeError as oops:
        try:
            if n.imag or n.real < 0: raise AttributeError
            # Treat a complex value with zero imaginary part as its real part:
            n = n.real
        except AttributeError:
            oops.args += ("Order of norm must behave like a non-negative real", n)
            raise oops

    if cmath.isinf(n):
        return max(abs(x) for x in seq)
    if n == 0:
        return sum(1 for x in seq if abs(x))

    return _iter_norm(seq, n, scale, count)
