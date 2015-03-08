"""Fibonacci's sequence.

This sequence grows assymptotically as a power of the golden ratio,
(1+5.**.5)/2; there is even a geometrical justification for this.  The usual
algebraic reasoning takes the recurrence relation f(n+1) = f(n) +f(n-1) and
guesses f(n) = k * x**n for some k and x, yielding x = 1 + 1/x whence x*x = 1 +
x, the quadratic whose roots (satisfying (x -1/2)**2 = x*x -x +1/4 = 5/4) are
the golden ratio and its negated inverse (which is equally the result of
subtracting it from one).  This establishes that any solution to Fibonacci's
relation is indeed of form f(n) = k * x**n +h * (-x)**(-n) for some k and h,
with x the golden ratio.

The geometric argument starts from the golden ratio's geometric definition: it
is the ratio of longer to shorter side in a rectangle which is similar to the
rectangle obtained by cutting off from it an internal square on one of its
shorter sides.  If the long side is b and the short side is a, the trimmed
rectangle has sides b-a and a, which must be in the same ratio as a to b, one
way round or the other.  Since b/a cannot equal (b-a)/a for finite b and
non-zero a, the ratio must actually be the other way round: b/a = a/(b-a), which
can be re-arranged as 1 +a/b = b/a, the same equation we solved above with b/a
in place of x.

If we take an arbitrary rectangle and *add* an exterior square on its longer
side, we certainly obtain a rectangle with what was the longer side as its
shorter side.  If we start with a rectangle whose longer side is x+e times its
shorter side, we obtain one whose longer side is (1+x+e)/(x+e); which differs
from (1+x)/x = x by e times the derivative, at x, of (: (1+y)/y &larr;y :),
i.e. by -e/x/x, which is smaller than e since x &gt; 1.  Thus, at each
iteration, adding an external square brings the ratio of sides closer to the
golden ratio (by a factor of about 2.6).  If we start with unit sides we are
effectively computing the Fibonacci sequence, so we may infer that the ratio
between successive terms of this sequence steadilly approaches the golden ratio.

For related discussion, see http://www.chaos.org.uk/~eddy/craft/Fibonacci.html
See study.LICENSE for copyright and license information.
"""

def fibonacci(what, was=0, result=1):
    """Computes Fibonacci's sequence.

    Required first argument is the index into Fibonacci's sequence: it should be
    an integer and may be negative.  (If it is not an integer, it is effectively
    rounded towards zero.)  Optional second and third arguments are the
    sequence's entries with indices 0 and 1 respectively; their defaults are 0
    and 1, respectively.

    Defined here by: for all integers n, f(n+1) = f(n) + f(n-1).  For the
    default initial entries, this gives f(-n) = f(n) for odd n, f(-n) = -f(n)
    for even n; in particular, both f(-1) and f(1) are 1.  Note that
    conventional definitions tend to index from 1 rather than zero and take the
    first two entries to be 1: this helpfully matches what we get here, since
    f(2) = f(1) + f(0) = 1 + 0 = 1.

    Performs its calculations assuming its data to be integers, so handles
    overflow by coercing values to indefinite-precision integers (Python's long
    integer type).  If actual initial values are floats, this handling of
    overflow is probably wrong; you may be better off using
    study.value.bigfloat.BigFloat()s.\n"""

    if -1 < what < 1: return was
    while what > 1:
        # f(j-1), f(j), j = f(i), f(i) + f(i-1), 1+i
        was, result, what = result, result + was, what - 1

    while what < -1:
        # f(j), f(1+j), j = f(1+i) - f(i), f(i), i-1
        result, was, what = was - result, result, what + 1

    return result

def fibtimes((a,b), (c,d)):
    """A multiplication with which to speed Fibonacci computation.

    See: http://www.inwap.com/pdp10/hbaker/hakmem/hakmem.html

    Define multiplication on ordered pairs

        (A,B) (C,D) = (A C + A D + B C, A C + B D).

    This is just (A X + B) * (C X + D) mod X^2 - X - 1, and so is associative,
    etc. We note (A,B) (1,0) = (A + B, A), which is the Fibonacci
    iteration. Thus, (1,0)^N = (FIB(N), FIB(N-1)), which can be computed in log
    N steps by repeated squaring, for instance.\n"""

    return a * (c + d) + b * c, a * c + b * d

def fibpow(n, c=0, d=1):
    """Returns (c, d) * (1, 0)**n"""
    if n < 0: a, b, n = 1, -1, -n
    else: a, b = 1, 0
    while n > 0:
        n, i = divmod(n, 2)
        if i: c, d = fibtimes((a,b),(c,d))
        a, b = fibtimes((a,b), (a,b))
    return c, d

def fastonacci(n, zero, one):
    """As for fibonacci, but computed in logarithmic time :-)

    This (together with the supporting functions above) is essentially just
    the bare necessities condensed out of Fibpair.fibonacci, below; but
    Fibpair provides greater flexibility, e.g. see its .beyond().\n"""

    return fibpow(n, zero, one)[0]

class Fibpair (tuple):
    """Pairs representing polynomials modulo lambda x: x*x -x -1

    This approach (or fastonacci, which is equivalent) is best when working
    with sparse entries, some at large index, in Fibonacci's sequence.  If
    working with a contiguous block, it is typically better to use a cached
    approach, although the pair whose first entry .fibonacci() returns can be
    used to seed that.  See beyond() for an iterator like this.

    See http://www.chaos.org.uk/~eddy/craft/Fibonacci.html#Poly for details.\n"""
    # automagically @classmethod:
    def __new__(cls, a, b): return tuple.__new__(cls, (a, b))
    # def __init__(self, a, b): pass # over-ride tuple.__init__'s different signature.

    # Support for arithmetic:
    @staticmethod
    def __unpack(other, what):
        try: c, d = other
        except TypeError:
            try: 0 + other
            except:
                raise ValueError(
                    'Fibpair can %s another Fibpair or a simple scalar' % what, other)
            c, d = 0, other
        return c, d

    @classmethod
    def __add(cls, (a, b), (c, d)): return cls(a + c, b + d)
    def __add__(self, other):
        return self.__add(self, self.__unpack(other, "add"))
    def __radd__(self, other):
        return self.__add(self.__unpack(other, "be added to"), self)

    @classmethod
    def __sub(cls, (a, b), (c, d)): return cls(a - c, b - d)
    def __sub__(self, other):
        return self.__sub(self, self.__unpack(other, "subtract"))
    def __rsub__(self, other):
        return self.__sub(self.__unpack(other, "be subtracted from"), self)

    @classmethod
    def __mul(cls, (a, b), (c, d)):
        return cls(a * (c + d) + b * c, a * c + b * d)
    def __mul__(self, other):
        return self.__mul(self, self.__unpack(other, "multiply by"))
    def __rmul__(self, other):
        return self.__mul(self.__unpack(other, "multiply"), self)

    @classmethod
    def __div(cls, (a, b), (c, d)):
        """Division.

        This solves: (a, b) = (c, d) * (x, y), i.e.
           a, b = c * (x + y) + d * x, c * x + d * y
        The equation for b re-arranges to:
           y = (b - c * x) / d
        Then that for a becomes:
           a * d - c * b = (c * d - c * c + d * d) * x
        whence
           x = (a * d - c * b) / (c * d - c * c + d * d)
        so:
           y = (b * (c * d -c * c +d * d) -c * (a * d -c * b)) / d / (c * d -c * c +d * d)
             = (b * c + b * d - a * c) / (c * d - c * c + d * d)
        Then (a, b) = (c, d) * (x, y), so (x, y) is (a, b) / (c, d).\n"""

        den = (c + d) * d - c * c
        x, y = a * d - c * b, b * (c + d) - a * c
        if den * (x / den) == x: x /= den
        else: x *= 1. / den
        if den * (y / den) == y: y /= den
        else: y *= 1. / den
        return cls(x, y)

    def __div__(self, other):
        return self.__div(self, self.__unpack(other, "divide by"))
    def __rdiv__(self, other):
        return self.__div(self.__unpack(other, "divide"), self)

    @classmethod
    def __unit(cls): return cls(0, 1)
    def __pow__(self, n):
        if n < 0: x, n = self.__div((0, 1), self), -n
        else: x = self

        n, i = divmod(n, 2)
        if i: ans = x
        else: ans = self.__unit()

        while n > 0:
            x *= x
            n, i = divmod(n, 2)
            if i: ans *= x
        return ans

    # Make it useful:

    @classmethod
    def beyond(cls, n=0, zero=0, one=1):
        """Returns an iterator over the Fibonacci sequence.

        All arguments are optional:

        This takes the given f(0) and f(1) and iterates i from n away from
        zero, yielding each f(i).  If n is 0, you'll get f(0), f(1), ...; if n
        is -1, you'll get f(-1), f(-2), ...; any other n simply skips the
        early entries in one of these.\n"""

        x = cls(1, 0)
        here = cls(zero, one) * x**n
        if n < 0: x = cls(1, -1)
        while True:
            yield here[0]
            here *= x

    @classmethod
    def fibonacci(cls, n, zero=0, one=1):
        return (cls(zero, one) * cls(1, 0)**n)[0]

class Fibonacci (object):
    """Cached computation of Fibonacci's sequence."""
    def __init__(self, zero=0, one=1):
        self.__natural, self.__negative = [ zero, one ], [ one - zero, 2 * zero - one ]

    def __iter__(self):
        i = 0
        while True:
            yield self[i]
            i += 1

    def __up(self, n):
        assert n >= 0
        row = self.__natural
        todo = n + 1 - len(row)
        while todo > 0:
            row.append(row[-1] + row[-2])
            todo -= 1

        return row[n]

    def __down(self, n):
        assert n > 0
        row = self.__negative
        todo = n - len(row)
        while todo > 0:
            row.append(row[-2] - row[-1])
            todo -= 1

        return row[n-1]

    def __getitem__(self, n):
        if isinstance(n, slice):
            seq, i = [], n.start or 0
            if n.step is None: step = -1 if n.start > n.stop else 1
            else: step = n.step
            while i < n.stop:
                seq.append(self[i])
                i += step

            return tuple(seq)

        if n < 0: return self.__down(-n)
        return self.__up(n)

    def __contains__(self, other, g=.5*(1 +5.**.5)):
        if self.__natural[:2] == [ 0, 1 ] and other > 0:
            # http://en.wikipedia.org/wiki/Fibonacci_number#Recognizing_Fibonacci_numbers
            lo, hi = g * other, 1./other
            lo, hi = lo - hi, lo + hi
            return lo <= int(hi) > 0

        raise NotImplementedError
