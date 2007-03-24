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
"""


def fibonacci(what, was=0, result=1):
	"""Computes Fibonacci's sequence.

        Required first argument is the index into Fibonacci's sequence: it
        should be an integer and may be negative.  (If it is not an integer, it
        is effectively rounded towards zero.)  Optional second and third
        arguments are the sequence's entries with indices 0 and 1 respectively;
        their defaults are 0 and 1, respectively.

	Defined here by: for all integers n, f(n+1) = f(n) + f(n-1).  For the
	default initial entries, this gives f(-n) = f(n) for odd n, f(-n) =
	-f(n) for even n; in particular, both f(-1) and f(1) are 1.  Note that
	conventional definitions tend to index from 1 rather than zero and take
	the first two entries to be 1: this helpfully matches what we get here,
	since f(2) = f(1) + f(0) = 1 + 0 = 1.

	Performs its calculations assuming its data to be integers, so handles
	overflow by coercing values to indefinite-precision integers (Python's
	long integer type).  If actual initial values are floats, this handling
	of overflow is probably wrong; you may be better off using
	basEddy.bigfloat.BigFloat()s.\n"""

	if -1 < what < 1: return was
	while what > 1:
		# f(j-1), f(j) = f(i), f(i) + f(i-1)
		# j = 1+i
		try: was, result = result, result + was
		except OverflowError:
			was, result = result, long(result) + was
		what = what - 1

	while what < -1:
		# f(j), f(1+j) = f(1+i) - f(i), f(i)
		# j = i-1
		try: result, was = was - result, result
		except OverflowError:
			result, was = was - long(result), result
		what = what + 1

	return result

def fibtimes((a,b), (c,d)):
	"""A multiplication with which to speed Fibonacci computation.

	See: http://www.inwap.com/pdp10/hbaker/hakmem/hakmem.html

	Define multiplication on ordered pairs

	(A,B) (C,D) = (A C + A D + B C, A C + B D).

	This is just (A X + B) * (C X + D) mod X^2 - X - 1, and so is
	associative, etc. We note (A,B) (1,0) = (A + B, A), which is the
	Fibonacci iteration. Thus, (1,0)^N = (FIB(N), FIB(N-1)), which can be
	computed in log N steps by repeated squaring, for instance.\n"""

	return a * (c + d) + b * c, a * c + b * d

def fibpow(n, a=1, b=0):
	c, d = 0, 1 # the identity
	while n > 0:
		n, i = divmod(n, 2)
		if i: c, d = fibtimes((a,b),(c,d))
		a, b = fibtimes((a,b), (a,b))
	return c, d

def fastonacci(n, zero, one):
	"""As for fibonacci, but computed in logarithmic time :-)
	"""

	return fibtimes((zero, one), fibpow(n))[0]

# or ...
class Fibonacci:
    def __init__(self, zero=0, one=1):
	self.__natural, self.__negative = [ zero, one ], []

    def __up(self, n):
	assert n >= 0
	row = self.__natural
	left = n + 1 - len(row)
	while left > 0:
	    row.append(row[-1] + row[-2])
	    left = left - 1

	return self.__natural[n]

    def __down(self, n):
	assert n > 0
	row = self.__negative
	left = n - len(row)
	while left > 0:
	    row.append(row[-2] - row[-1])
	    left = left - 1

	return row[n-1]

    def __getitem__(self, n):
	if n < 0: return self.__down(-n)
	return self.__up(n)
