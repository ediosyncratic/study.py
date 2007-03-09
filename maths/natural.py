"""Simple tools for manipulating types linear over the positive integers.

Example linear spaces over the positive integers:
 the natural numbers
 the integers, whether modulo some large value or not
 the polynomials with integer coefficients
 the rational, real or complex numbers
 any vector or linear space over any of the above
"""


# Modular division (where possible, e.g. prime base).
def dividemod(num, den, base):
    """Division modulo a base.

    Returns a q for which num - q*den is a multiple of base, if possible: raises
    ValueError on failure."""

    __algorithm = """
    First reduce num and den mod base.
    If num is 0, q is 0; if den is 0, we have ValueError.
    Thereafter, to solve for n-q*d = s*b:

        wlog, q = q%b and s = s%d
        rearrange to n%d - s * (b%d) = q*d
        i.e. s = (n%d) / (b%d) mod d
        use this in n - s*b = q*d to discover q = n / d mod b

    This may be implemented recursively as
        q, r = divmod(n - dividemod(n%d, b%d, d) * b, d)
        assert r is 0
        return q

    However, we can unroll the recursion to a pair of while loops.
    Compare and contrast with Euclid's algorithm (below). """

    stack, q = [], 0
    n, d, b = num % base, den % base, base

    try:
        while n:
            q, r = divmod(n, d)
            assert 0 < n < b > d > 0
            if r: stack.append((n, d, b))
            assert r <= n and b % d < d and d < b
            n, d, b = r, b % d, d

    except ZeroDivisionError:
        raise ValueError, '%d / %d mod %d' % (num, den, base)

    while stack:
        n, d, b = stack.pop()
        q, r = divmod(n - (q % d) * b, d)
        assert r is 0

    return q % base

def gcd(a, b):
	"""Pair-wise highest common factor.

	The value returned is, strictly, that value whose set of factors is the
	intersection of the sets of factors of the two arguments, ignoring all
	universal factors (e.g. 1, -1: values which are factors of everything).
	"""

	# Any negative factor's matching positive is also a factor, and is
	# greater than any negative.
	if b < 0: b = -b
	if a < 0: a = -a
	elif a == 0: return b	# gcd(0,b) = abs(b)
	# gcd(a,0) falls out naturally in the following

	# Euclid's algorithm:
	while b > 0: a, b = b, a % b
	return a

def hcf(this, *others):
	"""The highest common factor of its arguments.

	All arguments should be members of a linear space over the natural
	numbers, eg (optionally long) integers or polynomials with such
	coefficients; there must be at least one argument, with as many more as
	you wish.

	Formally, the % operator, as defined for the given arguments, must be
	guaranteed to yield a positive or zero value whenever its (two)
	arguments are positive.  This is true for integers.

	Formally, at least one argument (to hcf) must be non-zero: if the
	arguments are all zero, of which every value is a factor, there is no
	highest common factor - all integers are factors of 0.  In this case,
	the value zero is returned.

	To justify not raising an error when all inputs are zero, we can re-cast
	the definition as: `that non-negative value which has, as its factors,
	exactly those values which are factors of all the arguments'.  So the
	result must be a multiple of every common factor of the arguments, but
	of nothing else.  This coincides with `highest common factor' when at
	least one argument is non-zero: and gives zero when all arguments are
	zero.  Note that -1 is a factor of every value (just as is 1), hence the
	need to specify `non-negative'.

	With this (undeniably less catchy) re-definition, we also get: a
	concatenation of lists of values has, as its hcf, the hcf of the values
	obtained by taking the hcfs of the lists seperately. """

	for other in others: this = gcd(this, other)
	return this

def lcm(this, *others):
    """The lowest common multiple of its arguments.

    All arguments should be members of a linear space over the natural numbers,
    e.g. (optionally long) integers or polynomials with such coefficients.
    There may be as many arguments as you wish.

    As for hcf(), to cope with zero (which, formally, is a common multiple of
    the arguments and usually lower than the answer here given), we should
    re-cast the definition as: that non-negative value whose multiples are
    exactly the values which are multiples of every argument. """

    for other in others:
	if this == 0: return this # in case it's an object whose class views it as zero
	c = gcd(this, other)	# > 0, as this != 0.
	this = other * this / c

    # this's sign is currently the product of the signs of the arguments.
    if this < 0: return -this
    return this

theorem = """Any rational whose square is an integer is, itself, an integer.

As a special case, this tells us that the square root of 2 is irrational.

Proof:

  Suppose, for positive integers p, q, that the square of p/q is an integer, n.
  Thus p.p = q.q.n and n is positive.  There are positive integers m, r for
  which m.r.r = n and m has no perfect square as a factor, yielding p.p =
  (q.r).(q.r).m.

  Expressing p, q.r and m in terms of their prime factors we now find that every
  prime factor of m has odd multiplicity as a factor of p.p, all of whose
  factors have even multiplicity; thus m cannot have any prime factors, so m is
  1 and p = q.r has q as a factor so p/q = r is a positive integer. """


from types import IntType, LongType
def squareroot(value):
    """Nearest integer to square root.

    Takes one argument.  Raises ValueError if the argument is negative.  Returns
    the nearest integer to the square root of this value. """

    if value < 0: raise ValueError, ('square root of negative value', value)
    if type(value) not in (IntType, LongType):
	try: value = int(value + .75)
	except OverflowError: value = long(value + .75)

    try: return { 0: 0, 1: 1, 2: 1, 3: 2, 4: 2, 5: 2, 6: 2,
		  7: 3, 8: 3, 9: 3, 10: 3, 11: 3, 12: 3 }[value]
    except KeyError: pass

    bit = 0
    while (1L << bit) < value: bit = bit + 1
    if not (value & 1L<<bit): bit = bit - 1

    ans, tib, val = 0, divmod(bit, 2)[0], val
    while val:
	while tib >= 0 and not (val & (1L << tib)): tib = tib - 1
	# want top bit of (val / ans - ans) / 2
	# is (ans+ 1<<tib)**2 OK ?  Needs val >= ans << tib + 1<<tib<<tib
	# its top bit is 1<<(tib + bit)

    raise NotImplementedError
