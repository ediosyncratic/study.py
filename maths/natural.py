"""Simple tools for manipulating types linear over the positive integers.

Example linear spaces over the positive integers:
 the natural numbers
 the integers, whether modulo some value or not
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
    universal factors (e.g. 1, -1: values which are factors of everything).\n"""

    # Any negative factor's matching positive is also a factor, and is
    # greater than any negative.
    if b < 0: b = -b
    if a < 0: a = -a
    elif a == 0: return b	# gcd(0,b) = abs(b)
    # gcd(a,0) falls out naturally in the following

    # Euclid's algorithm (see also its extension, below):
    while b > 0: a, b = b, a % b
    return a

def hcf(*others):
    """The highest natural common factor of its arguments.

    All arguments should be members of a linear space over the natural numbers,
    eg (optionally long) integers or polynomials with such coefficients; there
    may be arbitrarily many arguments.

    Formally, the % operator, as defined for the given arguments, must be
    guaranteed to yield a positive or zero value whenever its (two) arguments
    are positive.  This is true for integers.

    Formally, at least one argument (to hcf) must be non-zero: if (there are no
    arguments, or) the arguments are all zero, of which every value is a factor,
    there is no highest common factor - all integers are factors of 0.  In this
    case, the value zero is returned.

    To justify not raising an error when (there are no inputs, or) all inputs
    are zero, we can re-cast the definition as: `that non-negative value which
    has, as its factors, exactly those naturals which are factors of all the
    arguments' (reading 'is n a factor of all arguments' as the negation of 'no
    argument does not have n as a factor' for the case of no arguments).  So the
    result must be a multiple of every common factor of the arguments, but of
    nothing else.  This coincides with `highest common factor' when at least one
    argument is non-zero: and gives zero when all arguments are zero.  Note that
    -1 is a factor of every value (just as is 1), hence the need to specify
    `non-negative'.

    With this (undeniably less catchy) re-definition, we also get: a
    concatenation of lists of values has, as its hcf, the hcf of the values
    obtained by taking the hcfs of the lists seperately; i.e. hcf is a
    transitive binary operator.\n"""

    this = 0
    for other in others: this = gcd(this, other)
    return this

def lcm(*others):
    """The smallest (non-negative) common multiple of its arguments.

    All arguments should be members of a linear space over the natural numbers,
    e.g. (optionally long) integers or polynomials with such coefficients.
    There may be arbitrarily many arguments.

    If any entry is zero, so is the result: zero is a multiple of everything and
    nothing else is a multiple of zero, so it is the only candidate.  The return
    when no arguments are supplied is 1, to ensure that lcm is associative.\n"""

    this = 1
    for other in others:
	if not other: return other # in case it's an object whose class views it as zero
	c = gcd(this, other)	# > 0, as other != 0.
	this = other * this / c

    # this's sign is currently the product of the signs of the arguments.
    if this < 0: return -this
    return this

def Euclid(a, b):
    """Solves a*i + b*j == hcf(a, b)

    Takes two naturals (or members of a suitable ring), a and b; returns a pair
    i, j for which: a*i + b*j == hcf(a, b).  Uses the extended version of
    Euclid's algorithm.  When the hcf is 1, we have (a*i) % b == 1 == (b*j) % a,
    so i, j are multiplicative inverses of a, b modulo one another.\n"""
    if b == 0: return cmp(a, 0), 0
    q, r = divmod(a, b)
    i, j = Euclid(b, r)
    # hcf == i * b + j * (a - q * b) == j * a + (i - q * j) * b
    return j, i - q * j

theorem = """Any rational whose square is an integer is, itself, an integer.

As a special case, this tells us that the square root of 2 is 'irrational'.

Proof:

  Suppose, for positive integers p, q, that the square of p/q is an integer, n.
  Thus p.p = q.q.n and n is positive.  There are positive integers m, r for
  which m.r.r = n and m has no perfect square > 1 as a factor, yielding p.p =
  (q.r).(q.r).m.

  Expressing p, q.r and m in terms of their prime factors we now find that every
  prime factor of m has multiplicity 1 (which is odd) as a factor of p.p, all of
  whose factors have even multiplicity; thus m has no prime factors, so m is 1
  and p = q.r has q as a factor so p/q = r is a positive integer."""

_early_primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
def factorsum(N):
    """Returns the sum of the proper factors of N.

    Thus N is perfect precisely if N == factorsum(N).
    """

    # Find upper bound, i, on highest proper factor of N:
    for p in _early_primes:
        i, r = divmod(N, p)
        if r == 0: break

    S = 0 # sum proper factors:
    while i > 0:
        if N % i == 0: S += i
        i -= 1

    return S

def perfect():
    """Returns an iterator over the perfect numbers.

    All known perfect numbers are 2**n * (2**(1+n) -1) for some positive natural
    n (for which the second factor, 2**(1+n) -1, is prime), so iterating over
    these (which is *much* quicker, even pausing to check primality of the
    second factor) shall (probably) yield the same result.  The iterator yielded
    by this implementation checks for other perfect numbers and prints a message
    if it ever finds one.\n"""

    i = 1
    while True:
        # functionally, call factorsum(i): but abort if > i.
        for p in _early_primes:
            j, r = divmod(i, p)
            if r == 0: break

        S = 0
        while j > 0 and S <= i:
            if i % j == 0: S += j
            j -= 1

        if i == S: # iff i == factorsum(i)
            yield i
            # Check to see if i fits the usual pattern:
            n = 0 # max power of two that is a factor of i.
            while not (i & (1L << n)): n += 1
            if (i >> n) != ((1L << (1+n)) -1):
                print 'Unusually perfect', hex(i), n, hex(i >> (n-1))

        i += 1

def Collatz(n):
    """Iterator for the Collatz conjecture's sequence for n.

    It is conjectured that, whatever positive integer n you give to this
    function, the resulting sequence shall ultimately terminate (by yielding 1).
    It is known (by experiment) that the conjecture is good up to n = 10 * 2**58
    (and, hence, also good for any n which is just a power of 2 times some
    positive integer less than this limit).

    The function iterated is, with Z+ = {positive integers}, the union of (: n
    &larr; 2.n :Z+) and (: 6.j+4 &larr;2.j+1 :Z+).  The conjecture effectively
    says that its transitive closure subsumes ({1}:|Z+).\n"""

    yield n
    while n != 1:
        if n % 2: n = 3 * n + 1
        else: n = n / 2
        yield n

    raise StopIteration

def sqrt(val):
    """Returns the highest natural whose square does not exceed val"""
    if val < 0: # Every natural's square exceeds val.
        raise ValueError('Negative value has no square root', val)

    v, bit = val, 0
    while v:
        v >>= 2
        bit += 1

    # input = val; assert v == 0 # hereafter, val stores input - v**2
    bb = 1 << (2 * bit) # > val but <= val*4

    while bit and val:
        assert (v<<(1+bit)) +bb > val > 0 and bb == (1 << 2 * bit)
        # i.e. (v + (1<<bit))**2 = input - val +(v<<(1+bit)) +bb > input > v**2
        bb >>= 2
        up = (v << bit) + bb
        assert v & ((1 << bit) -1) == 0
        bit -= 1
        assert up == (v | (1<<bit))**2 - v**2
        if up <= val: # (v + (1<<bit))**2 == input -val +up <= input
            v |= 1 << bit
            val -= up

    # v**2 <= input < (1+v)**2
    return v

# and now for something python-2.2-specific:
class Naturals (list):
    class Suc (dict):
        def __init__(self, bok=None):
            self.clear()
            if bok is not None:
                self.update(bok)
                self[bok] = self

        def suc(self): return self.__class__(self)

    Suc.__hash__ = Suc.__len__
    def __init__(self, zero=Suc()): self[:] = [zero]
    del Suc
    __upget = list.__getitem__
    def __getitem__(self, ind):
        while ind >= len(self): self.append(self[-1].suc())
        return self.__upget(ind)

naturals = Naturals()
del Naturals
# NB: len(str(naturals[1+n])) = 13 * 3**n -2
