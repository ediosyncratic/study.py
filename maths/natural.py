"""Simple tools for manipulating types linear over the positive integers.

Exports:
  dividemod(num, den, base) -- division modulo a base
  gcd(a, b) -- pair-wise hcf
  hcf(a, ...) -- highest common factor
  lcm(a, ...) -- least common multiple
  Euclid(a, b) -- solve for i, j for which: a*i + b*j == hcf(a, b)
  factorsum(N) -- sum of proper factors of N
  perfect() -- iterator over all N for which N == factorsum(N)
  Collatz(n) -- iterate the Collatz conjecture's sequence starting at n
  desquare(n) -- integer square root with remainder
  unsquare(n) integer square root of perfect square; else ValueError
  sqrt(n) -- integer square root, discarding remainder
  isprime(n) -- test whether a natural is prime
  eachprime() -- iterate over the primes
  depower(n, p) -- as desquare, but for p-th power
  naturals -- list: naturals[i][naturals[j]] is naturals[j+1] iff i > j are natural
  lattice(dim, [signed, [mode, [total]]]) -- iterator over tuples of whole numbers

Example linear spaces over the positive integers:
 the natural numbers
 the integers, whether modulo some value or not
 the polynomials with integer coefficients
 the rational, real or complex numbers
 any vector or linear space over any of the above

In particular, lattice (q.v.) provides for iteration over the space of tuples,
of any given length, whose entries are integers or naturals.

See study.LICENSE for copyright and license information.
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

def hcf(*args):
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
    for n in args:
        # Any negative factor's matching positive is also a factor, and is
        # greater than any negative.
        if n < 0: n = -n
        # Euclid's algorithm (see also its extension, Euclid(), below):
        while n:
            n, this = this % n, n
    return this

# Legacy / Alias: "greatest common divisor".
gcd = hcf

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
        c = hcf(this, other)    # > 0, as other != 0.
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

def bitcount(i, eff=0xffffffff, seven=0x77777777, three=0x33333333, one=0x11111111):
    """Count the number of bits set to 1 in an integer.

    Supply just one input: the integer (which may be long) whose bit-count is
    desired.

    The sign bit is treated as a 1, for these purposes; a negative integer
    scores one higher than the corresponding positive integer - this need not
    actually be faithful to how negative numbers are represented by the
    machine.  For the algorithm used on each word, see fortune -m BITCOUNT\n"""
    if i < 0: count, i = 1, -i # treat sign as one bit
    else: count = 0

    while i:
        # Chomp off a word:
        x, i = int(i & eff), i >> 32
        # Each term is x shifted right but without the bits, from the bottom of
        # each half-byte, that shifted into the next half-byte down.  Subtracting
        # the sum of these terms reduces each half-byte to its bitcount.
        x -= ((x >> 1) & seven) + ((x >> 2) & three) + ((x >> 3) & one)
        # Each half-byte now holds a bit-count; we want to sum them. Each is at
        # most 4, so summing adjacent half-byte will never over-flow any
        # half-byte; we can then discard alternate half-bytes so that the sum of
        # half-bytes is unchanged; and equal to the sum of bytes since each byte
        # has clear top half.  Bytes are digits base 256; so summing bytes of a
        # word gives a value equal, mod 256-1, to the word itself; as long as we
        # have less than 32 bytes in our word, the byte sum is less than 255, so
        # equal to the word reduced mod 255.
        count += ((x + (x >> 4)) & 0x0f0f0f0f) % 255

    return count

theorem = """Any rational whose square is an integer is, itself, an integer.

As a special case, this tells us that the square root of 2 is 'irrational'.

Proof:

  Suppose, for positive integers p, q, that the square of p/q is an integer, n.
  Thus p.p = q.q.n and n is positive.  There are positive integers m, r for
  which m.r.r = n and m has no perfect square > 1 as a factor, yielding p.p =
  (q.r).(q.r).m.

  Expressing p, q.r and m in terms of their prime factors we now find that every
  prime factor of m has odd multiplicity as a factor of p.p, all of whose
  factors have even multiplicity; thus m has no prime factors, so m is 1 and p =
  q.r has q as a factor so p/q = r is a positive integer.\n"""

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

def desquare(val):
    """Whole square root with remainder.

    Input, val, is a non-negative real, typically a natural.  Raises ValueError
    if negative.  Otherwise, returns a twople n, v with n natural, n*n + v ==
    val and 0 <= v < 2*n+1.\n"""
    if val < 0: # Every natural's square exceeds val.
        raise ValueError('Negative value has no square root', val)

    # Find least bit = 2**ind > sqrt(val):
    v, ind, bit = val, 0, 1
    while v >= 1:
        v /= 4
        ind += 1
        bit <<= 1

    # input = val # hereafter, val holds input - v**2
    v = 0
    while ind and val:
        assert v & (bit - 1) == 0 and bit == (1 << ind)
        assert ((v << 1) +bit) << ind > val > 0
        # i.e. (v + bit)**2 = v**2 +(2*v +bit)*bit > input > v**2
        bit >>= 1
        ind -= 1
        up = ((v << 1) | bit) << ind
        assert up == (v | bit)**2 - v**2
        # Do we want to set this bit ?
        if up <= val: # (v + (1<<ind))**2 == input -val +up <= input
            v |= bit
            val -= up

    assert 0 <= val < 1 + (v << 1) # v**2 <= input < (1+v)**2
    return v, val

def unsquare(val):
    "Return n with n*n == val or raise ValueError if no such n"
    v, n = desquare(val)
    if n: raise ValueError("Not a perfect square", val, n, v)
    return v

def sqrt(val):
    "max({natural n: n*n <= val})"
    return desquare(val)[0]

def factor(n):
    """Returns a proper factor of n, if it has one.

    If n is prime, returns None; if n < 0, returns -1; if n is 0 (everything is
    a factor of it) or 1 (has a multiplicative inverse), returns n.  Otherwise,
    returns a number s, strictly between 1 and n, for which n % s = 0.  This
    number need not be prime, but is a proper factor of n.\n"""
    if n < 0: return -1
    if n in (0, 1): return n
    s, r = desquare(n) # (s+1)**2 > n >= s**2
    if not r: return s # n is s * s
    while s > 1:
        if n % s: s -= 1
        else: return s
    return None

def isprime(n):
    "Tests whether a natural is prime"
    return factor(n) is None

def eachprime():
    """A trivial iterator over all primes.

    Contrast study.maths.primes and study.maths.prime, both for efficiency and
    for complexity !  This should be entirely adequate for primes up to modest
    size; while I typed this sentence, printing each yield of this iterator
    got me to primes well beyond half a million.  It'll slow down eventually,
    and doesn't remember what it's done before, but it may be all you need.\n"""
    n = 1
    while True:
        n += 1
        if isprime(n): yield n

def depower(val, p):
    """Whole inverse of a power, with remainder.

    Requires two arguments, val and p with val real (typically natural) and p
    a positive natural (typically > 1).  If p is even and val < 0, raises
    ValueError.  Otherwise, returns a twople n, v with n an integer,
    satisfying n**p +v = val, with n and v having the same sign as val and v
    as small as possible (so rounding towards zero; contrast divmod, which
    rounds down).\n"""

    if p == 0:
        if val == 1: return 1, 0
        raise ValueError("Zeroth power only produces 1 as value", val, p)
    elif p == 1:
        n = int(val) # int() rounds towards zero
        return n, val - n
    elif p == 2: return desquare(val) # more efficient
    elif p < 3 or p != int(p):
        raise ValueError("Unsupported power to undo", p, val)

    p = int(p)
    if val < 0:
        if not p % 2:
            raise ValueError("Negative value nas no even root", val, p)
        n, v = depower(-val, p)
        return -n, -v

    elif val < 1: return 0, val # trivial

    # Find highest bit = 1 << ind with bit**p <= val:
    v, ind, bit, pb = int(val), 0, 1, 1
    while True:
        v >>= p
        if not v: break
        ind += 1
        bit <<= 1
        pb <<= p
    assert val >> p < pb <= val and pb == bit**p

    v = bit # start building our answer, one bit at a time
    # input = val # hereafter, val shall store input - v**p
    val -= pb

    while val and ind > 0:
        ind -= 1
        bit >>= 1
        pb >>= p
        assert pb == bit**p and not v & bit
        # term = lambda i: v**i * bit**(p-i) * p!/i!/(p-i)!
        # sum(map(term, range(p))) +v**p is (v + bit)**p
        # We need to compare sum(map(term, range(p))) with val.
        i, t, up = 1, pb, pb # i, term(i-1), sum(term, range(i))
        while i < p:
            # term(i) = (term(i-1) >> ind)*v*(p+1-i)/i
            assert not t & (bit - 1) # i.e. t % bit is 0
            t *= v * (p + 1 - i)
            t, r = divmod(t >> ind, i) # t /= bit * i
            assert 0 == r # the division was exact
            up += t
            i += 1

        assert up == (v | bit)**p - v**p
        if up <= val:
            v |= bit
            val -= up

        assert pb == 1 << (p * ind) and bit == 1 << ind and not v & (bit - 1)
    assert not val or bit == 1 # i.e the next >>= 1 would have wiped it.

    return v, val

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
        q, r = divmod(n, 2)
        n = 3 * n + 1 if r else q
        yield n

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
# NB: len(str(naturals[1+n])) + 2 = 13 * 3**n

def lattice(dim, signed=False, mode=True, total=None):
    """Iterator over {({whole numbers}:|dim)}

    Required argument, dim, is the dimension of the lattice, i.e. the length of
    each tuple yielded by the resulting iterator.

    Optional arguments:
      signed -- selects whether to iterate over an integer lattice (when signed
                is true) or a natural (non-negative integer) lattice (when
                signed is false, as it is by default).
      mode -- None to iterate over sets, False to skip permutations of earlier
              results, True (the default) to iterate over all tuples; or a
              number used in implementing these; see below for details.
      total -- defaults to None; otherwise, restrict iteration to those tuples
               whose sum of absolute values is total.

    Returns an iterator which yields dim-tuples whose entries are whole numbers
    of the indicated kind, subject to mode's constraint.  For present purposes,
    use an ordering on the integers in our tuples which treats each negative
    integer -n as the positive value n-.5.
     * When mode is None or -ve, each tuple yielded is in strictly decreasing
       order (so entries with smaller absolute value appear later than those
       with larger absolute values; and, for any natural n, -n appears after n
       if both appear); when mode is -ve, every entry is, furthermore, less than
       -mode (which is either a natural or half more than a natural, to encode a
       negative entry);
     * when mode is False or +ve, each tuple yielded is in never-increasing
       order (like decreasing, but allows an entry to be repeated); when mode is
       +ve it is an upper bound on the entries;
    * otherwise, mode is True and there is no constraint on the order of entries
      in the tuple.\n"""

    if total is None:
        total = 0
        while True:
            for it in lattice(dim, signed, mode, total):
                yield it
            total += 1

        assert False, 'We should never get here !'

    elif dim == 0 and total == 0: yield ()
    elif dim < 1 or total < 0: pass
    else: # The non-trivial case:

        # Initialize i to the least allowable first entry in our tuple:
        if signed:
            if mode is True or mode is False: i = - total
            elif mode is None or mode < 0:
                # sum of abs of dim distinct integers is at least:
                if dim % 2: tail = (dim // 2) * (dim // 2 + 1)
                else: tail = (dim // 2)**2
                if total < tail: raise StopIteration
                elif mode is None or total + mode < 0: i = - total
                else: i = int(mode)
            else:
                assert mode >= 0
                if total < mode: i = - total
                else:
                    i = - int(mode + .6)
                    if -i * dim < total: raise StopIteration

        else:
            i = 0
            if mode is True or mode is False: pass
            elif mode is None or mode < 0:
                if total < dim * (dim - 1) / 2: raise StopIteration
            else:
                assert mode >= 0
                if mode * dim < total: raise StopIteration

        i -= 1 # to counter first iteration's += 1
        while i < total:
            i += 1
            if mode is True: clip = mode
            elif mode is None:
                if i < 0: clip = i + .5
                else: clip = -i
            elif mode is False:
                if i < 0: clip = -(i + .5)
                else: clip = i
            elif mode < 0:
                if i < 0: clip = i + .5
                elif i >= -mode: break
                else: clip = -i
            else:
                assert mode >= 0
                if i > mode: break
                else: clip = abs(i)
                if total > clip * dim: continue

            for it in lattice(dim-1, signed, clip, total - abs(i)):
                yield (i,) + it
