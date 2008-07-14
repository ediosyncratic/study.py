"""Support for the multi-octet format for storing data on primes.

Once we've identified the first n primes, we can assert that all other primes
are equal, modulo the product of the first n, to one of the numbers less than
this product and coprime to it.  For n = 1 this just says every prime after the
first is odd; and, for n = 2, that every prime after the first two is equivalent
to either 1 or 5 modulo 6.  For n = 3, it turns out there are exactly eight
candidates modulo 30; and, for all higher n, the number of candidates is a
multiple of eight.  Let

  P = (: product(: primes[j] &larr; j: i) &larr;i :{naturals})
  N = (: product(: primes[j]-1 &larr; j: i) &larr;i :{naturals})
  C = (: {p in P(i): p is coprime to P(i)} &larr;i :{naturals})

Then every prime after the first n is equvalent, modulo P(i), to some member of
C(i).

    Theorem: for each natural n, C(n) has N(n) members.

    Proof: for n = 0, P(0) and N(0) are products of empty lists, so formally 1;
    all primes (indeed, all naturals) are zero modulo 1 = P(0) so C(0) has
    exactly N(0) = 1 member.

    Suppose, then, that C(n) has N(n) members for some natural n; we know
    this holds true for n = 0 and it suffices, inductively, to show that (for
    general n) this implies that C(1+n) has N(1+n) members.

    Trivially, C(1+n) is subsumed by Q(n) = {p in P(1+n): p is coprime to P(n)},
    since everything coprime to P(1+n) is also coprime to P(n); but Q(n) is
    equally {c + P(n)*p: p in primes[n], c in C(n)}, since P(1+n) = primes[n] *
    P(n) and every number coprime to P(n) is equal, mod P(n), to a member of
    C(n).  Equally trivially, Q(n) has primes[n] * N(n) members; of these, the
    ones not coprime to P(1+n) necessarily (since they are coprime to P(n); and
    P(1+n) only has one other prime factor) of form c * primes[n] with c coprime
    to P(n), i.e. c in C(n).  Each primes[n] * c with c in C(n) necessarily is
    coprime to P(n), hence in Q(n), and isn't coprime to P(1+n), since it has a
    factor of primes[n]; and there are exactly N(n) of these, one for each
    member of C(n).

    The remaining members of Q(n) are exactly the members of C(1+n); Q(n) has
    N(n)*primes[n] members and C(1+n) leaves out N(n) of them, so has
    N(n)*(primes[n] -1) = N(1+n) members, which is what we set out to prove.

One can actually use the same argument to show that, modulo the product of an
arbitrary finite set of primes, all other primes fall in a set whose size is the
product of the numbers one less than the primes in the given set of primes.  The
primes for which the ratio p / (p-1) is highest are the smallest ones, so we
gain the best ratio of modulus to size of candidate set by selecting an initial
sequence of the primes, for given size of set of primes.

Thus, for natural n and positive natural i, every prime between i*P(n) and
(1+i)*P(n) is i*P(n) + c for some c in C(n); so we can record which numbers in
this interval are prime by recording C(n) bits, each associated with one c in
C(n); we set this bit precisely if i*P(n) + c is prime.  Since N(3) = 8 and
every N(n) with n >= 3 is a multiple of N(3), the number of bits involved is
always a multiple of 8, hence neatly fills a whole number of bytes.  Thus N(n)/8
bytes suffice, for each positive natural i, to record all the primes from i*P(n)
to (1+i)*P(n); and, for the case i = 0, we can do the same as long as we
separately remember the first n primes (which are excluded since not coprime to
themselves).  To remember the primes >= i*P(n) and < (k+i)*P(n) then takes
k*N(n)/8 bytes packaged by a mildly sophisticated class that knows how to
interpret them.

  For example, it takes 41943040 bytes (40 MB) to store the 4194304 primes from
  310268261 to 392796199, one per line, in ASCII decimal, with nothing else in
  the file but a newline to separate each from the next (with a bit of extra
  punctuation and formatting, my first implementation of the primes needed a bit
  over 44.5 MB to store this lot).  If we store these numbers in raw binary,
  each takes up 29 bits and we could have a header on the binary file declaring
  that fact, so we'd need no delimiter; this would reduce the file size to 14.5
  MB - nearly a factor of three smaller.  The primes up to (and including) 17
  have product P(7) = 510510, so our 40 MB of primes span (somewhat less than)
  162 intervals; multiplying by N(7)/8 = 11520 bytes, this yields 1.78 MB needed
  to store the same information - a further factor of eight smaller.  Of course,
  there may be differences in how well general purpose compression algorithms
  can shrink each of these formats; but it is unlikely such differences would
  suffice to counter the factor of eight or twenty-two we gain by exploiting the
  intrinsic nature of the data.  Furthermore, generic compression only helps for
  the data as stored on disk (you'll have to uncompress it to make use of it);
  whereas exploiting the properties of the primes gives us a compressed format
  which we can use directly when loaded into the computer's memory.

One can exploit the same approach, albeit without getting down to bit-fields, to
reduce the amount of memory needed to record a proper factor of each non-prime;
if a number isn't equivalent, mod P(n), to a member of C(n), then we can find a
factor of it easilly, since we only have to check primes[:n].  Otherwise, we
need to record an actual proper factor; by always chosing its least proper
factor, we can keep the memory overhead low.  We thus need to record only
N(n)/P(n) times as much information as we otherwise would; at n = 7, this is
0.18; we need less than a fifth as much disk space (and memory) to do the job.

For reduction modulo 30, the first case where we have a multiple of 8 as our
number of primes, and for the earlier cases, the list of values coprime to our
modulus is conveniently just the list of primes up to it.  This happens because
the square of the next prime is bigger than it, 7*7 > 5*3*2, but this never
happens again:7*5*3*2 = 210 > 121 = 11*11 and the product is growing roughly
combinatorially, while the square is roughly quadratically, which is
comparatively slow.  So 30 is the last time that the simple list of primes up to
the modulus suffices as the list of coprimes.

$Id: octet.py,v 1.12 2008-07-14 05:57:56 eddy Exp $
"""

def coprimes(primes):
    """Determine the candidate primes modulo the product of some primes.

    Required argument, primes, is a finite sequence of distinct primes.  Returns
    a pair (prod, vals); prod is the product of the primes, vals is a tuple of
    the values, modulo prod, coprime to prod; i.e. the return is (P(n), C(n)) if
    the input is primes[:n].

    This implementation iteratively constructs each C(1+i) by generating Q(i)
    and filtering out {primes[i] * c: c in C(i)}, exactly according to the
    process in the proof above (see file comment).\n"""

    row, last = (0,), 1
    for p in primes:
        row = filter(lambda i, p=p: i % p != 0,
                     reduce(lambda x, y: x + y,
                            map(lambda i, s=last, r=row: map(lambda j, n=s*i: j+n, r),
                                range(p)),
                            []))
        last *= p

    return last, tuple(row)

from study.snake.sequence import Tuple

class OctetType (Tuple):
    __slots__ = ('primes', 'modulus', 'size')
    def __tuple__(self, val, seq=Tuple): return seq(val) # for slicing, etc.

    __upinit = Tuple.__init__
    def __init__(self, ps):
        """Sets up a descriptor for a type of octet-based block.

        Required argument, ps, is an initial segment of the infinite sequence of
        primes.\n"""

        assert tuple(ps[:3]) == (2, 3, 5), \
               "I need at least the first three primes to work with octets"
        # ... actually, 17 and arbitrary others would suffice without them; and
        # 2's not crucial.  But I want an initial chunk of primes, anyway.

        self.primes = tuple(ps)
        self.modulus, vals = coprimes(ps)
        self.__upinit(vals)
        self.size, r = divmod(len(vals), 8)
        assert r == 0

    def index(self, p):
        """Find where p is in self.

        Returns that i for which self[i] is p, if any; otherwise, raises
        ValueError(i) where i is the least for which self[i] > p or i is
        len(self) if no entry in self is > p.  Given that self[-1]+1 is
        self.modulus, when p is given modulo this modulus it can't cause
        ValueError(len(self)).\n"""
        if self[0] == p: return 0
        if p < self[0]: raise ValueError(0)
        if p > self[-1]: raise ValueError(len(self))

        lo, hi = 0, len(self) - 1
        if self[hi] == p: return hi
        while hi > 1 + lo:
            mid = (lo + hi) // 2
            if self[mid] > p: hi = mid
            elif self[mid] < p: lo = mid
            else: return mid

        raise ValueError(hi)

    def iterate(self, start):
        """Returns a perpetual iterator over candidates.

        Required argument, start, is a natural you think might be a prime.  Each
        yield is the next value that has any chance of being a prime, i.e. is
        coprime to self.modulus; we never run out of such values, so this
        iterator never terminates; its values, modulo self.modulus, cycle
        through the candidates iter(self) would yield.\n"""
        n, r = divmod(start, self.modulus)
        try: i = self.index(r)
        except ValueError, what:
            i = what.args[0]
            assert i < len(self)
            assert i == 0 or self[i-1] < r < self[i]

        base = n * self.modulus
        while True:
            yield self[i] + base
            i += 1
            if i >= len(self):
                base += self.modulus
                i -= len(self)

    def factor(self, k):
        for p in self.primes:
            if not k % p: return p
        return None

del Tuple
def OctetType(ps, cache={}, klaz=OctetType):
    ps = tuple(ps) # just to be sure ...
    try: ans = cache[ps]
    except KeyError:
        cache[ps] = ans = klaz(ps)
    return ans

from study.snake import regular

class Octet (object):
    """Base class for octet-structured mappings from a range of naturals.
    """

    def __init__(self, kind, base, data=None, count=None, gap=regular.Interval):
        """Set up to describe a range of the naturals..

        Required arguments:
          kind -- an OctetType describing the representation to use
          base -- beginning of the range of naturals to be described

        Optional argument:
          data -- length, within range, for which data is already available.
          count -- number of such blocks to be described; or None (default), to
                   let it be determined by data.

        Given data, this computes a length sufficient to use all of it, rounding
        up to the next full block size for its kind if needed; if count is None
        or less than this computed length, it is rounded up.  Caller is
        responsible for remembering data; aside from len(data) its only use here
        is to make an exception more information-rich.\n"""

        assert base % kind.modulus == 0
        if data:
            q, r = divmod(data, kind.size * 8)
            if r: q += 1
            if count is None or q > count: count = q
        elif count is None:
            raise ValueError('I need to know how big to be', count, data)

        self.kind, self._count = kind, count
        self.span = gap(base, count * kind.modulus)

    def keys(self): return range(self.span.start, self.span.stop)
    def items(self): return map(lambda k, b=self: (k, b[k]), self.span)
    def values(self): return map(lambda k, b=self: b[k], self.span)
    # But you probably don't want to use those last three !
    def __len__(self): return len(self.span)

class Sieve (Octet):
    """Mixin to provide sieving for octets.

    Derived classes need to implement __getitem__, which should raise
    LookupError rather than report a value p as prime, unless .valid(p); also a
    mark(p) method, which updates information on primes or factors to record the
    multiples of p, and a prime(i) method, which returns true if i is a prime.
    The last two may sensibly be inherited from a base class.

    An instance of this class, when iterated, yields all the primes in its .span
    (note that this is different to the usual default for mappings, including
    Octet, which is to iterate the keys(), which is the whole span in our case),
    including any in the initial data whose length was given to the constructor.
    After an iteration of it has completed, aside from any further iteration
    (which just repeats the prior), it behaves the same as the non-sieving
    version of its class.\n"""

    def __init__(self, primes, given=0):
        """Initialize for sieving.

        Required argument:
          primes -- an iterable whose iterator shall yield every prime up to
                    self.span.stop.

        Optional third argument, given, is the length of an initial portion of
        self.span for which full data is already available when initialized.\n"""

        self.__src, self.__valid = iter(primes), given

    def valid(self, ind): return ind < self.span.start + self.__valid

    def __iter__(self):
        base = self.span.start
        k = self.kind.iterate(base)
        i, at = k.next(), base

        while i < self.span.stop:
            if i < self.__valid + base:
                if self.prime(i): yield i
                i = k.next()
            else:
                p = self.__src.next()
                self.mark(p)
                self.__valid = max(self.__valid, (p + 1) ** 2 - at)

        raise StopIteration

class FlagOctet (Octet):
    """A mapping from a range of naturals to True if prime, else False.
    """
    __upinit = Octet.__init__
    def __init__(self, kind, base, data='', count=None):
        """Set up a multi-byte flag octet.

        For arguments, see Octet.__init__, with the optional data being here a
        string encoding (an initial portion of) the data for the blocks
        described (default: empty); it'll be padded to the required length with
        '\0' bytes.  Caller (typically a derived class) is responsible for
        keeping track which parts of the result are padding and which are real
        data.\n"""

        self.__upinit(kind, base, count, len(data) * 8)
        self.__flags = data + '\xff' * (self._count * kind.size - len(data))

    def prime(self, i): return self[i]
    def __getitem__(self, key):
        try:
            byte, bit = self.__find(key)
            if ord(self.__flags[byte]) >> bit & 1: return True
        except ValueError: pass
        return False

    # Only really of any use for importing data from some alien source
    def __setitem__(self, key, flag):
        try: byte, bit = self.__find(key)
        except ValueError:
            if flag: raise ValueError("Cannot mark value true unless coprime to octet type",
                                      key, self.kind.modulus, flag)
        else:
            if flag: self.__flags[byte] = chr(1 << bit | ord(self.__flags[byte]))
            else: self.__flags[byte] = chr(ord(self.__flags[byte]) & ~(1 << bit))

    def __find(self, key, bad=regular.Regular):
        """Identify which bit describes a given key.

        Single argument, key, should be a natural.  If it's a slice (or Slice),
        we raise TypeError; otherwise, if it's not in .span we raise KeyError;
        these should not usually be caught in this class.  If it's in range but
        not in .kind we raise ValueError, which should generally be caught and
        handled by this class.  Otherwise, the (byte, bit) returned indicates
        that the given key is the natural associated with 1<<bit &
        ord(self.__flags[byte]).\n"""
        if isinstance(key, (slice, bad)):
            raise TypeError("Can't slice a mapping", key)

        if key not in self.span:
            raise KeyError("Out of range", key, self.span)

        q, r = divmod(key - self.span.start, self.kind.modulus)
        byte, bit = divmod(self.kind.index(r), 8)
        return q * self.kind.size + byte, bit

    def mark(self, p):
        """Marks relevant multiples of p as non-primes."""
        f, z, t = self.__flags, self.span.start, self.kind
        m, s = t.modulus, t.size
        for k in self.span.trim(slice(p*p, None, p)):
            q, r = divmod(k - z, m)
            try: byte, bit = divmod(t.index(r), 8)
            except ValueError: pass
            else:
                byte += q * s
                f[byte] = chr(ord(f[byte]) & ~(1 << bit))

class FlagSieve (FlagOctet, Sieve):
    """Sieve a range for primes.

    Use this to find all the primes in a range of the naturals, given all the
    primes whose squares aren't beyond the end of the range.  More usually,
    however, I'll want to also know the least proper factor of each non-prime;
    see FactorSieve, below.  Arguments are as for Octet except that count is
    required and comes before data.\n"""

    __upinit = FlagOctet.__init__
    __mixinit = Sieve.__init__
    def __init__(self, primes, kind, base, count, data=''):
        self.__upinit(kind, base, data, count)
        self.__mixinit(primes, 8 * len(data))

    __upget = FlagOctet.__getitem__
    def __getitem__(self, ind):
        val = self.__upget(ind) # raises various excpetions if suitable
        if not val or self.valid(ind): return val
        # else: we haven't yet found a factor, but there may be one
        raise LookupError('Too early to be sure', ind)

class FactorOctet (Octet):
    """A mapping object storing factor information.

    The keys supported are all the naturals in some range; each is mapped to its
    least proper factor; or, if it is a prime, to None.\n"""
    __upinit = Octet.__init__
    def __init__(self, kind, base, data=(), count=None):
        """Set up a multi-byte factor octet.

        For arguments, see Octet.__init__, with the optional data being here a
        sequence containing (an initial portion of) the data for the blocks
        described (default: empty); it'll be padded with None to the required
        length.  Caller (typically a derived class) is responsible for keeping
        track of which parts are padding and which are real data.\n"""

        self.__upinit(kind, base, count, len(data))
        self.__factors = list(data) + [ None ] * (self._count * kind.size * 8 - len(data))

    def prime(self, i): return self[i] is None
    def __getitem__(self, key):
        try: ind = self.__find(key)
        # don't bother to catch KeyError or TypeError
        except ValueError: # from self.kind.index
            if self.span.start == 0:
                if key in self.kind.octet.primes: return None
            return self.kind.factor(key % self.kind.modulus)
        return self.__factors[ind]

    # Only really of any use for importing data from some alien source
    def __setitem__(self, key, factor):
        try: ind = self.__find(key)
        except ValueError:
            if factor is None:
                raise ValueError("Cannot mark value prime unless coprime to octet type",
                                 key, self.kind.modulus)
            # else silently ignore marking non-prime as such.
        else:
            f = self.__factors
            if factor is None:
                assert f[ind] is None
            else:
                assert factor > 1
                if f[ind] is None or f[ind] > factor:
                    f[ind] = factor

    def __find(self, key, bad=regular.Regular):
        """Identify which entry in .__factors describes a given key.

        Single argument, key, should be a natural.  If it's a slice (or Slice),
        we raise TypeError; otherwise, if it's not in .span we raise KeyError;
        these should not usually be caught in this class.  If it's in range but
        not in .kind we raise ValueError, which should generally be caught and
        handled by this class.  Otherwise, returns i for which self.__factors[i]
        describes the given key.\n"""
        if isinstance(key, (slice, bad)):
            raise TypeError("Can't slice a mapping", key)

        if key not in self.span:
            raise KeyError("Out of range", key, self.span)

        q, r = divmod(key - self.span.start, self.kind.modulus)
        return q * self.kind.size + self.kind.index(r)

    def flag(self, seq=regular.Slice):
        """Re-express self's data in a FlagOctet.

        This discards knowledge of proper factors but yields a much more compact
        list of primes.\n"""

        text, ns = '', seq(0, len(self.kind), 8)
        for off in self.span.trim(slice(self.base, None, self.kind.modulus)):
            for n in ns:
                bit, byte = 1, 0
                for flag in map(lambda p, o=off, s=self: self[p+o] is None, self.kind[n:n+8]):
                    if flag: byte |= bit
                    bit <<= 1
                text += chr(byte)

        assert len(data) % self.kind.size == 0 == len(self.span) % self.kind.modulus
        assert len(data) // self.kind.size == len(self.span) // self.kind.modulus == self._count
        return FlagOctet(self.kind, self.span.start, text)

    def mark(self, p):
        """Marks relevant multiples of a prime p with index i as such."""
        f, z, t = self.__flags, self.span.start, self.kind
        m, s = t.modulus, t.size
        for k in self.span.trim(slice(p*p, None, p)):
            q, r = divmod(k - z, m)
            try: ind = q * s + t.index(r)
            except ValueError: pass
            else:
                if f[ind] is None: f[ind] = p

class FactorSieve (FactorOctet, Sieve):
    __upinit = FactorOctet.__init__
    __mixinit = Sieve.__init__
    def __init__(self, primes, kind, base, count, data=()):
        self.__upinit(kind, base, data, count)
        self.__mixinit(primes, len(data))

    __upget = FactorOctet.__getitem__
    def __getitem__(self, ind):
        val = self.__upget(ind) # raising suitable exception if needed
        if self.valid(ind) or val is not None: return val
        raise LookupError('Too early to be sure', ind)

del regular
