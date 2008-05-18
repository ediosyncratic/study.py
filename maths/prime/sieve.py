"""The Sieve of Eratosthenes.

When checking a range of integers to determine which of them are primes, it is
efficient to check them all at once, using a technique discovered in antiquity
and known as 'the sieve of Eratosthenes'.  The sieve works by, for each prime p
whose square is not beyond the end of the range, running through the range of
numbers marking each multiple of p as not a prime.  In fact it suffices to start
with p*p or the smallest multiple of p in the range (if this is larger), since
any earlier multiples of p shall be marked as multiples of some earlier prime.
When you have done this for all relevant primes, any numbers that remain
unmarked must be primes; the non-primes get caught in the sieve, letting only
the primes fall through.

One can, furthermore, use the sieve to discover the least proper factor of each
non-prime, as a bonus: to mark a multiple of p as non-prime, simply label it p
unless it already has a (smaller) label.

For these purposes, we treat 0 and 1 as their own lowest proper factors even
though they aren't proper factors by the usual definition (a factor of n is
proper if it is neither n nor 1).  We can justify this for 0 by redefining
proper factor in a way that doesn't affect anything but 0; if neither of
naturals a and b is 1, then each is a proper factor of a*b.  We cannot salvage
the case of 1, since it has no other factor than itself.

$Id: sieve.py,v 1.3 2008-05-18 15:11:46 eddy Exp $
"""
from study.snake.lazy import Lazy

class Sieve (Lazy):
    """The Sieve of Eratosthenes.

    Calls to .scan() provoke sieving, returning true if there's more to do.
    Records the start-point of the range it sieves as .start; and .factors is a
    tuple whose [i] entry is None if .start+i is prime, else the lowest proper
    factor of .start+i.  When .scan() returns false, .factors describes the full
    range.

    Lazy attribute .primes is the tuple of primes found by this sieve.
    Evaluating it involves calling .scan() repeatedly until it returns False.
    Contrast with .factors, which isn't lazily evaluated; the sieve may be in
    use by primes to discover what to return from its iterator's .next() when
    called by scan(), so primes can't afford to wait for the full range to have
    been scanned before it can read any entries.\n"""

    def __init__(self, primes, start, span, head=()):
        """Prepare to sieve a specified range of naturals.

        Required arguments:
          primes -- an iterable yielding all of the primes < start
          start -- lowest natural in the range to be sieved
          span -- number of naturals in the range to be sieved

        Optional argument:
          head -- tuple listing prior knowledge of least proper factors of an
                  initial sub-range of these naturals; default is empty

        Note that 0 and 1 are deemed to be their own least proper factors; see
        file comment.  An instance constructed with start == 0 should pass in
        head=(0,1); and its primes must know to consult the sieve's .factors to
        find which prime is next; chose a suitably large span and get all the
        primes up to it with sieve(primes, 0, span, (0,1)).\n"""

        self.__slab = list(head) + [ None ] * (span - len(head))
        self.base, self.factors, self.__src = start, tuple(head), iter(primes)

    def __mark(self, p):
        """Mark all multiples of p in slab.

        Required arguments:
          p -- a prime
          slab -- a modifiable sequence
          base -- a natural number

        Optional argument
          off -- length of previously-marked prefix of slab (default zero); see
                 note, below.

        For each i in range(off, len(slab)), if slab[i] is None and i+base is a
        multiple of p, mark sets slab[i] to p.  Raises StopIteration if every
        non-prime in slab has least proper factor < p (i.e. it's pointless to
        try to mark with p or any higher prime).

        Note that, when off is non-zero, all entries in slab[:off] must be known
        to be the true lowest proper factors of the corresponding naturals
        (entry [i] corresponds to i+base); for this to be true, you must have
        scanned it with all primes up to sqrt(base+off).\n"""

        q, r = divmod(self.base + len(self.factors), p)
        if q < p: q = p # Potentially pause here to yield each prime < p*p
        elif r: q += 1
        n, span = q * p - self.base, len(self.__slab)

        while n < span:
            if self.__slab[n] is None: self.__slab[n] = p
            n += p

        # Everything up to p's successor's square is now correctly marked.
        cut = (p+1)**2 # next prime after p is at least 1+p
        if cut > self.base:
            self.factors += tuple(self.__slab[len(self.factors) : cut - self.base])

    def _lazy_get_primes_(self, ig):
        while self.scan(): pass
        return tuple(map(lambda i, c=self.base: i+c,
                         filter(lambda i, f=self.factors: f[i] is None,
                                range(len(self.factors)))))

    def scan(self):
        if len(self.factors) < len(self.__slab):
            self.__mark(self.__src.next())
        return len(self.factors) < len(self.__slab)

class Fake (object):
    """A fake primes object to wrap a Sieve for testing.

    See __main__ stanza below for illustrative (if pointless) usage.\n"""

    def __init__(self, prior=()): self.__prior = iter(prior)
    def wrap(self, other): self.__wrap, self.__i = other, 0

    def __iter__(self): return self
    def next(self):
        try: return self.__prior.next()
        except AttributeError: pass # no prior, or previously consumed
        except StopIteration:
            del self.__prior # used up
            if not self.__wrap.base and None not in self.__wrap.factors:
                assert self.__wrap.factors[:2] == (0, 1), "0 and 1 become primes !"
                print 'boot-strap'
                self.__i = 3
                return 2

        i, f = self.__i, self.__wrap.factors
        while f[i] is not None: i += 1
        self.__i = 1 + i
        return i + self.__wrap.base

if __name__ == '__main__':
    # Not really intended for use, purely illustration; see Fake.
    fake = Fake()
    sieve = Sieve(fake, 0, 1024, (0,1))
    fake.wrap(sieve)
    primes = sieve.primes
    # NB: evaluate .factors after .primes, so it's been lazilly filled.
    got = len(sieve.factors)

    while True:
        fake = Fake(primes)
        sieve = Sieve(fake, got, 42*1024)
        fake.wrap(sieve)
        primes += sieve.primes
        got += len(sieve.factors)
