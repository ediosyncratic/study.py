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
the case of 1, since it has no other factor than itself, so it's probably
simpler just to arbitrarily declare 0 and 1 special, rather than redefining
'proper factor' !

$Id: sieve.py,v 1.4 2008-05-18 19:45:57 eddy Exp $
"""
from study.snake.lazy import Lazy

class Sieve (Lazy):
    """The Sieve of Eratosthenes.

    Iterating over an instance provokes sieving, yielding each prime found in
    the range sieved, until none remain.

    Public attributes:
      base -- start-point of the range being sieved
      factors -- tuple whose [i] entry is None if .base+i is a prime, else the
                 lowest proper factor of .base+i.
      primes -- tuple of primes known to be in the range

    Only one iterator over an instance should ever be used; until it has
    completed, .factors and .primes describe only the portion of the range for
    which answers are known.\n"""

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
        file comment.  An instance constructed with start == 0 should pass in at
        least head=(0,1); and its primes must know to consult the sieve's
        .primes to find which prime is next; chose a suitably large span and get
        all the primes up to it with sieve(primes, 0, span, (0,1)).

        When head is non-empty, all entries in it must be known to be the true
        lowest proper factors (with the above oddity for 0 and 1) of the
        corresponding naturals (head[i] corresponds to i+start); for this to be
        true, you must have sieved it with all primes <= sqrt(start+len(head)).\n"""

        self.__slab = list(head) + [ None ] * (span - len(head))
        self.base, self.factors, self.__src = start, tuple(head), iter(primes)

    def __iter__(self):
        for q in self.primes: yield q
        stop = len(self.__slab)
        while len(self.factors) < stop:
            p = self.__src.next()
            self.__mark(p)
            for q in self.__grow(1+p): yield q

        raise StopIteration

    def primes(base, fs):
        return tuple(map(lambda i, c=base: i+c,
                         filter(lambda i, f=fs: f[i] is None,
                                range(len(fs)))))

    def _lazy_get_primes_(self, ig, extract=primes):
        return extract(self.base, self.factors)

    def __grow(self, p, extract=primes):
        """Update now that everything up to p**2 is known.

        Single input, p, is a number for which we know that .__slab is complete
        for all entries corresponding to numbers less than the square of p.
        Updates .factors to include these entries and .primes to include any
        primes they imply.  Returns a tuple of the primes thus added.

        Superficially, it would seem best to call this with the next prime after
        the one most recently passed to .__mark(); that would advance our
        knowledge of factors and primes as far as possible as soon as possible.
        However, at the very start of our first Sieve, we need to register what
        we learned, by __mark()ing 2, before we can ask for our next prime,
        since the iterator for that has to look at our .primes (or .factors) to
        work that out.  That means we can't yet know what the next prime is.  We
        do, however, know that it's at least one more than the prime we just
        __mark()ed - and this is entirely sufficient since, in fact, the worst
        it does is delay registration of things we've learned but don't yet
        need.\n"""

        cut, was = p**2, len(self.factors)
        if cut > was + self.base:
            more = tuple(self.__slab[was:cut-self.base])
            self.factors += more
            more = extract(was + self.base, more)
            self.primes += more
            assert min(cut - self.base, len(self.__slab)) == len(self.factors)
        else: more = ()
        return more

    del primes

    def __mark(self, p):
        """Mark all multiples of p in range.

        Required argument, p, is a prime; for len(.factors) <= i < len(.__slab),
        if i+.base is a multiple of p and .__slab[i] is None, set .__slab[i] to
        p (i.e. mark i+.base as having p as its lowest proper factor).

        For each i in range(len(.factors), len(.__slab)), if .__slab[i] is None
        and i+base is a multiple of p, mark sets slab[i] to p.  Raises
        StopIteration if every non-prime in slab has least proper factor < p
        (i.e. it's pointless to try to mark with p or any higher prime).\n"""

        q, r = divmod(self.base + len(self.factors), p)
        if q < p: q = p
        elif r: q += 1
        n, span = q * p - self.base, len(self.__slab)

        while n < span:
            if self.__slab[n] is None: self.__slab[n] = p
            n += p

class Fake (object):
    """A fake primes object to wrap a Sieve for testing.

    See __main__ stanza below for illustrative (if pointless) usage.\n"""

    def __init__(self, prior=()): self.__prior = prior
    def wrap(self, other): self.__wrap, self.__i = other, 0

    def __iter__(self):
        for it in self.__prior: yield it
        try: yield self.__wrap.primes[0]
        except IndexError:
            if not self.__wrap.base:
                print 'boot-strap'
                yield 2
                assert self.__wrap.primes[0] == 2

        i = 1
        try:
            while True:
                yield self.__wrap.primes[i]
                i += 1
        except IndexError:
            raise StopIteration

if __name__ == '__main__':
    # Not really intended for use, purely illustration; see Fake.
    fake = Fake()
    sieve = Sieve(fake, 0, 1024, (0,1))
    fake.wrap(sieve)
    map(None, sieve)
    got = len(sieve.factors)

    while True:
        fake = Fake(fake)
        sieve = Sieve(fake, got, 42*1024)
        fake.wrap(sieve)
        map(None, sieve)
        got += len(sieve.factors)
