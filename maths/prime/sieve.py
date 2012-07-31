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
"""

def mark(p, slab, base, off=0):
    """Mark all multiples of p in slab.

    Required arguments:
      p -- a prime
      slab -- a modifiable sequence
      base -- a natural number

    Optional argument
      off -- length of previously-marked prefix of slab (default zero);
             see note, below.

    For each i in range(off, len(slab)), if slab[i] is None and i+base is a
    multiple of p, mark sets slab[i] to p.  Raises StopIteration if every
    non-prime in slab has least proper factor < p (i.e. it's pointless to try to
    mark with p or any higher prime).

    Note that, when off is non-zero, all entries in slab[:off] must be known to
    be the true lowest proper factors of the corresponding naturals (entry [i]
    corresponds to i+base); for this to be true, you must have scanned it with
    all primes up to sqrt(base+off).\n"""

    q, r = divmod(base+off, p)
    if q < p: q = p
    elif r: q += 1
    n, span = q * p - base, len(slab)
    if n >= span and q == p: raise StopIteration

    while n < span:
        if slab[n] is None: slab[n] = p
        n += p

def sieve(primes, start, span, head=()):
    """The Sieve of Eratosthenes.

    Required arguments:
      primes -- an iterable yielding all of the primes < start
      start -- lowest natural in the range to be sieved
      span -- number of naturals in the range to be sieved

    Optional argument:
      head -- tuple listing prior knowledge of least proper factors of an
              initial sub-range of these naturals; default is empty

    Returns a tuple whose [i] entry is None if start+i is prime, else the lowest
    proper factor of start+i; if you passed head, you should assert that the
    returned tuple's [:len(head)] agrees with head.

    Note that 0 and 1 are deemed to be their own least proper factors; see file
    comment.  A call with start == 0 should pass in head=(0,1) and can pass in
    any empty sequence as primes; chose a suitably large span and get all the
    primes up to it with sieve((), 0, span, (0,1)).\n"""

    off = len(head)
    slab = list(head) + [ None ] * (span - off)
    try:
        for p in primes: mark(p, slab, start, off)

        i = 0
        while i < span:
            if slab[i] is None:
                mark(i + start, slab, start, off)
            i += 1

    except StopIteration: pass
    return tuple(slab)

def nondices(seq, base=0):
    """Extracts the primes from an output of sieve (q.v.).

    Required argument, seq, is a sequence for which seq[i] is None iff base+i is
    prime; optional argument, base, defaults to 0.

    Returns a list of the primes >= base but < base + len(seq).\n"""

    ans = filter(lambda i, s=seq: s[i] is None, range(len(seq)))
    if b: ans = map(lambda i, b=base: i+b, ans)
    return tuple(ans)
