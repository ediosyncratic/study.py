"""The Sieve of Eratosthenes.

$Id: sieve.py,v 1.1 2008-05-18 10:09:48 eddy Exp $
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

# The boot-strap call should be sieve((), 0, 0x80000, (0, 1))
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
    returned tuple's [:len(head)] agrees with head.\n"""

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
