"""Package to handle the lazy infinite list of primes.

A natural number n is a factor (or divisor) of n*m for every integer m; it is,
furthermore, a proper factor unless either n or m is 1.  (Careful wording
ensures: 0 = n * 0 makes every natural > 1 a proper factor of 0; and -n = n *
-1 makes every negative < -1 have the matching positive as a proper factor.)
1 and -1 thus have no proper factors.  A prime is a least proper factor of a
natural; it has no proper factors of its own (else these would be proper
factors, less than it, of the natural of which it is least proper
factor).  One may alternatlively define: a natural p is prime iff, for every
list ({naturals}:f|n) with product(f) = p, {1, p} subsumes (:i&larr;i:f)
subsumes {p}.

If a natural p > 1 has no proper factors, then p*p is a natural and has p as a
proper factor, so p is a proper factor of it; that p is its least proper
factor follows since any other factor of it would necessarily be either n or m
for some naturals n, m with n < p < m and n*m = p*p, in which case n or its
least proper factor would necessarily be a factor of p, and the only such < p
is 1, so the factor of p*p other than p would not be proper.  Thus any natural
> 1 with no proper factors is necessarily prime; this is usually used as
definition.

One can infer that every positive integer may be expressed as a product of
primes, raised to various powers (with 1 as the empty product).  It can also
be shown (quite easilly) that, for any finite set of primes, there is a prime
not in the set (this amounts to saying there are infinitely many primes);
simply multiply the given primes together and add one to obtain a number which
is not a multiple of any of the given primes; when it is written as a product
of primes, the primes used are thus not in your set.

To determine whether a positive number is a prime, one must check to see if
any integer greater than 1 divides it.  Since only smaller numbers *can*
divide it, one only needs to check smaller numbers.  Since any divisor's
divisors shall divide it, we only need to check *primes* smaller than our
candidate prime. Furthermore, if n does have a factor then it is n = i * j for
some i and j; and min(i, j) is then sure to be no greater than the square root
of n; any prime factor of min(i, j) is thus necessarily no greater than n's
square root.  Thus we only actually have to check for divisors among the
primes whose squares do not exceed the candidate.  See sieve.py for code to
check a range of the naturals looking for primes.

When checking an individual number, to determine whether it is a prime, I
first iterate through the already-known primes with squares less than it,
checking each as a candidate factor.  If we find a factor, it's not prime.  If
all primes up to its square root are known and we find no factor, it's
prime.  Otherwise, for all numbers below some cut-off, we know whether or not
they're prime; our candidate exceeds the square of that cut-off.  We may know
some primes beyond that cut-off, so it's worth checking all of these (even
those whose squares exceed our candidate) before going onwards.  Lacking a
factor at this point, we begin (or resume) work on sieving the numbers between
the cut-off and some new cut-off whose square is slightly above our candidate,
albeit we might do this in several steps rather than trying to do it all at
once.  When we've finished sieving out multiples, in this range, of the prime
p we know all primes up tp p*p; as this extends our list of known primes, we
check our candidate against new entries; if we find a factor, we suspend the
sieve (leave it to be resumed next time we need to extend our range).

One could also resort to some clever number-theoretic tricks, which provide
computationally cheap tests which will, with probability very close to one
(closer than the probability of a determinist check completing without being
perturbed by cosmic rays or other hardware errors), spot a non-prime with much
less computational effort than would be needed to actually find a
divisor. These depend on raising randomly chosen values to carefully chosen
powers modulo the alleged prime (or possibly some related numbers, I'm not
familiar with the details) and checking the results against the value that
should result if the number were a prime; for example, for any prime p and
natural n which is not a multiple of p, n**(p-1) is 1 modulo p.  However,
these techniques are not (yet) used here, since my interest is principally in
factorising values, rather than determining whether they are primes.

Note (see Eureka 45, The Riemann Hypothesis, Mark Coleman) that the number of
primes <= x grows with x as x/ln(x) or, for better precision, integral(:
dt/ln(t) &larr;t, 2<=t<=x :{reals}).  Consequently, the number of primes
between t and t+N for N small compared to t is about N/ln(t).

For a mindless and simple iterator, study.maths.natural.eachprime() may be
sufficient for your needs.  The aim here (and in ../primes.py) is to remember
what we've worked out before and exploit that knowledge.  Various potential
improvements on existing study.maths.primes:

 * Turn it all into a separate sub-package, maths/prime/ !
    - abandon the misleading 'generalization' of lazyTuple; be entirely
      focussed on a lazy tuple of primes.  In particular, the grow() protocol
      was inappropriate and broken.

 * Store factorisation information.  The sieve can be used to record a prime
   factor of each non-prime; with care, the lowest.  This should limit growth
   of size of files recording this information.  The octet-based approach can
   still be used (saving a tuple instead of a byte string); we know everything
   not represented by the octets is a multiple of one of the (few) primes
   defining our octet-block.  Store factor information in separate cache files
   from prime-ness information; they're bigger, so may be worth breaking into
   shorter chunks.
   - cope with the possibility that the user may chose to use one directory to
     cache both kinds of data.

 * Iterator support:
   - Support interface in existing class.
   - By providing an iterator class.
   - Use this iterator class in the computation of new primes.

 * Bootstrap:
   - sieve.sieve((), 0, 18, (0, 1))
   - Save these first few primes as an overt tuple in top-level __init__.py
   - Create the octet.OctetType implied by those primes.
   - Make an initial octet.FactorSieve for some multiple of its modulus; start
     sieving and saving.

 * Generalized octet representation:
   - more compact representation of the data, both in memory and on disk
   - Think in terms of writing a .so module to implement crucial parts, when
     implementing octet format.
   - Want the generalized-octet class to be holding o(1MB) of infrastructure
     information, notably the list of numbers coprime to the first few primes,
     modulo the product of those primes;
     + the first 132096 primes take up > 1MB in memory;
     + the primes up to 19 yield a generalized octet requiring more than that
       many coprimes, so likely taking up > 1MB in memory;
     + so limit block size to the 11520 kB blocks, generated by the primes up
       to 17, each with a span of 510510.
   - Use customized compression to make the cached data even more compact:
     + prime byte-blocks can be bzipped, when that makes them smaller, and
       base-64 encoded.
     + factor tuples can be Huffman-encoded exploiting the predictable
       relative frequencies of the primes as least proper factors.
       o can use a 91-character representation; see crypt.Huffman.alphabet
       o for some N, for each i <= N, factors of naturals up to
         prime[91**i]**2 can be encoded by the relevant Huffman with
         prime[:91**i] as symbol-set
       o we don't have to care about the i < 2 cases, but the later ones
         rapidly get big, which is why we need to chose an N as limit
       o for blocks involving any factor in primes[91**N:], we can split the
         tuple into chunks encodable using the last Huffman, interspersed with
         actual (large) primes, saving the result as a tuple.
       o TODO: compare this vs simple enc64(bzip2(str(factor_tuple)))
     + add a suffix on attribute name, indicating how attribute has been
       encoded; candidate suffixes depend on attribute.

 * Cache improvements (see cache.py for details):
   - Ensure that all uses load from cache in preference to working out afresh
     (current .grow()-based approach doesn't).
   - Support use of sub-directories in the cache to provide a hierarchical
     cache; avoid over-large individual directories.
   - Turn primary object into a holder for objects describing sub-ranges; load
     and unload these as needed, to limit how much is held in memory;
     iterators need to remember their positions in terms of chunk
     identification and offset, since chunk may get unloaded between next()s
     and file hierarchy may get re-organized so the chunk's file-name has
     changed.
   - Support (at least initially) digesting an old-style cache dir's data so
     that we can save it into the new cache format.
     + although this lacks the proper factor information, it still lets us
       obtain that relatively cheaply
   - Iteration over cache objects is a tree-walking iteration, not iteration
     over the primes they describe.  A loaded cache file object owns a
     sub-list of the primes which can be iterated.
   - The Master (modifiable root) cache object
     + manages coversion of gap objects to sieve objects which, when
       completed, get turned into cache file objects
     + has a method by which we can give it more cache root objects,
       e.g. based on an URL (which won't fit nicely in our cache-path
       environment variable, due to its internal punctuators).

 * Miscellaneous
    - After exhausting factorise()'s trawl through known primes, try around
      the number's (approximate) square root (see study.maths.natural.sqrt and
      'Conjecture' in ../primes.py).
      + this only involves sieving for all primes up to its fourth root.

Special cases:

  * When is pow(2,n)+1 a prime ?  When n is in (1,2,4,8,16) and then no more,
    at least as far as 42.  Note that pow(2,32)+1 is 641*6700417

  * When is pow(2,n)-1 a prime ?  Only when n itself is prime; pow(2,a*b)-1
    necessarily has pow(2,a)-1 as a factor.  When n is in
    (2,3,5,7,13,17,19,31) and then no more, at least as far as 60.
    - So MAXINT is a prime on 32-bit machines, but not 64-bit machines.
    - Observe 31=pow(2,5)-1, 17=pow(2,4)+1, 7=pow(2,3)-1, 5=pow(2,2)+1, with
      pow(2,1)-1=1.
    - So when is pow(2, pow(2,i)+pow(-1,i))-1 a prime ?  For i=1 we get 1
      which we ignore, then for i in (2,3,4,5,6) we get primes.

  * For prime p, if p**2 + n is prime for some natural n then: n is not 0; for
    q in {2, 3}, (n+1)%q is not zero unless p == q; otherwise, n is even, n%6
    isn't 2 and, for n <= 210, I've found it easy to find moderately large
    members of {primes p: p**2+n is prime}, so I conjecture that this set is
    infinite.

  * primes.factorise(1535694353829581938477229739693926457830L) was too much
    for my 757 MHz Duron (from 2002) with half a gig of RAM to handle with the
    old system.  Being able to cope with it is a crucial test of the new
    system: see failure_log in this module.  Using the new code's oldCache,
    via primes.find_factor, the existing cache reduces the above to
    1748180621121264130683145827013L * 1058377 * 83 * 5 * 2, but cannot
    determine whether the first of these is prime.

This sub-package requires python 2.5 or later; it uses try...yield...finally.

See study.LICENSE for copyright and license information.
"""

failure_log = '''
Python 2.4.4 (#2, Apr 15 2008, 23:43:20) 
[GCC 4.1.2 20061115 (prerelease) (Debian 4.1.1-21)] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> from study.maths.primes import primes
>>> 2147483647L * 145295143558111L * 4921798434112837L + 1
1535694353829581938477229739693926457830L
>>> primes.factorise(_)

Process python stopped (signal)

Process python killed
'''

# from sequence import primes
# so you can: from study.maths.prime import primes
# See also: study.maths.prime.tool (once I've renamed ../primes.py and pruned it)
# Notably, iterating its Sisyphus is one way to expand your cache.
