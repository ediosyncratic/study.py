"""Package to handle lazy infinite list of primes.

Various potential improvements on existing ../primes.py:

 * Turn it all into a separate sub-package, maths/prime/ !

 * Iterator support:
   - Support interface in existing class.
   - By providing an iterator class.
   - Use this iterator class in the computation of new primes.

 * Cache improvements:
   - Ensure that all uses load from cache in preference to working out afresh
     (current .grow()-based approach doesn't).
   - Use hex in file-names, rather than base ten.
   - Support use of sub-directories in the cache to provide a hierarchical cache
     (avoid over-large individual directories).
   - Add a variable second letter to file-name prefix; change each time we add a
     digit to start index (so ls gets order right); allow to wrap round a-z
     since we should be in a separate sub-dir by then !
   - Change format to sequence of [ 1, 7, 11, 13, 17, 19, 23, 29 ] octets to
     limit growth in file size (change init to [2, 3, 5], to go with that);
     this'll make a bigger block-size practical.
   - Think in terms of writing a .so module to implement crucial parts, when
     implementing octet format.
   - Retain *one* old-style cache file for an initial chunk of (at least 3)
     primes, plus sparse (which has no place in octet format).
   - Turn primary object into a holder for objects describing sub-ranges; load
     and unload these as needed, to limit how much is held in memory; iterators
     need to remember their positions in terms of chunk index and offset, since
     chunk may get unloaded between next()s.
   - Support for list of read-only caches from which to borrow data; default
     from $STUDY_PRIME_PATH else empty.
   - Change default cache dir; $STUDY_PRIME_DIR else ~/.study/primes
   - Support (at least initially) an 'old-style cache dir' so we can digest old
     data into new cache format; let it keep the old default.

 * Miscellaneous
   - After exhausting factorise()'s trawl through known primes, try around the
     number's (approximate) square root (see 'Conjecture').

Special cases:

  * When is pow(2,n)+1 a prime ?  When n is in (1,2,4,8,16) and then no more, at
    least as far as 42.  Note that pow(2,32)+1 is 641*6700417

  * When is pow(2,n)-1 a prime ?  Never with n even; and n=1 gives 1.  When n is
    in (3,5,7,13,17,19,31) and then no more, at least as far as 60.  Observe
    31=pow(2,5)-1, 17=pow(2,4)+1, 7=pow(2,3)-1, 5=pow(2,2)+1, with pow(2,1)-1=1.

    - So when is pow(2, pow(2,i)+pow(-1,i))-1 a prime ?  For i=1 we get 1 which
      we ignore, then for i in (2,3,4,5,6) we get primes.

$Id: __init__.py,v 1.2 2007-05-06 01:00:18 eddy Exp $
"""
