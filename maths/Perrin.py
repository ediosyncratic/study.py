"""The Perrin iterator and its pseudo-primes.

For the theory, see
http://www.solipsys.co.uk/new/FindingPerrinPseudoPrimes_Part1.html
http://www.chaos.org.uk/~eddy/math/Perrin.html

See study.LICENSE for copyright and license information.
"""

class Perrin (tuple):
    """The Perrin iterator.

    Implements triples with the multiplicative structure that makes repeated
    multiplication by (0, 0, 1) be the Perrin iteration, [a,b,c] ->
    [b,c,a+b].  With the usual initial values, k[:3] = (3, 0, 2), this makes
    k[n] a multiple of n whenever n is prime; and the few composite n for which
    k[n] is a multiple of n are known as Perrin pseudo-primes.  The value of
    k[n] is available here as .entry(n) and that of k[n] % m as .entry(n,
    m).\n"""
    __upnew = tuple.__new__
    def __new__(cls, a, b, c): return cls.__upnew(cls, (a, b, c))

    @classmethod
    def _perrin_(cls, a, b, c):
        return cls(a, b, c)

    def __add__(self, other):
        return self._perrin_(*[x + y for x, y in zip(self, other)])

    def __mul__(self, other):
        """Multiplication modulo x**3 = x +1 of [0] +[2]*x +[1]*x*x

        This leads to (a +c*x +b*x*x)*(u +w*x +v*x*x) = a*u +(a*w +c*u)*x + (a*v
        +c*w +b*u)*x*x +(c*v +b*w)*(x +1) +b*v*x*(x +1) = a*u +b*w +c*v +(a*v
        +b*u +b*v +c*w)*x*x +(a*w +b*v +b*w +c*u +c*v)*x.\n"""
        a, b, c = self
        u, v, w = other
        # 9 multiplies, 8 adds, all integer arithmetic
        uv = u + v
        return self._perrin_(a * u + b * w + c * v,
                             a * v + b * uv + c * w,
                             a * w + b * (v + w) + c * uv)

    def __mod__(self, m):
        return self._perrin_(*[x % m for x in self])

    def __pow__(self, n, m=None):
        return self.successor(n - 1, m, self)

    def successor(self, n=1, mod=None, step=(0, 0, 1)):
        step = self._perrin_(*step)
        if n < 0:
            # Implement negative power as positive power of inverse:
            n, step = -n, step.__invert()

        if mod: step %= mod

        n, b = divmod(n, 2)
        if b:
            self *= step
            if mod: self %= mod

        while n > 0:
            step *= step
            if mod: step %= mod
            n, b = divmod(n, 2)
            if b:
                self *= step
                if mod: self %= mod

        return self

    from study.maths.natural import hcf
    from study.maths.ratio import Rational
    def __invert(self,
                 ratio=Rational, gcd=hcf,
                 # We can iteratively scale by basis to reduce score:
                 score=lambda t, R=Rational: R(abs(t[1]) + abs(t[2]), abs(t[0]) or 1),
                 # Each value is the product of the other two keys:
                 basis={(0, 0, 1): (-1, 1, 0),
                        (1, 0, 1): (0, 1, -1),
                        (-1, 0, 1): (0, 1, 1)}):
        """Search for an inverse of self.

        Only possible if self's entries' highest common factor is 1: see
        http://www.chaos.org.uk/~eddy/math/Perrin.html#Invert\n"""

        was = gcd(*self)
        if was != 1:
            raise ValueError('Invertible only if highest common factor is 1', self, was)

        if not isinstance(basis.keys()[0], Perrin): # Upgrade basis on first call:
            raw, P = basis.items(), self._perrin_
            basis.clear()
            basis.update((P(*k), P(*v)) for k, v in raw)

        try: return basis[self]
        except KeyError: pass # well, it was worth a try.

        keys, counts = basis.keys(), [0, 0, 0]
        was = score(self)
        while was > 0:
            ps = [self * x for x in keys]
            sums = [score(p) for p in ps]
            lo = min(sums)
            if lo < was:
                i = 0 if sums[0] == lo else 1 if sums[1] == lo else 2
                self, was = ps[i], lo
                counts[i] += 1
            else: # try taking a second step:
                ps = sum(([p * k for p in ps] for k in keys), [])
                sums = [score(p) for p in ps]
                lo = min(sums)
                if lo < was:
                    i = (j for j, s in enumerate(sums) if s == lo).next()
                    self, was = ps[i], lo
                    i, j = divmod(i, 3)
                    counts[i] += 1
                    counts[j] += 1
                else:
                    raise ValueError('Unable to refine towards inverse',
                                     self, ps, keys, counts)
            if all(counts):
                lo = min(counts)
                for i in range(3): counts[i] -= lo

        assert not all(counts)
        ps = (k ** c for k, c in zip(keys, counts) if c)
        try: inv = ps.next()
        except StopIteration: return self

        for p in ps: inv *= p
        if self[0] != 1: return self._perrin_(ratio(i, self[0]) for i in inv)
        return inv

    del Rational, hcf

    @classmethod
    def entry(cls, n, mod=None, start=(3, 0, 2), step=(0, 0, 1)):
        if n > 2: return cls._perrin_(*start).successor(n - 2, mod, step)[2]
        elif n < 0: return cls._perrin_(*start).successor(n, mod, step)[0]
        return start[n]

    # Support for .primal():

    class CycleCache (dict): # tool
        """Lazy cache dictionary for cycles modulo chosen primes.

        For any natural key, Perrin(3,0,2).successor(n, key) is a triple of
        naturals less than key, for each natural n; there are at most key**3
        distinct such triples; so some 0 < n <= key**3 must repeat a triple
        we've seen before.  Since the iterator can be wound backwards from where
        we saw it before to the start, doing the same to our repeat must in fact
        get us a 0 < n <= key**3 that repeats the initial (3,0,2) triple.  As
        our sequence of triples thus cycles, so does k[n] % key.

        This dictionary type auto-fills its instance[key] with the sequence of
        k[n] % key = Perrin(3, 0, 2).entry(n, key) values up to the first point
        after the start where the next three entries are (mod key) 3, 0 and 2
        (in that order).  Every natural n then has k[n] % key == instance[key][n
        % len(instance[key])].  If this is non-zero for any key that divides n,
        then Perrin.entry(n, n) is non-zero (and n is non-prime).

        For the rationale behind this, see:
        http://www.solipsys.co.uk/new/FindingPerrinPseudoPrimes_Part2.html
        """

        __upget = dict.__getitem__
        def __getitem__(self, key):
            # If we've been asked for this key before, we cached the answer:
            try: return self.__upget(key)
            except KeyError: 
                if not isinstance(key, (int, long)) or key < 2:
                    raise # i.e. that KeyError was the right answer !

            start, step = Perrin(3, 0, 2) % key, Perrin(1, 0, 1)
            # This step is (0,0,1)**3, so advances us three steps at a time.
            trip, run = start, True
            seq = list(trip) # seq[i] is k[i] mod key
            while run:
                # Get the next three entries in k:
                trip *= step
                trip %= key
                # Append them to seq:
                seq.extend(trip)
                # Check for cycling:
                for ind in (-5, -4, -3):
                    ind += len(seq)
                    if tuple(seq[ind:ind+3]) == start:
                        seq, run = seq[:ind], False
                        break

            # Perrin.entry(n, key) == seq[n % len(seq)] for every natural n
            self[key] = ans = tuple(seq)
            assert (Perrin(*ans[-3:]) * step) % key == start, (key, start, step, ans)
            return ans

    class Factors (bytearray):
        """Encodes divisibility by up to eight primes.

        Multiply together the primes in question to get how many bytes we'll be
        taking up for this; for just the first eight primes, this is 4,594,590
        (4.4 MiB) and it'll be vastly bigger (about 1 TiB) for the next eight,
        should you ever chose to go there.  (But this can cope with less than
        eight; if you just go to the next five, you'll only need 13.9 MiB.)

        Initialising for primes in ps, in each byte we set bit 1 << i precisely
        if ps[i] is a divisor of the byte's index into the array.  We can then
        overload indexing to reduce index mod our product of primes, fetch our
        byte at the resulting index and return a tuple of those primes for which
        the corresponding bit in the byte are set.  We can precompute a look-up
        table of all the tuples that can arise from distinct bytes and use the
        byte we found at our reduced index to select the right such tuple.\n"""

        __upinit = bytearray.__init__
        def __init__(self, *ps):
            if len(ps) > 8: # number of bits in a byte
                raise ValueError('Too many (more than 8) primes', ps)
            print "Initializing Perrin's factor table: may take a moment or twelve ..."

            # First set up our mapping of bytes to tuples of primes:
            self.__subsets = tuple(tuple(p for i, p in enumerate(ps)
                                         if (1 << i) & byte)
                                   for byte in xrange(1 << len(ps)))
            assert self.__subsets[-1] == ps

            # Initialise bytearray from an iterator over 0 <= int < 256:
            return self.__upinit(self.__sieve(self.__subsets))

        __upget = bytearray.__getitem__
        def __getitem__(self, ind):
            return self.__subsets[self.__upget(ind % len(self))]

        @staticmethod
        def __sieve(subs): # partial Eratosthenes
            """Iterate over divisibility bit-masks.

            Takes one argument, the tuple of sub-tuples of ps, whose [n] entry
            includes ps[i] precisely if bit 1 << i is set in n; the last entry
            in this is ps.  After n yields from this, the next has bit 1 << i
            set precisely if ps[i] divides n.\n"""

            ps = subs[-1]
            # Lookup for byte to represent tuple of primes whose ticker has hit 0:
            bits = dict((t, b) for b, t in enumerate(subs))
            yield bits[ps] # for 0

            # After n yields, (n + tick[i]) % ps[i] is 0, for each i.
            # So the period of each ticker is the matching entry in ps.
            tick = [p - 1 for p in ps]
            while any(tick): # until all are zero, after product(ps) yields
                i, ws = len(tick), []
                while i > 0:
                    i -= 1
                    if tick[i]: tick[i] -= 1
                    else:
                        ws.insert(0, ps[i])
                        tick[i] = ps[i] - 1

                yield bits[tuple(ws)]

    @staticmethod
    def __primal(n,
                 prods=[Factors],
                 cache=CycleCache()):
        """Pre-test easy factors of n for evidence that n is not primal.

        If any divisor p of n has .entry(n, p) non-zero, then p doesn't divide
        .entry(n) so nor does n and we can discard n right away.  We check this
        for the first few primes (those listed in the call to factor that
        initialises prods, which is a look-up table whose [n % len(prods)] entry
        lists the primes, of those listed, that divide n), using a cache to keep
        track of the cycles k falls into modulo each prime.\n"""
        try: ps = prods[1]
        except IndexError:
            ps = prods[0](2, 3, 5, 7, 9, 11, 13, 17)
            prods.append(ps)

        ps = ps[n]
        assert all(n % p == 0 for p in ps)
        for p in ps:
            pat = cache[p]
            if pat[n % len(pat)]: return False

        return True # we haven't ruled n out, but this really means "maybe"

    del Factors, CycleCache

    @classmethod
    def primal(cls, n):
        """True precisely if .entry(n) is a multiple of n."""
        return cls.__primal(n) and cls.entry(n, n) == 0

