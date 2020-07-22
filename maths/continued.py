"""Continued fractions.

Exports:
  real_continued(val) -- iterator over continued fraction approximation to val
  rationalize(x [, tol, depth]) -- try to approximate x = n / d for whole n, d
  Token, Cycle, Series -- integer iterators for use with Continued
  Continued -- class implementing continued fractions

See also:
http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
expounding the virtues of continued fractions.
For more on the theory of continued fractions, see Continued (below) and
http://en.wikipedia.org/wiki/Continued_fraction

See study.LICENSE for copyright and license information.
"""

def real_continued(val, whole=int):
    """Returns an iterator for the continued fraction for val.

    Required argument, val, is a real number - although it need not be
    represented by a python object of type real; it suffices that it plays
    nicely with divmod(val,1), 1./val and, when used as a boolean, is false
    exactly if it is 0.  Optional argument, whole, is a callable that takes the
    first return of divmod(val, 1), after possibly adding one to make this round
    to nearest instead of rounding down, and does any suitable coercion
    (default, int, turns an integer-valued float into the corresponding int)
    prior to yielding it.  Returned iterator yields a sequence n of such values,
    for which n[0] +1/(n[1] +1/(n[2] + ...)) is a progressively more refined
    approximation to val.  Aside from n[0], abs(n[i]) is always at least 2 (when
    val is actually a float and whole is int).

    The sequence terminates when the approximation is exact, if this ever
    arises.  Note, however, that (if val is a float) the vagaries of
    floating-point arithmetic may prevent an exact match from being noticed, or
    permit an inexact match to appear exact.  Callers may want to treat any
    sufficiently huge yield as meaning the previous yield was the end of the
    sequence.

    == On knowing when to stop ==

    A small uncertainty dx in x - p = 1/(q + y), with p, q integers having x-p
    and y between -1/2 and +1/2, implies an uncertainty in y of dy; with 1 = (x
    - p)*(y + q), we have 0 = dx*(y+q) +dy*(x-p) so dy = dx*(y+q)/(p-x) =
    -dx*(y+q)**2.  Going forward, if we know an upper bound u on abs(dx), we can
    infer u.(q.q +abs(q) +1/4) as an upper bound on abs(dy); if this ever gets
    above 1/2 there is little point continuing the sequence.  Working backwards,
    e.g. to work out when we can truncate while ensuring some upper bound u on
    the error in x, the upper bound we must impose on the possible error in y is
    u.(q.q -abs(q) +1/4); if this ever gets as high as 1/2, we might as well
    truncate.  See rationalize(), below, for application of this.\n"""

    if not val: yield 0
    while val:
        q, r = divmod(val, 1) # r > 0 even if val < 0
        # Round to nearest (favouring even on exact half):
        if r > .5 or (2 * r == 1 and q % 2):
            q, r = q + 1, r - 1

        yield whole(q)
        if r: val = 1. / r
        else: break

def rationalize(x, tol=1e-7, depth=5):
    """Find rational approximation to a real.

    Required parameter, x, is a real number; complex is not handled and there
    is no point doing this to integers.  Optional arguments are:
      tol -- error tolerance (default: 1e-7)
      depth -- depth of search (default: 5), see below.
    Returns a twople n, d of integers for which n = d * x, give or take errors
    of order tol, raising ValueError if unable to find suitable n and d.

    Result is sought by using continued fractions; that is, by first trying to
    approximate x as s[0] + 1./(s[1] + 1./(s[2] + ...)) for some sequence s of
    integers, then unwinding this expression to obtain n and d.  The search
    aborts if it needs more than depth entries in s to get within tol of x.\n"""

    s = real_continued(x)
    seq = [ s.next() ]
    for q in s:
        # See real_continued's uncertainty analysis for handling of tol:
        if abs(q) * tol > 1: break
        seq.append(q)
        if len(seq) > depth: raise ValueError('Hard to approximate', x, seq)
        tol *= (abs(q) - .5)**2

    # x == seq[0] + 1/(seq[1] + 1/(...))
    n, d = seq.pop(), 1
    while seq: n, d = d + n * seq.pop(), n
    if d < 0: return -n, -d
    return n, d

class Token (object):
    """Base-class for special tokens understood by Continued (q.v.).

    The constructor for Continued takes an iterator which should normally
    yield integer values.  However, it may also yield an instance of (some
    class derived from) this base-class, Token, to represent the rest of the
    values to be yielded.  The idea is that Continued may know how to do magic
    with the given Token; however, in case it doesn't, the Token should behave
    as an iterator, yielding the integers for which it stands.\n"""

    def __iter__(self): raise NotImplementedError('Derived classes should over-ride')

class Cycle (Token):
    """Token representing cyclic repetition of prior yields.

    A Cycle(*seq) object is recognized by Continued (q.v.) as a 'value' in a
    sequence of integers that means 'the rest of the values you want are just
    the entries in seq, repeated endlessly.'  The given sequence must be
    non-empty.  Such a sequence is commonly used in the representation of
    various algebraic numbers, particularly square roots of rationals.  The
    sequence of values to be yielded is exposed as property .values, a tuple.\n"""

    def __init__(self, first, *rest): self.__values = (first,) + rest
    def __iter__(self):
        i = 0
        while True:
            try: yield self.__values[i]
            except IndexError: i = 0
            else: i += 1

    @property
    def values(self): return self.__values
    def __repr__(self): return 'Cycle%s' % (self.__values,)
    __str__ = __repr__

class Series (Token):
    """Token representing an arithmetic succession.

    A Series(a, b) object is recognized by Continued (q.v.) as a 'value' in a
    sequence of integers that stands for a followed, successively, by a +n*b
    for n = 1, 2, ...  Such iterators tend to represent outputs of Bessel
    functions and other irrational values.  It exposes the values of a and b
    as properties .first and .step, respectively\n"""

    def __init__(self, first, step): self.__data = first, step
    def __iter__(self):
        n, s = self.__data
        while True:
            yield n
            n += s

    @property
    def first(self): return self.__data[0]
    @property
    def step(self): return self.__data[1]
    def __repr__(self): return 'Series(%s, %s)' % self.__data
    __str__ = __repr__

class Continued (object):
    """Continued fractions.

    A continued fraction is a sequence, n, of integers denoting the real
    number n[0] + 1/(n[1] + 1/(n[2] + 1/...)); this is rational precisely if n
    terminates, but can also represent assorted irrationals exactly.\n"""

    def __init__(self, ns):
        """Construct a continued fraction.

        Single parameter, ns, is an iterable which, when iterated, yields the
        sequence of integers encoding the continued fraction.  If it is in
        fact an iterator, nothing else should attempt to iterate it !  The
        constructed object shall record the values an iterator over it
        yields.  These values should normally be integers.  However, if it
        yields an object of class Token (q.v.), this is taken to be the last
        yield (any further values shall be ignored) and to stand for whatever
        sequence it would yield if iterated.

        The integers used to encode the continued fraction, aside from the
        first, should be non-zero (the code might cope with zeros; but ending
        on a zero is very impolite) and should preferrably avoid 1 and -1,
        too.\n"""

        if iter(ns) is ns: self.__ds = self.__IterStore(ns)
        else: self.__ds = self.__digest(ns)

    class __IterStore (list):
        __upinit = list.__init__
        def __init__(self, it):
            self.__upinit()
            self.__src = it

        @property
        def source(self): return self.__src

        __upget = list.__getitem__
        def __getitem__(self, key):
            try:
                while key >= len(self):
                    it = self.__src.next()
                    self.append(it)
                    if isinstance(it, Token): del self.__src # done
            except AttributeError: raise IndexError # no __src
            except StopIteration:
                del self.__src
                raise IndexError

            return self.__upget(key)

        def __iter__(self):
            i = 0
            try:
                while True:
                    yield self[i]
                    i += 1
            except IndexError: pass

    def __digest(self, ns):
        i, ns = 1, tuple(ns)
        # Eliminate 0 entries from ns[1:]:
        while i < len(ns):
            if isinstance(ns[i], Token):
                ns = ns[:1+i] # discard everything after the Token
                break
            elif ns[i]: i += 1
            else:
                # j +1/(i +1/(0 +1/(h +1/(g +...)))) = j +1/(i+h +1/(g +...))
                try: head, last, next, tail = ns[:i-1], ns[i-1], ns[i+1], ns[i+2:]
                except IndexError: ns = ns[:i-1] # j +1/(i+infinity) = j
                else:
                    if isinstance(next, Cycle):
                        rest = next.values
                        next = rest[0]
                        rest = rest + (next,)
                        tail = (Cycle(*rest),)
                    elif isinstance(next, Series):
                        next, step = next.first, next.step
                        tail = (Series(next + step, step),)
                    elif isinstance(next, Token):
                        i += 1
                        continue # endure the 0, handle the unrecognized Token

                    ns = head + (last + next,) + tail
                    i = max(1, i-1) # NB: last+next may be zero, too !

        if isinstance(ns[-1], Cycle):
            vs = ns[-1].values
            while len(ns) > len(vs) and ns[-1-len(vs):-1] == vs:
                ns = ns[:-1-len(vs)] + ns[-1:]
        elif isinstance(ns[-1], Series):
            f, s = ns[-1].first, ns[-1].step
            i = len(ns) -1
            while i > 0:
                if ns[i-1] != f - s: break
                i -= 1
                f -= s
            if i+1 < len(ns):
                ns = ns[:i] + (Series(f, s),)

        # NB: if ns is now empty, it denotes infinity (of both signs)
        # while i > 1: i -= 1 and weed out the ns[i] in (1, -1) ?
        return ns

    class Grinder (object):
        """Iterator to orchestrate combination of continued fractions.

        Represents a combination (e.g. product, sum, difference, ratio) of
        some (e.g. two) continued fractions as an iterator that generates the
        integers of the continued fraction for the combined value.

        See 'Item 101B (Gosper): Continued Fraction Arithmetic' in the hakmem
        document referenced in this module's header.  This class generalizes
        Gosper's analysis to any natural number of variables.\n"""

        def __iter__(self): return self
        def __init__(self, srcs, numerator, denominator):
            """Set up the iterator.

            Requires three arguments:
              srcs -- a tuple of iterators over the sequences representing the
                      numbers to combine as continued fractions;
              numerator -- a tuple of coefficients for the numerator and
              denominator -- a tuple of coefficients for the denominator.

            The length of each tuple of coefficients is 2**len(srcs).  The
            numbers to be combined are referred to as X[b], described by
            iterators x[b], for b in range(len(srcs)).  In each tuple of
            coefficients, the one at index i is the multiplier for a product
            of the X[b] with i&(1<<b) set; so, if cs is a tuple of
            coefficients, the actual numerator or denominator it represents
            is:

            P(X, cs)
              = cs[0] +cs[1]*X[0] +cs[2]*X[1] +cs[3]*X[0]*X[1] +cs[4]*X[2] +...
              = sum(reduce(lambda x, y: x * y, xs, c) for c, xs in
                    zip(cs,
                        [[X(b) for b in range(len(srcs)) if i & (1 << b)]
                         for i in range(1 << len(srcs))]))

            Note that the case len(srcs) is explicitly allowed; then self just
            represents numerator[0] / denominator[0].  A typical term in P(X,
            cs) can be understood as

            T(X, cs, bs) =
              reduce(lambda b, p, X=X: X[b] * p, bs, cs[sum(1 << b for b in bs)])

            for some sorted tuple bs of distinct (so the sum in cs[...]
            implements | on the bits) naturals < len(srcs); when srcs is
            empty, the only candidate for bs is (). For details of how these
            data are used, see next().\n"""

            assert len(numerator) == 1<<len(srcs) == len(denominator)
            self.__x = list(srcs)
            self.__n = list(numerator)
            self.__d = list(denominator)
            # .__p[i] = Period of Token in which srcs[i] terminated, if any:
            self.__p = [ None ] * len(srcs)

            # Now digest the first term of each iterator, to deal with all
            # entries that might be -1, 0 or 1, to spare next() having to think
            # about them:
            b = len(srcs)
            while b > 0:
                b -= 1
                self.__step(b)

        @classmethod
        def __show(G, it):
            try: src = it.source # __IterStore
            except AttributeError:
                if isinstance(it, Token): return it
            else:
                if isinstance(src, G): return src.status
            return True

        @property
        def status(self):
            return tuple(self.__show(x) for x in self.__x)

        @staticmethod
        def _pop(p, ns, ds):
            """Rearrange F(.__x, ns, ds) to take account of emitting an integer, p.

            Takes three parameters - integer p and lists ns and ds of
            coefficients for numerator and denominator respectively - and
            returns the result of rearranging the coefficients, as a twople of
            lists of coefficients. This base-class version rearranges the
            expression for F to represent 1/(F-p). Note, however, that Digits
            (q.v.) does a different re-arrangement.\n"""

            return ds, [n - p * d for n, d in zip(ns, ds)]

        def edges(i, bit): # tool used by other tools and __prune
            """Yields naturals < i in pairs, without and with the given bit.

            Both integers should be powers of two; the first is the length of
            an array of coefficients; it should be greater than the second,
            which is the bit 1<<b associated with a variable X[b].  Result
            iterates over pairs of indices into the array, in strictly
            decreasing order (of each member of the pair separately); each
            pair (i, j) yielded has i ^ j == bit and i == j | bit.\n"""

            while i > bit:
                i -= 1
                if i & bit:
                    yield i ^ bit, i

        from natural import hcf
        def __prune(self, gcd=hcf, pairs=edges):
            """Simplifies self, in so far as practical.

            If .__n and .__d are parallel, self can be reduced to a simple
            rational.  If any bit has all associated coefficients zero, we can
            eliminate the variable associated with that bit.\n"""

            ns, ds = self.__n, self.__d

            n, d = sum(abs(n) for n in ns), sum(abs(d) for d in ds)
            if all(n * e == d * i for i, e in zip(self.__n, self.__d)):
                # self just represents the rational n/d.
                i = gcd(n, d) or 1
                ns[:], ds[:] = [ n/i ], [ d/i ]
                del self.__x[:], self.__p[:]
                return

            # Which bits aren't set in any index with a non-zero coefficient ?
            zs = ~ reduce(lambda x, y: x | y,
                          (i for i in range(1, len(ns)) if not (ns[i] == 0 == ds[i])),
                          0)
            if zs:
                b, bit = len(self.__x), len(ns)
                assert bit == 1<<b
                while b > 0: # It's crucial to start with highest bit and work down.
                    b -= 1
                    bit >>= 1
                    if zs & bit:
                        del self.__x[b], self.__p[b]
                        for i, j in pairs(len(ns), bit):
                            assert ns[j] == 0 == ds[j]
                            del ns[j], ds[j]
                assert bit == 1

            # Eliminate any global common factor:
            i = gcd(*(ns + ds))
            assert i > 0
            if i > 1:
                ns[:] = [n / i for n in ns]
                ds[:] = [d / i for d in ds]

            # TODO: any special magic we can do when len(self.__x) == 1 ?
        del hcf

        def span(cs, bit, each=edges): # tool used by worst
            """Indicates how bad the edges associated with bit are.

            Required argument are a list of values of F at the corners of our
            bounding cube; and a bit encoding 1<<b for some X[b]; the edges on
            which only this X[b] varies are studied.  If any such edge sees a
            change of sign in F's denominator, ValueError is raised; this bit
            should be __step()ped.  Otherwise, a pair n, d of positives
            denoting n/d is returned, for which n/d is the biggest difference
            between F's values at the end-points of any of the edges
            studied.\n"""

            t, b = 0, 1
            for i, j in each(len(cs), bit):
                (n, d), (m, e) = cs[i], cs[j]
                if d * e <= 0: raise ValueError
                n, d = abs(e * n -m * d), d * e
                if n * b > t * d: t, b = n, d

            return t, b

        def stir(p, bit, ns, ds, pairs=edges): # tool used by __step, not method
            """Coefficient update for X[b] with 1<<b == bit

            Consider a pair i, j of indices into cs with j = i|bit and i =
            j&~bit; the term in P(X, cs) with cs[j] as coefficient has X[b] as
            a factor along with all the same X-factors as the cs[i] term,
            which doesn't have X[b] as a factor.  Aside from these shared
            factors, then, these two terms are cs[j]*X[b] +cs[i] and we
            replace X[b] by p+1/X[b] then multiply every term (both in
            numerator and in denominator, so that it cancels) by X[b].  This
            turns our pair of terms into cs[j]*(p*X[b] +1) +cs[i]*X[b] =
            (cs[j]*p +cs[i])*X[b] +cs[j], so we replace cs[j] and cs[i] with
            cs[j]*p +cs[i] and cs[j], respectively.  Every index into cs
            either has bit set or not, so shows up in exactly one such pair of
            indices.\n"""

            for i, j in pairs(len(ns), bit):
                ns[i], ns[j] = ns[j], ns[i] + p * ns[j]
                ds[i], ds[j] = ds[j], ds[i] + p * ds[j]

        def clear(bit, ns, ds, pairs=edges): # tool used by __step, not method
            """Coefficient adjustment when x[b] runs out, with 1<<b == bit.

            As for stir, consider i, j with j = i|bit, i = j&~bit; the terms
            in P(X, cs) with cs[i] and cs[j] as coefficient are, aside from a
            shared product of X-factors other than X[b], cs[j]*X[b]
            +cs[i].  Replacing X[b] simply with p, this becomes cs[j]*p
            +cs[i], which is exactly what we replaced cs[j] with when we
            supposed we should replace X[b] with p+1/X[b].  So we can correct
            our error by replacing what we now have as cs[j] and cs[i] with 0
            and the value we computed earlier for cs[j].\n"""

            for i, j in pairs(len(ns), bit):
                ns[i], ds[i] = ns[j], ds[j]
                del ns[j], ds[j]

        del edges

        def __step(self, b, fix=clear, mix=stir):
            """Advance .__x[b], if not yet exhausted.

            When we step x[b] we get some integer p = x[b].next() and x[b]
            (hence X[b]) changes; we can write our prior value of X[b] in
            terms of the new value X[b] represented by x[b] after iterating as
            p +1/X[b], which we can then substitute in everywhere we had X[b]
            in our formula for F; after a little rearrangement, see stir(),
            we'll have mixed the coefficients up with one another and with p
            but we get back an expression of the same form as before.

            When we try to step x[p] and get StopIteration, matters are a
            little more complex: this effectively says X[p] is infinite.  We
            thus implicitly rewind to when we last stepped that iterator and
            work out what our formula would have become if we'd substituted
            the final .next() value, without a +1/X[b] term, in place of our
            prior X or Y.  Fortunately, what would then have resulted can
            indeed be inferred from what we worked out before we knew we'd
            reached the end of our iteration; see clear().

            We also have to take account of the possibility that .next() may
            give us a Token; in that case, we need to record relevant
            information from this Token and let it take over as our iterator,
            supplying its first value as the value we should have had from
            .next(); it is presumed that every Token functions as a non-empty
            iterator.\n"""

            src, bit = self.__x[b], 1<<b
            if src is None: raise StopIteration
            try: p = src.next()
            except StopIteration:
                fix(bit, self.__n, self.__d)
                del self.__x[b], self.__p[b]
            else:
                while isinstance(p, Token):
                    if isinstance(p, Cycle):
                        self.__p[b] = len(p.values)
                    elif isinstance(p, Series) and p.step == 0:
                        self.__p[b] = 1
                    self.__x[b] = src = iter(p)
                    p = src.next()
                mix(p, bit, self.__n, self.__d)

        del clear, stir

        def worst(cs, wide=span): # tool used by __shrink
            bit, bad, bent, m, e = len(cs), [], [], 0, 1
            while bit > 1:
                bit >>= 1
                try: n, d = wide(cs, bit)
                except ValueError: bent.append(bit)
                else:
                    if n * e > m * n: bad, m, e = [ bit ], n, d
                    elif n * e == m * n: bad.append(bit)

            assert bent or bad
            if bent: return tuple(bent)
            return tuple(bad)

        def __shrink(self, cs, judge=worst):
            b = len(self.__x)
            for bit in judge(cs):
                while (1 << b) > bit: b -= 1
                self.__step(b)

        def next(self):
            """Compute next integer in continued fraction for our number.

            At any moment, self represents
                F(X, .__n, .__d) = P(.__n, X) / P(.__d, Y)
            and, if the range of possible values this could represent lies
            entirely between p-.5 and p+.5 for some integer p, we can return p
            after changing the coefficients around to make self represent
            1/(F-p); see ._pop(p).  Otherwise we can work out which X[b], by
            the uncertainty in its value, contributed most to the width of the
            range of values for Z; we then step the iterator associated with
            the culprit, by calling .__step(b).

            For purposes of working out error-bar estimates, this code assumes
            that, aside from the first terms (which the constructor steps
            through), no iterator yields any integers strictly between -2 and
            2.  This is equivalent to assuming that, for each b, 1/X[b] is
            always between -1/2 and +1/2.

            Since F is a ratio P(.__n, X)/P(.__d, X), we can divide both
            numerator and denominator by product(X) to get each into the form
            of a function with the same form as P, using the same
            coefficients, but with X replaced by the variables Y[b] = 1/X[b]
            for b < len(X) and Y[b] shows up as a factor in the [i]
            coefficient's term precisely if bit b of i is *not* set.  Since
            each Y[b] lies between -.5 and +.5, we can fairly readilly compute
            bounds on the values the result can take.

            If F's denominator is zero anywhere inside our unit cube (and F's
            numerator isn't also zero in all the same places - but __prune()
            deals with this possibility), then the range of values F may take
            is unbounded and, in particular, not restricted to any interval
            from p-.5 to p+.5 with p natural.\n"""

            while True:
                # First, see if we can simplify our expression:
                if len(self.__x): self.__prune()
                # If it's really simple, life's easy:
                if len(self.__x) == 0:
                    if self.__d[0] == 0:
                        # self is infinite, represented by the empty sequence
                        raise StopIteration

                    n, d = self.__n[0], self.__d[0]
                    if d < 0: n, d = -n, -d
                    qs = self._nice(n, d)
                    if len(qs) > 1: qs = [q for q in qs if q % 2 == 1]
                    self.__n, self.__d = self._pop(qs[0], self.__n, self.__d)
                    return qs[0]

                # Otherwise, we have to think a bit:
                cs = self.__corners()
                try: q = self.__consensus(cs, self._nice, self._match)
                except ValueError: self.__shrink(cs) # ... and try again ...
                else:
                    self.__n, self.__d = self._pop(q, self.__n, self.__d)
                    return q

        @staticmethod
        def _nice(n, d):
            """Which integers can legitimately represent n/d ?

            Takes two integers - n and d, with d positive - and returns a
            tuple of integers, each of which is a suitable approximation for
            n/d.  This base-class deems nearest integers suitable; if n/d is
            exactly half way between two integers, both are suitable.  Note
            that the preference for an even integer, given ambiguity, is
            handled later in __consensus()'s decision-making process, so
            should be ignored here.  Derived class Digits shows how to do this
            for rounding towards zero.\n"""

            assert d > 0
            q, r = divmod(n, d)
            if 2 * r > d: return ( q + 1, )
            elif 2 * r < d: return ( q, )
            else: return (q, q + 1)

        @staticmethod
        def _match(w, n, d):
            """Can w legitimately represent n/d ?

            Takes three integers - w, n and d with d positive - and returns
            true precisely if w is a legitimate representation of n/d.  This
            base-class deems nearest integers to be suitable.  Derived class
            Digits over-rides this to handle rounding towards zero.\n"""

            assert d > 0
            return 2 * abs(n - w * d) < d

        @staticmethod
        def __consensus(cs, nice, match):
            """See if corner values agree on a single integer."""

            n, d = cs[0]
            if any(e * d <= 0 for m, e in cs[1:]):
                raise ValueError, "Denominator's sign varies"

            if d > 0: j = 1
            else: j, n, d = -1, -n, -d
            ok = nice(n, d)

            i = len(cs),
            while i > 1 and ok:
                i -= 1
                n, d = cs[i]
                n, d = n * j, d * j
                ok = [q for q in ok if match(q, n, d)]

            if len(ok) > 1: # When we have a choice, prefer even
                assert any(q % 2 == 0 for q in ok), q
                ok = [q for q in ok if q % 2 == 0]
                # Should we sort what's left in ok ?
            elif len(ok) < 1:
                raise ValueError, "No consensus integer"

            return ok[0]

        def __corners(self):
            """Returns F's values at the corners of X's range of values.

            At each corner, each X[b] is either -2 or 2; each corner can be
            characterised by the subset of b with either sign; each such
            subset can be encoded as an integer with as many bits as there are
            variables in X.  The return is a tuple of twoples, (n, d), giving
            the values of numerator and denominator; its entry at index i is
            for the corner at which each X[b] is 2 if bit b of i is set, else
            -2.\n"""

            ns, ds, ans, i = self.__n, self.__d, [], 0
            assert len(ds) == len(ns) == 1 << len(self.__x)
            while i < len(ns):
                tn = td = 0 # accumulators
                j = len(ns)
                while j > 0:
                    j -= 1
                    # First, compute product({ X[b]: bit b of j is set}):
                    bit, xs = len(ns), 1
                    while bit > 1:
                        bit >>= 1
                        if bit & j:
                            xs *= 2
                            if bit & i == 0: xs = -xs

                    tn += xs * ns[j]
                    td += xs * ds[j]

                ans.append((tn, td))
                i += 1

            return tuple(ans)
        # </Grinder>

    @property
    def status(self, G=Grinder):
        n = len(self.__ds)
        if isinstance(self.__ds, self.__IterStore):
            src = self.__ds.source
            if isinstance(src, Token):
                return n, src
            elif isinstance(src, G):
                return n, src.status
            return n, True
        return n, False

    def ingest(val):
        try: return iter(val.__ds)
        except AttributeError: pass
        return real_continued(val)

    def __add__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, 1, 1, 0),
                               (1, 0, 0, 0)))
    __radd__ = __add__
    def __sub__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, -1, 1, 0),
                               (1, 0, 0, 0)))

    def __rsub__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, 1, -1, 0),
                               (1, 0, 0, 0)))

    def __mul__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, 0, 0, 1),
                               (1, 0, 0, 0)))
    __rmul__ = __mul__

    def __truediv__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, 0, 1, 0),
                               (0, 1, 0, 0)))

    def __rtruediv__(self, other, get=ingest, grind=Grinder):
        return Continued(grind((iter(self.__ds), get(other)),
                               (0, 1, 0, 0),
                               (0, 0, 1, 0)))

    class Digits (Grinder):
        """Iterator over a continued fraction's sequence of digits.

        Can take any base, or even an iterator; yields the whole-number part
        of multiplying the continued fraction successively by the base or by
        successive yields of the iterator.  Initial yield is thus the
        whole-number part of the continued fraction, which may itself need to
        be expressed as a sequence of digits in terms of your base.\n"""

        def endless(b):
            while True: yield b
        def __init__(self, ctd, base, repeat=endless):
            """Set up a digit-emitter.

            Takes two arguments:
              ctd -- iterator over the denominators of x = d[0] +1/(d[1] +1/(d[2] + ...))
              base -- number base, or iterator over successive multipliers

            First yield is the non-fractional part of x; each subsequent yield
            is the non-fractional part of multiplying the fractional part,
            left by the previous yield, by base or its .next(), if it's an
            iterator.\n"""

            try: base + 1
            except TypeError:
                if base is not iter(base): # iter() may raise TypeError, too.
                    raise TypeError("If it's not numeric, it should be an iterator", base)
                self.__src = base
            else: self.__src = repeat(base)

            try: self.__grindit
            except AttributeError:
                # Digits' body can't reference Grinder to set this ...
                D = self.__class__
                D.__grindit = D.__bases__[0].__init__

            self.__grindit((ctd,), (0, 1), (1, 0))

        del endless

        def _pop(self, p, ns, ds):
            """Rearranges coefficients on digit p being emitted.

            Unlike Grinder, we here want to change F(.__x, ns, ds) to
            represent base * (F - p), with base being our number base or other
            multiplier, when using an iterator instead of a
            base.  Furthermore, if F is 0, we simply want to end the digit
            iteration.\n"""

            if not any(ns):
                assert p == 0
                raise StopIteration

            s = self.__src.next()
            return [s * (n - p * d) for n, d in zip(ns, ds)], ds

        @staticmethod
        def _nice(n, d):
            """Round n/d towards zero.

            See Grinder._nice for details.\n"""
            if d * n < 0: d, n = -d, -n
            q, r = divmod(n, d)
            return ( q, )

        @staticmethod
        def _match(w, n, d):
            """Does n/d rounded towards zero give w ?

            See Grinder._match for details.\n"""
            if d < 0: n, d = -n, -d
            if w > 0: return n <= w * d < n + d
            elif w < 0: return n - d < w * d <= n
            else: return -d < n < d

    def digits(self, base, D=Digits):
        """Emit self's digits to a given base.

        Takes one argument, base: this should either be the number base to be
        used or an iterator whose yields are successive multipliers for our
        number.  Returns an iterator yielding: first, the whole-number part of
        self; then, after each yield, the whole number part that results when
        the fractional part left over by the previous yield is multiplied by
        the base or its .next(), if it's an iterator.\n"""

        return D(iter(self.__ds), base)

    # any more ?
    del Grinder, Digits

    def __cmp__(self, other, get=ingest):
        i, ns = 0, self.__ds
        try:
            for y in get(other):
                y = cmp(ns[i], y)
                if y:
                    # allegedly ...
                    if i % 2: return -y
                    return y
                i += 1

            if i == len(ns): return 0

        except IndexError: # no such ns[i]
            raise NotImplementedError
        else:
            raise NotImplementedError

    del ingest
