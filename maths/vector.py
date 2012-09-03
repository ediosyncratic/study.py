"""Vectors (and tensors).

Exports one class, Vector, also exposed under the name Tensor.
See its documentation for more.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop
from study.snake.sequence import Tuple
from study.snake.decorate import postcompose

class Vector (Tuple):
    """Tuple type supporting entry-by-entry arithmetic.

    Built-in tuple and list types support addition by concatenation,
    producing a longer sequence, and multiplication (only by whole numbers) as
    repeated addition.  In contrast, Vector supports
     * addition and subtraction as operations on corresponding entries
       (i.e. (x+y)[i] = x[i]+y[i], etc.), and
     * multiplication as acting on each entry, i.e. (x*k)[i] = x[i]*k, with
       (only) scalar multiplication defined commutative (so, for scalar k, k*x
       is just x*k); this leads to a (non-commutative) tensorial
       multiplication when k, as well as x, is a Vector.

    In general, division is not well-defined (even where it can be defined, it
    is not always unambiguous).  I may, some day, add support for at least
    some of the cases where it is well-defined; but that hasn't happened yet.

    The product of two Vector objects is technically a Vector whose entries
    are Vector-valued; such a vector is termed a tensor.  The depth of nesting
    of Vector within Vector is known as the 'rank' of the tensor; a plain
    Vector, with numeric entries, has rank 1; multiplying two tensors of ranks
    n and m yields a tensor of rank n+m.

    For a sequence to be a valid input to the constructor, either its entries
    should all be numeric (and behave reasonably like reals) or its entries
    should all be Vector instances, with equal .dimension attributes.  See
    pseudo-constructor fromSeq() for a more liberal approach.

    Pseudo-constructors:
      fromSeq(seq [, dim]) -- recursively traverse into seq's entries
      diagonal(seq) -- specify the diagonal entries of a square

    Lazy properties:
      rank -- depth of nesting of Vector; see above.
      dimension -- tuple of ranges of successive viable indices

    Methods:
      symmetrise([ranks]) -- average over permutations of given ranks (or all)
      antisymmetrise([ranks]) -- as symmetrise, scaling by signature
      transpose([n=1]) -- swap rank 0 with rank n
      tau(pattern) -- generalised permutate-and-trace
      permutrace(shuffle, *pairs) -- alternative permute-and-trace
      dot(other, [n=1, out=True]) -- contract with other on self's right
      rdot(other, [n=1, out=True]) -- contract with other on self's left

    Arithmetic operations allow the other operand to be vectors - for addition
    and subtraction, they should have the same .dimension - or sequences that
    are acceptable to the constructor.  Its indexing accepts a tuple, applying
    each entry successively.\n"""

    @classmethod
    def __vector__(cls, seq):
        """Pseudo-constructor by which derived classes can mimic base.

        By default, various methods of this class create objects of the same
        class as the instance whose method was called; this is achieved simply
        by passing this method the sequence of (or iterator over) values to be
        used as entries in the new object, exactly as for Tuple.  If a derived
        class has a new/constructor with a different signature, it should
        over-ride this method to do something sensible; it is always called as
        vec.__vector__(seq) with vec either a class based on Vector or an
        instance of such a class.\n"""
        return cls(seq)

    @classmethod
    def xerox(cls, dims, leaf=0.):
        """A tensor duplicating a given leaf through specified dimensions.

        Required argument, dims, is a sequence of (or iterator over)
        dimensions.  Optional second argument, leaf, is either a number
        (default is zero) or a Tensor.

        A pure number tacitly has an empty tuple as its .dimension; the
        .dimension of the return value shall be dims (as a tuple) plus that of
        leaf.  For every valid index tuple of a tensor with dims as its
        .dimension, the result's value for that tuple shall be leaf.

        Note that each Tensor created by this method uses the same object for
        all of its entries - a Vector of dims[-1] references to leaf is
        created; that Vector is then referenced dim[-2] times by the Tensor of
        next higher rank; and so on.  For any valid index tuples, s and t, of
        equal length no greater than that of dims, into r = cls.xerox(dims,
        leaf), we can assert r[s] is r[t].  Thus only len(dims) Tensor objects
        are created (albeit sum(dims) references are created); this is
        significantly more compact than what cls.xerox(dims, 1) * leaf will
        give you.

        Example: cls.xerox(dims) is the zero tensor whose .dimension is
        dims.\n"""

        dims = iter(dims) # no-op if dims is already an iterator
        try: n = dims.next()
        except StopIteration: return leaf
        return cls.__vector__((cls.xerox(dims, leaf),) * n)

    @classmethod
    def delta(cls, dim, *ns):
        """Vector selecting given co-ordinates.

        First argument, dim, is the dimension of the resulting vector; all
        subsequent arguments are indices into it.  The returned vector has
        value 1 at each of these (ignoring any >= dim) and all other entries
        zero.\n"""
        return cls.__vector__([1 if i in ns else 0 for i in range(dim)])

    @classmethod
    def diagonal(cls, seq):
        """Returns a tensor whose [i][i] is seq[i].

        All other entries are zero.  Required argument, seq, gives the values
        on the diagonal.  These should normally be numeric, but vectors or
        tensors are accommodated.  For the usual 'identity' matrix of
        dimension n, Vector.diagonal([1]*n) will do fine.\n"""

        row, n = [], len(seq)
        for i, v in enumerate(seq):
            row.append(cls.delta(n, i) * v)

        return cls.__vector__(row)

    @classmethod
    def fromSeq(cls, seq, dim=None):
        """Construct a Vector from a loosely suitable sequence.

        Required first argument, seq, is a sequence whose entries either are
        all numbers or are all suitable as first arguments to this method.  In
        the latter case, each is indeed passed, recursively, to this method;
        the sequence of returns is used in place of seq - save that, if any
        two of them have different .dimension, a ValueError is raised.

        Optional second argument, dim, may be None (its default), in which
        case it is ignored.  Otherwise, it should be a sequence of expected
        dimensions; if the return from this function would not have this as
        its .dimension property, a ValueError shall be raised.

        Returns a Vector object that's entry-by-entry equal to seq, where such
        equality is defined by: two sequences r, s are entry-by-entry equal
        precisely if, for every i for which either r[i] or s[i] doesn't raise
        an IndexError:
         * neither r[i] nor s[i] raises any exception when evaluated
         * if either r[i] or s[i] is a sequence, so is the other and the two
           are entry-by-entry equal;
         * otherwise, both r[i] and s[i] are numbers and r[i] == s[i].

       This method exists so that repr() can avoid repeating this class's name
       a whole lot !  It is, hopefully, also useful generally.\n"""

        assert None not in seq, 'Vector entries should be numeric'
        if isinstance(seq, Vector):
            if dim is not None and seq.dimension != dim:
                raise ValueError('Mismatched dimensions', dim, seq)
        else:
            seq = tuple(seq) # in case it's an iterator
            if dim is None: tail = None
            elif len(seq) != dim[0]:
                raise ValueError('Mismatched dimension', dim[0], seq)
            else: tail = dim[1:]

            try: map(iter, seq)
            except (TypeError, AttributeError): pass
            else: seq = cls.__each(seq, tail)

        return cls(seq)

    @classmethod
    def __each(cls, seq, dim=None):
        ans = map(lambda s, v=cls.fromSeq, d=dim: v(s, d), seq)
        if dim is None:
            ds = map(lambda x: x.dimension, ans)
            if filter(lambda e, d=ds[0]: e != d, ds[1:]):
                raise ValueError('Inconsistent dimensions', seq, ds)
        return ans

    from math import pi, sin, cos, sinh, cosh, atanh
    @classmethod
    def circulate(cls, angle, unit=2*pi, s=sin, c=cos):
        """Rotation through the given angle (of a circle).

        Required argument, angle, is the angle through which to rotate; by
        default, the rotation is anti-clockwise and the angle is measured in
        turns.

        Optional second argument, unit, defaults to 2*pi; it is the unit of
        angle, measured in radians; pass unit=1 for angles in radians.  To
        pass an angle in degrees, simply divide it by 360 and pass it as
        angle, using the default unit; this is easier than computing pi/180 as
        unit !

        Returns a tensor of .dimension (2, 2) representing the specified
        rotation.  Use .embed() for higher dimensions.\n"""
        angle *= unit
        s, c = s(angle), c(angle)
        return cls.fromSeq(((c, s), (-s, c)), (2, 2))

    @classmethod
    def hyperbolate(cls, speed, mode=atanh, s=sinh, c=cosh):
        """Hyperbolic 'rotation'

        Required argument, speed, controls how far the 'rotation' deviates
        from the identity; by default, its meaning corresponds to the fraction
        of the speed of light at which an observer is moving, whose frame of
        reference is 'rotated' to the degree in question.

        Optional second argument is a callable; it defaults to math.atanh and
        is used to convert speed to an appropriate input to cosh and
        sinh.  Passing math.log in its place will read speed as the Doppler
        shift of the observer's frame, instead of its velocity.  If None is
        passed instead, the identity is implicitly used; this is suitable for
        use with the .Lorentz attribute of a Quantity whose value is a speed.

        Returns a tensor of .dimension (2, 2) representing the Lorentz
        transformation corresponding to the specified relative motion.  Use
        .embed() for higher dimensions.\n"""
        if mode is not None: a = mode(speed)
        s, c = s(a), c(a)
        return cls.fromSeq(((c, s), (s, c)), (2, 2))
    del pi, sin, cos, sinh, cosh, atanh

    def __repr__(self):
        if self.rank > 1: nom = 'Tensor.fromSeq'
        else: nom = 'Vector'
        return '%s(%s)' % (nom, str(self))

    def __str__(self):
        if len(self) == 1: return '(%s,)' % str(self[0])
        return '(' + ', '.join(self.map(str)) + ')'

    def __nonzero__(self):
        for it in self:
            if it: return True
        return False

    def __add__(self, other, add=lambda x, y: x+y):
        assert len(other) == len(self)
        return self.__vector__(map(add, self, other))

    __radd__ = __add__

    def __sub__(self, other, sub=lambda x, y: x-y):
        assert len(other) == len(self)
        return self.__vector__(map(sub, self, other))

    def __rsub__(self, other, sub=lambda x, y: x-y):
        assert len(other) == len(self)
        return self.__vector__(map(sub, other, self))

    def __mul__(self, other):
        try: other[:]
        except TypeError: pass
        else:
            if not isinstance(other, Vector):
                other = self.__vector__(other)

        return self.__mul(other)

    def __mul(self, other):
        return self.__vector__(map(lambda s, o=other: s * o, self))

    def __rmul__(self, other):
        try: other[:]
        except TypeError: return self.__mul(other)
        return self.__vector__(map(lambda o, s=self: o * s, other))

    def __pow__(self, other, base=None):
        assert base is None
        # TODO: can this be optimised ?
        result = 1
        while other > 0:
            other, r = divmod(other, 2)
            if r: result = self * result
            self = self * self

        return result

    __upget = tuple.__getitem__
    def __getitem__(self, key):
        """Extend indexing to accept list of indices at successive ranks.

        Single argument, key, may be a simple integer, in which case it is
        used as index into self, as a tuple, in the usual way.  Otherwise, key
        should be a sequence of indices, of length at most self.rank; the
        result is defined inductively by: self[()] = self, self[(h, *t)] =
        self[h][t] or, more intuitively if marginally less rigorously,
        specified as
          self[(i, j, ..., n)] = self[i][j][...][n].
        Such dereferencing is apt to lead to IndexError unless 0 <= key[i] <
        self.dimension[i] for 0 <= i < len(key).\n"""

        try: ks = key.to_slice() # See study.snake.regular
        except AttributeError: pass
        else: key = ks
        if isinstance(key, slice):
            return self.__vector__(self.__upget(key))

        try: key[:]
        except TypeError:
            return self.__upget(key)

        assert not None in key
        for k in key: self = self[k]
        return self

    @lazyprop
    def rank(self, cls=None):
        """The rank of self, as a tensor.

        If the entries in self are numbers, self's rank is 1; otherwise, each
        entry in self is a Vector and all have the same rank; self's rank is
        one greater than that of its entries.\n"""
        if isinstance(self[0], Vector):
            assert all(isinstance(x, Vector) for x in self)
            r = self[0].rank
            assert all(x.rank == r for x in self)
            return 1 + r
        return 1

    @lazyprop
    def dimension(self, cls=None):
        """The sequence of dimensions at different ranks.

        For a tensor, len(self[i]) is the same for all 0 <= i < len(self); for
        each such i, len(self[i][j]) is the same for all 0 <= j <
        len(self[i]); and so on.  The tuple self.dimension, of length
        self.rank, collects up these lengths, starting with dimension[0] =
        len(self).

        Given the extended indexing provided by __getitem__, in so far as
        dimension[:i] is determined and i <= self.rank, for any sequence s of
        length i having, for 0 <= j < i, 0 <= s[j] < dimension[j], we define
        dimension[i] to be len(self[s]) and assert that its value is
        independent of choice of s, subject to given constraints.\n"""
        if isinstance(self[0], Vector):
            assert all(isinstance(x, Vector) for x in self)
            tail = self[0]. dimension
            assert all(x.dimension == tail for x in self)
            return (len(self),) + tail

        return (len(self),)

    def symmetrise(self, ranks=None):
        """Return symmetric part of a tensor.

        Single optional argument, ranks, is either None or an iterable
        yielding some sub-set of range(0, self.rank); if None, the whole
        range is presumed.  All members, i, of the set indicated by
        ranks must have self.dimension[i] equal.  The result is obtained
        by averaging self.tau(s) over all permutations s of range(0,
        self.rank) that preserve the indices not in ranks.\n"""

        return self.__perm_average(self.dimension, ranks, self.tau)

    def antisymmetrise(self, ranks=None):
        """Return antisymmetric part of a tensor.

        Single optional argument, ranks, is either None or an iterable
        yielding some sub-set of range(0, self.rank); if None, the whole
        range is presumed.  All members, i, of the set indicated by
        ranks must have self.dimension[i] equal.  The result is obtained
        by averaging self.tau(s) * s.sign over all permutations s of
        range(0, self.rank) that preserve the indices not in ranks.\n"""

        return self.__perm_average(self.dimension, ranks,
                                   lambda e, t=self.tau: t(e) * e.sign)

    def transpose(self, n=1):
        """Transpose a tensor; only applicable if self.rank > 1.

        Optional argument, n, defaults to 1; it must be a natural number less
        than self.rank (and passing 0 is fatuous; you get self).\n"""

        if n < 0 or n != int(n):
            raise ValueError("Should be a natural number", n)
        elif n >= self.rank:
            raise ValueError("Should be less than rank", n, self.rank)
        elif n == 1:
            return self.__vector__(self[0].mapwith(
                    lambda *args: args, *self[1:]).map(self.__vector__))
        elif n == 0: return self

        return self.__vector__(
            self.transpose().map(lambda v: v.transpose(n-1))).transpose()

    def embed(self, route, other=None):
        """Embeds a tensor into a higher dimension.

        Required argument, route, is a sequence of sequences of indices, with
        len(route) <= self.rank, len(route[i]) <= self.dimension[i] for each i
        and no duplicate entries in any given route[i].  (Note that route and
        each of its entries shall be iterated repeatedly; so must be
        sequences, not iterators.)  The returned tensor shall have each
        .dimension[i] >= that of self and > max(route[i]).

        Any index-tuple s valid for the result with each s[i] in route[i],
        hence s[i] = route[i][t[i]] for some tuple t, has self[t] as the entry
        at [s], at least when len(s) == len(route); otherwise, the entry in
        the result is a suitable .embed() of the entry in self.

        Optional second argument, other, defaults to None, denoting a suitably
        large Tensor whose entries are all zero; otherwise, it is a Tensor
        from which to draw the values not determined by the fore-going rule
        for entries in the result.  As such, it must have the same .rank as
        self, with max(route[i]) < other.dimension[i] >= self.dimension[i] for
        each i.  In this case, self and other must have equal
        .dimension[len(route):] to make the 'leaf' values of the embedding
        match up.  (We could pad with map(range, self.dimension[len(route):])
        if this condition were not met, but it'd be less efficient and risk
        hiding errors; the caller can do such padding if really intended.)

        Example: for n < k > m,
        Tensor.circulate(a).embed(((n, m), (n, m)), Tensor.diagonal([1] * k))
        is a linear map on a k-dimensional space, acting as a rotation on the
        n-th and m-th directions and the identity on all others.\n"""

        n = len(route)
        if n > self.rank:
            raise ValueError('Too many index sequences', route, self.rank)

        bad = filter(lambda r: min(r) < 0, route)
        if bad: raise ValueError('Negative indices', bad)

        bad = filter(lambda r: len(set(r)) != len(r), route)
        if bad: raise ValueError('Duplicate entries in indexing', bad)

        bad = filter(lambda i, r=route, d=self.dimension: len(r[i]) > d[i],
                     range(n))
        if bad: raise ValueError(
            'Indexing outside available range', bad, self.dimension, route)

        if other is None:
            # Compute implicit 'big enough' Tensor of zeros:
            other = self.xerox(tuple(map(
                        lambda r, d: max(1+r, d),
                        map(max, route), self.dimension[:n])) +
                               self.dimension[n:])

        elif other.rank != self.rank:
            raise ValueError('Mismatched rank', other.rank, self.rank)
        elif other.dimension[n:] != self.dimension[n:]: raise ValueError(
            'Mismatched leaf dimensions', n, other.dimension, self.dimension)
        else:
            bad = filter(
                lambda i, r=map(max, route), m=other.dimension: r[i] >= m[i],
                range(n))
            if bad: raise ValueError('Indexing outside reference tensor',
                                     bad, other.dimension, route)

        return self.fromSeq(self.__embed(route, other))

    def __embed(self, route, other):
        """See embed.  Computes the brute sequence [of sequences ...]."""

        row, here, route = list(other), route[0], route[1:]
        if route:
            assert self.rank > len(route)
            assert len(row) > max(here) and min(here) >= 0
            assert self.dimension[0] >= len(here)
            for i, r in enumerate(here):
                row[r] = self[i].__embed(route, other[r])
        else:
            assert other.dimension[1:] == self.dimension[1:]
            for i, r in enumerate(here):
                row[r] = self[i]

        return row

    # TODO: can this be more efficient ?
    # __mul__ is quite heavy-weight; can we short-cut it ?
    # Is it worth it ?
    def dot(self, other, n=1, out=True):
        """Contracts self with other.

        Required argument, other, should be a Vector.  Optional arguments:
          n -- number of ranks of self to contract out with equally many of
               other; defaults to 1.
          out -- when n > 1, determines how ranks of self are matched up with
                 ranks of other; see below.

        This method first computes self * other, then traces away some ranks
        from the product.  If n is 1, the last rank from self is traced with
        the first of out.  When n > 1, if out is true, ranks of self are
        matched with ranks of other starting 'where they meet', with self's
        last and other's first, and 'working outwards from there', so self's
        last-but-one rank is contracted with others second rank, and so on,
        until self's last-but-(n-1) rank is contracted with other's n-th
        rank.  If out is false, self's last rank is contracted with other's
        n-th rank; self's last-but-i rank is contracted with other's (n-i)-th
        rank and so on, until self's last-but-(n-1) rank is contracted with
        other's first rank.

        Returns what's left of the product after all this tracing has been
        applied.\n"""
        return apply((self * other).permutrace,
                     self.__derange(out, n, self.rank))

    @staticmethod
    @postcompose(lambda x: ((),) + tuple(x))
    def __derange(out, n, r):
        """Returns pairs of indices for contraction.

        Arguments out and n are as for .dot() and .rdot(); r is the index of
        the first rank of the right operand.  We thus pair up the indices from
        r to n+r-1 with those from r-n to r-1, either in same order or reverse
        order according to out.  Since both callers use the sequence of pairs
        after an empty tuple as parameters to .permutrace(), package it thus
        by use of @postcompose.\n"""

        i = n + r
        if out: j, s = r - n, 1
        else: j, s = r - 1, -1

        while i > r:
            i -= 1
            yield j, i
            j += s

    def rdot(self, other, n=1, out=True):
        """Reverse contraction; c.f. .dot()

        Takes the same arguments as .dot(); the effect is exactly as if self
        and other were swapped, save that other need not actually be a Vector
        (or Tensor) for it to work; indeed, the implementation expects that it
        is not (so is mildly less efficient than other.dot(self) when other is
        a Vector or Tensor).\n"""
        prod = other * self
        return apply(prod.permutrace,
                     self.__derange(out, n, prod.rank - self.rank))

    def tau(self, pattern):
        """Generalised trace-permutation operator.

        Single operand, pattern, is a sequence of length at most self.rank
        whose entries may be of two kinds: for some natural n, the whole
        numbers 0 through n-1 appear once each in pattern; any other entries
        in pattern must be strings, each of which must appear exactly twice in
        pattern, at indices whose matching entries in self.dimension are
        equal.  (The tensor modelled by self must in fact have mutually dual
        spaces at its relevant tensor rank factors; but this implementation
        only knows about their dimensions.)

        For each pair of indices in pattern that share the same string, we
        contract out (trace) the ranks of self having those indices.  The
        remaining ranks of self we permute according to the integer indices;
        if pattern[a] is an integer i, then the result's i-th rank shall
        correspond to self's a-th rank.

        The result is the tensor that would be obtained by taking the
        following steps, with dim = self.dimension:

          * whenever pattern[a] == pattern[b], b > a, we require dim[a] ==
            dim[b] and replace self with a tensor having two fewer ranks; this
            has dimension = dim[:a] +dim[a+1:b] +dim[b+1:]; for each valid
            index-tuple s into it, with len(s) == b-1, its [s] entry is the
            sum over i in range(dim[b]) of self[s[:a] + (i,) + s[a:] + (i,)].

            After replacing self with the thus-contracted tensor, we use
            pattern[:a] +pattern[a+1:b] +pattern[b+1:] in place of pattern.

          * once all strings are thus eliminated, we are left with a
            permutation as pattern; we re-organise self to produce a result:
            whose dimension[pattern[a]] is dim[a], for each a; and, for each
            valid index-tuple s into it, its [s] entry is self[t] where t[a] =
            s[pattern[a]] for each index a into pattern.

        The final result is not, however, computed as inefficiently as this
        would imply.  Contrast .permutrace().\n"""

        stub, pairs, bok, n = [ None ] * len(pattern), [], {}, []
        for i, a in enumerate(pattern):
            if isinstance(a, basestring):
                try: j = bok[a]
                except KeyError: bok[a] = i
                else:
                    bok[a] = None
                    if j is None: raise ValueError(
                        'Trace marker appears more than twice', a, pattern)
                    if self.dimension[i] != self.dimension[j]:
                        raise ValueError(
                            'Can only trace between ranks of equal dimension',
                            (j, i), self.dimension, a, pattern)
                    pairs.append((j, i))
            else:
                stub[i] = a
                if a+1 > len(n): n += range(len(n), a+1)
                if n[a] is None:
                    raise ValueError('Permutation index repeated', a, pattern)
                assert n[a] == a
                n[a] = None

        if filter(lambda a: a is not None, n):
            raise ValueError('Incomplete permutation', tuple(n), pattern)

        return self.__trace_permute(stub, pairs)

    def permutrace(self, shuffle, *pairs):
        """Alternate trace-permute operation.

        First argument, shuffle, is a permutation optionally padded with None
        entries; its length must not exceed self.rank and the non-None entries
        in it should be the integers 0 through n-1, each appearing exactly
        once, for some natural n.

        Each subsequent argument, if any, must be a pair (i, j) of naturals
        less than self.rank, with self.dimension[i] == self.dimension[j].  If
        either is less than len(shuffle) the entry in shuffle at this index
        must be None.  No two pairs may have an entry in common.  Each None
        entry in shuffle must appear in exactly one pair.

        If any pair includes an index greater than or equal to shuffle's
        length, shuffle is implicitly padded with None entries for each such
        index in a pair and an order-preserving continuation of its
        permutation.  Thus permutrace((None, 1, 0), (0, 5), (3, 7)) uses
        (None, 1, 0, None, 2, None, 3, None) as its implicit extended shuffle.

        Has the same effect as tau (q.v.) but with a string, unique for each
        pair (i, j), used in place of shuffle[i] and shuffle[j], to make a
        suitable pattern.  Each pair indicates two ranks to trace out; the
        remaining ranks are permuted according to shuffle.\n"""

        bad = filter(lambda x: len(x) != 2, pairs)
        if bad:
            raise ValueError("Each index pair's length should be two", bad)

        bad = filter(lambda (i, j), d=self.dimension: d[i] != d[j], pairs)
        if bad: raise ValueError(
            "Traced ranks must have equal dimension", bad, self.dimension)

        bok = set()
        for i in reduce(lambda x, y: x+y, pairs, ()):
            if i < 0 or (
                i < len(shuffle) and shuffle[i] is not None
                ) or i in bok:
                bad.append(i)
            bok.add(i)
        if bad: raise ValueError(
            "Bad or repeat index in tracing pairs", bad, tuple(bok), shuffle)

        n = filter(lambda x: x is not None, shuffle)
        if n: n = range(1 + max(n))
        for i, a in enumerate(shuffle):
            if a is None:
                if i not in bok: raise ValueError(
                    "Rank neither traced nor permuted", i, shuffle, pairs)
            else:
                if n[a] is None: bad.append[a]
                n[a] = None
        if bad:
            raise ValueError("Not a permutation", bad, shuffle)
        bad = filter(lambda i: i is not None, n)
        if bad:
            raise ValueError("Incomplete permutation", bad, shuffle)

        return self.__trace_permute(shuffle, pairs)

    # Implementation of __trace_permute:
    def __listify(self):
        if self.rank < 2: return list(self)
        return map(lambda x: x.__listify(), self)

    @classmethod
    def __ranger(cls, ms):
        """Iterator over index tuples.

        Single argument, ms, is an iterator over dimensions.  Yields every
        tuple s, of the same length as ms, for which, for every 0 <= i <
        len(ms), 0 <= s[i] < ms[i].\n"""

        try: m = ms.next()
        except StopIteration: yield ()
        else:
            for s in cls.__ranger(ms):
                i = m
                while i > 0:
                    i -= 1
                    yield (i,) + s

    def __indices(self, tmpl, pairs):
        try: i, j = pairs.next()
        except StopIteration: yield tuple(tmpl)
        else:
            assert self.dimension[i] == self.dimension[j]
            assert tmpl[i] is None is tmpl[j]
            if i > j: i, j = j, i
            for s in self.__indices(tmpl, pairs):
                m = self.dimension[i]
                while m > 0:
                    m -= 1
                    yield s[:i] + (m,) + s[i+1:j] + (m,) + s[j+1:]

    def __total(self, tmpl, pairs):
        es = self.__indices(tmpl, iter(pairs))
        tot = self[es.next()]
        for e in es: tot += self[e]
        return tot

    def setcell(grid, key, val): # tool function for __trace_permute
        old = key.next()
        for it in key: grid, old = grid[old], it
        assert grid[old] is None
        grid[old] = val

    def __trace_permute(self, shuffle, pairs, store=setcell):
        """Implementation of tau and permutrace, q.v.

        Takes two arguments, a permutation optionally padded with None
        entries and a sequence of pairs of indices to trace.\n"""

        ns, i = filter(lambda x: x is not None, shuffle), len(shuffle)
        if ns: j = max(ns) + 1
        else: j = 0

        if pairs:
            # Pad shuffle so that every pair's indices have None in it:
            ns = reduce(lambda x, y: x+y, pairs)
            n, shuffle = max(ns), list(shuffle)
            while i <= n:
                if i in ns: shuffle.append(None)
                else:
                    shuffle.append(j)
                    j += 1
                i += 1
        shuffle = tuple(shuffle) # we're done modifying it

        # Construct reverse-lookup for shuffle:
        rev = [ None ] * j
        while i > 0:
            i -= 1
            if shuffle[i] is not None:
                assert rev[shuffle[i]] is None
                rev[shuffle[i]] = i
        assert None not in rev, (rev, shuffle)

        # Perform contraction:
        slab, total = (None,) * len(shuffle), self.__total
        if rev:
            dim = tuple( self.dimension[r] for r in rev )
            grid = self.xerox(dim, None).__listify()

            for s in self.__ranger(iter(dim)):
                tmpl = list(slab)
                for i, a in enumerate(rev): tmpl[a] = s[i]
                store(grid, iter(s), total(tmpl, pairs))

            # Don't pass dim; may lack a few late entries:
            return self.fromSeq(grid)

        return total(slab, pairs)
    del setcell

    # Further implementation details
    from study.maths.permute import Permutation
    from study.maths.ratio import Rational
    @staticmethod
    def __perm_average(dims, ranks, func, gen=Permutation.fixed, rat=Rational):
        if ranks is None: ranks = tuple(range(len(dims)))
        else: ranks = tuple(ranks)
        if len(set(map(lambda i, d=dims: d[i], ranks))) != 1:
            raise ValueError('Can only average over permutations of ranks of equal dimension',
                             ranks, dims)

        if ranks: n = max(ranks) + 1
        else: n = 0
        pat = range(n)
        for i in ranks: pat[i] = None
        es = gen(n, pat)

        ans, n = func(es.next()), 1
        for e in es: ans, n = ans + func(e), n + 1
        return ans * rat(1, n)

    del Permutation, Rational

Tensor = Vector # alias

del lazyprop, Tuple, postcompose
