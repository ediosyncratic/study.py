"""Vectors (and tensors).
"""
from study.cache.property import lazyprop
from study.snake.sequence import ReadSeq

class Vector (ReadSeq, tuple):
    """Simple tuple type supporting basic arithmetic.

    Supports entry-wise addition and subtraction (i.e. (x+y)[i] = x[i]+y[i],
    etc.); supports multiplication by a scalar (applied entry-wise, so (k*x)[i]
    = k * x[i]) and tensorial multiplication, (3,5)*(1,2) = ((3,5),(6,10)),
    which is not commutative.

    The product of two Vector objects is technically a Vector whose entries are
    Vector-valued; such a vector is termed a tensor. The depth of nesting of
    Vector within Vector is known as the 'rank' of the tensor; a plain Vector,
    with numeric entries, has rank 1; multiplying two tensors of ranks n and m
    yields a tensor of rank n+m.\n"""

    @classmethod
    def __vector__(cls, seq):
        """Pseudo-constructor derived classes can over-ride.

        By default, various methods of this class create objects of the same
        class as the instance whose method was called; this is achieved simply
        by passing this method the sequence of values to be used as entries in
        the new object.  If a derived class has a new/constructor with a
        different signature, it should over-ride this method to do something
        sensible; it is always called as self.__vector__(seq) with self an
        instance of this class.\n"""
        return cls(seq)

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

        return self.__vector__(map(lambda s, o=other: s * o, self))

    def __rmul__(self, other):
        try: other[:]
        except TypeError:
            return self.__vector__(map(lambda s, o=other: o * s, self))

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

        Single argument, key, may be a simple integer, in which case it is used
        as index into self, as a tuple, in the usual way.  Otherwise, key should
        be a sequence of indices, of length at most self.rank; the result is
        defined inductively by: self[()] = self, self[(h, *t)] = self[h][t] or,
        more intuitively if marginally less rigorously, specified as
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
        entry in self is a Vector and all have the same rank; self's rank is one
        greater than that of its entries.\n"""
        if isinstance(self[0], Vector):
            assert not filter(lambda x: not isinstance(x, Vector), self)
            r = self[0].rank
            assert not filter(lambda x, n=r: x.rank != r, self[1:])
            return 1 + r
        return 1

    @lazyprop
    def dimension(self, cls=None):
        """The sequence of dimensions at different ranks.

        For a tensor, len(self[i]) is the same for all 0 <= i < len(self); for
        each such i, len(self[i][j]) is the same for all 0 <= j < len(self[i]);
        and so on.  The tuple self.dimension, of length self.rank, collects up
        these lengths, starting with dimension[0] = len(self).

        Given the extended indexing provided by __getitem__, in so far as
        dimension[:i] is determined and i <= self.rank, for any sequence s of
        length i having, for 0 <= j < i, 0 <= s[j] < dimension[j], we define
        dimension[i] to be len(self[s]) and assert that its value is independent
        of choice of s, subject to given constraints.\n"""
        if isinstance(self[0], Vector):
            assert not filter(lambda x: not isinstance(x, Vector), self)
            tail = self[0]. dimension
            assert not filter(lambda x, t=tail: x.dimension != t, self[1:])
            return (len(self),) + tail

        return (len(self),)

    def transpose(self, n=1):
        """Transpose a tensor; only applicable if self.rank > 1.

        Optional argument, n, defaults to 1; it must be a natural number less
        than self.rank (and passing 0 is fatuous; you get self).\n"""

        if n < 1 or n != int(n): raise ValueError("Should be a natural number", n)
        elif n >= self.rank: raise ValueError("Should be less than rank", n, self.rank)
        elif n == 1:
            return self.__vector__(self[0].mapwith(
                    lambda *args: args, *self[1:]).map(self.__vector__))
        elif n == 0: return self

        return self.__vector__(self.transpose().map(lambda v: v.transpose(n-1))).transpose()

    def tau(self, pattern):
        """Generalised trace-permutation operator.

        Single operand, pattern, is a sequence of length at most
        self.rank whose entries are of two kinds: for some natural n,
        the whole numbers 0 through n-1 appear once each in pattern; all
        other entries in pattern must be strings, each of which must
        appear exactly twice in pattern, at indices whose matching
        entries in self.dimension are equal.

        For each pair of indices in pattern that share the same string,
        we trace out the ranks of self having those indices.  The
        remaining ranks of self we permute according to the integer
        indices; if pattern[a] is an integer i, then the result's i-th
        rank shall correspond to self's a-th rank.

        The result is the tensor that would be obtained by taking the
        following steps, with dim = self.dimension:

          * whenever pattern[a] == pattern[b], b > a, we require dim[a]
            == dim[b] and replace self with a tensor having two fewer
            ranks; this has dimension = dim[:a] +dim[a+1:b] +dim[b+1:];
            for each valid index-tuple s into it, with len(s) == b-1,
            its [s] entry is the sum over i in range(dim[b]) of
            self[s[:a] + (i,) + s[a:] + (i,)].

            After replacing self with the thus-contracted tensor, we use
            pattern[:a] +pattern[a+1:b] +pattern[b+1:] in place of
            pattern.

          * once all strings are thus eliminated, we are left with a
            permutation as pattern; we re-organise self to produce a
            result: whose dimension[pattern[a]] is dim[a], for each a;
            and, for each valid index-tuple s into it, its [s] entry is
            self[t] where t[a] = s[pattern[a]].

        The final result is not, however, computed as inefficiently as
        this would imply.\n"""

        stub, pairs, bok, n = [ None ] * len(pattern), [], {}, []
        for i, a in enumerate(pattern):
            if isinstance(a, basestring):
                try: j = bok[a]
                except KeyError: bok[a] = i
                else:
                    bok[a] = None
                    if j is None:
                        raise ValueError('Trace marker appears more than twice', a, pattern)
                    if self.dimension[i] != self.dimension[j]:
                        raise ValueError('Can only trace between ranks of equal dimension',
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

        First argument, shuffle, is a permutation optionally padded with
        None entries; its length must not exceed self.rank and the
        non-None entries in it should be the integers 0 through n-1,
        each appearing exactly once, for some natural n.

        Each subsequent argument, if any, must be a pair (i, j) of
        naturals less than self.rank; if either is less than
        len(shuffle) the entry in shuffle at this index must be
        None.  No two pairs may have an entry in common.  Each None
        entry in suffle must appear in exactly one pair.

        If any pair includes an index greater than or equal to shuffle's
        length, shuffle is implicitly padded with None entries for each
        such index in a pair and an order-preserving continuation of its
        permutation.  Thus permutrace((None, 1, 0), (0, 5), (3, 7)) uses
        (None, 1, 0, None, 2, None, 3, None) as its extended shuffle.

        Has the same effect as tau (q.v.) but with a string, unique for
        each pair (i, j), used in place of suffle[i] and suffle[j], to
        make a suitable pattern.  Each pair indicates two ranks to trace
        out, the remaining ranks are permuted according to shuffle.\n"""

        bad = filter(lambda x: len(x) != 2, pairs)
        if bad:
            raise ValueError("Each index pair's length should be two", bad)

        bad = filter(lambda (i, j), d=self.dimension: d[i] != d[j], pairs)
        if bad:
            raise ValueError("Traced ranks must have equal dimension", bad, self.dimension)

        bok = set()
        for i in reduce(lambda x, y: x+y, pairs, ()):
            if i < 0 or (i < len(shuffle) and shuffle[i] is not None) or i in bok:
                bad.append(i)
            bok.add(i)
        if bad:
            raise ValueError("Bad or repeat index in tracing pairs", bad, tuple(bok), shuffle)

        n = filter(lambda x: x is not None, shuffle)
        if n: n = range(1 + max(n))
        for i, a in enumerate(shuffle):
            if a is None:
                if i not in bok:
                    raise ValueError("Rank neither traced nor permuted", i, shuffle, pairs)
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

    def __vectorise(self, grid):
        # *not* a classmethod, as derived classes might make __vector__
        # an instance method
        try: grid[0][:]
        except TypeError: return self.__vector__(grid)
        return self.__vector__(map(self.__vectorise, grid))

    @classmethod
    def __ranger(cls, ms):
        """Iterator over index tuples.

        Single argument, ms, is a dimension tuple.  Yields every tuple
        s, of the same length as ms, for which, for every 0 <= i <
        len(ms), 0 <= s[i] < ms[i].\n"""

        if len(ms) < 1: yield ()
        else:
            for s in cls.__ranger(ms[1:]):
                i = ms[0]
                while i > 0:
                    i -= 1
                    yield (i,) + s

    def __indices(self, tmpl, pairs):
        if pairs:
            (i, j), pairs = pairs[0], pairs[1:]
            assert self.dimension[i] == self.dimension[j]
            assert tmpl[i] is None is tmpl[j]
            if i > j: i, j = j, i
            for s in self.__indices(tmpl, pairs):
                m = self.dimension[i]
                while m > 0:
                    m -= 1
                    yield s[:i] + (m,) + s[i+1:j] + (m,) + s[j+1:]
        else:
            yield tuple(tmpl)

    def __trace_permute(self, shuffle, pairs):
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

        def plate(s, r=rev, n=len(shuffle)):
            ans = [None] * n
            for i, a in enumerate(r):
                ans[a] = s[i]
            return tuple(ans)

        # Construct answer collector:
        grid, i = 0, len(rev)
        while i > 0:
            i -= 1
            grid = Vector([0] * self.dimension[rev[i]]) * grid

        if rev:
            dim = grid.dimension
            grid = grid.__listify()

            for s in self.__ranger(dim):
                es = self.__indices(plate(s), pairs)
                cell = self[es.next()]
                for e in es: cell = cell + self[e]
                g = grid
                for i in s[:-1]: g = g[i]
                g[s[-1]] = cell

            grid = self.__vectorise(grid)

        else:
            es = self.__indices(plate(()), pairs)
            grid = self[es.next()]
            for e in es: grid = grid + self[e]

        return grid

del lazyprop, ReadSeq
