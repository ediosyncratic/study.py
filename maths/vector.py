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
        except TypeError:
            return self.__vector__(map(lambda x, o=other: x * o, self))

        return self.__vector__(map(lambda o, s=self: s * o, other))

    def __rmul__(self, other):
        try: other[:]
        except TypeError:
            return self.__vector__(map(lambda x, o=other: o * x, self))

        return self.__vector__(map(lambda o, s=self: o * s, other))

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
        try: key[:]
        except TypeError: return self.__upget(key)
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
            return (len(self), *tail)

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

del lazyprop, ReadSeq
