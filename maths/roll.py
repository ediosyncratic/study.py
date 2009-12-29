"""Exact Analysis of discrete random processes.

Exported class:
  Spread -- description of discrete distributions
This uses study.maths.vector.Vector as a key-type, in places.
"""
from study.snake.sequence import Dict
from study.cache.property import lazyprop

class Spread (Dict):
    """Describe a discrete distribution.

    An instance of this class is a mapping from possible values to relative
    frequency of each.  Lookup on a non-key never raises KeyError: instead, it
    returns 0.  Typically, the keys shall be possible outcomes of some discrete
    random variate with rationally commensurate probabilities, such as arises
    from looking at the outcomes of fair die rolls.  When considered as a
    distribution, the relative frequencies are divided by their sum to get a
    probability distribution.

    Default constructor behaves as dict (q.v.), accepting arbitrary keys and
    values.  Class methods provided for specific cases:
      uniform(seq) -- map each entry in seq to 1
      die(n [,step=1]) -- outcome of an n-sided (or n/step sided) die-roll
      dice(i [, j, ...]) -- outcome of a roll of diverse dice

    Existing Spread objects may be combined to produce more, most generally by
    the class method join(f, [spreads...]), q.v.  In particular, this is used to
    support arithmetic between spread objects, combining keys using the given
    arithmetic operators and giving relative frequencies obtained by multiplying
    the relative frequencies associated, by operands, with the keys
    combined.  Thus die(6)+die(6) shall give the distribution for the sum of two
    fair six-sided die rolls.  Further transformations of existing Spread
    objects are provided by:
      filter(test) -- self restricted to its keys k for which test(k) is true
      vector(n) -- outcomes of n instances of self
      map(func) -- apply func to each key of self

    The len() of a Spread is the sum of its values (not the more usual number of
    key-value pairs; use len(.keys()) for that).  A Spread object also supports
    methods:
      split(n) -- (n-1)-tuple of split-points for n-iles
      p(key) -- actual frequency, relative frequency divided by total
      E([func, add]) -- expected values, see its own documentation
    the last two of which uses study.maths.ratio.Rational objects to represent
    fractions, when they arise.  Various statistical attributes:
      mean -- E(lambda (x,w): x*w)
      variance -- E(lambda (x,w), m=mean: (x-m)**2 * w)
      median -- split(2)[0]
    are also supported lazily, provided self's keys support addition (for mean,
    variance) and/or ordering (for median).\n"""
    __upget = Dict.__getitem__
    def __getitem__(self, key):
        # Make all missing keys appear to exist with value 0:
        try: return self.__upget(key)
        except KeyError: return 0

    @classmethod
    def uniform(cls, seq):
        "Represents a uniform distribution on the given sequence of values"
        ans = cls()
        for i in seq: ans[i] += 1
        return ans

    @classmethod
    def die(cls, n, step=1):
        "Represents an n-sided fair die with faces labelled 1 through n"
        return cls.uniform(range(1, 1+n, step))

    @classmethod
    def dice(cls, *ns):
        """Vector with each component a separate die-roll.

        Each argument should be a positive integer; if all are equal to some n,
        die(n).vector() will produce the same result.  Otherwise, each is used
        as if with die (q.v.) to produce one component of a vector, as for
        vector (q.v.).  Thus dice(4, 6, 8) shall describe a variate whose first
        component is uniformly distributed on 1 through 4 and so on.\n"""

        return cls.join(None, *map(cls.die, ns))

    def __len__(self): return self.itervalues().sum()
    def p(self, key): return self.__rat(self[key], len(self))
    def E(self, func=lambda (x, w): x * w, add=lambda x, y: x + y):
        """Computes expected values.

        First argument, func, is a callable that takes a two-ple as input: a key
        of self and a rational representing that key's probability.  Its default
        just multiplies the two together (assuming this to be possible).  Second
        argument is a function that adds either two outputs of func or one
        output of func and an output of an earlier call to itself; its default
        is simple addition.\n"""
        return self.iteritems().map(func).reduce(add) * self.__rat(1, len(self))

    @lazyprop
    def mean(self, cls=None):
        assert cls is None
        return self.E()

    @lazyprop
    def variance(self, cls=None):
        assert cls is None
        return self.E(lambda (x, w), m=self.mean: (x - m)**2 * w)

    @lazyprop
    def median(self, cls=None):
        assert cls is None
        return self.split(2)

    def split(self, n, par=cmp, mid=None):
        """Tuple of split-points between n-iles of distribution.

        Single required parameter, n, is the number of sub-divisions into which
        to split the range of self.keys(), such that each subdivision has (as
        nearly as possible) equal sum of values.

        Optional arguments are:
          par -- a comparison operator to use when deciding the order of
                 self.keys(); it defaults to cmp, which will only work if the
                 keys have a defined ordering.
          mid -- a function taking two keys of self and returning the mid-point
                 between them; or None (the default) to use study.maths.ratio's
                 Rational class, unless self's keys are vectors, in which case
                 Rational is used on each co-ordinate, to halve the sum of the
                 two keys.

        Returns an (n-1)-tuple, each entry in which is either a key of self or
        an output of mid() given two keys; for each integer i with 0 < i < n,
        this tuple's [i-1] entry is the i-th n-ile split-point, s, of the
        distribution, for which:
          sum([self.p(k) for k in self.keys() with k < s])*n <= i and
          sum([self.p(k) for k in self.keys() with k > s])*n <= n-i
        with equality in either both cases or neither case; if both, s is the
        mid() of two keys of self, if neither s is a key of self.\n"""

        assert n > 0
        if mid is None: mid = self.__mid
        ans, m, ks, all = (), n, self.keys(), len(self)
        ks.sort(par)

        i = len(ks) - 1
        tot = self[ks[i]]
        while m > 1:
            m -= 1
            while tot * n < all * m and i > 0:
                i -= 1
                tot += self[ks[i]]
            if tot * n > all * m or i == 0: ans = (ks[i],) + ans
            else:
                assert tot * n == all * m
                ans = (mid(ks[i-1], ks[i]),) + ans

        assert sum(map(self.p, ks[:i])) * n <= 1
        assert len(ans) + 1 == n
        return ans

    def __add__(self, other):
        if isinstance(other, Spread):
            return self.join(lambda x, y: x + y, self, other)

        return self.__class__(self.iteritems().map(lambda (k, v), o=other: (k+o, v)))

    __radd__ = __add__

    def __sub__(self, other):
        if isinstance(other, Spread):
            return self.join(lambda x, y: x - y, self, other)

        return self.__class__(self.iteritems().map(lambda (k, v), o=other: (k-o, v)))

    def __rsub__(self, other):
        if isinstance(other, Spread):
            return self.join(lambda x, y: y - x, self, other)

        return self.__class__(self.iteritems().map(lambda (k, v), o=other: (o-k, v)))

    def __mul__(self, other):
        if isinstance(other, Spread):
            return self.join(lambda x, y: x * y, self, other)

        if isinstance(other, (int, long)):
            if other > 0:
                return self.join(lambda *a: sum(a), * (self,) * other)

            raise ValueError('Should be positive', other)
        raise TypeError('Should be a Spread object or positive integer', other)

    __rmul__ = __mul__

    def filter(self, test):
        ans = self.__class__()
        for k, v in self.iteritems():
            if test(k): ans[k] = v
        return ans

    def vector(self, n):
        """Multiplex a distribution.

        Single argument, n, must be a natural number.  Each key of the result is
        a study.maths.vector.Vector of n keys of self; its value is the result
        of multiplying self's values for the entries in the Vector.  When self
        represents some random process, self.vector(n) represents the outcome of
        n independent samples from that random process.\n"""
        return self.join(None, * (self,) * n)

    def map(self, func):
        """Transform a distribution.

        Single argument, func, is a function that accepts self's keys as inputs;
        its outputs shall be used as keys of the result distribution, each with
        the value of the key of self that produced it (or the sum of all such
        values, if func maps several of self's keys to the same output).\n"""
        return self.join(func, self)

    @classmethod
    def join(cls, func=None, *what):
        """Build a new Spread object out of some existing ones.

        Generally, other methods of this class package this one more usably (see
        map(func) and vector(n); also arithmetic operations); but this is the
        sledgehammer that should be able to crack every nut ...

        First argument, func, is None (the default) or a function whose outputs
        shall be used as keys of the new object; if func is None, lambda *a:
        Vector(a) is used (study.maths.vector.Vector extends tuple with
        entry-wise arithmetic).  All subsequent arguments (there must be some)
        should be Spread objects (but may be dict objects whose values are all
        numeric).

        Each call to func receives, as parameters, one key from each of these
        objects, in the same order as the objects are passed to join().  The
        return from func is used as a key of the new object, whose value is the
        product of the values each of join()'s argument objects associates to
        the one of its keys that was used as parameter.

        A given Spread object may be used repeatedly as an argument to join();
        it shall be handled as if each use of it was a separate copy of the
        object.\n"""

        if func is None: func = cls.__tor
        assert what
        ans = cls()
        for t, n in cls.__renee(*what):
            if n: ans[func(*t)] += n
        return ans

    # Implementation details:
    from study.maths.vector import Vector as __vec
    @classmethod
    def __tor(cls, *vs): return cls.__vec(vs)

    from ratio import Rational as __rat
    @classmethod
    def __mid(cls, lo, hi):
        """Mid-point between two keys of a Spread"""
        try: lo[:0]
        except TypeError: return cls.__rat(lo + hi, 2)
        hi[:0] # shouldn't raise TypeError
        try: return lo.map(cls.__mid, hi)
        except AttributeError:
            return tuple(map(cls.__mid, lo, hi))

    @staticmethod
    def __product(o, r=None):
        """Iterate over key-tuple, weight pairs of o's product with r.

        Required argument, o, is a Spread object (or a mapping with numeric
        values).  Optional argument, r, is an iterator, each yield of which is a
        pair of a tuple and a weight; if omitted, the result is as if r yielded
        one pair, ((), 1).  The result yields, for each tuple, weight from r and
        each key and value of o, a pair of: a tuple that prepends o's key to r's
        tuple; a weight that's the product of r's weight and o's value.  This is
        used by __renee (q.v.) to build a cartesian product of keys, with
        numeric products for values.\n"""
        if r is None:
            for i, n in o.iteritems():
                yield (i,), n
        else:
            for s, m in r:
                for i, n in o.iteritems():
                    yield (i,) + s, n * m

    @classmethod
    def __renee(cls, one, *rest):
        """Cartesian iterator.

        All arguments should normally be Spread objects (but they can,
        alternatively, be dict objects whose values are numeric); takes at least
        one, plus arbitrarily many more.  Returns an iterator, each yield of
        which is a pair of a tuple and a count; the tuple's length is equal to
        the number of dict objects passed as arguments to cls.__renee().  Each
        entry in the tuple is a key of the corresponding dict object; the count
        is the product, over entries in the tuple, of their values in their
        corresponding dict objects.  The iterator traverses every possible tuple
        and count conforming to this spec.\n"""

        if rest: return cls.__product(one, cls.__renee(*rest))
        return cls.__product(one)

del Dict, lazyprop
