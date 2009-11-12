"""Exact Analysis of discrete random processes.
"""

class Vector (tuple):
    def __add__(self, other, add=lambda x, y: x+y):
        assert len(other) == len(self)
        return self.__class__(map(add, self, other))

    def __sub__(self, other, sub=lambda x, y: x-y):
        assert len(other) == len(self)
        return self.__class__(map(sub, self, other))

    def __mul__(self, other):
        try: other[:]
        except TypeError:
            return self.__class__(map(lambda x, o=other: x * o, self))

        return self.__class__(map(lambda o, s=self: s * o, other))

    def __rmul__(self, other):
        try: other[:]
        except TypeError:
            return self.__class__(map(lambda x, o=other: o * x, self))

        return self.__class__(map(lambda o, s=self: o * s, other))

from study.snake.sequence import Dict
class Spread (Dict):
    __upget = Dict.__getitem__
    def __getitem__(self, key):
        # Make all missing keys appear to exist with value 0:
        try: return self.__upget(key)
        except KeyError: return 0

    @classmethod
    def uniform(k, seq):
        "Represents a uniform distribution on the given sequence of values"
        ans = k()
        for i in seq: ans[i] += 1
        return ans

    @classmethod
    def die(k, n, step=1):
        "Represents an n-sided fair die with faces labelled 1 through n"
        return k.uniform(range(1, 1+n, step))

    def __len__(self): return self.itervalues().sum()
    from ratio import Rational as __rat
    def p(self, key): return self.__rat(self[key], len(self))
    def E(self, func=lambda (x, w): x * w, add=lambda x, y: x + y):
        """Computes expected values.

        First argument, func, is a callable that takes a two-ple as input: a key
        of self and a rational representing that key's probability.  Its default
        just multiplies the two together (assuming this to be possible).  Second
        argument is a function that reduces a list of outputs of func to a
        single value; its default is the builtin sum.\n"""
        return self.iteritems().map(func).reduce(add) * self.__rat(1, len(self))

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

    def vector(self, n):
        return self.join(None, * (self,) * n)

    def map(self, func):
        return self.join(func, self)

    @classmethod
    def join(k, func=None, *what):
        """Build a new Spread object out of some existing ones.

        Generally, other methods of this class package this one more usably; but
        this is the sledgehammer that should be able to crack every nut ...

        First argument, func, is None (the default) or a function whose outputs
        shall be used as keys of the new object; if func is None, lambda *a: a
        is used.  All subsequent arguments (there must be some) should be Spread
        objects (but may be dict objects whose values are all numeric).  Each
        call to func receives, as parameters, one key from each of these
        objects, in the same order as the objects are passed to join().  The
        return from func is used as a key of the new object, whose value is the
        product of the value each of join()'s argument objects associates to the
        one of its keys that was used as parameter.

        A given Spread object may be used repeatedly as an argument to join();
        it shall be handled as if each use of it was a separate copy of the
        object.\n"""

        if func is None: func = lambda *a: Vector(a)
        assert what
        ans = k()
        for t, n in k.__renee(*what):
            ans[func(*t)] += n
        return ans

    @staticmethod
    def __product(o, r=None):
        if r is None:
            for i, n in o.iteritems():
                yield (i,), n
        else:
            for s, m in r:
                for i, n in o.iteritems():
                    yield (i,) + s, n * m

    @classmethod
    def __renee(k, one, *rest):
        """Cartesian iterator.

        All arguments should normally be Spread objects (but they can,
        alternatively, be dict objects whose values are numeric); takes at least
        one, plus arbitrarily many more.  Returns an iterator, each yield of
        which is a pair of a tuple and a count; the tuple's length is equal to
        the number of dict objects passed as arguments to k.__renee().  Each
        entry in the tuple is a key of the corresponding dict object; the count
        is the product, over entries in the tuple, of their values in their
        corresponding dict objects.  The iterator traverses every possible tuple
        and count conforming to this spec.\n"""

        if rest: return k.__product(one, k.__renee(*rest))
        return k.__product(one)

del Dict
