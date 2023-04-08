"""Tools related to linear spaces and maps.

Class:
  Function -- wraps a linear combination of functions

See study.LICENSE for copyright and license information.
"""
from study.snake.addict import AddDict
from study.snake.sequence import Tuple

class Function (object):
    """Wraps a linear combination of callables as a vector.

    Normally, python doesn't let you multiply a callable by a scaling,
    that can scale its outputs.  Nor, when you have two functions that
    take the same parameters and return arithmetically-compatible
    results, does python let you add them.  However, the algebraic
    formalism of vector spaces allows arithmetic operations on such
    functions, provided their outputs are compatible for the
    operation, with the result being a function taking the same
    parameters as the function scaled or the functions combined.  This
    lets us build up arbitrary linear combinations of functions, as
    long as they take the same parameters.  The combination takes the
    same parameters, forwards them to each function and applies its
    linear combination to the results.

    Callables need to be hashable to be included in a linear
    combination: they shall be used as keys in a dictionary.

    A Function is immutable once created and hashable.  Function
    objects can be multiplied; the functions they wrap are combined
    pointwise, as for addition.
    """
    def __init__(self, *pairs):
         """Construct a Function object.

         Takes a sequence of pairs (assumed to behave like twoples),
         each with a callable as first entry and its weight as second.
         For example, these may be the items() of a mapping from
         functions to weights.  The callables may theselves be
         Function objects and the same callable may appear as the
         first member of several pairs.  A Function object effectively
         contributes, as further pairs replacing it, the functions snd
         weights describing its linear combination.  All instances
         """
         self.__sum = AddDict()
         while pairs:
             (func, weight), pairs = pairs[0], pairs[1:]
             if isinstance(func, Function):
                 pairs += tuple((f, w * weight) for f, w in func.items())
             else:
                 self.__sum[func] += weight

    @classmethod
    def wrap(cls, function):
        """Convenience constructor to simply wrap a function.

        Pass this a function to simply wrap that function; it'll get
        weight 1.
        """
        return cls((function, 1))

    @classmethod
    def _iterdict_(cls, what=()):
        if isinstance(what, dict):
            return cls(*what.items())
        # Hope it's iterable:
        return cls(*what)

    def __call__(self, *args, **kw):
        return sum(f(*args, **kw) * w for f, w in self.__sum.items())

    def __hash__(self):
        return hash(tuple(self.__sum.items())) ^ hash(self.__class__)
    def __eq__(self, other):
        if isinstance(other, Function):
            return self.__sum == other.__sum
        if len(self.__sum) == 1:
            try:
                if callable(other):
                    return self.__sum[other] == 1
                return self.__sum[self.__one] == other
            except KeyError:
                pass
        return False

    def __len__(self):
        return len(self.__sum)

    def __neg__(self):
        return Function(*((k, -w) for k, w in self.__sum.items()))

    def __add__(self, other):
        if isinstance(other, Function):
            return Function(*(tuple(self.__sum.items()) +
                              tuple(other.__sum.items())))
        if callable(other):
            return Function(*(tuple(self.__sum.items()) + ((other, 1),)))
        return Function(*(tuple(self.__sum.items()) + ((self.__one, other),)))
    __radd__ = __add__
    def __rsub__(self, other):
        return other + (-self)
    def __sub__(self, other):
        if isinstance(other, Function):
            return self + (-other)
        if callable(other):
            return Function(*(tuple(self.__sum.items()) + ((other, -1),)))
        return Function(*(tuple(self.__sum.items()) + ((self.__one, -other),)))

    @staticmethod
    def __one(*args, **kw):
        """A callable to scale with constants when added.

        Accepts and ignores arbitrary arguments.  Returns 1.
        """
        return 1

    class FuncProd (Tuple):
        """A pointwise product of functions.

        Since the outputs of the functions might not commute, this is
        just a tuple of the functions, possibly with some repeats.
        Caller is responsible for implementing pointwise
        multiplication, see Fuction.__multiply(), for which this class
        is a tool.  In particular, that takes care of a scalar
        constant commuting with all others.
        """
        def __call__(self, *args, **kw):
            return self.map(lambda f: f(*args, **kw)).product()

    @classmethod
    def __multiply(cls, u, v, prod=FuncProd):
        f, w = u
        g, z = v
        weight = w * z
        if f is cls.__one:
            return (g, weight)
        if g is cls.__one:
            return (f, weight)
        if isinstance(f, prod):
            if isinstance(g, prod):
                return prod(f + g), weight
            return prod(f +(g,)), weight
        if isinstance(g, prod):
            return prod((f,) + g), weight
        return prod((f, g)), weight
    del FuncProd

    def __mul__(self, other):
        if isinstance(other, Function):
            return Function(*(self.__multiply(u, v)
                              for u in self.__sum.items()
                              for v in other.__sum.items()))
        if callable(other):
            return Function(*(self._multiply(u, (other, 1))
                              for u in self.__sum.items()))
        return Function(*((f, w * other) for f, w in self.__sum.items()))

    def __rmul__(self, other):
        if isinstance(other, Function):
            return Function(*(self.__multiply(u, v)
                              for u in other.__sum.items()
                              for v in self.__sum.items()))
        if callable(other):
            return Function(*(self._multiply((other, 1), u)
                              for u in self.__sum.items()))
        return Function(*((f, other * w) for f, w in self.__sum.items()))

del Tuple
