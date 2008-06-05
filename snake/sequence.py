"""Assorted classes relating to sequences.

$Id: sequence.py,v 1.9 2008-06-05 07:26:39 eddy Exp $
"""

class Tuple (object):
    """Pretend to be a tuple.

    Infuriatingly, if a class inherits from tuple, whatever you pass to its
    constructor is passed to tuple's constructor (which barfs if there are too
    many arguments), before your __init__ can do anything about it; and __init__
    can't usefully call tuple.__init__ later, to supply what you wanted it to
    get.\n""" # How messed up is that ?

    def __init__(self, vals): self.__tuple = tuple(vals)
    def __len__(self): return len(self.__tuple)
    def __repr__(self): return `self.__tuple`
    def __hash__(self): return hash(self.__tuple)
    def __iter__(self): return iter(self.__tuple)
    def __contains__(self, val): return val in self.__tuple

    def __eq__(self, other): return other == self.__tuple
    def __ge__(self, other): return other <= self.__tuple
    def __le__(self, other): return other >= self.__tuple
    def __lt__(self, other): return other > self.__tuple
    def __gt__(self, other): return other < self.__tuple
    def __ne__(self, other): return other != self.__tuple

    def _tuple_(self, val):
        """Pseudo-constructor.

        Takes a sequence and returns an instance of Tuple.  If derived classes
        have constructors taking different parameter lists, they should
        over-ride this with something suitable; if they don't support arithmetic
        and slicing, they should over-ride it with something that raises an
        error.  Otherwise, it uses the class of self to construct a new Tuple of
        suitable type.\n"""
        return self.__class__(val)

    def __mul__(self, other): return self._tuple_(self.__tuple * other)
    def __rmul__(self, other): return self._tuple_(other * self.__tuple)
    def __add__(self, other):
        if isinstance(other, Tuple): other = other.__tuple
        return self._tuple_(self.__tuple + other)

    from interval import Slice
    def __getitem__(self, key, S=Slice):
        if isinstance(key, slice) or isinstance(key, S):
            return self._tuple_(self.__tuple[key])
        return self.__tuple[key]
    del Slice

    # It is also absurd that tuple doesn't support index:
    def index(self, val): return list(self.__tuple).index(val)
    # and throw in something suitable in place of sort:
    def order(self, are=cmp): return self.__order(are)
    def __order(self, par):
        # boot-strap round the fact that permute.Permute inherits from Tuple
        from study.maths import permute
        def order(who, p=permute.order, are=cmp): return p(who.__tuple, are)
        Tuple.__order = order
        return permute.order(self.__tuple, par)

class Iterable (object):
    """Mix-in class to extend iterables in some handy ways.

    Implements map, reduce, sum, product and filter as methods of the iterable.
    """
    def map(self, func):
        ans = []
        for p in self:
            ans.append(func(p))
        return ans

    def reduce(self, func, init=0):
        for p in self:
            init = func(init, p)
        return init

    def sum(self): return self.reduce(lambda x, y: x + y)
    def product(self): return self.reduce(lambda x, y: x * y, 1)

    def filter(self, test=None):
        if test is None: return self
        ans = []
        for p in self:
            if test(p):
                ans.append(p)
        return ans

    # The default __contains__ simply iterates over the list testing ==

class WrapIterable (Iterable):
    def __init__(self, seq): self.__seq = seq
    def __iter__(self): return iter(self.__seq)
    def __getattr__(self, key): return getattr(self.__seq, key)
