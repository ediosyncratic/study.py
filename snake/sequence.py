"""Assorted classes relating to sequences.

$Id: sequence.py,v 1.10 2008-06-08 15:07:10 eddy Exp $
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

    Implements map, reduce, sum, product and filter as methods of the iterable.\n"""
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

class Ordered (list):
    """An ordered set.

    Like a list except that you don't get to chose where in it to put each
    entry; and duplication is silently ignored.  Entries must support comparison
    with one another; they shall be kept in increasing order.\n"""

    __upinit = list.__init__
    def __init__(self, val=None):
        self.__upinit(val or [])

    def __locate(self, value):
        """Where does value belong in this list ?

        If value is equal to some entry in this list, return the index of that
        entry; otherwise, return the index at which it should be inserted.  A
        return of -1 means 'after the end'.\n"""
        if not self or self[0] > value: return 0
        if self[-1] < value: return -1 # magic token for "after end"
        lo, hi = 0, len(self) - 1
        while hi > 1 + lo:
            mid, ig = divmod(lo + hi, 2)
            if self[mid] > value: hi = mid
            elif self[mid] < value: lo = mid
            else: return mid

        return hi

    def index(self, value):
        ind = self.__locate(value)
        if ind < 0 or value != self[ind]:
            raise ValueError('Not in list', value)
        return ind

    __listins = list.insert
    __listapp = list.append
    def append(self, value):
        # Insert in correctly-ordered position.
        at = self.__locate(value)
        if at < 0: self.__listapp(value)
        elif self[at] == value: return False
        else: self.__listins(at, value)
        return True

    insert = append

    def prune(self, upto):
        """Discard all entries <= upto.

        Returns the discarded entries.\n"""
        at = self.__locate(upto)
        if at < 0:
            was = self[:]
            del self[:]
        else:
            if self[at] == upto: at += 1
            was = self[:at]
            if at > 0: del self[:at]

        return was
