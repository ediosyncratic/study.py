"""Assorted classes relating to sequences.

$Id: sequence.py,v 1.15 2008-06-13 07:26:43 eddy Exp $
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
    entry; and duplication can be silently ignored.  Entries must support
    comparison with one another; they shall be kept in increasing order.  The
    comparison may be customised in the same ways as are supported by the usual
    list.sort method; and calling sort() can revise the customisation.  Slicing
    with a reversed slice order yields a reverse-ordered slice.\n"""

    __upinit = list.__init__
    def __init__(self, val=None, unique=True, reverse=False, key=None, cmp=None):
        """Construct ordered list.

        All arguments are optional:
          val -- sequence of (or iterator over) initial entries
          unique -- ignore duplicate entries
          reverse -- sort in reverse order (default: False)
          key -- name of attribute on which to compare or (default) None to use
                 values directly
          cmp -- comparison function or None (to use the default cmp)
        """
        self.__upinit()
        self.__unique, self.__cmp, self.__key, self.__rev = unique, cmp, key, reverse
        if val is not None:
            for it in val: self.append(it)

    __upget = list.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return an Ordered to hold it; if it's going in the
        reverse direction, suitably reverse the order.\n"""
        if isinstance(key, slice):
            if key.step is not None and key.step < 0: rev = not self.__rev
            else: rev = self.__rev
            return Ordered(self.__upget(key), rev, self.__key, self.__cmp)
        return self.__upget(key)

    try: list.__setslice__
    except AttributeError: pass
    else: # list still has this deprecated method, so we need to over-ride it:
        def __setslice__(self, i, j, val):
            self.__setitem__(slice(i, j), val)

    def __setitem__(self, key, val):
        """Replace value(s) indicated by key with value(s) supplied as val.

        This is interpreted as deletion of each replaced value and insertion of
        each supplied value, potentially at a different location.\n"""
        del self[key]
        if isinstance(key, slice):
            for it in val:
                self.append(it)
        else: self.append(val)

    __upsort = list.sort
    def sort(self, cmp=None, key=None, reverse=False):
        """Change sort criteria.

        Parameters are all optional, with the usual semantics for list.sort(),
        but each defaults to self's existing sort properties.  If given, cmp
        replaces self's prior comparison function, if any (you can restore the
        effect of no custom comparison by passing the built-in cmp function).
        If key is omitted, self's existing key is preserved; if it is passed as
        the empty string, any prior key is discarded and self uses values as
        they are, rather than an attribute; otherwise, key replaces self's prior
        key.  If reverse is true, it is combined with self's prior reversal
        using xor (so reverse-sorting a reverse-sorted list yields a normally
        sorted list, for example).\n"""

        if cmp is None: cmp = self.__cmp
        else: self.__cmp = cmp

        if key is None: key = self.__key
        elif key: self.__key = key
        else: key = self.__key = None

        self.__rev ^= reverse
        self.__upsort(cmp, key, self.__rev)

    def __ne(self, ind, val):
        if self.__key:
            ind, val = getattr(self[ind], self.__key), getattr(val, self.__key)
        else: ind = self[ind]

        if self.__cmp: return self.__cmp(ind, val)
        else: return cmp(ind, val)

    def __eq(self, ind, val):
        return self.__ne(ind, val) == 0

    def __lt(self, ind, val):
        ans = self.__ne(ind, val)
        if self.__rev: return ans > 0
        return ans < 0

    def __gt(self, ind, val):
        ans = self.__ne(ind, val)
        if self.__rev: return ans < 0
        return ans > 0

    def __locate(self, value):
        """Where does value belong in this list ?

        If value is equal to some entry in this list, return the index of that
        entry; otherwise, return the index at which it should be inserted.  A
        return of -1 means 'after the end'; otherwise, only valid non-negative
        indices into self are returned.\n"""
        if not self or self.__lt(-1, value): return -1
        if self.__gt(0, value): return 0
        lo, hi = 0, len(self) - 1
        while hi > 1 + lo:
            mid, ig = divmod(lo + hi, 2)
            if self.__gt(mid, value): hi = mid
            elif self.__lt(mid, value): lo = mid
            else: return mid

        return hi

    def index(self, value):
        ind = self.__locate(value)
        if ind < 0 or self.__ne(ind, value):
            raise ValueError('Not in list', value)
        return ind

    __listins = list.insert
    __listapp = list.append
    def append(self, ind, value=None):
        """Put a value into the list.

        Should be called with one argument, the value to be added to the list.
        Supports being called like list.insert(ind, value), in which case it
        silently ignores the index, ind, and uses its second argument as the
        value to insert.  Returns True if self ignores duplicates and the new
        item was a duplicate; else None.\n"""
        if value is None: value = ind
        # Insert in correctly-ordered position.
        at = self.__locate(value)
        if at < 0: self.__listapp(value)
        elif self.__unique and self.__eq(at, value): return True
        else: self.__listins(at, value)

    insert = append

    def prune(self, upto):
        """Discard all entries <= upto.

        Returns the discarded entries.\n"""
        at = self.__locate(upto)
        if at < 0:
            was = self[:]
            del self[:]
        else:
            if self.__eq(at, upto): at += 1
            was = self[:at]
            if at > 0: del self[:at]

        return was
