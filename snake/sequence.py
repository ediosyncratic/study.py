"""Assorted classes relating to sequences.

$Id: sequence.py,v 1.21 2008-06-28 11:45:34 eddy Exp $
"""

class Tuple (object):
    """Pretend to be a tuple.

    Infuriatingly, if a class inherits from tuple, whatever you pass to its
    constructor is passed to tuple's constructor (which barfs if there are too
    many arguments), before your __init__ can do anything about it; and __init__
    can't usefully call tuple.__init__ later, to supply what you wanted it to
    get.\n""" # How messed up is that ?

    __slots__ = ('__tuple',)
    def __init__(self, vals=()): self.__tuple = tuple(vals)
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

    from regular import Slice
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
        Tuple.__order = order # over-writing this boot-strap implementation
        return permute.order(self.__tuple, par)

class Iterable (object):
    """Mix-in class to extend iterables in some handy ways.

    Implements map, reduce, sum, product and filter as methods of the iterable;
    map and filter return iterators - derived classes may wish to collect the
    yielded entries into a sequence of some suitable type.\n"""

    __slots__ = ()
    def map(self, *funcs):
        """Map self through a sequence of functions.

        Takes arbitrarily many functions (None is interpreted as the identity
        function) as positional parameters; calls the first on each entry in
        self, passing the output of each as input to the next and yields
        whatever results at the end of the chain.\n"""
        for p in self:
            for f in funcs:
                if f is not None: p = f(p)
            yield p

        raise StopIteration

    def reduce(self, func, init=0):
        for p in self:
            init = func(init, p)
        return init

    def sum(self): return self.reduce(lambda x, y: x + y)
    def product(self): return self.reduce(lambda x, y: x * y, 1)

    def filter(self, *tests):
        """Filter self with test functions.

        Takes arbitrarily many functions (None is interpreted as the identity
        function) as positional parameters; if any of these maps an entry of
        self to a false value, that entry is ignored; each non-ignored entry in
        self is yielded, in turn.  Note that self.filter() is just iter(self),
        whereas self.filter(None) filters out false entries; there is no default
        test function.\n"""
        for p in self:
            for t in tests:
                if t is None:
                    if not p: break
                elif not t(p): break
            else:
                yield p

        raise StopIteration

    # The default __contains__ simply iterates, testing ==

class WrapIterable (Iterable):
    def __init__(self, seq): self.__seq = seq
    def __iter__(self): return iter(self.__seq)
    def __getattr__(self, key): return getattr(self.__seq, key)

class Ordered (Iterable, list): # list as base => can't use __slots__
    """An ordered set.

    Like a list except that you don't get to chose where in it to put each
    entry; and duplication can be silently ignored.  Entries (or their values of
    the selected sort-key attribute) must support comparison with one another;
    they shall be kept in increasing (or decreasing, if reversed) order.  The
    comparison may be customised in the same ways as are supported by the usual
    list.sort method; calling sort() can revise the customisation and .reverse()
    reverses it.  Slicing with a reversed slice order yields a reverse-ordered
    slice; other operations that would normally yield a list yield an Ordered
    (but see _ordered_) with the same sort properties as self.\n"""

    __upinit = list.__init__
    def __init__(self, val=None, reverse=False, key=None, cmp=None,
                 attr=None, unique=False):
        """Construct ordered list.

        All arguments are optional:
          val -- sequence of (or iterator over) initial entries or (default) None
          reverse -- sort in reverse order (default: False)
          key -- function to apply to each entry to get sort value or (default)
                 None to use the value as is
          cmp -- comparison function or (default) None to use the built-in cmp
          attr -- name of attribute on which to compare or (default) None to use
                  values directly
          unique -- ignore duplicate entries (default: False)

        The last three determine the sort order of the list (and lists obtained
        from it); if key is given, this attribute of each entry is used in place
        of the entry when comparing; reverse has the same effect as composing
        lambda x: -x after cmp.  If two entries compare equal in terms of these,
        and unique is true, one of the entries is discarded; this is rarely
        desirable when key is given.\n"""
        self.__upinit()
        self.__unique, self.__attr = unique, attr
        self.__cmp, self.__key, self.__rev = cmp, key, reverse
        if val is not None: self.extend(val)

    __upget = list.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return an Ordered to hold it; if it's going in the
        reverse direction, suitably reverse the order.\n"""
        if isinstance(key, slice):
            if key.step is not None and key.step < 0: rev = not self.__rev
            else: rev = self.__rev
            return self._ordered_(self.__upget(key), rev, self.__key, self.__cmp,
                                  self.__attr, self.__unique)

        return self.__upget(key)

    __reduce = Iterable.reduce
    def reduce(self, *what):
        return self._ordered_(self.__reduce(*what),
                              self.__rev, self.__key, self.__cmp,
                              self.__attr, self.__unique)
    __map = Iterable.map
    def map(self, *what):
        return self._ordered_(self.__map(*what),
                              self.__rev, self.__key, self.__cmp,
                              self.__attr, self.__unique)

    def _ordered_(self, *what):
        """Create a new object like self, but with other init args.

        Takes the same args as Ordered.__init__ (q.v.) but builds a new object
        of the same kind as self - derived classes should over-ride this if they
        change the signature of __init__.  Should normally be invoked indirectly
        via slicing, e.g., self[:0] or self[:].\n"""
        return self.__class__(*what)

    try: __upsets = list.__setslice__ # used by multiplications
    except AttributeError: __upsets = list.__setitem__
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

    def __add__(self, seq):
        ans = self[:]
        ans.extend(seq)
        return ans
    __radd__ = __add__
    def extend(self, seq):
        for it in seq: self.append(it)
    def __iadd__(self, seq):
        self.extend(seq)
        return self

    def __mul__(self, n):
        if n < 1: return self[:0]
        if self.__unique or n == 1: return self[:]

        ans, i = self[:0], len(self)
        while i > 0:
            i -= 1
            ans.__upsets(slice(0, 0), [ self[i] ] * n)

        return ans

    __rmul__ = __mul__
    def __imul__(self, n):
        if n < 1:
            self[:] = []
            return
        if self.__unique or n == 1: return

        i = len(self)
        while i > 0:
            i -= 1
            if n == 2:
                self.__listins(i, self[i])
            else:
                self.__upsets(slice(i, i), [ self[i] ] * (n - 1))

        return self

    __upsort = list.sort
    def sort(self, cmp=None, key=None, reverse=False):
        """Change sort criteria.

        Parameters are all optional, with the usual semantics for list.sort(),
        but each defaults to self's corresponding sort property.  If given, cmp
        replaces self's prior comparison function, if any (you can restore the
        effect of no custom comparison by passing the built-in cmp function).
        If key is omitted, self's existing key is preserved; if it is passed as
        None, any prior key is discarded and self uses values as they are,
        subject to any attribute name look-up; otherwise, key replaces self's
        prior key.  If reverse is true, it is combined with self's prior
        reversal using xor (so reverse-sorting a reverse-sorted list yields a
        normally sorted list, for example).  Since no list.sort() parameter
        matches attribute look-up (for all that key may be used to do this),
        nothing can change self's choice of delegating attribute.\n"""

        if cmp is None: cmp = self.__cmp
        else: self.__cmp = cmp

        if key is None: key = self.__key
        elif key: self.__key = key
        else: key = self.__key = None

        if self.__attr is not None:
            if key is None: key = lambda x, a=self.__attr: getattr(x, a)
            else: key = lambda x, a=self.__attr, k=key: k(getattr(x, a))

        self.__rev ^= reverse
        self.__upsort(cmp, key, self.__rev)

    def reverse(self):
        self.sort(reverse=True)

    def __ne(self, ind, val):
        if self.__attr:
            ind, val = getattr(self[ind], self.__attr), getattr(val, self.__attr)
        else: ind = self[ind]
        if self.__key:
            ind, val = self.__key(ind), self.__key(val)

        if self.__cmp: return self.__cmp(ind, val)
        # cmp mishandles the case where <, > and == are all false, so hand-code:
        elif ind < val: return -1
        elif ind > val: reutrn +1
        else: return 0 # even if not(ind == val)

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

    def __contains__(self, value):
        ind = self.__locate(value)
        return not ( ind < 0 or self.__ne(ind, value) )

    def index(self, value):
        ind = self.__locate(value)
        if ind < 0 or self.__ne(ind, value):
            raise ValueError('Not in list', value, ind)
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
