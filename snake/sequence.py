"""Assorted tools relating to sequences.

Classes:
  Iterable -- mix-in to enrich iterable classes with functional tools
  WrapIterable -- simple wrapper to add Iterable's methods to an iterable
  Dict -- dict with its iterators wrapped in WrapIterable
  ReadSeq -- mix-in extending Iterable to support most tuple methods
  Tuple -- mixes ReadSeq and tuple suitably
  List -- mixes ReadSeq and list suitably
  Ordered -- a List that maintains ordering of its entries

Decorator:
  iterable -- apply WrapIterable to iterators returned by a function

$Id: sequence.py,v 1.45 2009-12-23 22:12:38 eddy Exp $
"""
from study.snake.decorate import mimicking

# Decorator deploying WrapIterable (q.v., at end of this page):
@mimicking
def iterable(func):
    """Decorator to wrap the iterables returned by a function in WrapIterable.

    Takes a single function argument, func, and returns a function which is
    called as if it were func but the return from func is passed to WrapIterable
    (q.v., below) and the return from *that* is the replacement function's
    return.  When used to decorate a function that returns an iterable, this
    just ensures that the iterable has the methods of Iterable (q.v., below).

    In particular, this can be used to wrap a generator (i.e. a function that
    uses yield, instead of return; this implicitly returns an iterator over the
    values its body yields) so that the resulting iterator supports the methods
    of Iterable.  Naturally, it is used to decorate the generator methods of
    Iterable itself.\n"""
    return lambda *args, **kw: WrapIterable(func(*args, **kw))
del mimicking

class Iterable (object):
    """Mix-in class to extend iterables in some handy ways.

    Implements map, reduce, sum, product and filter as methods of the iterable;
    map and filter take arbitrarily many functions and return iterators -
    derived classes may wish to collect the yielded entries into a sequence of
    some suitable type.  The mapwith method more closely matches the built-in
    map function, but also returns an iterator (which may be worth wrapping,
    too).\n"""

    __slots__ = ()

    @iterable
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

    @staticmethod
    def __endless(seq):
        for it in seq: yield it
        while True: yield None

    @iterable
    def mapwith(self, func, *others):
        """Map self and some other iterables through a function.

        Required first parameter is a function (None is understood as a
        surrogate for lambda *args: args); subsequent positional parameters are
        arbitrarily many iterables.  Each of these other iterables is
        effectively extended with arbitrarily many None if it runs out of
        values.  For each entry in self, func is called with the entry in self
        as first parameter and each subsequent parameter drawn from the matching
        iterable; the function's return is yielded.  The result is thus similar
        to using the builtin map(self, *others), except that it returns an
        iterator over the values.\n"""

        others = map(self.__endless, others)
	if func is None: func = lambda *args: args
        for val in self:
            yield func(val, *map(lambda x: x.next(), others))

    def reduce(self, func, init=None):
        it = iter(self)
        if init is None: init = it.next()
        for p in it: init = func(init, p)
        return init

    def sum(self, zero=0, add=lambda x, y: x + y): return self.reduce(add, zero)
    def product(self, one=1, mul=lambda x, y: x * y): return self.reduce(mul, one)

    @iterable
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

    @iterable
    def enumerate(self):
        i = 0
        for it in self:
            yield i, it
            i += 1

class WrapIterable (Iterable):
    # For when you aren't defining a class to mix in with:
    def __init__(self, seq): self.__seq = seq
    def __iter__(self): return iter(self.__seq)
    def __getattr__(self, key): return getattr(self.__seq, key)

class Dict (dict):
    __iter = dict.__iter__
    __itit, __itke, __itva = dict.iteritems, dict.iterkeys, dict.itervalues
    # Can't use iterable: slot wrappers and method descriptors don't play nicely
    # with mimicking, to let it know their signatures.
    def __iter__(self,   Wrap=WrapIterable): return Wrap(self.__iter())
    def iterkeys(self,   Wrap=WrapIterable): return Wrap(self.__itke())
    def iteritems(self,  Wrap=WrapIterable): return Wrap(self.__itit())
    def itervalues(self, Wrap=WrapIterable): return Wrap(self.__itva())

class ReadSeq (Iterable):
    """Mix-in class extending Iterable to support most tuple methods.

    While Iterable is a sensible class to mix in to classes that already know
    how to behave like a sequence, ReadSeq is intended to produce as much of the
    usual sequence behaviour as possible from bare iteration.  Derived classes
    likely want to over-ride various methods (notably getitem and len), but this
    mix-in ensures most other interesting methods are available given the basics
    (and can even manage len and getitem given iterability).\n"""
    __slots__ = ()
    def __len__(self):
        i = 0
        for it in self: i += 1
        return i

    from regular import Slice
    def __getitem__(self, ind, S=Slice):
        if isinstance(ind, slice): ind = S(ind)
        try: iter(ind)
        except TypeError: pass
        else: return self.__get(ind)

        for it in self:
            if ind == 0: return it
            ind -= 1

        raise IndexError(ind)
    del Slice

    @iterable
    def __get(self, ind):
	for i in ind:
	    try: yield self[i]
	    except IndexError: pass

    def __repr__(self):
        row = []
        for it in self: row.append(repr(it))
        return self.__class__.__name__ + '((' + ', '.join(row) + '))'

    def __hash__(self):
        ans = id(self)
        for it in self: ans ^= hash(it)
        return ans

    # The default __contains__ simply iterates, testing ==

    def __eq__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if not(it == other): return False
            return True # No entry in self isn't other.

        try:
            for it in self:
                if not(it == src.next()): return False
        except StopIteration: return False # it ran out first

        try: src.next()
        except StopIteration: return True
        return False

    # NB difference between "not(x == y)" above, vs "x != y" below !

    def __ne__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if it != other: return True
            return False # Some entry in self isn't other

        try:
            for it in self:
                if it != src.next(): return True
        except StopIteration: return True # it ran out first

        try: src.next()
        except StopIteration: return False # we're equal
        return True # it was longer

    def __ge__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if not(it >= other): return False
            return True

        try:
            for it in self:
                val = src.next()
                if it > val: return True
                elif it == val: pass
                else: return False
        except StopIteration: return True # it ran out first

        try: src.next()
        except StopIteration: return True # equality
        return False

    def __le__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if not(it <= other): return False
            return True

        try:
            for it in self:
                val = src.next()
                if it < val: return True
                elif it == val: pass
                else: return False
        except StopIteration: return False # it ran out first
        return True # equality, but other may be longer

    def __lt__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if not(it < other): return False
            return True

        try:
            for it in self:
                val = src.next()
                if it < val: return True
                elif it == val: pass
                else: return False
        except StopIteration: return False # it ran out first

        try: src.next()
        except StopIteration: return False
        return True # it was longer

    def __gt__(self, other):
        try: src = iter(other)
        except TypeError:
            for it in self:
                if not(it > other): return False
            return True

        try:
            for it in self:
                val = src.next()
                if it > val: return True
                elif it == val: pass
                else: return False
        except StopIteration: return True # it ran out first
        return False # equality, but other may be longer

    @iterable
    def __mul__(self, other):
        while other > 0:
            other -= 1
            for it in self: yield it

    __rmul__ = __mul__
    @iterable
    def __add__(self, other):
        for it in self: yield it
        for it in other: yield it

    @iterable
    def __radd__(self, other):
        for it in other: yield it
        for it in self: yield it

    def index(self, val):
        i = 0
        for it in self:
            if val == it: return i
            i += 1

        raise ValueError('not in sequence', val)

    # Throw in something suitable in place of sort:
    def order(self, are=cmp):
        """Return self's indices in increasing order of matching entries.

        Takes one optional argument, defaulting to the built-in cmp: a function
        to be used for comparison. The return from this method is a Permutation
        (see study.maths.permute) of range(len(self)) in which, for each i, j in
        this range, if cmp(self[i], self[j]) < 0, i appears before j in the
        result list; if > 0, i appears after j; else, the order of i and j is
        unspecified.\n"""
        return self.__order(are)

    def __order(self, par):
        # boot-strap round the fact that permute.Permute inherits from Tuple
        from study.maths import permute
        # over-write this boot-strap implementation
        ReadSeq.__order = lambda who, are=cmp, p=permute.order: p(who[:], are)
        return permute.order(self[:], par)

    @iterable
    def best(self, n, are=cmp):
        """Selects the best n entries in self, preserving self's order.

        Required first argument, n, is the number of entries of self to retain,
        if positive; else -n is the number of entries of self to
        discard.  Optional second argument is a comparison function, are; its
        default is the built-in cmp; entries of self whose indices lie in
        self.order(are)[-n:] are kept.  The returned Iterable preserves the
        order of the entries within self; if you want them sorted, use
        self.order(are)(self)[-n:] instead.\n"""

        
        return self.enumerate().filter(lambda (k, v),
                                       ks=self.order(are)[-n:]: k in ks).map(lambda (k, v): v)

class Tuple (ReadSeq, tuple):
    """Pretend to be a tuple.

    Note that classes based on this need to over-ride __new__(), which *returns*
    the newly created object (obtained by calling base-class's __new__(), of
    course), instead of (or as well as) __init__().\n"""

    @classmethod
    def __tuple__(mode, val):
        """Pseudo-constructor.

        Takes a sequence and returns an instance of Tuple.  If a derived class
        has __new__ or __init__ taking some different parameter list, it should
        over-ride this with something suitable; if it doesn't support arithmetic
        and slicing, it should over-ride this with something that raises an
        error.  Otherwise, this uses the class of self to construct a new Tuple
        of suitable type.\n"""
        return mode(val)

    __upmul = tuple.__mul__
    def __mul__(self, other): return self.__tuple__(self.__upmul(other))
    __uprmul = tuple.__rmul__
    def __rmul__(self, other): return self.__tuple__(self.__uprmul(other))
    __upadd = ReadSeq.__add__
    def __add__(self, other): return self.__tuple__(self.__upadd(other))
    __upradd = ReadSeq.__radd__
    def __radd__(self, other): return self.__tuple__(self.__upradd(other))

    __rsget, __tpget = ReadSeq.__getitem__, tuple.__getitem__
    def __getitem__(self, key):
        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                return self.__tpget(key)

        return self.__tuple__(self.__rsget(key))

class List (ReadSeq, list): # list as base => can't use __slots__

    # Use some things from list in preference to ReadSeq:
    __init__ = list.__init__
    __hash__ = list.__hash__
    __len__ = list.__len__
    __contains__ = list.__contains__
    append = list.append
    insert = list.insert
    index = list.index

    # Fix some infelicities in list:
    try: list.__getslice__
    except AttributeError: pass
    else: # list still has this deprecated method: over-ride it !
        def __getslice__(self, i, j): return self.__getitem__(slice(i, j))

    try: list.__setslice__
    except AttributeError: pass
    else: # list still has this deprecated method: over-ride it !
        def __setslice__(self, i, j, val):
            self.__setitem__(slice(i, j), val)

    # Over-ride getitem and setitem:
    __upget = list.__getitem__
    __rsget = ReadSeq.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return an Ordered to hold it; if it's going in the
        reverse direction, suitably reverse the order.\n"""

        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                return self.__upget(key)

        return self.__list__(self.__rsget(key))

    __updel = list.__delitem__
    def __delitem__(self, key):
        try: k = iter(key)
        except TypeError:
            return self.__updel(key)

        for it in k: self.__updel[it]

    from regular import Slice
    __upset = list.__setitem__
    def __setitem__(self, key, val, S=Slice):
        try: iter(key)
        except TypeError:
            if isinstance(key, slice): key = S(key)
            else: return self.__upset(key, val)

        try:
            if key.step == 1:
                return self.__upset(key.to_slice(), val)
        except AttributeError: pass
        if len(key) != len(val):
            raise ValueError('Mismatched lengths in extended slice assignment',
                             key, val)
        src = iter(val) # TypeError if non-sequence given to assign to slice
        for it in key: self[it] = src.next()

    del Slice

    __mul = ReadSeq.__mul__
    def __mul__(self, other): return self.__list__(self.__mul(other))
    __rmul__ = __mul__

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

    __radd = ReadSeq.__radd__
    def __radd__(self, other): return self.__list__(self.__radd(other))

    __map = ReadSeq.map
    def map(self, *what): return self.__list__(self.__map(*what))

    __mapwith = ReadSeq.mapwith
    def mapwith(self, *what): return self.__list__(self.__mapwith(*what))

    @classmethod
    def __list__(mode, *what):
        """Create a new object like self, but with other init args.

        Takes the same args as list.__init__ (q.v.) but builds a new object
        of the same kind as self - derived classes should over-ride this if they
        change the signature of __init__.  Should normally be invoked indirectly
        via slicing, e.g., self[:0] or self[:].\n"""
        return mode(*what)

class Ordered (List):
    """An ordered set.

    Like a list except that you don't get to chose where in it to put each
    entry; and duplication can be allowed, silently ignored, or treated as
    error.  Entries (or their values of the selected sort-key attribute) must
    support comparison with one another; they shall be kept in increasing (or
    decreasing, if reversed) order.  The comparison may be customised in the
    same ways as are supported by the usual list.sort method; calling sort() can
    revise the customisation and .reverse() reverses it.  Slicing with a
    reversed slice order yields a reverse-ordered slice; other operations that
    would normally yield a list yield an Ordered (but see __ordered__) with the
    same sort properties as self.\n"""

    __upinit = List.__init__
    def __init__(self, val=None, reverse=False, key=None, cmp=None,
                 attr=None, unique=False):
        """Construct ordered list.

        All arguments are optional:
          val -- sequence of (or iterator over) initial entries or (default) None
          reverse -- sort in reverse order (default: False)
          key -- function to apply to each entry (or its attr attribute) to get
                 sort value or (default) None to use the value as is
          cmp -- comparison function or (default) None to use the built-in cmp
          attr -- name of attribute on which to compare or (default) None to use
                  values directly (or via key)
          unique -- ignore duplicate entries (default: False) or, if None, raise
                    ValueError on any attempted duplication.

        The middle four of these determine the sort order of the list (and lists
        obtained from it); if attr is given, this attribute of each entry is
        used in place of the entry when comparing; if key is given, it is called
        on the value (or its attribute) and the resulting value used in
        comparisons; reverse has the same effect as composing lambda x: -x after
        cmp.  If two entries compare equal in terms of these, and unique is
        true, the entry supplied later is discarded; this is rarely desirable
        when key is given.\n"""

        self.__upinit()
        self.__unique, self.__attr = unique, attr
        self.__cmp, self.__key, self.__rev = cmp, key, reverse
        if val is not None: self.extend(val)

    def __list__(self, val=None):
        return self.__ordered__(val, self.__rev, self.__key, self.__cmp,
                                self.__attr, self.__unique)

    __upget = List.__getitem__
    __rsget = ReadSeq.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return an Ordered to hold it; if it's going in the
        reverse direction, suitably reverse the order.\n"""

        try:
            if key.step is None or key.step > 0:
                raise AttributeError # simple delegation to __upget will do fine
        except AttributeError:
            return self.__upget(key)

        return self.__ordered__(self.__rsget(key),
                                not self.__rev, self.__key, self.__cmp,
                                self.__attr, self.__unique)

    @classmethod
    def __ordered__(mode, *what):
        """Create a new object like self, but with other init args.

        Takes the same args as Ordered.__init__ (q.v.) but builds a new object
        of the same kind as self - derived classes should over-ride this if they
        change the signature of __init__.  Should normally be invoked indirectly
        via slicing, e.g., self[:0] or self[:].\n"""
        return mode(*what)

    # Take this from list, not List:
    try: __upsets = list.__setslice__ # used by multiplications
    except AttributeError: __upsets = list.__setitem__

    def __setitem__(self, key, val):
        """Replace value(s) indicated by key with value(s) supplied as val.

        This is interpreted as deletion of each replaced value and insertion of
        each supplied value, potentially at a different location.\n"""

        del self[key]

        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                self.append(val)
                return

        # No need to raise ValueError if len(val) != len(key)
        for it in val: self.append(it)

    def __mul__(self, n):
        if n < 1: return self[:0]
        if self.__unique or n == 1: return self[:]
        if self.__unique is None:
            raise ValueError("Duplication", n)

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
        if self.__unique is None:
            raise ValueError("Duplication", n)

        i = len(self)
        while i > 0:
            i -= 1
            if n == 2:
                self.__listins(i, self[i])
            else:
                self.__upsets(slice(i, i), [ self[i] ] * (n - 1))

        return self

    __upsort = List.sort
    def sort(self, cmp=None, key=None, reverse=False):
        """Change sort criteria.

        Parameters are all optional, with the usual semantics for list.sort(),
        but each defaults to self's corresponding sort property.  If given, cmp
        replaces self's prior comparison function, if any (you can restore the
        effect of no custom comparison by passing the built-in cmp function). If
        key is omitted or None, self's existing key is preserved; if it is any
        other false value, any prior key is discarded and self uses values as
        they are, subject to any attribute name look-up; otherwise, key replaces
        self's prior key.  If reverse is true, it is combined with self's prior
        reversal using xor (so reverse-sorting a reverse-sorted list yields a
        normally sorted list, for example).  Since no list.sort() parameter
        matches attribute look-up (for all that key may be used to do this),
        nothing can change self's choice of delegating attribute.

        When no parameters are passed, this should normally be an expensive
        no-op; however, if entries in the list have changed in ways that affect
        their position, this should restore proper sorting.\n"""

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
	"""Compares an entry in self to a given value.

	Arguments:
	  ind -- an index into self
	  val -- a value

	Returns the result of self's defined comparison; if val is less than
	self[ind], the return is -1, if greater +1; otherwise 0.  Caller is
	responsible for deciding whether less or greater values belong earlier
	or later in the list (i.e. handling of self.__rev).\n"""

        if self.__attr:
            ind, val = getattr(self[ind], self.__attr), getattr(val, self.__attr)
        else: ind = self[ind]
        if self.__key:
            ind, val = self.__key(ind), self.__key(val)

        if self.__cmp: return self.__cmp(ind, val)
        # cmp mishandles the case where <, > and == are all false, so hand-code:
        elif ind < val: return -1
        elif ind > val: return +1
        else: return 0 # even if not(ind == val)

    def __eq(self, ind, val):
	"""True precisely if val belongs at position ind."""
        return self.__ne(ind, val) == 0

    def __lt(self, ind, val):
	"""True precisely if val belongs before position ind."""
        ans = self.__ne(ind, val)
        if self.__rev: return ans > 0
        return ans < 0

    def __gt(self, ind, val):
	"""True precisely if val belongs after position ind."""
        ans = self.__ne(ind, val)
        if self.__rev: return ans < 0
        return ans > 0

    def __locate(self, value, lo=0, hi=-1):
        """Where does value belong in this list ?

        Required argument, value, is the value whose position is to be
        determined.  Optional arguments lo (default: 0) and hi (default: -1)
        constrain the range of answers; it shall be assumed that value belongs
        at an index no later than lo or earlier than hi.

        If value is equal to some entry in this list, return the index of that
        entry; otherwise, return the index at which it should be inserted.  A
        return of -1 means 'after the end'; otherwise, only valid non-negative
        indices into self are returned.\n"""
        if not self or self.__lt(hi, value): return hi
        if self.__gt(lo, value): return 0
        if hi < lo:
            assert hi == -1
            hi += len(self)

        while hi > 1 + lo:
            mid, ig = divmod(lo + hi, 2)
            if self.__gt(mid, value): hi = mid
            elif self.__lt(mid, value): lo = mid
            else: return mid

        return hi

    def __contains__(self, value):
	"""True if self would consider value a duplicate entry, if inserted.

	Note that this happens if self has an entry that self deems neither less
	nor greater than value, even if this entry does not compare equal to
	value.\n"""
        ind = self.__locate(value)
        return not ( ind < 0 or self.__ne(ind, value) )

    def index(self, value, lo=0, hi=-1):
        """Extends and optimises list.index

        Required argument, value, is an entry to look for in this list.  Note,
	however, that the comparison self uses may be non-simple (see __ne) and
	that value shall be `found' if this comparison considers it neither less
	nor greater than some entry in the list; the entry found need not be
	equal to value (although it likely is for most sensible choices of
	comparison).

        Optional arguments lo (default: 0) and hi (default: -1) bound the range
        in which to look for it; self[lo:hi] is searched instead of the whole of
        self.  Exploits the fact that self is ordered so that the search only
        needs O(log(len(self))) instead of O(len(self)) comparisons between
        value and an entry in self.

        On failure, i.e. when the given value is not in self[lo:hi], a
        ValueError is raised, whose [0] entry is the index at which value would
        be inserted; or -1 if it would be inserted after the last element.\n"""
        ind = self.__locate(value, lo, hi)
        if ind < 0 or self.__ne(ind, value):
            raise ValueError(ind, 'Not in list', value, lo, hi)
        return ind

    __listins = List.insert
    __listapp = List.append
    def append(self, ind, value=None):
        """Put a value into the list.

        Should be called with one argument, the value to be added to the
        list.  Supports being called like list.insert(ind, value), provided
        value is not None (which would be an odd thing to add to an ordered
        list), in which case it silently ignores the index, ind, and uses its
        second argument as the value to insert.  Returns True if self ignores
        duplicates and the new item was a duplicate; else None, unless it raises
        ValueError due to rejecting the new item as a duplicate.\n"""
        if value is None: value = ind
        # Insert in correctly-ordered position.
        at = self.__locate(value)
        if at < 0: self.__listapp(value)
        elif self.__unique and self.__eq(at, value): return True
        elif self.__unique is None and self.__eq(at, value):
            raise ValueError("Duplicate", value, ind, self)
        else: self.__listins(at, value)

    insert = append

    def prune(self, upto):
        """Discard all entries that belong no later than upto.

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
