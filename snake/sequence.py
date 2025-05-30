"""Assorted tools relating to sequences.

Classes:
  Iterable -- mix-in to enrich iterable classes with functional tools
  WrapIterable -- simple wrapper to add Iterable's methods to an iterable
  ReadSeq -- mix-in extending Iterable to support most tuple methods
  Tuple -- mixes ReadSeq and tuple suitably
  Dict -- dict with its sequence and iterator methods suitably wrapped
  List -- mixes ReadSeq and list suitably
  Ordered -- a List that maintains ordering of its entries

Decorators:
  iterable -- apply WrapIterable to iterators returned by a function
  iterinstance -- apply ._iterable_ to a method's return

See study.LICENSE for copyright and license information.
"""
from study.snake.decorate import mimicking

# Decorator deploying WrapIterable (q.v., at end of this page):
@mimicking
def iterable(func):
    """Decorator to wrap the return from a function as a WrapIterable.

    Takes a single function argument, func, and returns a function which is
    called as if it were func but the return from func is wrapped as an Iterable
    (see below), using WrapIterable.  To use some more specific wrapper, see
    iterinstance, below.

    In particular, this can be used to wrap a generator (i.e. a function that
    uses yield, instead of return; this implicitly returns an iterator over
    the values its body yields) so that the resulting iterator supports the
    methods of Iterable.\n"""
    def ans(*args, **kw):
        return WrapIterable(func(*args, **kw))
    return ans

@mimicking
def iterinstance(func):
    """Decorator to wrap a method's return as a suitable Iterable.

    For use on (class and instance) methods whose return should be wrapped by
    the class's ._iterable_(), rather than by WrapIterable; only applicable if
    the class is based on Iterable, of course.  Otherwise, the same as
    @iterable, above.  Naturally, I use it to decorate the generator methods of
    Iterable itself, and of some classes based on it.\n"""
    def ans(*args, **kw):
        wrap = args[0]._iterable_
        return wrap(func(*args, **kw))
    return ans
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
    @staticmethod
    def _iterable_(what):
        """Pseudo-constructor for iterable.

        Takes one argument, an iterable (typically a generator function) to be
        packaged with the infrastructure provided by Iterable.  Derived
        classes should over-ride this with something having the right
        signature, e.g.

            @classmethod
            def _iterable_(cls, what):
                return cls(what)

        for the usual pseudo-constructor pattern (so only the class that mixes
        in Iterator usually needs to define this).  This method is mostly
        accessed vai the @iterable and @iterinstance decorators (see
        above).  This base version uses WrapIterable, since Iterator itself has no
        constructor; it's only a mix-in.  WrapIterable over-rides this version
        with the class-method quoted above.\n"""
        return WrapIterable(what)

    @iterinstance
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

    @iterinstance
    def mapwith(self, func, *others):
        """Map self and some other iterables through a function.

        Required first parameter is a function (None is understood as a
        surrogate for lambda *args: args); subsequent positional parameters
        are arbitrarily many iterables.  Each of these other iterables is
        effectively extended with arbitrarily many None if it runs out of
        values.  For each entry in self, func is called with the entry in self
        as first parameter and each subsequent parameter drawn from the
        matching iterable; the function's return is yielded.  The result is
        thus similar to using the builtin map(self, *others), except that it
        returns an iterator over the values.  Contrast with cartesian().\n"""

        others = [self.__endless(o) for o in others]
        if func is None: func = lambda *args: args
        for val in self:
            yield func(val, *[x.next() for x in others])

        # assert all(x.next() is None for x in others) ?

    def reduce(self, func, init=None, count=False):
        """Combine entries using a given function.

        Required argument, func, is a function of two args, used for combining
        entries.  Optional arguments:
          init -- initial value, combined with entries; ignored if None (default)
          count -- if true, pair number of entries in self with return value

        If init is not supplied (or is None), the first value is used in its
        placed and skipped from the remaining values.  Each func(init, value) is
        computed and used in place of init for the next value, with the final
        value of init returned.  Thus, if the iterable has just one entry and
        init is not supplied, func is never called and the single entry is
        returned.  If init is not supplied (or is None) and the iterable is
        empty, a ValueError is raised.

        A natural value to supply for init, if given, is an identity for the
        binary operator encoded by func, so that func(init, x) is x for all
        relevant x; thus init is effectively ignored when the iterable has
        values, but this identity value is returned if the sequence is
        empty.  Other values may be passed for init, for example where
        func(init, x) is a different type of value from x, an entry, and init is
        a 'seed' value that combines with the first entry to produce a value of
        the right type.

        If count is true then the return value is a twople (n, init) of the
        number n of entries seen in self with the accumulated answer that would
        have been returned otherwise.  This simplifies a common case that would
        otherwise need a non-trivial func and init.

        Example: to compute moments (averages of the first n powers), you could
        use study.maths.vector's Vector class in:
            .map(lambda x, v=Vector, r=range(n+1): v([x**i for i in r])).sum()
        and divide the result by its [0] entry; the [i] entry is then the i-th
        moment of the data iterated by self.  (Note that the n=1 case of this is
        one of the cases count simplifies so you don't need such a heavy-weight
        solution.)\n"""

        it, n = iter(self), 0
        if init is None:
            try: init = it.next()
            except StopIteration:
                raise ValueError('Reducing empty with no initial value')
            else: n = 1

        for p in it:
            init = func(init, p)
            n += 1

        if count: return n, init
        return init

    import operator
    def sum(self, zero=None, add=operator.add): return self.reduce(add, zero)
    def product(self, one=None, mul=operator.mul): return self.reduce(mul, one)

    def geomean(self, one=None, mul=operator.mul):
        try: n, prod = self.reduce(mul, one, True)
        except ValueError:
            # The product of the empty list is 1; and pow(1/0, 1) is still 1.
            return 1 if one is None else one

        return prod ** (1. / n)

    def mean(self, zero=None, add=operator.add):
        n, tot = self.reduce(add, zero, True)
        try: q, r = divmod(tot, n)
        except (TypeError, ValueError): pass
        else: # avoid making it a float when it doesn't need to be:
            if not r: return q
        return tot * 1. / n

    del operator

    @iterinstance
    def filter(self, *tests):
        """Filter self with test functions.

        Takes arbitrarily many functions (None is interpreted as the identity
        function) as positional parameters; if any of these maps an entry of
        self to a false value, that entry is ignored; each non-ignored entry
        in self is yielded, in turn.  Note that self.filter() is just
        iter(self), whereas self.filter(None) filters out false entries; there
        is no default test function.\n"""
        for p in self:
            if all((p if t is None else t(p)) for t in tests):
                yield p

    @iterinstance
    def enumerate(self):
        i = 0
        for it in self:
            yield i, it
            i += 1

    @iterable
    def contiguous(self, n=0, func=None):
        """Iterates the contiguous sub-sequences of self.

        If a first parameter is passed, it should support comparison
        with 0; if it is greater than 0 (its default), it should be
        the number of entries in each contiguous sub-sequence to be
        iterated.  Otherwise, all all contiguous sub-sequences of all
        lengths are iterated, ending with one empty sequence (in
        principle there's one (or, indeed, as many of them as you
        like) of these between each pair of entries in the list, but
        there is little value to repeating it; and putting it last
        gives the iterating code advance warning that the end is
        nigh).

        If no second parameter is passed (or if its default, None, is
        passed), each sub-sequence iterated is yielded as a tuple.
        Otherwise, the passed parameter must be a function, func: in
        this case, for each seq that would otherwise have been
        yielded, func(*seq) is yielded instead.

        For the special case n = 2, see pairs().
        """
        if n > 0:
            it = iter(self)
            seq = tuple(it.next() for i in range(n - 1))
            for entry in it: # drop first, append entry:
                seq += (entry,)
                if func is None: yield seq
                else: yield func(*seq)
                seq = seq[1:]
        else:
            seq = tuple(self)
            count = len(seq)
            for i in range(count):
                for j in range(i, count):
                    if func is None: yield seq[i, j+1]
                    else: yield func(*seq[i, j+1])
            yield seq[count: count]

    def pairs(self, func=None):
        """Iterates pairs of adjacent entries in self.

        When no parameter is passed, this is equivalent to
        zip(self[:-1], self[1:]), but avoids making two copies of
        almost the whole sequence.  If a parameter is passed, it must
        be a function taking two parameters; pairs(func) will then
        iterate the values of func(*pair) for each pair it would
        otherwise have yielded.
        """
        return self.contiguous(2, func)

    @iterinstance
    def __reversed__(self):
        seq = tuple(self)
        i = len(seq)
        while i > 0:
            i -= 1
            yield seq[i]

    def reversed(self):
        return self.__reversed__()

    @staticmethod
    def __descartes(o, r=((),)): # helper for __renee()
        """Iterate over product of o with r.

        Required argument, o, is an iterable (and not an iterator).  Optional
        argument, r, defaults to ((),), a sequence whose sole entry is the empty
        tuple; if given, it should be an iterable (it may be an iterator: it is
        fully iterated exactly once) over tuples.  For each tuple in r, this
        method yields each result of adding an entry in o to the start of the
        tuple.  Thus the default for r yields one-tuples, wrapping each entry of
        o in one such tuple.\n"""
        for em in r:
            for it in o:
                yield (it,) + em

    @classmethod
    def __renee(cls, func, one, *rest): # helper for cartesian()
        """Raw cartesian product of iterators.

        Has to be separate from .__descartes() since its recursive call to
        itself needs it to be a function, not a generator.  Is separate from
        cartesian so as to use raw tuples as keys for efficiency in the
        recursion, leaving cartesian to Tuple()ify the final results.\n"""
        if func is not None: one = func(one)
        if rest: return cls.__descartes(one, cls.__renee(func, *rest))
        return cls.__descartes(one)

    @classmethod
    def cartesian(cls, func, *whom):
        """Cartesian product on iterators.

        First argument, func, must be a callable (e.g. tuple) or None (in which
        case lambda x: x is implicitly used).  There must be at least one
        subsequent argument; func shall be called on each and must return an
        iterable that can be repeatedly iterated.  It is important that func's
        return *not* be an iterator, as it is apt to be iterated repeatedly (and
        an iterable would be exhausted after the first time).

        Let seq refer to the returns from func, called on each subsequent
        argument; each yield of the returned iterator is a Tuple res for which
        res[i] is an entry in seq[i] (so res's length is the number of arguments
        following func); and every such tuple arises.  Thus the iterable
        returned by this method (via ._iterable_(), so that derived classes can
        tweak its details) has, as number of entries, the product of the numbers
        of entries in the various iterables in seq.

        Example: Iterable.cartesian(range, 4, 4, 4, 4, 4) yields every tuple, of
        length five (there were five 4s), whose entries are in range(4);
        Iterable.cartesian(range, *((n,)*m)) yields every tuple, of length m,
        whose entries are drawn from range(n).\n"""
        return cls._iterable_(cls.__renee(func, *whom)).map(Tuple)

class WrapIterable (Iterable):
    # For when you aren't defining a class to mix in with:
    def __iter__(self): return self
    def __init__(self, seq):
        self.__get = lambda k, s=seq: getattr(s, k)
        self.__seq = iter(seq)
    def __getattr__(self, key): return self.__get(key)
    def next(self): return self.__seq.next()
    @classmethod
    def _iterable_(cls, what): return cls(what)

class ReadSeq (Iterable):
    """Mix-in class extending Iterable to support most tuple methods.

    While Iterable is a sensible class to mix in to classes that already know
    how to behave like a sequence, ReadSeq is intended to produce as much of
    the usual sequence behaviour as possible from bare iteration.  Derived
    classes likely want to over-ride various methods (notably getitem and
    len), but this mix-in ensures most other interesting methods are available
    given the basics (and can even manage len and getitem given
    iterability).\n"""
    __slots__ = ()
    def __len__(self):
        i = 0
        for it in self: i += 1
        return i

    seq = [] # lazy access to regular.Slice; else we get an import cycle ...
    @staticmethod
    def _isslice_(item, cache=seq):
        if not cache:
            from study.snake.regular import Slice
            cache.append(Slice)

        return isinstance(item, cache[0])

    @staticmethod
    def _asslice_(item, cache=seq):
        if not cache:
            from study.snake.regular import Slice
            cache.append(Slice)

        return cache[0](item)
    del seq

    def __getitem__(self, ind):
        if isinstance(ind, slice): ind = self._asslice_(ind)
        try: iter(ind)
        except TypeError: pass
        else: return self.__get(ind)

        for i, it in enumerate(self):
            if ind == i: return it

        raise IndexError(ind)

    @iterinstance
    def __get(self, ind):
        for i in ind:
            try: yield self[i]
            except IndexError:
                # A [:]'s slice has become a Slice(0, maxint), which isn't a
                # happy thing to iterate, merely to ignore the out-of-bounds
                # entries.  To support arbitrary sequences as ind, or a slice
                # that starts outside self's range but works its way in, yet
                # avoid spinning on maxint, apply a Slice-specific check:
                if self._isslice_(ind):
                    if ind.step > 0:
                        if i > len(self): break
                    elif ind.step < 0:
                        if i < 0: break
                    else: break # not getting any different i hereafter !

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
        return True # equality, or other is longer so greater

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

    @iterinstance
    def __mul__(self, other):
        try: other - 1
        except TypeError:
            for it in self.cartesian(None, other):
                yield it
        else: # numeric other
            while other > 0:
                other -= 1
                for it in self: yield it

    __rmul__ = __mul__
    @iterinstance
    def __add__(self, other):
        for it in self: yield it
        for it in other: yield it

    @iterinstance
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
    def order(self, cmp=cmp, key=None, reverse=False):
        """Return self's indices in increasing order of matching entries.

        Takes the same optional arguments as the built-in sorted() and the
        list.sort() method:
          cmp -- a comparison function
          key -- a transformation of inputs
          reverse -- whether to reverse the order

        The return from this method is a Permutation (see study.maths.permute)
        of range(len(self)) in which, for each i, j in this range, if
        cmp(key(self[i]), key(self[j])) < 0, i appears before j in the result
        list; if > 0, i appears after j; else, the order of i and j is
        unspecified; except that, if reverse is true, the reverse of this
        ordering is used instead.\n"""
        return self.__order(cmp, key, reverse)

    def __order(self, par, key, rev):
        # boot-strap round the fact that permute.Permutation inherits from Tuple
        from study.maths.permute import order
        # over-write this boot-strap implementation
        ReadSeq.__order = order
        return order(tuple(self), par, key, rev)

    def sorted(self, cmp=cmp, key=None, reverse=False):
        return self.order(cmp, key, reverse)(tuple(self))

    @iterinstance
    def best(self, n, par=cmp):
        """Selects the best n entries in self, preserving self's order.

        Required first argument, n, is the number of entries of self to
        retain, if positive; else -n is the number of entries of self to
        discard.  Optional second argument is a comparison function, par; its
        default is the built-in cmp; entries of self whose indices lie in
        self.order(par)[-n:] are kept.  The returned Iterable preserves the
        order of the entries within self; if you want them sorted, use
        self.sorted(par)[-n:] instead.\n"""

        ks = self.order(par)[-n:]
        return (v for k, v in self.enumerate() if k in ks)

from study.cache.property import lazyprop, Cached
class ReadOnlySeq (ReadSeq, Cached):
    @lazyprop
    def min(self): return min(self)
    @lazyprop
    def max(self): return max(self)
    __upmean, __upgeom = ReadSeq.mean, ReadSeq.geomean
    @lazyprop
    def mean(self): return self.__upmean()
    @lazyprop
    def geomean(self): return self.__upgeom()

    # Turn some methods of ReadSeq into (lazy) properties:
    __order = ReadSeq.order
    @lazyprop
    def order(self, lazy=lazyprop): return self.__order()
    @lazyprop
    def sorted(self): return self.order.permute(self)

del lazyprop, Cached

class Tuple (ReadOnlySeq, tuple):
    """Repackage tuple.

    Note that classes based on this need to over-ride __new__() instead of (or
    as well as) __init__().  Remember that __new__() is automagically an
    @staticmethod, taking the class it's instantiating as first argument (as if
    it were an @classmethod); and it *returns* the newly created object -
    obtained, typically, by calling the __new__() of some base of the class
    passed as first argument.\n"""

    @classmethod
    def _tuple_(cls, val):
        """Pseudo-constructor.

        Takes a sequence and returns an instance of Tuple.  If a derived class
        has __new__ or __init__ with a different signature, it should over-ride
        this with something suitable; if it doesn't support arithmetic and
        slicing, it should over-ride this with something that raises an
        error.  Otherwise, this uses the class on (an instance of) which it's
        called to construct a new Tuple of suitable type.\n"""
        return cls(val)

    # Fix an infelicity in tuple:
    try: tuple.__getslice__
    except AttributeError: pass
    else: # tuple still has this deprecated method: over-ride it !
        def __getslice__(self, i, j): return self.__getitem__(slice(i, j))

    # Over-ride item access:
    __upget = tuple.__getitem__
    __rsget = ReadOnlySeq.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return the results in a Tuple.\n"""

        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                return self.__upget(key)

        return self._tuple_(self.__rsget(key))

    @classmethod
    def _iterable_(cls, what): return cls._tuple_(what)
    __hash__ = tuple.__hash__

    __upmul = tuple.__mul__
    def __mul__(self, other): return self._tuple_(self.__upmul(other))
    __uprmul = tuple.__rmul__
    def __rmul__(self, other): return self._tuple_(self.__uprmul(other))
    __upadd = ReadSeq.__add__
    def __add__(self, other): return self._tuple_(self.__upadd(other))
    __upradd = ReadSeq.__radd__
    def __radd__(self, other): return self._tuple_(self.__upradd(other))

    __rsget, __tpget = ReadSeq.__getitem__, tuple.__getitem__
    def __getitem__(self, key):
        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                return self.__tpget(key)

        return self._tuple_(self.__rsget(key))

class Dict (dict):
    # Can't use iterable: slot wrappers and method descriptors don't play
    # nicely with mimicking, to let it know their signatures.
    __iter = dict.__iter__
    def __iter__(self,   Wrap=WrapIterable): return Wrap(self.__iter())

    __itit, __itke, __itva = dict.iteritems, dict.iterkeys, dict.itervalues
    def iterkeys(self,   Wrap=WrapIterable): return Wrap(self.__itke())
    def iteritems(self,  Wrap=WrapIterable): return Wrap(self.__itit())
    def itervalues(self, Wrap=WrapIterable): return Wrap(self.__itva())

    __sqit, __sqke, __sqva = dict.items, dict.keys, dict.values
    def keys(self,   Wrap=Tuple): return Wrap(self.__sqke())
    def items(self,  Wrap=Tuple): return Wrap(self.__sqit())
    def values(self, Wrap=Tuple): return Wrap(self.__sqva())

    @classmethod
    def UsingDefault(cls, passkey, gen, *args, **kw):
        """A derived class with lazily-created entries for all keys.

        An instance of the returned class will remember any value set for a
        given key but, when asked for a key that hasn't been set, will lazily
        construct a value for it just in time to be used.  This effectively
        makes every bok[key] behave as bok.setdefault(key, ...), but saves the
        need to actually construct the default value (in order to pass it) when
        the key is already present in bok.  The parameters passed to this class
        method are a recipe for how to construct the default value (see below).

        For example, bok = Dict.UsingDefault(False, set)() will let you simply
        do bok[k].add(member) where you would otherwise need bok.setdefault(k,
        set()).add(member), wastefully creating a set() that'll be unused on
        all but the first call to it.  Thus the common pattern of grouping
        entries in some iterable by equality of some function applied can be
        written as:

        def group_by(seq, func, Holder = Dict.UsingDefault(False, set)):
            bok = Holder()
            for member in iterable:
                bok[func(member)].add(member)
            return Dict((k, frozenset(v)) for k, v in bok.items())

        If passkey is false, the default value for an unknown key is gen(*args,
        **kw), independent of the key.  If passkey is true, the key for which
        the value is to be used will be prepended to args (or passed as sole
        positional parameter if args is empty).

        For consistency, this makes deleting a non-existent key a no-op, as if
        the key's value were lazily created just in time to be deleted, instead
        of raising a KeyError.  This remains true even if lazily creating the
        entry for the key would have raised some exception.\n"""
        class Dict(cls): pass
        Dict.__asdefault = gen, args, kw, passkey
        return Dict
    __asdefault = ()

    __upget, __updel = dict.__getitem__, dict.__delitem__
    def __getitem__(self, key):
        try: return self.__upget(key)
        except KeyError:
            if not self.__asdefault: raise
            gen, args, kw, passkey = self.__asdefault
            ans = gen(*((key,) + args), **kw) if passkey else gen(*args, **kw)
            self[key] = ans
            return ans
    def __delitem__(self, key):
        try: return self.__updel(key)
        except KeyError:
            if not self.__asdefault: raise

    @classmethod
    def _iterdict_(cls, what=()):
        """Pseudo-constructor for derived classes to over-ride.

        Where this class's methods need a new instance, they use this method
        to create it; thus, by default, any derived class's use of those
        methods will return instances of the derived class.  Derived classes
        are encouraged to use this method likewise, when then need to
        construct new instances, for the sake of classes derived from
        *them*.  If a derived class has a different constructor signature, or
        is for some other reason unsuitable for use as the source of new
        instances, it can over-ride this method with something suitable.\n"""
        return cls(what)

    __upcopy = dict.copy # returns a dict, even when used via a derived class
    def copy(self): return self._iterdict_(self.__upcopy())

    @staticmethod
    def __unterleave(args, T=Tuple):
        assert all(len(it) == 2 for it in args)
        return tuple(T(kv) for kv in zip(*args))

    @classmethod
    def cartesian(cls, *args):
        """Cartesian product of dictionaries.

        Each argument should be a mapping.  With args as the sequence of such
        arguments, this method returns an Iterator over (key, val) pairs, in
        which key and val are Tuples with args[i][key[i]] == val[i] for each
        valid index i into args.  So each entry in key is a key of the
        corresponding mapping in args; and this mapping maps the given key to
        the corresponding entry in val.

        Passing the result to self._iterdict_() will get you a suitable mapping
        object with these keys and values; that isn't done here, since you're
        likely to want, first, to transform the key or value tuples.  For
        example, if the mappings represent vectors, mapping names of components
        to values thereof, their tensor product would be obtained by mapping
        each (key, val) to (key, val.product())\n"""
        return Tuple.cartesian(tuple, *[o.items() for o in args]
                               ).map(cls.__unterleave)

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

    # Over-ride item access:
    __upget = list.__getitem__
    __rsget = ReadSeq.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return the results in a List.\n"""

        try: iter(key)
        except TypeError:
            if not isinstance(key, slice):
                return self.__upget(key)

        return self._list_(self.__rsget(key))

    __updel = list.__delitem__
    def __delitem__(self, key):
        try: k = iter(key)
        except TypeError:
            return self.__updel(key)

        for it in k: self.__updel[it]

    __upset = list.__setitem__
    def __setitem__(self, key, val):
        try: iter(key)
        except TypeError:
            if isinstance(key, slice): key = self._asslice_(key)
            else: return self.__upset(key, val)

        try:
            if key.step == 1:
                return self.__upset(key.to_slice(), val)
        except AttributeError: pass
        if len(key) != len(val):
            raise ValueError(
                'Mismatched lengths in extended slice assignment', key, val)
        src = iter(val) # TypeError if non-sequence given to assign to slice
        for it in key: self[it] = src.next()

    __mul = ReadSeq.__mul__
    def __mul__(self, other): return self._list_(self.__mul(other))
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
    def __radd__(self, other): return self._list_(self.__radd(other))

    __map = ReadSeq.map
    def map(self, *what): return self._list_(self.__map(*what))

    __mapwith = ReadSeq.mapwith
    def mapwith(self, *what): return self._list_(self.__mapwith(*what))

    @classmethod
    def _list_(cls, *what):
        """Create a new object like self, but with other init args.

        Takes the same args as list.__init__ (q.v.) but builds a new object of
        the same kind as self - derived classes should over-ride this if they
        change the signature of __init__.  Should normally be invoked
        indirectly via slicing, e.g., self[:0] or self[:].\n"""
        return cls(*what)

class Ordered (List):
    """An ordered set.

    Like a list except that you don't get to chose where in it to put each
    entry; and duplication can be allowed, silently ignored, or treated as
    error.  Entries (or their values of the selected sort-key attribute) must
    support comparison with one another; they shall be kept in increasing (or
    decreasing, if reversed) order.  The comparison may be customised in the
    same ways as are supported by the usual list.sort method; calling sort()
    can revise the customisation and .reverse() reverses it.  Slicing with a
    reversed slice order yields a reverse-ordered slice; other operations that
    would normally yield a list yield an Ordered (but see _ordered_) with
    the same sort properties as self.\n"""

    __upinit = List.__init__
    def __init__(self, val=None, reverse=False, key=None, cmp=None,
                 attr=None, unique=False):
        """Construct ordered list.

        All arguments are optional:

          val -- sequence of (or iterator over) initial entries or (default)
                 None
          reverse -- sort in reverse order (default: False)
          key -- function to apply to each entry (or its attr attribute) to
                 get sort value or (default) None to use the value as is
          cmp -- comparison function or (default) None to use the built-in cmp
          attr -- name of attribute on which to compare or (default) None to
                  use values directly (or via key)
          unique -- ignore duplicate entries (default: False) or, if None,
                    raise ValueError on any attempted duplication.

        The middle four of these determine the sort order of the list (and
        lists obtained from it); if attr is given, this attribute of each
        entry is used in place of the entry when comparing; if key is given,
        it is called on the value (or its attribute) and the resulting value
        used in comparisons; reverse has the same effect as composing lambda
        x: -x after cmp.  If two entries compare equal in terms of these, and
        unique is true, the entry supplied later is discarded; this is rarely
        desirable when key is given.\n"""

        self.__upinit()
        self.__unique, self.__attr = unique, attr
        self.__cmp, self.__key, self.__rev = cmp, key, reverse
        if val is not None: self.extend(val)

    def _list_(self, val=None):
        return self._ordered_(val, self.__rev, self.__key, self.__cmp,
                                self.__attr, self.__unique)

    __upget = List.__getitem__
    __rsget = ReadSeq.__getitem__
    def __getitem__(self, key):
        """Get value(s) indicated by key.

        If key is a slice, return an Ordered to hold it; if it's going in the
        reverse direction, suitably reverse the order.\n"""

        try:
            # If key has no .step, treat as plain value:
            if key.step is None or key.step > 0:
                # Simple delegation to __upget will do fine:
                raise AttributeError
        except AttributeError:
            return self.__upget(key)

        return self._ordered_(self.__rsget(key),
                                not self.__rev, self.__key, self.__cmp,
                                self.__attr, self.__unique)

    @classmethod
    def _ordered_(cls, *what):
        """Create a new object like self, but with other init args.

        Takes the same args as Ordered.__init__ (q.v.) but builds a new object
        of the same kind as self - derived classes should over-ride this if
        they change the signature of __init__.  Should normally be invoked
        indirectly via slicing, e.g., self[:0] or self[:].\n"""
        return cls(*what)

    # Take this from list, not List:
    try: __upsets = list.__setslice__ # used by multiplications
    except AttributeError: __upsets = list.__setitem__

    def __setitem__(self, key, val):
        """Replace value(s) indicated by key with value(s) supplied as val.

        This is interpreted as deletion of each replaced value and insertion
        of each supplied value, potentially at a different location.\n"""

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

    def sorted(self, cmp=None, key=None, reverse=False, attr=None, unique=()):
        """Return a copy of self with changed sorting criteria.

        Arguments are all optional, in the order cmp, key, reverse, attr,
        unique, or passed as keywords in any order, with the same meanings and
        defaults as for Ordered.__init__ (q.v.), aside from:
         * reverse is understood relative to self (so false, the default,
           means to use the same as self and true means to use the reverse of
           self, which shall be forward ordering if self is reversed); and
         * for cmp, key or attr, passing None (their default) means to use
           self's corresponding datum; any other false value means the same as
           None did for the constructor.
         * unique's default is (), which means to use self's value; all other
           false values mean the same as they did for the constructor, notably
           including its special handling of None.

        Thus, by default, self.sorted() returns a copy of self; but any of
        self's constructor parameters can be overridden in the copy.  Compare
        self.sort(), which re-sorts self (returning None) and can't change
        attr or unique.\n"""

        if cmp is None: cmp = self.__cmp
        elif not cmp: cmp = None

        if key is None: key = self.__key
        elif not key: key = None

        if self.__reverse: reverse = not reverse

        if attr is None: attr = self.__attr
        elif not attr: attr = None

        if unique is (): unique = self.__unique

        return self._ordered_(self, reverse, key, cmp, attr, unique)

    def __reversed__(self):
        return self.sorted(reverse=True)

    __upsort = List.sort
    def sort(self, cmp=None, key=None, reverse=False):
        """Change sort criteria.

        Parameters are all optional, with the usual semantics for list.sort(),
        but each defaults to self's corresponding sort property.  If given,
        cmp replaces self's prior comparison function, if any (you can restore
        the effect of no custom comparison by passing the built-in cmp
        function).  If key is omitted or None, self's existing key is
        preserved; if it is any other false value, any prior key is discarded
        and self uses values as they are, subject to any attribute name
        look-up; otherwise, key replaces self's prior key.  The meaning of
        reverse is relative to self, so it is combined with self's prior
        reversal using xor (so reverse-sorting a reverse-sorted list yields a
        normally sorted list, for example).  Since no list.sort() parameter
        matches attribute look-up (for all that key may be used to do this),
        nothing can change self's choice of delegating attribute.

        When no parameters are passed, this should normally be an expensive
        no-op; however, if entries in the list have changed in ways that
        affect their position, this should restore proper sorting.  Compare
        self.sorted(), which returns a copy of self with changed sort criteria
        and can override the attr and unique passed to the constructor.\n"""

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

        attr = self.__attr
        if attr: ind, val = getattr(self[ind], attr), getattr(val, attr)
        else: ind = self[ind]
        key = self.__key
        if key: ind, val = key(ind), key(val)

        if self.__cmp: return self.__cmp(ind, val)
        # When <, > and == are all false, cmp() treats as if > were true;
        # hand-code so as to treat that case as if == were true, instead:
        elif ind < val: return -1
        elif ind > val: return +1
        return 0 # even if not(ind == val)

    def __eq(self, ind, val):
        """True precisely if val belongs at position ind."""
        return self.__ne(ind, val) == 0

    def __lt(self, ind, val):
        """True precisely if val belongs after position ind."""
        ans = self.__ne(ind, val)
        if self.__rev: return ans > 0
        return ans < 0

    def __gt(self, ind, val):
        """True precisely if val belongs no later than position ind."""
        ans = self.__ne(ind, val)
        if self.__rev: return ans < 0
        return ans > 0

    def __locate(self, value, lo=0, hi=-1):
        """Where does value belong in this list ?

        Required argument, value, is the value whose position is to be
        determined.  Optional arguments lo (default: 0) and hi (default: -1)
        constrain the range of answers; it shall be assumed that value belongs
        at an index neither later than lo nor earlier than hi.

        If value is equal to some entry in this list, return the index of that
        entry; otherwise, return the index at which it should be inserted.  A
        return of -1 means 'after the end'; otherwise, only valid non-negative
        indices into self are returned.\n"""
        if not self or self.__lt(hi, value): return hi
        if self.__gt(lo, value): return 0
        if hi < lo:
            assert hi == -1
            hi += len(self)

        while hi > lo:
            mid, ig = divmod(lo + hi, 2)
            if self.__gt(mid, value): hi = mid
            elif self.__lt(mid, value): lo = mid + 1
            else: return mid

        return hi

    def __contains__(self, value):
        """True if self would consider value a duplicate entry, if inserted.

        Note that this happens if self has an entry that self deems neither
        less nor greater than value, even if this entry does not compare equal
        to value.\n"""
        ind = self.__locate(value)
        return not ( ind < 0 or self.__ne(ind, value) )

    def index(self, value, lo=0, hi=-1):
        """Extends and optimises list.index

        Required argument, value, is an entry to look for in this list.  Note,
        however, that the comparison self uses may be non-simple (see __ne)
        and that value shall be `found' if this comparison considers it
        neither less nor greater than some entry in the list; the entry found
        need not be equal to value (although it likely is for most sensible
        choices of comparison).

        Optional arguments lo (default: 0) and hi (default: -1) bound the
        range in which to look for it; self[lo:hi] is searched instead of the
        whole of self.  Exploits the fact that self is ordered so that the
        search only needs O(log(len(self))) instead of O(len(self))
        comparisons between value and an entry in self.

        On failure, i.e. when the given value is not in self[lo:hi], a
        ValueError is raised, whose [0] entry is the index at which value
        would be inserted; or -1 if it would be inserted after the last
        element.\n"""
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
        duplicates and the new item was a duplicate; else None, unless it
        raises ValueError due to rejecting the new item as a duplicate.\n"""
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
