"""Cached weak references.

In various contexts, one can compute a datum (entry in a list, value of a
mapping's key, attribute of an object) which it's expensive to compute so
usually worth remembering once it's been computed; but worth being willing to
let the interpreter forget if it gets short on memory.  In these situations, a
weak reference is usually the answer.

Provides:
  WeakTuple -- behaves like a tuple, cacheing returns from a function mapping
               index to entries.
  weakprop -- recurseprop using propstore to cache weakrefs to attribute values
  weakattr -- combines weakprop and dictattr
"""

from study.snake.sequence import ReadSeq
class WeakTuple (ReadSeq):
    """Read-only sequence of weak references.

    Packages a function, taking an index into the sequence as sole parameter,
    as a read-only sequence, cacheing the values using weakref.  Derived
    classes should typically implement __len__, at least.\n"""
    __upinit = ReadSeq.__init__
    def __init__(self, getter):
        """Package getter as a tuple-like sequence.

        Single argument, getter, shall be called with a single parameter and
        should return the entry desired at that index into the sequence; it
        should raise IndexError if the parameter is not a suitable
        index.  Later calls with the same input should produce an equivalent
        response.\n"""
        self.__seq, self.__get = [], getter

    import weakref
    __upget = ReadSeq.__getitem__
    def __getitem__(self, key, ref=weakref.ref, lost = lambda : None):
        try: f = self.__seq[key]
        except IndexError: ans = None
        else: ans = val()

        if ans is None:
            ans = self.__get(key) # may raise IndexError
            while len(self.__seq) <= key:
                self.__seq.append(lost)
            self.__seq[key] = ref(ans)

        return ans
    del weakref
del ReadSeq

# TODO: WeakMapping

from study.snake.property import recurseprop
from study.cache.property import propstore
from study.snake import decorate
class weakprop (propstore, recurseprop):
    """Weakly-referenced attribute look-up.

    Sub-classes recurseprop and propstore; uses the latter's cache to store a
    weakref to each attribute value.  This ensures that the attribute is
    always available, as if it were never garbabe-collected, while allowing
    for it to be garbage-collected when not actively in use.  The referenced
    value is retrieved automatically from the weakref when available; else it
    is recomputed (and a fresh weakref cached).

    Can only be used for attributes whose values are of types to which
    weakrefs are permitted; in particular, the value cannot be a tuple (or an
    instance of a class based on tuple).

    See also: classmethod .mutual() returning a decorator, for weak properties
    with automatic link-back from the property to the object.\n"""

    import weakref
    __upget = recurseprop.__get__
    @decorate.overriding(__upget)
    def __get__(self, obj, cls=None, ref=weakref.ref):
        bok = self.cache(obj)
        try: f = bok[self]
        except KeyError: ans = None
        else: ans = f()

        if ans is None:
            ans = self.__upget(obj)
            bok[self] = ref(ans)

        return ans
    del weakref

    assert propstore.__init__.im_func is recurseprop.__init__.im_func
    class mutual (object):
        """Mix-in base-class to handle mutual()'s fiddliness.

        One could go further and deal with an arbitrarily long cycle of
        properties, looping back to the original object, as in:
            x is x.a.b.c.d is x.b.c.d.a is x.c.d.a.b is x.d.a.b.c;
        but I can't think of a concrete use-case !

        It's also hard to see how this might be implemented - unless each has
        a paired property, at which point use of pairs should suffice anyway -
        without evaluating the whole of the rest of the loop when evaluating
        any member (i.e. when asked for x.a, also compute and record x.a.b and
        x.a.b.c so as to mark the last's .d as being x), so as to be able to
        close the loop back to the object we started from.  However, even
        inlining the methods below into __back()'s class, I failed to work out
        how __get__/noteback can compute the onwards attributes round the
        loop.  Perhaps it could overtly call __get__ with an extra parameter
        on the successor properties.\n"""

        import weakref
        def __init__(self, check, pair, ref=weakref.ref):
            if pair is None:
                if check is not None: self.__check = check
            else:
                try: f = pair.__check
                except AttributeError: pass
                else: self.__check = f
                if check is not None: pair.__check = check

                self.__partner = ref(pair)
                try: f = pair.__partner
                except AttributeErrror: back = None
                else: back = f()
                if back is None: # typically it is
                    pair.__partner = ref(self)

        def __get__(self, src, val, bok, ref=weakref.ref):
            # Not usable as a getter; only named __get__ to limit
            # namespace pollution.

            try: f = self.__partner
            except AttributeError: pair = self
            else: pair = f() # may be None (but unlikely)

            if pair is not None:
                try: f = bok[pair]
                except KeyError: back = None
                else: back = f()

                if back is None:
                    bok[pair] = ref(src)

            try: check = self.__check
            except AttributeError: pass
            else: assert check(val) == src
        del weakref

    @classmethod
    def __back(cls, reflex=mutual, extend=decorate.overriding):
        """Lazily-evaluated class for mutual.

        Each class based on weakprop hereby obtains, as a .__mutual attribute,
        a sub-class that can be used for building mutually referential
        lazily-computed properties.\n"""

        try: mutual = cls.__mutual
        except AttributeError:
            class mutual (cls, reflex):
                """A property class for mutual weak references."""
                __upinit = cls.__init__
                __reinit = reflex.__init__
                def __init__(self, getit, check=False, pair=None, base=reflex):
                    self.__upinit(getit)

                    assert pair is None or isinstance(pair, base), \
                        "Paired properties must both be @.mutual()s"

                    if check: check = getit
                    else: check = None
                    self.__reinit(check, pair)

                __upget = cls.__get__
                __reget = reflex.__get__
                @extend(__upget)
                def __get__(self, obj, cls=None):
                    ans = self.__upget(obj)
                    self.__reget(obj, ans, self.cache(ans))
                    return ans

            mutual.__name__ = cls.__name__ + '.__mutual'
            cls.__mutual = mutual
        return mutual
    del mutual

    @classmethod
    @decorate.accepting(lambda cls, pair=None, check=False: None)
    def mutual(cls, pair=None, check=False, wrap=decorate.aliasing):
        """Weakly-referenced attribute look-up with link-back.

        One of the important cases for weak references is where two objects
        reference one another; this messes up garbage collection unless we use
        weak references.  When lazily evaluating, on one of the objects, the
        attribute whose value is the other object, we want to record on the
        latter that its link-back attribute is the former, to avoid computing
        this lazily (and getting a different object, albeit presumably equal
        to the original) if it ever gets referenced.  Thus if x.a.b is x, when
        evaluating x.a we need to record that its .b is x (for whatever names
        in place of a and b; they may be the same).

        Arguments are optional:
          pair -- the property paird with this one, if any.
          check -- enables a consistency check, see below.

        This method returns a decorator which can be applied to the getter for
        a property (with the usual (self, cls=None) signature).  The getter
        simply computes the value for the property; the decorator sorts out
        the rest.

        If pair is not None it should be a property built using a prior call
        to mutual.  This need not have been on the same class derived from
        weakprop as the present call to mutual; nor need it be a property of
        the same class.  Values of the paired property must be of the class in
        which .mutual() is being used and the property being created must take
        values of the class to which the paired property belongs (i.e. if
        class B uses mutual(pair=A.a) to create a property B.b, then values of
        A.a must be instances of B and values of B.b must be instances of
        A).  The paired property should have been created by an earlier call
        to .mutual() with no pair (or pair=None, the default); no instance of
        the class (A in the case above) providing this property should attempt
        to evaluate this property until after this second .mutual() has
        completed the pairing.  The paired property shall be modified by the
        decorator returned by .mutual(), so as to link it up with the new
        property created by the decorator.  No property created using
        .mutual() should ever be passed as the pair to more than one other
        call to .mutual().

        If pair is unspecified (or None, the default) and the resulting
        property is nowhere passed as the pair to another call to .mutual(),
        the property will set itself on the returned object, to point back to
        the object from which it is created.  Values computed by the getter
        must be instances of the class in which this property is defined.  So
        if class A uses .mutual() with no pair to construct property a, all
        values of .a must be instances of A and, when x.a is computed. x.a.a
        is set to x.

        If A.a is created without pair but later passed as pair when defining
        property B.b (where B may be A), then any instance x of A, when asked
        for x.a, will set x.a.b to x; and any instance y of B, when asked for
        y.b, will set y.b.a to y.

        If check is true, it enables an assertion that round-tripping the
        computation of the properties does actually return a value equal to
        the object whose property was requested.  In the simple case where A.a
        points back at itself, when x.a is computed and has x.a.a set to x,
        the raw getter is called on x.a and the return is verified as being
        equal to x; this is, of course, only meaningful if equality testing is
        defined for instances of A.

        In the case of paired attributes A.a and B.b, if check was passed true
        when creating A.a and y is an instance of B, computation of y.b shall
        call A.a's raw getter on y.b's value to verify that the result (the
        naive value for y.b.a) is in fact equal to y, to which y.b.a shall be
        set; naturally, this only makes sense if B defines equality
        comparison.  Conversely, if B.b's creation set check true, then
        computation of A.a shall verify that-tripping x.a.b would have been
        equal to x.\n"""

        @wrap
        def decor(get, mutual=cls.__back(), chk=check, par=pair):
            return mutual(get, check, par)

        return decor
del recurseprop, propstore

from study.snake.property import dictattr
class weakattr (dictattr, weakprop):
    # TODO: is this right ?  Shall the weak prop be saved in __dict__ ?  Shall
    # values set (in __dict__) be weakly held ?  Should either answer be Yes ?
    __wget = weakprop.__get__
    __dget = dictattr.__get__
    @decorate.overriding(__dget)
    def __get__(self, obj, cls=None):
        try: return self.__dget(obj, cls)
        except AttributeError: return self.__wget(obj, cls)
del dictattr, decorate
