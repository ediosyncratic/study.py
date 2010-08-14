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

    Packages a function, taking an index into the sequence as sole parameter, as
    a read-only sequence, cacheing the values using weakref.  Derived classes
    should typically implement __len__, at least.\n"""
    __upinit = ReadSeq.__init__
    def __init__(self, getter):
        """Package getter as a tuple-like sequence.

        Single argument, getter, shall be called with a single parameter and
        should return the entry desired at that index into the sequence; it
        should raise IndexError if the parameter is not a suitable index.  Later
        calls with the same input should produce an equivalent response.\n"""
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

from study.snake.property import recurseprop, dictattr
from study.cache.property import propstore
class weakprop (propstore, recurseprop):
    """Weakly-referenced attribute look-up.

    Sub-classes recurseprop and propstore; uses the latter's cache to
    store a weakref to each attribute value.  This ensures that the
    attribute is always available, as if it were never
    garbabe-collected, while allowing for it to be garbage-collected
    when not actively in use.  The referenced value is retrieved
    automatically from the weakref when available; else it is recomputed
    (and a fresh weakref cached).

    Can only be used for attributes whose values are of types to which
    weakrefs are permitted; in particular, the value cannot be a tuple
    (or an instance of a class based on tuple).\n"""

    import weakref
    __upget = recurseprop.__get__
    def __get__(self, obj, mode=None, ref=weakref.ref):
        bok = self.cache(obj)
        try: f = bok[self]
        except KeyError: ans = None
        else: ans = f()

        if ans is None:
            ans = self.__upget(obj)
            bok[self] = ref(ans)

        return ans

    assert propstore.__init__.im_func is recurseprop.__init__.im_func
    @classmethod
    def __back(cls, fer=weakref.ref):
        """Lazily-evaluated class for mutual and reflex.

        Each class based on weakprop hereby obtains, as a .__reflex
        attribute, a sub-class that can be used for building mutually
        referential lazily-computed properties.\n"""
        try: reflex = cls.__reflex
        except AttributeError:
            class reflex (cls):
                """A property class for mutual weak references."""
                __upinit = cls.__init__
                def __init__(self, getit, pair=None, ref=fer):
                    self.__upinit(getit)
                    if pair is not None:
                        self.__partner = ref(pair)
                        try: f = pair.__partner
                        except AttributeErrror: back = None
                        else: back = f()
                        if back is None: # typically it is
                            pair.__partner = ref(self)

                __upget = cls.__get__
                def __get__(self, obj, mode=None, ref=fer):
                    ans = self.__upget(obj)
                    bok = self.cache(ans)
                    try: f = self.__partner
                    except AttributeError: pair = self
                    else: pair = f() # may be None (but unlikely)

                    if pair is not None:
                        try: f = bok[pair]
                        except KeyError: back = None
                        else: back = f()

                        if back is None:
                            bok[base] = ref(obj)

                    return ans

            reflex.__name__ = cls.__name__ + '.__reflex'
            cls.__reflex = reflex
        return reflex
    del weakref

    @classmethod
    def mutual(cls, get):
        """Weakly-referenced mutual attribute look-up.

        One of the important cases for weak references is where two
        objects reference one another; this messes up garbage collection
        unless we use weak references.  When lazily evaluating one of
        the objects, we want to record on the other that it is the
        original's relevantly-named attribute, to save computing *its*
        lazy attribute.  Contrast .reflex(), where two attributes are
        each the reverse of the other.

        Returns a property computed using get, so can be used as a
        decorator to turn a function (with the usual (self, cls=None)
        signature for property getters) into a property.\n"""

        return cls.__back()(get)

    @classmethod
    def reflex(cls, left, right):
        """Takes two getters and makes them weak reflex attributes.

        By reflex, I mean the left of an object has that object as its
        right, and vice versa.  So each getter computes an attribute, to
        be saved on an object, which should also be used as saved
        value's attribute with the other getter's name.  Both attributes
        are saved as weak refs, to avoid garbage-collection issues due
        to the cyclic reference.  Contrast .mutual(), where an attribute
        is its own reflex.

        Returns a pair of properties, (L, R), with L computed using left
        and R computed using right.  Can't be used as a decorator, but
        works essentially similarly to one.\n"""

        # Complication: the two properties have to know about each
        # other, also creating potential problems with
        # garbage-collection; so have them remember one another via
        # weakrefs.  Fortunately, although a weakref to a raw property
        # isn't supported, I seem to be able to get away with weakrefs
        # to instances of weakprop, hence presumably of classes derived
        # from it.

        reflex = self.__back()
        left = reflex(left)
        right = reflex(right, left)
        return left, right

    # One could go further and deal with an arbitrarily long cycle of
    # properties, looping back to the original object, as in:
    # x is x.a.b.c.d is x.b.c.d.a is x.c.d.a.b is x.d.a.b.c;
    # but I can't think of a concrete use-case !  It's also hard to see
    # how this might be implemented - unless each has a reflex property,
    # at which point use of reflex should suffice anyway - without
    # evaluating the whole of the rest of the loop when evaluating any
    # member (i.e. when asked for x.a, also compute and record x.a.b and
    # x.a.b.c so as to mark the last's .d as being x), so as to be able
    # to close the loop back to the object we started from.

class weakattr (dictattr, weakprop):
    # TODO: is this right ?  Shall the weak prop be saved in __dict__ ?  Shall
    # values set (in __dict__) be weakly held ?  Should either answer be Yes ?
    __wget = weakprop.__get__
    __dget = dictattr.__get__
    def __get__(self, obj, mode=None):
        try: return self.__dget(obj, mode)
        except AttributeError: return self.__wget(obj, mode)

del recurseprop, propstore, dictattr
