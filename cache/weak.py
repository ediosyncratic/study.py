"""Cached weak references.

In various contexts, one can compute a datum (entry in a list, value of a
mapping's key, attribute of an object) which it's expensive to compute so
usually worth remembering once it's been computed; but worth being willing to
let the interpreter forget if it gets short on memory.  In these situations, a
weak reference is usually the answer.

Provides:
  WeakTuple -- behaves like a tuple, cacheing returns from a function mapping
               index to entries.
  weakattr -- recurseprop using attrstore to cache weakrefs to attribute values
  weakprop -- combines weakattr and dictprop

$Id: weak.py,v 1.2 2008-08-03 20:59:56 eddy Exp $
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

from study.snake.property import recurseprop
from property import attrstore
class weakattr (attrstore, recurseprop):
    """Weakly-referenced attribute look-up.

    Sub-classes recurseprop and attrstore and uses the latters cache to store a
    weakref to each attribute value.  This ensures that the attribute is always
    available, as if it were never garbabe-collected, while allowing for it to
    be garbage-collected when not actively in use.  The referenced value is
    retrieved automatically from the weakref when available; else it is
    recomputed (and a fresh weakref cached).\n"""

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
    del weakref

class weakprop (weakattr, dictprop):
    __wget = weakattr.__get__
    __dget = dictprop.__get__
    def __get__(self, obj, mode=None):
        try: return self.__dget(obj, mode)
        except AttributeError: return self.__wget(obj, mode)

del recurseprop, attrstore
