"""Cached properties, with lazy and weekly referenced variants.

This module should eventually replace lazy.Lazy; it provides:
  docprop -- extend property by borrowing getter's doc-string
  dictprop -- extend docprop by implementing set/del via object's __dict__
  recurseprop -- extend property to manage recursion in getters
  Cached -- mix-in base-class useful when using properties based on attrstore
  attrstore -- extend property with cache-management
  lazyattr -- recurseprop using attrstore to cache attribute values
  lazyprop -- combines lazyattr and dictprop
  weakattr -- recurseprop using attrstore to cache weakrefs to attribute values
  weakprop -- combines weakattr and dictprop

See individual classes for details.

$Id: property.py,v 1.1 2008-06-26 07:22:22 eddy Exp $
"""

class docprop (property):
    """Mix-in base-class to use getter doc-string as property documentation.

    Extends property merely by using the doc-string of the getter function as
    fall-back for the doc-string of the property; this makes it (and classes
    derived from it) nicer to use as a decorator.\n"""

    __upinit = property.__init__
    def __init__(self, getit, setit=None, delit=None, doc=None):
        if doc is None:
            try: doc = getit.__doc__
            except AttributeError: pass

        self.__upinit(getit, setit, delit, doc)

class dictprop (docprop):
    """Provides support for set/del in an object's __dict__

    This is a convenience class intended for mixing with other classes derived
    from docprop: it provides for use of an object's usual attribute dictionary
    for the setting and deletion of the attribute; derived classes should call
    its getter to extract the attribute from that dictionary, if present
    (whether they do so before or after any other approach they have is their
    own choice).  In support of this, its constructor takes the name of the
    attribute it implements, as an extra first argument; but it doesn't need (or
    accept) a setter or deleter function.  Like docprop, it is intended for use
    as a decorator.\n"""

    __upinit = docprop.__init__
    def __init__(self, name, getit, doc=None):
        self.__upinit(getit, self.__set, self.__del, doc)
        self.__name = name

    def __del(self, obj):
        try: del obj.__dict__[self.__name]
        except KeyError: pass

    def __set(self, obj, val):
        if self.__check is not None: self.__check(val)
        obj.__dict__[self.__name] = val

    def __get__(self, obj, type=None):
        try: return obj.__dict__[self.__name]
        except KeyError: raise AttributeError, self.__name

class recurseprop (property):
    """Cope with recursion in getters of properties.

    The getter of this class implements recursion-protection, so that getter
    functions passed to the constructor can attempt to use other attributes
    which may have been set overtly even if these attributes' getters (when not
    overtly set) in their turn may attempt to use the first attribute's value.
    When such mutual dependencies among attributes leads to recursion - the
    getter for each property marks each object while it's actually computing
    that object's property value; it recognizes recursion if it finds its own
    mark on the object before starting the computation - it simply raises
    AttributeError to let the sub-ordinate attribute look-up know this isn't
    going to work.\n"""

    __upget = property.__get__
    def __get__(self, obj, type=None):
        # Compute attribute, but protect from recursion:
        try: check = obj.__recurse_
        except AttributeError:
            check = obj.__recurse_ = {}

        try: check[self] # are we in the midst of computing this already ?
        except KeyError: check[self] = None # OK, no problem.
        else: raise AttributeError(obj, self, 'recursive laziness')

        # Do the actual computation:
        try: ans = self.__upget(obj) # might AttributeError
        finally: del check[self]
        return ans

class Cached (object):
    """Mix-in convenience class for use with cached attributes.

    Since attrstore manages an attribute cache on the object whose properties
    are instances of attrstore, it also adds a .clear_attrstore_cache method to
    those objects - however, it doesn't do so until some instance of attrstore
    actually sets an attribute on the object.  Thus calling this method on an
    instance of a class which uses attrstore is apt to raise AttributeError.
    This base-class provides that method ab initio, as a vacuous method, which
    attrstore.cache() over-rides.\n"""

    def clear_attrstore_cache(self): pass

class attrstore (property):
    """Base-class for cached properties.

    This class actually does no cacheing: it manages a cache for each object
    whose class uses attrstore-derived properties, but leaves the getter methods
    of property classes derived from it to decide what to actually cache.  It
    presumes that the cache is used for the values computed by its getter
    function; any support for setting its value should store values elsewhere,
    as its set and delete support extend that of property by, when the latter
    succeeds, clearing any cached entry.  See recurseprop for recursion
    protection which may also be useful in derived classes.\n"""

    @staticmethod
    def cache(obj):
        """Get cache for obj, optionally initializing it.

        This static method of attrstore ensures a __cache attribute, whose value
        is a dictionary, is present on the given object; and returns that
        dictionary.  It is intended for use solely by derived classes, in their
        get methods.\n"""
        # Get or initialise cache:
        try: bok = obj.__cache
        except AttributeError:
            bok = obj.__cache = {}
            obj.clear_attrstore_cache = bok.clear

        return bok

    def __clear(self, obj):
        try: del obj.__cache[self]
        except AttributeError: pass # uninitialized caache
        except KeyError: pass # uninitialized attribute

    __updel = property.__delete__
    def __delete__(self, obj):
        self.__updel(obj)
        self.__clear(obj) # after del, in case it fails => forbidden

    __upset = property.__set__
    def __set__(self, obj, val):
        self.__upset(obj, val)
        self.__clear(obj) # after set, in case it fails => forbidden

class lazyattr (attrstore, recurseprop):
    """Lazy attribute look-up.

    Sub-classes recurseprop and attrstore and uses the latter's cache to store
    attribute values.  This ensures that the value only gets computed once,
    unless you actively delete or set the value (thereby clearing the cached
    value).\n"""

    __upget = recurseprop.__get__
    def __get__(self, obj, type=None):
        # Do the lazy lookup:
        bok = self.cache(obj)
        try: ans = bok[self]
        except KeyError:
            bok[self] = ans = self.__upget(obj)

        return ans

class lazyprop (lazyattr, dictprop):
    __lget = lazyattr.__get__
    __dget = dictprop.__get__
    def __get__(self, obj, type=None):
        try: return self.__dget(obj, type)
        except AttributeError: return self.__lget(obj, type)

import weakref
class weakattr (attrstore, recurseprop):
    """Weakly-referenced attribute look-up.

    Sub-classes recurseprop and attrstore and uses the latters cache to store a
    weakref to each attribute value.  This ensures that the attribute is always
    available, as if it were never garbabe-collected, while allowing for it to
    be garbage-collected when not actively in use.  The referenced value is
    retrieved automatically from the weakref when available; else it is
    recomputed (and a fresh weakref cached).\n"""
    __upget = recurseprop.__get__
    def __get__(self, obj, type=None, ref=weakref.ref):
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
    def __get__(self, obj, type=None):
        try: return self.__dget(obj, type)
        except AttributeError: return self.__wget(obj, type)

