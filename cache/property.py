"""Cached properties, notably including lazy variants.

This module should eventually replace snake.lazy.Lazy; it provides:
  Cached -- mix-in base-class useful when using properties based on propstore
  propstore -- extend property with cache-management
  lazyprop -- recurseprop using propstore to cache attribute values
  lazyattr -- combines lazyprop and dictattr

See also weak.py for weak variants.

$Id: property.py,v 1.13 2009-11-11 21:31:34 eddy Exp $
"""
from study.snake.property import docprop, recurseprop, dictattr

class Cached (object):
    """Mix-in convenience class for use with cached attributes.

    Since propstore manages an attribute cache on the object whose properties
    are instances of propstore, it also adds a .clear_propstore_cache method to
    those objects - however, it doesn't do so until some instance of propstore
    actually sets an attribute on the object.  Thus calling this method on an
    instance of a class which uses propstore is apt to raise AttributeError.
    This base-class provides that method ab initio, as a vacuous method, which
    propstore.cache() over-rides.\n"""

    def clear_propstore_cache(self): pass

class propstore (docprop):
    """Base-class for cached properties.

    This class actually does no cacheing: it manages a cache for each object
    whose class uses propstore-derived properties, but leaves the getter methods
    of property classes derived from it to decide what to actually cache.  It
    presumes that the cache is used for the values computed by its getter
    function; any support for setting its value should store values elsewhere,
    as its set and delete support extend that of property by, when the latter
    succeeds, clearing any cached entry.  See recurseprop for recursion
    protection which may also be useful in derived classes.\n"""

    @staticmethod
    def cache(obj):
        """Get cache for obj, optionally initializing it.

        This static method of propstore ensures a __cache attribute, whose value
        is a dictionary, is present on the given object; and returns that
        dictionary.  It is intended for use solely by derived classes, in their
        get methods.\n"""
        # Get or initialise cache:
        try: bok = obj.__cache
        except AttributeError:
            bok = obj.__cache = {}
            obj.clear_propstore_cache = bok.clear

        return bok

    def __clear(self, obj):
        try: del obj.__cache[self]
        except AttributeError: pass # uninitialized caache
        except KeyError: pass # uninitialized attribute

    __updel = docprop.__delete__
    def __delete__(self, obj):
        self.__updel(obj)
        self.__clear(obj) # after __updel, in case it fails => forbidden

    __upset = docprop.__set__
    def __set__(self, obj, val):
        self.__upset(obj, val)
        self.__clear(obj) # after __upset, in case it fails => forbidden

class lazyprop (propstore, recurseprop):
    """Lazy attribute look-up.

    Sub-classes recurseprop and propstore; uses the latter's cache to store
    attribute values.  This ensures that the value only gets computed once,
    unless you actively delete or set the value (thereby clearing the cached
    value).  Since the attribute is tacitly present even when it hasn't yet been
    computed, deleting it never raises an AttributeError (unless some derived
    class choses to over-ride that).

    See docprop.group() for support (both here and on any derived class) for
    several attributes all computed together by one function call.\n"""

    assert propstore.__init__.im_func is recurseprop.__init__.im_func is docprop.__init__.im_func
    __upinit = docprop.__init__ # Over-ridden to make deletion mellow.
    def __init__(self, getit, setit=None, delit=lambda x: None, doc=None):
        self.__upinit(getit, setit, delit, doc)

    __upget = recurseprop.__get__
    def __get__(self, obj, kind=None):
        # Do the lazy lookup:
        bok = self.cache(obj)
        try: ans = bok[self]
        except KeyError:
            bok[self] = ans = self.__upget(obj, kind)

        return ans

class lazyattr (dictattr, lazyprop):
    """Lazy attribute which can also be over-ridden in __dict__

    Sub-classes dictattr and lazyprop, consulting the former (i.e. __dict__) in
    preference to the latter, thereby ensuring that any explicitly set value
    over-rides the lazily-computed one.  Note that clear_propstore_cache() on
    the object only clears the lazily-computed values; to also clear any entry
    in __dict__, you need to del the attribute explicitly.\n"""

    __lget = lazyprop.__get__
    __dget = dictattr.__get__
    def __get__(self, obj, kind=None):
        try: return self.__dget(obj, kind)
        except AttributeError: return self.__lget(obj, kind)

del docprop, recurseprop, dictattr
