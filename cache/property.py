"""Cached properties, notably including lazy variants.

This module should eventually replace snake.lazy.Lazy; it provides:
  Cached -- mix-in base-class useful when using properties based on attrstore
  attrstore -- extend property with cache-management
  lazyattr -- recurseprop using attrstore to cache attribute values
  lazyprop -- combines lazyattr and dictprop

See also weak.py for weak variants.

$Id: property.py,v 1.1 2008-08-03 21:00:22 eddy Exp $
"""
from study.snake.property import recurseprop, dictprop

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
    def __get__(self, obj, mode=None):
        # Do the lazy lookup:
        bok = self.cache(obj)
        try: ans = bok[self]
        except KeyError:
            bok[self] = ans = self.__upget(obj)

        return ans

class lazyprop (lazyattr, dictprop):
    __lget = lazyattr.__get__
    __dget = dictprop.__get__
    def __get__(self, obj, mode=None):
        try: return self.__dget(obj, mode)
        except AttributeError: return self.__lget(obj, mode)
