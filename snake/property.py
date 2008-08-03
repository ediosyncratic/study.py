"""Cached properties, with lazy and weekly referenced variants.

This module should eventually replace lazy.Lazy; it provides:
  docprop -- extend property by borrowing getter's doc-string
  dictprop -- extend docprop by implementing set/del via object's __dict__
  recurseprop -- extend property to manage recursion in getters

See individual classes for details.
See also study.cache for related classes.

$Id: property.py,v 1.6 2008-08-03 21:00:52 eddy Exp $
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
    own choice).

    In support of this, its constructor takes the name of the attribute it
    implements, as an extra first argument; but it doesn't need (or accept) a
    setter or deleter function.  Like docprop, it is intended for use as a
    decorator (or a base for property classes to be used as such).

    Because this class is intended as a mix-in for other property classes, so
    that its attribute may be available even if not in __dict__, the deleter
    does not raise AttributeError when the attribute is absent from __dict__;
    the deletion is deemed fatuously successful in this case.\n"""

    __upinit = docprop.__init__
    def __init__(self, name, getit, doc=None):
        if doc is not None:
            assert isinstance(doc, basestring), 'Pass me no setter'
        self.__upinit(getit, self.__set, self.__del, doc)
        self.__name = name

    def __del(self, obj):
        try: del obj.__dict__[self.__name]
        except KeyError: pass

    def __set(self, obj, val):
        if self.__check is not None: self.__check(val)
        obj.__dict__[self.__name] = val

    def __get__(self, obj, mode=None):
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
    def __get__(self, obj, mode=None):
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
