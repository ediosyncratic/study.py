"""Objects to describe values with namespaces.

$Id: object.py,v 1.4 1999-12-29 17:52:21 eddy Exp $
"""

def aslookup(whom):
    try: whom('__call__')
    except TypeError: pass
    except AttributeError:
        # functions don't have __dict__
        try: whom.__dict__
        except AttributeError: return whom

        # whom is an instance: if it has a __call__, it's a lookup.
        try: whom.__call__
        except AttributeError: pass
        else: return whom

    else: return whom

    return lambda k, __w=whom: getattr(__w, k)

from types import FloatType
from basEddy.lazy import Lazy

class Object(Lazy):
    """A primitive object variety.

    Provides the following methods:

      copy() -- produces a duplicate object

      also(name=value, ...) -- perform assignment in the object's namespace

      borrow(lookup) -- arranges for object's attribute lookup to try the given
      lookup among others.  If the lookup isn't a callable of the right kind for
      use as a lookup, it is taken to be an object on which attribute lookup
      should be performed to achieve lookup.

    along with a __call__ method which ensures that a Value is a callable of
    the right kind to be used as a lookup - this method just does attribute
    lookup on the object.

    Inherits from Lazy and supports an attribute, dir, which is a copy of the
    object's namespace dictionary, omitting names which start with '_'. """

    def copy(self, *args): return apply(self.__class__, args, self.__star())
    def _lazy_get_dir_(self, ignored): return self.__star()

    def __ephem(self):
        # Ensure _lazy_preserve_ is held locally and return it.
        row = self._lazy_preserve_

        if row is self.__class__._lazy_preserve_:
            row = self._lazy_preserve_ = list(row)

        return row

    def also(self, **what):
        self.__dict__.update(what)

        row = self.__ephem()
        add = row.append

        for key in what.keys():
            if key not in row: add(key)

    def __init__(self, *lookups, **what):
        """Initialiser for Value objects.

        Arguments in name=value form are used to initialise the new object's
        namespace.  Other arguments (which, if present, should appear before
        name=value ones) should be lookups for use when computing attributes of
        the object.  These will only ever be called once for any key.  These
        lookups should be callables, accepting one argument (the attribute name)
        and either returning a value (for the attribute) or raising
        AttributeError.  No attempt is made to coerce them into this form.  Note
        that any Value object is a lookup. """

        # Observation: hijacking self's __getattr__ doesn't fool getattr() !
        # i.e., it'll still ask class's attribute lookup, ignoring personal one.

	Lazy.__init__(self)
	apply(self.also, (), what)

        # now we prepare to replace self's lazy lookup method with one which
        # remembers to call the original before it's finished.
        row = list(lookups) + [ self._lazy_lookup_ ]

        def borrow(where, _row=row):
            _row.insert(-1, aslookup(where))
	self.borrow = borrow

	def getit(key, _row=row):
	    for item in _row:
		try: return item(key)
		except AttributeError: pass
	    raise AttributeError, key
	self._lazy_lookup_ = getit

    def __delattr__(self, key):
        row = self.__ephem()
        try: row.remove(key)
        except ValueError: pass

        try: del self.__dict__[key]
        except KeyError:
            raise AttributeError, ('No such attribute to delete', key)

    def __star(self):
        bok = {}
        for key in filter(lambda k: k[0] != '_', self.__dict__.keys()):
            if not (key in self._borrowed_value_ or self._lazy_ephemeral_(key)):
                bok[key] = self.__dict__[key]
        return bok

    _borrowed_value_ = ()

    # make a value be, at the same time, a lookup ...
    def __call__(self, key): return getattr(self, key)
    # ... which curries self as getattr's first argument.
    # Then we can borrow from it.

class Value(Object): pass       # Backwards compatibility ...

"""
 $Log: object.py,v $
 Revision 1.4  1999-12-29 17:52:21  eddy
 Removed Sample infrastructure to sample.py; much further change.

 Revision 1.3  1999/02/21 01:49:47  eddy
 Redundant Numeric.

 Revision 1.2  1999/02/21 01:31:57  eddy
 Revolution.

 Initial Revision 1.1  1999/01/24 22:34:32  eddy
"""
