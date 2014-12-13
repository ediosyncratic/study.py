"""Objects to describe values with namespaces.

See study.LICENSE for copyright and license information.
"""

def aslookup(whom):
    """Coerces an arbitrary value to a lookup.

    If the value is callable and accepts string arguments, it *is* a lookup,
    though strictly it should never raise any error (given a string argument)
    aside from AttributeError - which isn't checked.  Otherwise, the lookup
    returned borrows attributes from the value given - i.e. returns the result,
    when given any name, of looking for an attribute of that name on the given
    value. """

    # Every lookup is callable:
    if callable(whom):
        # If it raises TypeError when given one string arg, it isn't a lookup.
        try: whom('__class__')
        # I ask it for a __class__ attribute because returning that shouldn't be
        # too dangerous ... compared to any other alternative I can think of.
        except TypeError: pass
        # of course, a lookup will raise AttributeError unless it provides __class__
        except AttributeError: return whom
        # but if it raises any other errors, it's Bad.
        else: return whom

    return lambda k, __w=whom: getattr(__w, k)
    # which coincides with how an Object behaves as a callable.
# In theory, it might be worth calling whom.__getattr__ if that doesn't come
# from its class, since getattr() bypassed this.

from study.snake.lazy import Lazy

class Object (Lazy):
    """A primitive object variety.

    Provides the following methods:

      copy() -- produces a duplicate object

      also(name=value, ...) -- perform assignment in the object's namespace

      borrow(lookup) -- arranges for object's attribute lookup to try the given
                        lookup among others.  If the lookup isn't a callable of
                        the right kind for use as a lookup, it is taken to be an
                        object on which attribute lookup should be performed to
                        achieve lookup.

    along with a __call__ method which ensures that an Object is a callable of
    the right kind to be used as a lookup - this method just does attribute
    lookup on the object.

    Note that borrow() doesn't give access to private names (those starting, but
    not ending, with '__'); and builtins that call a magic method of an object
    do so by asking its class, not by asking the object directly; thus, for
    example, x.__mul__(y) can work where x * y won't, if x borrowed its .__mul__
    rather than getting it from its own class.  This can present problems when
    an object borrows from a value that supports (for example) arithmetic.

    Inherits from Lazy and supports an attribute, dir, which is a copy of the
    object's namespace dictionary, omitting names which start with '_'. """

    def copy(self, *args): return self.__class__(*args, **self.__star())
    def _lazy_get_dir_(self, ignored): return self.__star()
    def help(self): print self.__doc__

    def __ephem(self):
        # Ensure _lazy_preserve_ is held locally and return it.
        row = self._lazy_preserve_

        if row is self.__class__._lazy_preserve_:
            row = self._lazy_preserve_ = list(row)

        return row

    def also(self, **what):
        self.__dict__.update(what)

        # Note that lazy cache flush should *not* clear these attributes ...
        for k in self._lazy_preserve_: what[k] = None
        self._lazy_preserve_ = what.keys()

    _unborrowable_attributes_ = ()
    __upinit = Lazy.__init__
    def __init__(self, *lookups, **what):
        """Initialiser for Object()s.

        Arguments in name=value form are used to initialise the new object's
        namespace.  The name 'lazy_aliases', if used, is sacred to Lazy (q.v.).
        Positional arguments (which, if present, must appear before name=value
        ones) should be lookups for use when computing attributes of the object.
        These will only ever be called once for any key; and they will never be
        called for attributes named in self._unborrowable_attributes_ (evaluated
        during initialisation, after setting attributes supplied as name=value
        arguments, but before extending Lazy's attribute mechanisms with
        Object's borrowing mechanisms).

        Positional arguments should be lookups, i.e. callables, accepting one
        argument (the attribute name) and either returning a value (for the
        attribute) or raising AttributeError.  No attempt is made to coerce them
        into this form.  Note that any Object is a lookup.

        The list of lookups may be extended, after initialisation, by passing a
        lookup to the .borrow() method of the object; the new lookup will be
        added after all lookups supplied to initialisation, but before self's
        own lazy infrastructure. """

        # Note: setting self.__getattr__ won't work, due to the special handling
        # of magic methods mentioned in class doc - getattr() uses
        # self.__class__.__getattr__(self, key) instead of using
        # self.__getattr__ as such.  Fortunately, Lazy.__getattr__ calls
        # self._lazy_lookup_ as such, so we can hijack that instead.  TODO:
        # re-write to have a __getattr__ on this class that removes reliance on
        # that - I want to retire Lazy - but be sure to catch recursive calls,
        # which Lazy presently handles.

        try: aliases = what['lazy_aliases']
        except KeyError: self.__upinit()
        else:
            del what['lazy_aliases']
            self.__upinit(lazy_aliases=aliases)

        self.also(**what)

        # Now we prepare to replace self's lazy lookup method with one which
        # remembers to call the original before it's finished.
        row = list(lookups) + [ self._lazy_lookup_ ]

        def borrow(where, r=row):
            r.insert(-1, aslookup(where))

        def getit(key, r=row, inalien=self._unborrowable_attributes_):
            # Don't borrow if unborrowable or if private (but magic doesn't
            # count as private):
            if key in inalien or (key.startswith('__') and not key.endswith('__')):
                row = (r[-1],) # retain only the original _lazy_lookup_
            else: row = r
            # Note, however, that Quantity relies on ._scale_units_() being
            # borrow()ed successfully, so don't block on key.startswith('_').

            for item in row:
                try: return item(key)
                except AttributeError: pass

            raise AttributeError, key

        self.borrow = borrow
        self._lazy_lookup_ = getit

    def __delattr__(self, key):
        if key in self._lazy_preserve_:
            self.__ephem().remove(key)

        try: del self.__dict__[key]
        except KeyError:
            raise AttributeError('No such attribute to delete', key)

    def __star(self):
        bok = {}
        for key in filter(lambda k: k[0] != '_', self.__dict__.keys()):
            if not (key in self._borrowed_value_ or self._lazy_ephemeral_(key)):
                bok[key] = self.__dict__[key]
        return bok

    _borrowed_value_ = ()

    # :^( sadly __call__ = getattr doesn't work )^:
    def __call__(self, key):
        """Make each Object callable as a lookup.

        This curries self as getattr's first argument, enabling us to .borrow()
        from self.\n"""
        return getattr(self, key)
