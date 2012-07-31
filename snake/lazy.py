"""Lazy toys.
"""
# This is in the same spirit as cacheing, but quite independent.
class Lazy (object):
    """Helper class for lazy evaluation.

    Provides a __getattr__ which revises the object (via setattr) so that
    __getattr__ won't be called with the given key again.  Actual lookup of the
    value for a key is delegated to _lazy_lookup_(), which should raise
    AttributeError on failure.

    That being the only functionality required of the base method, no derived
    class is obliged to call it in overriding methods.  However, it may be
    useful so to do, in which case note the idioms the base method introduces
    for how it (in turn) delegates key lookup.

    The basic idiom this base-class supplies is this: define a method on your
    class called _lazy_get_`name'_(key) for any `name' you want in your object's
    namespace.  Don't initialise this name: the first time a piece of code
    actually accesses that name in your object's namespace, this method will be
    invoked (and key will be `name', so you can have one method handle several
    names) and the result will be recorded in the object's namespace with the
    given name.  All subsequent accesses to the name will read that value
    (until, of course, something changes it).

    Thus, for example, a class whose objects have a filename attribute,
    self.filename, can support self.basename via a method

        def _lazy_get_basename_(self, key):
            assert key == 'basename'
            i = len(self.filename)
            while i > 0 and self.filename[i-1] != '/': i = i - 1
            return self.filename[i:]

    and the relevant computation won't be done unless it's actually needed.

    It should be noted that writing your own __getattr__() methods may well
    produce more efficient code than this: and everything this does can be (in
    fact, *is*) done by __getattr__().  However, that involves (in some order) a
    call to ancestor class __getattr__() plus a sequence of clauses (usually
    `try...except: pass' or `if...return'), each of which copes with some name
    or names, records the value it computed against that name, then returns that
    value.  There's a lot of repeated structure in that, which Lazy packages for
    you: and names that get sorted out late in the sequence of clauses (spread
    across several __getattr__() methods in a lineage of classes) involve
    amounts of computation that grow in rough proportion to the number of names
    supported by the object, all of which Lazy turns into some dictionary
    lookups (which are formally as much decision-making, but significantly
    optimised).  So Lazy might even be faster.

    Since each method can have its own docstring (which doesn't come so
    naturally to the clauses in a __getattr__()), Lazy also gives scope for
    explaining what's going on - and for making recommendations about what
    derived classes should bear in mind when overriding one.

    Three flavours of use to this come swiftly to mind:

      When a base-class contributes a default value for an instance's attribute,
      a derived class may override this; if this happens during initialisation,
      the base computes and stores an answer that the derived class discards;
      but a lazy-get method will have been inherited correctly, so only the
      value actually kept will be computed.  This gets particularly significant
      if several ancestors of a class compute an attribute's value during
      initialisation - especially if the derived classes have simpler
      computations to perform than their bases.

      When a class supports access to some attributes that it doesn't need to
      compute in `normal' use, particularly if there are several such
      attributes, I don't want to be (computing and) storing those attributes
      during initialisation of objects that turn out never to use them: it seems
      wasteful.  Indeed, one can write a lazy-get method which imports some
      functions into an object's namespace to give it an alternate calling
      interface - but anyone being that ambitious should at least read the
      documentation of Lazy's methods.

      Some data of an object may not be computable until after the object has
      been created: for instance, if a class exists to have a specific set of
      instances created and some attributes of those instances depend on what
      other instances exist; or there's some attribute of an object which is an
      object of the same class for which that attribute should be the original
      object; or some attributes of the object are to be set by something else
      at some point after creation, and other attributes of the object will
      depend on these.

    There are plenty more.
    """

    # Needs an idiot's guide to non-sophisticated use !

    __recursion_bok_ = None
    def __getattr__(self, key):
        """Attribute lookup with memory.

        Delegates attribute lookup to _lazy_lookup_() and updates self's
        namespace so as to avoid being asked the value of that again.

        Includes checking against recursive call for a given object and key: if
        lazy lookup of self.thing involves some computation which depends on
        knowing self.thing, this will raise an AttributeError.  This is not just
        an assertion/debug: it is intended to enable lazy lookup which will try
        to compute some attribute from one possible source but, if that is not
        available, will fall back on some other possible computation. """

        # print 'Looking up', key       # a powerful debug tool ...
        # (reveals fascinating detail about python internals, too !)
        # Could use a debug.debugStack() to control those messages ...

        # Fix for bug resulting from subtle changes in coercion semantics at 2.3 or so ...
        if key == '__coerce__': raise AttributeError # not supplied by class, so punt.

        # check not in protected region:
        if self.__recursion_bok_ is None:
            # We borrowed it off the Lazy base-class, so it's not yet set on
            # self.  N.B.: trying to detect this by catching an attribute error
            # would induce wild recursion !  (making python 1.5.1 segfault)
            self.__recursion_bok_ = {}
        else:
            try: self.__recursion_bok_[key]
            except KeyError: pass
            else: raise AttributeError, (key, 'recursive lookup')

        try:
            # begin protected region:
            self.__recursion_bok_[key] = None

            # in which to perform the computation:
            val = self._lazy_lookup_(key)

        finally:
            # end protected region:
            del self.__recursion_bok_[key]

        setattr(self, key, val)
        return val

    def __init__(self, lazy_source=None, lazy_aliases=None):
        """Initialise some easy things for lazy lookup.

        Arguments are optional (and their names have a lazy_ prefix for the sake
        of init methods using * and ** in their arg lists and potentially
        wanting to use the un-prefixed names for other purposes):

          lazy_source -- a callable value (or None, the default).  This will be
          used as _lazy_early_ by the object initialised, if given (and not
          None).

          lazy_aliases -- a readable mapping (or None, the default).  This will
          be used to augment _lazy_late_ for the object initialised, if given
          (and not None).

        Both _lazy_early_ and _lazy_late_ are used by the _lazy_lookup_() of
        the Lazy base-class.  Consult the _lazy_lookup_() of the object you are
        actually using for their relevance, if any.

        If lazy_source is omitted (or None), the object will inherit
        _lazy_early_() from its class: which necessarily descends from Lazy,
        which does define _lazy_early_() - to simply raise AttributeError(key),
        as if it hadn't been defined (it's only there for its docs).

        If lazy_aliases is provided, its keys and values are interpreted as
        attribute names.  If the object's _lazy_late_ is asked for the attribute
        named by a key and has the attribute named by the corresponding value,
        the object will use this last as the requested attribute.

        See the doc of your object's methods for definitive truth about either
        _lazy_late_() or _lazy_early_().  Class designers, likewise, think
        about how far you want to interact with these idioms.\n"""

        if lazy_source is not None: self._lazy_early_ = lazy_source

        if lazy_aliases is not None:

            def alias(key, me=self, mine=lazy_aliases, back=self._lazy_late_):
                try: alt = mine[key]
                except KeyError: pass
                else:
                    try: return getattr(me, alt)
                    except AttributeError: pass

                return back(key)

            self._lazy_late_ = alias

        # Note: this class doesn't actually rely on sub-classes calling this
        # initialiser ... it allows sub-classes to be lazy about that ;^>

    def _lazy_lookup_(self, key):
        """One-off attribute lookup.

        Argument, key, is the name of an attribute.  If we can work out a value
        for that name, we return it: otherwise, return AttributeError.  Raises
        AttributeError(key) on failure.

        The Lazy base-class defines a three-phase lazy lookup.  The first and
        last may be controlled, for each object, by the base __init__():
        otherwise, like the second, they are determined by the object's class.
        Each phase is controlled by a lookup method (i.e. one taking the
        attribute name as single argument).  These are named and described
        below.

          _lazy_early_(key) -- returns the value to use for the given key.
          Should answer for keys the class doesn't want overriden by its derived
          classes: if calling the _lazy_early_() of an ancestor class, do it
          early in the derived class' _lazy_early_().

          _lazy_method_get_(key) -- returns a callable which, in turn, is given
          the same key as input (though it is allowed to ignore it): the result
          of that, if any, will be used as the key's value.

          _lazy_late_(key) -- returns the value to use for the given key.
          Should answer for keys the class wants in its namespace, but for which
          it would rather someone else provided a value: if calling the
          _lazy_late_() of an ancestor class, do it late in the derived class'
          _lazy_late_().

        See the documentation of each method (on an object of your choice, or on
        the Lazy base class) for further details. """

        try: return self._lazy_early_(key)
        except AttributeError: pass

        try: meth = self._lazy_method_get_(key)
        except AttributeError: pass
        else:
            assert callable(meth)
            try:
                if isinstance(meth, type(self._lazy_lookup_)): return meth(key)
                else: return meth(self, key) # do the currying for it
            except TypeError: pass

        return self._lazy_late_(key)

    def _lazy_early_(self, key):
        # This is only here for its doc: it behaves as if it weren't.
        """Lazy evaluation: early lookup.

        Argument, key, is a name to be looked up.  Returns the value self wants
        in its namespace against this name.

        Recommended usage: early in a derived class' _lazy_early_(), it should
        call that of an ancestor class.  The _lazy_early_() method should only
        give answers to things it expects its derived classes not to want to
        override.  See _lazy_lookup_() for all other cases.

        The Lazy initialisation accepts, as its 'source' argument, a callable
        entity with which to override the class-derived _lazy_early_().  If that
        is omitted, Lazy._lazy_early_() just raises an AttributeError: derived
        classes should override this if they've anything more constructive to
        do. """

        raise AttributeError, key

    def _lazy_late_(self, key):
        """Lazy evaluation fallback.

        Argument, key, is the name to be looked up.  Returns the value self
        wants in its namespace against that key.  Raises AttributeError(key)
        on failure.

        Recommended usage: late in a derived class' _lazy_late_(), it should
        call that of an ancestor class.  A tidy thing to do with _lazy_late_
        is to implement aliasing and default values, e.g.

            def _lazy_late_(self, key,
                            defaults={'unit': 1},
                            naming={'broad': 'wide', 'narrow': 'thin'},
                            down=Lazy._lazy_late_):

                try: alt = naming[key]
                except KeyError: pass
                else:
                    try: return getattr(self, alt)
                    except AttributeError: pass

                try: return defaults[key]
                except KeyError: pass

                return down(self, key)

        possibly with a direct base-class in place of Lazy.  The base Lazy
        initialisation also provides for per-object aliasing in the style of the
        above. """

        raise AttributeError(key)

    def _lazy_method_get_(self, key):
        """Really lazy lookup of how to look up the value for a key.

        Argument, key, is a name (which'll get added to self's namespace, if
        all goes well).

        This method tries to find a callable entity which, when given key, will
        return the value we want in self's namespace with the key as its name.
        If it can't do that, it will raise AttributeError.

        This base method looks for an attribute of self, with name of form
        '_lazy_get_'*key*'_', and returns that - in the expectation that it is a
        method which accepts (and probably ignores) *key*, returning the value
        self should have for its attribute *key*.  [It doesn't do this if *key*
        ends in '_', though, unless it begins and ends in exactly two
        underscores - a special case for the sake of subtlety with magic
        methods.]  Derived classes may find it less hassle to use this than to
        define its own _lazy_method_get_().

        A derived class' _lazy_method_get_() might well give the same answer for
        several keys: though that answer may well respond differently to the
        keys in response to which it is provided.  For instance,
        _lazy_method_get_() could import a module and add something to self's
        namespace (or even to that of a class) which will do the actual lookup
        wanted.

        If inheriting Lazy.__hash__(), defining a _lazy_get__lazy_hash_() will
        render a derived class hashable, provided _lazy_get__lazy_hash_() will
        accept '_lazy_hash' as input.  It should return a sensible snapshot hash
        value for self at the moment it gets called: the result will be used as
        hash value thereafter even if the data used to compute it have changed.
        Such a class will also have to define a __cmp__() for which this hash
        will make sense, of course ... """

        try:
            if (key[-1:] != '_' or
                (key[-2:] == '__' == key[:2] and key[-3:-2] != '_' != key[2:3])):
                ans = getattr(self, '_lazy_get_' + key + '_')
                if callable(ans): return ans
        except AttributeError: pass

        raise AttributeError(key)

    __ephemeral_doc = """ IWBN to change the lazy/ephem information so we know
    when the value present was arrived at by laziness.  This requires the object
    to carry around a list of presently lazy (i.e. recoverable) attributes.  Any
    setattr on one of these must remove it from the list, unless it's the
    setattr that gets done by the lazy lookup process.  To achieve that, we can
    have setattr automatically blast any name it sets off the list, while having
    the lazy lookup process put it back after doing the setattr.  However, this
    requires every Lazy class to delegate all changes to __dict__ to Lazy, which
    is a bit cumbrous. """

    _lazy_preserve_ = ()
    def _lazy_ephemeral_(self, key):
        """Returns true if deleting key shouldn't really matter."""
        if key in self._lazy_preserve_: return None
        try: self._lazy_method_get_(key)
        except AttributeError: return None
        return 1

    def _lazy_reset_(self, *preserve):
        """Empties namespace of all lazy-regenerable data.

        Deletes all attributes of self for which a lazy lookup is available
        unless the attribute name: is given as an argument to this function or;
        is given in self._lazy_preserve_ (which will typically be a class
        attribute).

        For use, e.g.: when changing the data from which lazy attributes are
        computed; or after heavy use of the object, when you know you won't be
        using it again for a while. """

        for key in filter(self._lazy_ephemeral_, self.__dict__.keys()):
            if key not in preserve:
                delattr(self, key)

    def __hash__(self):
        """Hash value for a lazy object.

        Define _lazy_get__lazy_hash_() or persuade your _lazy_early_() or
        _lazy_late_ to respond to the name '_lazy_hash' and you're away.
        Because this is a one-off computation, you get a sensible hash value:
        even if the data from which you compute self._lazy_hash changes, you'll
        still have a constant hash value. """

        try: return self._lazy_hash
        except AttributeError: raise AttributeError('__hash__')

class lazyClass (Lazy):
    """How to complete a class' definition at run-time ;^>

    Define a _lazy_class_lookup_ method on a derived class to set the resulting
    entry in the namespace of the class defining the _lazy_class_lookup_ (*not*
    that of self.__class__).  This is done *before* _lazy_early_, even though
    classes derived from Lazy should usually call their parent._lazy_early_
    before doing anything else (see lazyClass.why_early_doc).

    A derived class's _lazy_class_lookup_() should call that of its parent
    before doing its own effort.

    might cause your parent's lookup to happen sometimes before you, and your
    program will behave differently if so (unless, for some reason, you're
    computing the *same* value as your parent, which sounds a bit of a
    time-waster).

    As an example, two classes which need to know about one another can lead
    to problems with circular inclusion among modules (which can usually be sorted
    out with a bit of care).  One solution is to have each refer to the other via some
    name in its own namespace, which it doesn't actually define: but its
    _lazy_class_lookup_
    A lazyClass hijacks the first attempt
    at accessing a relevant name on *an object* of either class

    you could define your class' _lazy_class_lookup_ to import a
    module which provides a base-class with which yours has such a close
    relationship that you'd run into problems
    """

    why_early_doc = """Consider the alternative:

    Suppose inClass inherits from outClass inherits from lazyClass.  Suppose
    outClass has a name which it evaluates lazily and inClass has a way of
    evaluating the same datum on each object (rather than on itself), and that
    inClass' object-evaluation gets evaluated before outClass' class-evaluation.

    I create an object, green, of class inClass and ask for the relevant datum:
    it gets computed for the object by the inClass evaluator; whenever it is
    asked for in future, that value will be used.  I create an object, red, of
    outClass and ask for its value for that name: this is computed by the
    outClass and recorded there; hereafter, nothing I create in either class
    will bother to look up the given name - they'll always use the value from
    outClass.

    If the value computed for the inClass object was the same as that on
    outClass (so independent of the object), recording its value on an object of
    inClass seems a bit pointless.  If it isn't the same as the outClass value,
    then objects of inClass created before outClass will behave differently from
    objects created after: which isn't how to make robust software !

    """

    def _lazy_early_(self, key):
        try: return self._lazy_class_lookup_(key)
        except AttributeError: pass

        # Honour thy parents, even when it's pointless.
        return Lazy._lazy_early_(self, key)

    def _lazy_class_lookup_(self, key):
        """Gets an attribute *for a class*.

        If an instance of a class derived from yours hasn't got the value it
        needs, this gives your your class the chance to set the value in its own
        namespace - rather than that of the object in question, or its (direct)
        class.  Setting attributes on self.__class__ should be approached with
        some care, since the class whose code does it might not be the one it
        sets this way - if nothing else, setting it on the class which
        defines

        """

        raise AttributeError

class Delegator:
    """Self-revising object class.

    For use by a base-class which expect to discover things about its objects,
    after creation, which mean the object should now turn itself into an
    instance of a derived class.

    If you have a delegator in your namespace, it is sensible to revise it to
    its delegate from time to time: for example,
        name = name.self()
    This would typically replace an object in a base class with one in a more
    specific class which has methods appropriate to the data it carries. """

    def __init__(self):
        """Records self as self._self_."""
        self._self_ = self

    # I want to be able to lazily add methods to self, whenever self._self_
    # supports them, as def method(self): return self._self_.method()

    def _delegate_to_(self, other):
        """Delegates to other.

        Argument, other, should be an object faithfully representing self,
        idealy better so than self does.  This method should only be called by
        self's own class when it is sure that other is a suitable substitute for
        self. """

        self._self_ = other

    def self(self):
        """Revise delegation and return new delegate.

        If self is delegated, this revises its delegation to the best available
        delegate.

        Returns self's delegate (after any such revision). """

        if self is not self._self_:
            self._self_ = self._self_.self()

        return self._self_
