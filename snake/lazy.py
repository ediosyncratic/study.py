"""Lazy toys.

$Id: lazy.py,v 1.1 1999-02-21 01:33:41 eddy Exp $
"""
# This is in the same spirit as cacheing, but quite independent.
class Lazy:
    """Helper class for lazy evaluation.

    Provides a __getattr__ which revises an object's __dict__ so that
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
    _lazy_get_basename_(key='basename'), and the relevant computation won't be
    done unless it's actually needed.

    It should be noted that writing your own __getattr__() methods may well
    produce more efficient code than this: and everything this does can be done
    by __getattr__().  However, that involves (in some order) a call to ancestor
    class __getattr__() plus a sequence of clauses (usually `try...except: pass'
    or `if...return'), each of which copes with some name or names, records the
    value it computed against that name, then returns that value.  There's a lot
    of repeated structure in that, which Lazy packages for you: and names that
    get sorted out late in the sequence of clauses (spread across several
    __getattr__() methods in a lineage of classes) involve amounts of
    computation that grow in rough proportion to the number of names supported
    by the object, all of which Lazy turns into some dictionary lookups (which
    are formally as much decision-making, but significantly optimised).  So Lazy
    might even be faster.

    Since each method can have its own docstring (which doesn't come so
    naturally to the clauses in a __getattr__()), Lazy also gives scope for
    explaining what's going on - and for making recommendations about what
    derived classes should bear in mind when overriding one.

    Two flavours of use to this come swiftly to mind:

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

    There are plenty more.
    """

    # Needs an idiot's guide to non-sophisticated use !

    def __getattr__(self, key):
	"""Attribute lookup with memory.

	Delegates attribute lookup to _lazy_lookup_() and updates self's
	namespace so as to avoid being asked the value of that again. """

	try: val = self._lazy_lookup_(key)
	except AttributeError:
	    raise AttributeError, key
	# thus skipping lazy lookup, etc., from the traceback.

	self.__dict__[key] = val
	return val

    def __init__(self, source=None, fallbacks=None):
	"""Initialise some easy things for lazy lookup.

	Arguments are optional:

	  source -- (default: None; or) a callable value.  This will be used as
	  _lazy_early_ by the object initialised, if given (and not None).

	  fallbacks -- (default: None; or) a readable mapping.  This will be
	  used as _lazy_fallbacks by the object initialised, if given (and not
	  None).  Note that sequences are mappings, for these purposes.

	Both _lazy_early_ and _lazy_fallbacks are used by the _lazy_lookup_()
	of the Lazy base-class.  Consult the _lazy_lookup_() of the object
	you are actually using for their relevance, if any.

	If source is omitted (or None), the object will inherit _lazy_early_()
	from its class: which necessarily descends from Lazy, which does define
	_lazy_early_() - to simply raise AttributeError(key), as if it hadn't
	been defined (it's only there for its docs).

	If fallbacks is omitted, _lazy_fallbacks will be inherited from the
	object's class: if all else fails, Lazy gives it the value ().  The
	value of _lazy_fallbacks is used by the _lazy_late_() method of the
	Lazy base-class to provide `last-ditch' values for unknown keys over
	which you're not willing to raise AttributeError (eg name, in some
	situations).

	The base-class _lazy_late_() also plays with some methods whose
	names begin with _lazy_, but see the doc of your object's methods for
	definitive truth about either _lazy_fallback() or _lazy_early_().
	Class designers, likewise, think about how far you want to interact with
	these idioms. """

	if source != None:
	    self._lazy_early_ = source
	if fallbacks != None:
	    self._lazy_fallbacks = fallbacks

    def _lazy_lookup_(self, key):
	"""One-off attribute lookup.

	Argument, key, is the name of an attribute.  If we can work out a value
	for that name, we return it: otherwise, return AttributeError.  Raises
	AttributeError(key) on failure.

	The Lazy base-class defines a three-phase lazy lookup.  The first and
	last may be controlled, for each object, by the base __init__():
	otherwise, like the second, they are determined by the object's class.
	The names of the methods defining the three phases are given below, with
	the meanings they should have if defined on a class: each takes just one
	argument, a key.

	  _lazy_early_ -- returns the value to use for the given key.  Should
	  answer for keys the class doesn't want overriden by its derived
	  classes: if calling the _lazy_early_() of an ancestor class, do it
	  early in the derived class' _lazy_early_().

	  _lazy_method_get_ -- returns a callable which, in turn, is given the
	  same key as input (though it is allowed to ignore it): the result of
	  that, if any, will be used as the key's value.

	  _lazy_late_ -- returns the value to use for the given key.  Should
	  answer for keys the class wants in its namespace, but would rather
	  someone else came up with a value for them: if calling the
	  _lazy_late_() of an ancestor class, do it late in the derived class'
	  _lazy_late_().

	See the documentation of each method (on an object of your choice, or on
	the Lazy base class) for further details. """

	try: return self._lazy_early_(key)
	except AttributeError: pass

	try: meth = self._lazy_method_get_(key)
	except AttributeError: pass
	else:
	    try: return meth(key)
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

	raise AttributeError(key)

    _lazy_fallbacks = ()
    def _lazy_late_(self, key):
	"""Catches lazy evaluation fallbacks.

	Argument, key, is the name to be looked up.  Returns the value self
	wants in its namespace against that key.  Raises AttributeError(key)
	on failure.

	Recommended usage: late in a derived class' _lazy_late_(), it should
	call that of an ancestor class.  Lazy._lazy_late_ falls back on
	self._lazy_fallbacks[key] if defined, raising AttributeError on failure.

	The Lazy initialisation provides for a mapping (or sequence) to be
	used as the _lazy_fallbacks of the object initialised.  If this is
	not exercised, the object will inherit _lazy_fallbacks from its class:
	failing all else, it will inherit the empty tuple Lazy._lazy_fallbacks.

	The correct way for a derived class to specify its _lazy_fallbacks, if
	inheriting most of it from an ancestor, is shown in this example:

	  class derived(mum, dad):
		_lazy_fallbacks = ancestor._lazy_fallbacks.copy()
		_lazy_fallbacks.update({'My': 'defaults'})

	so that the derived class overrides its ancestor's default without
	tampering with the ancestor's _lazy_fallbacks mapping.  Of course, after
	copying the original but before update()ing with your own, you might
	want to update() with some other ancestor's mapping. """

	try: back = self._lazy_fallbacks
	except AttributeError: pass
	else:
	    try: return back[key]
	    except (KeyError, IndexError, TypeError): pass

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
	ends in '_', though.]  A derived classes may find it less hassle to use
	this than to define its own _lazy_method_get_().

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
	    if key[-1:] != '_':
		return getattr(self, '_lazy_get_' + key + '_')
	except AttributeError: pass

	raise AttributeError(key)

    def __hash__(self):
	"""Hash value for a lazy object.

	Define _lazy_get__lazy_hash_() or persuade your _lazy_early_() or
	_lazy_late_ to respond to the name '_lazy_hash' and you're away.
	Because this is a one-off computation, you get a sensible hash value:
	even if the data from which you compute self._lazy_hash changes, you'll
	still have a constant hash value. """

	try: return self._lazy_hash
	except AttributeError: raise AttributeError('__hash__')

class lazyClass(Lazy):
    """How to complete a class' definition at run-time ;^>

    Define a _lazy_class_lookup_ method on a derived class to set the resulting
    entry in the namespace of the class defining the _lazy_class_lookup_ (*not*
    that of self.__class__).  This is done *before* _lazy_early_, even though
    classes derived from Lazy should usually call their parent._lazy_early_
    before doing anything else (see lazyClass.why_early_doc).

    A derived class' _lazy_class_lookup_() should call that of its parent before
    doing its own effort.  

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
	return self._lazy_class_lookup_(key)	# raising AttributeError on failure

	# Honour thy parents, even when it's pointless.
	try: return Lazy._lazy_early_(self, key)
	except AttributeError: pass

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

    For use by base-classes which expect to discover things about its objects,
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

_rcs_log = """
$Log: lazy.py,v $
Revision 1.1  1999-02-21 01:33:41  eddy
Initial revision

"""
