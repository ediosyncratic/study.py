"""Cached properties, with lazy and weekly referenced variants.

This module should eventually replace lazy.Lazy; it provides:
  docprop -- extend property by borrowing getter's doc-string and name
  recurseprop -- extend docprop to manage recursion in getters
  dictattr -- extend recurseprop by implementing set/del via object's __dict__

See individual classes for details.
See also study.cache for related classes.

$Id: property.py,v 1.17 2009-10-16 06:18:18 eddy Exp $
"""

class docprop (property):
    """Mix-in base-class to use getter doc-string as property documentation.

    Extends property merely by using the doc-string (as fall-back) and .__name__
    of the getter function for those of the property; this makes it (and classes
    derived from it) nicer to use as a decorator.

    Also provides .group(n) class method (designed to work for derived classes),
    which turns a function, that should return an n-tuple of values, into an
    n-tuple of properties.\n"""

    __upinit = property.__init__
    def __init__(self, getit, setit=None, delit=None, doc=None):
        if doc is None:
            try: doc = getit.__doc__
            except AttributeError: pass

        self.__name__ = getit.__name__
        self.__doc__ = doc # otherwise, doc-string of docprop takes precedence !
        self.__upinit(getit, setit, delit, doc)

    class each (property): # tool used by group
        def __init__(self, group, index):
            self.__all = group
            self.__ind = index
        def __get__(self, obj, kind=None):
            return self.__all.__get__(obj, kind)[self.__ind]

    @classmethod
    def group(klaz, count, hvert=each):
        """Decorator for several attributes computed by one function.

        Required argument, count, is the number of attributes whose values the
        function computes.  Returns a function which, given a callable, maps it
        to a tuple of count property objects.  Use as a decorator on the
        definition of a callable named for one of the attributes being computed;
        subsequently assign this to the tuple of all those attribute names.  The
        callable should return the values for these attributes, as a tuple in
        the same order.  This tuple is managed internally as a property of the
        class whose .group() is used (i.e. group is a class method).

        For example, using study.cache.property.lazyprop (q.v., based on
        docprop), in an imagined sequence-of-numeric class:

            @lazyprop.group(2)
            def variance(self, kind=None):
                tot = totsq = 0.
                for it in self:
                    tot += it
                    totsq += it**2
                mean = tot / len(self)
                return mean, totsq / (len(self) - 1.) - mean**2
            mean, variance = variance

        Instances of the sequence class are then equipped with independently
        accessible properties mean and variance, lazily computed together the
        first time either is accessed (since that's how lazyprop manages the
        tuple returned).\n"""

        def deco(get, k=klaz, n=count, e=hvert):
            all = k(get) # apply underlying decorator to "method"
            return tuple(map(lambda i, a=all, h=e: h(a, i), range(n)))
        return deco
    del each

class recurseprop (docprop):
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
    going to work.  This gives the sub-ordinate attribute look-up the
    opportunity to fall back on some alternative way of determining its
    attribute's value; if it can't do that it fails and the original attribute
    look-up likewise has the opportunity to try some other way of getting its
    answer.

    For example (see study.space.body.Body), an object representing a celestial
    body can, given any two of mass, volume and density, compute the third; so
    properties of the object can attempt to compute each from the other two; if
    only one is unknown, this works.  However, if two of them are unknown,
    attempting to compute either shall ask for the other's value, so attempt to
    compute it, so recurse into attempting to compute the first.  Using
    recurseprop, getters for the properties can simply do the naive computation
    and get an answer when possible, raising AttributeError otherwise, instead
    of causing a RuntimeError due to over-flowing the stack.

    Furthermore (see Object and Satellites in study.space.body), for any
    satellite of the celestial body, given any two of
     * the celestial body's mass
     * the satellite's orbit's period
     * the semi-major axis of the satellite's orbit
    one can compute the third; objects representing celestial bodies and their
    satellitess' orbits can thus have lazy attributes that attempt to compute
    each quantity from the other two, failing if they aren't both set.  We can
    thus compute the celestial body's mass from any satellite orbit, or from its
    volume and density, in so far as data is available.  Using recurseprop, the
    getter for the body's mass can simply try all candidates, ignoring the ones
    that fail, and use any successes to determine its answer.  Each candidate
    that failed through having only one of its needed data can now combine this
    with the mass to compute the missing one; indeed, the mass computation could
    be initiated by such an attempt.\n"""

    __upget = docprop.__get__
    def __get__(self, obj, kind=None):
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

class dictattr (recurseprop):
    """Provides support for set/del in an object's __dict__

    This is a convenience class intended for mixing with other classes derived
    from docprop: it provides for use of an object's usual attribute dictionary
    for the setting and deletion of the attribute; derived classes should call
    its getter to extract the attribute from that dictionary, if present
    (whether they do so before or after any other approach they have is their
    own choice).  The attribute's name is the __name__ of the getter passed
    to the constructor (as for docprop).

    After the usual getter and optional doc parameters, its constructor also
    accepts a callable, wrap (default, None, is equivalent to lambda x: x).  If
    supplied, each value set for the property is passed through this callable,
    which can thus serve both to check validity of the value and to wrap (or
    adjust) it in any needed way.  If it raises any exception, the attribute
    will not be set; otherwise, its return value is stored in __dict__.

    Because this class is intended as a mix-in for other property classes, so
    that its attribute may be available even if not in __dict__, the deleter
    does not raise AttributeError when the attribute is absent from __dict__;
    the deletion is deemed fatuously successful in this case.\n"""

    __upinit = recurseprop.__init__
    def __init__(self, getit, doc=None, wrap=None):
        if doc is not None:
            assert isinstance(doc, basestring), 'Pass me no setter'
        self.__upinit(getit, self.__set, self.__del, doc)
        self.__wrap = wrap

    def __del(self, obj):
        try: del obj.__dict__[self.__name__]
        except KeyError: pass

    def __set(self, obj, val):
        if self.__wrap is not None: val = self.__wrap(val)
        obj.__dict__[self.__name__] = val

    __upget = recurseprop.__get__
    def __get__(self, obj, kind=None):
        try:
            if obj is None: return kind.__dict__[self.__name__]
            else: return obj.__dict__[self.__name__]
        except KeyError: pass
        return self.__upget(obj, kind)
