"""Generic decorators.

Provides a set of tools (initially inspired by Michele Simionato's decorator
module [0]) for making decorators work better, plus some decorators that
deploy these.  The primary decorators provided here should suffice to do
everything you might need with the low-level tools; however, use of the
low-level tools, particularly in the implementations of other decorators, may
be more efficient.

Low-level tools:
  wrapas(function, prototype) -- wrap function with prototype's signature
  labelas(function, original) -- transcribe superficial attributes of original onto function
  mimic(function, original [, prototype]) -- combine the above; prototype defaults to original
  inherit(function, base [, join]) -- function implements an API defined by base

Prototypes (for use with wrapas, accepting):
  funcorator(function) -- signature of the simplest kind of decorator
  decodeco(decorator) -- signature of a decorator-decorator
You can equally use lambda expressions as prototypes, as needed.

Primary Decorators:
  @accepting(prototype) -- makes decorated function have prototype's signature
  @overriding(base) -- decorated function implements API defined by base
  @aliasing -- makes a decorator preserve superficial attributes
  @mimicking -- as aliasing, but also preserve signature

Note that the last two are decorator-decorators; they act on decorators, to
produce decorators that preserve properties of the functions they
decorate.  Naturally, the decorators produced preserve the superficial
attributes (name, doc string, module and anything in __dict__) of the
decorators they enhance; and have the lambda function: None signature of a
simple decorator.

Further Decorators:
  @postcompose(post [, ...]) -- compose post after decorated function

See also:
 * [0] http://www.phyast.pitt.edu/~micheles/python/decorator.zip
 * http://wiki.python.org/moin/PythonDecoratorLibrary
 * the functools module's wraps() decorator.

See study.LICENSE for copyright and license information.
"""

# Note: all of these are wrapped, below, to hide their tunnelled args !
import inspect
def wrapas(function, prototype,
           fetch=inspect.getargspec, format=inspect.formatargspec, isfunc=inspect.isroutine,
           valfmt='=__default_arg_%x', fname='__implementation',
           skip=lambda x: ''):
    """Masks the signature of a function.

    Takes exactly two callables as arguments, function and prototype.  Returns
    a callable which acts as function but has the signature of
    prototype.  Note that function must, in fact, accept being called in any
    way in which prototype supports being called.  See accepting, below, for a
    decorator deploying this.\n"""
    assert isfunc(function)
    n, a, k, d = fetch(prototype) # TypeError if prototype is a built-in :-(
    glob, i = { fname: function }, 0
    # default values might not repr nicely, so tunnel them via glob:
    for v in d or ():
        glob[valfmt[1:] % i] = v
        i += 1
    # OK, now make a callable that packages all that:
    text = 'lambda %s: %s%s' % (
        format(n, a, k, tuple(range(i)),
               formatvalue=lambda i, f=valfmt: f % i)[1:-1],
        fname,
        format(n, a, k, d, formatvalue=skip))
    # print 'Wrapping %s as "%s" using globals' % (function.__name__, text), glob.keys()
    return eval(text, glob)
del inspect

def labelas(function, original):
    """Transcribe superficial details from original to function.

    Takes exactly two callabless as arguments, function and
    original.  Transcribes name, documentation string, module and anything in
    __dict__ from original to function and returns function.\n"""
    function.__doc__ = original.__doc__
    function.__name__ = original.__name__
    function.__module__ = original.__module__
    function.__dict__.update(original.__dict__)
    return function

joinlines = '\n'.join
def inherit(function, base, join=joinlines, wrap=wrapas):
    """Makes a re-implementation of a function look like its original.

    Similar to mimic (q.v.) but intended for use by a derived class, where it
    has a method that over-rides one on a base-class, or in other situations
    where one callable implements an API defined by another.

    Takes two required arguments: function is the new implementation, base is
    the canonical form of the API.  Accepts an optional third argument, join,
    which should accept a (possibly empty) list of strings and return a
    string; it defaults to '\n'.join.  The returned callable has the signature
    of base, the name of function and composite doc string and __dict__.  For
    __dict__, the result prefers function over base (i.e. it updates first
    from base, then from function).

    Any documentation from base is presumed to be definitive; any
    documentation provided by function is presumed to just elaborate on how it
    implements the API.  Thus the composite puts the doc string of base before
    that of function.  If either is empty or None, it is discarded; the list
    of what remains is passed to join, whose return is used as __doc__, unless
    it is empty (in which case None is used).

    Since the result's signature shall match base's, authors should write
    function using the same argument names as base, so that they'll match
    references in function's immediate doc string, if any.\n"""

    # TODO: decide what to do when several bases are combined.
    ans = wrap(function, base)
    assert function.__name__ == base.__name__
    ans.__name__ = function.__name__
    # TODO: match up base indentation of docs
    # TODO: add "over-rides base-class blah blah" language
    ans.__doc__ = join(filter(None, (base.__doc__, function.__doc__))) or None
    ans.__dict__.update(base.__dict__)
    ans.__dict__.update(function.__dict__)
    return ans

def mimic(function, original, prototype=None, wrap=wrapas):
    """Makes one function look like another.

    Requires two callables as arguments, function and original.  Accepts an
    optional third argument, prototype; if omitted (or None, the default)
    original is used as prototype.  Returns a callable that performs the
    computation of function but has the signature of prototype and original's
    name, doc string, module and any attributes stored in __dict__.\n"""
    if prototype is None: prototype = original
    return labelas(wrap(function, prototype), original)

# Templates for use as prototypes.
def funcorator(function):
    """A decorator that repackages a function.

    Takes a single input, function: this should be a callable.  Returns a
    repackaging of it.\n"""
    raise NotImplementedError

def decodeco(decorator):
    """A decorator-decorator.

    Takes a single input, decorator, and repackages it; decorator itself
    should be a callable that takes a callable and returns a repackaging of
    *that*; the result of decorating decorator itself is a similar callable.\n"""
    raise NotImplementedError

def accepting(prototype, wrapping=wrapas, mime=mimic, rator=funcorator):
    """Decorator to fake function signature.

    Takes one parameter, prototype: only its signature (argument names,
    defaults and whether *more and **keywords are included) matters, so a
    simple lambda expression is sufficient.  Returns a decorator which wraps a
    function so that it can only be called in the ways that prototype
    supports.  The wrapped function preserves the name, doc string, module and
    any attributes stored in __dict__ of the function it wraps.

    Example use: when some other decorator has replaced a function with one
    using *args, **kw as signature, this can restore the original.  This can
    equally be used to hide any optional arguments that were intended to serve
    only as tunnels, so as to prevent callers from inadvertently supplying
    surplus arguments that displace these defaults.\n"""
    def decor(function, form=prototype, wrap=wrapping):
        return labelas(wrap(function, form), function)
    return mime(decor, rator)

def overriding(base, heir=inherit, mime=mimic, wrap=wrapas, rator=funcorator):
    """Decorator for a method that over-rides one on a base-class.

    Single argument, base, is whatever defines the API being overridden
    (e.g. a base class's method).  Returns a decorator which, given the
    replacement (e.g. a derived class's implementation of a method), ensures
    the latter behaves as the former (same signature) and the latter's
    documentation string incorporates the former's; see inherit() for
    details.\n"""

    def decor(function, form=base, mime=heir):
        return mime(function, form)
    return mime(decor, rator)

def aliasing(decorator, mime=mimic, rator=funcorator):
    """Decorator-decorator to make original preserve superficial details.

    Single argument, decorator, is a decorator; return is a replacement
    decorator with the same behaviour except that the results of its
    decoration retain the name and doc-string of the functions they package.\n"""
    def decor(function, base=decorator, label=labelas):
        return label(base(function), function)
    return mimic(decor, decorator, rator)

def mimicking(decorator, mime=mimic, rator=funcorator):
    """Decorator-decorator to make original's wrappers mimic what they wrap.

    Takes one argument, a decorator; returns a replacement decorator that
    preserves the signature, name, doc string, module and anything in __dict__
    of each function decorated.\n"""

    def decor(function, base=decorator, fake=mime):
        return fake(base(function), function)
    return mime(decor, decorator, rator)

def postcompose(post, *more):
    """Apply some post-processing to every output of a function.

    Each argument must be a function.  Returns a decorator which, given a
    function f, turns it into a function that is called exactly as f is but
    the return from f is passed through each function from the decorator in
    turn. Thus, following:
        @postcompose(a, b, c)
        def f(x): return x**2
    a call to f(n) shall return c(b(a(n**2))).

    For example, @postcompose(tuple) will turn a function that returns a list
    (e.g. because that's the easiest way to compute the sequence it wants to
    return) into one that returns a tuple that freezes that list.  Likewise
    for @postcompose(frozenset) on a function returning a set.  When the
    function has many return statements, this can save you repeating the final
    wrapping of its return value (and avoid the risk of neglecting this).

    Equally, one can use postcompose itself as a decorator: it turns the
    decorated function into a decorator which post-processes the returns of
    functions to which *it* is applied.\n"""
    @mimicking
    def decor(decorated, after=(post,) + more):
        def ans(*args, **kw):
            ret = decorated(*args, **kw)
            for f in after: ret = f(ret)
            return ret
        return ans
    return decor

class RecursionError (RuntimeError):
    "A function called itself recursively with the same arguments."

def recurseproof(*args, **kw):
    """Returns a decorator protecting against recursive calling.

    Arguments are booleans, one per (relevant) argument of the function to be
    decorated, indicating whether the argument is hash()able; if it is, the
    argument is used as is in the key used internally to track the call stack;
    otherwise, the argument's id() is used instead.  Raises RecursionError if it
    detects a call to the function with the same key computed for its parameters
    as one already on the stack.

    An argument whose hashability is given as None here means that the matching
    argument to a call of the decorated function should be ignored.  Arguments
    for which nothing specifies whether they are hashable or not are also
    ignored (details below).  Two calls differing only in ignored arguments are
    deemed to conflict, so if one happens during the other's evaluation it is
    treated as bad recursion.

    If the positional argument here at index 1+n is the builtin Ellipsis, all
    later positional arguments here are ignored and the argument at index n is
    used for each positional argument (if any) passed to the actual function
    call at index n or greater; this can be used to characterise an optional
    *args tail to the parameter list.  If Ellipsis is the first argument (n <
    0), a ValueError is raised when invoking recurseproof (i.e. before we even
    get to defining the function to be decorated).

    In the absence of Ellipsis, if the parameter list given here is shorter than
    that used for the decorated function, later arguments passed to the
    decorated function shall be ignored (useful when, for example, these later
    arguments are function detaults used to tunnel a value from the calling
    scope into the function).  If the function is called with fewer arguments
    than were supplied here, the later arguments from here are ignored (i.e. no
    attempt is made to infer what the extras shall get from .func_defaults).

    For keyword arguments, an empty keyword (you'll need to pass this to
    recurseproof() using **{'': val} at the end of the parameters) supplies the
    boolean for all unspecified keys; without this, unspecified keys are ignored
    in the actual function call.  (If an actual function call is given an empty
    keyword argument, by whatever contrivance, it shall be treated as if it were
    an unspecified key.)  All other keywords passed to recurseproof specify, as
    for positional parameters, whether the thus-named parameter to a the
    decorated function can be hashed or must be represented by its id() in the
    key used to check for recursion.  (Specifying None can be used to ignore
    particular keywords when the empty keyword is specified.)  Keywords specifid
    in the call to recurseproof but absent from the call to the decorated
    function are also ignored.  Note that positional parameters passed by
    keyword do not show up as keyword parameters to the internals of a call, so
    there is no point mentioning positional parameters of the decorated function
    in recurseproof's parameter list.

    Arguments to recurseproof other than None and, for positional arguments,
    Ellipsis are interpreted as booleans in the usual way; while use of True and
    False is recommended, you can use anything you like.  (The values are not
    copied, merely remembered; so, if you use a list (for example), changes to
    that list later on may cause its boolean value to change; if this happens
    during recursive calls to a function using the resulting decorator, strange
    things may happen.  If this breaks your code, you get to keep all the
    pieces.  With any luck, though, it'll just delay detection of bad
    recursion.  I do not recommend doing this deliberately.)  In particular, if
    used for a keyword argument, Ellipsis is just another true value.

    This is not completely generic (a *args might alternate hashable entries
    with non-hashable ones, for example; or you might want to specify keyword
    hashability by a regex) and is moderately heavy-weight compared to specific
    well-crafted protection in many simple cases, but it copes with a fairly
    broad class of possible applications and lets you be lazy in the simple
    cases.  Hand-code it later if it turns out to matter.\n"""

    try: cut = args.index(Ellipsis)
    except ValueError: pass
    else:
        if cut: args = args[:1 + cut]
        else: raise ValueError('Ellipsis needs a preceding positional parameter', args)

    @mimicking
    def decor(decorated, pos=args, bok=tuple(kw.items())):
        def key(args, kw):
            ks, i = [], 0
            for arg in args:
                if pos[i] is not Ellipsis:
                    h = pos[i]
                    i += 1
                else: assert i > 0 # so h *has* been set
                if h is not None:
                    ks.append(arg if h else id(arg))

            rest, k = kw.keys(), None
            for nom, h in bok:
                if not nom: k = h
                else:
                    try: rest.remove(nom)
                    except ValueError: pass
                    else:
                        if h is not None:
                            ks.append((nom, kw[nom] if h else id(kw[nom])))

            if k is not None and rest:
                rest.sort()
                for nom in rest:
                    ks.append((nom, kw[nom] if k else id(kw(nom))))

            return tuple(ks)

        pending = set()
        def ans(*args, **kw):
            k = key(args, kw)
            if k in pending: raise RecursionError(decorated, args, kw)
            pending.add(k)
            ret = decorated(*args, **kw)
            pending.discard(k)
            return ret

        return ans
    return decor

# Hide tunnelled args (where used):
accepting = mimic(accepting, accepting, lambda prototype: None)
overriding = mimic(overriding, overriding, lambda base: None)
aliasing = mimic(aliasing, aliasing, decodeco)
mimicking = mimic(mimicking, mimicking, decodeco)
wrapas = mimic(wrapas, wrapas, lambda function, prototype: None)
inherit = mimic(inherit, inherit, lambda function, base, join=joinlines: None)
mimic = mimic(mimic, mimic, lambda function, original: None)
del joinlines
