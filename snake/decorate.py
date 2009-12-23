"""Generic decorators.

Provides a set of tools (inspired by Michele Simionato's decorator module [0])
for making decorators work better, plus some decorators that deploy these.  The
primary decorators provided here should suffice to do everything you might need
with the low-level tools; however, use of the low-level tools, particularly in
the implementations of other decorators, may be more efficient.

Low-level tools:
  wrapas(func, proto) -- wrap func with proto's signature
  labelas(func, orig) -- transcribe superficial attributes of orig onto func
  mimic(func, orig [, proto]) -- combine the above; proto defaults to orig

Primary Decorators:
  @accepting(proto) -- makes decorated function have proto's signature
  @aliasing -- makes a decorator preserve superficial attributes
  @mimicking -- as aliasing, but also preserve signature

Note that the last two are decorator-decorators; they act on decorators, to
produce decorators that preserve properties of the functions they
decorate.  Naturally, the decorators produced preserve the superficial
attributes (name, doc string, module and anything in __dict__) of the decorators
they enhance; and have the lambda func: None signature of a simple decorator.

Further Decorators:
  @postcompose(post [, ...]) -- compose post after decorated function

[0] http://www.phyast.pitt.edu/~micheles/python/decorator.zip
See also:
 * http://wiki.python.org/moin/PythonDecoratorLibrary
 * the functools module's wraps() decorator.
"""

# Note: all of these are wrapped, below, to hide their tunnelled args !
import inspect
def wrapas(func, proto,
           fetch=inspect.getargspec, format=inspect.formatargspec, isfunc=inspect.isroutine,
           valfmt='=__default_arg_%x', fname='__implementation',
           skip=lambda x: ''):
    """Masks the signature of a function.

    Takes exactly two functions as arguments, func and proto.  Returns a
    function which acts as func but has the signature of proto.  Note that func
    must, in fact, accept being called in any way in which proto supports being
    called.  See accepting, below, for a decorator deploying this.\n"""
    assert isfunc(func)
    n, a, k, d = fetch(proto)
    glob, i = { fname: func }, 0
    # default values might not repr nicely, so tunnel them via glob:
    for v in d or ():
        glob[valfmt[1:] % i] = v
        i += 1
    # OK, now make a function that packages all that:
    text = 'lambda %s: %s%s' % (
        format(n, a, k, tuple(range(i)),
               formatvalue=lambda i, f=valfmt: f % i)[1:-1],
        fname,
        format(n, a, k, d, formatvalue=skip))
    # print 'Wrapping %s as "%s" using globals' % (func.__name__, text), glob.keys()
    return eval(text, glob)
del inspect

def labelas(func, orig):
    """Transcribe superficial details from orig to func.

    Takes exactly two functions as arguments, func and orig.  Transcribes name,
    documentation string, module and anything in __dict__ from orig to func and
    returns func.\n"""
    func.__doc__ = orig.__doc__
    func.__name__ = orig.__name__
    func.__module__ = orig.__module__
    func.__dict__.update(orig.__dict__)
    return func

def inherit(func, orig, join='\n'.join, wrap=wrapas):
    """Makes a re-implementation of a function look like its original.

    Similar to mimic (q.v.) but intended for use by a derived class, where it
    has a method that over-rides one on a base-class, or in other situations
    where a given API is implemented.

    Takes two required arguments: func is the new implementation, orig is the
    canonical form of the API.  Accepts an optional third argument, join, which
    should accept a (possibly empty) list of strings and return a string; it
    defaults to '\n'.join.  The returned function has the prototype of orig, the
    name of func and composite doc string and __dict__.  For __dict__, the
    result prefers func over orig (i.e. it updates first from orig, then from
    func).

    Any documentation from orig is presumed to be definitive; any documentation
    provided by func is presumed to just elaborate on how it implements the
    API.  Thus the composite puts the doc string of orig before that of func. If
    either is empty or None, it is discarded; the list of what remains is passed
    to join, whose return is used as __doc__, unless it is empty (in which case
    None is used).  Since the result's signature shall match orig's, authors
    should write func using the same argument names as orig, so that they'll
    match references in func's immediate doc string, if any.\n"""
    ans = wrap(func, orig)
    assert func.__name__ == orig.__name__
    ans.__name__ = func.__name__
    # TODO: match up base indentation of docs
    ans.__doc__ = join(filter(None, (orig.__doc__, func.__doc__))) or None
    ans.__dict__.update(orig.__dict__)
    ans.__dict__.update(func.__dict__)
    return ans

def mimic(func, orig, proto=None, wrap=wrapas):
    """Makes one function look like another.

    Requires two functions as arguments, func and orig.  Accepts one optional
    third, proto; if omitted (or None, the default) orig is used as
    proto.  Returns a function that performs the computation of func but has the
    signature of proto and orig's name, doc string, module and any attributes
    stored in __dict__.\n"""
    if proto is None: proto = orig
    return labelas(wrap(func, proto), orig)

def funcorator(func):
    """A decorator that repackages a function.

    Takes a single input, func: this should be a callable - typically a
    function.  Returns a repackaging of this function.\n"""
    raise NotImplementedError
def decodeco(decorator):
    """A decorator-decorator.

    Takes a single input, decorator, and repackages it; decorator itself should
    be a callable that takes a callable and returns a repackaging of *that*; the
    result of decorating decorator itself is a similar callable.\n"""
    raise NotImplementedError

def accepting(proto, wrapping=wrapas, heir=inherit, rator=funcorator):
    """Decorator to fake function signature.

    Takes one parameter, proto: only its signature (argument names, defaults and
    whether *more and **keywords are included) matters, so a simple lambda
    expression is sufficient.  Returns a decorator which wraps a function so
    that it can only be called in the ways that proto supports.  The wrapped
    function preserves the name, doc string, module and any attributes stored in
    __dict__ of the function it wraps.

    Example use: when some other decorator has replaced a function with one
    using *args, **kw as signature, this can restore the original.  This can
    equally be used to hide any optional arguments that were intended to serve
    only as tunnels, so as to prevent callers from inadvertently supplying
    surplus arguments that displace these defaults.\n"""
    def decor(func, form=proto, wrap=wrapping):
        return labelas(wrap(func, form), func)
    return heir(decor, rator)

def overriding(base, heir=inherit, wrap=wrapas, rator=funcorator):
    def decor(func, form=base, mime=heir):
        return mime(func, form)
    return heir(decor, rator)

def aliasing(orig, mime=mimic):
    """Decorator-decorator to make original preserve superficial details.

    Single argument, orig, is a decorator; return is a replacement decorator
    with the same behaviour except that the results of its decoration retain the
    name and doc-string of the functions they package.\n"""
    def decor(func, base=orig, label=labelas):
        return label(base(func), func)
    return mimic(decor, orig, lambda func: None)

def mimicking(orig, mime=mimic):
    """Decorator-decorator to make original's wrappers look like what they wrap.

    Takes one argument, a decorator; returns a replacement decorator that
    preserves the signature, name, doc string, module and anything in __dict__
    of each function decorated.\n"""

    def decor(func, base=orig, fake=mime):
        return fake(base(func), func)
    return mime(decor, orig, lambda func: None)

def postcompose(post, *more):
    """Apply some post-processing to every output of a function.

    Each argument must be a function.  Returns a decorator which, given a
    function f, turns it into a function that is called exactly as f is but the
    return from f is passed through each function from the decorator in turn.
    Thus, following:
        @postcompose(a, b, c)
        def f(x): return x**2
    a call to f(n) shall return c(b(a(n**2))).

    For example, @postcompose(tuple) will turn a function that returns a list
    (e.g. because that's the easiest way to compute the sequence it wants to
    return) into one that returns a tuple that freezes that list.

    Equally, one can use postcompose itself as a decorator: it turns the
    decorated function into a decorator which post-processes the returns of
    functions to which *it* is applied.\n"""
    @mimicking
    def decor(func, after=(post,) + more):
        def ans(*args, **kw):
            ret = func(*args, **kw)
            for f in after: ret = f(ret)
            return ret
        return ans
    return decor

# Hide tunnelled args:
wrapas = mimic(wrapas, wrapas, lambda func, proto: None)
accepting = mimic(accepting, accepting, lambda proto: None)
aliasing = mimic(aliasing, aliasing, lambda orig: None)
mimicking = mimic(mimicking, mimicking, lambda orig: None)
mimic = mimic(mimic, mimic, lambda func, orig: None)
