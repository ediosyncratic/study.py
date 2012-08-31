"""Decorators to frob locale.

Changing locale settings is generally unwise - see the documentation of
the built-in module locale - but at least we can arrange to do it
tidily, when we must.  This module provides one decorator-creator:

@withlocale(category, locale)
def name(what, ever): ...

will cause all calls to name(...) to be preceded by a locale-change (any
error in this shall cause the call to name(...) to be aborted) and
followed by a locale-restore; otherwise, name() shall accept the same
signature, perform the same computation and return the same result as
the written implementation.

See study.LICENSE for copyright and license information.
"""

from study.snake.decorate import mimicking, accepting
from locale import setlocale

@accepting(lambda category, locale: None)
def withlocale(category, locale, setloc=setlocale, mimic=mimicking):
    """Decorator to make a function be called in a specified locale.

    Takes two arguments, the same as for locale.setlocale(), q.v., when
    called to set (rather than just query) a category.  Returns a
    decorator which, given a function, turns it into one with the same
    signature, doc string argument names and return value, but wraps its
    call in a prior call to setlocale() with the given arguments and a
    final call to restore the prior locale.

    Use with care: setting locale can be expensive - see the locale
    module's documentation.\n"""

    @mimic
    def deco(func):
        def rated(*args, **kw):
            prior = setloc(category)
            setloc(category, locale) # may raise locale.Error
            try: return func(*args, **kw)
            finally: setloc(category, prior)
        return rated
    return deco

del setlocale, mimicking, accepting
