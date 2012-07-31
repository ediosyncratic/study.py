"""Miscellaneous things to do with exceptions.

  exceptionlist([bok]) -- list exceptions names (or keys in bok with exception values)
  printexceptions([es]) -- pretty-print table of exceptions (from list)
  showexception(exc [, pre]) -- print information about (usually) an exception
"""

def exceptionlist(bok=None):
    """Returns a list of exception names.

    Optional argument, bok, maps names to values; otherwise, the builtin
    name-space is used; each name whose value is an exception is returned.\n"""
    if bok is None:
        try: row = __builtins__.items()
        except AttributeError:
            row = map(lambda nom: (nom, getattr(__builtins__, nom)), dir(__builtins__))
    else: row = bok.items()

    def ok((k, v), b=Exception, t=type(Exception)):
        return isinstance(v, t) and issubclass(v, b) # ignoring k
    return map(lambda (k,v): k, filter(ok, row))

from show import printmenu
def printexceptions(row=None, *args, **what):
    """Display exceptions

    Optional first argument is a sequence of strings; exceptionlist() is used by
    default.  All other positional and keyword arguments are passed with it to
    show.printmenu (q.v.); the strings are printed out in a tidy table.\n"""
    if row is None: row = exceptionlist()
    return printmenu(row, *args, **what)

def showexception(exc, preamble=None):
    """Display information about an exception.

    Required argument, exc, is an exception of which to print out a description
    (actually, anything else is accepted; its repr is shown).  Optional
    argument, preamble, is something to print first, if given, without an
    intervening newline; for example, the context in which the exception was
    encountered.\n"""
    if preamble: print preamble,
    if isinstance(exc, Exception):
        if exc.__class__.__module__ == 'exceptions':
            cls = exc.__class__.__name__
        else: cls = str(exc.__class__)
        print cls + `exc.args`

    else: print `exc`
