"""Lazily-evaluated and cache-aware mappings and functions.

Contents:
  LazyDict -- mapping populated with data only when needed
  LazyFunc -- callable that caches responses for an underlying callable

See study.LICENSE for copyright and license information.
"""
from study.snake.sequence import Dict, List

class LazyDict (Dict):
    def __init__(self, each=None, fill=None):
        """Initialize a lazilly-filled dictionary.

        Accepts two optional arguments:
          each -- callable to look up a single entry, or (default) None.
          fill -- callable to initially populate self, or (default) None.

        If fill is not None, the first look-up in self shall begin by calling
        self.update(fill()), so fill can be used to populate self (e.g. by
        parsing a file on disk).  This can be convenient if a dictionary object,
        that would be expensive to populate, must be created before it is
        practical to know whether it shall actually be needed.

        If each is not None and an attempt to access self[key] fails, for some
        key, then each(key) shall be called: whatever it returns is saved as
        self[key] and returned.  If each cannot provide a value for key, it
        should raise KeyError (and it shall be called again on all future
        attempts to access self[key]).\n"""

        if fill is not None: self.__fill = fill
        if each is not None: self.__each = each

    __upget = dict.__getitem__
    def __getitem__(self, key):
        try: f = self.__fill
        except AttributeError: pass
        else:
            del self.__fill
            self.update(f())

        try: return self.__upget(key)
        except KeyError: pass

        try: e = self.__each
        except AttributeError:
            raise KeyError(key)

        self[key] = ans = e(key)
        return ans

class LazyFunc (object):
    """Wrapper for a function, to cache its values.

    Use class method wrap() to wrap a function unless you know the function
    isn't already cached; otherwise, you'll duplicate the cache !\n"""
    @classmethod
    def wrap(cls, func):
        """Use this in preference to direct construction."""
        return func if isinstance(func, cls) else cls(func)

    def __init__(self, func):
        self.__func = func
        self.__cache = self.__bok()

    @staticmethod
    def __bok(D=Dict): return D()
    def __seq(self, S=List): return S(self.__cache.iteritems())

    def __call__(self, *args, **what):
        key = (args, tuple(what.items()))
        try: ans = self.__cache[key]
        except KeyError:
            ans = self.__cache[key] = apply(self.__func, args, what)
        return ans

    def known(self):
        """Returns an iterator over known (key, value) pairs.

        Note that the keys are (args, tuple(kws.items())) twoples from calls
        to func(*args, **kws), where the caller likely thinks of args[0], or
        args[:n] for some small n, as the input to the cached function,
        func.  If so, client code should wrap this method and use the .map()
        method the returned study.snake.sequence.Iterable supports.\n"""
        return self.__cache.iteritems()

    def flush(self, keep=0, are=lambda (k, v), (h, u): cmp(abs(v), abs(u))):
        """Forget surplus cached values.

        Arguments are optional:
          keep -- number of cache entries; defaults to 0
          are - comparison function, taking two (input, output) pairs; should
                return -1 if you'd sooner remeber the first pair, +1 if you'd
                sooner keep the later pair or 0 if you don't care.

        After a call to .flush(keep), previously-evaluated calls to the
        function self packages shall be evaluated again, if needed.\n"""
        if len(self.__cache) > keep:
            for (k, v) in self.__seq().sorted(are):
                if keep > 0: keep -= 1
                else: del self.__cache[k]

del List, Dict
