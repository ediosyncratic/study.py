"""Lazily-evaluated sequence.

See study.LICENSE for copyright and license information.
"""
from study.snake.sequence import ReadSeq

class LazySeq (ReadSeq):
    def __init__(self, gen):
        """Initialize a lazily-evaluated sequence.

        Takes one (required) argument, an iterator whose successive .next()
        calls shall yield the entries in our sequence.  It shall only be
        iterated as far as it is actually needed.\n"""
        self.__src = gen
        self.__cache = []

    __upget = ReadOnlySeq.__getitem__
    from study.snake.regular import Slice
    def __getitem__(self, i, S=Slice):
        if isinstance(i, slice): i = S(i)
        try: iter(i)
        except TypeError: pass
        else: return self.__upget(i)

        while i >= len(self.__cache):
            self.__cache.append(self.__src.next())
        return self.__cache[i]
    del Slice
