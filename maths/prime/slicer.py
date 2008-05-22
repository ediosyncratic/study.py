"""Slice objects of the prime object.
"""
from study.snake.sequence import Iterable
from study.snake.interval import Slice

class PrimeSlice (Iterable):
    def __init__(self, boss, seq, S=Slice(0, None, 1)):
        self.__boss, self.__slice = boss, S.trim(seq)

    def __len__(self): return len(self.__slice)

    def index(self, value, lo=0, hi=None):
        lo = self.__slice[lo]
        if hi is None: hi = self.__slice.stop
        else: hi = self.__slice[hi]
        return self.__boss.index(value, lo, hi)

    def __getitem__(self, key, S=Slice):
        if isinstance(key, slice) or isinstance(key, S):
            return PrimeSlice(self.__boss, self.__slice[key])
        # actually, that's sort of redundant, since this would do the same:
        return self.__boss[self.__slice[key]]

    def __ontains__(self, value):
        # TODO: make this more efficient ! e.g. binary chop ?
        step = self.__slice.step

        for entry in self:
            if entry == value: return True
            if not value % entry: break
            if step < 0:
                if entry < value: break
            elif step > 0:
                if entry > value: break
            else: break

        return False

    def __iter__(self):
        try:
            for i in self.__slice:
                yield self.__boss[i]

        except IndexError:
            raise StopIteration

del Iterable, Slice
