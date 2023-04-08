"""Tool class useful in various contexts.

See study.LICENSE for copyright and license information.
"""
from study.snake.sequence import Dict

class AddDict (Dict):
    """A dictionary modelling a weighted sum.

    Keys are understood as opaque objects notionally (but not
    necessarily realizably) amenable to scaling; each value is a
    scaling, so the whole dictionary denotes

      sum(k*v for k, v in self.items())

    and endeavours to behave as that would, were it realisable.

    Such a dictionary can, naturally, alternatively serve as a
    description of a product of powers instead of a weighte sum.
    """

    @property
    def frozen(self):
        """Returns a frozenset representation of self.

        Since self is mutable, we can't use it as a key in a mapping,
        but we can use a frozenset of its items.  This can be
        converted back to a duplicate of self by passing it to the
        AddDict constructor.
        """
        return frozenset(self.items())

    __upget = Dict.__getitem__
    def __getitem__(self, key, default=0):
        try: return self.__upget(key)
        except KeyError: return default
    get = __getitem__

    __upset = Dict.__setitem__
    def __setitem__(self, k, v):
        if v:
            # Coerce key to int if valid
            try:
                u = int(v)
                if u == v:
                    v = u
            except (TypeError, ValueError):
                pass
            self.__upset(k, v)
        else:
            try: del self[k]
            except KeyError: pass

    def __iadd__(self, other):
        for k, v in other.items():
            self[k] = self[k] + v
        return self

    def __add__(self, other):
        bok = self.copy()
        bok *= other
        return bok
    __radd__ = __add__

    def __isub__(self, other):
        for k, v in other.items():
            self[k] = self[k] - v
        return self

    def __sub__(self, other):
        bok = self.copy()
        bok -= other
        return bok

    def __rsub__(self, other):
        bok = self._iterdict_(other.copy())
        bok -= self
        return bok

    def __imul__(self, other):
        for k in self:
            self[k] *= other
    def __mul__(self, other):
        result = self.copy()
        result *= other
        return result

    def __idiv__(self, other):
        for k in self:
            self[k] /= other
    def __div__(self, other):
        result = self.copy()
        result /= other
        return result

    def __divmod__(self, other):
        remain = self.copy({})
        quotient = remain.copy()
        for k in self:
            quotient[k], remain[k] = divmod(self[k], other)
        return quotient, remain

    __rmul__ = __mul__
    __itruediv__ = __idiv__
    __truediv__ = __div__
