"""Tool class useful in various contexts.

See study.LICENSE for copyright and license information.
"""

# TODO: replace with an additive idiom; see addict.AddDict
class Prodict (dict):
    """A dictionary modelling a product of powers.

    Keys are understood as opaque objects notionally (but not necessarily
    realizably) amenable to multiplication; each value is a multiplicity, so the
    whole dictionary denotes the product, over k, v in .items(), of k**v.\n"""

    def copy(self, other=None, up=dict.copy):
        """Produce a thing just like self, but independently mutable.

        Returns an object of the same kind as self.  Optional argument is a
        mapping object specifying the keys and values to use; if None (the
        default), a shallow copy (i.e. dict.copy) of self is used.

        Wherever this class creates a new object that should be an instance of
        this class, it uses self.copy to do it; thus derived classes can, by
        over-riding copy, add richer semantics to the objects produced, without
        having to over-ride the other methods of this class.  This
        implementation invokes self.__class__ on other (or the shallow copy of
        self, as a dict), which should suffice for most derived classes;
        however, those with messier constructors need to over-ride copy.\n"""
        if other is None: other = up(self)
        return self.__class__(other)

    def __getitem__(self, key, default=0, up=dict.__getitem__):
        try: return up(self, key)
        except KeyError: return default
    get = __getitem__

    def __setitem__(self, k, v, up=dict.__setitem__):
        if v:
            # Coerce key to int if valid
            if not isinstance(v, (int, long)) and v == int(v): v = int(v)
            up(self, k, v)
        else:
            try: del self[k]
            except KeyError: pass

    def __imul__(self, other):
        for k, v in other.items():
            self[k] = self[k] + v
        return self

    def __mul__(self, other):
        bok = self.copy()
        bok *= other
        return bok
    __rmul__ = __mul__

    def __idiv__(self, other):
        for k, v in other.items(): self[k] = self[k] - v
        return self
    __itruediv__ = __idiv__

    def __div__(self, other):
        bok = self.copy()
        bok /= other
        return bok
    __truediv__ = __div__

    def __rdiv__(self, other):
        bok = self.copy(other.copy())
        bok /= self
        return bok
    __rtruediv__ = __rdiv__

    # No whole-division (floordiv, divmod, div); they make no sense.

    def __ipow__(self, n):
        if n:
            for k in self.keys(): self[k] *= n
        else: self.clear() # Raising to zero power yields 1
        return self

    def __pow__(self, n): # Third arg would be modulo; makes no sense.
        bok = self.copy()
        bok **= n
        return bok
