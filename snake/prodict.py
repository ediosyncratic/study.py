"""Tool class useful in various contexts.

$Id: prodict.py,v 1.1 2008-05-12 06:10:35 eddy Exp $
"""


class Prodict (dict):
    """A dictionary modelling a product of powers.

    Keys are understood as opaque objects notionally (but not necessarily
    realizably) amenable to multiplication; each value is a multiplicity, so the
    whole dictionary denotes the product, over k, v in .items(), of k**v.\n"""

    __upcopy = dict.copy
    def copy(self): return prodict(self.__upcopy)

    __upget = dict.__getitem__
    def __getitem__(self, key, default=0):
        try: return self.__upget(key)
        except KeyError: return default
    get = __getitem__

    __upset = dict.__setitem__
    def __setitem__(self, k, v):
        if v:
            # Coerce key to int if valid
            if type(v) not in (int, long) and v == int(v): v = int(v)
            self.__upset(k, v)
        else:
            try: del self[k]
            except KeyError: pass

    def __mul__(self, other):
        bok = self.copy()
        for k, v in other.items():
            bok[k] = bok.get(k, 0) + v

        return bok

    def __div__(self, other):
        bok = self.copy()
        for k, v in other.items(): bok[k] = bok.get(k, 0) - v
        return bok

    def __pow__(self, n): # Third arg would be modulo
        bok = prodict({})

        # Raising to zero power yields 1
        if n:
            for k, v in self.items():
                bok[k] = n * v

        return bok
