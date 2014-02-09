"""Generate pythagorean triangles.

See study.LICENSE for copyright and license information.
"""

def whole(i, j):
    """Generate a pythagorean triangle.

    Takes two naturals, i and j, with i > j.  Returns the tuple of three
    entries given by:
        2 * (i * (i + 1) + (j + 1) * j) + 1
        2 * (i * (i + 1) - (j + 1) * j)
        (2 * i + 1) * (2 * j + 1)

    These are the sides of a right-angle triangle, as may be verified by summing
    the squares of the later two and comparing to the square of the first (see
    http://www.chaos.org.uk/~eddy/math/pythagorean.html).  Of course, any
    scaling of one of these is also a pythagorean triangle.  See
    Triangles.coprime() for the same triangle rescaled to make its sides
    mutually coprime.  Every pythagorean triangle has this form, for some i, j
    and scaling.\n"""
    assert i > j
    ans = ( 2*(i*(i+1) +(j+1)*j) +1, 2*(i*(i+1) -(j+1)*j), (2*i+1)*(2*j+1) )
    assert ans[1]**2 +ans[2]**2 == ans[0]**2
    return ans

from study.snake.sequence import Iterable, Ordered
class Triangles (Iterable):
    """Iterator over known pythagorean triangles."""
    def __init__(self): self.__nxt = self.__iter()
    def __iter__(self): return self
    def next(self): return self.__nxt.next()

    __cache = Ordered(unique=None) # ValueError on appending duplicates
    @classmethod
    def __iter(cls):
        for hij in cls.__cache: yield hij
        extra = [] # TODO - FIXME: proper safety under async access by many instances
        cls.__fresh.append(extra)
        try:
            for i, j in cls.__src:
                hac = cls.coprime(i, j)
                try: cls.__append(hac)
                except ValueError: pass
                else: yield hac

                # Now pick up anything anyone else append()ed recently:
                while extra:
                    it = extra.pop()
                    # Ignore the one *we* appended, of course:
                    if it != hac: yield it
        finally:
            cls.__stale(extra)

    def pairs(): # Local tool function
        """Yield all pairs of naturals i, j with i > j."""
        i = 1
        while True:
            j = i = i + 1
            while j > 1:
                j -= 1
                yield  i, j
    __src = pairs()
    del pairs

    @classmethod
    def __append(cls, val):
        cls.__cache.append(val)
        # If that didn't ValueError:
        for it in cls.__fresh: it.append(val)

    __fresh = []
    @classmethod
    def __stale(cls, xtr):
        i, seq = 0, cls.__fresh
        while True:
            try:
                if seq[i] is xtr:
                    got = seq.pop(i)
                    if got is xtr: break
                    # unless asynchronous stuff happened !
                    else: seq.insert(0, got)
                    # BUG: anything appended between pop and append got missed :-(
            except IndexError: i = 0
            else: i += 1

    from natural import hcf as __hcf
    __hcf = staticmethod(__hcf)

    @classmethod
    def coprime(cls, i, j):
        """Returns a pythagorean triangle whose sides are coprime.

        See whole() for details; this takes the same parameters.  Its return
        is that from whole with all common factors eliminated.  The middle
        member of the returned tuple is always even, the other two odd.\n"""
        h, a, c = whole(i, j)
        f = cls.__hcf(h, a, c)
        return h/f, a/f, c/f

del Ordered, Iterable
coprime = Triangles.coprime # deprecated; use Triangles.coprime instead

class Pythagorean (object):

    def __init__(self, dim):
        self.__dim = dim
