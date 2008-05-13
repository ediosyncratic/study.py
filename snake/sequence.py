"""Base-class to mix in certain useful features to sequence types.

$Id: sequence.py,v 1.1 2008-05-13 21:36:30 eddy Exp $
"""

class Iterable (object):
    """Mix-in class to extend iterables in some handy ways.

    Implements map, reduce, sum, product and filter as methods of the iterable.
    """
    def map(self, func):
        ans = []
        for p in self:
            ans.append(func(p))
        return ans

    def reduce(self, func, init=0):
        for p in self:
            init = func(init, p)
        return init

    def sum(self): return self.reduce(lambda x, y: x + y)
    def product(self): return self.reduce(lambda x, y: x * y, 1)

    def filter(self, test=None):
        if test is None: return self
        ans = []
        for p in self:
            if test(p):
                ans.append(p)
        return ans

    # The default __contains__ simply iterates over the list testing ==

class WrapIterable (Iterable):
    def __init__(self, seq): self.__seq = seq
    def __iter__(self): return iter(self.__seq)
    def __getattr__(self, key): return getattr(self.__seq, key)

class Slice (object):
    """Make a slice behave a bit more like a range.

    This isn't always what you want; for example, range(8)[slice(-2,3)] is []
    rather than [6, 7, 0, 1. 2] and range(8)[slice(3,-2)] is [3, 4, 5] not [];
    the following's way of thinking about slices regards the latter as the right
    answer, in each case.\n"""

    # Can't actually use slice as a base class, so fake it:
    def __init__(self, *args): self.__seq = slice(*args)
    def __repr__(self): return 'S' + repr(self.__seq)[1:]
    def __getattr__(self, key):
        ans = getattr(self.__seq, key)
        if key == 'start' and ans is None: return 0
        if key == 'step' and ans is None: return 1
        return ans

    # Iteration:
    def __iter__(self):
        val, step = self.start, self.step

        if self.stop is None:
            def check(ind): return True
        elif self.stop == val:
            def check(ind): return False
        elif step > 0:
            def check(ind, n=self.stop): return ind < n
        elif step < 0:
            def check(ind, n=self.stop): return ind > n
        else:
            def check(ind): return True

        while check(val):
            yield val
            val = val + step

        raise StopIteration

    from infinite import Aleph0
    def __len__(self, infinity=Aleph0):
        if self.stop is None or not self.step: return infinity
        return divmod(self.stop - self.start, self.step)[0]
    del Aleph0

    def __last(self):
        try: ans = self.__last_val
        except AttributeError:
            assert self.stop is not None, 'Condition should always be ensured by caller'
            ans = self.start + ((self.stop - self.start) // self.step) * self.step
            self.__last_val = ans
        return ans

    def __getitem__(self, key):
        if isinstance(key, slice) or isinstance(key, Slice):
            # Presume key.start, key.stop are +ve (or None)
            # TODO: decide what, if anything, makes sense for -ve, esp. for mixed
            lo, step = self[key.start or 0], key.step
            if step is None: step = 1

            try:
                if key.stop is None: raise IndexError
                hi = self[key.stop]
            except IndexError:
                if step > 0: hi = self.stop
                elif self.step > 0: hi = self.start - 1
                else: hi = self.start + 1

            return Slice(lo, hi, step * self.step)

        lo = self.start or 0
        if not key: return lo

        if self.step is None: step = 1
        else: step = self.step
        ind = key * step + lo
        if self.stop is None or (self.stop - ind) * step > 0:
            return ind

        raise IndexError(key)

    from study.maths.natural import Euclid
    def trim(self, other, Euclid=Euclid):
        """Returns intersection of self and other.

        Required single argument (do *not* pass any more) is another Slice (or
        slice); returns a Slice describing the entries in this other that are
        also in self (traversed in the same order as other gave them).\n"""
        try: ar, op, ep = other.start or 0, other.stop, other.step
        except AttributeError: raise TypeError("Can't intersect", self, other)
        if ep is None: ep = 1

        i, j = Euclid(ep, self.step)
        j = -j
        h = i * ep - j * self.step # highest common factor
        if h: q, r = divmod(self.start - ar, h)
        elif self.start != ar or len(self) == 0 or len(other) == 0:
            return Slice(ar+1, ar, 1)
        else: return Slice(ar, op, h) # same as self and other

        m = ep * self.step / h # lowest common multiple
        if self.step < 0: m = -m # so our result has the direction of other
        if r or self < other or self > other:
            return Slice(ar+m, ar, m) # empty, but with right step

        s = q * i * ep + ar # in the same arithmetic sequence as the intersection
        assert s == self.start + q * j * self.step
        # intersection is {s + k * m: lo <= k < hi} for some integers lo, hi
        # what's the least ar + ep * n, for natural n, in that set ?
        if m:
            q, r = divmod(s - ar, m)
            s -= q * m # so now s is ar + r, other's earliest s + k * m
            if s not in other: return Slice(s, op, m) # empty
        elif s in self and s in other: return Slice(s, s+1, m) # singleton
        else: return Slice(s, s, m) # empty
        # intersection is now {s + k * m: lo <= k < hi} for some naturals lo, hi.
        if op is None: hi = None
        else: hi = (op - s) // m
        # 0, hi are now the lower and upper bounds imposed by other
        assert hi > 0, 'We previously checked s is in other'
        # So, how much of that is in self ?

        if self.step * ep < 0:
            ih = (self.start - self.step - s) // m
            if self.stop is None: lo = s - m
            else: lo = (self.stop - self.step - s) // m
        else:
            lo = (self.start - s) // m
            if self.stop is None: ih = None
            else: ih = (self.stop - s) // m

        lo = max(0, lo)
        if hi is None: hi = ih
        elif ih is None: pass
        else: hi = min(hi, ih)
        if hi is not None: hi = s + m * hi
        s += lo * m

        return Slice(s, hi, m)

    def __contains__(self, ind):
        step, lo = self.step, self.start

        if step and (ind - lo) % step: return False
        elif self.stop is None: return ind == lo or (ind - lo) * step > 0
        elif self.stop == lo: return False
        elif step > 0: return lo <= ind < self.stop
        elif step < 0: return lo >= ind > self.stop
        else: return ind == lo

    # Define comparison in terms of "is this true for all values in the slice ?"
    # Always make other be left operand of recursing compariisons, in case it's a slice.
    def __cmp(self, other, no, yes):
        if isinstance(other, slice): other = Slice(other.start, other.stop, other.step)
        if yes(0, self.step): return yes(other, self.stop)
        if self.stop is None: return False
        if no(other, self.start) or no(other, self.stop - self.step): return False
        if yes(other, self.stop): return True
        return yes(other, self.__last())

    def __gt__(self, other): return self.__cmp(other, lambda x, y: x >= y, lambda x, y: x < y)
    def __lt__(self, other): return self.__cmp(other, lambda x, y: x <= y, lambda x, y: x > y)
    def __ge__(self, other): return self.__cmp(other, lambda x, y: x > y, lambda x, y: x <= y)
    def __lt__(self, other): return self.__cmp(other, lambda x, y: x < y, lambda x, y: x >= y)
    def __eq__(self, other): # Only if self has one entry, which equals other; or both empty.
        if len(self) > 1: return False # Can only be equal if self has only one value
        elif len(self) < 1: # sequence is empty
            try: a, o = other.start or 0, other.stop
            except AttributeError: return True # no entry in self isn't equal to it ...
            else: return o == a or (o is not None and (a-o)*e > 0) # Both sequences are empty.

        return other == self.start

    def __ne__(self, other, hcf=gcd, lcm=lcm):
        # true unless there's a common entry
        if len(self) < 1: return True # empty :-)
        if self.step == 0 or len(self) == 1: return other != self.start
        if self < other or other < self: return True

        try: a, o, e = other.start or 0, other.stop, other.step
        except AttributeError: return not (other in self) # simple value
        assert o != a, 'empty should have been both < and > self !'
        if e is None: e = 1
        if not e or len(other) == 1: return not(a in self)
        if (a - self.start) % hcf(e, self.step): return False

        if e == self.step:
            if e > 0:
                return ((o is None or self.start < o) and
                        (self.stop is None or a < self.stop))
            else: # assert: is is not zero
                return ((o is None or self.start > o) and
                        (self.stop is None or a > self.stop))

        # TODO: avoid iterating
        if e * self.step < 0:
            for it in self:
                if it in other: return False
                if (a - it) * e > 0: break
        elif self.stop is not None:
            for it in self:
                if it in other: return False
        elif o is not None:
            for it in other:
                if it in self: return False
        # else: infinitely many terms in each, going same way
        return True
