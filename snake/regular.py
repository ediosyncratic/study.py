"""Descriptors for arithmetic series (bounded on at least one side).

$Id: regular.py,v 1.4 2008-06-09 06:58:06 eddy Exp $
"""

class Regular (object):
    """Base-class for arithmetic sub-sequences of the integers.

    Derived classes must implement indexing, the index method, the attributes of
    a slice - start, stop (None for endless) and step - and (where applicable) a
    .last attribute.  For subtraction support, they must also support
    negation.\n"""

    def __contains__(self, ind):
        try: self.index(ind)
        except ValueError: return False
        return True

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

    def __sub__(self, other): return -other + self
    def __rsub__(self, other): return -self + other
    def __neg__(self):
        if self.stop is None: stop = None
        else: stop = -self.stop
        return self.__slice(-self.start, stop, -self.step)

    # Define comparison in terms of "is this true for all values in self ?"
    def __cmp(self, other, no, yes):
        try: ar, op. ep = other.start, other.stop, other.step
        except AttributeError: pass
        else: other = self.__slice(ar, op, ep)
        # Always make other be left operand of recursing compariisons:

        if yes(0, self.step): return yes(other, self.stop)
        if self.stop is None: return False
        if no(other, self.start) or no(other, self.stop - self.step): return False
        if yes(other, self.stop): return True
        return yes(other, self.last)

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

    def __ne__(self, other):
        try: return len(self.trim(other)) == 0
        except TypeError: return other not in self

    def __add__(self, other):
        """Adding arithmetic series.

        This is only feasible if the result is also an arithmetic series.  When
        other is a simple number, this is easy - just offset self.  If either
        other or self is a series with only one value in it, we can replace it
        with that one value.  If either series is empty, the result is empty.

        Otherwise, the set of values self and other contain can only be an
        arithmetic series if: one has, as step, a divisor of the step of the
        second; and the former's length is enough that it straddles a full step
        of the latter.  In this case, we return a series with the appropriate
        members.\n"""
        try: ar, op, ep = other.start, other.stop, other.step
        except AttributeError:
            if self.stop is None: op = None
            else: op = other + self.stop
            return self.__slice(other + self.start, op, self.step)

        if op == ar or self.stop == self.start:
            return Interval(self.start + ar, 0)
        if ep == 0:
            return self + ar
        if self.step == 0:
            return other + self.start

        if op is not None: # properly align it
            q, r = divmod(op - ar, ep)
            if r: q += 1
            if q < 1: return Interval(self.start + ar, 0)
            if q == 1: return self + ar
            op = ar + q * ep

        stop = self.stop
        if stop is not None: # align suitably
            q, r = divmod(stop - self.start, self.step)
            if r: q += 1
            if q < 1: return Interval(self.start + ar, 0)
            if q == 1: return other + self.start
            stop = self.start + q * self.step

        if ep % self.step == 0 and len(self) >= abs(ep // self.step):
            start, step = self.start, self.step
        elif self.step % ep == 0 and len(other) >= abs(self.step // ep):
            start, stop, step, ar, op, ep = ar, op, ep, self.start, stop, self.step
        else:
            raise ValueError, 'Unrepresentable sum of arithmetic series'

        if step * ep > 0:
            if stop is None or op is None:
                return self.__slice(start + ar, None, step)
        else:
            assert step * ep < 0
            if stop is None:
                if op is None: raise ValueError, \
                   'Sum of arithmetic series has neither upper nor lower bound'
                return self.__slice(start + op - ep, None, step)
            elif op is None:
                return self.__slice(stop - step + ar, None, -step)

        assert stop is not None and op is not None
        return self.__slice(ar + start, op - ep + stop, step)

    __radd__ = __add__

    from study.maths.natural import Euclid
    def trim(self, other, Euclid=Euclid):
        """Returns intersection of self and other.

        Required single argument (do *not* pass any more) is another Regular (or
        a slice); returns a Regular describing the entries in this other that
        are also in self.\n"""
        try: ar, op, ep = other.start or 0, other.stop, other.step
        except AttributeError: raise TypeError("Can't intersect", self, other)
        if ep is None: ep = 1

        i, j = Euclid(ep, self.step)
        j = -j
        h = i * ep - j * self.step # highest common factor
        if h: q, r = divmod(self.start - ar, h)
        elif self.start != ar or len(self) == 0 or len(other) == 0:
            return Interval(ar, 0)
        else: return self.__slice(ar, op, h) # same as self or other

        m = ep * self.step / h # lowest common multiple
        if self.step < 0: m = -m # so our result has the direction of other
        if r or self < other or self > other:
            return self.__slice(ar+m, ar, m) # empty, but with right step

        s = q * i * ep + ar # in the same arithmetic sequence as the intersection
        assert s == self.start + q * j * self.step
        # intersection is {s + k * m: lo <= k < hi} for some integers lo, hi
        # what's the least ar + ep * n, for natural n, in that set ?
        if m:
            q, r = divmod(s - ar, m)
            s -= q * m # so now s is ar + r, other's earliest s + k * m
            if s not in other: return self.__slice(s, op, m) # empty
        elif s in self and s in other: return self.__slice(s, s+1, m) # singleton
        else: return self.__slice(s, s, m) # empty
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

        return self.__slice(s, hi, m)

    del Euclid

    @staticmethod
    def __slice(start, stop, step):
        if step == 1:
            if stop is None: return Interval(start, None)
            return Interval(start, stop - start)
        elif step == -1 and stop is not None:
            return Interval(stop + 1, start - stop)
        return Slice(start, stop, step)

class Interval (Regular):
    def __init__(self, start, span): self.__start, self.__span = start, span
    def __repr__(self): return 'Interval(%s, %s)' % (self.__start, self.__span)

    from infinite import Aleph0
    def __len__(self, huge=Aleph0):
        if self.__span is None: return huge
        return self.__span
    del Aleph0

    def __iter__(self):
        i = 0
        while self.__span is None or i < self.__span:
            yield i + self.__start
            i += 1

        raise StopIteration

    def __getattr__(self, key):
        if key == 'step': return 1
        if key == 'start': return self.__start
        if key == 'stop':
            if self.__span is None: return None
            return self.__start + self.__span
        if key == 'last':
            if self.__span is None:
                raise AttributeError, 'No last element in infinite Interval'
            return self.__start + self.__span - 1

        raise AttributeError(key)

    def __getitem__(self, ind):
        if ind < 0 or (self.__span is not None and ind >= self.__span):
            raise IndexError(ind, self.__span)
        return ind + self.__start

    def index(self, ind):
        ans = ind - self.__start
        if ans < 0: raise ValueError(0, 'Not in range', ind, self)
        if self.__span is not None and ans >= self.__span:
            raise ValueError('Not in range', ind, self)
        if ans != int(ans): raise ValueError(int(1+ans), 'Not in range', ind, self)
        return int(ans)

class Slice (Regular):
    """Make a slice behave a bit more like a range.

    This isn't always what you want; for example, range(8)[slice(-2,3)] is []
    rather than [6, 7, 0, 1. 2] and range(8)[slice(3,-2)] is [3, 4, 5] not [];
    the following's way of thinking about slices regards the latter as the right
    answer, in each case.\n"""

    # Can't actually use slice as a base class, so fake it:
    def __init__(self, *args): self.__seq = slice(*args)
    def __repr__(self): return 'S' + repr(self.__seq)[1:]

    def __getattr__(self, key):
        if key == 'last':
            if self.__seq.step == 0 and self.__seq.stop != self.start:
                return self.start # Sequence is actually infinite, yet has well-defined .last
            if self.__seq.stop is None:
                raise AttributeError, 'No last element in infinite Slice'
            q = len(self)
            if q > 0: return self.start + self.step * q
            raise AttributeError, 'No last element in empty slice'

        ans = getattr(self.__seq, key) # raises AttributeError if needed
        if ans is None:
            if key == 'start': return 0
            elif key == 'step': return 1
        return ans

    from infinite import Aleph0
    def __len__(self, infinity=Aleph0):
        if self.__seq.stop is None: return infinity
        if self.__seq.stop == self.start: q, r = -1, self.step
        elif self.__seq.step == 0: return infinity
        else: q, r = divmod(self.stop - self.start, self.step)
        if r: q += 1
        return max(q, 0)
    del Aleph0

    def __getitem__(self, key):
        if isinstance(key, (slice, Slice)):
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

        ind = key * self.step + lo
        if self.stop is None or (self.stop - ind) * self.step > 0:
            return ind

        raise IndexError(key)

    def index(self, ind):
        step, lo = self.step, self.start
        if step: q, r = divmod(ind - lo, step)
        else: q, r = 0, ind - lo

        if step and r: pass
        elif self.stop is None:
            if ind == lo: return 0
            if (ind - lo) * step > 0: return q
        elif self.stop == lo: pass
        elif step > 0:
            if lo <= ind < self.stop: return q
        elif step < 0:
            if lo >= ind > self.stop: return q
        elif ind == lo: return q

        if q < 0:
            if step < 0: q = len(self)
            else: q = 0
        elif q > len(self):
            if step < 0: q = 0
            else: q = len(self)
        elif step: q += 1
        raise ValueError(int(q), 'Not in range', ind, self)