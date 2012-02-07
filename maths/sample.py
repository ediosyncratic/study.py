"""Analysing a statistical sample.
"""

from study.cache.property import lazyprop

class Sample (tuple):
    @lazyprop
    def order(self, cls=None):
        assert cls is None
        from study.maths.permute import order
        return order(self)

    @lazyprop
    def sorted(self, cls=None):
        assert cls is None
        return self.order.permute(self)

    @lazyprop
    def span(self, cls=None):
        assert cls is None
        all = self.sorted
        return all[0], all[-1]

    @lazyprop
    def prop(self, cls=None):
        assert cls is None

    @staticmethod
    def __diff(seq):
        "Differences between adjacent members of a sequence"
        return map(lambda x, y: y - x, seq[:-1], seq[1:])

    def density(self, cuts):
        idx = self.partition(cuts)
        return map(lambda x, y: x / y,
                   self.__diff(idx), self.__diff(cuts))

    def partition(self, cuts):
        assert not filter(None, map(lambda x, y: y <= x, cuts[:-1],
                                    cuts[1:])), "Mis-ordered cuts"
        all, idx, j = self.sorted, [], 0
        # all[idx[i]-1] <= cuts[i] < all[idx[i]], strict where possible
        for cut in cuts:
            try:
                while all[j] <= cut: j += 1
            except IndexError: assert j == len(self)
            idx.append(j)

        return idx

    def blocks(self, cuts):
        """The data you need to draw self partitioned using cuts

        Returns a sequence of tuples, one per interval between entries in
        cuts, of form (start, width, weight), where start is an entry in cuts
        (and is not the last), width is the next entry minus start and weight
        is the number of entries in self between these two entries, divided by
        width.\n"""
        return tuple(map(lambda *args: args,
                         cuts[:-1], self.__diff(cuts), self.density(cuts)))

    # Assorted ways to get lists of cuts for such use:
    @staticmethod
    def __half(value): # contrast __mid, below
        if (value // 2) * 2 == value:
            return value // 2
        return value * 0.5

    def niles(self, n):
        """Returns values that evenly partition self's entries.

        Single parameter, n, is the number of ranges into which self.span is
        to be sub-divided.  Returns a tuple T of values, in increasing order,
        such that the number of entries in self that are < T[i], when
        multiplied by n, is <= i*len(self) while the number of entries > T(i),
        when multiplied by n, is <= (n-i)*len(self).

        Note that T[0] is constrained by the specification to be <= all
        entries in self; a matching T(n) is also included, >= all entries in
        self.\n"""

        if n < 1:
            raise ValueError("Senseless number of sub-ranges", n)
        if not self:
            raise ValueError("A range needs values", self, n)

        if len(self) == 1: return ( self[0], ) * (1 + n)

        all, cuts, i = self.sorted, [], 1
        if n > 1:
            while i < n:
                j = (len(self) * i) // n
                if j < 1: cut = all[0]
                else: cut = self.__half(all[j] + all[j-1])

                if i == 1: # prepend the i = 0 entry:
                    cuts.append(all[0] + cut - all[j])
                cuts.append(cut)
                i += 1
        else:
            cut = self.__half(all[0] + all[1])
            cuts.append(all[0] + cut - all[0])
            cut = self.__half(all[-2] + all[-1])
            j = -1

        cuts.append(all[-1] + all[j] - cut)
        return tuple(cuts)

    @staticmethod
    def __mid(lo, hi, base):
        """Returns a terse number between lo and hi.

        Requires three arguments: a lower bound, an upper bound and a (whole
        number) base.  The lower bound must be strictly less than the upper
        bound.  The result is roughly half way between lo and hi and a
        multiple of 1./base**n for the least natural n for which this is
        possible.\n"""

        gap = 1.
        assert lo < hi
        while lo + gap > hi: gap /= base
        n = (1 + (hi - lo) / gap) // 2
        ans = lo - lo % gap + n * gap
        assert lo <= ans < hi, (lo, hi, gap, n, ans)
        return ans

    @classmethod
    def __split(cls, i, idx, cuts, all, c, s, base):
        """Try to split cuts[i:i+1]

        ... into up to s pieces containing at least c entries.
        See enranged().
        """
        while s > 1:
            gap = cuts[i+1] - cuts[i]
            if (gap // s) * s == gap: gap = gap // 2
            else: gap = gap * 1. / s
            # What we'll insert between cuts[i] and cuts[i+1]:
            knife = map(lambda j, b=cuts[i], g=gap: j * g + b, range(1, s))
            # work out the corresponding indices for idx:
            index, low, m, j = [], idx[i], idx[i+1], s - 1
            # Think of m as index[s-j], even when index is empty
            while j > 0:
                # We need to insert j entries at the start of index
                # For each k in range(j, s),
                # all[index[k-j]-1 <= knife[k] < all[index[k-j]]
                # s/idx[i+1]/index[s-j]/, s/cuts[i+1]/knife[s]/ for k = s.
                if m - c * j < low: break # too few entries in all remain
                j -= 1
                # Find m s.t. all[m-1] <= knife[j] < all[m]
                # Require: m + c < index[0], which is our prior m
                if all[m - c] <= knife[j]: break # too few entries in interval
                m -= c
                while m > low and all[m] > knife[j]: m -= 1
                if m < low + c: break # too few entries below interval
                # Tweak knife[j] to mid-point:
                assert all[m+1] > knife[j]
                knife[j] = cls.__mid(all[m], all[m+1], base)
                m += 1 # restore knife[j] < all[m]
                index.insert(0, m)
            else: # didn't break; success !
                i += 1
                idx[i:i], cuts[i:i] = index, knife
                return True

            s -= 1
        return False

    @classmethod
    def __relax(cls, cuts, idx, all, base):
        """Adjusts cuts to terse values.

        Rounds each cuts[i] to a multiple of 1./base**n (for the smallest n,
        in each case, that allows this) that lies between the same
        all[idx[i]-1] <= cuts[i] < all[idx[i]].\n"""
        i = len(cuts)
        while i > 0:
            i -= 1
            j = idx[i]
            if j < 1: # no lo
                lo, hi = cuts[i], all[j]
                lo -= hi - lo
            elif j < len(all):
                lo, hi = all[j-1], all[j]
            else: # no hi
                lo, hi = all[j-1], cuts[i]
                hi += hi - lo

            if lo < hi: # else: can't help this one
                cuts[i] = cls.__mid(lo, hi, base)
                assert lo <= cuts[i] < hi

    def enranged(self, c=1, s=7, n=None, base=10):
        """Subdivide range of values for mixed range/count equipartition.

        All parameters are optional:
          c -- minimum number of entries in each sub-division; default is 1
          s -- initial number of sub-ranges into which to try splitting any
               given 'big' range; smaller numbers shall be tried if that
               doesn't work out; default is 7.
          n -- number of sub-divisions to use, or None (the default) for no
               limit.
          base -- aim to make cut-points terse when written in this base;
                  defaults to ten.

        Returns a list of cut-points between which to aggregate self's
        values.\n"""
        cuts, idx = list(self.niles(1)), [0, len(self)]
        if n is None: check = lambda m: True
        else: check = lambda m, n=n: m <= n
        all = self.sorted
        self.__relax(cuts, idx, all, base)
        # all[idx[i]-1] <= cuts[i] < all[idx[i]], strict when possible

        while check(len(cuts)):
            gaps = self.__diff(idx)
            wide, i = max(gaps), -1
            while wide > 0:
                try: i = gaps.index(wide, 1 + i)
                except ValueError: wide, i = wide - 1, -1
                else:
                    # idx[i+1] - idx[i] is a maximal candidate for splitting
                    t = min(s, wide / c)
                    if n is not None: t = min(t, 2 + n - len(cuts))
                    if self.__split(i, idx, cuts, all, c, t, base): break
            else: break # wide wound down to 0; no split possible

        return cuts

    def confused(self, r=7, base=10):
        """Group elements so as to get nice density.

        Parameters are optional:
          r -- density ratio limit (defaults: 7); adjacent ranges are required
               to have average densities whose ratios to one another are no
               bigger than it.
          base -- aim to make cut-points terse when written in this base;
                  defaults to ten.

        Returns a list of cut-points between which to aggregate self's
        values.\n"""
        all, cuts, idx = iter(self.sorted), [], []
        # all[idx[i]-1] < cuts[i] < all[idx[i]]
        was, j = all.next(), 0
        for it in all: # self.sorted[j] == was
            j += 1
            if it > was: # self.sorted[j] == it
                cuts.append(self.__mid(was, it, base))
                idx.append(j)
                was = it

        while len(cuts) > 2:
            gaps = self.__diff(cuts)
            dense = map(lambda x, y: x / y, self.__diff(idx), gaps)
            # dense[i] = (idx[i+1] - idx[i]) / (cuts[i+1] - cuts[i])
            ratio = map(lambda x, y: max(x * 1. / y, y * 1. / x),
                        dense[1:], dense[:-1])
            worst = max(ratio)
            if worst < r: break
            j = ratio.index(worst)
            # dense[j] and dense[j+1] are wildly different
            if gaps[j] > gaps[j+1]: j += 1
            # gaps[j] is the narrower of the two
            if j == 0 or (j+1 < len(gaps) and gaps[j-1] > gaps[j+1]): j += 1
            #print "Culling", j, 'from', map(lambda x: '%.2f' % x, dense),
            #print "to kill", worst, 'in', map(lambda x: '%.2f' % x, ratio)
            del idx[j], cuts[j] # the boundary between these two intervals

        all = self.sorted
        # That will never ditch the outermost cuts:
        assert all[0] == all[idx[0] - 1]
        assert all[-1] == all[idx[-1]]

        # Add outliers for start and end:
        cuts.insert(0, self.__outlier(all[0], cuts[0], idx[0],
                                      r, dense[0], dense[1], -1))
        cuts.append(self.__outlier(all[-1], cuts[-1], len(self) - idx[-1],
                                   r, dense[-1], dense[-2], +1))
        return cuts

    @staticmethod
    def __outlier(bound, slice, weight, r, dense, denser, sign):
        # Rough cut, as far beyond bound as slice is after:
        cut = bound * 2 - slice
        d = weight / (slice - cut) # density
        if d > dense * r: # too high !
            # Fix by making interval wider:
            return slice + sign * weight / (dense * r)
        elif d < dense / r: # too low !
            # Fudge factor:
            if dense < denser: u = denser / dense
            else: u = 1
            # Make interval narrower, if we can:
            w = weight / dense
            cut = slice + sign * w * u
            # Check interval straddles bound:
            if bound < cut: cut = slice + sign * w * r
            # If still short, fall back on slightly outside:
            if bound < cut: cut = bound + sign * 1. / dense / r
            # 1/dense/r should be adequately tiny.

        return cut
del lazyprop
