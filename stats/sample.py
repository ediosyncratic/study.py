"""Analysing a statistical sample.

TODO: this has a bunch of overlap with Interpolator; factoring it into a
suitable base-class would not be unreasonable.

See study.LICENSE for copyright and license information.
"""

from study.cache.property import lazyprop
from study.snake.sequence import Tuple

class Sample (Tuple):
    @lazyprop
    def span(self):
        full = self.sorted
        return full[0], full[-1]

    @staticmethod
    def __diff(seq, step=1):
        """Differences between successive members of a sequence.

        Required argument, seq, is a sequence of values.  Optional argument
        step (default: 1) is the index-difference to use.  Returns a list d
        with d[i] = seq[i+step]-seq[i] for each i at which this is valid (so
        len(d) = len(seq) step for step > 0).

        Note that a negative value for step will be read as len(self)-step;
        this might not be as useful as you'd suppose.  If you want reverse
        steps, use a positive value for step and negate each entry in the
        result.  A step of zero will is silly but allowed; you'll get the same
        as [x - x for x in seq].\n"""
        return [y - x for x, y in zip(seq[:-step], seq[step:])]

    def partition(self, cuts):
        assert all(x < y for x, y in zip(cuts[:-1], cuts[1:])), "Mis-ordered cuts"
        full, idx, j = self.sorted, [], 0
        # full[idx[i]-1] <= cuts[i] < full[idx[i]], strict where possible
        for cut in cuts:
            try:
                while full[j] <= cut: j += 1
            except IndexError: assert j == len(self)
            idx.append(j)

        return idx

    def blocks(self, cuts, blur=0):
        """The data you need to draw self partitioned using cuts.

        Required arguments, cuts, is an iterable (or iterator) over the
        boundary values of the intervals between which to average self's
        density.  Optional argument blur (default: 0) is the number of
        intervals, on either side of each interval, to include with it when
        averaging; so the density on each interval will be the average over
        the 1+2*blur intervals of which it is the middle portion.  However,
        for intervals within blur extras of an end, the number of intervals to
        the relevant end is used in place of blur (symmetrically).

        Returns a sequence of tuples, one per interval between entries in
        cuts, of form (start, width, weight), where start is an entry in cuts
        (and is not the last), width is the next entry minus start and weight
        is the number of entries in self between these two entries, divided by
        width.\n"""

        cuts = tuple(cuts) # to consume an iterator once
        idx, wide = self.partition(cuts), 1 + 2 * blur
        weigh, gaps = self.__diff(idx, wide), self.__diff(cuts, wide)
        # Missing blur lower-blur intervals at each end:
        while wide > 1:
            wide -= 2
            weigh.insert(0, idx[wide] - idx[0])
            gaps.insert(0, cuts[wide] - cuts[0])
            weigh.append(idx[-1] - idx[-1-wide])
            gaps.append(cuts[-1] - cuts[-1-wide])

        return tuple(zip(cuts[:-1], self.__diff(cuts),
                         # density in each interval:
                         [x / y for x, y in zip(weigh, gaps)]))

    def density(self, n):
        """Compute density using a moving interval.

        Single parameter, n, is the number of self's data-points that should
        lie within the interval about each point, centred on that point, used
        to assess the density at that point.  The symmetric interval, about
        each point, used is the minimal-width one with >= n data-points in it
        or at its boundary; when there are data-points on its boundary, its
        total may be > n and its interior shall contain < n; even so, it is
        deemed to contain exactly n data-points for purposes of computing the
        density.  The density is continuous everywhere, but non-smooth (it has
        discontinuous gradient) at each mid-point of an interval between
        data-points with n-1 or n-2 others strictly between them; the density
        is locally maximal (n-1) or minimal (n-2) at these points.

        ValueError is raised if n < 2 (intervals are undefined or some are
        zero-width) or (not enough data-points) n > len(self).  Otherwise,
        returns a list of (value, density) points at which the density is not
        smooth, starting and ending with (value, 0) points approximating the
        tails by linear drop-off.  The density follows a lambda x: k/(x-c)
        curve in each smooth segment; each tail's approximation uses the
        gradient this would have given at the relevant outermost data-point,
        decreasing away from the last discontinuity to reach zero density at
        the point given.  The density should be deemed zero before the first
        and after the last and may (fairly) sensibly be simply linearly
        interpolated between the returned points otherwise.

        Details of the tail:

        Consider either a boundary data-point at t; let c be the n-th
        data-point inwards, starting with t as first; then, outwards from
        (t+c)/2, the density has form f = lambda x: k/(x-c), where
          * k is -n/2 with t < c and x < (t+c)/2 for a left tail, or
          * k is n/2 with t > c and x > (t+c)/2 for a right tail.

        This has derivative f'(t) = -k/(t-c)**2 at t and we want a tail of
        this gradient passing through the same point as f at x = (t+c)/2,
        where f(x) is 2*k/(t-c), so we're solving
        0 = 2*k/(t-c) -k*(x -(t+c)/2)/(t-c)**2
        i.e. x = 2*(t-c) + (t+c)/2 = t +1.5*(t-c) = 2.5*t -1.5*c

        The last discontinuity point was at (t+c)/2 = t -.5*(t-c), so this is
        three times as far beyond the end of the range of data-points as the
        last discontinuity was within the range.\n"""

        # TODO: work out y-mid-point of each k/(x-c) curve, return the curve
        # between these points instead.

        if n < 2:
            raise ValueError("Too few of data-points per interval", n)
        if n > len(self):
            raise ValueError("Demanding too much", n, len(self))

        i, j, pts = 0, n-1, []
        full = self.sorted
        # t = full[0], c = full[n-1]
        lo, hi = full[i], full[j]
        pts.append((self.__half(5*lo -3*hi), 0))
        pts.append((self.__half(lo +hi), n * 1. / (hi - lo)))
        while i + n < len(self):
            if i + n < j + 1: # we have spare points; advance i
                i += 1
                if full[i] <= lo: continue
                lo = full[i]

            else: # need wider interval
                j += 1
                if full[j] <= hi: continue
                hi = full[j]

            pts.append((self.__half(lo + hi), n * 1. / (hi - lo)))
        pts.append((self.__half(5*hi -3*lo), 0))
        return tuple(pts)

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

        full, cuts, i = self.sorted, [], 1
        if n > 1:
            while i < n:
                j = (len(self) * i) // n
                if j < 1: cut = full[0]
                else: cut = self.__half(full[j] + full[j-1])

                if i == 1: # prepend the i = 0 entry:
                    cuts.append(full[0] + cut - full[j])
                cuts.append(cut)
                i += 1
        else:
            cut = self.__half(full[0] + full[1])
            cuts.append(full[0] + cut - full[0])
            cut = self.__half(full[-2] + full[-1])
            j = -1

        cuts.append(full[-1] + full[j] - cut)
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
    def __split(cls, i, idx, cuts, full, c, s, base):
        """Try to split cuts[i:i+1]

        ... into up to s pieces containing at least c entries.
        See enranged().
        """
        while s > 1:
            gap = cuts[i+1] - cuts[i]
            if (gap // s) * s == gap: gap = gap // 2
            else: gap = gap * 1. / s
            # What we'll insert between cuts[i] and cuts[i+1]:
            knife = [j * gap + cuts[i] for j in range(1, s)]
            # work out the corresponding indices for idx:
            index, low, m, j = [], idx[i], idx[i+1], s - 1
            # Think of m as index[s-j], even when index is empty
            while j > 0:
                # We need to insert j entries at the start of index
                # For each k in range(j, s),
                # full[index[k-j]-1 <= knife[k] < full[index[k-j]]
                # s/idx[i+1]/index[s-j]/, s/cuts[i+1]/knife[s]/ for k = s.
                if m - c * j < low: break # too few entries in full remain
                j -= 1
                # Find m s.t. full[m-1] <= knife[j] < full[m]
                # Require: m + c < index[0], which is our prior m
                if full[m - c] <= knife[j]: break # too few entries in interval
                m -= c
                while m > low and full[m] > knife[j]: m -= 1
                if m < low + c: break # too few entries below interval
                # Tweak knife[j] to mid-point:
                assert full[m+1] > knife[j]
                knife[j] = cls.__mid(full[m], full[m+1], base)
                m += 1 # restore knife[j] < full[m]
                index.insert(0, m)
            else: # didn't break; success !
                i += 1
                idx[i:i], cuts[i:i] = index, knife
                return True

            s -= 1
        return False

    @classmethod
    def __relax(cls, cuts, idx, full, base):
        """Adjusts cuts to terse values.

        Rounds each cuts[i] to a multiple of 1./base**n (for the smallest n,
        in each case, that fullows this) that lies between the same
        full[idx[i]-1] <= cuts[i] < full[idx[i]].\n"""
        i = len(cuts)
        while i > 0:
            i -= 1
            j = idx[i]
            if j < 1: # no lo
                lo, hi = cuts[i], full[j]
                lo -= hi - lo
            elif j < len(full):
                lo, hi = full[j-1], full[j]
            else: # no hi
                lo, hi = full[j-1], cuts[i]
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
        full = self.sorted
        self.__relax(cuts, idx, full, base)
        # full[idx[i]-1] <= cuts[i] < full[idx[i]], strict when possible

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
                    if self.__split(i, idx, cuts, full, c, t, base): break
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
        full, cuts, idx = iter(self.sorted), [], []
        # full[idx[i]-1] < cuts[i] < full[idx[i]]
        was, j = full.next(), 0
        for it in full: # self.sorted[j] == was
            j += 1
            if it > was: # self.sorted[j] == it
                cuts.append(self.__mid(was, it, base))
                idx.append(j)
                was = it

        while len(cuts) > 2:
            gaps = self.__diff(cuts)
            dense = [x / y for x, y in zip(self.__diff(idx), gaps)]
            # dense[i] = (idx[i+1] - idx[i]) / (cuts[i+1] - cuts[i])
            ratio = [(x * 1. / y if x > y else y * 1. / x)
                     for x, y in zip(dense[1:], dense[:-1])]
            worst = max(ratio)
            if worst < r: break
            j = ratio.index(worst)
            # dense[j] and dense[j+1] are wildly different
            if gaps[j] > gaps[j+1]: j += 1
            # gaps[j] is the narrower of the two
            if j == 0 or (j+1 < len(gaps) and gaps[j-1] > gaps[j+1]): j += 1
            #print "Culling", j, 'from', ['%.2f' % x for x in dense],
            #print "to kill", worst, 'in', ['%.2f' % x for x in ratio]
            del idx[j], cuts[j] # the boundary between these two intervals

        full = self.sorted
        # That will never ditch the outermost cuts:
        assert full[0] == full[idx[0] - 1]
        assert full[-1] == full[idx[-1]]

        # Add outliers for start and end:
        cuts.insert(0, self.__outlier(full[0], cuts[0], idx[0],
                                      r, dense[0], dense[1], -1))
        cuts.append(self.__outlier(full[-1], cuts[-1], len(self) - idx[-1],
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

del lazyprop, Tuple
