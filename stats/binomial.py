"""The binomial distribution.

See study.LICENSE for copyright and license information.
"""

class Binomial (object):
    def __init__(self, bias = 0):
        """Describe a two-outcome random process.

        Single parameter, bias, defaults to 0 and must not be bigger
        than 1/2; it is how far the two outcomes probabilities are
        from 1/2.  Docs here reter to the outcome with probability 0.5
        +bias as U and that with probability 0.5 -bias as L (for
        nominally upper and lower probabilities; but bias may of
        course be negative).
        """
        if bias >= .5 or bias <= -.5:
            raise ValueError("Bias makes a probability zero or negative", bias)
        self.__ps = 0.5 +bias, 0.5 -bias

    def p(self, n, m):
        """Probability of n U outcomes and m L outcomes.

        Tacitly, in a series of n+m trials.  Both n and m must be
        whole numbers and neither should be negative.
        """
        if n < 0 or m < 0:
            raise ValueError("Counts should not be negative", n, m)
        if n != int(n) or m != int(m):
            raise ValueError("Counts should be whole numbers", n, m)

        u, v = self.__ps
        # return chose(n +m, n) * u**n * v**m
        # But that may involve multiplying an astronomically big
        # integer by a denormal-or-underflowed float.
        if n > m:
            n, m, u, v = m, n, v, u
        if not n:
            return v**m

        result, i = 1., n
        # Aim to apply m / n of powers of v per iteration.
        q, r = divmod(m, n)
        assert q > 0 # because m >= n
        # Start with q factors of v per iteration.
        # Boost that to q +1 factors of v for the last r iterations.
        u *= v**q
        while i > 0:
            result *= (m +i) * u / i
            i -= 1
            if r == i > 0:
                u *= v

        return result

    def between(self, count, lo=0, hi=None):
        """sum(self.p(i, count -i) for i in range(lo, hi+1))

        This is the probability that the number of U outcomes in count
        trials will be in the range lo, ..., hi.  If hi is omitted or
        None, count is used.  Default for lo is, naturally, 0.

        The summation is done 'towards the middle' so that, at least
        for small bias, smaller terms are included in the sum earlier,
        to avoid losing precision."""
        if count < 0 or not count == int(count):
            raise ValueError("Number of trials must be natural", count)
        if lo < 0:
            lo = 0
        if hi is None or hi > count:
            hi = count
        if hi < lo:
            return 0
        bot = self.p(lo, count -lo)
        if lo == hi:
            return bot
        top = self.p(hi, count -hi)
        tot = bot + top
        u, v = self.__ps
        # bot == count *... (count +1 -lo) * u**lo * v**(count -lo) / lo /... / 2 / 1
        # top == count *... (hi +1) * u**hi * v**(count -hi) / (count -hi) /... / 2 / 1
        while lo +1 < hi:
            if count < lo +hi:
                top *= hi * v / u
                hi -= 1
                top /= count -hi
                tot += top
            else:
                bot *= count -lo
                lo += 1
                bot *= u / v / lo
                tot += bot
        return tot

    @classmethod
    def _test_between(cls):
        fair = cls()
        assert fair.between(0, 1) == 0
        assert fair.between(4) == 1
        assert fair.between(2, 1) * 4 == 3
        assert fair.between(4, 2) * 16 == 11
        assert fair.between(17, 0, 8) * 2 == 1
        assert fair.between(17, 9) * 2 == 1
        third = cls(1./6)
        assert third.between(0, 1) == 0
        assert -1e-6 < third.between(4) -1 < 1e-6
        assert -1e-6 < third.between(2, 0, 1) * 9 - 5 < 1e-6
        assert -1e-6 < third.between(2, 1) * 9 - 8 < 1e-6
        # 1, 14, 84, 280, 560, 672, 448, 128; /2187
        assert -1e-6 < third.between(7, 4) * 2187 - 1808 < 1e-6
        assert -1e-6 < third.between(7, 0, 3) * 2187 - 379 < 1e-6
        assert -1e-6 < third.between(7, 2, 5) * 729 - 532 < 1e-6

    def median(self, count):
        """The median number of U outcomes among count trials.

        If this is n, then the probabilitis of less than n U outcomes
        and more than n U outcomes are both at most 1/2; and the
        probability of exactly n outcomes is at least the difference
        between them.  The median number of L outcomes among count
        trials is count -median(count).

        When there is an n for which the probability of up to n U
        outcomes is equal to the probability of more than n U
        outcomes, n +0.5 is returned; the probability of this is zero,
        but that's still at least the difference in probability
        between the two sides of it.  It indicates that n and n+1 have
        equally good claims to be considered the median."""
        if count < 0 or not count == int(count):
            raise ValueError("Number of trials must be natural", count)

        u, v = self.__ps
        n = m = k = h = 0
        # Terms:
        up = dn = 1.
        # up = chose(count, k) * u**h * v**k = p(count -k, k) / u**(count -k -h)
        # dn = chose(count, n) * u**n * v**m = p(n, count -n) / v**(count -n -m)

        # Sums:
        hi = lo = 0.
        # hi = between(count, count -k +1, count) * u**(h +k -count)
        # lo = between(count, 0, n-1) * v**(m +n -count)

        while min(n +m, h +k) < count:
            if n +m < count:
                while lo +dn >= 0.5:
                    lo *= v
                    dn *= v
                    m += 1
                    if n +m >= count:
                        break
                else: # Didn't break:
                    lo += dn
                    lo *= v
                    dn *= count -n
                    n += 1
                    dn *= u / n
                assert lo < .5

            if k +h < count:
                while hi +up >= 0.5:
                    hi *= u
                    up *= u
                    h += 1
                    if k +h >= count:
                        break
                else: # Didn't break
                    hi += up
                    hi *= u
                    up *= count -k
                    k += 1
                    up *= v / k
                assert hi < .5

            # Neither hi * u**(count -h -k) nor lo * v**(count -n -m)
            # is >= 0.5; so their sum is < 1, leaving some p(i, count
            # -i) unaccounted for, so some i between n and count -k,
            # inclusive, i.e. n <= count -k:
            assert n +k <= count, (h, k, up, hi, m, n, dn, lo, count)
        assert n +m == count == h +k, (h, k, up, hi, m, n, dn, lo, count)
        # All missing factors accounted for.

        u, v = u / v, v / u
        while n +k <= count:
            if lo +dn <= (hi +up if n +k < count else hi):
                lo += dn
                dn *= count -n
                n += 1
                dn *= u / n
            elif hi +up <= (lo +dn if n +k < count else lo):
                hi += up
                up *= count -k
                k += 1
                up *= v / k
            else:
                assert n +k == count
                # up, dn are both the "middle" element,
                # and either side claiming it would exceed the other
                return n
        assert n +k == count +1
        # Totals up to n -1 = count -k and from n onwards are equal.
        return n -.5

    @classmethod
    def _test_median(cls):
        fair = cls()
        assert fair.median(20) == 10
        assert fair.median(21) == 10.5
        third = cls(1./6)
        assert third.median(3) == 2
        assert third.median(6) == 4
        third = cls(-1./6)
        assert third.median(3) == 1
        assert third.median(6) == 2

    @classmethod
    def __tail(cls, count, frac, u, v):
        """Least n s.t. sum(chose(count, i) * u**i * v**(count -i)
        for i in range(1+n)) > frac.
        """
        if count < 0 or not count == int(count):
            raise ValueError("Number of trials must be natural", count)
        if frac >= 1:
            return count +1
        if frac <= 0:
            return 0

        n = m = 0
        # term = product(u * (count +i) / i for i in range(n, 0, -1)) * v**m
        # tot = sum(u**i * chose(count, i) * v**(m +n -i) for i in range(1+n))
        tot = term = 1.
        # We defer the other count -m -n factors of v, to each, so as
        # to avoid loss of precision.
        while n +m < count:
            while tot > frac:
                tot *= v
                term *= v
                m += 1
                if m +n >= count:
                    break
            else: # We didn't break:
                term *= count -n
                n += 1
                term *= u / n
                tot = tot * v +term
        assert n +m == count, (n, m, term, tot, frac)

        # We've now applied count -n factors of v to term (and count
        # to tot); later terms want fewer.
        u /= v

        # term = product(u * (count +i) / v / i for i in range(n, 0, -1)) * v**count
        while tot <= frac and n < count:
            term *= count -n
            n += 1
            term *= u / n
            tot += term
        # Technically the n == count case may have tot still <= frac
        # but frac < 1 (we handled frac >= 1 above), and tot *should*
        # be 1 when n == count, so n is still the right answer.
        return n

    def upper(self, count, frac):
        """Greatest n s.t. P(under n U outcomes in count trials) <= frac.

        The probability here is between(count, 0, n-1).  Return is 0 if frac
        < p(0, count).  For frac >= 1, it is count +1.  Care is taken
        to preserve precision from smaller terms early in the sum.
        Note that the last term in the sum is p(n-1, count+1-n);
        adding p(n, count-n) takes the total over frac."""
        u, v = self.__ps
        return self.__tail(count, frac, u, v)

    @classmethod
    def _test_upper(cls):
        fair = cls()
        assert fair.upper(6, 7./64) == 2
        assert fair.upper(6, 11./32) == 3
        assert fair.upper(6, 21./32) == 4
        third = cls(1./6)
        assert third.upper(6, 13.1/729) == 2
        assert third.upper(6, 73.1/729) == 3
        assert third.upper(6, 233.1/729) == 4
        assert third.upper(6, 473.1/729) == 5
        third = cls(-1./6)
        assert third.upper(6, 64.1/729) == 1
        assert third.upper(6, 256.1/729) == 2
        assert third.upper(6, 496.1/729) == 3
        assert third.upper(6, 656.1/729) == 4
        assert third.upper(6, 716.1/729) == 5
        assert third.upper(6, 1 -0.0000000000000001) == 6

    @classmethod
    def _test_lower(cls):
        fair = cls()
        assert fair.lower(6, 7./64) == 2
        assert fair.lower(6, 11./32) == 3
        assert fair.lower(6, 21./32) == 4
        third = cls(-1./6)
        assert third.lower(6, 13.1/729) == 2
        assert third.lower(6, 73.1/729) == 3
        assert third.lower(6, 233.1/729) == 4
        assert third.lower(6, 473.1/729) == 5
        third = cls(1./6)
        assert third.lower(6, 64.1/729) == 1
        assert third.lower(6, 256.1/729) == 2
        assert third.lower(6, 496.1/729) == 3
        assert third.lower(6, 656.1/729) == 4
        assert third.lower(6, 716.1/729) == 5
        assert third.lower(6, 1 -0.0000000000000001) == 6

    def lower(self, count, frac):
        """Greatest n s.t. P(under n L outcomes in count trials) <= frac.

        The probability here is just between(count, count -n +1, count).
        Return is count +1 if frac < p(count, 0).  For frac >= 1, it
        is count +1.  Care is taken to preserve precision from smaller
        terms early in the sum.  Note that the last term in the sum is
        p(count-1-n, 1+n); adding p(count-n, n) takes the total over
        frac."""
        u, v = self.__ps
        return self.__tail(count, frac, v, u)

    @classmethod
    def _test(cls):
        cls._test_median()
        cls._test_lower()
        cls._test_upper()
        cls._test_between()
