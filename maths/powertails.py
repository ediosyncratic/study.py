"""One-sided power distributions.

The integral of power(q) over an interval is the change of power(1+q)/(1+q)
over that same interval, provided q is not -1 (where we get log, instead; and
this goes to infinities at zero and infinity).  For this to be finite with one
end of the interval zero, we need 1+q > 0; for it to be finite when one end of
the interval is infinite, we need q < -1.  So a density power(-1)/(power(-z)
+power(t)) is normalisable for z and t positive.  This can alternatively be
written:

* power(z -1)/(1 +power(z +t))
* power(-1 -t)/(1 +power(-(z +t))

The former shows that our distribution is well-approximated by power(z -1) for
inputs small enough to make power(z +t) ignorable compared to 1, and the latter
that it is well-approximated by power(-1 -t) when the input is large enough to
make power(-(z +t)) negligible compared to 1.  These approximations enable us
to get good estimates of the tails of the distributions.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop
from study.snake.infinite import Infinite

class PowerTails (object):
    __cache = {}
    @classmethod
    def instance(cls, z, t, tiny = .01):
        key = z, t, tiny
        try:
            return cls.__cache[key]
        except KeyError:
            return cls(z, t, tiny)

    def moment(self, n):
        return self.__moment(n) / self.total

    @lazyprop
    def variance(self):
        mean = self.moment(1)
        return self.moment(2) -mean**2

    @lazyprop
    def total(self):
        """Normalization of a one-sided power distribution.

        Density functions of form R(z, t) = power(-1)/(power(-z) + power(t)) on
        {positives}, with z and t also both positive, should be normalizable.
        This computes their normalisation factors.  The n-th moments of R(z, t)
        are just the ratios of the normalisation constants of R(n+z, t-n) and
        R(z, t).  Notably, for n >= t, they are infinite.

        To compute the normalisation we must, in general, do brute force
        arithmetic integration.  We can take a low-interval (from zero) well
        approximated by power(z -1), with integral power(z)/z evaluated at the
        high end of the interval and a high-interval (to infinity) well
        approximated by power(-(1 +t)), with integral power(-t)/t evaluated at
        the low end of the interval.  In between we must use brute force on
        intervals, but we can at least chose the widths of the intervals so as
        to use narrower intervals where the density is large enough to matter
        and varying fast, while using wider intervals where it is low or
        stable."""
        return sum(self.__bands())

    def __init__(self, z, t, tiny):
        assert z > 0
        assert t > 0
        if tiny > .1 or tiny < -.1 or not tiny:
            tiny = .1
        else:
            tiny = abs(tiny)

        self.__zt = z, t
        self.__tiny = tiny
        self.__cache[(z, t, tiny)] = self

    def __density(self, x):
        z, t = self.__zt
        return 1/(x**(1 -z) +x**(1 +t))

    def __deriv(self, x):
        z, t = self.__zt
        return ((z -1) / x**z -(1 +t) * x**t)/(x**(1 -z) +x**(1 +t))**2

    def __moment(self, n = 0, inf = Infinite(+1)):
        if n == 0:
            return self.total
        z, t = self.__zt
        z, t = z + n, t - n
        if t <= 0:
            return inf
        return self.instance(z, t).total

    def __inflect(self):
        """Where is the second derivative zero ?

        This delivers the z +t powers of the actual points where the
        derivative is zero; caller can take care of that (and the possibility
        that one of these may be negative).

        The derivative's numerator is (z -1)/power(z) -(t +1)*power(t); this is
        the negated derivative of the density's denominator, which is squared
        in the derivative; the second derivative's term from differentiating
        the derivative's denominator is thus twice the square of the
        derivative's numerator, divided by the cube of the density's
        denominator.  The other term in the second derivative is obtained by
        differentiating the derivative's numerator, leaving the density's
        denominator squared; to combine the two terms we use the cubed
        denominator so multiply the derivative's numerator's derivative by the
        density's denominator, making the second derivative's numerator,
        evaluated at some input x:

        2*((z -1) / x**z -(t +1) * x**t)**2 -((z -1) * z / x**(z +1)
        +(t +1) * t * x**(t -1)) * (x**(1 -z) +x**(t +1))

        Multiply this through by x**(2*z) to make all powers positive, write
        u = x**(z +t) and rearrange:

        2*(z -1 -(t +1) * u)**2 -((z -1) * z +(t +1) * t * u) * (1 +u)
        = (2 +t) * (t +1) * u*u
          -(t*t +4*t*z +z*z -3*t +3*z -4) * u
          +(z -2) * (z -1)

        a quadratic in u.  Because we scaled by x**(2*z), we will (for z in {1,
        2}) get a zero root of this quadratic in u that doesn't correspond to a
        root of the original equation in x.  This is harmless as the caller
        discards any u <= 0."""
        z, t = self.__zt
        a = (t +2) * (t +1) # > 2
        # (This is the negative of how b is usually defined.)
        b = t*t +4*t*z +z*z -3*t +3*z -4
        c = (z -2) * (z -1)
        u = b * .5 / a # (Hence this doesn't need negation.)
        d = u**2 - c / a
        if d < 0:
            return ()
        if d > 0:
            d = d**.5
            return u -d, u +d
        return (u,)

    def __cuts(self):
        """Initial subdivision of range of integration.

        For the lower interval, we'll approximate by power(z -1) in place of
        that divided by 1 +power(z +t), making the given power our
        fractional error, which we want less than tiny.  The fractional error
        in the integrand is at its greatest at the upper end of this tail, so
        we can make the tail integral's fractional error less than tiny by
        picking an upper bound whose power(z +t) is tiny.  Similarly, the
        upper tail uses power(-(t +1)) as approximation, with fractional error
        power(-(z +t)); and the fractional error of the integrand is worst
        at the lower bound, so picking a lower bound at which this power is
        tiny, i.e. power(z +t) is 1/tiny, ensures the integral's fractional
        error is tiny.

        The peak has derivative zero; the condition for this is that (1
        +t)*power(t) equals (z -1)/power(z), so (z -1)/(t +1) is power(z
        +t).  The same power likewise arises in the points of inflection.

        In the course of constructing a sequence of cuts from these values, and
        before yielding any of them, also compute an order-of-magnitude
        estimate of total and store it as self.__scale, for later use by
        __split()."""
        z, t = self.__zt
        e = 1. / (z +t)
        low = self.__tiny ** e
        top = 1. / low
        # Mode (only used if between low and top):
        peak = low if z < 1 else ((z -1.) / (t +1.)) ** e
        # Corners of a triangle to roughly estimate total.
        high = self.__density(peak)
        left, right = low, top # ends of base-line, at height 0
        # Points of inflection:
        inflect = tuple(x ** e for x in self.__inflect() if x > 0)
        # (Raising to power e could be a problem unless +ve; and we only care
        # about +ve anyway.  See also __inflect()'s note on false zero roots.)

        seq = [low, top]
        if low < peak < top:
            seq.insert(1, peak)
        elif peak < top:
            left = peak
        else:
            assert peak > low
            right = peak

        if inflect:
            if low < inflect[0] < seq[1]:
                seq.insert(1, inflect[0])

            if left < inflect[0] < peak:
                left = peak - (peak - inflect[0]) * high / (
                    high - self.__density(inflect[0]))

            if seq[-2] < inflect[-1] < top:
                seq.insert(-1, inflect[-1])

            if peak < inflect[-1] < right:
                right = peak + (inflect[-1] - peak) * high / (
                    high - self.__density(inflect[-1]))

        # Set __scale before first yield, so __split() can use it:
        self.__scale = (right - left) * .5 * high

        for cut in seq:
            yield cut

    def __finecuts(self):
        seq = self.__cuts()
        last = next(seq)
        yield last

        for cut in seq:
            for c in self.__split(last, cut):
                yield c
            last = cut

    def __split(self, low, top):
        # low has already been seen.
        ld, td = self.__deriv(low), self.__deriv(top)
        # What's the scale of this integral ?
        if ld > 0:
            if td >= 0:
                high = self.__density(top)
            else:
                # Evaluate at weighted average of top and low:
                high = self.__density((ld * top -td * low) / (ld -td))
        elif td < -ld:
            # If both decreasing, prefer low; near a minimum, pick the steeper end.
            high = self.__density(low)
        else:
            high = self.__density(top)
        # This strip as proportion of total:
        scale = high * (top - low) / self.__scale
        # Relative variation in slope, scaled by that:
        r = scale * abs(td - ld) * 2. / (abs(ld) + abs(td))

        if r > self.__tiny:
            n, last = min(16, int(r / self.__tiny)), low
            for i in range(n):
                x = (top * (1 +i) +(n -i) * low) * 1. / (1 +n)
                # Rounding might repeat a value or deliver top:
                if top > x > last:
                    for c in self.__split(last, x):
                        yield c
                    last = x
        yield top

    def __bands(self):
        z, t = self.__zt
        seq = self.__finecuts()
        last = next(seq)
        yield last ** z / z

        left = self.__density(last)
        for cut in seq:
            here = self.__density(cut)
            yield (cut - last) * .5 * (here + left)
            last, left = cut, here

        yield last ** -t / t
