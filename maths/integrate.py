"""Integration made easy.

Potentially useful formula (from the method of exhaustion):

integral(: f(x).dx &larr;x; a &le;x&le;b :)
= sum(: 2**(1+n) * sum(: (-1)**m f(a +(b-a)*(m+1)/2**(1+n)) &larr;m :2**(1+n) -2)
        &larr;n :{naturals})*(b-a)

see: http://en.wikipedia.org/wiki/Method_of_exhaustion
"""
class Integrator:
    """Base class for integrators.

    Provides crude trapezium rule integrator, which derived classes might wish
    to over-ride.  More importantly, defines an API for integrators:
        measure(func) -- integrator scaling self.integrand by func
        between(start, stop) -- integrates over an interval
        before(stop) -- as between, but with minus infinity as start
        beyond(start) -- as between, but with plus infinity as stop

    Both before() and beyond() presume that they're integrating a `tail' of a
    distribution: to compute an integral between one side of a distribution's
    mode and infinity on the other side, it is probably wisest to call between()
    on the given bound and a more-or-less arbitrary position on the same side of
    the mode as the infinity in question, then to use before() or beyond() to
    compute the remainder, using between()'s output as offset (see below).

    All three integrator methods take an optional argument, test, following the
    given required arguments; and an optional keyword argument, offset, after
    that.  These are used in deciding when the integral has been determined
    accurately enough: after each iteration, the integrator calls test with the
    change in its estimate of the integral as first argument and, as second
    argument, the new estimate, with offset (if supplied) added to it.  If the
    change is small enough that further refinement is a waste of time, test
    should return true.  The integrator will then return the given estimate
    added to an error bar whose width is the last change.

    By default, the code is geared up to deal with values of class Quantity (see
    study.value.quantity) and, in that case, the integrators return Quantity()s.  If
    the first estimate at the integral (plus optional offset) has a positive
    .width attribute, it is presumed to be a Quantity() and the default test
    simply compares the change in estimate with the .width of the new estimate;
    if the change is within this existing error bar, it is taken to be small
    enough. In the absence of .width, or if the .width is not positive, the
    default test simply looks to see whether the change in estimate is smaller
    than 1e-6 times the estimate (plus optional offset); in the absence of an
    offset, this last test will work poorly if the integral should yield
    zero.\n"""

    def __init__(self, func, lower=None, upper=None, width=None):
        """Initialises an integrator.

        Required first argument is the function to be integrated.  This will be
        exposed as self.integrand(), subject to any clipping (see optional
        arguments).

        Optional arguments:
           lower -- a strict lower bound on func's domain
           upper -- a strict upper bound on func's domain
           width -- indicates scale of func's domain

        Each defaults to None, in which case it is ignored.  If both lower and
        upper are supplied (and not None), width should normally be None; if it
        is, in this case, it shall be inferred from lower and upper.  It is only
        needed if .beyond() or .before() is liable to be called with bound
        zero. It should ideally be approximately the difference between highest
        and lowest inputs for which the integrand differs significantly from 0;
        e.g., if func is the density of a random variate, 5 standard deviations
        would be prudent.\n"""

        self.__integrand = func
        if lower is not None:
            if upper is not None: assert upper > lower
            self.__lo = lower
        if upper is not None: self.__hi = upper
        if width is not None: self.__unit = abs(width) * 1.

    def integrand(self, val):
        lo, hi = self.__span(val, val)
        if lo is not val or hi is not val: scale = 0 # clipped
        else: scale = 1
        return self.__integrand(val) * scale

    @staticmethod
    def _integrator_(func, lower=None, upper=None, width=None):
        """Indirection for instantiating derived integrators.

        Takes the same parameters as Integrator.  Derived classes should
        over-ride this method if they want measure() to return something other
        than a plain Integrator.\n"""
        return Integrator(func, lower, upper, width)

    def measure(self, func, lower=None, upper=None, width=None):
        """New integrator scaling self.integrand pointwise by a given function.

        Required argument, func, is a function accepting the same inputs as
        self.integrand().  Optional arguments - lower, upper and width - are as
        for the Integrator constructor; if omitted, the values used when self
        was constructed (if any) are used.  For lower and upper, if supplied,
        the intersection of the implied range with that of self is used to infer
        bounds.

        Returns an Integrator whose integrand is (: func(x) * self.integrand(x)
        &larr;x :) which can be construed as integrating func using self as
        measure; or as integrating self using func as measure.  If self is a
        probability distribution, its moments can be computed using func()s of
        form (: x**i &larr;x :) for i = 1, 2, 3 ...

        Derived classes should over-ride this if their constructors don't have
        the same signature as Integrator.\n"""

        try: lower, upper, wide = self.__clip(lower, upper)
        except AttributeError: pass # no helpful hints on anything !
        else:
            if width is None: width = wide
        return self._integrator_(lambda x, f=func, i=self.integrand: f(x) * i(x),
                                 lower, upper, width)

    def total(self, cut=None, test=None, offset=None):
        """Total integral, from minus infinity to plus infinity.

        Semi-optional argument, cut, is a valid input to self.integrand at which
        to split the integral; if the constructor was given any of lower, upper
        and width, this is not needed (but supplying it is
        permitted).  Otherwise, if it is not supplied, 1 is used (which may be
        wildly inappropriate for some integrands).  Also accepts the usual
        optional tolerance specifiers, test and offset: see class doc for
        details.\n"""
        lo, hi = self.__span(cut, cut)
        if lo is None or hi is None:
            try: wide = self.__unit
            except AttributeError:
                if lo is None:
                    if hi is None: hi = 1 # ouch
                    lo = hi
                elif hi is None:
                    hi = lo
            else:
                if lo is None:
                    if hi is None: hi = wide
                    lo = hi - wide
                elif hi is None:
                    hi = lo + wide

        if lo == hi: lo, hi = .5 * hi, 1.5 * hi
        if lo > hi: hi, lo = lo, hi
        mid = self.between(lo, hi, test, offset)
        if offset is None: offset = mid
        return self.before(lo, test, offset) + mid + self.beyond(hi, test, offset)

    def between(self, start, stop, test=None, offset=None):
        """Integral over a range.

        Two required arguments, start and stop, give the bounds of the
        range. Also accepts the usual optional tolerance specifiers, test and
        offset: see class doc for details.  If start > stop, they are reversed
        and the resulting integral is negated.\n"""

        if start > stop: sign, start, stop = -1, stop, start
        else: sign = 1
        lo, hi, wide = self.__clip(start, stop)
        return self.__interval(lo, hi, wide, test, offset) * sign

    def before(self, stop, test=None, offset=None):
        """Integration from minus infinity.

        Equivalent to between() with start set to a value less than any at which
        self's distribution is distinguishable from zero.  Required argument is
        the upper bound of the integral; also accepts the usual optional
        tolerance specifiers, test and offset: see class doc for details.\n"""

        lo, hi, wide = self.__clip(None, stop)
        assert hi is not None
        if lo is None: return -self.__outwards(lo, -wide, test, offset)
        return self.__interval(lo, hi, wide, test, offset)

    def beyond(self, start, test=None, offset=None):
        """Integration to plus infinity.

        As before() but required argument is the lower bound of integration, the
        upper bound being greater than any value at which self's distribution is
        distinguishable from zero.\n"""

        lo, hi, wide = self.__clip(start, None)
        assert lo is not None
        if hi is None: return self.__outwards(start, wide, test, offset)
        return self.__interval(lo, hi, wide, test, offset)

    # hairy implementation follows: no further exports.

    from study.value.quantity import Quantity
    def __blur(mid, spread, H=Quantity.within): return H(mid, spread * .5)
    del Quantity
    # __blur and __gettest will be del'd shortly ... they're *not* methods

    def bywidth(d, n):
        try: d = d.best
        except AttributeError: pass
        return abs(d) <= n.width
    def __gettest(eg,
                  microclose=lambda d, n: abs(d) <= 1e-6 * abs(n),
                  bywidth=bywidth):
        """Returns a sensible tolerance test.

        Where between, below and beyond are not given a tolerance test function,
        this determines one given an example value kindred to the final output
        (plus optional offset) relative to which the tolerance is computed.\n"""

        try:
            w = eg.width
            try:
                count = 10
                while count > 0: eg, count = eg.best, count-1
                while eg.width > 0 * eg.best: eg = eg.best
            except AttributeError: pass

            if w > 0 * eg: return bywidth

        except AttributeError: pass

        return microclose
    del bywidth

    def __interval(self, lo, hi, wide, test, offset):
        return self.__between(lo, wide,
                              (self.integrand(lo) + self.integrand(hi)) * .5,
                              test, offset)

    def __between(self, start, gap, edge, test, offset,
                  blur=__blur, gettest=__gettest):

        now = edge * gap # first crude estimate

        # get advertised default for offset:
        if offset is None:
            try: offset = now - now.best
            except AttributeError: offset = now * 0

        # get advertised default test:
        if test is None: test = gettest(offset + now)

        n = 1
        while True:
            n, was = 3 * n, now
            h = gap / n
            now = (sum(map(lambda i, b=start, s=h, f=self.__integrand: f(b+i*s),
                           range(1, n))) + edge) * h

            dif = now - was
            if test(dif, now + offset): return blur(now, dif)

    def __outwards(self, bound, step, test, offset,
                   blur=__blur, gettest=__gettest):
        small = abs(self.__integrand(bound)) / 1e3
        while self.__probe(bound, step) < small: step = step / 7.
        while self.__probe(bound, step) > small: step = step * 7
        if test is None: test = gettest(small * step)

        next = bound + step
        total, bound = self.__interval(bound, next, step, test, offset), next
        if offset is None:
            try: offset = total - total.best
            except AttributeError: offset = 0 * total

        while True:
            step = step * 3
            next = bound + step
            more = self.__interval(bound, next, step, test, total + offset)
            total, bound = total + more, next
            if test(more, total + offset): return blur(total, more)

    del __blur, __gettest

    def __span(self, start, stop):
        try: lo = self.__lo
        except AttributeError: pass
        else:
            if start is None or start < lo: start = lo

        try: hi = self.__hi
        except AttributeError: pass
        else:
            if stop is None or hi < stop: stop = hi

        return start, stop

    def __clip(self, start, stop):
        start, stop = self.__span(start, stop)

        if start is None:
            if stop is None: wide = self.__unit # may AttributeError
            else: wide = self.__step(stop)
        elif stop is None: wide = self.__step(start)
        else: wide = stop - start
        return start, stop, wide * 1. # avoid whole-number gotchas ...

    # need better initial step than abs(bound) ... bound could be zero.
    def __step(self, bound):
        try: return self.__unit
        except AttributeError: pass
        try: ans = abs(bound.copy(lambda x: 1.)) # for Quantity()s
        except (AttributeError, TypeError): ans = abs(bound) * 1.
        if ans: self.__unit = ans
        else: raise ValueError, \
              'Integrator needs a width parameter for .before(0) or .beyond(0)'
        return ans

    import math
    def __probe(self, base, scale,
                samples=[1, math.exp(1/math.pi), math.sqrt(2.0), 2, math.e, 3, math.pi]):
        """Scale of integrand's values for inputs base + of order scale."""
        return max(map(lambda x, f=self.integrand, s=scale, b=base: abs(f(b + x*s)), samples))
    del math
