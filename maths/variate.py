"""Modelling random variates.

A random variate's model needs to be able to generate `unpredictably'
(chaotically or pseudo-randomly) from the vairate's distribution; it also needs
to be able to perform the integration needed to determine moments (e.g. the
mean) and the probability of the variate falling in any given range of its
permitted values.

See study.LICENSE for copyright and license information.
"""

from study.maths.integrate import Integrator
from study.cache.property import lazyprop, lazyattr, Cached

class Variate (Integrator, Cached):
    __upinit = Integrator.__init__
    # Integrator provides total(), between(), beyond() and before().
    def __init__(self, func, lower=None, upper=None, width=None):
        self.__upinit(func, lower, upper, width)
        self.__moments = ()
        if not (lower is None and upper is None and width is None):
            self.__cut = None # else unset; access to error on invalid use of self

    def moments(self, n):
        """Returns expected values of various powers of the variate.

        Single argument, n, will be the length of the tuple returned: its
        entry[i] is the expected value of self.sample()**(1+i).  Thus the first
        entry is the mean, the second is the `mean square' and so on; the last
        entry is the expected value of the n-th power of the variate.\n"""

        if n > len(self.__moments):
            self.__moments += tuple(map(lambda i, t=self.__total, m=self.__moment: m(i)/t,
                                        range(1+len(self.__moments), 1+n)))
        return self.__moments[:n]

    def __moment(self, i):
        return self.measure(lambda x, j=i: x**j).total(self.__cut)
    @lazyprop
    def __total(self): return self.total(self.__cut)
    @lazyattr
    def __cut(self): return self.sample()
    @lazyprop
    def mean(self): return self.moments(1)[0]

    @lazyprop
    def variance(self):
        self.mean, two = self.moments(2)
        return two - self.mean**2

    @lazyprop
    def median(self):
        lo = hi = self.sample()
        while lo == hi: lo = self.sample()
        if lo > hi: lo, hi = hi, lo
        wlo, whi, tgt = self.before(lo), self.before(hi), self.__total / 2
        while lo < hi:
            mid = ((tgt - wlo) * hi + (whi - tgt) * lo) / (whi - wlo)
            wmid = self.before(mid)
            if mid > hi: lo, hi, wlo, whi = hi, mid, whi, wmid
            elif mid < lo: lo, hi, wlo, whi = mid, lo, wmid, wlo
            elif wmid < tgt: lo, wlo = mid, wmid
            elif wmid > tgt: hi, whi = mid, wmid
            else: return mid
        return lo

    def sample(self):
        """Returns a sample value from this distribution.

        Each call returns a fresh value independently of previous calls.
        This method should be over-ridden in derived classes.
        See RatioGenerator for a generic algorithm for implementing samples.
        """
        raise NotImplementedError

    @property
    def samples(self):
        while True:
            yield self.sample()

del lazyprop, lazyattr, Integrator, Cached

from random import random # 0 <= uniform < 1
class Uniform (Variate):
    __upinit = Variate.__init__
    def __init__(self, lo=1, hi=None):
        if hi is None: lo, hi = 0 * lo, lo # i.e. lo has default 0, really hi was supplied ...
        self.min, self.max = lo, hi
        self.__height = 1. / (hi - lo)
        self.__upinit(lambda x, h=self.__height: h, lo, hi)

    def __w(self, r=random): r() / self.__height

    def sample(self):
        return self.__w() + self.min

# TODO: turn into a class method (generator) of Variate
class RatioGenerator:
    """Illustration of a technique.

    If source() generates samples from a distribution g, ratio = (: f(y)/g(y)
    &larr;y :), and (|ratio|source) is bounded below by 0 and above by bound,
    calls to the object returned by RatioGenerator(source, ratio, bound) will be
    samples from the distribution f. """

    def __init__(self, source, ratio, bound):
        self.__source, self.__ratio, self.__bound = source, ratio, bound

    def __b(self, r=random): return r() * self.__bound
    def sample(self):
        """The ratio test algorithm."""
        while True:
            ans = self.__source()
            if self.__ratio(ans) > self.__b():
                return ans
del random

