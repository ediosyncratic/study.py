"""Modelling random variates.

A random variate's model needs to be able to generate `unpredictably'
(chaotically or pseudo-randomly) from the vairate's distribution; it also needs
to be able to perform the integration needed to determine moments (e.g. the
mean) and the probability of the variate falling in any given range of its
permitted values.

$Id: variate.py,v 1.3 2007-03-24 22:42:21 eddy Exp $
"""

from integrate import Integrator
from study.value.lazy import Lazy

class Variate (Integrator, Lazy):
    __upinit = Integrator.__init__
    # Integrator provides between(), beyond() and before().
    def __init__(self, func, width=None):
        self.__upinit(func, width)
        self.__moments = []

    def __moment(self, i, s):
        g = self.measure(lambda x, j=i: x**j)
        return g.before(s) + g.between(s,s) + g.beyond(s)

    def moments(self, n):
        """Returns expected values of various powers of the variate.

        Single argument, n, will be the length of the tuple returned: its
        entry[i] is the expected value of self.sample()**(1+i).  Thus the first
        entry is the mean, the second is the `mean square' and so on. """

        seed = self.sample()
        total = self.before(seed) + self.beyond(seed) # self.__moment(0, s)
        return tuple(map(lambda i, s=seed, t=total, m=self.__moment: m(i,s)/t,
                         range(1, 1+n)))

    def _lazy_get_mean_(self, ignored):
        return self.moments(1)[0]

    def _lazy_get_variance_(self, ignored):
        self.mean, two = self.moments(2)
        return two - self.mean**2

    def sample(self):
        """Returns a sample value from this distribution.

        Each call returns a fresh value independently of previous calls.
        This method should be over-ridden in derived classes.
        See RatioGenerator for a generic algorithm for implementing samples.
        """
        raise NotImplementedError

from random import random # 0 <= uniform < 1
class Uniform (Variate):
    __upinit = Variate.__init__
    def __init__(self, lo=1, hi=None):
        if hi is None: lo, hi = 0, lo # i.e. lo has default 0, really hi was supplied ...
        self.min, self.max, self.width = lo, hi, hi - lo
        self.__height = 1./self.width
        self.__upinit(self.__p, self.width)

    def __p(self, x):
        if self.min < x < self.max: return self.__height
        return 0 * self.__height # may have units of measurement ...

    def sample(self):
        return random() * self.width + self.min

class RatioGenerator:
    """Illustration of a technique.

    If source() generates samples from a distribution g, ratio = (: f(y)/g(y)
    &larr;y :), and (|ratio|source) is bounded below by 0 and above by bound,
    calls to the object returned by RatioGenerator(source, ratio, bound) will be
    samples from the distribution f. """

    def __init__(self, source, ratio, bound):
        self.__source, self.__ratio, self.__bound = source, ratio, bound

    def __call__(self):
        """The ratio test algorithm."""
        while 1:
            ans = self.__source()
            if self.__ratio(ans) > random() * bound:
                return ans
