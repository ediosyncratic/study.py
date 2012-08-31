"""The gaussian distribution.

The primitive distribution is given by function gauss().  For (one-dimensional)
gaussian distributions, use the class Normal(mean, stddev), derived from
variate.Variate, q.v., so as to provide probabilities as integrals of gauss(),
with suitable scalings.

Some day I may add support for multi-dimensional gaussian distributions.  I'll
use Gaussian(mean, variance) as its interface; mean will be a vector quantity,
variance will be a tensor quantity of the same rank as mean*mean.  For the
present, a function Gaussian(mean, variance) is defined (actually an alias for
static method Normal.fromMeanVary), working only for one-dimensional data,
implemented using Normal.

See also gamma.py for Gamma distributions.

Note that a Gaussian distribution always has non-zero probability of delivering
negative values.  Many variates are incapable of delivering negative values (for
example, the heights of human beings) so clearly cannot be normally distributed
(though they may be Gamma distributed).  However, provided the variate is also
incapable of being zero, we can take the logarithm of the variate (divided, if
necessary, by some suitable unit) to obtain a variate which may be negative or
positive, so may be well modelled by a normal distribution.  From this we can
then infer a distribution for the original variate, the `lognormal'
distribution, which does guarantee positive values, yet has roughly the form of
the normal distribution (i.e. thin at one end, thick in the middle and then thin
again at the other end).

From HAKMEM, http://www.inwap.com/pdp10/hbaker/hakmem/random.html:

  ITEM 26 (? via Salamin):

  A mathematically exact method of generating a Gaussian distribution
  from a uniform distribution: let x be uniform on [0,1] and y uniform
  on [0, 2 pi], x and y independent. Calculate r = sqrt(-log x). Then
  r cos y and r sin y are two independent Gaussian distributed random
  numbers.

  ITEM 27 (Salamin):

  PROBLEM: Generate random unit vectors in N-space uniform on the unit
  sphere. SOLUTION: Generate N Gaussian random numbers and normalize
  to unit length.

See study.LICENSE for copyright and license information.
"""

import math
def gauss(x, e=math.exp, n=(2*math.pi)**.5): return e(-x*x/2)/n
del math

from variate import Variate
class Normal (Variate):
    __upinit = Variate.__init__
    def __init__(self, mean, stddev):
	"""Initialises a (one-dimensional) Normal distribution.

	Takes two arguments; mean and stddev, the mean and standard deviation
	of the normal distribution. """

	scalar = True
	try:
	    mean.width, mean.best
	    if callable(mean.evaluate): scalar = False
	except AttributeError: pass
	try:
	    stddev.width, stddev.best
	    if callable(stddev.evaluate): scalar = False
	except AttributeError: pass

	if scalar:
	    # ordinary python numeric types, or functional equivalents
	    def func(val, m=mean, s=stddev, g=gauss):
		return g((val-m)/s)/s
	else:
	    # Quantity(), supporting sophistication :^)
	    def func(val, m=mean, s=stddev, g=gauss):
		return ((val-m)/s).evaluate(g)/s

	self.__upinit(func, width = 5 * stddev)
        self.mean, self.sigma = mean, stddev

    import random
    def sample(self, g=random.gauss):
        return g(self.mean, self.sigma)
    del random

    @staticmethod
    def fromMeanVary(mean, variance):
        ans = Normal(mean, variance**.5)
        ans.variance = variance
        return ans

Gaussian = Normal.fromMeanVary

class logNormal (Variate):
    """Distribution of a variate whose logarithm is normally distributed.

    If the variate is X, then there is some constant m for which log(X/m) is
    normally distributed with expected value 0.  [To find such an m; chose any
    value X could take, say x, and find the mean, say q, of log(X/x); then
    log(X/x) -q is normally distributed with mean 0; and log(X/x) -q is just
    log(X/x.exp(q)) so use m = x.exp(q).]

    """

    __upinit = Variate.__init__
    def __init__(self, mean, vary):
        raise NotImplementedError # TODO

del Variate
