"""Gamma distributions.

Any time your data are of some kind guaranteed positive (e.g. how tall human
beings are), a suitable gamma distribution is more apt than a gaussian.  This
module provides implementations of suitable interfaces.

Gamma distributions have the general form

 p = (: exp(-beta*x)*(beta*x)**(alpha-1) &larr;x :{positives})*beta/gamma(alpha)

for some positive beta and alpha.  The normalisation factor gamma(alpha)
generalises factorial; gamma(1+n) = n! for natural n.  It is defined by

  gamma(alpha) = integral(: exp(-t) * t**(alpha-1) &larr;t :{positives})

for which one can show that gamma(1+a) = a*gamma(a), corresponding to
   (1+n)! = gamma(2+n) = (1+n).gamma(1+n) = (1+n).n!

Various tweaks (see stirling.lngamma) are available to enable accurate
calculation of gamma().

For alpha < 1, the distribution has a pole at 0; for sufficiently small beta*X,

 integral(: p(x) &larr;x; 0<x<X :)

has exp(-beta*x) always close enough to 1 that the difference is ignorable;
then

 integral(: (beta*x)**(alpha-1) &larr;x; 0<x<X :)

is the increase in (beta*x)**alpha / (beta*alpha) between x=0 and x=X;
which, for alpha > 0, is just (beta*X)**alpha / (beta * alpha), making
our integral of p up to X pretty close to

 (beta*X)**alpha / alpha / gamma(alpha)

"""

from integrate import Integrator
from basEddy import Lazy
import math, stirling

class Gamma (Integrator, Lazy):
    __upinit = Integrator.__init__
    def __init__(self, alpha, beta):
        try: alpha + 1 # test we can do arithmetic between alpha and scalars
        except TypeError:
            raise TypeError(alpha,
                            "Gamma distribution's order parameter should be dimensionless")

	self.alpha, self.beta = alpha, beta
	if alpha > 1: self.__atzero = 0
	elif alpha == 1: self.__atzero = 1
	elif alpha <= 0:
	    raise ValueError(alpha, 'Gamma function cannot be normalised for this alpha')
	# else, 0 < alpha < 1; don't set __atzero, it's infinite.

	self.__upinit(self.__p, 1/beta)
	self.__setup(alpha, beta)

    def __setup(x, alpha, beta, log=math.log, exp=math.exp, lngam=stirling.lngamma):
	"""Fiddly bits of setup dealing with whether alpha, beta are Quantity()s.

        Overall goal is to set self.__ep to the function
          (: (beta * x)**(alpha-1) * exp(-beta * x) &larr; x :) * beta / gamma(alpha)
        i.e. the density function for our Gamma variate.  However, assorted
        complications raise their heads: we only have gamma as log&on;gamma,
        evaluation of which, along with that of log and gamma, should be done
        via .evaluate() if pertinent values are Quantity()s.

        Note that self.__ep() should not be used directly, only via self.__p(),
        which performs necessary checks against invalid input. """

	scalar = 1
	# alpha is dimensionless ...
	try: lognorm = alpha.evaluate(lngam)
	except AttributeError: lognorm = lngam(alpha)
	else: scalar = None

	# ... but beta might not be
	try:
	    beta.width, beta.best
	    if not callable(beta.evaluate): raise AttributeError
	except AttributeError:
	    def logp(x, b=beta, a=alpha-1, ln=log, n=lognorm):
		bx = b * x
		return a * ln(bx) -bx -n
	else:
	    scalar = None
	    def logp(x, b=beta, a=alpha-1, ln=log, n=lognorm):
		bx = b * x
		return a * bx.evaluate(ln) -bx -n

	if scalar: self.__ep = lambda x, e=exp, lp=logp, b=beta: e(lp(x)) * b
	else: self.__ep = lambda x, e=exp, lp=logp, b=beta: lp(x).evaluate(e) * b

    def __p(self, x, exp=math.exp):
	"""The probability distribution"""
	if x == 0:
	    try: return self.__atzero
	    except AttributeError:
		raise OverflowError, 'Gamma, with alpha < 1, is infinite at zero'
	if x < 0: raise ValueError(x, 'Gamma distribution only defined on positives.')

	return self.__ep(x)

    def __sliver(self, end, gam=stirling.gamma):
	"""Approximates integration from zero to some small value < end.

	This involves chosing an X < end for which exp(beta*X) differs
	negligibly from 1 = exp(0), then using 

	    eps = (beta*X)**alpha / alpha / gamma(alpha)

	as estimate of the integral.  Returns a twople, (eps, X). """

	if end * self.beta > 1: end = 1./self.beta
	end = end * 1e-6
	try: end = end.best # throw away any error bars; not needed.
	except AttributeError: pass
	return (self.beta * end)**self.alpha / self.alpha / gam(self.alpha), end
	# hmm - implies Quantity needs an __rpow__, for alpha.

    __between = Integrator.between
    def between(self, start, stop, *args, **what):
	# distribution is undefined before 0
	# but should be treated as zero on negatives, for purposes of integration.
	if start < -start: start = 0 * start
	if stop < -stop: stop = 0 * stop
	if start == stop: return 0 * self.beta # need correct dimensions
	if stop < start: return -apply(self.between, (stop, start) + args, what)

	if self.alpha < 1 and start == 0:
	    # bodge round initial pole
	    assert stop > 0
	    eps, cut = self.__sliver(stop)
	    try: off = what['offset']
	    except KeyError: what['offset'] = eps
	    else: what['offset'] = eps + off
	    return eps + apply(self.__between, (start + cut, stop) + args, what)

	return apply(self.__between, (start, stop) + args, what)

    def _lazy_get_mean_(self, ignored):
        """Mean of the gamma distribution.

        This is
            integrate(: (beta * x)**alpha * exp(-beta * x) &larr; x; x > 0 :) / gamma(alpha)
            = integrate(: u**alpha * exp(-u) &larr; u; u > 0 :) / beta / gamma(alpha)
        whose integrand is
            d(-exp(-u) * u**alpha) +alpha * u**(alpha-1) * exp(-u)
        the differential term in which is zero at both 0 and infinity, for alpha > 0,
        so we can integrate by parts to obtain the mean as:
            integrate(: u**(alpha-1) * exp(-u) &larr; u; u > 0
                       :) * alpha / beta / gamma(alpha)
        which is just alpha / beta, since the ingegral is gamma(alpha). """

        return self.alpha / self.beta

    def _lazy_get_variance_(self, ignored):
        """Variance of the gamma distribution.

        This is the expected value of the variate's square, minus the square of
        the mean.  The first term is:
            integrate(: (beta * x)**(alpha+1) * exp(-beta * x) &larr; x; x > 0
                       :) / beta / gamma(alpha)
            = integrate(: u**(alpha+1) * exp(-u) &larr; u; u > 0
                         :) / beta**2 / gamma(alpha)
        whose integrand is
            d(-exp(-u) * u**(alpha+1)) + (alpha+1) * u**alpha * exp(-u)
            = d(-exp(-u) * (u**(alpha+1) + (alpha+1) * u**alpha))
              + (alpha+1) * alpha * u**(alpha-1) * exp(-u)
        again with its differential terms zero at 0 and infinity, for alpha > 0,
        enabling us to integrate by parts, as for the mean, to get the expected
        square as
            alpha * (alpha+1) / beta**2
        from which we subtract (alpha/beta)**2 to get alpha / beta**2. """

        return self.alpha / self.beta ** 2

del Integrator, Lazy, math, stirling

class GammaByMeanVar (Gamma):
    __upinit = Gamma.__init__
    def __init__(self, mean, variance):
        """Initialises a Gamma using mean and variance as inputs.

        This simply infers alpha and beta from the formulae used, above, to
        compute mean and variance lazily from alpha and beta. """

        try: mean + variance**2 # expected value of square of variate
        except TypeError:
            raise TypeError(mean, variance,
                            "Incompatible quantities for mean and variance of a distribution")
        except (OverflowError, OverflowWarning): pass

        self.mean, self.variance = mean, variance
        beta = mean / variance
        self.__upinit(mean * beta, beta)
