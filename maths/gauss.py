"""The gaussian distribution.

The primitive distribution is given by function gauss().  For (one-dimensional)
gaussian distributions, use the class Normal(mean, stddev), derived from
integrate.Integrator, q.v., so as to provide probabilities as integrals of
gauss(), with suitable scalings.

Some day I may add support for multi-dimensional gaussian distributions.  I'll
use Gaussian(mean, variance) as its interface; mean will be a vector quantity,
variance will be a tensor quantity of the same rank as mean*mean.  For the
present, a function Gaussian(mean, variance) is defined, working only for
one-dimensional data, implemented using Normal.

See also gamma.py for Gamma distributions.

$Id: gauss.py,v 1.1 2002-10-08 21:35:02 eddy Exp $
"""

import math
def gauss(x, e=math.exp, n=(2*math.pi)**.5): return e(-x*x/2)/n
del math

from integrate import Integrator
class Normal (Integrator):
    __upinit = Integrator.__init__
    def __init__(self, mean, stddev):
	"""Initialises a (one-dimensional) Normal distribution.

	Takes two arguments; mean and stddev, the mean and standard deviation
	of the normal distribution.

	If either mean or stddev (standard deviation) is a basEddy.Quantity(),
	the integrator will use the error bars intrinsic to the data in
	determining how precisely to compute integrals; otherwise, integrals are
	computed to one part in a million.  In either case, integration is done
	via the trapezium rule.

	"""

	scalar = 1
	try:
	    mean.width, mean.best
	    if callable(mean.evaluate): scalar = None
	except AttributeError: pass
	try:
	    stddev.width, stddev.best
	    if callable(stddev.evaluate): scalar = None
	except AttributeError: pass

	if scalar:
	    # ordinary python numeric types, or functional equivalents
	    def func(val, m=mean, s=stddev, g=gauss):
		return g((val-m)/s)/s
	else:
	    # Quantity(), supporting sophistication :^)
	    def func(val, m=mean, s=stddev, g=gauss):
		return ((val-m)/s).evaluate(g)/s

	self.__upinit(func, stddev)

del Integrator

def Gaussian(mean, variance):
    return Normal(mean, variance**.5)

_rcs_log = """
 $Log: gauss.py,v $
 Revision 1.1  2002-10-08 21:35:02  eddy
 Initial revision

"""
