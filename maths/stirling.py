"""Approximations to factorial (and its natural logarithm)

Stirling's formula gives an approximation to factorial:
  log(n!) = (n+.5) * log(n) - n + .5 * log(2*pi) -1/12/n 
with errors of order 1/n/n.

Lanczos's formula does pretty well too - see lngamma() below.  Note: the gamma
function attains a local minimum, 0.885603194411, at about 1.4616321 and takes
the value sqrt(pi)/2 = .886 at 3/2.

It is worth knowing that, once n! &larr; n is extended to the whole complex
plane, we find: (-n)!.n!.sinc(n.pi) = 1 for each complex non-integer n (and for
n=0).  Thus factorial has a simple pole at each negative integer. """

_rcs_id_ = """
$Id: stirling.py,v 1.2 2003-10-11 15:15:38 eddy Exp $
"""

import math
roottwopi = math.sqrt(2 * math.pi) # I del this later

def factorial(n):
    result = 1

    while n > 1:
	try: result, n = result * n, n-1
	except OverflowError:
	    result, n = long(result) * n, n-1

    return result

def lnfactorial(n, log=math.log):
    result = 0.
    while n > 1: result, n = result + log(n), n-1

    return result

# Stirling's approximation:
def lnStirling(n, base=math.log(roottwopi), log=math.log):
    return (n + .5) * log(n) -n +base -1./12/n

def Stirling(n, __s=roottwopi, exp=math.exp):
    # approximates factorial
    if n: __s = __s / exp(n+ 1./12/n) # change lost between calls
    return pow(n, .5 + n) * __s

# Lanczos's approximation (with 6 coefficients and an offset of 5)
def lngamma(x,
	    coefficients=[76.18009172947146, -86.50532032941677,
			  24.01409824083091, -1.231739572450155,
			  0.1208650973866179e-2, -0.5395239384953e-5],
	    sum=1.000000000190015,
	    scale=roottwopi,
	    log=math.log):
    """Lanczos's approximation to log(Gamma).

    This works by taking Stirling's formula, with Gamma(1+n) = n!, and putting
    in corrections for the first few poles in Gamma: for some whole g, N:

      Gamma(z+1) = pow(z +g +.5, z +.5)*exp(-(z +g +.5)) *sqrt(2*pi) *series
      with series = a + b/(z+1) +... +c/(z+N)
      for suitable constants a, b, ..., c

    Substitute x = z+1 to turn this into

      Gamma(x) = pow(x +g -.5, x -.5) *exp(-(x +g -.5)) *sqrt(2*pi) *series
      with series = a + sum(: coefficients[i]/(x+i) &larr; i :)
      for suitable a.

    Interestingly, Numerical Recipies (without explaining itself) uses Gamma(z)
    = Gamma(z+1)/z rather than substitution, as here; it also asserts that the
    error in the above requires a correction of less than 2e-10 in the series,
    for z with positive real part. """

    base = x +4.5
    base = (x -.5) * log(base) - base

    for c in coefficients: sum, x = sum + c/x, 1+x

    return base + log(scale * sum)

def gamma(x, exp=math.exp, special=math.sqrt(math.pi/4)):
    result = 1

    # coerce to real part between 1 and 2
    try: r = x.real
    except AttributeError: r = x
    try:
	while r < 1: result, x, r = result * 1. / x, 1 + x, 1 + r
    except ZeroDivisionError:
	raise ZeroDivisionError, 'The Gamma function has poles at all non-positive integers'

    while r > 2:
	x, r = x - 1, r - 1
	result = result * x

    if x in (1, 2): return result
    if x == 1.5: return result * special
    else: return result * exp(lngamma(x))

def gactorial(x):
    try: return gamma(x+1)
    except ZeroDivisionError:
	raise ZeroDivisionError, 'The factorial function has a pole at each negative integer'


def expterm(x, n, scale=1, exp=math.exp): # returns pow(x,n)/n!
    while n >= 1: scale, n = scale * x * 1. / n, n-1
    while n < 0:
	n = n + 1
	scale = scale * n * 1. / x

    if n == 0: return scale
    return scale * x**n * exp(-lngamma(1+n))

# volume of the sphere, parameterised by dimension, optional radius
def sphere(dim, radius=1., pi=math.pi):
    return expterm(pi * radius**2, dim / 2.)

del roottwopi, math

# test code
def error(x):
    f = factorial(x)
    return abs(Stirling(x) - f) / f

def errorln(x):
    f = lnfactorial(x)
    return abs(lnStirling(x) - f) / f

def gerror(x):
    f = lnfactorial(x)
    return abs(lngamma(1+x) - f) / f


_rcs_log_ = """
$Log: stirling.py,v $
Revision 1.2  2003-10-11 15:15:38  eddy
Made gamma cope better with complex (comparison has changed behaviour; it's
now not OK to ask whether greater/less than one), told it about special case
at 1.5 (since that happens to be analytically exact, and relevant to sphere).

Revision 1.1  2003/09/21 16:29:14  eddy
Initial revision
"""
