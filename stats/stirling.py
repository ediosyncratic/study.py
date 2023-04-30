"""Approximations to factorial (and its natural logarithm)

Stirling's formula gives an approximation to factorial:
  log(n!) = (n+.5) * log(n) - n + .5 * log(2*pi) -1/12/n 
with errors of order 1/n/n.

Lanczos's formula does pretty well too - see lngamma() below.  Note: the gamma
function attains a local minimum, 0.885603194411, at about 1.4616321 and takes
the value sqrt(pi)/2 = .886 at 3/2.

It is worth knowing that, once n! &larr; n is extended to the whole complex
plane, we find: (-n)!.n!.sinc(n.pi) = 1 for each complex non-integer n (and
for n=0).  Thus factorial has a simple pole at each negative integer.

The HAKMEM papers e.g. at
http://www.inwap.com/pdp10/hbaker/hakmem/hakmem.html
offer

  n! * (-n)! * sinc(pi*n) = 1
  (2*pi)**(.5*(n-1)) * (n*z)! / n**(n*z+.5) = z!*(z-1./n)!*...*(z-(n-1)/z)!

Stirling's formula gives an approximation to factorial:
  log(n!) = (n+.5) * log(n) - n + .5 * log(2*pi) -1/12/n 
with errors of order 1/n/n.  I have another scribble elsewhere which claims
  n! = n**(n+.5) * exp(-n) * (2*pi)**.5 * (1 +1/12/n +1/288/n/n +...)
which would appear to be disagreeing about the sign of the 1/12/n term.
It also suggests the tail will have a factor of 12 for each factor of n.

Lanczos's formula does pretty well too - see lngamma() below.  Note: the gamma
function attains a local minimum, 0.885603194411, at about 1.4616321 and takes
the value sqrt(pi)/2 = .886 at 3/2.

It is worth knowing that, once n! &larr; n is extended to the whole complex
plane, we find: (-n)!.n!.sinc(n.pi) = 1 for each complex non-integer n (and
for n=0).  Thus factorial has a simple pole at each negative integer.

For chose(N,m) = N!/m!/(N-m)! Stirling implies the approximation
  log(chose(n+m,m))
  = (n+m+.5)*log(n+m) -(n+.5)*log(n) -(m+.5)*log(m) -.5*log(2*pi) +(1/n +1/m -1/(n+m))/12
  = .5 * log((1/n +1/m)/2/pi) +n*log(1+m/n) +m*log(1+n/m) +(1+n/m+m/n)/(n+m)/12

See study.LICENSE for copyright and license information.
"""

import math, cmath
roottwopi = math.sqrt(2 * math.pi) # I del this later
from study.snake.decorate import postcompose
@postcompose
def asreal(val):
    try:
        if val.imag == 0: return val.real
    except AttributeError: pass
    return val
del postcompose

from study.maths.Pascal import factorial

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

# Related, see http://www.chaos.org.uk/~eddy/math/factorial.html#Approx
def quarup(x): return x * (1 +2./(9 +(8*x)**2)) # temp, deleted below.
def halfac(n, rootish=lambda x, q=quarup: q(x)**.5):
    """n => n!/(n-.5)!

    i.e. gamma(n+1)/gamma(n+.5), for which a respectably good
    approximation is available.\n"""
    return rootish(n + .25)
def c2nn(n, rnp=lambda x, pi=math.pi, q=quarup: (q(x) * pi)**.5):
    """Short-cut for chose(2*n, n).

    Like halfac, a respectably good approximation is available.
    See also study.maths.Pascal.c2nno4n(n).\n"""
    return 4**n / rnp(n + .25)
del quarup

# Lanczos's approximation (with 6 coefficients and an offset of 5)
@asreal
def lngamma(x,
            coefficients=[76.18009172947146, -86.50532032941677,
                          24.01409824083091, -1.231739572450155,
                          0.1208650973866179e-2, -0.5395239384953e-5],
            sum=1.000000000190015,
            scale=roottwopi,
            log=cmath.log):
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

    Interestingly, Numerical Recipies (without explaining itself) uses
    Gamma(z) = Gamma(z+1)/z rather than substitution, as here; it also asserts
    that the error in the above requires a correction of less than 2e-10 in
    the series, for z with positive real part.\n"""

    base = x +4.5
    base = (x -.5) * log(base) - base

    for c in coefficients: sum, x = sum + c/x, 1+x
    return base + log(scale * sum)

@asreal
def gamma(x, exp=cmath.exp, special=math.sqrt(math.pi/4)):
    result = 1

    # coerce to real part between 1 and 2
    try: r = x.real
    except AttributeError: r = x
    try:
        while r < 1: result, x, r = result * 1. / x, 1 + x, 1 + r
    except ZeroDivisionError:
        raise ZeroDivisionError(
            'The Gamma function has poles at all non-positive integers')

    while r > 2:
        x, r = x - 1, r - 1
        result = result * x

    if x in (1, 2): return result
    if x == 1.5: return result * special
    return result * exp(lngamma(x))

def gactorial(x):
    try: return gamma(x+1)
    except ZeroDivisionError:
        raise ZeroDivisionError(
            'The factorial function has a pole at each negative integer', x)

def chose(n, m):
    """Uses gamma(1+x) = x! to generalise chose(n, m) = n!/m!/(n-m)!

    This gets complicated by the case where n is a negative integer: n! is
    infinite.  If m is an integer, at least one of m! and (n-m)! is also
    infinite and we have to resolve cancelling infinities; otherwise, the
    result is infinite (so this raises ZeroDivisionError).

    When n < 0 and m are integers, if 0 > m > n then both m! and (n-m)! are
    infinite so we should expect the answer 0, matching the n > 0 case's value
    for 0 > m or m > n.  For m <= n < 0, we have n!/m!/(n-m)! with n-m > 0 and
    n!/m! = n.(n-1)...(m+1) = (-1)**(n-m) * (-(m+1))!/(-(n+1))! so chose(n, m)
    = (-1)**(n-m)*chose(-(m+1), -(n+1)).  For n < 0 <= m we likewise have
    n!/m!/(n-m)! = (-1)**m * (m-1-n)!/m!/(-(1+n))! = (-1)**m * chose(m-1-n,
    m).

    Note that chose(-1, -1) +chose(-1, 0) is 2 instead of 1 = chose(0, 0):
    otherwise, the recurrence relation chose(n,m) = chose(n-1,m)
    +chose(n-1,m-1) is honoured; and that chose is badly behaved near the tidy
    values derived above.  See
    http://www.chaos.org.uk/~eddy/math/lacsaP.html
    for the rationale behind handling of n < 0.\n"""

    try: num = gactorial(n)
    except ZeroDivisionError: pass
    else:
        try: return num / gactorial(m) / gactorial(n-m)
        except ZeroDivisionError: return 0
    assert n < 0 and n == long(n)
    try: den = gactorial(m) * gactorial(n-m)
    except ZeroDivisionError: pass
    else: raise ZeroDivisionError(
     'chose(n, m) has a pole, for non-integer m, at each negative integer n',
     n, m)
    assert m == long(m)
    if 0 > m > n: return 0
    if m >= 0 > n: return (-1)**m * chose(m-1-n, m)
    assert 0 > n >= m and n == long(n)
    return (-1)**(n-m) * chose(-m-1, -n-1)

@asreal
def expterm(x, n, scale=1, exp=cmath.exp): # returns pow(x,n)/n!
    while n >= 1: scale, n = scale * x * 1. / n, n-1
    while n < 0:
        n = n + 1
        scale = scale * n * 1. / x

    if n == 0: return scale
    return scale * x**n * exp(-lngamma(1+n))

# volume of the sphere, parameterised by dimension, optional radius
def sphere(dim, radius=1, pi=math.pi):
    return expterm(pi * radius**2, .5 * dim)

del roottwopi, math, cmath, asreal

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
