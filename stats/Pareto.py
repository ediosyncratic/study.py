"""Computing Pareto parameters.

Where a resource is distributed among a population, we can rank the
members of that population by how much of the resource they have and
we can, for reach proportion f of the population, 0 <= f <= 1, compute
the proportion r(f) of the resouce held by the proportion f of the
population with the most of the resource.  Of necessity, r(0) = 0 and
r(1) = 1; and r is a monotonically non-decreasing function of f.
Furthermore, since the half of the population with above-median shares
of the resource have no less than the half with below-median shares,
r(1/2) >= 1/2.  So there must be some value of f at which f +r(f)
makes the transition from below 1 to above 1; this value of f is
necessarily <= 1/2; I refer to it as the Pareto parameter for the
distribution of the resource over the population.  If the greediest
20% of the population holds 80% of the resource, the Pareto parameter
is 0.2, i.e. 20%.

Functions to compute the parameter for various distributions:
  solid -- for the energy quanta distribution of an Einstein solid

More distributions shall follow when I get to them.

See study.LICENSE for copyright and license information.
"""
from study.value.quantity import Quantity
import math

def solid(b):
    """Compute the Pareto parameter for an Einstein solid.

    If the solid's modes of vibration, that carry thermal energy, have
    quantum E of energy and the solid is at temperature T, then pass b
    = E/k/T as parameter to this function; it will return the fraction
    f of the body's modes with most quanta each that hold fraction 1-f
    of the total energy.  This satisfies

        -log(f)/(1/f -2) = b/(exp(b) -1)

    Computed using Newton-Raphson, returns a Quantity.
    """

    try:
        b + 1 # compute to force TypeError if wrong dimensions
        e = b.exp
    except AttributeError:
        b = Quantity.within(b, b * .01) # 1% error bar by default
        e = b.exp
    target = b / (e - 1)

    # Solve for f with target = -log(f)/(1/f -2)
    # We know 0 <= f <= 1/2; so start in the middle of that range:
    f = Quantity(0.25)
    while True: # Apply Newton-Raphson
        # h(f) = target +log(f) / (1/f -2)
        error = target +f.evaluate(lambda x: math.log(x) / (1./x -2))
        # h'(f) = (1 -2*f +log(f))/(1 -2*f)**2
        deriv = f.evaluate(lambda x: (1 -2 * x +math.log(x)) / (1 - 2 * x)**2)
        f -= error / deriv
        # If the range of values error describes spans zero, I guess
        # we're close enough; if f runs negative, give up:
        if error.low * error.high <= 0 or f.low <= 0:
            return f
