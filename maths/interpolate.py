"""Polynomial interpolation.

$Id: interpolate.py,v 1.2 2009-03-05 07:19:47 eddy Exp $
"""

from polynomial import Polynomial
from reduce import System

def interpolator(data):
    """Construct a Polynomial through desired points.

    Since argument, data, is a dictionary mapping inputs to the polynomial to
    their desired outputs.  Current implementation only supports integer
    (including long) keys; I should ultimately make it more liberal, at least as
    to outputs.  Returned polynomial's order is equal to the number of entries
    in the supplied dictionary.

    We're solving for a list c of coefficients given:
      value = sum(: c[i] * power(i, key) &larr;i :)
    for each key, valu in our dictionary.  This is a simple matrix problem :-)\n"""

    if filter(lambda x: not ( isinstance(x, int) or isinstance(x, long) ), data.keys()):
        raise NotImplementedError, 'Only integral arguments supported for now'
    else:
        n = len(data)
        solve = System(len(data),
                       map(lambda k: map(lambda i, k=k: k**i, range(len(data))),
                           data.keys()))
        coeffs = solve.obtain(data.values())
        if len(coeffs) > n:
            assert len(coeffs) == n + 1
            coeffs, scale = coeffs[:n], coeffs[n]
            if scale != 1:
                return Polynomial(coeffs, denominator=scale)

    return Polynomial(coeffs)
