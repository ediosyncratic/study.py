"""Generalising the golden ratio.

See http://www.chaos.org.uk/~eddy/math/golden.html
See study.LICENSE for copyright and license information.
"""

class Golden (object):
    """Iterator over the generalised golden ratios.

    The (n-1)-th golden ratio is the unique x with 1 <= x < 2 satisfying
        x**n = sum(x**i for i in range(n))
    The zeroth golden ratio has n-1 = 0 so n = 1, giving x = 1 trivially; the
    first has n = 2 and is what's usually known as The Golden Ratio, satisfying
    x*x = x + 1; subsequent yields of this iterator solve for a power of x being
    the sum of all earlier powers.  It is fairly easy to see that the solutions
    (for x > 0) lie between 1 and 2 and each is greater than the previous.  Note
    that the polynomial equation's right-hand side is just (x**n - 1) / (x - 1),
    so the equation is almost equivalent to (x - 1) * x**n = x**n - 1 (but this
    has 1 as an extra root that the original equation usually lacks).

    Although the first three ratios can be obtained analytically, the rest solve
    polynomial equations of degree > 3, so I haven't even tried to solve them
    that way; I use Newton-Raphson.  Given that they're an increasing sequence
    between 1 and 2, the result from each provides a half-way decent first
    estimate for the next.\n"""
    def __iter__(self): return self
    from study.maths.polynomial import Polynomial
    __x = Polynomial.power(1)
    del Polynomial
    def __init__(self): self.__k = self.__p = 1
    def next(self):
        self.__p *= self.__x # a power of x
        k = 2 # sentinel: if self.__k gets to 2, we've converged !
        # Newton-Raphson to find a zero of f:
        f = self.__p - (self.__p - 1) / (self.__x - 1)
        g = f.derivative
        while k != self.__k:
            k = self.__k
            self.__k -= f(k) * 1. / g(k)

        return k

from study.value.quantity import Quantity

golden = Quantity((1 + 5.**.5) / 2,
                  doc = """The golden ratio.

This is the positive solution to the quadratic equation x*x = x+1; divide -1 by
it to get the negative solution.  One can re-write the equation as (2*x-1)**2 =
4*x*x -4*x +1 = 4*(x*x-x) + 1 = 5, whence the solutions are (1 +/- 5**.5)/2.

A rectangle whose long side is this times its short side has the property that,
if your cut from it a square on a short side, the remainder rectangle, though
smaller, has the same shape, i.e. ratio of long side (which was the original's
short side) to short side (the difference between the original's sides).
""")

assert golden**2 == golden+1

del Quantity
