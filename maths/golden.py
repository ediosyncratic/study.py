"""Generalising the golden ratio.

See http://www.chaos.org.uk/~eddy/math/golden.html
"""

class Golden (object):
    """Iterator over the generalised golden ratios.

    The (n-1)-th golden ratio is the unique x with 1 <= x < 2 satisfying
        x**n = sum(map(lambda i: x**i, range(n)))
    The zeroth golden ratio has n-1 = 0 so n = 1, giving x = 1 trivially; the
    first has n = 2 and is what's usually known as The Golden Ratio,
    satisfying x*x = x + 1; subsequent yields of this iterator solve for a
    power of x being the sum of all earlier powers.  It is fairly easy to
    see that the solutions (for x > 0) lie between 1 and 2 and each is greater
    than the previous.

    Although the first three ratios can be obtained analytically, the rest
    solve polynomial equations of degree > 3, so I haven't even tried to solve
    them that way; I use Newton-Raphson.  Given that they're an increasing
    sequence between 1 and 2, the result from each provides a half-way decent
    first estimate for the next.\n"""
    def __iter__(self): return self
    from study.maths.polynomial import Polynomial
    __x = Polynomial((0, 1))
    del Polynomial
    def __init__(self): self.__k = self.__p = 1
    def next(self):
        self.__p *= self.__x # a power of x
        k = 2 # sentinel: if self.__k gets to 2, we've converged !
        # Newton-Raphson:
        f = self.__p - (self.__p - 1) / (self.__x - 1)
        g = f.derivative
        while k != self.__k:
            k = self.__k
            self.__k -= f(k) * 1. / g(k)

        return k
