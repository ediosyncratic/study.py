"""Generate pythagorean triangles.

For each naturals i, j with i > j, the triangle with sides
        2*(i*(i+1) +(j+1)*j) +1
        2*(i*(i+1) -(j+1)*j)
        (2*i+1)*(2*j+1)
is a right-angle triangle, as may be verified by summing the squares of
the later two and comparing to the square of the first
(see http://www.chaos.org.uk/~eddy/math/pythagoras.html#Whole).
"""

_rcs_id = "$Id: pythagorean.py,v 1.2 2005-12-31 16:59:10 eddy Exp $"

def whole(i, j):
    ans = ( 2*(i*(i+1) +(j+1)*j) +1, 2*(i*(i+1) -(j+1)*j), (2*i+1)*(2*j+1) )
    assert ans[1]**2 +ans[2]**2 == ans[0]**2
    return ans

from natural import hcf
def coprime(i, j):
    h, a, c = whole(i, j)
    f = hcf(h, a, c)
    return h/f, a/f, c/f

pythanim = """
For the pythagorean animation, I need the size-lists of the two small squares,
and a list of times (expressed as fractions of a half turn) for which linear
interpolation among those sizes equates to smooth variation of angle.  At any
given time-tick, the two sizes are cos and sin of an angle; we need each to be
an exact decimal in not many digits, and we need the angle, as fraction of the
half turn, to also be an exact decimal in not many digits.  We only really need
to cover a quarter turn, since the half turn can be completed by reversal and
shuffling.

So what we really need is three lists n, s, c, of equal length N
 * their entries must all be short decimal fractions,
 * n must be sorted in increasing order, start at 0 and end at .5
 * successive entries in n must be close together
 * for each i in range(N),
   * s must be a good approximation to sin(n),
   * c must be a good approximation to cos(n) and
   * s*s + c*c must be a very good approximation to 1.
"""


_rcs_log = """
 $Log: pythagorean.py,v $
 Revision 1.2  2005-12-31 16:59:10  eddy
 No need to search: general solution known :-)

 Initial Revision 1.1  2005/08/18 16:48:44  eddy
"""
