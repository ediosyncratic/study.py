"""Generate pythagorean triangles.
"""

def whole(i, j):
    """Generate a pythagorean triangle.

    Takes two naturals, i and j, with i > j.  Returns the tuple of three
    entries given by:
        2*(i*(i+1) +(j+1)*j) +1
        2*(i*(i+1) -(j+1)*j)
        (2*i+1)*(2*j+1)

    These are the sides of a right-angle triangle, as may be verified by
    summing the squares of the later two and comparing to the square of the
    first (see http://www.chaos.org.uk/~eddy/math/pythagoras.html#Whole).  Of
    course, any scaling of one of these is also a pythagorean triangle.  See
    coprime() for the same triangle rescaled to make its sides mutually
    coprime.  Every pythagorean triangle has this form, for some i, j and
    scaling.\n"""
    ans = ( 2*(i*(i+1) +(j+1)*j) +1, 2*(i*(i+1) -(j+1)*j), (2*i+1)*(2*j+1) )
    assert ans[1]**2 +ans[2]**2 == ans[0]**2
    return ans

from natural import hcf
def coprime(i, j):
    """Returns a pythagorean triangle whose sides are coprime.

    See whole() for details; this takes the same parameters.\n"""
    h, a, c = whole(i, j)
    f = hcf(h, a, c)
    return h/f, a/f, c/f

