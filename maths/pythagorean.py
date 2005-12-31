"""Search for pythagorean triangles.

For each natural i,
        (2*i+1)**2 = 4*i*(i+1) +1 = (2*i*(i+1) +1)**2 -(2*i*(i+1))**2
yields
        (2*i+1)**2 +(2*i*(i+1))**2 = (2*i*(i+1) +1)**2
hence a (2*i+1):(2*i*(i+1)):(2*i*(i+1)+1) pythagorean triangle.
Any scaling of a pythagorean triangle is likewise pythagorean.

That's the j=1 case of:
        (n+j)**2 - n**2 = 2*j*n +j**2
and whenever the latter is a perfect square we get another p.t.
"""

_rcs_id = "$Id: pythagorean.py,v 1.1 2005-12-31 16:48:44 eddy Exp $"

from natural import hcf

def hunt((n,i,j)=(0,0,0)):
    # hypotenuse n, longest other side i
    if n < i: i, n = n, i
    while 1:
        i = 1 + i
        if i >= n:
            # print 'Done:', n,
            n, i = 1 + n, 1
            while 2*i*i < n*n: i = 1 + i
            i = i - 1
            # print '\tNext:', n, i
        else:
            # check whether n**2 -i**2 is a perfect square
            gap, j = (n-i) * (n+i), 1
            while j*j < gap: j = 1 + j
            if j*j == gap and hcf(n, i, j) == 1:
                assert n*n == i*i + j*j
                return n, i, j
            # print 'Failed:', n, i

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
 Revision 1.1  2005-12-31 16:48:44  eddy
 Initial revision

"""
