"""Generate pythagorean triangles.

For each naturals i, j with i > j, the triangle with sides
        2*(i*(i+1) +(j+1)*j) +1
        2*(i*(i+1) -(j+1)*j)
        (2*i+1)*(2*j+1)
is a right-angle triangle, as may be verified by summing the squares of
the later two and comparing to the square of the first
(see http://www.chaos.org.uk/~eddy/math/pythagoras.html#Whole).

$Id: pythagorean.py,v 1.6 2008-01-28 08:25:08 eddy Exp $
"""

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

def shapes(n):
    """Returns a mapping from pythagorean rays to their slopes.

    Required argument, n, limits the search: all distinct yields from
    coprime(i,j) with 0 <= j < i < n are returned.\n"""

    i, bok = 1, {}
    while i < n:
        j = i
        while j > 0:
            j = j - 1
            h, a, c = coprime(i, j)
            if c < a: a, c = c, a
            try: bok[(h, a, c)]
            except KeyError: bok[(h, a, c)] = c * 1. / a
        i = 1 + i

    return bok

def raysvg(bok, height=1024, base=10, maxp=3, font=14):
    """Return SVG depicting the rays coprime(i,j) for j < i < n.

    Required first argument is a mapping from pythagorean rays to their slopes
    (see shapes(), above).  Optional arguments:

      height -- largest y-co-ordinate to use for depicted rays (default: 1024),
      base -- only numerator to be used when rescaling (default: 10),
      maxp -- limit on number of factors of base to use,
      font -- height of font being used by labels.

    Attempts to arrange for text and rays to avoid collisions, but only scaling
    up by naturals and down by powers of the base, never going beyond
    base**maxp.\n"""

    slop, top, text = bok.items(), 0, []
    slop.sort(lambda ((h,a,c), s), ((g,b,d), t): cmp(t,s) or cmp(a,b) or cmp(d,c) or cmp(g,h))
    while slop:
        top, skip = height, []
        for ((h, a, c), s) in slop:
            top = top - font
            if top < c + font: skip.append(((h, a, c), s))
            else:
                cut, d = (top * base ** maxp) / c, maxp
                while cut % base == 0: cut, d = cut / base, d - 1
                s = cut * 1. / base ** d
                text.append('   <line x1="0" y1="0" x2="%.*f" y2="-%.*f" /><text x="%.*f" y="-%.*f"> %d: [%d, %d] &times; %.*f </text>'
                            % (d, a * s, d, c * s, d, a * s + 1, d, c * s, h, a, c, d, s))
                top = s * c
        text.append('')
        if len(slop) > len(skip): slop = skip
        else:
            print 'Giving up on last %d items' % len(slop)
            break

    return '\n'.join(text)
