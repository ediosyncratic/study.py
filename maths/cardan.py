"""Cardan's solution of the cubic.

See also HAKMEM note on cubics in search.py
See study.LICENSE for copyright and license information.
"""
from math import cos, acos, pi

def Cardan(cube, square, linear, constant):
    """Solves a cubic polynomial equation.

    Takes four arguments, the coefficients of the cube, square, linear and
    constant terms in the cubic, respectively.  Finds all the inputs at which
    that cubic yields zero as output; returns these as a tuple.  Repeated roots
    are appropriately repeated.  Raises ValueError if there are no roots (or if
    *every* input yields zero output - i.e. all arguments are zero); this can
    only happen if the first argument is zero.

    To be specific: all entries in
        map(lambda x: ((a*x +b)*x +c)*x +d, Cardan(a, b, c, d))
    will be tiny.  See cardan(), which wraps Cardan and asserts this.
"""

    assert cube, "Cardan solves cubics, use other tools for quadratics"
    #print 'Cardan(%s, %s, %s, %s)' % (cube, square, linear, constant)

    # canonicalise:
    # u*x**3 +s*x**2 +i*x +c == 0 iff
    # x**3 +(s/u)*x**2 +(i/u)*x +(c/u) == 0 iff, with y = x +s/u/3, x = y - s/u/3,
    # (y - s/u/3)**3 +(s/u)*(y - s/u/3)**2 +(i/u)*(y - s/u/3) +c/u == 0 iff
    # y**3 +3*y*(s/u/3)**2 -(s/u/3)**3 -6*y*(s/u/3)**2 +3*(s/u/3)**3 +y*i/u -i*s/u/u/3 +c/u == 0 iff
    # y**3 -3*y*((s/u/3)**2 -i/u/3) +2*(s/u/3)**3 -i*s/u/u/3 +c/u == 0
    offset = square / 3. / cube # y = x + offset
    E = offset**2 - linear / 3. / cube
    F = linear * square / 6. / cube**2 -offset**3 - constant * .5 / cube
    # so cube*(y**3 -3*E*y -2*F) = ((cube*x +square)*x +linear)*x +constant;
    # now solve y**3 -3*E*y -2*F = 0 and subtract offset from each y to get x.
    #print 'offset, E, F = %s, %s, %s' % (offset, E, F)

    def cuberoot(x, third=1./3):
        # python doesn't like fractional powers of negative values ...
        if x < 0: return - (-x)**third
        return x**third

    # deal with two more easy cases:
    if not F: # 0 = y**3 -3*E*y = y*(y**2 -3*E)
        if E < 0: return -offset, # only one
        E = (3*E)**.5
        return -offset, -E-offset, E-offset
    if not E: # y**3 = 2.F
        return cuberoot(2*F) - offset,

    disc = F**2 -E**3
    if disc > 0: # only one root
        disc = disc**.5
        return cuberoot(F + disc) + cuberoot(F - disc) - offset,

    assert E > 0
    E = E**.5
    if disc < 0: # three roots; 
        a = acos(F / E**3) / 3
        #print 'angle:', a * 180 / pi
        return 2*E*cos(a) - offset, 2*E*cos(a + 2*pi/3) - offset, 2*E*cos(a + 4*pi/3) - offset

    # disc == 0, two roots, one repeated
    if F < 0:
        F = E - offset
        return F, F, -E-offset

    F = -E - offset
    return E-offset, F, F

def quadratic(square, linear, constant, realonly=None):
    disc = linear **2 -4. * square * constant
    try: disc.imag
    except AttributeError:
        if disc < 0:
            if realonly: raise ValueError, 'Positive definite quadratic has no real roots'
            disc = 1j * (-disc)**.5
        elif disc > 0: disc = disc**.5
    else: disc = disc**.5
    mid, gap = -linear * .5 / square, .5 * disc / square
    return mid + gap, mid - gap

def cubic(cube, square, linear, constant, realonly=False):
    """Returns a tuple of roots of the specified cubic polynomial.

    Required arguments cube, square, linear and constant give the coefficients
    of the eponymous powers.  Optional argument, realonly, controls whether
    complex roots should be included (at least when all coefficients are real;
    it's possible this implementation ignores it when some are complex): its
    default is False, allowing complex roots; setting it True restricts results
    to complex roots.\n"""

    if not cube:
        # deal with degenerate cases
        if not square:
            try: return -constant * 1. / linear,
            except ZeroDivisionError:
                raise ValueError, 'Constant cubic has no roots (or everything)'
        try: return quadratic(square, linear, constant, realonly)
        except ValueError: return ()

    # deal with easy special case:
    if not constant:
        try: ans = quadratic(cube, square, linear, realonly)
        except ValueError: ans = ()
        return (0,) + ans

    ans = Cardan(cube, square, linear, constant)
    if realonly or len(ans) == 3: return ans
    root, = ans # assert: len(ans) is 1
    # divide our cubic by (: x - root &larr; x :) and get its roots
    assert root, 'zero root was meant to be dealt with earlier !'
    # u*x*x*x +s*x*x +i*x +c = (x-r)*(u*x*x +(r*u+s)*x -c/r)
    # assert root*(root*cube+square) +constant/root +linear == 0, "it should at least be close ;^>"
    return ans + quadratic(cube, square + root * cube, -constant / root)

def cardan(u, s, i, c, realonly=1, tol=1e-14):
    # debug wrapper on the above, doing the assertion
    ans = cubic(u, s, i, c, realonly)

    for x, v in map(lambda x, u=u, s=s, i=i, c=c: (x, ((u*x +s)*x +i)*x +c), ans):
        if v:
            #print '%s -> %s' % (x, v)
            assert abs(v) < tol, '%s -> %s' % (x, v)

    return ans
