"""Cardan's solution of the cubic.

See also HAKMEM note on cubics in search.py
See study.LICENSE for copyright and license information.
"""

note = """Mathologer
https://www.youtube.com/watch?v=N-KXStupwsc
claims a.power(3) +b.power(2) +c.power(1) +d has a zero at
-b/a/3
+power(1/3, u -power(1/2, power(2, u) +power(3, v))
+power(1/3, u +power(1/2, power(2, u) +power(3, v))
wherein
u = -power(3, b/3/a) +b.c/a/a/6 -d/a/2
v = c/a/3 -power(2, b/a/3)

Since the power(1/2) term appears with both signs, there is no [+-],
so this is only one root - except when u.u +v.v.v < 0, when our square
roots get us two conjugate complex values, whose cube roots we then
take; and each has three of those.  This, of course, turns into the
cos(angle/3) formula.

When there are three roots:

Draw tangents to a cubic at each of its zeros; each cuts the cubic at
another point, unless the root is the inflection, in which case count
it as the other cut.  These three cut points lie on a line.


Further, there is an equilateral triangle whose centre has the same x
coordinate as the inflection and whose corners have the x co-ordinates
of the three roots; its incircle's maximal and minimal x co-ordiantes
are the extrema of the cubic.

For any triangle, there's a unique elipse that's tangent to each edge
at its mid-point.  (It's surely obtained by shearing and stretching an
equilateral's incircle.)  Form the triangle whose three vertices are
the roots of your cubic (with possibly complex coefficients); the
elipse that touches each of its sides in its mid-point has the cubic's
extrema as its foci.  Marden's theorem.  The elipse's centre is the
inflection.

General quartic, sum(: [e, d, c, b, a].power :)
divide through by a, shift origin by b/a/4 to reduce to
power(4) +p.power(2) +q.power(1) +r
power(2, x.x +p) = power(4, x) +2.p.power(2, x) +p.p = p.x.x -q.x -r +p.p
For any z, we then have
power(2, x.x +p +z) = (p +2.z).x.x -q.x -r +power(2, p +z)
so pick a z that makes the rhs a square;
(p +2.z)(x +h)^2 has h.h.(p +2.z) = power(2, p +z) -r, 2.h.(p +2.z) = q
so h = q/(p +2.z)/2,
h.h.(p +2.z) = q.q/(p +2.z)/4 = power(2, p +z) -r
q.q/4 = (p +2.z).((p +z)^2 -r); cubic in z; solve; substitute
Then each z-root gives a quaratic for x that we can solve.
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

    To be specific: for each x in Cardan(a, b, c, d)
        ((a*x +b)*x +c)*x +d 
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
    try:
        if not disc.imag:
            disc = disc.real
            raise AttributeError
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

    for x in ans:
        v = ((u * x + s) * x + i) * x + c
        if v:
            #print '%s -> %s' % (x, v)
            assert abs(v) < tol, '%s -> %s' % (x, v)

    return ans
