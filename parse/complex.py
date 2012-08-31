"""Depicting functions on the complex plane.

See study.LICENSE for copyright and license information.
"""

class Meromorph (object):
    """Helper for depicting functions in the complex plane.

    Can be used with an arbitrary complex function but, as the name may hint,
    is designed with the meromorphic case (a function from the complex numbers
    to itself, complex-differentiable except possibly at finitely many poles).

    Idea: to give some idea of how functions vary with phase, at a modest
    dislacement out from some significant point, draw one cycle of a slowly
    expanding spiral, at about some chosen radius from the given centre; and
    draw the image of that spiral under the function.  Ideally, vary colour
    along the spiral in a rainbow fashion, and correspondingly on its image
    (but I don't see a way to do that in SVG).  Because the source is a
    spiral, we can distinguish its ends (hence see the gross rotation of the
    source neighbourhood being mapped to destination) and, if it ends up going
    round several times once mapped, the variation in radius will avoid having
    the different cycles overlap, making it possible to see that the function
    goes round more times than it used to.\n"""

    def __init__(self, func, deriv=None, fmt="%.2f", transform=None, output=None):
        """Initialise for depiction.

        Required parameter, func, is the function on the complex plane to be
        depicted; it takes one complex input and produces a complex
        output.  Other arguments are optional and may be given either as
        positional parameters (in the following order) or as keywords:

          deriv -- the derivative of func; defaults to None, selecting brute
                   force differentiation of func as needed.
          fmt -- a format specifier to use for each floating point value in
                 the returned data; default is '%.2f' to give two significant
                 digits.
          transform -- input transform (see below); defaults to None,
                       indicating the identity.
          output -- output transform (see below); defaults to None, meaning
                    the input transform should be used also for output.

        The input and output transformations represent how z = x +1j*y
        co-ordinates in the complex plane map to display co-ordinates.  If the
        function is depicted 'in place', with inputs and outputs in the same
        display area, output=None is what you want; but, when depicting fixed
        points of the function, this is unlikely to display well, so you may
        prefer to have separate display areas in the SVG for input and output
        spaces.\n"""

        self.__inxfm = self.__tfm(transform)
        self.__outfm = self.__inxfm if output is None else self.__tfm(output)
        self.__func = func
        if deriv is None: self.__deriv = self.__rate(func)
        else: self.__deriv = deriv
        self.__fmt = lambda x, f=fmt: f % x

    def __format(self, x, y):
        return self.__fmt(x), self.__fmt(y)

    def __in(self, z):
        x, y = self.__inxfm((z.real, z.imag))
        return self.__format(x, y)

    def __out(self, z):
        x, y = self.__outfm((z.real, z.imag))
        return self.__format(x, y)

    def show(self, z, r=1, phi=0, n=6, g=.1, sweep=1):
        """Depict the mapping near z.

        Required argument, z, is the point in the complex plane about which we
        are to illustrate our function's behaviour.  Optional arguments are:

          r -- distance from z (default 1) of the mid-point of the input spiral
          phi -- phase (default 0) at which to start and end the input spiral
          n -- number of equal subdivisions into which to break up the angle
               swept by the input spiral; defaults to 6; the spirals shall be
               defined by 1+n points.
          g -- fractional increase in radius from start to middle and from
               middle to end of the input spiral; defaults to 0.1; a negative
               value will make it spiral inwards instead of outwards when
               traversed in the direction of increasing phase.  Value must be
               less than 1.
          sweep -- total angle swept out by the input spiral, in turns;
                   defaults to 1.

        These data may be given in this order as positional parameters, or
        passed as keywords; r, phi, g and sweep, together with z, describe a
        spiral curve, centred on z, that starts at z+r*(1-g)*exp(1j*phi) and
        ends at z+r*(1+g)*exp(1j*phi +2j*pi*sweep), having passed through
        z+r*exp(1j*phi +1j*pi*sweep) as its mid-point and swept out a roughly
        circular path along the way.  The spiral is exponential: the radius
        increases by the same ratio, as we move through a given angle,
        regardless of where that angle starts.  The representation of this
        spiral breaks it up into n equal segments, presenting control points
        at even angular spacing round the spiral, with the tangent at each
        computed suitably.

        Returns a tuple whose members are:
          * a twople (x, y) of strings representing z in input co-ordinates,
          * content for the d="..." attribute of a spiral <path /> about z, in
            input co-ordinates,
          * a twople (x, y) of strings representing f(z) in output co-ordinates, and
          * content for the d="..." attribute of a spiral <path /> about f(z),
            in output co-ordinates, that faithfully represents how f
            transforms the spiral given in input space.
        These can, collectively, be used to depict the action of the function
        near the given input z; the co-ordinates of z and f(z) can be used as
        end-points for an arrow from z to f(z), for example.\n"""

        if abs(g) >= 1: raise ValueError('Fractional change is too big', g)

        spiral = self.__spiral(r, g, phi, sweep, n)
        p, t = spiral.next() # first point and tangent:
        p += z
        din = [ 'M%s,%s' % self.__in(p) ]
        dou = [ 'M%s,%s' % self.__out(self.__func(p)) ]
        next = p+t, self.__func(p) +self.__deriv(p)*t

        for p, t in spiral: # each subsequent point and tangent:
            p += z
            din.append('C%s,%s %s,%s %s,%s' % (self.__in(next[0]) +
                                               self.__in(p-t) + self.__in(p)))
            q, w = self.__func(p), self.__deriv(p) * t
            dou.append('C%s,%s %s,%s %s,%s' % (self.__out(next[1]) +
                                               self.__out(q-w) + self.__out(q)))
            next = p+t, q+w

        return self.__in(z), ' '.join(din), self.__out(self.__func(z)), ' '.join(dou)

    import cmath
    @staticmethod
    def __spiral(r, g, phi, sweep, n,
                 pi=cmath.pi, exp=cmath.exp, ln=cmath.log):
        """Iterate over a spiral about 0.

        Parameters are the ones passed to __init__, q.v.  Yields each point p
        on the input spiral along with the suitably-scaled tangent q at each;
        these provide the last two cubic Bezier control points of the interval
        ending at p, namely p-q and p, and the first control point of the next
        interval, p+q.

        The spiral is of form s(t) = R*exp(t*(k +1j)) for some choice of reals
        R, k.  We have r.(1-g) = R*exp(k*phi) while r*(1+g) =
        R*exp(k*(phi+2.pi*sweep)), so (1+g)/(1-g) = exp(2*pi*k*sweep) and k =
        log((1+g)/(1-g))/2/pi/sweep.  The derivative of s(t) is just s'(t) =
        R*(k +1j)*exp(t*(k +1j)) = (k +1j).s(t).

        The cubic Bezier data at each point are the point itself, s(t), and
        the point, on the tangent at s(t), a distance along it corresponding
        to a third of the new parameter interval, s(t) +s'(t).dt/3 if the next
        point of interest along the curve is at t+dt.  We're using evenly
        spaced points of interest, in t-parameter space, so dt/3 is simply
        constant; it's sweep*2*pi/n/3.

        Thus s'(t)*dt/3/s(t) = (1j +k)*sweep*2*pi/n/3 = (log((1+g)/(1-g))
        +sweep*2j*pi)/n/3 and, aside from the factor of 3, exp(this) is simply
        the scaling to be applied from one s(t) to the next.\n"""

        point = r / (1. - g)
        if phi: point *= exp(1j * phi)
        step = (2j * pi * sweep +ln((1+g)/(1-g))) / n # change in ln(z) per step
        each = exp(step)
        step *= point / 3

        yield point, step
        while n > 0:
            n -= 1
            point *= each
            step *= each
            yield point, step

    del cmath

    from study.maths.differentiate import Single
    def __rate(self, func, brute=Single): return brute(func, True)
    del Single

    from study.parse.svgtools import Transform
    @staticmethod
    def __tfm(text, tfm=Transform.parse, idtt=Transform()):
        if text: return tfm(text)
        return idtt # the identity transform
    del Transform
