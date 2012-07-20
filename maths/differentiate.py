"""Differentiation by brute force, to tolerable accuracy.
"""

class Single (object):
    """Function of one (real or complex) variable.

    Note that a function on the complex plane (such as abs or conjugation)
    which isn't complex-differentiable may still be real-differentiable,
    regarding the complex plane as a two-dimensional vector space over the
    reals.  This class is not suitable for such a case: use a vector
    derivative class instead.\n"""
    from study.snake.sequence import Dict as __bok
    from study.snake.sequence import List as __seq

    def __init__(self, func, complex=False, scale=1):
        """Prepare for differentiation.

        Required argument, func, is the function to differentiate.  Optional
        arguments:
          complex -- False, the default, for real differentiation; true for
                     complex differentiation.
          scale -- size of zone, around an input, to explore (see below)

        When called on an input, assorted values of func within scale of the
        given input, mostly closer to it, shall be evaluated and used to
        compute estimates at the derivative.\n"""
        self.__f, self.__scale, self.__cache = func, scale, self.__bok()
        self.__dust, self.__moment = (
            tuple(self.__swirly()), self.__messy) if complex else (
            tuple(self.__dusty()), self.__simple)

    @staticmethod
    def __dusty(scale=1, count=15, step=-.5):
        # See __swirly
        yield scale
        while count > 0:
            count -= 1
            scale *= step
            yield scale

    @staticmethod
    def __simple(vals):
        # See __messy
        mean = vals.mean()
        return mean, vals.map(lambda v, m=mean: v-m).map(
            lambda v: v*v).sum() / (len(vals) - 1.)

    import cmath
    ninth = 2j * cmath.pi / 9
    @staticmethod
    def __swirly(star=tuple(map(cmath.exp, (ninth, 4*ninth, 7*ninth))),
                 count=10, step=.4j-.3):
        """A bunch of points near zero, denser the nearer they get.

        The instance's .__dust is a set of offsets, of scale up to 1, that can
        be scaled by the radius, and added to the centre-point, of a
        neighbourhood so as to get a good sampling of points within the
        neighbourhood, at which to evaluate our function in order to look at
        chords to infer a gradient.  Either this method, for complex, or
        .__dusty() for reals, is used to compute .__dust.\n"""

        for x in star: yield x
        while count > 0:
            count -= 1
            star = map(lambda x, s=step: x*s, star)
            for x in star: yield x
    del ninth, cmath

    from study.maths.vector import Vector
    @staticmethod
    def __messy(vals, V=Vector, zero=Vector.fromSeq(((0, 0), (0, 0)))):
        mean = vals.mean()
        vary = vals.map(lambda z, vec=V: vec((z.real, z.imag))).map(
            lambda v, m=V((mean.real, mean.imag)): v-m).map(
            lambda v: v*v).sum(zero) * (1. / (len(vals) - 1))
        # That's a quadratic form, the covariance matrix: use the square root
        # of its determinant (the geometric mean of its two diagonal entries,
        # when diagonalised) as variance:
        return mean, (vary[0][0] * vary[1][1] -vary[0][1] * vary[1][0])**.5

    def __eval(self, val):
        try: ans = self.__cache[val]
        except KeyError:
            ans = self.__cache[val] = self.__f(val)
        return ans

    def __call__(self, val, scale=None):
        if scale is None: scale = self.__scale
        # Populate cache with data near val:
        self.__eval(val)
        map(lambda x, v=val, s=scale, f=self.__eval: f(v + x * s), self.__dust)
        # Find all cached data within scale of val:
        data = self.__seq(self.__cache.iteritems().filter(
                lambda (k, v), a=val, s=abs(scale): abs(k -a) <= s))
        # Find gradients of all non-empty chords between these, tagged with
        # max distance from val:
        data = self.__seq(self.__seq(data.enumerate().map(
            # For each pair of a point in data and an earlier point in data:
            lambda (i, p), d=data, m=val: d[:i].map(
                # Compute chord between points, along with max distance from val:
                lambda (q, r), (k, v)=p, m=m: (max(abs(k-m), abs(q-m)), q-k, r-v)
                # Join sub-lists (one per point and list of earlier points),
                )).sum(self.__seq()).filter(
            # Filter out empty chords, convert each to its gradient:
            lambda (m, x, y): x).map(lambda (m, x, y): (m, y/x))).sorted(
            # Sort by closeness of chords about val:
            lambda (m, w), (n, z): cmp(m, n)
            # and discard the closeness data, that we no longer need:
            ).map(lambda (m, z): z))

        # Now select a good gradient from data, preferring gradients obtained
        # from chords close about val but mistrusting those too close if
        # they're wildly different from those similarly close, as this may
        # indicate numerical instability.
        while len(data) > 15: # we start with over 100
            n = min(5, len(data) / 5)
            cuts = map(lambda i, t=len(data), n=n: (i * t + n/2)/n, range(1+n))
            part = map(lambda i, j, d=data: d[i:j], cuts[:-1], cuts[1:])
            assert len(part) == n

            # Find the part with smallest variance:
            score = map(self.__variance, part)
            best = score.index(min(score)) # favour earlier parts over later, if equal
            data = part[best]

            # but retain adjacent thirds of its neighbours:
            third = (len(data) + 1) / 3
            if best > 0: data = part[best-1][-third:] + data
            try: data += part[best+1][:third]
            except IndexError: pass

            # So each iteration reduces len(data) by a factor of 3 or, if best
            # was a boundary value, a factor of 15/4 < 4.
            assert len(data) > 3

        # Now select the mean of the low-variance sub-list we settled on:
        return self.__moment(data)[0]
