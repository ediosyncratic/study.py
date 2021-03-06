"""Differentiation by brute force, to tolerable accuracy.

See study.LICENSE for copyright and license information.
"""

class Single (object):
    """Function of one (real or complex) variable.

    Note that a function on the complex plane (such as abs or conjugation)
    which isn't complex-differentiable may still be real-differentiable,
    regarding the complex plane as a two-dimensional vector space over the
    reals.  This class is not suitable for such a case: use a vector
    derivative class instead.\n"""

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
        self.__f, self.__scale = self.__lazy(func), scale
        self.__dust, self.__variance = (
            tuple(self.__swirly()), self.__messy) if complex else (
            tuple(self.__dusty()), self.__simple)

    from study.cache.mapping import LazyFunc
    @staticmethod
    def __lazy(f, w=LazyFunc.wrap): return w(f)
    del LazyFunc
    def __known(self):
        """Unpack LazyFunc's generic key to obtain our simple input"""
        return self.__f.known().map(lambda (k, v): (k[0][0], v))

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
        return vals.map(lambda v, m=vals.mean(): v-m).map(
            lambda v: v*v).sum() / (len(vals) - 1.)

    import cmath
    ninth = 2j * cmath.pi / 9
    @staticmethod
    def __swirly(star=tuple(cmath.exp(x) for x in (ninth, 4*ninth, 7*ninth)),
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
            star = [x * step for x in star]
            for x in star: yield x
    del ninth, cmath

    from study.maths.vector import Vector
    @staticmethod
    def __messy(vals, V=Vector, zero=Vector.xerox((2, 2))):
        """Compute (real) variance of vals.

        The instance's .__variance() takes a list of candidate derivatives and
        returns its variance.  For the real case, this is trivial, see
        .__simple(); for the complex case, since we want a real variance and
        (in any case) the usual variance doesn't make so much sense, I treat
        the complex values as real 2-vectors, compute their covariance matrix
        and return the square root of its determinant (i.e. the geometric mean
        of the two diagonal entries, when it's put into diagonal form).\n"""
        mean = vals.mean()
        vary = vals.map(lambda z, m=mean: z-m).map(
            lambda z, vec=V: vec((z.real, z.imag))).map(
            lambda v: v*v).sum(zero) * (1. / (len(vals) - 1))

        return (vary[0][0] * vary[1][1] -vary[0][1] * vary[1][0])**.5

    from study.snake.sequence import List
    @staticmethod
    def __gradients(val, near, Seq=List):
        """Return a list of gradients from an iterable over data.

        Requires (exactly) two arguments:
          val -- an input to self.__f
          near -- an iterable over (x, self.__f(x)) pairs

        The latter should include plenty of x values near val.  Constructs all
        chords between points therein, sorted by closeness of each chord's end
        furthest from val and returns a list of gradients of chords.\n"""

        data = Seq(near)
        return Seq(Seq(data.enumerate().map(
            # For each pair of a point in data and an earlier point in data:
            lambda (i, p), d=data, m=val: d[:i].map(
                # Compute chord between points, along with max distance from val:
                lambda (q, r), (k, v)=p, m=m: (max(abs(k-m), abs(q-m)), q-k, r-v)
                # Join sub-lists (one per point and list-of-earlier-points):
                )).sum(Seq()).filter(
            # Filter out empty chords, convert each to its gradient:
            lambda (m, x, y): x).map(lambda (m, x, y): (m, y/x))).sorted(
            # Sort by closeness of chords about val:
            lambda (m, w), (n, z): cmp(m, n)
            # and discard the closeness data, that we no longer need:
            ).map(lambda (m, z): z))
    del List

    def __call__(self, val, scale=None):
        if scale is None: scale = self.__scale
        # Populate cache with data near val:
        self.__f(val)
        [self.__f(val + x * scale) for x in self.__dust]

        # Get available chord-gradients near val:
        data = self.__gradients(val, self.__known().filter(
                # All cached data within scale of val:
                lambda (k, v), a=val, s=abs(scale): abs(k -a) <= s))

        # Now select a good gradient from data, preferring gradients obtained
        # from chords close about val but mistrusting those too close if
        # they're wildly different from those similarly close, as this may
        # indicate numerical instability.
        while len(data) > 15: # we start with over 100
            n = min(5, len(data) / 5)
            cuts = [(i * len(data) + n / 2) / n for i in range(1 + n)]
            part = [data[i:j] for i, j in zip(cuts[:-1], cuts[1:])]
            assert len(part) == n

            # Find the part with smallest variance:
            score = [self.__variance(p) for p in part]
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
        return data.mean()
