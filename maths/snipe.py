"""An attempt to solve  XKCD 356's problem by relaxation.

Unfortunately, convergence is slow.
TODO: convert to use NumPy.
Using a dict as lattice is probably expensive.

See study.LICENSE for copyright and license information.
"""

class Grid (object):
    def __init__(self, source=(2, 1), sink=None, ranges=None):
        if sink is None:
            sink = tuple(0 * x for x in source)
        if ranges is None:
            ranges = 1 << (16 / len(source))
        try: ranges[:]
        except (TypeError, ValueError, AttributeError):
            ranges = tuple((min(p) - ranges, max(p) + ranges) for p in zip(source, sink))
        self.__ranges = ranges
        self.__givens = {source: +1, sink: -1}
        self.__flow = 0
        lattice = self.__grid(self.__prop(sink, source))
        self.lattice = lattice

    @staticmethod
    def __prop(sink, source):
        """Implement a half-way sensible initial lattice.

        Returns a function from position to initial value for the
        relative potential at that position.  Ideally, this would be
        an exact solution to the equation; but I don't know an
        analytic form for that.
        """
        def share(pos, lo=sink, hi=source):
            # Distances, along lattice, from pos to source and sink:
            dn = sum(abs(p - x) for p, x in zip(pos, lo))
            up = sum(abs(p - x) for p, x in zip(pos, hi))
            # return (dn - up) * 1. / (dn + up)
            return +1 if up == 0 else -1 if dn == 0 else 1. / up - 1. / dn
        return share

    def __grid(self, f):
        bok = {}
        for pos in self.__indices():
            bok[pos] = f(pos)
        return bok

    def faces(trip, i):
        """Tool used by __bounds

        Used to select indices for co-ordinate direction trip[0],
        whose range is trip[1], when iterating the two faces in
        direction i.  Avoids duplicating edges between faces by having
        lower-index directions iterate the full range of each
        higher-index direction; each higher-index direction can then
        presume the lower-index directions' faces have already dealt
        with their intersection."""
        j, pair = trip
        if i < j: return range(pair[0], pair[1] + 1)
        if i > j: return range(pair[0] + 1, pair[1])
        return pair

    from study.snake.sequence import Iterable
    def __indices(self, c=Iterable.cartesian):
        return c(lambda p: range(p[0], p[1] + 1), *self.__ranges)
    def __bounds(self, c=Iterable.cartesian, f=faces):
        triples = tuple(enumerate(self.__ranges))
        for trip in triples:
            i, (low, high) = trip
            for pos in c(lambda t, j=i: f(t, j), *triples):
                yield pos
    del Iterable, faces

    def __clip(self, pos):
        for data in zip(pos, self.__ranges):
            x, (low, high) = data
            if x < low: yield low
            elif x < high: yield x
            else: yield high

    def __getitem__(self, pos):
        return self.lattice[tuple(self.__clip(pos))]

    def __about(self, pos):
        total, count = 0, 0
        for i, v in enumerate(pos):
            count += 2
            total += self[pos[:i] + (v -1,) + pos[i+1:]] + self[pos[:i] + (v +1,) + pos[i+1:]]
        return total * 1. / count

    def step(self):
        """Update lattice to a new time-step.

        Keeps source and sink at +1 and -1, updates every other vertex
        to the average of its neighbours' prior values."""
        fresh = self.__grid(lambda p: 0)
        delta, flow = 0, 0
        for pos in self.__indices():
            mean = self.__about(pos)
            try: here = self.__givens[pos]
            except KeyError:
                here = mean
                diff = abs(self[pos] - mean)
                if diff > delta: delta = diff
            else:
                diff = abs(here - mean)
                if diff > flow: flow = diff
            fresh[pos] = here
        self.lattice = fresh
        self.__flow = flow * 2 * len(self.__ranges)
        return delta

    def hop(self, f=lambda o, n: .5 * (o +n)):
        """Munge together lattices from before and after a step, then step again.

        Takes one parameter, a munger function taking two values; this
        defaults to taking their average.  For each node in the
        lattice, after taking a step, this combines the prior and
        updated potentials at the node (as first and second parameters
        to the function, respectively) to get the potential to use for
        that node at the subsequent step(), whose return hop()
        returns.

        Thus, if you notice successive steps give impedance
        alternating between two values, slowly getting closer
        together, using the default munger gives you a compromise
        between the two states it's bouncing between; the impedance is
        then apt to 'jump' to an intermediate value, saving many
        step()s.  Alternatively, if each step() seems to be taking the
        impedance in the same direction, using lambda o, n: 2*n -o as
        munger may jump ahead a step; similar extrapolations might
        jump ahead several steps.  For general positive k, (1+k)*n
        -k*o is a plausible munging of the new and old values n and o;
        k < 1 compromises while k > 1 extrapolates.

        Of course, there's no guarantee that the impedance shall
        change in a manner similar to what the munger imposes on the
        potentials, but experiment may reveal neat ways to speed up
        the convergence."""
        lattice = self.lattice
        self.step()
        for k, v in lattice.items():
            self.lattice[k] = f(v, self.lattice[k])
        return self.step()

    def grind(self, tol=None):
        """Keep step()ping until a given tolerance is met.

        Single argument, tol, is a threshold below which to drive
        step().  If tol is None (its default) half of an initial
        step()'s return is used.  This calls step() until the return
        is less than tol, then returns the result of a final step()."""
        if tol is None: tol = .5 * self.step()
        while self.step() > tol: pass
        return self.step()

    def support(self, tol=1e-6):
        """Returns the size of region with potential above a threshold.

        Single argument, tol, defaults to 1e-6.  Finds the furthest
        node from source or sink at which the potential exceeds tol,
        using the L1 (a.k.a. Taxi-Cab) metric, i.e. sum of absolute
        values of co-ordinates, which is the number of resistors on a
        shortest current path from the node to source or sink."""
        def radius(pos, roots=tuple(self.__givens.keys())):
            return max(sum(abs(x - y)) for x, y in zip(pos, r)
                       for r in roots)
        best = sum(abs(y - x) for x, y in self.__ranges)
        for pos in self.__indices():
            if abs(self[pos]) > tol:
                here = radius(pos)
                if here < best: best = here
        return best

    @property
    def bound(self):
        return max(abs(self.lattice[pos]) for pos in self.__bounds())

    @property
    def impedance(self):
        return 2. / self.__flow
