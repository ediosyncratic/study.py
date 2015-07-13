# -*- coding: iso-8859-1 -*-
"""The Titius-Bode law, a.k.a. Bode's law:

A mathematical formula which generates, with modest accuracy, the semimajor
axes of the planets in order out from the Sun.  Write down the sequence
        0, 3, 6, 12, 24, 48, 96, ...
and add 4 to each term:
        4, 7, 10, 16, 28, 52, 100, ...
Then divide each term by 10.  This leaves you with the series
        0.4, 0.7, 1.0, 1.6, 2.8, 5.2, 10.0, 19.6, 38.8, ...
which reasonably approximate the semimajor axes of the planets, measured in
astronomical units; more accurately they're
    0.38692, 0.72300, 1, 1.52296, 5.1998, 9.556, 19.25, 30.18, 39.59
but the last few of these (which are better modelled by an arithmetic
progression in steps of 10 AU) weren't known when the pattern was first
noticed.

Bode's law had no theoretical justification when it was first introduced; it
did, however, agree with the soon-to-be-discovered planet Uranus' orbit (19.25
AU actual; 19.6 AU predicted).  Similarly, it predicted a missing planet
between Mars and Jupiter, and shortly thereafter the asteroids were found in
very similar orbits (2.77 AU actual for Ceres; 2.8 AU predicted).  The series,
however, seems to skip over Neptune's orbit.  The form of Bode's law (that is,
a roughly geometric series) is not surprising, considering our theories on the
formation of solar systems, but its particular formulation is thought of as
coincidental.  In particular, it is notable that the pattern from Saturn
outwards is better modelled by an arithmetic series (in steps of 10 AU), whose
next value inwards would be roughly zero, -0.46 AU.

(That's from http://www.alcyone.com/max/physics/laws/b.html, and 0.46 AU is
about 100 times the Sun's radius; while
http://www.astropa.unipa.it/versione_inglese/Hystory/BODE'S_LAW.htm
gives some history, as follows.)

The law first appeared in 1766 in a translation, by Johann Daniel Titius von
Wittenberg, of Contemplation de la Nature (1764), by the French natural
philosopher Charles Bonnet.  Titius appears to have drawn the law from private
correspondence with others.  Johann Bode repeated it in a foot-note, then went
on to become a professional astronomer, so everyone cited him as source ...

Herschel's discovery (March 1781) of Uranus encouraged various astronomers to
search for a planet in the gap between Mars and Jupiter, where the law
predicts a planet at 2.8 AU; in 1800, a team set up an international
collaboration; starting on the first day of the next year, Piazzi made the
first of a series of observations of what he soon realised was such a missing
planet, which he named Ceres Ferdinandea.

It is possible to match the actual radii marginally better with an offset plus
arithmetic series (as in Bode's law) if we use a factor slightly below two as
ratio in the arithmetic series.  The hard part is optimizing the
approximation. Alternatively, we can look at the patten from the outskirts
inward and see the 10AU linear sequence from Neptune in to Saturn, with zero
as next sequence entry, then describe the inner portion in terms of scaling
down with an offset.

See study.LICENSE for copyright and license information.
"""

from study.snake.lazy import Lazy

class Bodalizer (Lazy):
    """The auto-Bodalizer.

An instance of this class tries to automatically select three constants zero,
unit and base for which a family of orbits' radii have values close to
        zero + unit * base**i
for various i.  It allows i to change in steps other than 1, but should (though
it presently does not) try to ensure that such exceptions are rare.

Constructor takes a sequence of space.body.Object instances, selects a sub-set
of them (the instances of the earliest of Planet, Planetoid, Body or Object to
have several instances in the sequence) and records the orbital radii of this
sub-set.  When it comes to need values for zero, unit and base it does the
necessary computation to match this sequence of radii to the above pattern.

Public attributes:
 * zero, unit, base -- the defining attributes mentioned above.

Public methods:
 * use(zero, unit, base) -- over-ride the given attributes
 * index(radius) -- i = log((radius - zero) / unit) / log(base)
"""

    def __init__(self, seq):
        # First identfy your demos ...
        plenty, k = len(seq), 3
        while k * k < plenty: k = 1 + k
        plenty = k # max(3, sqrt(len(seq)) rounded up)
        self.__seq = self.__enough(seq, plenty)

    def __len__(self): return len(self.__seq)
    def __getitem__(self, key):
        return self.zero + self.unit * self.base**key

    def __repr__(self):
        return 'lambda i: %s + %s * %s ** i' % (self.zero, self.unit, self.base)

    def index(self, radius):
        r = (radius - self.zero) / self.unit
        if r.low < .0001: return 0 # avoid domain errors and hugely -ve answers ...
        # (while incidentally being polite to Mercury)
        return r.log / self.__base

    # Auto-detection of zero, unit and base:

    from study.value.quantity import Quantity, tera
    # NB: Any scalar Quantity has attributes exp and log (among others) we can exploit ;^)

    def median(seq, span=1, Q=Quantity.flat): # tool func
        seq.sort()
        i, b = divmod(len(seq), 2)
        if b: best = seq[i].best
        else: best = .5 * (seq[i-1].best + seq[i].best)
        i, b = divmod(len(seq) - span, 2)
        if b: lo, hi = seq[i].best, seq[-1-i].best
        else: lo, hi = seq[i].low, seq[-1-i].high
        return Q(lo, hi, best)

    def __enough(self, seq, plenty, mid=median, cache=[]):
        try: bodytypes = cache[0]
        except IndexError:
            from body import Planet, Planetoid, Body, Object
            bodytypes = ( Planet, Planetoid, Body, Object )
            cache.append(bodytypes)

        for k in bodytypes:
            row = [x for x in seq if isinstance(x, k)]
            if len(row) < plenty: continue

            # try to eliminate any initial or final arithmetic sequences
            row = [ x.orbit.radius for x in row ]
            gap = [ y - x for x, y in zip(row[:-1], row[1:]) ]
            rat = [ (y / x).log for x, y in zip(gap[:-1], gap[1:]) ]
            cut = mid(rat) / 5

            # Could perhaps do better by considering every ratio of differences
            # among entries; these are all base**j * (base**i - 1)/(base**k - 1)
            # if we do things in the right order; and every difference between
            # entries with adjacent indices yields, where it's used as
            # denominator, a simple k = 1 so these ratios are base**j *
            # (base**(i-1) + ... + base**2 + base + 1)

            score = rat[0]
            while score < cut:
                row, rat = row[1:], rat[1:]
                score = score + rat[0]

            score = rat[-1]
            while score < cut:
                row, rat = row[:-1], rat[:-1]
                score = score + rat[-1]

            if len(row) >= plenty:
                return row

        return [ x.orbit.radius for x in seq ]

    from study.value.SI import metre
    Unit = Quantity(tera * metre / 7) # Arbitrary Unit of length (approximates the AU)
    del tera, metre

    def use(self, zero=None, unit=None, base=None, Q=Quantity):
        if zero is None:
            try: del self.zero
            except AttributeError: pass
        else: self.zero = zero

        if unit is None:
            try: del self.unit
            except AttributeError: pass
        else: self.unit = unit

        if base is None:
            try: del self.base
            except AttributeError: pass
            try: del self.__base
            except AttributeError: pass
        else:
            self.unit # Force lazy evaluation so we can blot out its __base ...
            self.base = Q(base)
            self.__base = self.base.log

    def _lazy_get_unit_(self, ig, mid=median, AU=Unit):
        row = [x - self.zero for x in self.__seq if x > self.zero]
        # That forced computation of zero, making base available ...
        offs = [ ((x / AU).log / self.__base) % 1 for x in row ]
        frac = offs[:]
        frac.sort()
        gaps = [ frac[0] + 1 - frac[-1] ] + [y - x for x, y in zip(frac[:-1], frac[1:])]
        ind = gaps.index(max(gaps)) # frac[ind-1] and frac[ind] differ by max(gaps)
        dim = (frac[ind-1] + frac[ind]) * .5 # the anti-middle

        # Now index row as nicely as we can hope for:
        i, ind = len(offs), []
        while i > 0:
            i = i - 1
            r = offs[i]
            if r > dim: r = r - 1
            n = (row[i] / AU).log / self.__base - r
            ind.insert(0, int(n.best))
        # but that leaves an arbitrary offset in ind.
        offs = [n - i - 1 for i, n in enumerate(ind)]
        # print 'offsets:', offs
        offs.sort()
        n = offs[len(offs) / 3]
        return mid([ r / self.base**(i - n) for r, i in zip(row, ind) ])

    del median

    def _lazy_get_base_(self, ignored):
        self.zero # force evaluation so we compute __base
        return self.__base.exp

    import math
    def __spread(self, z, e=math.exp, AU=Unit):
        # icky complex-to-complex used in hunting good values for zero and base
        zero, b = e(z.imag) * AU, e(z.real)
        # it may be prudent to frob oz ...
        oz = (zero / self.__seq[0] / 10).evaluate(lambda x: x + 1/x)

        row = [x for x in self.__seq if x > zero]
        if b > 0 and len(row) > 1:
            row = [ ((x / AU).log / b) % 1 for x in row ]
            row.sort()
            gap = max([ row[0] + 1 - row[-1] ] +
                      [ y - x for x, y in zip(row[:-1], row[1:]) ])
            span, oz = ((1 - gap) / len(row)).best, (oz / len(row)).best
        else:
            count = len(self.__seq) + 1 - len(row)
            try: span, oz = (e(b) + e(-b)) * count, (oz * count).best
            except OverflowError:
                raise OverflowError(z, b, oz)

        return float(span) + 1j * float(oz)

    from study.maths.search import Search

    def _lazy_get_zero_(self, ignored, S=Search, e=math.exp, AU=Unit, Q=Quantity):
        # Mercury's orbit as zero:
        z = self.__seq[0]
        # Saturn vs. Venus gives median log(ratio)/(difference in index)
        try: b = ((self.__seq[5] - z) / (self.__seq[1] - z)).log * .2
        except IndexError:
            b = Q(2).log # fall back on 2

        guess = float(b.log.best) + 1j * float((z / AU).log.best)
        hunt = S(self.__spread, guess, stride=.1)
        hunt.rummage()

        z = hunt.best[0]
        self.__base = Q(e(z.real))
        return e(z.imag) * AU

    del Quantity, Search, Unit, math

del Lazy
