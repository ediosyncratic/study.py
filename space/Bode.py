# -*- coding: iso-8859-1 -*-
"""The auto-Bodalizer.

Trying to invent a Titius-Body sort of a sequence for an arbitrary family of
bodies orbiting a common centre.  Bogus and experimental.

$Id: Bode.py,v 1.1 2005-03-12 14:00:04 eddy Exp $
"""

class Bodalizer (Lazy):
    """The Titius-Bode law, a.k.a. Bode's law:

A mathematical formula which generates, with modest accuracy, the semimajor axes
of the planets in order out from the Sun.  Write down the sequence
        0, 3, 6, 12, 24, ...
and add 4 to each term:
        4, 7, 10, 16, 28, ...
Then divide each term by 10.  This leaves you with the series
        0.4, 0.7, 1.0, 1.6, 2.8, ...
which is intended to give you the semimajor axes of the planets measured in
astronomical units.

Bode's law had no theoretical justification when it was first introduced; it
did, however, agree with the soon-to-be-discovered planet Uranus' orbit (19.2 AU
actual; 19.7 AU predicted).  Similarly, it predicted a missing planet between
Mars and Jupiter, and shortly thereafter the asteroids were found in very
similar orbits (2.77 AU actual for Ceres; 2.8 AU predicted).  The series,
however, seems to skip over Neptune's orbit.  The form of Bode's law (that is, a
roughly geometric series) is not surprising, considering our theories on the
formation of solar systems, but its particular formulation is thought of as
coincidental.

(That's from http://www.alcyone.com/max/physics/laws/b.html; while
http://www.astropa.unipa.it/versione_inglese/Hystory/BODE'S_LAW.htm
gives some history, as follows.)

The law first appeared in 1766 in a translation, by Johann Daniel Titius of
Wittenberg, of Contemplation de la Nature (1764), by the French natural
philosopher Charles Bonnet.  Titius appears to have drawn the law from private
correspondence with others.  Bode repeated it in a foot-note, then went on to
become a professional astronomer, so everyone cited him as source ...

Herschel's discovery (March 1781) of Uranus encouraged various astronomers to
search for a planet in the gap between Mars and Jupiter, where the law predicts
a planet at 2.8 AU; in 1800, a team set up an international collaboration;
starting on the first day of the next year, Piazzi made the first of a series of
observations of what he soon realised was such a missing planet, which he named
Ceres Ferdinandea.\n"""

    def __init__(self, seq, ignored):
        # First identfy your demos ...
        plenty, k = len(seq), 3
        while k * k < plenty: k = 1 + k
        plenty = k
        for k in self.bodytypes:
            row = filter(lambda x, k=k: isinstance(x, k), seq)
            if len(row) > plenty: break
        self.__seq = map(lambda x: x.orbit.radius, row)

    def _lazy_get_bodytypes_(self, ignored, seq=[]):
        if seq: return tuple(seq)
        from body import Planet, Planetoid, Body
        seq[:] = [ Planet, Planetoid, Body ]

    def __len__(self): return len(self.__seq)
    def __getitem__(self, key):
        return self.zero + self.unit * self.base**key

    def __repr__(self):
        return 'lambda i: %s + %s * %s ** i' % (self.zero, self.unit, self.base)

    # Auto-detection of zero, unit and base:

    def median(seq, span=1): # tool func
        seq.sort()
        i, b = divmod(len(seq), 2)
        if b: best = seq[i].best
        else: best = .5 * (seq[i-1].best + seq[i].best)
        i, b = divmod(len(seq) - span, 2)
        if b: lo, hi = seq[i].best, seq[-1-i].best
        else: lo, hi = seq[i].low, seq[-1-i].high
        return Quantity(1, .5 * (lo + hi) + tophat * (hi - lo), best=best)

    import math
    def _lazy_get_unit_(self, ig, ln=math.log, mid=median):
        row = map(lambda x, z=self.zero: x - z, filter(lambda x, z=self.zero: x > z, self.__seq))
        # That forced computation of zero, making base available ...
        offs = map(lambda x, log=ln, b=self.__base, u=AU.best: ((x/u).evaluate(log)/b) % 1, row)
        frac = offs[:]
        frac.sort()
        gaps = [ frac[0] + 1 - frac[-1] ] + map(lambda x, y: y - x, frac[:-1], frac[1:])
        ind = gaps.index(max(gaps)) # frac[ind-1] and frac[ind] differ by max(gaps)
        dim = (frac[ind-1] + frac[ind]) * .5 # the anti-middle

        # Now index row as nicely as we can hope for:
        i, ind = len(offs), []
        while i > 0:
            i = i - 1
            r = offs[i]
            if r > dim: r = r - 1
            n = (row[i] / AU.best).evaluate(ln) / self.__base - r
            ind.insert(0, int(n.best))
        # but that leaves an arbitrary offset in ind.
        offs = map(lambda n, i: n - i - 1, ind, range(len(ind)))
        print 'offsets:', offs
        offs.sort()
        ind = map(lambda i, n=offs[len(offs)/3]: i - n, ind)
        return mid(map(lambda r, i, b=self.base: r / b**i, row, ind))

    del median

    def _lazy_get_base_(self, ignored, e=math.exp):
        self.zero # force evaluation so we compute __base
        return e(self.__base)

    def __spread(self, z, e=math.exp, ln=math.log):
        # icky complex-to-complex used in hunting good values for zero and base
        zero, b = e(z.imag) * AU, e(z.real)
        # it may be prudent to frob oz ...
        oz = (zero / self.__seq[0] / 10).evaluate(lambda x: x + 1/x)

        row = filter(lambda x, z=zero: x > z, self.__seq)
        if b > 0 and len(row) > 1:
            row = map(lambda x, log=ln, b=b, u=AU.best: ((x/u).evaluate(log)/b) % 1, row)
            row.sort()
            gap = max([ row[0] + 1 - row[-1] ] + map(lambda x, y: y - x, row[:-1], row[1:]))
            span, oz = ((1 - gap) / len(row)).best, (oz / len(row)).best
        else:
            count = len(self.__seq) + 1 - len(row)
            try: span, oz = (e(b) + e(-b)) * count, (oz * count).best
            except OverflowError:
                raise OverflowError(z, b, oz)

        return float(span) + 1j * float(oz)

    from search import Search

    def _lazy_get_zero_(self, ignored, S=Search, ln=math.log, e=math.exp):
        # Mercury's orbit as zero:
        z = self.__seq[0]
        # Saturn vs. Venus gives median log(ratio)/(difference in index)
        b = ((self.__seq[5] - z) / (self.__seq[1] - z)).evaluate(ln) * .2

        guess = float(b.evaluate(ln).best) + 1j * float((z / AU).evaluate(ln).best)
        hunt = S(self.__spread, guess, stride=.1)
        hunt.rummage()

        z = hunt.best[0]
        self.__base = e(z.real)
        return e(z.imag) * AU

    del Search

    def index(self, radius, ln=math.log):
        r = (radius - self.zero) / self.unit
        if r.low < .0001: return 0 # avoid domain errors and hugely -ve answers ...
        # (while incidentally being polite to Mercury)
        return r.evaluate(ln) / self.__base

    del math

_rcs_log = """
$Log: Bode.py,v $
Revision 1.1  2005-03-12 14:00:04  eddy
Initial revision

"""
