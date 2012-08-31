"""Description of decay processes.

Presently conceived as a specification of: if you put these things together,
with at least this much energy available, then you can get any one of the
following mixtures out, with various probabilities each.

Arguably should be described as: this reaction is characterised by a key
comprised of a bunch of quantum numbers, one of which is an activation energy;
here are several combinations of particles, each with an associated indication
of how far short of that activation energy it falls; each of which matches the
given quantum numbers; any of which can, given its activation energy defecit,
turn into any of the others, releasing however much energy their defecits differ
by.  Probably needs to be capable of coping with being told a temperature from
which to infer a distribution of available energy.

This last is really a description of a reaction, from which one should be able
to infer the equilibium mixture at any given temperature.  Note that actual
reaction rates do depend on how many particles are involved: if you really rely
on three things colliding, your rate is going to be *much* lower than a reaction
in which some two of those things can collide, forming an intermediate with a
modest half-life, during which this intermediate can run into the third party.

Description of reactions/decays is thus quite a subtle problem.  Which is my
excuse for the present implementation being over-simple ...

TODO: add (and integrate this with) a class to model reactions generally; need
to take account of how ingredients are mixed (depends on pressure, temperature,
via frequencies of relevant collisions with various amounts of energy; c.f. the
nucleosynthesis data in elements.py); should be able to infer rates from
activation energies and mixing data.

See study.LICENSE for copyright and license information.
"""
import math # del it later
_ln2 = math.log(2)

def ratedDecay(source, halflife, *procs):
    """As for a Decay, but does some pre-processing for you.

    Experiment yields the half-life of a thing that decays and the various modes
    of decay, each with its proportion of the whole.  This function
    pre-processes that data to produce the data Decay needs.

    First argument, source, is the thing that decays, a Particle.  Second is the
    half-life of this particle species.  Each subsequent argument is a sequence
    describing a single decay process: its first item is the proportion of
    decays that are of the type it describes; its second is an energy or None;
    all subsequent items are Particle()s produced by the decay.  The energy, if
    given, is the kinetic energy released by the decay: any photons produced
    should be described separately as particles.  The proportions merely need to
    be in the right ratio to one another: each will be divided by their total.

    Theory:

      A decay process with rate r is described by the probability distribution
        p(t) = r.exp(-r.t)
      for the time, t, at which the decay occurs.  The probability of decay
      before some specified time T is the integral of this from t=0 to t=T,
      namely 1-exp(-r.T), so the half-life is just log(2)/r.

      However, for several processes happening in parallel, we have a mapping
      ({rates}: r :{processes}) giving the several rates for the processes.
      Then the probability that decay happens after time T is exp(-r(i).T) for
      each process, i; assuming independence, the several processes then give
      product(: exp(-r(i).T) &larr;i :{processes}) which is just exp(-sum(r).T);
      the collective decay rate is the sum of the individual decay rates.  Thus
      we can infer sum(r) = log(2)/halflife.

      Equally, the proportion of decays that follow each of the processes is
      proportional to its rate.  Thus the rate for each process is just sum(r)
      times the proportion of decays that follow that process.\n"""

    scale = sum(map(lambda x: x[0], procs)) * halflife / _ln2
    procs = tuple(map(lambda p, s=scale: (p[0] / s,) + tuple(p[1:]), procs))
    return Decay(source, *procs)

from study.snake.lazy import Lazy
from study.value.units import second

class Decay (Lazy):
    """Describes all the decays of some species of particle."""

    def __init__(self, source, *procs):
        """Initialises a Decay description.

        First argument is what decays; this must be either a Particle() or a
        sequence whose members are either Particle()s or Quantity()s with units
        of energy; this first argument shall subsequently be accessible as
        self.source.

        Each subsequent argument (if any) is a sequence whose:

            * first member is a decay rate (the probability per unit time of the
              thing which decays doing this decay),

            * second member is the energy released (as kinetic energy - any
              photon energy should be included among the particles produced), or
              None (in which case the energy defecit between other yields and
              what decayed will be computed - no matter how bogusly), and

            * all subsequent members are decay products, each of which must be a
              Particle(), of the decay mode which, at the given rate, releases
              the given energy.

        Each such sequence is used to construct an object holding the decay rate
        as its .rate, the (possibly computed) energy as its .energy and the
        decay products as its .fragments, a tuple.  These objects (if any) will
        be accessible as the members of self.processes, a tuple.

        The overall rate of decay of self.source is provided by self.rate; it is
        simply the sum of the .rate attributes over self.processes.\n"""

        self.source = source
        row, rate = [], 0 / second
        for proc in procs:
            rate = rate + proc[0] # raising TypeError if not number/second
            row.append(self.__Decay(source, *proc))
        self.processes, self.rate = tuple(row), rate

    class __Decay (Lazy):
        def __init__(self, source, energy, *bits):
            self.source, self.__bits = source, bits
            if energy is not None: self.energy = energy

        def _lazy_get_energy_(self, ignored):
            return self.source.energy - sum(map(lambda x: x.energy, self.__bits))

        def _lazy_get_fragments_(self, ignored):
            """Mapping from particles emitted to likely kinetic energy.

            Assumes kinetic energy is distributed in proportion to rest mass. """

            bok, scale = {}, self.energy / sum(map(lambda x: x.mass, self.__bits))
            for bit in self.__bits:
                bok[bit] = scale * bit.mass
            return bok

    def _lazy_get_halflife_(self, ignored, ln2=_ln2):
        return ln2 / self.rate

    def before(self, when, exp=math.exp):
        return 1 - exp(self.rate * when)

del math, Lazy
