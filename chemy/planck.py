# -*- coding: iso-8859-1 -*-
"""Planck's units, and kindred systems of units.

See study.LICENSE for copyright and license information.
"""
from physics import Vacuum, Quantum, Cosmos, Thermal, Object, pi

# Should really define a more general `system of units' class ... see dev/py/'s
# eddy.science.unit
class Planckoid (Object):
    """A system of units, having the form of Planck's units.

    The constructor's defaults yield my preferred system of this kind; the
    Planck instance of this class over-rides these (as explained in its own
    documentation, q.v.) to be faithful to what Plank actually used.  The unit
    of speed is (unless over-ridden by key-word argument) the speed of light;
    the eot (energy over temperature) attribute used to obtain temperature
    from energy is (again, unless over-ridden) Boltzmann's constant.

    The constructor's parameters, given in the following order or via
    key-words, are used to initialize eponymous attributes, with the indicated
    defaults:

      geoid -- Newton's constant times 8.pi (G-oid)
      action -- Planck's constant (unit of action)
      impedance -- the impedance of free space

    Lazy attributes:
      tom -- time over mass = geoid / speed**3
      arearate -- area / time
      voltime -- 4-volume, volume*time
      magneton -- arearate * charge = charge * action / mass

    plus charge, momentum, length, area, volume, mass, energy, torque, time,
    current, force, temperature, acceleration - each with its conventional
    meaning.\n"""

    __upinit = Object.__init__
    def __init__(self, geoid=Cosmos.G * 8 * pi, action=Quantum.h,
                 impedance=Vacuum.impedance, *args, **what):
        what.update(impedance=impedance, # electromagnetic field
                    action=action, # quantum of action
                    geoid=geoid) # Newton's constant, suitably scaled #'
        self.__upinit(*args, **what)

    speed = Vacuum.c    # fabric of space-time
    eot = Thermal.k     # energy over temperature

    def _lazy_get_tom_(self, ig):       return self.geoid / self.speed**3
    def _lazy_get_charge_(self, ig):    return (self.action / self.impedance)**.5
    def _lazy_get_momentum_(self, ig):  return (self.action / self.tom)**.5
    def _lazy_get_length_(self, ig):    return (self.action * self.tom)**.5
    def _lazy_get_mass_(self, ig):      return self.momentum / self.speed
    def _lazy_get_energy_(self, ig):    return self.momentum * self.speed
    _lazy_get_torque_ = _lazy_get_energy_
    def _lazy_get_time_(self, ig):      return self.length / self.speed
    def _lazy_get_area_(self, ig):      return self.length**2
    def _lazy_get_arearate_(self, ig):  return self.length * self.speed
    def _lazy_get_volume_(self, ig):    return self.length**3
    def _lazy_get_voltime_(self, ig):   return self.length**3 * self.time
    def _lazy_get_current_(self, ig):   return self.charge / self.time
    def _lazy_get_magneton_(self, ig):  return self.charge * self.arearate
    def _lazy_get_force_(self, ig):     return self.momentum / self.time
    def _lazy_get_temperature_(self, ig): return self.energy / self.eot
    def _lazy_get_acceleration_(self, ig): return self.speed / self.time

# Planck's units (c.f. Hartree's in /usr/share/misc/units.dat):
Planck = Planckoid(Cosmos.G, Quantum.h, Vacuum.impedance / 4 / pi,
                   __doc__ = """Planck's units.

When Planck discovered his solution to the problem then known as the
`ultraviolet catastrophe' and hence discovered the physical constant named
after him, he noticed that it, Newton's gravitational constant and the speed
of light were all expressible in terms of units of length, time and mass; yet
that no product of powers of two of them yields a quantity with the same
dimensions as the third.  He thus had three dimensionally independent
quantities expressible in terms of the three primitive units of measurement of
classical mechanics: which necessarily implied there must be a way to invert
the `expressed in terms of' and obtain units of length, time and mass from h,
G and c.  This object represents the resulting system of units, in which h,
G and c are units of their respective kinds of quantity, from which units of
various other kinds of quantity are derived.

== Charge ==

Some have augmented this system of units with the charge on the electron (e,
a.k.a. Millikan's quantum) to produce units of electrodynamic measurement
(charge, current, voltage, etc.); however, G and c arise as constants in the
field equations of gravity and electrodynamics, while h (at least first)
appeared as a constant in a proportionality law (albeit one whose causes
mystified Planck); it would thus be clearly preferable to extend the system to
electromagnetic units by chosing some constant from electromagnetism's *field
equations*, rather than a property of some of the particles *governed by*
those equations.

It so happens that natural candidates present themselves; the permeability,
permittivity and impedance of free space.  Which of these we chose makes no
difference to our system of units, since combining it suitably with c yields
the other two.  Equally, some constant scaling applied to one of these may be
appropriate; indeed, as argued below, 4*pi times the permittivity provides a
natural candidate (c.f. also Cosmos.qperm).  Planck used G, the constant of
proportionality in the force law between two bodies from gravitation: the
analogous constant from electrostatics is 1/4/pi/epsilon0, so that's what I've
used here (albeit via Z0/4/pi).

== Reducibility ==

Much has been said to the effect that the Planck time and length `must
represent' minimal scales of the universe; I treat this with some skepticism,
given that the Planck mass, of order a dozen microgrammes, isn't in any sense
`irreducible' - see Planck.mass.__doc__ for details.  The Planck momentum is
of the same order as the momentum of a full-grown cat running.  The Planck
charge, meanwhile, is of order eight positrons-worth (give or take some
factors of two, pi and their square roots; see `Other choices' below) - so
manifestly reducible - and the universe clearly *does* quantise charge (albeit
possibly in units of a third that on the positron).  Here we have a definite
case where we do see a quantity with a `minimal scale'; while it *is* of
similar order to that of the Planck unit, it *isn't* the Planck unit per se.

== Other choices ==

It should also be noted that one has some choice in the quantities used; hence
the `of order' clauses above.  The speed of light is well established, but:
for the action one may use Planck's constant, h, or Dirac's, h/2/pi, or even
argue for half this (the spin of a fermion); for the constants governing an
inverse square law, one has a choice of a factor of 4*pi according as one uses
the constant from the force law between two compact isolated bodies or the
constant from the equation of proportionality between the associated field's
divergence and source density (the field equation).  Indeed, as concerns the
latter, G is selected from gravity's inverse square law, so a petty
consistency persuaded me to select the matching constant from Coulomb's law,
1/4/pi/epsilon0 = Z0 * c / 4 / pi (see above under `Charge').

The fine structure constant, alpha = e*e/(4*pi*epsilon0*hbar*c), arises
naturally as a coefficient in perturbation expansions - well, actually,
2*alpha does - and effectively expresses e in terms of the charge q =
sqrt(h/Z0) = sqrt(2*pi*epsilon0*hbar*c), making e = q * sqrt(2*alpha),
i.e. (given 137*alpha is almost exactly 1) q = e*sqrt(137/2): this would make
a more natural unit of charge, but the above petty consistency obliges me to
use 2*q*sqrt(pi) instead (on this object).

== Petty consistency ==

Since I believe Planck used h, c and G as units, and this object is named
after him, I should be faithful to that choice, as far as it goes: which, with
sqrt(4*pi*h/Z0) as unit of charge, requires that Z0 be 4*pi, hence so is mu0,
while epsilon0 is 1/4/pi.  But for Planck's historical choice, I would prefer
to use the Einstein-Maxwell charge-to-mass ratio (Cosmos.qperm - see above) in
place of G, along with Z0 as just justified (in terms of q = sqrt(h/Z0) as
unit of charge); which would be equivalent to replacing G, in the system
actually used here, with its matching unit from Einstein's field-equation,
kappa = 8*pi*G, and using Z0 rather than Z0/4/pi.  However, Planck chose G
and, to the best of my knowledge, stuck to quantities derived from length,
mass and time; the most faithful way to extend his system is by making a
reasonable choice for what he didn't include.  The resulting system yields a
charge equal to 29.3446 times that on a positron, a momentum of 8.451 stone
foot / second, and very tiny length and time - a mole of the lengths add up to
almost a quarter of an Ångstrøm.

For a kindred system of units with my preferred choice of constants (kappa, h,
Z0 and c), instantiate this module's class Planckoid() with no arguments; or
roll your own - see Planckoid.__doc__ for details.

== Verlinde ==

Divide the Planck area by 2*pi to get the area per bit assumed by Verlinde [0]
when deriving gravity as an entropic force [1].  He introduces G (which shall
turn out to be Newton's gravitational constant) as a constant in the law of
proportionality between the number N of bits of information needed to describe
a system and the area A of the `holographic screen' enclosing the system.  The
area per bit is a constant: dividing that constant by hbar and multiplying by
c**3 gives another constant, which he choses to name G, so that N =
A*c**3/G/hbar.

Likewise, the hbar in that is really just another arbitrary constant of
proportionality between the rate, dS/dx, of entropy change as an object
approaches the holographic boundary, and the mass, m, of the object: dS/dx =
2*pi*k*c*m/hbar (in which I suspect c*m shold be the object's momentum, with
dS/dx then becoming a vector derivative).  Thus A*c**3/G/N = hbar =
2*pi*k*c*m/(dS/dx) and dS/dx = 2*pi*k*m*G*N/A/c/c.

All of Verlinde's constants are some factors of two pi off the Planck units
here but are clearly motivated by Planck-like choices of system of units;

[0] http://arxiv.org/abs/1001.0785
[1] http://en.wikipedia.org/wiki/Entropic_gravity
""")

Planck.mass.also(__doc__="""The Planck mass.

See general documentation of Planck's units, on object Planck.

Note, for contrast, that amoeba have masses of order 4 micro grams, bacteria
of order a pico gram, individual genes of order 40 atto grams; the Planck mass
is over a dozen times the first of these.  Tardigrades (a.k.a. water bears)
are fully articulated animals with less mass than the Planck mass.  Thus,
plenty of living organisms are smaller than the Planck mass; a cube of water
with this mass has sides over a third of a millimetre long.\n""")

del Vacuum, Quantum, Cosmos, Thermal, Object, pi
