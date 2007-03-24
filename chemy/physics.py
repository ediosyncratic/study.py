"""Basic physics.

See also:
  http://physics.nist.gov/cuu/Constants/
  http://www.alcyone.com/max/physics/laws/

$Id: physics.py,v 1.2 2007-03-24 14:18:01 eddy Exp $
"""
from value.units import *
def sample(mid, tol, flat=2*tophat): return mid + tol * flat

Quantum = Object(
    Planck = Quantity(sample(662.606876, .000052), 1e-36 * Joule * second / turn,
                   doc = """Angular Planck's constant

Planck's constant is definitively given by the equation E = h.f relating the
frequency, f, of electromagnetic radiation to the amount of energy in parcels
(quanta) of which the electromagnetic field's spectral line at that frequency
interacts with matter.  Properly, f should be recognised as having, as its
units, angle/time, or at least cycles/time.  In particular, the frequency Planck
was refering to was turn/period, one cycle divided by the time taken for the
field to return to its prior state.  Thus, strictly, the constant of
proportionality should be a quantity with units Joule*second/turn.

This then represents Dirac's constant `h-bar' as h*radian.  For the sake of
widely accepted usage which takes h without the turn divisor, I'll reserve the
name h for h*turn and use the name Planck for the `correct' quantity, with the
turn unit in it. """),
    Millikan = Quantity(sample(160.210, .007), zepto * Coulomb,
                        doc = """Millikan's Quantum; size of electron charge"""))

Quantum.also(h = Quantity(Quantum.Planck, turn, doc="""Planck's constant

See Quantum.Planck's __doc__ for details. """),
             hbar = Quantity(Quantum.Planck, radian, doc="""Dirac's constant

See Quantum.Planck's __doc__ for details. """))

mol.charge.observe(mol.Avogadro * Quantum.Millikan)

Vacuum = Object(
    c = second.light / second, # given exactly as an integer, these days.
    permeability = Quantity(.4 * pi, micro * Henry / metre, # definition
                            doc = """Magnetic permeability of free space.

The magnetic force per unit length between two long parallel wires a distance R
apart carrying currents j and J is j*J/R times the magnetic permeability of the
medium between them. """))

Vacuum.also(
    Z0 = Quantity(Vacuum.c, Vacuum.permeability,
                  doc="""Impedance of free space, Z0.

This is the square root of the ratio of the vacuum's magnetic permeability and
electric permittivity, whose product is 1/c/c.  Thus:

    c*c*epsilon0*mu0 = 1
    Z0*Z0 = mu0/epsilon0

whence we may infer:

    Z0 = c*mu0
    c*Z0*epsilon0 = 1

so that c and Z0 suffice to determine the permeability and permittivity of free
space.  Note that, since c has (by the definition of the metre) an exact integer
value and mu0 is (by the definition of the Ampere) exactly pi*4e-7, the values
of Z0 and epsilon0 are also exact.\n"""),
    mu0 = Vacuum.permeability)

# Should mu0, e0, Z0 involve steradian and/or radian ? alpha is begging for it ...

Vacuum.also(
    impedance = Vacuum.Z0,
    permittivity = Quantity(1 / Vacuum.Z0, 1 / Vacuum.c,
                            doc="""Electrical permittivity of free space.

The electrostatic force between two point charges q and Q a distance R apart is
q*Q / (4 * pi * R**2) divided by the permittivity of the medium between the
charges. """),
    alpha = Quantity(Quantum.Millikan**2 / 2, Vacuum.Z0 / Quantum.h,
                     doc="""The fine structure constant.

The fine structure constant arises naturally in the perturbation expansions of
various physical quantities.  It is a dimensionless quantity which expresses the
charge on the electron (Millikan's quantum) in terms of the natural unit of
charge, sqrt(h/Z0), one can obtain (after the style of Planck's units, see
below) from Planck's constant, h, and the impedance of free space, Z0.  The
definitive formula for the fine structure constant is:

    e**2 / (4 * pi * epsilon0 * hbar * c)

or, equivalently (as 2*pi*hbar is h and epsilon0*c*Z0 is 1):

    e**2 * (Z0 / h) / 2

as expressed here.  I am given to believe that the fine-structure splitting of
the spectrum of Hydrogen (and, I am thus inclined to guess, other quantum
electrodynamic systems) is expressed as a power-series in 2*alpha, which the
latter formula gives as the ratio of e**2 and h/Z0.

It is perhaps worth noting that, when Planck's constant is undestood to include
units of angle, alpha actually emerges as an angle, rather than being strictly
dimensionless.  Then again, either G or epsilon0 arguably involves a factor of
solid angle, which would complicate the matter even further ...\n"""))

Vacuum.alpha.observe(Quantity(1/sample(137.03604, .00011)))

# a couple more aliases ...
Vacuum.epsilon0 = Vacuum.permittivity
Quantum.fineStructure = Vacuum.alpha # w/ factor of turn or radian ?

Cosmos = Object(
        G = Quantity(sample(66.720, .04), # .86 cc/g /hour**2
                     (milli * metre)**3 / gram / (kilo * second)**2,
                     """Newton's constant of gravitation.

The gravitational force between two small massive bodies is the product of their
masses divided by the square of the distance between them, multiplied by
Newton's constant, which is normally called G.\n"""),

        Hubble = Quantity(2.3 * (1 + tophat * .1), atto * Hertz,
                          doc = """Hubble's constant.

This describes the rate of expansion of the universe: it is the velocity
difference between widely-separated parts of the universe divided by the
distance between them.  We should probably actually use hyperbolic angles in
place of velocities, though.  A velocity should be expressed as c*tanh(b) and
described in terms of the value of b; the correct relativistic addition of
velocities adds these hyperbolic angles.  Thus Hubble/c gives the rate of change
of the hyperbolic angle b with distance.  The Doppler effect that goes with the
velocity scales frequencies by the factor exp(b).

The Wilkinson Microwave Anisotropy Probe's study of the microwave background
(emitted only 380,000 years after the Big Bang) vastly improved the available
accuracy of this datum; previously the best estimate's uncertainty included its
most significant digit.  The new figure is 71 km / sec / Mpc, accurate to 5%.

See also planets.universe\n"""),

        temperature = Quantity(2.7248 + tophat * .0004, Kelvin,
                               """Cosmic Microwave Background Temperature.

The after-glow of the Big Band shows itself as an almost uniform background glow
in the sky with the spectrum of an ideal black body (one which absorbs light
perfectly, regardless of the light's colour) at a temperature of 2.725 Kelvin.

Such a close match to the ideal black body spectrum says a lot in its own right:
normally, all matter has spectral lines where it is more or less apt to interact
with light.  We know the radiation has been cooled adiabatically by the
universe's expansion since the background radiation fell out of thermal
equilibrium with matter (by ceasing to interact with it sufficiently
intimately), and this means it's been red-shifted from a much higher temperature
black body spectrum when it last interacted with matter.  The matter at that
time must have had no distinct spectral lines, strongly favouring the belief
that it was much hotter than anything we see today - since all matter has
spectral irregularities at all familiar temperatures.

The background radiation is also extremely close to uniform across all
directions in the sky.  The principal variation has the form we would expect
from a Doppler shift, which tells us our velocity relative to the rest-frame of
the background radiation.  Once the effects of this are eliminated, a tiny
variation remains, of order one part in 10000.  For parts of the universe that
are now vastly distant from one another to have temperatures so remarkably close
to equal would itself be surprising, if they hadn't come from a common past when
they were close together.

See http://www.astro.ubc.ca/people/scott/faq_email.html for further details.
"""))

Cosmos.Hubble.also(length = Vacuum.c / Cosmos.Hubble,
                   time = 1 / Cosmos.Hubble)

Cosmos.also(kappa = Quantity(8 * pi, Cosmos.G / Vacuum.c**3,
                             doc="""Einstein's constant of gravitation.

One side of Einstein's field equation for general relativity is constructed out
of the the metric of space-time (describing distances), the Ricci tensor (which
describes space-time's curvature) and the cosmological constant.  The other side
is just the energy-momentum-stress tensor (which describes matter and related
phenomena, such as the electromagnetic field) scaled by Einstein's gravitational
constant, usually called &kappa;, which is 8 * pi times Newton's gravitational
constant divided by a suitable power of the speed of light.  Thus &kappa; has
the dimensions of a length per mass, give or take some factors of velocity
(which is nominally dimensionless, like angles).\n"""),
            qperm = Quantity(1, (4 * pi * Cosmos.G * Vacuum.permittivity)**.5,
                             doc="""The Einstein/Maxwell Charge-to-Mass ratio.

The free-field equations of general relativity with an electromagnetic field can
be put in a form which doesn't involve the usual electromagnetic and
gravitational constants:

   (D/g\\D)a = 0
   (d^a)/g\\(d^a) -g.trace((d^a)/g\\(d^a)/g)/4 = -R +g.(trace(R/g)/4 -L)

where R is the Ricci tensor, L is the cosmological constant and a is the result
of multiplying Maxwell's co-vector potential, usually called A, by the
charge-to-mass ratio, sqrt(4.pi.G/epsilon0), given here.

  The use, here, of /g\\ means the same as contracting via g's inverse; while
  the use of /g means the same as contracting with g's inverse (on the right).
  The D/g\\D operator is the usual `box' (or box-squared) operator equivalent to
  the del-squared operator in three dimensions.  Since (modulo factors of c) A
  has the dimensions of energy per unit charge, this gives a the dimensions of
  energy per unit mass, i.e. the square of a velocity, which is implicitly
  dimensionless (because we're working modulo factors of c).

  Note that R + (d^a)/g\\(d^a) is parallel to (i.e. a scalar multiple of) g; and
  that this says everything the second equation above says - because trace(g/g)
  is 4 (or, rather, the dimension of space-time) and trace commutes with scaling
  - whence, in particular, we can infer that L is zero.

If we add in charges and currents as sources for the electromagnetic field, the
first equation above becomes (D/g\\D)a = g.j with j equal to the conventional
4-vector current times a scalar, roughly 9.731 (m/s)**3 /Amp, obtained by
dividing the charge-to-mass ratio by the permittivity of free space.  The
thus-scaled j is, consequently, the cube of a velocity - i.e. dimensionless.

If we compare Newton's and Coulomb's force laws for gravity and electrostatics,
respectively, this charge-to-mass ratio (about 86.2 nano Coulombs per tonne)
also emerges as a natural way of comparing the strengths of the two fields; two
objects, each of mass one million tonnes and carrying 86.2 milli Coulombs of
charge (almost 0.9 micro-moles of electrons), of equal sign, would repel one
another electrostatically with a force equal to that with which they would
attract one another gravitationally.

This ratio is equally the Planck charge (see planck.Planck) divided by the
Planck mass.  Note the non-involvement of Planck's constant.  Contrast with the
charge-to-mass ratios of the proton (95.79 MC/kg) and electron (-175.9 GC/kg),
roughly 1e19/9 and 2e21 times as big.  Even the Beauty+2.Truth equivalent of the
proton only gets down to around 1 MC/kg, around 1e16 times as high as the
Einstein-Maxwell charge-to-mass ratio.  One electron's charge is worth the mass
of over a million million million nucleons; the mass of a mole of nucleons is
worth only a bit over half a million electrons' charge.\n"""),
            Schwarzschild = Quantity(2, Cosmos.G / Vacuum.c**2,
                                     doc="""Schwarzschild's factor.

The Schwarzschild radius of a black hole of mass m is 2.G.m/c/c; outside
this radius, the radial co-ordinate is spatial and the time-wards co-ordinate
is time-like, as one would expect; inside, they swap.\n"""))

Thermal = Object(
        k = Quantity(sample(13.8054, .0018), yocto * Joule / Kelvin,
                     doc = """Boltzmann constant"""),
        Stefan = Quantity(sample(56.7032, .0125),
                          nano * Watt / (metre * Kelvin**2)**2,
                          doc = """Stefan-Boltzmann constant

The total radiant power output per unit area from a black body at temperature T
is given by sigma T**4 where sigma is the Stefan-Boltzmann constant,
        sigma = pi**2 k**4 / 60 / c**2 / hbar**3

See also: Radiator.total, space.hole.BlackHole, below; notably (in the latter)
the observation that the above law must clearly be modified when k.T exceeds the
rest mass of any type of particle (unless this modification is, for some reason,
specific to black holes).
"""))
Thermal.also(Boltzmann = Thermal.k,
             Hawking = Quantity(1,  Quantum.hbar / Cosmos.kappa / Thermal.k,
                                doc="""The Hawking radiation constant.

A black hole radiates away energy after the manner of a black body of
temperature T given by

        k T = hbar g / 2 / pi / c = hbar c / 4 / pi / r
            = hbar / kappa / M

where g is its 'surface' gravity G.M/r/r = c.c/2/r and r is its Schwarzschild
radius.  The product M.T is thus the constant given here as the Hawking
radiation constant.
"""))

# Mole-related quantities
mol.R = Thermal.Boltzmann * mol.Avogadro
# 8.31434 * Joule / Kelvin / mol
mol.gasConstant = mol.R
