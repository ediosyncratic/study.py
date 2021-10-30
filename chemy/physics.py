"""Basic constants of physics.

See also:
  http://physics.nist.gov/cuu/Constants/
  http://www.alcyone.com/max/physics/laws/
See study.LICENSE for copyright and license information.
"""
from study.value.units import *

Quantum = Object(
    __doc__ = """Material parameters of the Quantum Universe.

See also study.chemy.particle.electron.magneton for the 'Bohr magneton', along
with assorted other properties of specific particles from the same
sub-module. Some of those are treated as parameters of the universe, when
they're actually specific to particular particles.  Of course, those particles'
properties may well _be_ parameters of the universe, but they're better
characterised as properties of those particles.  I focus, here, on the
properties that are independent of the entities they implicate, although there
are legitimate grounds for debate about which belong here or elsewhere.
""",
    Planck = Quantity.within(662.606876, .000052,
                             1e-36 * Joule * second / turn,
                             """Angular Planck's constant

Planck's constant is definitively given by the equation E = h.f relating the
frequency, f, of electromagnetic radiation to the amount of energy in parcels
(quanta) of which the electromagnetic field's spectral line at that frequency
interacts with matter.  Properly, f should be recognised as having, as its
units, angle/time, or at least cycles/time.  In particular, the frequency
Planck was refering to was turn/period, one cycle divided by the time taken
for the field to return to its prior state.  Thus, strictly, the constant of
proportionality should be a quantity with units Joule*second/turn.

This then represents Dirac's constant `h-bar' as h*radian.  For the sake of
widely accepted usage which takes h without the turn divisor, I'll reserve the
name h for h*turn and use the name Planck for the `correct' quantity, with the
turn unit in it.
"""),
    Millikan = Quantity.within(160.210, .007, zepto * Coulomb,
                               """Millikan's Quantum

This is the magnitude of the electron's (negative) charge, equal to the charge
on the proton and positron.  The beautiful quarks (down, strange and bottom)
have a third of the charge of the electron; the true quarks (up, charm and
top) have two thirds of the charge of the proton; so this quantum isn't
actually the smallest irreducible unit of charge - but it is the smallest
charge possessed by any partlcle we've ever seen in isolation (quarks have
only, thus far, been seen in combinations with whole multiples of the electron
charge; it is suspected that they can't be isolated, at least in
four-dimensional space-time as we know it).  It is also exactly three times
the largest difference in charge between particles known to exist.
"""))

Quantum.also(h = Quantity(turn, Quantum.Planck, """Planck's constant

See Quantum.Planck's __doc__ for details."""),
             hbar = Quantity(radian, Quantum.Planck, """Dirac's constant

See Quantum.Planck's __doc__ for details."""))

Vacuum = Object(
    c = Quantity(1, second.light / second,
                 """The speed of light in vacuum.

The modern definition of the metre makes this an exact integer, when measured
in metres/second.  There are important senses, in relativity theory, in which
the speed of light is really just a conversion factor between two different
units we use, for historical and practical reasons, to measure space-like and
time-like displacements in space-time.  There being no fundamental difference
between these (they can be added, for example, yielding either), there is a
strong case for doing theoretical work in units which make c take the value 1
so that we can treat it as dimensionless.
"""),
    # TODO: units of angle ! 0.2 micro Henry / metre / radian ?
    # I have two names for each affected unit, so can make one orthodox and
    # the other sensible ...
    permeability = Quantity(.4 * pi, micro * Henry / metre, # definition
                            """Magnetic permeability of free space.

The magnetic force per unit length between two long parallel wires a distance
R apart carrying currents j and J is j*J/R/2/pi times the magnetic permeability of
the medium between them.  The definition of the Ampere fixes the value for
the permeability of free space.
"""))

Vacuum.also(
    # TODO: units of angle ?
    Z0 = Quantity(Vacuum.c, Vacuum.permeability,
                  """Impedance of free space, Z0.

This is the square root of the ratio of the vacuum's magnetic permeability and
electric permittivity, whose product is 1/c/c.  Thus:

    c*c*epsilon0*mu0 = 1
    Z0*Z0 = mu0/epsilon0

whence we may infer:

    Z0 = c*mu0
    c*Z0*epsilon0 = 1

so that c and Z0 suffice to determine the permeability and permittivity of
free space.  Note that, since c has (by the definition of the metre) an exact
integer value and mu0 is (by the definition of the Ampere) exactly pi*4e-7,
the values of Z0 and epsilon0 are also exact.
"""),
    mu0 = Vacuum.permeability)

Vacuum.also(
    impedance = Vacuum.Z0,
    # TODO: units of angle ?
    permittivity = Quantity(1 / Vacuum.c, 1 / Vacuum.Z0,
                            """Electrical permittivity of free space.

The electrostatic force between two point charges q and Q a distance R apart
is q*Q / (4 * pi * R**2) divided by the permittivity of the medium between the
charges.
"""),
    fineStructureAngle = Quantity(Quantum.Millikan**2, Vacuum.Z0 / Quantum.Planck,
                                  """The fine structure angle.

This is an angle that naturally emerges from considering the fine structure
constant, alpha; see Vacuum.alpha for details.  When we include units of angle
in Planck's constant (see Quantum.Planck) we get an angle divided by the square
of a charge; multiplying that by the square of the charge on the electron, we
get an angle.  This doesn't have any obvious physical meaning, but arises as a
natural simple combination of other physical constants, equal to two turns times
the fine structure constant.

An alternative approach to this would be to infer that Z0/turn is what we should
really be using in place of Z0, making it an impedance / angle.  That, in turn,
tells us to use mu0 / turn (1/5 micro Henry / metre / radian) in place of mu0
and epsilon0 * turn (about 55.632503 pico Farad Radian / metre) in place of
epsilon0.  The definition of mu0 in terms of force = j*J*mu0/R/2/pi, does
encourage mu0/turn as a unit, making this force = j*J*mu0*radian/R, although
dividing radian by a radius feels somewhat unnatural.  The case of e0, which
commonly teams up with factors of 4*pi, rather than 2*pi, could be construed as
a hint that it'd sooner implicate the steradian, suggesting e0*radian**2 would
be a more natural quantity, rather than e0*turn.

All of which hints that a systematic investigation of the proper description of
physics without ignoring units of angle (i.e. treating radian as a dimensionless
unit) has much to reveal.
"""))

Vacuum.alpha = Quantity(0.5, Vacuum.fineStructureAngle / turn,
                        """The fine structure constant.

The fine structure constant arises naturally in the perturbation expansions of
various physical quantities.  It is a dimensionless quantity which expresses
the charge on the electron (Millikan's quantum) in terms of the natural unit
of charge, sqrt(h/Z0), one can obtain (after the style of Planck's units, see
chemy.planck.Planck) from Planck's constant, h, and the impedance of free
space, Z0.  The definitive formula for the fine structure constant is:

    e**2 / (4 * pi * epsilon0 * hbar * c)

or, equivalently (as 2*pi*hbar is h and epsilon0*c*Z0 is 1):

    e**2 * (Z0 / h) / 2

as expressed here.  I am given to believe that the fine-structure splitting of
the spectrum of Hydrogen (and, I am thus inclined to guess, other quantum
electrodynamic systems) is expressed as a power-series in 2*alpha, which the
latter formula gives as the ratio of e**2 and h/Z0.

See Vacuum.fineStructureAngle for the associated angle that arises when that
factor of two and the factor of turn hiding in h are not divided out.
""")
Vacuum.alpha.observe(1 / Quantity.within(137.03604, .00011))

# a couple more aliases ...
Vacuum.epsilon0 = Vacuum.permittivity
Quantum.fineStructure = Vacuum.alpha

# TODO: move G, kappa, qperm, Schwarzschild from Cosmos to Vacuum;
# unify Comsmos with space.home.Universe

Cosmos = Object(__doc__ = """Properties of the universe.

See also space.home.Universe
""",

                G = Quantity.within(
        66.720, .04, # .86 cc/g /hour**2
        (milli * metre)**3 / gram / (kilo * second)**2,
        """Newton's constant of gravitation.

The gravitational force between two small massive bodies is the product of
their masses divided by the square of the distance between them, multiplied by
Newton's constant, which is normally called G.
"""),

                Hubble = Quantity.within(
        70.1, 1.3, 32.4 * zepto * Hertz, # 2.27 aHz
        # km/s/mega/parsec = 32.40 zepto / second
        # NASA (Wikipedia): 70.8 +/- 4, (km/s)/Mpc
        # Britannica: 22.45 +/- .95, mm / second / year.light,
        """Hubble's constant.

This describes the rate of expansion of the universe: it is the velocity
difference between widely-separated parts of the universe divided by the
distance between them.  We should probably actually use hyperbolic angles in
place of velocities, though.  A velocity should be expressed as c*tanh(b) and
described in terms of the value of b; the correct relativistic addition of
velocities adds these hyperbolic angles.  Thus Hubble/c gives the rate of
change of the hyperbolic angle b with distance.  The Doppler effect that goes
with the velocity scales frequencies by the factor exp(b).

The Wilkinson Microwave Anisotropy Probe's study of the microwave background
(emitted only 380,000 years after the Big Bang) vastly improved the available
accuracy of this datum; previously the best estimate's uncertainty included
its most significant digit.  (This is, indeed, why I initially chose to have
Quantity and Sample display the first uncertain digit in a number's
representation, as well as the confidently known digits; otherwise, there was
nothing to display here !)  The new figure is 71 km / sec / Mpc, accurate to
5%.

Notice that, as things move away from us due to this, the rate at
which they move away from us increases; dr/dt = H.r implies ddr/dt/dt
= H.dr/dt = H.H.r, so the distant universe is also accellerating away
from us at a rate proportional to distance from us, the constant of
proportionality being the (vanishingly small) square of Hubble's
(tiny) constant.

See also: Cosmos.Hubble's attributes length and time.
"""),

                temperature = Quantity.within(
        2.7248, .0002, Kelvin,
        """Cosmic Microwave Background Temperature.

The after-glow of the Big Bang shows itself as an almost uniform background
glow in the sky with the spectrum of an ideal black body (one which absorbs
light perfectly, regardless of the light's colour) at a temperature of 2.725
Kelvin.

Such a close match to the ideal black body spectrum says a lot in its own
right: normally, all matter has spectral lines where it is more or less apt to
interact with light.  We know the radiation has been cooled adiabatically by
the universe's expansion since the background radiation fell out of thermal
equilibrium with matter (by ceasing to interact with it sufficiently
intimately), and this means it's been red-shifted from a much higher
temperature black body spectrum when it last interacted with matter.  The
matter at that time must have had no distinct spectral lines, strongly
favouring the belief that it was much hotter than anything we see today -
since all matter has spectral irregularities at all familiar temperatures.

The background radiation is also extremely close to uniform across all
directions in the sky.  The principal variation has the form we would expect
from a Doppler shift, which tells us our velocity relative to the rest-frame
of the background radiation.  Once the effects of this are eliminated, a tiny
variation remains, of order one part in 10000.  For parts of the universe that
are now vastly distant from one another to have temperatures so remarkably
close to equal would itself be surprising, if they hadn't come from a common
past when they were close together.

See http://www.astro.ubc.ca/people/scott/faq_email.html for further details.

Note (see Thermal.Unruh) that an accellerating observer sees black body
radiation due to the acceleration; and we *are* accellerating away from all of
the universe (in all directions ! - see Cosmos.Hubble); however, the
acceleration that would be needed to produce even the low temperature we
observe as the cosmic microwave background is 68.52e18 times Earth's surface
gravity; and the distance at which the universe is accellerating away from us
(and we accellerate away from it) at this rate is 13.8e39 light years,
significantly further than the size of the observable universe and so far away
(see Cosmos.Hubble.length, which is about 14 giga light-year) as to be
causally decoupled from us; so this is cannot account for the observed
background (quite apart from Unruh's effect depending on actual accelleration,
rather than distances merely growing, regardless of the local motions of
things).

Black holes also produce black-body radiation (see study.space.hole's
BlackHole); a black hole with the temperature of the cosmic microwave
background would have a radius of 66.8 microns and a mass that's 0.613 times
that of the Moon.
"""))

Cosmos.Hubble.also(length = Quantity(1, Vacuum.c / Cosmos.Hubble,
                                     """The Hubble length

This is the distance, about 14 giga light years, at which light
travelling towards us doesn't get closer to us; the space between us
and it is expanding as fast as it's moving.  Things this far away, or
further off, cannot influence us and we cannot influence them; so this
is the radius of the portion of the universe causally connected to us;
in an important sense, this is the radius of the universe.  Things
beyond are irrelevant to us.  Since things nearly this far away are,
of course, moving away from us (at almost the speed of light), these
things soon become irrelevant likewise; there is a steady flow of
universe out of our causally-connected domain.  The amount and engergy
of matter causally linked to us is thus always diminishing.

Furthermore, as the expansion of the universe is accellerating, this
distance ia always decreasing, which further speeds the rate at which
the amount of stuff causally linked to us diminishes.

The distance r between a source and the light it once emitted, taking
into account the expansion of the space it has travelled through,
grows with time t since emission as dr/dt = H.r +c, implying d(r
+c/H)/dt = H.(r +c/H) and r +c/H = exp(H.t).c/H, where c/H is the
Hubble length; for small t, r is well-approximated by c.t, but it
grows faster for larger t.  Naturally, this derivation shifts if the
rate of expansion varies, as is believed to be the case; instead, you
get log(r.H/c +1) as the integral of H over the period of time the
light has been travelling.
"""),
                   # volume = length **3, optionally times 4*pi/3
                   time = Quantity(1, 1 / Cosmos.Hubble,
                                   """The Hubble time.

This is just the Hubble length expressed as a time; it's the time it
would take light to travel that distance, if space weren't expanding.
As space *is* expanding, light actually takes less time than this to
get far enough away from where it started to become causally decoupled
from the part of the universe that emitted it.  Light's actual
distance from a source that emitted it a time t ago grows as (exp(H.t)
-1).c/H; so light reaches distance c/H in time ln(2)/H; this is only
9.7 G yr, rather than the 14ish G yr light would take to cover that
distance without the expansion of the space it's passed through.
"""))

# Smollin's quantum dual of cosmological time is (give or take factors of c)
# kappa * hbar times the cosmological constant.  The kappa * hbar in that is
# an area; the Planck area times some factors of two and pi.
Cosmos.also(kappa = Quantity(8 * pi, Cosmos.G / Vacuum.c**3,
                             """Einstein's constant of gravitation.

One side of Einstein's field equation for general relativity is constructed
out of the the metric of space-time (describing distances), the Ricci tensor
(which describes space-time's curvature) and the cosmological constant.  The
other side is just the energy-momentum-stress tensor (which describes matter
and related phenomena, such as the electromagnetic field) scaled by Einstein's
gravitational constant, usually called &kappa;, which is 8 * pi times Newton's
gravitational constant divided by a suitable power of the speed of
light.  Thus &kappa; has the dimensions of a length per mass, give or take
some factors of velocity (which is nominally dimensionless - like angles, only
more convincingly so).
"""),
            qperm = Quantity(1, (4 * pi * Cosmos.G * Vacuum.permittivity)**.5,
                             """The Einstein/Maxwell Charge-to-Mass ratio.

The free-field equations of general relativity with an electromagnetic field
can be put in a form which doesn't involve the usual electromagnetic and
gravitational constants:

   d^f = 0, d^((sqrt(-det(g)))(g\\f/g)) = 0
   f/g\\f -g.trace(f/g\\f/g)/4 = R -g.(trace(R/g)/2 -L)

where R is the Ricci tensor, L is the cosmological constant and f is the
result of multiplying the relativistic electromagnetic tensor (usually called
F, which encodes E and B.c), by the charge-to-mass ratio,
sqrt(4.pi.G/epsilon0), given here, then dividing by the square of the speed of
light.

  The use, here, of /g\\ means the same as contracting via g's inverse; while
  the use of /g means the same as contracting with g's inverse (on the
  right). The d^ operator is the natural antisymmetric derivative operator on
  alternating forms on the tangents of a smooth manifold.

  Since F/g has the dimensions of force per unit charge, this gives f/g the
  dimensions of force per unit mass over squared speed, i.e. inverse
  length. The cosmological constant has dimensions of an inverse area.

  Note that R -f/g\\f is parallel to (i.e. a scalar multiple of) g; and that
  the only thing the last equation above adds to this - given that trace(g/g)
  is the dimension of space-time, dim, and trace commutes with scaling - is
  that trace(R/g) = L/(1/2 -1/dim) is constant; for dim = 4, this is 4.L.

If we add in charges and currents as sources for the electromagnetic field,
the second equation above becomes d^(mu(g\\f/g)) = mu(j), with mu =
sqrt(-det(g)) and j equal to the conventional 4-vector current density times a
scalar, roughly 0.3612e-24 / Amp, obtained by dividing the charge-to-mass
ratio by the permittivity of free space and then by the cube of the speed of
light.  The thus-scaled j is, consequently, the inverse of an area.

If we compare Newton's and Coulomb's force laws for gravity and
electrostatics, respectively, this charge-to-mass ratio (about 86.16 nano
Coulombs per tonne) also emerges as a natural way of comparing the strengths
of the two fields; two objects, each of mass one million tonnes and carrying
86.2 milli Coulombs of uniformly distributed charge (almost 0.9 micro-moles of
electrons), of equal sign, would repel one another electrostatically with a
force equal to that with which they would attract one another gravitationally
(regardless of the distance between them).

This ratio is equally the Planck charge (see planck.Planck) divided by the
Planck mass; but note the non-involvement of Planck's constant.  Contrast with
the charge-to-mass ratios of the proton (95.79 MC/kg) and electron (-175.9
GC/kg), roughly 1e19/9 and 2e21 times as big.  Even the Beauty+2.Truth
equivalent of the proton only gets down to around 1 MC/kg, around 1e16 times
as high as the Einstein-Maxwell charge-to-mass ratio.  One electron's charge
is worth the mass of over a million million million nucleons; the mass of a
mole of nucleons (or, indeed, of Hydrogen) is worth only a bit over half a
million electrons' charge.
"""),
            Schwarzschild = Quantity(2, Cosmos.G / Vacuum.c**2,
                                     """Schwarzschild's factor.

The Schwarzschild radius of a black hole of mass m is 2.G.m/c/c; outside this
radius, the radial co-ordinate is spatial and the time-wards co-ordinate is
time-like, as one would expect; inside, they swap.
"""))

Thermal = Object(
        k = Quantity.within(13.8054, .0018, yocto * Joule / Kelvin,
                            """Boltzmann constant.

This is the constant of proportionality in the ideal gas law in its molecular
form: if a gas of N particles fills a volume V at pressure P and temperature
T, then P.V/N/T is equal to this constant, k.  It is effectively a conversion
factor between temperature and typical energy per particle.
"""),
        Stefan = Quantity.within(56.7032, .0125,
                                 nano * Watt / (metre * Kelvin**2)**2,
                                 """Stefan-Boltzmann constant

The total radiant power output per unit area from a black body at temperature
T is given by sigma T**4 where sigma is the Stefan-Boltzmann constant,
        sigma = pi**2 k**4 / 60 / c**2 / hbar**3

See also: Radiator.total (in study.chemy.thermal) and BlackHole.luminosity (in
study.space.hole); notably (in the latter) the observation that the above law
must clearly be modified when k.T exceeds the rest mass of any type of
particle (unless this modification is, for some reason, specific to black
holes).
"""))
Thermal.also(Boltzmann = Thermal.k,
             Hawking = Quantity(radian,
                                Quantum.Planck / Cosmos.kappa / Thermal.k,
                                """The Hawking radiation constant.

A black hole radiates away energy after the manner of a black body of
temperature T given by

      k * T = hbar * g / 2 / pi / c = hbar * c / 4 / pi / r
            = hbar / kappa / M

where g is its 'surface' gravity G.M/r/r = c.c/2/r and r is its Schwarzschild
radius.  The product M.T is thus the constant given here as the Hawking
radiation constant.  Compare the Unruh constant, Thermal.Unruh, which is the
constant of proportionality between T and g in the above.
"""),
             Unruh = Quantity(2 * pi / radian,
                              Vacuum.c * Thermal.k / Quantum.Planck,
                              """The Unruh radiation constant.

An accellerating observer, with accelleration a, observes black-body radiation
corresponding to a temperature T, related by the equation

      k * T = hbar * a / 2 / pi / c

This is exactly analogous to the radiation from a black body whose surface
gravity is a - see Thermal.Hawking.  The constant given here is

      a / T = 2 * pi * c * k / hbar.
"""))

Volt.electron = Quantity(Quantum.Millikan, Volt, """The Electron Volt.

This is a standard unit of energy used in atomic and sub-atomic physics; it is
the amount of energy that an electron gains when it moves from some location
to one with an electrostatic potential one Volt more positive.
""")

mol.charge.observe(mol.Avogadro * Quantum.Millikan)

# Mole-related quantities
mol.R = Quantity(mol.Avogadro, Thermal.Boltzmann,
                 """The gas constant.

When the ideal gas law, P.V = N.R.T, is stated with N as the number of moles
of gas present, the constant R is this gas constant.  When the number of
molecules present is used as N, R is replaced by Boltzmann's constant; see
Thermal.k in study.chemy.physics.
""")
# 8.31434 * Joule / Kelvin / mol
