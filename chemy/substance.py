# -*- coding: iso-8859-1 -*-
"""Messing around with chemistry.

Ethyl alcohol boils at 78.4°C (173°F), so it would be all right for room
thermometers, and has been widely used for that purpose for many years.  The
alcohol is colored red (usually) so it can be seen easily.  Amyl alcohol
(1-pentanol) melts at -78.9°C and boils at 138.1°C, so it can be used to replace
mercury in laboratory thermometers that must read to 110°C.  Its coefficient of
cubical expansion is 0.902e-3 / K, so beta' = 0.874e-3.

$Id: substance.py,v 1.10 2007-05-17 14:49:11 eddy Exp $
"""
from element import * # q.v.
from particle import Nucleon
from study.value.quantity import Quantity, Object, tophat, micro
from study.value.units import second, metre, gram, kilogramme, litre, \
     Joule, Kelvin, Centigrade, Atmosphere, mach
from study.value.archaea import gallon, pound

# Properties of some substances:
class Gas (Substance):
    def _lazy_get__amupokt_(self, ignored, amuk=Nucleon.amuk): # AMU*P/k/T
        try: T = self.temperature
        except AttributeError: T = Centigrade(0)
        try: P = self.pressure
        except AttributeError: P = Atmosphere
        return amuk * P / T
        
    def _lazy_get_density_(self, ignored):
        return self.RMM * self._amupokt

    def _lazy_get_RMM_(self, ignored): # relative molecular mass
        return self.density / self._amupokt
del Nucleon

water = Substance(
	density = Quantity(1 -27e-6 +tophat * micro, kilogramme / litre,
                           """Density of water.

at 277.13K, when density is maximal.\n"""),
        heat = Heats(
    capacity = Quantity(4.2 + tophat * .1, Joule / gram / Kelvin,
                        "The specific heat capacity of water")),
        temperature = Temperatures(
    triple = Quantity(273.16, Kelvin,
                      "Triple point of water (by definition of the Kelvin)."),
    melt = Quantity(273.150, Kelvin,
                      "Freezing point of water (at one atmosphere)."),
    boil = Quantity(373.150, Kelvin, "Melting point of water (at one atmosphere).")))
IcePoint = water.temperature.freeze
milk = Substance(density = 10.5 * pound / gallon)

air = Gas(RMM = 1.6 * Nitrogen.A + .4 * Oxygen.A, # close enough ...
          sound = Object(speed = Quantity(mach)))
air.sound.speed.observe(331.36 * metre / second) # duno where I got this one ...

kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)

mixture_doc = """Modeling chemical mixtures.

The underlying model is the `atomist' model of molecular-scale thermodynamics.

A body of space (of volume V) contains a population of little objects; these
move about, may fall apart, especially when bouncing off one another, at which
moments they may likewise stick to one another.

As they bounce around, each has momentum; in this inheres some energy.
Likewise, other `overall' properties of an object - e.g. its total charge or its
spin - may imply, taken with the object's position (and any external fields,
such as electromagnetic potential, there) and movement, energy which I'll
describe as `external': to know it, we only need to know bulk properties (`what
the outside world can see') of the object, notably those which are conserved.

Some objects, e.g. molecules, may also have internal structure and movement,
wherein energy may inhere: I shall describe this as `internal' energy of the
object.  Others, such as photons and electrons, only have external energy.  The
internal energy of an object only gets to be relevant in so far as energy moves
between it and external energy; thus we can ignore the internal energy of stable
atomic nuclei when discussing thermodynamics at household temperatures, but if
we ignore that of molecules, we'll never make sense of a household candle's
flame.

We're given a volume of space containing a population of objects, potentially of
several kinds in varying proportions.  We'll know what we've put into the volume
- how much of which kinds of object, how hot these were - we'll know its volume
and we'll be able to measure other bulk properties, like pressure, temperature
and colour.

Our model of the behaviour of the objects will include some reactions in which
they may partake.  For each of these, we'll know the inputs, one of which is
activation energy, and outputs, among which is any released energy.  Both
energies may be internal or external.

One approach would have distinct kinds of object to describe a molecule in its
various internal-energy states: how these interact with other things does vary.
However, at least when dealing with many such molecules, it is more practical to
describe the molecules as all being of the same kind, to presume that the
internal energies of molecules of the given kind are independent of their
external energies but that the internal energies of the molecules have a
thermodynamic distribution with the same temperature as the external energies.

Since the internal energy levels are apt to be discrete, the thermodynamic
distribution over them will have a different form from the usual (continuous)
Boltzmann distribution: but it will share the common themes of thermodynamic
spread, most obviously exp(-energy/kT) dependence.

We must also model how often objects participate in collisions involving how
much energy.  The classic model of this considers an object of radius r moving
at speed v, sweeping out a cylinder of space, populated with a density N/V of
objects of typical radius R; a collision will happen within a time interval of
length t if the cylinder of volume pi * v * t * (r+R)**2 contains the
centre-point of another object, which happens with probability that, at least
for small t, grows as volume * N / V.  We thus find a probability distribution
for the length of time between collisions, dpt(t) = h * exp(-h*t) with
h = pi * N * v * (r+R)**2 / V.

For a given possible reaction, we can examine the inputs, determine the sum of
their radii, use this in place of (r+R) and thus obtain h, which is the
probability per unit time that an ingredient molecule, of either kind but with
speed v, will collide with one of the complementary kind in any given time
interval (N is the number of objects of the complementary kind).  It will
actually make sense to replace pi * (r+R)**2 with A,(the cross-sectional area of
the cylinder - this cross-section is the `window of opportunity' for the
collision - to get h = v * A * N / V.  We can then bury, in A, various other
factors which complicate `will the reaction happen ?': A is the scattering
cross-section for the reaction; in principle, we could cope with having it
depend on v or, more likely, the total energy available in the collision.

In a collision, the total (internal and external) energy gets thrown into one
big pot, out of which any activation energy must come, into which any released
energy will flow; subsequently, this pot must be divided among the available
modes that can carry it; this sharing will be done thermodynamically.
"""
