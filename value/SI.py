"""The SI units.

$Id: SI.py,v 1.3 2002-02-11 01:32:49 eddy Exp $
"""
from quantity import *

# The base units [kg and s get more sophisticated in units.py]
second = base_unit('s', 'second',
                      """The SI base unit of time.

9192631770 periods of the radiation corresponding to the transition between
the two hyperfine levels of the ground state of  the caesium-133 atom.""")

m = metre = base_unit('m', 'metre',
                     """The SI base unit of length.

The length of the path travelled by light in vacuum during a time interval of
1/299792458 of a second. """)

second.also(light = Quantity(299792458, metre,
                             doc = """The distance light travels each second in a vacuum.

This is exact, being now the definition of the metre."""))
# see also units.Time, which supplies the same datum systematically for times ...

kilogramme = base_unit('kg', 'kilogramme',
                                        """The SI base unit of mass.

The mass of the *International Prototype* kilogramme (a platinum-iridium
cylinder) kept in the Bureau International des Poids et Mesures (BIPM),
S&egrave;vres, Paris.""")

A = Ampere = base_unit('A', 'Ampere',
                      """The SI base unit of electric current.

That constant current which, if maintained in two parallel rectilinear
conductors, of immense length and negligible circular cross-section, placed 1
metre apart in a vacuum, would produce a force between these conductors equal to
2e-7 newton per meter of length.""")

K = Kelvin = base_unit('K', 'Kelvin',
                      """The SI base unit of temperature.

The fraction 1/273.16 (exactly) of the thermodynamic temperature at the triple
point of water. """)

mol = base_unit('mol', 'Mole',
               """The SI base unit of `amount of substance'.

The amount of substance which contains as many elementary units as there are
atoms in 12e-3 kilogrammes (exactly) of pure carbon-12.  The elementary unit
must be specified and may be atom, molecule, ion, radical, electron, photon
<I>etc</I>. or collection of elementary units. """)

cd = candela = base_unit('cd', 'Candela',
                        """The SI base unit of luminous intensity.

The luminous intensity, in a given direction, of a source that emits
monochromatic radiation of frequency 540e12 Hz and that has a radiant intensity
in that direction of 1/683 W/sr.

The luminous intensity, in the perpendicular direction, of a surface of 1/60
square centimetere of a black body at the freezing temperature of platinum under
a pressure of 101325 Pascal (1 atmosphere).""")

rad = radian = base_unit('rad', 'Radian',
                        """The SI supplementary unit of angle.

The angle subtended at the centre of a circle by an arc of the circumference
equal in length to the radius of the circle.""")

sr = steradian = base_unit('sr', 'Steradian',
                          """The SI supplementary unit of solid angle.

The unit of solid angle is the solid angle subtended at the center of a sphere
of radius r by a portion of the surface of the sphere having area r*r.""")

# Composite SI units
N = Newton = kilogramme * metre / second / second       # Force
Hz = Hertz = 1 / second         # Frequency
C = Coulomb = Ampere * second   # Charge
J = Joule = Newton * metre      # Energy
W = Watt = Joule / second       # Power
Pa = Pascal = Newton / metre / metre    # Pressure
lm = lumen = candela * steradian        # Luminous flux
lx = lux = lumen / metre / metre        # Illumination
stere = pow(metre, 3)
litre = milli * stere

V = Volt = Joule / Coulomb      # Electromagnetic potential
Wb = Weber = Joule / Ampere     # Magnetic flux
Ohm = Volt / Ampere             # Electrical resistance
S = Siemens = 1 / Ohm           # Conductance
F = Farad = second / Ohm        # Capacitance
H = Henry = Weber / Ampere      # Inductance
T = Tesla = Weber / metre / metre       # Magnetic flux density

P = poise = Pascal * second       # dynamic viscosity

# Other units
gram = milli * kilogramme
cm = centi * metre
cc = pow(cm, 3)

St = Stokes = pow(cm, 2) / second # kinematic viscosity
Angstrom = .1 * nano * metre    # &Aring;ngstr&ouml;m, aka &Aring;.
micron = micro * metre
fermi = femto * metre
are = pow(metre, 2)
hectare = hecto * are
barn = femto * hectare

Gs = Gauss = .1 * milli * Tesla
gamma = nano * Tesla
Mx = Maxwell = 10 * nano * Weber
stilb = 10 * kilo * candela / metre / metre
phot = 10 * kilo * lux

Bq = Becquerel = Hz             # Activity of a radionuclide
Gy = Gray = Joule / kilogramme  # Absorbed dose of radiation
Sv = sievert = Gy               # Dose equivalent
rem = 10 * milli * Sv
# 10 milli Gray is also called a rad (conflicts with radian)

"""
 $Log: SI.py,v $
 Revision 1.3  2002-02-11 01:32:49  eddy
 Migrated some aliasing to units.py, moved light-speed to a datum of times,
 repositioned poise and Stokes.  Untabified.

 Revision 1.2  1999/08/07 15:20:07  eddy
 Moved ancillaries, added log.
"""
