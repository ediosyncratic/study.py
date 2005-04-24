"""The SI units.

See also units.py for a huge bestiary of other units; and
        http://physics.nist.gov/cuu/Units/current.html
for a definitive source of information about units.

Note that Quantity equips certain kinds of quantity with extra attributes:
time.light, mass.weight or mass.force, trigonometric attributes for angles,
their inverses and a few relatives for scalars, Centigrade and Fahrenheit
equivalents for temperatures.  See quantity.py for details.

$Id: SI.py,v 1.10 2005-04-24 15:30:00 eddy Exp $
"""
from quantity import *

# The base units
second = sec = s = base_unit('s', 'second',
                             """The SI base unit of time.

9192631770 periods of the radiation corresponding to the transition between
the two hyperfine levels of the ground state of the caesium-133 atom.""")

m = metre = base_unit('m', 'metre',
                      """The SI base unit of length.

The length of the path travelled by light in vacuum during a time interval of
1/299792458 of a second. """)

kilogramme = kilogram = kg = base_unit('kg', 'kilogramme',
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
# see also const.py, which adds some attributes, like Avogadro's constant.

cd = candela = base_unit('cd', 'Candela',
                        """The SI base unit of luminous intensity.

The luminous intensity, in a given direction, of a source that emits
monochromatic radiation of frequency 540e12 Hz and that has a radiant intensity
in that direction of 1/683 W/sr.

[The luminous intensity, in the perpendicular direction, of a surface of 1/60
square centimetere of a black body at the freezing temperature of platinum under
a pressure of 101325 Pascal (1 atmosphere).]""")

rad = radian = base_unit('rad', 'Radian',
                        """The SI supplementary unit of angle.

The angle subtended at the centre of a circle by an arc of the circumference
equal in length to the radius of the circle.""")

sr = steradian = base_unit('sr', 'Steradian',
                          """The SI supplementary unit of solid angle.

The unit of solid angle is the solid angle subtended at the center of a sphere
of radius r by a portion of the surface of the sphere having area r*r.""")

# Composite SI units
stere = pow(metre, 3)   # c.f. litre
are = pow(metre, 2)     # c.f. hectare
lm = lumen = candela * steradian # Luminous flux
lx = lux = lumen / are          # Luminance, Illumination
Hz = Hertz = 1 / second         # Frequency
C = Coulomb = Ampere * second   # Charge

N = Newton = kilogramme * metre / second / second       # Force
J = Joule = Newton * metre      # Energy
W = Watt = Joule / second       # Power
Pa = Pascal = Newton / are      # Pressure
P = poise = Pascal * second     # dynamic viscosity

V = Volt = Joule / Coulomb      # Electromagnetic potential
Wb = Weber = Joule / Ampere     # Magnetic flux
Ohm = Volt / Ampere             # Electrical resistance
S = Siemens = 1 / Ohm           # Conductance
F = Farad = second / Ohm        # Capacitance
H = Henry = Weber / Ampere      # Inductance
T = Tesla = Weber / are         # Magnetic flux density

"""
 $Log: SI.py,v $
 Revision 1.10  2005-04-24 15:30:00  eddy
 Doc tweaks.

 Revision 1.9  2004/04/03 18:11:23  eddy
 Mass/Time bodge now redundant thanks to kind-specific _lazy_late_ in Quantity.

 Revision 1.8  2003/07/05 15:44:11  eddy
 Moved trigonometric functions out to trig.py

 Revision 1.7  2003/04/17 22:45:50  eddy
 Put non-SI units of angle back in units.py

 Revision 1.6  2003/04/17 22:40:09  eddy
 Removed (most) SI-compatibles to unit.py; added trig functions using units of angle.

 Revision 1.5  2003/02/16 14:31:15  eddy
 Added NIST URL and reference to const.py, which now stores some
 attributes on mol which it previously kept on an object called molar.

 Revision 1.4  2002/02/15 16:15:20  eddy
 Moved the Mass/Time bodges here (from units.py).
 Added units of textile fineness, tex and dtex.

 Revision 1.3  2002/02/11 01:32:49  eddy
 Migrated some aliasing to units.py, moved light-speed to a datum of times,
 repositioned poise and Stokes.  Untabified.

 Revision 1.2  1999/08/07 15:20:07  eddy
 Moved ancillaries, added log.
"""
