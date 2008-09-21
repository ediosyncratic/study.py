"""The SI units.

See also units.py for a huge bestiary of other units; and
        http://physics.nist.gov/cuu/Units/current.html
        http://www.bipm.org/en/si/si_brochure/
for definitive sources of information about units.

Note that Quantity equips certain kinds of quantity with extra attributes:
time.light, mass.weight or mass.force, trigonometric attributes for angles,
their inverses and a few relatives for scalars, Centigrade and Fahrenheit
equivalents for temperatures.  See quantity.py for details.

$Id: SI.py,v 1.17 2008-09-21 20:03:43 eddy Exp $
"""
from quantity import *

# The base units
second = sec = s = base_unit('s', 'second',
                             """The SI base unit of time.

9192631770 periods of the radiation corresponding to the transition between
the two hyperfine levels of the ground state of the caesium-133 atom.
""")

m = metre = base_unit('m', 'metre',
                      """The SI base unit of length.

The length of the path travelled by light in vacuum during a time interval of
1/299792458 of a second.
""")

kilogramme = kilogram = kg = base_unit('kg', 'kilogramme',
                                       """The SI base unit of mass.

The mass of the *International Prototype* kilogramme (a platinum-iridium
cylinder) kept in the Bureau International des Poids et Mesures (BIPM),
S&egrave;vres, Paris.
""")

A = Ampere = base_unit('A', 'Ampere',
                      """The SI base unit of electric current.

That constant current which, if maintained in two parallel rectilinear
conductors, of immense length and negligible circular cross-section, placed 1
metre apart in a vacuum, would produce a force between these conductors equal to
2e-7 newton per meter of length.
""")

K = Kelvin = base_unit('K', 'Kelvin',
                      """The SI base unit of temperature.

The fraction 1/273.16 (exactly) of the thermodynamic temperature at the triple
point of water.
""")

mol = base_unit('mol', 'Mole',
                """The SI base unit of `amount of substance'.

The amount of substance which contains as many elementary units as there are
atoms in 12e-3 kilogrammes (exactly) of pure carbon-12.  The elementary unit
must be specified and may be atom, molecule, ion, radical, electron, photon
<I>etc</I>. or collection of elementary units.
""")

cd = candela = base_unit('cd', 'Candela',
                        """The SI base unit of luminous intensity.

The luminous intensity, in a given direction, of a source that emits
monochromatic radiation of frequency 540e12 Hz and that has a radiant intensity
in that direction of 1/683 W/sr.

[540 tera Hz light has a wavelength of 555 nm; the human eye's peak response is
to light with a wavelength of about 550 nm.  An older variant of the definition
specified the candela as the luminous intensity, in the perpendicular direction,
of a surface of 1/60 square centimetere of a black body at the freezing (or,
equivalently, melting) temperature of platinum under a pressure of 101325 Pascal
(i.e. 1 atmosphere).]
""")

rad = radian = base_unit('rad', 'Radian',
                        """The SI supplementary unit of angle.

The angle subtended at the centre of a circle by an arc of the circumference
equal in length to the radius of the circle.  Equal to turn/2/pi.
""")

sr = steradian = base_unit('sr', 'Steradian',
                          """The SI supplementary unit of solid angle.

The unit of solid angle is the solid angle subtended at the center of a sphere
of radius r by a portion of the surface of the sphere having area r*r.  By
considering the case of small square solid angles, it may readilly be seen that
solid angle is simply the square of angle; indeed, the steradian is simply the
square of the radian, sr = rad**2; see, e.g.,
http://www.chaos.org.uk/~eddy/math/angle.html
""")

# Composite SI units
stere = metre**3                # c.f. litre
are = (10 * metre) ** 2         # c.f. hectare in units.py
lm = lumen = candela * steradian # Luminous flux
lx = lux = lumen / m**2         # Luminance, Illumination
Hz = Hertz = 1 / second         # Frequency
C = Coulomb = Ampere * second   # Charge

N = Newton = kilogramme * metre / second ** 2 # Force
J = Joule = Newton * metre      # Energy
W = Watt = Joule / second       # Power
Pa = Pascal = Newton / m**2     # Pressure
P = poise = Pascal * second     # dynamic viscosity

V = Volt = Joule / Coulomb      # Electromagnetic potential
Wb = Weber = Joule / Ampere     # Magnetic flux
Ohm = Volt / Ampere             # Electrical resistance
S = Siemens = 1 / Ohm           # Conductance
F = Farad = second / Ohm        # Capacitance
H = Henry = Weber / Ampere      # Inductance
T = Tesla = Weber / m**2        # Magnetic flux density

# More properties of the mole:
mol.also(
    Avogadro = Quantity(sample(602.2045, .003), zetta / mol, "Avogadro's number"),
    # 24! * 0.970596 / Mole
    charge = Quantity(sample(96.48456, .00027),
                      kilo * Coulomb / mol,
                      doc="""Faraday's Constant

This is the charge per mole of protons; it is the charge transferred when
electrolysis liberates some univalent electrolyte, per mole liberated.\n"""),
    volume = Quantity(sample(22.4136, .003), milli * stere / mol, # .54781 firkin / mol
                      """Molar volume of an ideal gas at STP.

At standard temperature (zero Centigrade, 273.15 Kelvin) and pressure (one
atomosphere), one mole of any ideal gas will have a volume of 22.4136 litres.
Compare particle.Nucleon.amuk and mol.Loschmidt.\n"""))

# (mol.volume / mol.Avogadro)**(1./3) = 3.33879 * nano * metre
mol.also(
    Loschmidt = Quantity(1, mol.Avogadro / mol.volume,
                         """Loschmidt's number (gently rescaled).

Loschmidt's name is normally associated with the number of molecules (or atoms,
as appropriate) in one cubic centimetre of ideal gas under standard conditions;
however, since that makes it really a number per unit volume, I've gently
abridged the standard definition to give the number density of molecules (or
atoms); multiply this by cm**3 to get the orthodox number. """),
    Faraday = mol.charge)
# See also ../chemy/physics.py, which adds the gas constant, R.

def bykind(value, bok,
           naming={ 's': 'time', 'm': 'length', 'kg': 'mass', 'A': 'current',
                    'mol': 'amount', 'K': 'temperature', 'rad': 'angle',
                    stere._unit_str: 'volume', are._unit_str: 'area',
                    Hz._unit_str: 'frequency', C._unit_str: 'charge',
                    N._unit_str: 'force', J._unit_str: 'energy',
                    W._unit_str: 'power', P._unit_str: 'viscosity',
                    Ohm._unit_str: 'impedance', S._unit_str: 'conductance',
                    F._unit_str: 'capacitance', H._unit_str: 'inductance',
                    # Miscellaneous compound properties:
                    (N/kg)._unit_str: 'gravity', (N/kg/m)._unit_str: 'tidal',
                    (N*s)._unit_str: 'momentum', (N*s*m)._unit_str: 'action' }):
    """Add value to bok using value's kind as key.

    Requires exactly two arguments:
      value -- a Quantity of a kind with a standard name
      bok -- a mapping from names to values

    This is a helper function for classes, based on Object, with attributes
    named by the kind of their value; their constructors can iterate over
    positional parameters, passing each to this function along with their
    keyword parameter dictionary so as to give each value its kind as name.
    Useful, for example, for black holes and photons, any one of several
    attributes may be specified to the constructor and implies diverse others
    via lazy lookups: this saves having to overtly name the parameter.

    Raises KeyError on unsupported value type: callers can catch that and work
    out what to do with the exceptions; or pre-filter anyway, e.g. so as to bind
    lengths to some more apt name than 'length' (e.g. wavelength, radius) or
    resistances to 'resistance' instead of 'impedance'.\n"""

    kind = naming[value._unit_str] # shall KeyError on unsupported type
    bok[kind] = value
    return bok
