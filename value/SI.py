"""The SI units.

See also units.py for a huge bestiary of other units; and
        http://physics.nist.gov/cuu/Units/current.html
        http://www.bipm.org/en/si/si_brochure/
for definitive sources of information about units.  The definitions in use here
are those from the turn of the millennium; a proposal for major reworking of the
units - to base them on measured physical constants of nature, thereby
eliminating the prototype kilogram artefact - is (2015) in preparation, with
2018 set as the likely date for transition.  Details of that are available at:
        http://en.wikipedia.org/wiki/New_SI_definitions
        http://www.bipm.org/utils/common/pdf/si_brochure_draft_ch2.pdf
TODO: the latter contains many useful details (unchanged from older versions of
the standard) that I should probably incorporate in this and units.py at some
point.

Note that Quantity equips certain kinds of quantity with extra attributes:
time.light; mass.weight or mass.force; trigonometric attributes for angles;
their inverses and a few relatives for scalars; Centigrade and Fahrenheit
equivalents for temperatures.  See quantity.py for details.

See study.LICENSE for copyright and license information.
"""
from quantity import *
TODO = """Revise to reflect

https://en.wikipedia.org/wiki/2019_redefinition_of_the_SI_base_units

and at least sketch the meaning of:
https://en.wikipedia.org/wiki/Kibble_balance
"""

# The base units
second = sec = s = Quantity.base_unit(
    's', 'second', """The SI base unit of time.

9192631770 periods of the radiation corresponding to the transition between the
two hyperfine levels of the ground state of the caesium-133 atom.  The big
number's prime factorisation is 2 * 3 * 3 * 5 * 7 * 7 * 47 * 44351.  The unit's
name comes from it being the 'second minute subdivision' of the hour; see
study.value.units.minute for further details.
""")

m = metre = Quantity.base_unit(
    'm', 'metre', """The SI base unit of length.

The length of the path travelled by light in vacuum during a time interval of
1/299792458 of a second.  That big number's prime factorisation is 2 * 7 * 73 *
293339.
""")

kilogramme = kilogram = kg = Quantity.base_unit(
    'kg', 'kilogramme', """The SI base unit of mass.

This is now (since 2019) defined in terms of a specified value for
Planck's constant (see chemy.physics), given the metre and second.

It had previously been defined to be the mass of the *International
Prototype* kilogramme (a platinum-iridium cylinder) kept in the Bureau
International des Poids et Mesures (BIPM), S&egrave;vres, Paris.  It
was thus the last unit to be specified in terms of a particular
physical object.
""")

A = Ampere = Quantity.base_unit(
    'A', 'Ampere', """The SI base unit of electric current.

That constant current which, if maintained in two parallel rectilinear
conductors, of immense length and negligible circular cross-section, placed 1
metre apart in a vacuum, would produce a force between these conductors equal
to 2e-7 newton per meter of length.
""")

K = Kelvin = Quantity.base_unit(
    'K', 'Kelvin', """The SI base unit of temperature.

The fraction 1/273.16 (exactly) of the thermodynamic temperature at the triple
point of water.  The fraction is equivalently 5 * 5 / 6829, in terms of primes.
""")

mol = Quantity.base_unit(
    'mol', 'Mole', """The SI base unit of `amount of substance'.

The amount of substance which contains as many elementary units as there are
atoms in 12e-3 kilogrammes (exactly) of pure carbon-12.  The elementary unit
must be specified and may be atom, molecule, ion, radical, electron, photon
etc. or collection of elementary units.
""")

cd = candela = Quantity.base_unit(
    'cd', 'Candela', """The SI base unit of luminous intensity.

The luminous intensity, in a given direction, of a source that emits
monochromatic radiation of frequency 540e12 Hz and that has a radiant
intensity in that direction of 1/683 W/sr.

[540 tera Hz light has a wavelength of 555 nm; the human eye's peak response
is to light with a wavelength of about 550 nm.  An older variant of the
definition specified the candela as the luminous intensity, in the
perpendicular direction, of a surface of 1/60 square centimetere of a black
body at the freezing (or, equivalently, melting) temperature of platinum under
a pressure of 101325 Pascal (i.e. 1 atmosphere).]
""")

rad = radian = Quantity.base_unit(
    'rad', 'Radian', """The SI supplementary unit of angle.

The angle subtended at the centre of a circle by an arc of the circumference
equal in length to the radius of the circle.  Equal to turn/2/pi.  SI deems the
radian to be a synonym for the dimensionless number 1; for my thoughts on that,
see http://www.chaos.org.uk/~eddy/math/angle.html#Scalar
""")

sr = steradian = Quantity.base_unit(
    'sr', 'Steradian', """The SI supplementary unit of solid angle.

The unit of solid angle is the solid angle subtended at the center of a sphere
of radius r by a portion of the surface of the sphere having area r*r.  By
considering the case of small square solid angles, it may readilly be seen
that solid angle is simply the square of angle; indeed, the steradian is
simply the square of the radian, sr = rad**2; see, e.g.,
http://www.chaos.org.uk/~eddy/math/angle.html#Solid

Consistent with this and its treatment of the radian (see radian.__doc__), SI
deems the steradian to be a synonym for the dimensionless number 1.
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
    Avogadro = Quantity(602214076, peta / mol,
                        """Avogadro's number

This is the number of units of some kind of thing that make up a mole
of that kind of thing.  Since 2019 its value is defined exactly, to a
specific nine-digit number times 1e15; it now constitutes the
definition of the mole.

It can be approximated in various ways:
 * 2**79 * .9962796165474711 or 256**10 * .49813980827373555
 * 24! * .9706110521321333 or 23! * 23 * 1.0128115326596172

Thus a set of size 24 has more than a mole of permutations and you
need at least ten bytes to faithfully represent it in binary.
"""),
    charge = Quantity.within(96.48456, .00027,
                             kilo * Coulomb / mol,
                             doc="""Faraday's Constant

This is the charge per mole of protons; it is the charge transferred, when
electrolysis liberates some univalent electrolyte, per mole liberated.\n"""),
    volume = Quantity.within(22.4136, .003, milli * stere / mol,
                             # .54781 firkin / mol
                             """Molar volume of an ideal gas at STP.

At standard temperature T and pressure P (see mol.STP, set in units.py), one
mole of any ideal gas will have a volume of R*T/P = 22.4136 litres.  Compare
particle.Nucleon.amuk and mol.Loschmidt.\n"""))

# (mol.volume / mol.Avogadro)**(1./3) = 3.33879 * nano * metre
mol.also(
    Loschmidt = Quantity(1, mol.Avogadro / mol.volume,
                         """Loschmidt's number (gently rescaled).

Loschmidt's name is normally associated with the number of molecules (or
atoms, as appropriate) in one cubic centimetre of ideal gas under standard
conditions; however, since that makes it really a number per unit volume, I've
gently abridged the standard definition to give the number density of
molecules (or atoms); multiply this by cm**3 to get the orthodox number.\n"""),
    Faraday = mol.charge)
# See also ../chemy/physics.py, which adds the gas constant, R.

def bykind(value, bok, alias={},
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

    Requires two arguments:
      value -- a Quantity of a kind with a standard name
      bok -- a mapping from names to values

    Accepts at most one further argument, alias, which should (if given) be a
    mapping from strings to strings: each key should be the canonical name of
    a kind, mapped to the attribute name that should be used for a parameter
    of this kind.  For example, { 'length': 'radius', 'impedance':
    'resistance' } would cause value to get the name radius if it were a
    length and the name resistance if it were an impedance.  Values of all
    other kinds simply get the canonical name of their kind.

    This is a helper function for classes, e.g. those based on Object, with
    attributes named by the kind of their value; their constructors can
    iterate over positional parameters that are instances of Quantity, passing
    each to this function along with their keyword parameter dictionary so as
    to give each value its kind as name.  Useful, for example, for black holes
    and photons: any one of several attributes may be specified to the
    constructor and implies diverse others via lazy lookups - this saves
    having to overtly name the parameter.

    Raises KeyError on unsupported value type: callers can catch that and work
    out what to do with the exceptions.  On success, returns the actual
    attribute name used.\n"""

    kind = naming[value._unit_str] # shall KeyError on unsupported type
    try: key = alias[kind]
    except KeyError: bok[kind] = value
    else: bok[key], kind = value, key
    return kind
