"""Particle physics.

The quark/lepton/neutrino table has three columns, each with four rows: one row
for the neutrino, one for the lepton, two for the quarks.  One quark in each
column (the -quark below) has charge 1/3 that of the electron, the other has -2
times this (which is a positive charge, hence this is the +quark row).  Since the neutrino is simply known by
association with the lepton, e.g. the `electron neutrino' in the first column, I
elide their row from the following table:

 lepton electron  muon   tau
 -quark   down  strange beauty
 +quark    up    charm  truth

The quarks in the last column are also known as bottom and top.  Most matter is
composed of the first column: indeed, most matter is hydrogen, comprising a
proton and an electron; the proton is made of two up quarks and one down.

$Id: particle.py,v 1.1 2002-07-07 17:28:44 eddy Exp $
"""

from const import *
eV = Quantum.Millikan * Volt
eV.document('The electron-Volt: a standard unit of energy in particle physics.')

class Particle (Object):
    # needs merged in with units-related toys etc.
    __obinit = Object.__init__
    def __init__(self, name, *args, **what):
	setattr(self.__class__, name, self)
	apply(self.__obinit, args, what)
	self.__name = name

    __obgetat = Object.__getattr__
    def __getattr__(self, key):
	if key == 'name': return self.__name
	return self.__obgetat(key)

    __oblook = Object._lazy_lookup_
    def _lazy_lookup_(self, key):
	ans = self.__oblook(key)
	try: ans.name(nom = '%s.%s' % (self.name, key))
	except (AttributeError, TypeError): pass
	return ans

    def _lazy_get_magneton_(self, ignored):
        return self.charge * self.spin / self.mass

    def _lazy_get_wavelength_(self, ignored, k=Quantum.h/Vacuum.c):
	"""de Broglie wavelength along world-line: h/c/mass"""
	return k / self.mass

    def _lazy_get_period_(self, ignored, c=Vacuum.c):
	return self.wavelength / c

    def _lazy_get_decay_(self, ignored, zero=0/second):
	"""Fractional decay rate.

	This is defined by: the probability density for decay of the particle at
	time t is r*exp(-t*r) with r as the .decay attribute.  Unless otherwise
	specified, this is presumed to be zero; however, it may be specified
	when you initialise, e.g. Fermion(decay=32/second).

	The defining formula implies that the probability of decay before some
	specified time T is 1-exp(-T*r) and the mean time until decay is 1/r.
	"""

	return zero

    def _lazy_get_anti_(self, ignored):
	"""Returns self's anti-particle."""

	try: nom = {'electron': 'positron'}[self.name]
	except KeyError: nom = 'anti-%s' % self.name

	try:
	    bits = {}
	    for k, v in self.constituents.items():
		bits[k] = -v

	except AttributeError: bits = {self: -1}

	ans = self.__class__(nom, self, charge=-self.charge, constituents=bits)
	ans.anti = self

	return ans

    def _lazy_get_charge_(self, ignored, zero=0*Coulomb):
	try: bits = self.constituents
	except AttributeError: return zero

	q = zero
	for k, v in bits.items():
	    q = q + v * k.charge

	return q

    def __repr__(self): return self.__name
    __str__ = __repr__

    def __hash__(self): return hash(self.__name)

class Boson (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar):
        return default

light = Boson('photon', speed = Vacuum.c)
light.speed.document("""The speed of light in vacuum""")

class Fermion (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar/2):
        return default

class Neutrino (Fermion):
    def _lazy_get_charge_(self, ignored, default=0*Coulomb):
        return default

class Lepton (Fermion): 
    def _lazy_get_charge_(self, ignored, default=-Quantum.Millikan):
        return default

class Quark (Fermion):
    def _lazy_get_symbol_(self, ignored): return str(self)[0]

class Family (Object):
    def __init__(self, neutrino, lepton, neg, pos):
	self.neutrino, self.lepton = neutrino, lepton
	self.quarks = (neg, pos)
	neutrino.family = lepton.family = neg.family = pos.family = self

    def __repr__(self):
        return '%s+%s-family' % (repr(self.lepton), repr(self.quarks))

    def __getitem__(self, ind):
	if ind == 0: return self.neutrino
	if ind == 1: return self.lepton
	if ind in (2, 3): return self.quarks[ind-2]
	raise IndexError, 'A quark/lepton family has only four members'

def KLfamily(nm, lnom, lm, lrate, mnom, mm, pnom, pm,
	     q=Quantum.Millikan/3, mev=mega*eV/light.speed**2):

    """Deciphering Kaye&Laby p449.

    Positional arguments are as follows:

      neutron mass -- upper bound, measured in MeV

      lepton name -- string
      lepton mass -- in MeV
      lepton decay rate -- fraction of the given lepton species which decay per second

      -ve quark name -- name of the quark of charge with -ve charge e/3
      -ve quark mass -- mass estimate, in GeV, for the -ve quark

      +ve quark name -- name of the quark of charge with +ve charge 2*e/3
      +ve quark mass -- mass estimate, in GeV, for the +ve quark

    """

    return Family(Neutrino('%s.neutrino' % lnom, mass=Quantity(below(nm), mev)),
		  Lepton(lnom, mass=Quantity(lm, mev), decay=Quantity(lrate, Hertz)),
		  Quark(mnom, mass=mm*kilo*mev, charge=-q),
		  Quark(pnom, mass=pm*kilo*mev, charge=2*q))


table = ( KLfamily(4.6e-5, 'electron', sample(.5110034, .0000014), below(1./6e28),
		   'down', 0.35, 'up', 0.35),
	  KLfamily(.52, 'muon', sample(105.65932, .00029), mega / sample(2.19709, 5e-5),
		   'strange', .5, 'charm', 1.5),
	  KLfamily(74, 'tau', sample(1784.2, 3.2), tera / sample(.34, .05),
		   'beauty', 4.7, 'truth', sample(40,10)) )

Lepton.electron.also(magneticmoment = 928.476362e-26 * Joule / Tesla)

nucleon = Fermion('nucleon',
                  doc="""The `average' of a proton and a neutron.

The Mole is so defined that a Mole of carbon-12 weighs exactly 12 grams.  The
carbon-12 nucleus comprises six protons and six neutrons.  Thus dividing one
gram by the number of items in a Mole thereof yields one twelfth of the mass of
a carbon-12 atom, nominally the mass of an electron plus the average of the
masses of neutron and proton, albeit the binding energy of the nucleus reduces
this value.  The nucleon is the mythical average of a neutron and a proton
presumed by the foregoing. """,
                  constituents={Quark.up: 1.5, Quark.down: 1.5},
		  mass = Quantity(1 / mol / molar.Avogadro, gram,
                                  doc="""Atomic Mass Unit, AMU.

This is the nominal mass of a nucleon.  In reality, both proton and neutron are
a fraction of a percent heavier because the AMU is obtained as a twelfth of the
carbon-12 atom's mass, which (despite including the mass of an electron) is
lower than the proton or neutron mass by the binding energy per nucleon of
carbon. """)

AMU = AtomicMassUnit = nucleon.mass

Lepton.electron.mass.observe(Quantity(sample(548.58026, .0002), micro * AMU))
Lepton.muon.mass.observe(0.1134289168 * AMU)

neutron = Fermion('neutron', nucleon,
		  mass = Quantity(sample(1674.82, .08), harpo * gram),
		  constituents={Quark.up: 1, Quark.down: 2},
		  magneticmoment = 0.96623640e-26 * Joule / Tesla, # same units as magneton ...
		  charge = 0 * Coulomb)

proton = Fermion('proton', nucleon,
		 mass = Quantity(sample(1672.52, .08), harpo * gram),
		 constituents={Quark.up: 2, Quark.down: 1},
		 magneticmoment = 1.410606633e-26 * Joule / Tesla, # ... namely, current * area
		 charge = Quantum.Millikan)

deuteron = Boson('deuteron', proton,
		 # roughly the sum of proton and neutron:
		 mass = 2.01355321271 * AMU,
		 constituents={Quark.up: 3, Quark.down: 3},
		 # roughly the *difference* between proton and neutron:
		 magneticmoment = 0.433073457e-26 * Joule / Tesla)

alpha = Boson('alpha', constituents={proton: 2, neutron: 2})

# More atom-scale constants
hydrogen = Particle('hydrogen', mass=Quantity(sample(1673.43, .08), harpo * gram),
		    composition={proton: 1, Lepton.electron: 1})
deuterium = Particle('deuterium',
                     composition={deuteron: 1, Lepton.electron: 1})

radiusBohr = Vacuum.epsilon0 * (Quantum.h / Quantum.Millikan)**2 / pi / Lepton.electron.mass
radiusBohr.observe(Quantity(sample(52.9167, .0007), pico * metre))

# Other physical constants
helium = Object(
    element = Particle('Helium',
                       symbol = 'He',
                       consituents = { alpha: 1, Lepton.electron: 2 }))

water = Object(
	density = .999973 * kilogramme / litre, # at 277.13K, when density is maximal
	freezes = 273.150 * Kelvin)
milk = Object(
    density = 10.5 * pound / gallon)
IcePoint = water.freezes
mercury = Object(
	element = Particle('Mercury',
			   symbol = 'Hg',  etymology = 'hydrargyrum',
			   constituents = {
			       proton: 80,
			       Lepton.electron: 80,
			       neutron: Sample({
				   116: .15, 118: 10.02, 119: 16.84,
				   120: 23.13, 121: 13.22, 122: 29.80,
				   124: 6.85 }, best=120.59)}),
	# Atmosphere / .76 / metre / Earth.surface.g = density
        # 135.9 pound / gallon
	density = 13595.1 * kilogramme / metre**3)

air = Object(
	sound = Object(speed = 331.36 * metre / second))
kerosene = Object(density = 8 * pound / gallon)
alcohol = Object(density = 8 * pound / gallon)
petrol = Object(density = 7.5 * pound / gallon)

_rcs_log = """
 $Log: particle.py,v $
 Revision 1.1  2002-07-07 17:28:44  eddy
 Initial revision

"""
