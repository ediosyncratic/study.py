"""Particle physics.

The quark/lepton/neutrino table has three columns, each with four rows: one row
for the neutrino, one for the lepton, two for the quarks.  One quark in each
column (the -quark below) has charge 1/3 that of the electron, the other has -2
times this (which is a positive charge, hence this is the +quark row).  Since
the neutrino is simply known by association with the lepton, e.g. the `electron
neutrino' in the first column, I elide their row from the following table:

 lepton electron  muon   tau
 -quark   down  strange beauty
 +quark    up    charm  truth

The quarks in the last column are also known as bottom and top.  Most matter is
composed of the first column: indeed, most matter is hydrogen, comprising a
proton and an electron; the proton is made of two up quarks and one down.

$Id: particle.py,v 1.2 2002-10-06 18:38:06 eddy Exp $
"""

from const import *
eV = Quantum.Millikan * Volt
eV.document('The electron-Volt: a standard unit of energy in particle physics.')

def below(val, unit=tophat*(1-nano)+.5*(1+nano)):
    """Returns a sample from `almost zero' up to a given value.

    Required argument is the upper bound on some quantity: optional second
    argument is a distribution on the unit interval (its default is nearly
    uniform) which doesn't quite straddle zero. """

    # more sophistication might use a less uniform distribution ...
    return unit * val

class NameSpace: pass

class Particle (Object):
    # needs merged in with units-related toys etc.
    __obinit = Object.__init__
    def __init__(self, name, *args, **what):
        self._store_as_(name, self.__class__)
	apply(self.__obinit, args, what)
	self.__name = name

    def _store_as_(self, name, klaz):
        """Each sub-class of Particle carries a namespace full of its instances.

        That includes indirect instances but only applies to strict sub-classes,
        not to Particle itself.  Since Neutrino uses anomalous naming, I let
        sub-classes over-ride _store_as_, but this base-class implementation
        should be good enough for most classes - it chases back up the __bases__
        graph towards Particle doing the work.

        The namespace carrying the instances of the class is the .item attribute
        of the class.  Classes which include some particles with common aliases
        should explicitly set up their own .item object (e.g. as a
        Lazy(lazy_aliases={...}) to implement the aliasing); otherwise, this
        method will create a NameSpace(), which is utterly minimal, the first
        time it needs to store anything on .item. """

        todo, done = [ klaz ], [ Particle ]
        while todo:
            k, todo = todo[0], todo[1:]
            try: i = k.item
            except AttributeError: i = k.item = NameSpace()
            setattr(i, name, self)
            done.append(k)
            for b in k.__bases__:
                if b not in done and issubclass(b, Particle):
                    todo.append(b)

    __obgetat = Object.__getattr__
    def __getattr__(self, key):
	if key == 'name': return self.__name
	return self.__obgetat(key)

    __oblook = Object._lazy_lookup_
    def _lazy_lookup_(self, key):
	ans = self.__oblook(key)
	try: ans.document('The %s of the %s %s.' % (key, self.name, self.__class__.__name__))
	except (AttributeError, TypeError): pass
	return ans

    def _lazy_get_magneton_(self, ignored):
        return self.charge * self.spin / self.mass

    def _lazy_get_decay_(self, ignored, zero=0/second):
	"""Fractional decay rate.

	This is defined by: the probability density for decay of the particle at
	time t is r*exp(-t*r) with r as the .decay attribute.  Unless otherwise
	specified, this is presumed to be zero; however, it may be specified
	when you initialise, e.g. Fermion(decay=32/second).

	The defining formula implies that the probability of decay before some
        specified time T is 1-exp(-T*r), making the half-life log(2)/r, and the
        mean time until decay is 1/r. """

	return zero

    def _lazy_get_anti_(self, ignored):
	"""Returns self's anti-particle."""

        # the anti-electron is anomalously named :^o
        try: nom = {'electron': 'positron'
                    # any other anomalies ?
                    }[self.name]
        except KeyError: nom = 'anti-%s' % self.name

	try:
	    bits = {}
	    for k, v in self.constituents.items():
		bits[k] = -v

	except AttributeError: bits = {self: -1}

	ans = self.__class__(nom, self, charge=-self.charge, constituents=bits)
        ans.anti = self # NB cyclic reference; ans is about to become self.anti

	return ans

    def _lazy_get_charge_(self, ignored, zero=0*Coulomb):
	try: bits = self.constituents
	except AttributeError: return zero

	q = zero
	for k, v in bits.items():
	    q = q + v * k.charge

	return q

    def _lazy_get_period_(self, ignored, k=Quantum.h/Vacuum.c**2):
        """de Broglie wave period along world-line: h/c/c/mass"""
        return k / self.restmass

    def _lazy_get_bindingenergy_(self, ignored):
        sum = -self.energy
        try:
            for k, v in self.constituents.items():
                sum = sum + k.energy * abs(v)

        except AttributeError:
            return 0 * sum # to get units right

        return sum

    def _lazy_get_bindingfraction_(self, ignored):
        return self.bindingenergy / self.energy

    def _lazy_get_energy_(self, ignored):

        try: m = self.__dict__['mass']
        except KeyError: pass
        else: return m * Vacuum.c**2

        try: f = self.__dict__['frequency']
        except KeyError: pass
        else: return Quantum.h * f
        try: f = self.__dict__['nu']
        except KeyError: pass
        else: return Quantum.hbar * f

        raise AttributeError('energy', 'mass', 'frequency', 'nu')

    def _lazy_get_mass_(self, ignored, csqr = Vacuum.c**2):
        return self.energy / csqr

    def _lazy_get_frequency_(self, ignored):
        return self.energy / Quantum.h

    def _lazy_get_nu_(self, ignored):
        return self.energy / Quantum.hbar

    def _lazy_get_momentum_(self, ignored):
        try: k = self.__dict__['wavevector']
        except KeyError: pass
        else: return Quantum.hbar * k

        try: d = self.__dict__['wavelength']
        except KeyError: pass
        else: return Quantum.h / d

        raise AttributeError('momentum', 'wavevector', 'wavelength')

    def _lazy_get_wavevector_(self, ignored):
        return self.momentum / Quantum.hbar

    def _lazy_get_wavelength_(self, ignored):
        return Quantum.h / self.momentum

    def _lazy_get_restmass_(self, ignored, csqr = Vacuum.c**2):
        return (self.mass**2 - abs(self.momentum)**2 / csqr)**.5

    def __repr__(self): return self.__name
    __str__ = __repr__

    def __hash__(self): return hash(self.__name)

class Decays (Lazy):
    """Describes all the decays of some species of particle.
    """

    def __init__(self, source, *procs):
        """Initialises a Decays description.

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
              what decayed will be computed), and

            * all subsequent members are decay products, which must be
              Particle()s, of the decay mode which, at the given rate, releases
              the given energy.

        Each such sequence is used to construct an object holding the decay rate
        as its .rate, the (possibly computed) energy as its .energy and the
        decay products as its .fragments, a tuple.  These objects (if any) will
        be accessible as the members of self.processes, a tuple.

        The overall rate of decay of self.source is provided by self.rate; it is
        simply the sum of the .rate attributes over self.processes. """

        self.source = source
        row, rate = [], 0 / second
        for proc in procs:
            rate = rate + proc[0] # raising TypeError if not number/second
            row.append(apply(self.__Decay, proc))
        self.processes, self.rate = tuple(row), rate

    class __Decay:
        def __init__(self, family, rate, energy, *frags):
            if energy is None:
                energy = family.energy
                for it in frags:
                    energy = energy - it.energy

            assert not filter(lambda y: not isinstance(y, Particle), frags)
            self.rate, self.source, self.fragments, self.energy = rate, family.source, frags, energy

    def _lazy_get_energy_(self, ignored, csqr = Vacuum.c**2):
        if isinstance(self.source, Particle): return self.source.mass * csqr
        sum = 0 * Joule
        # source should be either Particle or sequence of Particle()s and amounts of energy
        for it in self.source:
            try: sum = sum + it.mass * csqr
            except (TypeError, AttributeError):
                sum = sum + it # must be an energy
        return sum

class Boson (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar):
        return default

class Photon (Boson):
    speed = Vacuum.c
    speed.document("""The speed of light in vacuum""")

    symbol, name = '&gamma;', 'light'
    spin = Quantum.hbar # iirc

    __upinit = Boson.__init__
    def __init__(self, *args, **what):
        try: what['name']
        except KeyError: args = ('photon',) + args
        apply(self.__upinit, args, what)

    def __repr__(self):
        # avoid using '(metre/second)**2 * kilogramme' as unit
        try: e = self.energy / eV
        except AttributeError: return 'light'
        if e > 1e11: # assert: e should be scalar (dimensionless)
            return 'Photon(energy=%s * Joule)' % `self.energy / Joule`
        return 'Photon(energy=%s * eV)' % `e`

    def __str__(self):
        try: return '%s(%s)' % (self.symbol, self.__energystr())
        except AttributeError: return self.symbol

    def __energystr(self):
        e = self.energy # raises AttributeError if we can't work this out.
        siz = e / eV
        if e > 1e11: # more than a few nano Joules
            siz = str(e)
            cut = siz.rindex(' ') + 1 # assert: there *is* a space in it
            assert siz[cut:] == '(m/s)**2.kg'
            return siz[:cut] + 'Joule'

        siz = str(siz)
        cut = siz.rfind(' ')
        if cut >= 0:
            cut = 1 + cut # actually we want to cut *after* the space
            try: return str[:cut] + {
                'mega': 'M',
                'giga': 'G',
                'kilo': 'k' }[str[cut:]] + 'eV'
            except KeyError: pass

        return siz + ' eV'

    restmass = 0 * kilogramme # inducing a correlation between energy and momentum
    __energy = Particle._lazy_get_energy_
    __momentum = Particle._lazy_get_momentum_

    def _lazy_get_energy_(self, ignored):
        try: return self.__energy(ignored)
        except AttributeError: pass

        try: p = self.__momentum(ignored)
        except AttributeError: pass
        else: return abs(p) * Vacuum.c

        raise AttributeError('energy', 'mass', 'momentum', 'nu', 'wavelength', 'wavevector')

    def _lazy_get_momentum_(self, ignored):
        try: return self.__momentum(ignored)
        except AttributeError: pass

        return self.energy / Vacuum.c

light = Photon()

class Fermion (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar/2):
        return default

class Neutrino (Fermion):
    # pass the constructor the corresponding Lepton's name
    __store_as = Fermion._store_as_
    def _store_as_(self, name, klaz):
        todo, done = [ klaz ], []
        # well, OK, Neutrino is unlikely to have sub-classes, but cope with them anyway ...
        while todo:
            k, todo = todo[0], todo[1:]
            try: i = k.item
            except AttributeError: i = k.item = NameSpace()
            setattr(i, name, self)
            done.append(k)
            for b in k.__bases__:
                if b not in done and issubclass(b, Neutrino):
                    todo.append(b)

        # forward modified name to Fermion et al.
        self.__store_as('%s neutrino' % name, Fermion)

    def _lazy_get_charge_(self, ignored, default=0*Coulomb):
        return default

    def _lazy_get_symbol_(self, ignored):
        lep = self.family.lepton
        return '&nu;<sup>%s</sup>' % lep.symbol

class Lepton (Fermion):
    """Lepton: primitive fermion. """

    item = Lazy(lazy_aliases={'anti-electron': 'positron'})
    def _lazy_get_charge_(self, ignored, default=-Quantum.Millikan):
        return default

class Quark (Fermion):
    item = Lazy(lazy_aliases={'top': 'truth', 'bottom': 'beauty'})
    def _lazy_get_symbol_(self, ignored): return str(self)[0]

class Family (Object):
    """A family of the standard model's table of primitive fermions.

    Each family comprises a neutrino, a lepton and a pair of quarks.  The lepton
    is easy to detect and its charge is Millikan's quantum.  The neutrino is
    named for the lepton in its family (i.e. electron neutrino, muon neutrino or
    tau neutrino).  The quarks are named independently and there's some
    contention over the third family quark-names. """

    def __init__(self, neutrino, lepton, neg, pos):
	self.neutrino, self.lepton = neutrino, lepton
	self.quarks = (neg, pos)
	neutrino.family = lepton.family = neg.family = pos.family = self

    def __repr__(self):
        return '%s+%s-family' % (repr(self.lepton), repr(self.quarks))

    def __len__(self): return 4
    def __getitem__(self, ind):
	if ind == 0: return self.neutrino
	if ind == 1: return self.lepton
	if ind in (2, 3): return self.quarks[ind-2]
	raise IndexError, 'A quark/lepton family has only four members'

def KLfamily(nm, lnom, lsym, lm, lrate, mnom, mm, pnom, pm,
	     q=Quantum.Millikan/3, mev=mega*eV/light.speed**2):

    """Deciphering Kaye&Laby p449.

    Positional arguments are as follows:

      neutrino mass -- upper bound, measured in MeV

      lepton name -- string
      lepton symbol -- string
      lepton mass -- in MeV
      lepton decay rate -- fraction of the given lepton species which decay per second

      -ve quark name -- name of the quark of charge with -ve charge e/3
      -ve quark mass -- mass estimate, in GeV, for the -ve quark

      +ve quark name -- name of the quark of charge with +ve charge 2*e/3
      +ve quark mass -- mass estimate, in GeV, for the +ve quark

    """

    return Family(Neutrino(lnom, mass=Quantity(below(nm), mev)),
                  Lepton(lnom, mass=Quantity(lm, mev), symbol=lsym, decay=Quantity(lrate, Hertz)),
                  Quark(mnom, mass=mm*kilo*mev, charge=-q),
                  Quark(pnom, mass=pm*kilo*mev, charge=2*q))

table = ( KLfamily(4.6e-5, 'electron', 'e', sample(.5110034, .0000014), below(1./6e28),
                   'down', sample(0.35, .005), 'up', sample(0.35, .005)),
          KLfamily(.52, 'muon', '&mu;', sample(105.65932, .00029), mega / sample(2.19709, 5e-5),
                   'strange', sample(.5, .05), 'charm', sample(1.5, .05)),
          KLfamily(74, 'tau', '&tau;', sample(1784.2, 3.2), tera / sample(.34, .05),
                   'beauty', sample(4.7, .05), 'truth', sample(40, 10)) )
# NB: the error bars on quark masses other than truth's are my interpolation
# from K&L's truncation of the numbers.

Lepton.item.electron.also(magneticmoment = 928.476362e-26 * Joule / Tesla)

# perhaps I should have a `Hadron' class for this lot ...

proton = Fermion('proton',
                 mass = Quantity(sample(1672.52, .08), harpo * gram),
                 doc = "charged ingredient in nuclei",
                 magneticmoment = 1.410606633e-26 * Joule / Tesla, # same units as magneton ...
                 constituents={Quark.item.up: 2, Quark.item.down: 1})

neutron = Fermion('neutron',
                  mass = Quantity(sample(1674.82, .08), harpo * gram),
                  doc = "uncharged ingredient in nuclei",
                  magneticmoment = 0.96623640e-26 * Joule / Tesla, # ... namely, current * area
                  constituents = {Quark.item.up: 1, Quark.item.down: 2},
                  charge = 0 * Coulomb) # (if unstated, the computed value has an error bar)

nucleon = Fermion('nucleon',
                  doc="""The `average' of a proton and a neutron.

The Mole is so defined that a Mole of carbon-12 weighs exactly 12 grams.  The
carbon-12 nucleus comprises six protons and six neutrons.  Thus dividing one
gram by the number of items in a Mole thereof yields one twelfth of the mass of
a carbon-12 atom, nominally (half the mass of an electron plus) the average of
the masses of neutron and proton, albeit the binding energy of the nucleus
reduces this value (by more than the electron mass).  The nucleon is the
mythical average of a neutron and a proton presumed by the foregoing. """,
                  constituents={Quark.item.up: 1.5, Quark.item.down: 1.5},
		  mass = Quantity(1 / mol / molar.Avogadro, gram,
                                  doc="""Atomic Mass Unit, AMU.

This is the nominal mass of a nucleon.  In reality, both proton and neutron are
a fraction of a percent heavier because the AMU is obtained as a twelfth of the
carbon-12 atom's mass, which (despite including half the mass of an electron) is
lower than the proton or neutron mass by carbon's binding energy per nucleon.
"""))

AMU = AtomicMassUnit = nucleon.mass

Lepton.item.electron.mass.observe(Quantity(sample(548.58026, .0002), micro * AMU))
Lepton.item.muon.mass.observe(0.1134289168 * AMU)

deuteron = Boson('deuteron',
		 # roughly the sum of proton and neutron:
		 mass = 2.01355321271 * AMU,
                 doc = "Deuterium's nucleus",
		 constituents={Quark.item.up: 3, Quark.item.down: 3},
		 # roughly the *difference* between proton and neutron:
		 magneticmoment = 0.433073457e-26 * Joule / Tesla)

alpha = Boson('alpha', doc="Helium's nucleus", constituents={proton: 2, neutron: 2})

# More atom-scale constants
radiusBohr = Vacuum.epsilon0 * (Quantum.h / Quantum.Millikan)**2 / pi / Lepton.item.electron.mass
radiusBohr.observe(Quantity(sample(52.9167, .0007), pico * metre))
Rydberg = (light.speed / (1/Lepton.item.electron.mass +1/proton.mass) / 2 / Quantum.h) * Vacuum.alpha**2

class Element (Particle):
    __upinit = Particle.__init__
    def __init__(self, Q, N, *args, **what):
        """Element Constructor.

        Reads first two positional parameters as numbers of protons, Q, and of
        neutrons, N, in the nucleus; Q is also the number of electrons orbiting
        that.  Uses these to construct the composition; forwards this and all
        other args to Particle's constructor. """

        what['constituents'] = { proton: Q, neutron: N, Lepton.item.electron: Q }
        apply(self.__upinit, args, what)

# Properties of some substances:
class Substance (Object): pass

hydrogen1 = Substance(
    element = Element(1, 0, 'Hydrogen 1',
                      mass=Quantity(sample(1673.43, .08), harpo * gram),
                      doc = "The simplest atom"))
deuterium = Substance(
    element = Element(1, 1, 'Deuterium'))

tritium = Substance(
    element = Element(1, 2, 'Tritium'))

helium = Substance(
    element = Element(2, 2, 'Helium', symbol = 'He'))

water = Substance(
	density = .999973 * kilogramme / litre, # at 277.13K, when density is maximal
	freezes = 273.150 * Kelvin)
milk = Substance(
    density = 10.5 * pound / gallon)
IcePoint = water.freezes
mercury = Substance(
    element = Element(80,
                      Sample({ 116: .15, 118: 10.02, 119: 16.84,
                               120: 23.13, 121: 13.22, 122: 29.80,
                               124: 6.85 }, best=120.59),
                      'Mercury',
                      symbol = 'Hg',  etymology = 'hydrargyrum'),
    # Atmosphere / .76 / metre / Earth.surface.g = density
    # 135.9 pound / gallon
    density = 13595.1 * kilogramme / metre**3)

air = Substance(
	sound = Object(speed = 331.36 * metre / second))
kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)

_rcs_log = """
 $Log: particle.py,v $
 Revision 1.2  2002-10-06 18:38:06  eddy
 Added .item namespace as carrier for all particles of each class, thus
 particles no longer clutter up their class name-space directly (and
 aliasing is easy); this also cleaned up neutrino-naming; and made this
 storage recursive, so Leptons show up among the Fermions, etc.

 Major over-haul of attributes mutually implied by de Broglie &c. (mass,
 wavelength, period, momentum, energy, ...).  Added symbols for Leptons
 and Neutrinos and error bars on quark masses.  Neutron and proton no
 longer borrow from nucleon - that was just confusing.

 Moved below() in from const.py (which didn't use it).  Added Photon,
 Substance and Element classes for relevant things to be instances of.
 Tweaked docs.

 Initial Revision 1.1  2002/07/07 17:28:44  eddy
"""
