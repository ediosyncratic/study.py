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

$Id: particle.py,v 1.8 2003-04-21 20:09:46 eddy Exp $
"""

from const import *
eV = Quantity(Quantum.Millikan, Volt,
              doc='The electron-Volt: a standard unit of energy in particle physics.')

def below(val, unit=tophat*(1-nano)+.5*(1+nano)):
    """Returns a sample from `almost zero' up to a given value.

    Required argument is the upper bound on some quantity: optional second
    argument is a distribution on the unit interval (its default is nearly
    uniform) which doesn't quite straddle zero. """

    # more sophistication might use a less uniform distribution ...
    return unit * val

class Particle (Object):
    # needs merged in with units-related toys etc.
    __obinit = Object.__init__
    def __init__(self, name, *args, **what):
        try: self.__bits = what['constituents']
        except KeyError: pass # self will be deemed primitive
        else: del what['constituents']

	apply(self.__obinit, args, what)

        self._store_as_(name, self.__class__)
	self.__name = name

    _unborrowable_attributes_ = Object._unborrowable_attributes_ + (
        'name', 'symbol', 'charge', 'anti')

    def constituents(self, *primitives):
        """Returns self's composition.

        Takes any number of classes (irrelevant unless derived from Particle)
        and particles (irrelevant unless (possibly indirect) instances of
        Particle) to be deemed primitive and returns a dictionary mapping
        primitive particles to their multiplicities within self.  Regardless of
        any classes supplied as arguments, any particle whose composition wasn't
        specified when constructing it is deemed primitive.

        To get the composition specified when self was constructed, pass
        Particle as the sole argument; pass no arguments to get the particle
        reduced to its most primitive constituents; pass Nucleon to get a
        nucleus reduced to its nucleons; etc. """

        try: bits = self.__bits
        except AttributeError: return { self: 1 }
        bits, ans = bits.copy(), {}

        def carve(obj,
                  m=filter(lambda x: issubclass(x, Particle), primitives),
                  p=filter(lambda x: isinstance(x, Particle), primitives)):
            """Returns None if obj is primitive, else its constituents. """

            try: bok = obj.__bits
            except AttributeError: return None

            if obj in p: return None

            for k in m:
                if isinstance(obj, k): return None

            return bok

        while bits:
            for k in bits.keys():
                v, b = bits[k], carve(k)
                del bits[k]

                if b:
                    for q, r in b.items():
                        assert q is not k
                        bits[q] = bits.get(q, 0) + v * r
                else:
                    ans[k] = ans.get(k, 0) + v

        return ans

    def __bindener(self, bok):
        sum = -self.energy
        for k, v in bok.items():
            sum = sum + k.energy * abs(v)
        return sum

    def bindingenergy(self, *primitives):
        return self.__bindener(apply(self.constituents, primitives))

    def bindingfraction(self, *primitives):
        return apply(self.bindingenergy, primitives) / self.energy

    def bindingenergyper(self, *primitives):
        bok = apply(self.constituents, primitives)
        return self.__bindener(bok) / reduce(lambda a,b: a+b, map(abs, bok.values()), 0)

    class __ItemCarrier (Lazy):
        def __init__(self, *args, **what):
            ali = what.get('lazy_aliases', {})
            # Only relevant to Quark and its bases:
            ali.update({'top': 'truth', 'bottom': 'beauty'})
            what['lazy_aliases'] = ali
            apply(Lazy.__init__, (self,) + args, what)

        # Only relevant to Lepton and its bases:
        def _lazy_get_positron_(self, ignored):
            return self.electron.anti

        class _lazy_get_anti_ (Lazy):
            def __init__(self, source, ignored):
                self.__source = source

            def _lazy_lookup_(self, key):
                return getattr(self.__source, key).anti

    def _store_as_(self, name, klaz, root=None, ItemCarrier=__ItemCarrier):
        """Each sub-class of Particle carries a namespace full of its instances.

        That includes indirect instances but only applies to strict sub-classes,
        not to Particle itself.  Since Neutrino and Photon use anomalous naming,
        I let sub-classes over-ride _store_as_, but this base-class
        implementation should be good enough for most classes - it chases back
        up the __bases__ graph towards Particle doing the work.

        The namespace carrying the instances of the class is the .item attribute
        of the class, which is created the first time a particle of the class is
        stored.  The .item of a class should not be set otherwise: this method
        provides a special class for .item objects, which handles particle
        aliasing and other issues.  The .item object of a class also has a .anti
        sub-object for ease of reading; .item.anti.electron is effectively a
        synonym for .item.electron.anti, and likewise for other particles. """

        if root is None: root = Particle # can't set as default; not defined yet
        todo, done = [ klaz ], [ Particle ]
        while todo:
            k, todo = todo[0], todo[1:]
            try: i = k.item
            except AttributeError:
                i = k.item = ItemCarrier()

            done.append(k)
            try: getattr(i, name)
            except AttributeError:
                setattr(i, name, self)
                for b in k.__bases__:
                    if b not in done and issubclass(b, root):
                        todo.append(b)

    del __ItemCarrier

    def _lazy_get_name_(self, ignored): return self.__name

    __oblook = Object._lazy_lookup_
    def _lazy_lookup_(self, key):
	ans = self.__oblook(key)
	try: ans.document('The %s of the %s %s.' %
                          (key, self.name, self.__class__.__name__))
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
	    for k, v in self.__bits.items():
		bits[k] = -v

	except AttributeError: bits = {self: -1}

	ans = self.__class__(nom, self, _charge=-self._charge, constituents=bits)
        ans.anti = self # NB cyclic reference; ans is about to become self.anti

	return ans

    def _lazy_get__charge_(self, ignored):
        """Charge in units of on third the positron's charge.

        This is an exact integer value, far more suitable for working with than
        the actual charge, whose error bar grows with each arithmetic operation.
        """

        try: bits = self.__bits
        except AttributeError: return 0

        q = 0
        for k, v in bits.items():
            q = q + v * k._charge

        return q

    def _lazy_get_charge_(self, ignored, unit=Quantum.Millikan/3):
        return self._charge * unit

    def _lazy_get_period_(self, ignored, k=Quantum.h/Vacuum.c**2):
        """de Broglie wave period along world-line: h/c/c/mass"""
        return k / self.restmass

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

    def __str__(self): return self.__name
    def __repr__(self):
        return '%s.%s' % (self._namespace, self.__name)

    def _lazy_get__namespace_(self, ignored):
        """Namespace in which to look for self `normally'.

        This should usually be self's class; however, where a class has
        sub-classes to make distinctions (e.g. that between bosonic and
        fermionic nuclei, below) one orthodoxly ignores, self may prefer to be
        sought in the base-class with the nice familiar name rather than in the
        pedantically more apt derived class. """

        return '%s.item' % self.__class__.__name__

    def __hash__(self): return hash(self.__name)

class Boson (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar):
        return default

class Photon (Boson):
    speed = Vacuum.c
    speed.document("""The speed of light in vacuum""")

    symbol = '&gamma;'
    spin = Quantum.hbar # iirc

    __upinit = Boson.__init__
    def __init__(self, *args, **what):
        try: what['name']
        except KeyError: args = ('photon',) + args
        apply(self.__upinit, args, what)

    def __repr__(self):
        # Use name if it has a personal one:
        try: nom = self.name
        except AttributeError: pass
        else:
            if nom != 'photon': return nom

        # else use Photon(energy=...) for some suitable energy; but
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

    __store_as = Boson._store_as_
    def _store_as_(self, name, klaz):
        self.__store_as(name, klaz, Photon)
        if name != 'photon': name = '%s light' % name
        self.__store_as(name, Boson)

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

light = Photon(spectrum = ( # all rather approximate; see Nuffield, pp46--47.
    Photon(name='red', wavelength=Quantity(sample(642, 18), nano*metre)),
    Photon(name='orange', wavelength=Quantity(sample(615, 9), nano*metre)), # but see Na orange
    Photon(name='yellow', wavelength=Quantity(sample(598, 8), nano*metre)),
    # the human eye's peak response is at 550nm
    Photon(name='green', wavelength=Quantity(sample(555, 35), nano*metre)),
    Photon(name='cyan', wavelength=Quantity(sample(505, 15), nano*metre)), # blue-green
    Photon(name='blue', wavelength=Quantity(sample(465, 25), nano*metre)),
    Photon(name='indigo', wavelength=Quantity(sample(430, 10), nano*metre)), # dk blue
    Photon(name='violet', wavelength=Quantity(sample(408, 12), nano*metre)))) # purple
# Photon(name='infra-red', frequency=...), Photon('gamma', ...)
Photon(name='sodium orange', wavelength=590*nano*metre) # flagrantly contradicting spectrum

class Fermion (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar/2):
        return default

class Neutrino (Fermion):
    # pass the constructor the corresponding Lepton's name
    __store_as = Fermion._store_as_
    def _store_as_(self, name, klaz):
        # well, OK, Neutrino is unlikely to have sub-classes, but cope with them anyway ...
        self.__store_as(name, klaz, Neutrino)

        # forward modified name to Fermion et al.
        self.__store_as('%s neutrino' % name, Fermion)

    _charge = 0
    def _lazy_get_symbol_(self, ignored):
        lep = self.family.lepton
        return '&nu;<sup>%s</sup>' % lep.symbol

class Lepton (Fermion):
    """Lepton: primitive fermion. """

    _charge = -3

    __upinit = Fermion.__init__
    def __init__(self, *args, **what):
        try: self.__decay = what['decay']
        except KeyError: pass
        else: del what['decay']
        apply(self.__upinit, args, what)

    def _lazy_get_decay_(self, ignored):
        return Decay(self, (self.__decay, None, Lepton.item.electron, Neutrino.item.electron, self.family.neutrino.anti))

class Quark (Fermion):
    _namespace = 'Quark.item' # hide u/d distinction.
    def _lazy_get_symbol_(self, ignored): return str(self)[0]
    def _lazy_get_isospin_(self, ignored): return (self._charge -.5) / 3 # times hbar ?

class uQuark (Quark): _charge = 2
class dQuark (Quark): _charge = -1

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
	     mev=mega*eV/light.speed**2):

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
                  dQuark(mnom, mass=mm*kilo*mev),
                  uQuark(pnom, mass=pm*kilo*mev))

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
class Nucleon (Fermion):
    __upinit = Fermion.__init__
    def __init__(self, u, d, name, mass, doc, **what):
        what.update({'name': name, 'mass': mass, 'doc': doc,
                     'constituents': {Quark.item.up: u, Quark.item.down: d}})
        apply(self.__upinit, (), what)

    mass = Quantity(1 / mol / mol.Avogadro, gram,
                    doc="""Atomic Mass Unit, AMU.

This is the nominal mass of a nucleon.
In reality, both proton and neutron are a fraction of a percent heavier.

The Mole is so defined that a Mole of carbon-12 weighs exactly 12 grams.  The
carbon-12 nucleus comprises six protons and six neutrons.  Thus dividing one
gram by the number of items in a Mole thereof (Avogadro's constant) yields one
twelfth of the mass of a carbon-12 atom, nominally (half the mass of an electron
plus) the average of the masses of neutron and proton, albeit the binding energy
of the nucleus reduces this value (by more than the electron mass).
""")

AMU = AtomicMassUnit = Nucleon.mass
Lepton.item.electron.mass.observe(Quantity(sample(548.58026, .0002), micro * AMU))
Lepton.item.muon.mass.observe(0.1134289168 * AMU)

Nucleon(2, 1, 'proton',
        Quantity(sample(1672.52, .08), harpo * gram),
        "charged ingredient in nuclei",
        magneticmoment = 1.410606633e-26 * Joule / Tesla)
# magnetic moment has the same units as magneton: namely, current * area
# c.f. moment of inertia = mass * area
Nucleon(1, 2, 'neutron',
        Quantity(sample(1674.82, .08), harpo * gram),
        "uncharged ingredient in nuclei",
        magneticmoment = 0.96623640e-26 * Joule / Tesla)

class Nucleus (Particle): _namespace = 'Nucleus.item'
class bNucleus (Boson, Nucleus): 'Bosonic nucleus'
class fNucleus (Fermion, Nucleus): 'Fermionic nucleus'
def nucleus(Q, N, name, doc, **what):
    what.update({'name': name, 'doc': doc,
                 'constituents': {Nucleon.item.proton: Q, Nucleon.item.neutron: N}})

    try: klaz = {0: bNucleus, 1: fNucleus}[(Q + N) % 2]
    except KeyError: klaz = Nucleus
    return apply(klaz, (), what)

class Atom (Particle): _namespace = 'Atom.item'
class bAtom (Boson, Atom): 'Bosonic atom'
class fAtom (Fermion, Atom): 'Fermionic atom'
def atom(Q, N, name, symbol, doc, **what):
    ndoc = "%s's nucleus" % name
    try: n = what['nucleus']
    except KeyError:
        n = what['nucleus'] = nucleus(Q, N,
                                      '%s<sup>+%d</sup>' % (symbol, Q),
                                      ndoc)
    else:
        try:
            if not n.__dict__['__doc__']: raise KeyError
        except KeyError:
            n.__doc__ = ndoc

    what.update({'name': name, 'symbol': symbol, 'doc': doc,
                 'constituents': {n: 1, Lepton.item.electron: Q}})
    try: klaz = {0: bAtom, 1: fAtom}[N % 2]
    except KeyError: klaz = Atom
    return apply(klaz, (), what)

atom(1, 0, 'Hydrogen', 'H', 'The simplest atom; the most abundant form of matter',
     mass=Quantity(sample(1673.43, .08), harpo * gram),
     nucleus=Nucleon.item.proton)
atom(1, 1, 'Deuterium', 'D', '(Ordinary) Heavy Hydrogen',
     nucleus=nucleus(1, 1, 'deuteron', "Deuterium's nucleus",
                     # roughly the sum of proton and neutron:
                     mass = 2.01355321271 * AMU,
                     # roughly the *difference* between proton and neutron:
                     magneticmoment = 0.433073457e-26 * Joule / Tesla))
atom(1, 2, 'Tritium', 'T', 'Radioactive Heavy Hydrogen')
atom(2, 2, 'Helium', 'He', 'Second most abundant form of matter',
     nucleus=nucleus(2, 2, 'alpha', "Helium's nucleus"))

# Some atom-scale constants:
radiusBohr = Vacuum.epsilon0 * (Quantum.h / Quantum.Millikan)**2 / pi / Lepton.item.electron.mass
radiusBohr.observe(Quantity(sample(52.9167, .0007), pico * metre))
Rydberg = (light.speed / (1/Lepton.item.electron.mass
                          +1/Nucleon.item.proton.mass) / 2 / Quantum.h) * Vacuum.alpha**2

_rcs_log = """
 $Log: particle.py,v $
 Revision 1.8  2003-04-21 20:09:46  eddy
 _charge just ceased being borrowable anyway, so no need to say not to borrow it.

 Revision 1.7  2003/04/12 13:38:17  eddy
 Made various attributes, notably charge, non-borrowable (fixed problem
 of positron.charge being negative); made all .item carrier objects cope
 with name aliasing, also with .anti sub-object; thus made positron be
 created lazily without session needing to reference electron.anti first.

 Revision 1.6  2003/02/16 14:28:15  eddy
 Made Fermion use a Decay, lazily, as its .decay; added its __init__ to
 cope with that; adjusted to accommodate const.molar's attributes moving
 to the SI unit mol.

 Revision 1.5  2003/01/26 18:30:09  eddy
 Clean-up of light's naming.  Tweaked _store_as_ so that sub-classes can
 reuse the base-class implementation when over-riding it; made Photon do
 like Neutrino and change the name in its bases; stopped name propagation
 through bases from over-writing existing attributes.

 Revision 1.4  2003/01/26 16:43:30  eddy
 Removed Decays into its own module, for further mangling.
 Refined definition of eV, repr of Photon.
 Added .spectrum to light, noted sodium orange.

 Revision 1.3  2002/10/08 21:30:04  eddy
 Rearranged view of Particle's constituents; now wants them supplied as
 `highest-level' and provides .constituents([primitives...]) to decompose
 them to chosen level.  Likewise, scraps previous binding* attributes in
 favour of binging*([primitives...]) methods measured relative to
 particular choice of primitives.

 Changed charge to go via integer-valued (so exact) _charge attribute, in
 units of a third of Millikan's quantum.

 Made repr() produce namespace-qualified name (str() still gives leaf
 name) and provided for some classes to hide derived classes from the
 namespacing.  Separated iso+ and iso- quarks.  Added Nucleon class and
 scrapped nucleon.  Added Nucleus and Atom classes, with boson and
 fermion subs and nucleus() and atom() functions to access them.

 Removed substances to chemistry.py

 Revision 1.2  2002/10/06 18:38:06  eddy
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

 oh - and began sketching out new Decay class.

 Initial Revision 1.1  2002/07/07 17:28:44  eddy
"""
