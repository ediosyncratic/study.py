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

$Id: particle.py,v 1.21 2007-03-17 16:50:59 eddy Exp $
"""

from const import *
from basEddy import Lazy
from decay import Decay

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
        """Describe a particle's rest-state.

        Required argument, name, is the name of the particle.  Other arguments
        are handled as for Object (q.v.) except for these keywords:

          constituents -- should be a mapping { particle: number } with self
                          comprising the given number of instances of each
                          particle.  Shalln't appear as an attribute: see the
                          eponymous method.

          decays -- should (if given) be either a Decay object (q.v.) or a
                    sequence suitable to serve as the second and later args to
                    Decay's constructor (q.v.), save that (in this latter case),
                    if you have also specified decay (probability per unit time
                    of decaying) or halflife, each entry's first entry can be
                    the proportion of instances of self that decay in the given
                    way, rather than the actual decay rate for that decay mode.
                    You can even skip decay or halflife if *some* first entries
                    are actual (positive) rates.  A suitable Decays object shall
                    be inferred from the sequence (and any decay or halflife
                    attribute, if any first entry is a proportion rather than a
                    rate).

        You can use an Object(particle, kinetic=energy) to specify a particle of
        the given type with some specified kinetic energy.
        """
        try: self.__bits = what['constituents']
        except KeyError: pass # self will be deemed primitive
        else: del what['constituents']

        try: decays = what['decays']
        except KeyError: pass
        else:
            if isinstance(decays, Decay): pass
            else:
                self.__decays = decays
                del what['decays']

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
            for k, v in bits.items():
                del bits[k]

                b = carve(k)
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

    def _lazy_get_decays_(self, ig, klaz=Decay, first=lambda it: it[0]):
        modes = self.__decays # raises AttributeError if we can't succeed
        del self.__decays # so we fail if we recurse for self.decay, below

        seq, mend = (), []
        for mode in modes:
            try: mode[0] + 1
            except TypeError:
                try: mode + check
                except TypeError:
                    raise TypeError(mode[0], """Bad decay rate or proportion.

Each first item of decays attribute (when not a Decay object) should be either a
rate (probability per unit time of the relevant decay mode happening) or a
proportion of the decaying particle type that, on average, decay via the given
mode.""", mode, modes, self)
                seq = seq + (mode,)
            else:
                mend.append(mode)

        if mend:
            if seq:
                # we can work out to total from seq and mend ;-)
                known = sum(map(first, seq))
                share = sum(map(first, mend))
                # known rate accounts for 1-share of all decays
                # so self.decay should be known/(1-share)
                assert 0 <= share <= 1, 'bad sum of proportions'
                total = known / (1 - share)
                assert (not(self.__dict__.has_key('decay') or
                            self.__dict__.has_key('halflife')) or
                        abs(total - self.decay) < total.width + self.decay.width), \
                        'Estimated and specified total decay rates differ'

            else: total = self.decay # fail if unspecified
            for mode in mend:
                seq = seq + ((mode[0] * total,) + tuple(mode[1:]),)

        return apply(klaz, (self,) + seq)

    log2 = Quantity(2).log
    def _lazy_get_decay_(self, ignored, zero=0/second, ln2=log2):
	"""Fractional decay rate.

	This is defined by: the probability density for decay of the particle at
	time t is r*exp(-t*r) with r as the .decay attribute.  Unless otherwise
	specified, this is presumed to be zero; however, it may be specified
	when you initialise - either directly, e.g. Fermion(decay=32/second), or
	indirectly via attribute halflife or (better) decays; see constructor
	documentation.

	The defining formula implies that the probability of decay before some
        specified time T is 1-exp(-T*r), making the half-life log(2)/r, and the
        mean time until decay is 1/r. """

        try: halflife = self.__dict__['halflife']
        except AttributeError: pass
        else: return ln2/halflife

        try: decays = self.decays
        except AttributeError: pass
        else: return decays.rate

	return zero

    def _lazy_get_halflife_(self, ignored, ln2=log2):
        return ln2 / self.decay

    del log2

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
        else: return Quantum.Planck * f

        raise AttributeError('energy', 'mass', 'frequency', 'nu')

    def _lazy_get_mass_(self, ignored, csqr = Vacuum.c**2):
        return self.energy / csqr

    def _lazy_get_qperm_(self, ignored):
        """Charge-to-mass ratio"""
        return self.charge / self.mass

    def _lazy_get_frequency_(self, ignored):
        return self.energy / Quantum.h

    def _lazy_get_nu_(self, ignored):
        try: return self.frequency / turn
        except AttributeError: pass
        return self.energy / Quantum.Planck

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
    """Photons are the irreducible corpuscles of light.

Isaac would have been proud.
See also visible's doc and:
http://imagers.gsfc.nasa.gov/ems/ems.html
http://imagine.gsfc.nasa.gov/docs/science/know_l1/spectrum_chart.html
from the second of which I took the extra-visible spectral data below.
"""

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

def photon(lo, hi, name, **what):
    what['name'] = name
    what['wavelength'] = Quantity(.5 * (hi + lo) + tophat * (hi - lo), nano * metre)
    return apply(Photon, (), what)

visible = photon(380, 700, 'visible',
                 doc="""Visible light.

The visible spectrum ranges from .4 to .7 microns.  See, e.g.,
    http://www.sundog.clara.co.uk/rainbows/primcol.htm
on the site that persuaded me to broaden the spectrum to 380--700 nm;
but see also (conflicting in details)
    http://cimss.ssec.wisc.edu/wxwise/bluesky.html
for which I've widened red but not changed the rest.
Fundamentally, the boundaries between colours are severely subjective !

In particular, see attribute spectrum, a sequence of colour bands; and/or look
in visible's name-space for these bands by name; in due course I'll add
sub-bands and particular spectral lines within these, e.g. data from
    http://www.badastronomy.com/info/pix.html
and elsewhere on badastronmy.com, for some spectral line data within the bands
(the Bad Astronomer's data is in Angstroms, since that's the unit he uses).  A
common pattern among the spectral lines is an element name followed by a roman
numeral, which appears to denote *one more than* the level to which the atom has
been ionized; mayhap it's really the index of the electron whose transitions
we're seeing, counting inwards from the most easily dislodged ones.
""",
                 # All rather approximate; see Nuffield, pp46--47 and sources in doc string.
                 #' It'd be nice to have a way to use blurry-boundaries ... a chart of the spectrum ?
                 red=photon(624, 780, 'red',
                            NII=Photon(name='NII', wavelength=6580 * Angstrom, source='Nitrogen')),
                 orange=photon(606, 624, 'orange'), # but see Na orange
                 yellow=photon(590, 606, 'yellow',
                               # Flagrantly contradicting naming of bands ! (and should be two lines):
                               Na = Photon(name='sodium orange', wavelength=590*nano*metre)),
                 # ruby ? absorb green -> emit red channel ...
                 green=photon(520, 590, 'green'), #' the human eye's peak response is at 550nm
                 cyan=photon(490, 520, 'cyan', # blue-green
                             OIII=Photon(name='OIII', wavelength=5007 * Angstrom, source='doubly-ionized Oxygen')),
                 blue=photon(440, 490, 'blue',
                             Hbeta=Photon(name='H-beta', wavelength=4860 * Angstrom, source='Hydrogen')),
                 indigo=photon(420, 440, 'indigo'), # dk blue
                 violet=photon(380, 420, 'violet')) # purple

visible.also(spectrum=(visible.red, visible.orange, visible.yellow,
                       visible.green, visible.cyan, visible.blue,
                       visible.indigo, visible.violet))

_unit = .5 + tophat
radio = Photon(name="radio", frequency = Quantity(3 * _unit, giga * Hertz))
microwave = Photon(name="microwave",
                   wavelength = Quantity(1 + 99 * _unit, milli * metre),
                   frequency = Quantity(1 + 99 * _unit, 3 * giga * Hertz))
infrared = Photon(name="infra-red",
                  wavelength = Quantity(.7 + 999.3 * _unit, micro * metre),
                  frequency = Quantity(.3 + 399.7 * _unit, tera * Hertz),
                  near=Photon(name='near infra-red',
                              # (.7-1) to 5 microns
                              wavelength=Quantity(.7 + 4.3 * _unit, micro * metre)),
                  mid=Photon(name='mid infra-red',
                             # 5 to (25-40) microns
                             wavelength=Quantity(5 + 35 * _unit, micro * metre)),
                  far=Photon(name = 'far infra-red',
                             # (25-40) to (200-350) microns
                             wavelength = Quantity(25 + 325 * _unit, micro * metre)),
                  __doc__="""Infra-Red Light

The infra-red spectrum is loosely divided into three bands - near, mid and far - though
boundaries are even more subjective than those for the visible spectrum.  Near infra-red
over-laps with the red end of the visible spectrum, but its long-wavelength boundary is
set by the atmosphere: air is transparent to it.  The longer wavelengths can only be used,
for astronomy, from outside Earth's atmosphere.  See
    http://www.ipac.caltech.edu/Outreach/Edu/Regions/irregions.html
for further details.
""")
# visible fits in here
ultraviolet = Photon(name="ultra-violet",
                     wavelength = Quantity(10 + 390 * _unit, nano * metre),
                     frequency = Quantity(.75 + 29.25 * _unit, peta * Hertz))
Xray = Photon(name="X-ray",
              wavelength = Quantity(.01 + 9.99 * _unit, nano * metre),
              frequency = Quantity(.01 + 9.99 * _unit, 3 * exa * Hertz))
gamma = Photon(name="gamma", wavelength = Quantity(10 * _unit, pico * metre))
del _unit

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
    """Lepton: primitive fermion."""

    _charge = -3
    class __item (Lazy):
        def _lazy_get_positron_(self, ignored): return self.electron.anti
    item = __item(lazy_aliases={'anti-electron': 'positron'})
    del __item

    def _lazy_get_decays_(self, ignored):
        return Decay(self, (self.decay, None,
                            Lepton.item.electron,
                            Neutrino.item.electron,
                            self.family.neutrino.anti))

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

del Lazy

def KLfamily(nm, lnom, lsym, lm, lrate, mnom, mm, pnom, pm,
	     mev=mega*eV/Photon.speed**2):

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
# the muon's magnetic moment is approximately equal; 2e-6 fractional difference ...

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

    amuk = Quantity(mass / Thermal.k,
                    doc = """AMU scaled down by Boltzmann's constant.

If we look at the ideal gas law, P*V = N*k*T, for N items of a gas with mass m
per item, we get density = N*m/V = m*P/k/T.  Since m is the relative molecular
(or atomic) mass, M, times the atomic mass unit, we can write it as M * AMU and
obtain density = M*amuk*P/T with M a pure number (and, typically, very close to
an integer).  Thus, at standard temperature (zero Celsius) and pressure (one
Atmosphere), density is just M times 44.618 grams per cubic metre.\n""")

AMU = AtomicMassUnit = Nucleon.mass
Lepton.item.electron.mass.observe(Quantity(sample(548.58026, .0002), micro * AMU))
Lepton.item.muon.mass.observe(0.1134289168 * AMU)

Nucleon(2, 1, 'proton',
        Quantity(sample(1672.52, .08), harpo * gram),
        "The charged ingredient in nuclei",
        magneticmoment = 1.410606633e-26 * Joule / Tesla)
# magnetic moment has the same units as magneton: namely, current * area
# c.f. moment of inertia = mass * area

# Make proton and electron primary exports:
proton, electron = Nucleon.item.proton, Lepton.item.electron

Nucleon(1, 2, 'neutron',
        Quantity(sample(1674.82, .08), harpo * gram),
        "The neutral (uncharged) ingredient in nuclei",
        halflife=13*minute,
        decays=((1, None, proton, electron),) # plus neutrino/anti pair ? gamma ?
        magneticmoment = 0.96623640e-26 * Joule / Tesla)

# Also make neutron a primary export:
neutron = Nucleon.item.neutron

# what of:
# pion, mass = 273.2 * electron.mass, charges 0, +1, -1.

# Some atom-scale constants:
radiusBohr = Vacuum.epsilon0 * (Quantum.h / Quantum.Millikan)**2 / pi / electron.mass
radiusBohr.observe(Quantity(sample(52.9167, .0007), pico * metre))
Rydberg = (Photon.speed / Quantum.h / (2 / electron.mass +2 / proton.mass)) * Vacuum.alpha**2

_rcs_log = """
 $Log: particle.py,v $
 Revision 1.21  2007-03-17 16:50:59  eddy
 Better handling of decays.

 Revision 1.20  2007/03/08 23:49:31  eddy
 Replacing hbar with Planck

 Revision 1.19  2006/04/22 15:24:24  eddy
 const no longer exports Lazy !

 Revision 1.18  2005/04/10 17:41:00  eddy
 Another spectral href.

 Revision 1.17  2005/04/09 09:12:52  eddy
 Punctuation.

 Revision 1.16  2005/03/21 23:46:03  eddy
 Added some data on the infra-red.

 Revision 1.15  2005/03/21 22:49:36  eddy
 Doc/coment burble, expanded red out to 780 micron.

 Revision 1.14  2005/02/14 08:08:04  eddy
 Made visible into a namespace holding the spectral bands; added some
 spectral lines (from Bad Astronomy) to these bands.

 Revision 1.13  2005/01/28 01:29:09  eddy
 Added extra-visible specrum, thanks to NASA.

 Revision 1.12  2005/01/16 19:39:25  eddy
 Cleaner presentation of the visible spectrum.

 Revision 1.11  2004/02/17 00:14:47  eddy
 Noted muon's magnetic dipole, added Nucleon.amuk, AMU/k.
 Noted neutron's half life, mumbled about pion.

 Revision 1.10  2003/07/07 23:12:39  eddy
 Added .qperm (c.f. const.Cosmos.qperm) as charge-to-mass ratio of particles.

 Revision 1.9  2003/06/15 14:50:36  eddy
 Removed nucleus and atom (to new elements.py), made p,n,e into primary exports.

 Revision 1.8  2003/04/21 20:09:46  eddy
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
