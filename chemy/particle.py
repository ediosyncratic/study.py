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

See also: elements.py
See study.LICENSE for copyright and license information.
"""
from study.snake.lazy import Lazy
from study.value.quantity import Quantity, Object
from study.value.units import pi, arc, bykind, \
     harpo, femto, pico, nano, micro, milli, kilo, mega, giga, tera, peta, exa, \
     gram, metre, mol, second, year, Volt, Angstrom, Hertz, Joule, Tesla
from physics import Quantum, Vacuum, Thermal
from decay import Decay

eV = Quantity(Quantum.Millikan, Volt,
              doc='The electron-Volt: a standard unit of energy in particle physics.')

def between(lo, hi, units, *args, **what):
    return Quantity.flat(lo, hi, None, units, *args, **what)

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

        The following keyword arguments, if supplied, should match the
        interpretation of them used in this class:

          decay -- probability per unit time of decaying
          lifetime -- expected time to decay, inverse of decay
          halflife -- half-life of particle
          decays -- a decay.Decays (q.v.) object.

        You can use an Object(particle, kinetic=energy) to specify a particle of
        the given type with some specified kinetic energy.\n"""
        try: self.__bits = what['constituents']
        except KeyError: pass # self will be deemed primitive
        else: del what['constituents']

        self.__obinit(*args, **what)

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
        nucleus reduced to its nucleons; etc.\n"""

        try: bits = self.__bits
        except AttributeError: return { self: 1 }
        bits, ans = bits.copy(), {}

        def carve(obj,
                  m=[x for x in primitives if issubclass(x, Particle)],
                  p=[x for x in primitives if isinstance(x, Particle)]):
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
        return self.__bindener(self.constituents(*primitives))

    def bindingfraction(self, *primitives):
        return self.bindingenergy(*primitives) / self.energy

    def bindingenergyper(self, *primitives):
        bok = self.constituents(*primitives)
        return self.__bindener(bok) / sum(abs(v) for v in bok.values())

    class __ItemCarrier (Lazy):
        __upinit = Lazy.__init__
        def __init__(self, *args, **what):
            ali = what.get('lazy_aliases', {})
            # Only relevant to Quark and its bases:
            ali.update({'top': 'truth', 'bottom': 'beauty'})
            what['lazy_aliases'] = ali
            self.__upinit(*args, **what)

        # Only relevant to Lepton and its bases:
        def _lazy_get_positron_(self, ignored):
            return self.electron.anti

        class _lazy_get_anti_ (Lazy):
            def __init__(self, source, ignored):
                self.__source = source

            def _lazy_lookup_(self, key):
                return getattr(self.__source, key).anti

    def _store_as_(self, name, cls, root=None, ItemCarrier=__ItemCarrier):
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
        todo, done = [ cls ], [ Particle ]
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
        mean time until decay is 1/r.\n"""

        try: halflife = self.__dict__['halflife']
        except AttributeError: pass
        else: return ln2 / halflife

        try: lifetime = self.__dict__['lifetime']
        except AttributeError: pass
        else: return 1. / halflife

        try: decays = self.decays
        except AttributeError: pass
        else: return decays.rate

        return zero

    def _lazy_get_halflife_(self, ignored, ln2=log2):
        """Time taken for the probability of having decayed to reach half"""
        return ln2 / self.decay

    del log2

    def _lazy_get_lifetime_(self, ignored):
        """Expected time to decay, for a particle of the given type"""
        return 1 / self.decay

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
        the actual charge, whose error bar grows with each arithmetic operation.\n"""

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

    def _lazy_get_energy_(self, ignored, h=Quantum.h, P=Quantum.Planck):

        try: m = self.__dict__['mass']
        except KeyError: pass
        else: return m.energy

        try: t = self.__dict__['period']
        except KeyError: pass
        else: return h / t
        try: f = self.__dict__['frequency']
        except KeyError: pass
        else: return h * f
        try: f = self.__dict__['nu']
        except KeyError: pass
        else: return Planck * f

        raise AttributeError('energy', 'mass', 'period', 'frequency', 'nu')

    def _lazy_get_mass_(self, ignored):
        return self.energy.mass

    def _lazy_get_qperm_(self, ignored):
        """Charge-to-mass ratio"""
        return self.charge / self.mass

    def _lazy_get_frequency_(self, ignored, h=Quantum.h):
        return self.energy / h

    def _lazy_get_nu_(self, ignored, h=Quantum.Planck):
        try: return self.frequency / turn
        except AttributeError: pass
        return self.energy / h

    def _lazy_get_momentum_(self, ignored, hbar=Quantum.hbar, h=Quantum.h):
        try: k = self.__dict__['wavevector']
        except KeyError: pass
        else: return hbar * k

        try: d = self.__dict__['wavelength']
        except KeyError: pass
        else: return h / d

        raise AttributeError('momentum', 'wavevector', 'wavelength')

    def _lazy_get_wavevector_(self, ignored, hbar=Quantum.hbar):
        return self.momentum / hbar

    def _lazy_get_wavelength_(self, ignored, h=Quantum.h):
        return h / self.momentum

    def resolve(self, aperture):
        """Resolving power of an aperture.

        Returns the angle subtended, at an aperture of given diameter, by the
        gap between a pair of objects that can just be resolved by an aparatus
        observing these objects through that aperture using particles whose
        wavelength is equal to that of self.\n"""
        return (self.wavelength / aperture).arcSin

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
        pedantically more apt derived class.\n"""

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

    speed, symbol, spin = Vacuum.c, '&gamma;', Quantum.hbar # iirc

    __upinit = Boson.__init__
    def __init__(self, *args, **what):
        for val in args:
            bykind(val, what, { 'length' : 'wavelength', 'time': 'period' })

        try: what['name']
        except KeyError: what['name'] = 'photon'

        self.__upinit(**what)

    def __repr__(self, J=Joule):
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
            return 'Photon(energy=%s * Joule)' % `self.energy / J`
        return 'Photon(energy=%s * eV)' % `e`

    def __str__(self):
        try: return '%s(%s)' % (self.symbol, self.__energystr())
        except AttributeError: return self.symbol

    def __energystr(self, eV=eV):
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
    def _store_as_(self, name, cls):
        self.__store_as(name, cls, Photon)
        if name != 'photon': name = '%s light' % name
        self.__store_as(name, Boson)

    restmass = 0 * gram # inducing a correlation between energy and momentum
    __energy = Particle._lazy_get_energy_
    __momentum = Particle._lazy_get_momentum_

    def _lazy_get_energy_(self, ignored, c=Vacuum.c):
        try: return self.__energy(ignored)
        except AttributeError: pass

        try: p = self.__momentum(ignored)
        except AttributeError: pass
        else: return abs(p) * c

        raise AttributeError('energy', 'mass', 'momentum',
                             'period', 'frequency', 'nu',
                             'wavelength', 'wavevector')

    def _lazy_get_momentum_(self, ignored, c=Vacuum.c):
        try: return self.__momentum(ignored)
        except AttributeError: pass

        return self.energy / c

def photon(lo, hi, name, **what): # local tool
    what['name'] = name
    return Photon(between(lo, hi, nano * metre), **what)

visible = photon(380, 700, 'visible',
                 doc="""Visible light.

The visible spectrum ranges from .4 to .7 microns.  See, e.g.,
    http://www.atoptics.co.uk/rainbows/primcol.htm
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
                 red=photon(624, 700, 'red',
                            # There's a Hydrogen line in here, too
                            NII=Photon(name='NII', wavelength=6580 * Angstrom,
                                       source='Nitrogen')),
                 orange=photon(606, 624, 'orange'), # but see Na orange
                 yellow=photon(590, 606, 'yellow',
                               # Flagrantly contradicting naming of bands ! (and should be two lines):
                               Na = Photon(name='sodium orange', wavelength=590*nano*metre,
                                           source="Sodium")),
                 # ruby ? absorb green -> emit red channel ...
                 green=photon(520, 590, 'green'), #' the human eye's peak response is at 550nm
                 cyan=photon(490, 520, 'cyan', # blue-green
                             OIII=Photon(name='OIII', wavelength=5007 * Angstrom,
                                         source='doubly-ionized Oxygen')),
                 blue=photon(440, 490, 'blue',
                             Hbeta=Photon(name='H-beta', wavelength=4860 * Angstrom,
                                          source='Hydrogen')),
                 indigo=photon(420, 440, 'indigo'), # dk blue
                 violet=photon(380, 420, 'violet')) # purple
del photon

visible.also(spectrum=(visible.red, visible.orange, visible.yellow,
                       visible.green, visible.cyan, visible.blue,
                       visible.indigo, visible.violet),
             rainbow=Quantity.within(
        138.7, .7, arc.degree,
        """The angle through which a rainbow turns visible light.

This varies with the colour of the light, red being turned least and blue most.
Since the angle exceeds a quarter turn, the arc of a (pure water) rainbow
appears centred on the direction opposite to the light source (generally the
sun), at an angle ranging from 40.6 (violet) to 42 (red) degrees from that
direction.  The spray-bow resulting from sea-spray is tighter - sea water
droplets turn light through a larger angle than pure water droplets.
""",
                              secondary=between(
            127, 130, arc.degree,
            """The angle through which a secondary rainbow turns visible light.

Compare visible.rainbow, the primary angle: for the secondary rainbow, red (130
degrees) is turned more than violet (127 degrees); since this range is less than
that of the primary rainbow (but still more than a quarter turn), the secondary
bow appears outside the primary.
""")))

radio = Photon(name="radio", frequency = Quantity.below(3, giga * Hertz))
microwave = Photon(name="microwave",
                   wavelength = between(1, 100, milli * metre),
                   frequency = between(1, 100, 3 * giga * Hertz))
infrared = Photon(name="infra-red",
                  wavelength = between(.7, 1000, micro * metre),
                  frequency = between(.3, 400, tera * Hertz),
                  near=Photon(name='near infra-red',
                              # (.7-1) to 5 microns
                              wavelength=between(.7, 5, micro * metre)),
                  mid=Photon(name='mid infra-red',
                             # 5 to (25-40) microns
                             wavelength=between(5, 40, micro * metre)),
                  far=Photon(name = 'far infra-red',
                             # (25-40) to (200-350) microns
                             wavelength = between(25, 350, micro * metre)),
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
                     wavelength = between(10, 400, nano * metre),
                     frequency = between(.75, 30, peta * Hertz))
Xray = Photon(name="X-ray",
              wavelength = between(.01, 10, nano * metre),
              frequency = between(.01, 10, 3 * exa * Hertz))
gamma = Photon(name="gamma", wavelength = Quantity.below(10, pico * metre))

class Fermion (Particle):
    def _lazy_get_spin_(self, ignored, default=Quantum.hbar/2):
        return default

# Each neutrino and lepton has "lepton number" +1; each anti has -1.
class Neutrino (Fermion):
    # pass the constructor the corresponding Lepton's name
    __store_as = Fermion._store_as_
    def _store_as_(self, name, cls):
        # well, OK, Neutrino is unlikely to have sub-classes, but cope with them anyway ...
        self.__store_as(name, cls, Neutrino)

        # forward modified name to Fermion et al.
        self.__store_as('%s neutrino' % name, Fermion)

    # http://physicsworld.com/cws/article/news/32861;jsessionid=7F62EB73BA3BDB0E6DEBCFB01B56F9F3
    # magnetic dipole should allegedly be proportional to mass

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

    class CKM (Object):
        """Cabibbo-Kobayashi-Maskawa matrix

        The CKM matrix describes weak decays between uQuark and dQuark of the
        various flavours.  Its complex conjugate describes the decays between
        their anti-particles.  The matrix is necessarily unitary.  This implies
        that the sum of squared-moduli of each row or column is 1.  It also
        implies zero as various sums of three terms; each such sum can be
        represented on an Argand diagram as a triangle, known as 'a unitarity
        triangle', whose height (perpendicular to its longest side)
        characterises the amount of charge-parity symmetry violation involved in
        the decays described by the two columns (or two rows) involved.  The one
        describing transitions involving the bottom and down quarks, cd.cb*
        +td.tb* +ud.ub* = 0, is anticipated to have most height - the rest are
        all expected to be relatively flat - consequently, this unitarity
        triangle is generally referred to as *the* unitarity triangle; its usual
        depiction re-scales its longest side, cd.cb*, to lie along the real axis
        from 0 to 1, with ud.ub* as the edge coming out of the origin.  The
        angle at the origin, opposite td.tb*, is then called &gamma;, the angle
        at the top, opposite cd.cb*, is called &alpha; and the angle opposite
        ud.ub* is called &beta;.

        See: http://physicsweb.org/articles/world/20/4/4/1\n"""

    CKM = CKM() # TODO: data !

class uQuark (Quark): _charge = 2
class dQuark (Quark): _charge = -1

class Family (Object):
    """A family of the standard model's table of primitive fermions.

    Each family comprises a neutrino, a lepton and a pair of quarks.  The lepton
    is easy to detect and its charge is Millikan's quantum.  The neutrino is
    named for the lepton in its family (i.e. electron neutrino, muon neutrino or
    tau neutrino).  The quarks are named independently and there's some
    contention over the third family quark-names.

    http://golem.ph.utexas.edu/category/2007/06/this_weeks_finds_in_mathematic_14.html
    lists four force bosons along with the three families, alongside a table,
    suggestively lining up each boson with a particle type; although I would be
    inclined to line up gamma (the photon) with the charted leptons, W or Z with
    neutrinos, the other of these and the gluon with the quarks.\n"""

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

def KLfamily(nm, lnom, lsym, lm, lme, lrate,
             mnom, mm, mme, pnom, pm, pme,
             mev=mega*eV.mass, Hz=Hertz):
    """Deciphering Kaye&Laby p449.

    Positional arguments are as follows:

      neutrino mass -- upper bound, measured in MeV

      lepton name -- string
      lepton symbol -- string
      lepton mass -- in MeV
      lepton mass error -- half-width of error bar on previous
      lepton decay rate -- fraction of the given lepton species which decay per second

      -ve quark name -- name of the quark with -ve charge e/3
      -ve quark mass -- mass estimate, in GeV, for the -ve quark
      -ve quark mass error -- half-width of error-bar on previous

      +ve quark name -- name of the quark with +ve charge 2*e/3
      +ve quark mass -- mass estimate, in GeV, for the +ve quark
      +ve quark mass error -- half-width of error bar on previous\n"""

    return Family(Neutrino(lnom, mass=Quantity.below(nm, mev)),
                  Lepton(lnom, mass=Quantity.within(lm, lme, mev), symbol=lsym,
                         decay=lrate * Hz),
                  dQuark(mnom, mass=Quantity.within(mm, mme, kilo*mev)),
                  uQuark(pnom, mass=Quantity.within(pm, pme, kilo*mev)))

# physicsworld article cited in class Neutrino says neutrino mass <= 1 eV;
# cosmological reasons suggest the sum of the three neutrino masses <= 0.3 eV;
# both are less than the K&L's data, given here:
table = ( KLfamily(4.6e-5, 'electron', 'e', .5110034, .0000014,
                   Quantity.below(1./6e28),
                   'down', 0.35, .005, 'up', 0.35, .005),
          KLfamily(.52, 'muon', '&mu;', 105.65932, .00029,
                    mega / Quantity.within(2.19709, 5e-5),
                    'strange', .5, .05, 'charm', 1.5, .05),
          KLfamily(74, 'tau', '&tau;', 1784.2, 3.2,
                   tera / Quantity.within(.34, .05),
                   'beauty', 4.7, .05, 'truth', 40, 10) )
# NB: the error bars on quark masses other than truth's are my interpolation
# from K&L's truncation of the numbers.
del KLfamily

# Make electron a primary export:
electron = Lepton.item.electron
electron.also(magneticmoment = 928.476362e-26 * Joule / Tesla)
# the muon's magnetic moment is approximately equal; 2e-6 fractional difference ...

class Hadron (Particle):
    """Particles composed of quarks."""
class Meson (Boson, Hadron):
    """Quark anti-quark combinations."""
    # http://hyperphysics.phy-astr.gsu.edu/hbase/particles/meson.html
class Baryon (Fermion, Hadron):
    """Particles composed of three quarks."""
    # http://hyperphysics.phy-astr.gsu.edu/hbase/hframe.html
    # http://pdg.lbl.gov/2005/listings/bxxxcomb.html
    __upinit = Fermion.__init__
    def __init__(self, name, mass, doc, **what):
        what.update(name=name, mass=mass, doc=doc)
        self.__upinit(**what)

class Nucleon (Baryon):
    __upinit = Baryon.__init__
    def __init__(self, u, d, name, mass, doc, **what):
        what['constituents'] = { Quark.item.up: u, Quark.item.down: d}
        self.__upinit(name, mass, doc, **what)

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
Lepton.item.electron.mass.observe(Quantity.within(548.58026, .0002, micro * AMU))
Lepton.item.muon.mass.observe(0.1134289168 * AMU)

# http://en.wikipedia.org/wiki/Free_neutron
# Links to a good data source at the end:
# http://pdg.lbl.gov/2006/tables/bxxx.pdf

proton = Nucleon(2, 1, 'proton',
                 Quantity.within(938.27203, 6e-5, mega * eV.mass,
                                 sample = (Quantity.within(1.00727646688, .13e-9, AMU),
                                           Quantity.within(1672.52, .08, harpo * gram))),
                 "The charged ingredient in nuclei",
                 halflife=1e33*year,
                 magneticmoment = 1.410606633e-26 * Joule / Tesla,
                 polarizability = Object(electric = Quantity.within(120, .6,
                                                                    (femto * metre)**3),
                                         magnetic = Quantity.within(1.9, .5,
                                                                    (femto * metre)**3)))
# magnetic moment has the same units as magneton: namely, current * area
# c.f. moment of inertia = mass * area

neutron = Nucleon(1, 2, 'neutron',
                  Quantity.within(939565.36, .8, kilo * eV.mass,
                                  sample = (Quantity.within(1.0086649156, .6e-9, AMU),
                                            Quantity.within(1674.82, .08, harpo * gram))),
                  "The neutral (uncharged) ingredient in nuclei",
                  halflife=Quantity.within(613.9, .55, second,
                                           """Neutron half-life.

Early measurements of the neutron half-life (e.g. 'over 15 minuts' in 1948, 11.7
+/- .3 minutes in 1950s, 10.61 +/1 .16 in 1971) were incompatible (that is,
their error bars didn't over-lap), but showed a consistent downward trend.  That
trend appears to have stabilised, with (reassuringly) mutually compatible
results emerging in the 1990s ans since, converging on the value used here
('Review of Particle Properties', K. Hagiwara et al. (Particle Data Group),
Phys. Rev.  D 66 (2002) 010001).\n"""),
                  # http://hyperphysics.phy-astr.gsu.edu/hbase/particles/proton.html#c4
                  decays=((1, .7824e6 * eV, proton, electron, Neutrino.item.electron.anti),),
                  # charge: Quantity.within(-.4, 1.1, zepto * electron.charge), i.e. zero.
                  magneticmoment = 0.96623640e-26 * Joule / Tesla,
                  polarizability = Object(electric = Quantity.within(1.16, .15,
                                                                     (femto * metre)**3),
                                          magnetic = Quantity.within(.37, .2,
                                                                      (femto * metre)**3)))

# what of:
# pion, mass = 273.2 * electron.mass, charges 0, +1, -1.

# Some atom-scale constants:
Bohr = Quantity(Vacuum.epsilon0 / pi / electron.mass, (Quantum.h / Quantum.Millikan)**2,
                """The Bohr radius.

This is a length-scale that arises naturally in the description of electron
orbitals within atoms, defined by epsilon0 * (h/e)**2 / pi / m, where m is the
mass of the electron.  See also Rydberg, which is alpha/4/pi/Bohr.
""")
Bohr.observe(Quantity.within(52.9167, .0007, pico * metre))
Bohr.also(radius = Bohr)

Rydberg = Quantity(0.5 * Vacuum.alpha**2 / Quantum.h,
                   Vacuum.c / (1 / electron.mass + 1 / proton.mass),
                   """Rydberg's constant.

This is the inverse wavelength (a.k.a. spatial frequency) of a photon emitted
when a previously free electron falls into the ground state orbital of a
hydrogen atom (or, for an atom of atomic number Z, an orbital in shell Z, except
that we should then use the mass of the nucleus in place of the proton mass in
the formula used here).  For details, see
http://www.chaos.org.uk/~eddy/physics/atom.xhtml#Solved

See also Rydberg.energy and Bohr.radius
""")
Rydberg.also(frequency=Rydberg * Vacuum.c,
             energy=Quantity(Rydberg, Vacuum.c * Quantum.h,
                             """The Rydberg energy

This is (at least for the Hydrogen atom) the natural unit for energies of atomic
energy levels: (c*alpha)**2 times half the effective mass of the electron, the
inverse of the sum of inverses of masses of the electron and nucleus.  Double it
to get the Hartree energy.
"""))
Rydberg.energy.observe(13.605698 * Quantum.Millikan * Volt)

del Quantum, Vacuum, Thermal, pi, arc, Quantity, between, Decay, \
    harpo, femto, pico, nano, micro, milli, kilo, mega, giga, tera, peta, exa, \
    gram, metre, mol, second, year, Volt, Angstrom, Hertz, Joule, Tesla
