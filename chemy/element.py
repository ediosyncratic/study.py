"""Repository of data about the chemical elements and their isotopes.

The problem: we have two paths for isotopic mixing.  That is: an element's
composition describes it as a mix of atoms of various isotopes, each of which
has a composition comprising a definite number of neutrons, along with the
appropriate number of protons and electrons; but equally, e.g. from the point of
view of a chemical species (via its molecule), the element's atom has a definite
arrangement of electrons around a nucleus which is a mixture of possible
variants.  I chose to take the former description seriously and fake the latter.

For data, see (inter alia): http://www.webelements.com/

TODO: convert to use a minimalist constructor with hooks by which to be supplied
with data from diverse sources (e.g. Open Data sets, ad hoc files); this module
can be converted with minimal entanglement with others.

See study.LICENSE for copyright and license information.
"""
from study.value.units import Object, Sample, Quantity, kilo, nano, harpo, \
     Joule, Tesla, Ohm, Kelvin, Centigrade, gram, kg, tonne, metre, mol, torr, \
     cc, year
from particle import AMU, Particle, Boson, Fermion, \
     proton, neutron, electron

class Nucleus (Particle): _namespace = 'Nucleus.item'
class bNucleus (Boson, Nucleus): 'Bosonic nucleus'
class fNucleus (Fermion, Nucleus): 'Fermionic nucleus'
def nucleus(Q, N, name, **what):
    what.update(name=name, constituents={ proton: Q, neutron: N })

    try: cls = [ bNucleus, fNucleus ][(Q + N) % 2]
    except (IndexError, TypeError): cls = Nucleus # e.g. when N is a Sample
    return cls(**what)

class Atom (Particle): _namespace = 'Atom.item'
class bAtom (Boson, Atom): 'Bosonic atom'
class fAtom (Fermion, Atom): 'Fermionic atom'
def atom(Q, N, name, symbol, doc, **what):
    ndoc = "%s %d's nucleus" % (name, Q+N)
    try: n = what['nucleus']
    except KeyError: n = what['nucleus'] = nucleus(Q, N, ndoc)
    else:
        try: # i.e. if n's own __doc__ is empty, None or missing, use ndoc
            if not n.__dict__['__doc__']:
                raise KeyError
        except KeyError: n.__doc__ = ndoc

    what.update(name=name, symbol=symbol, __doc__=doc,
                constituents={n: 1, electron: Q})
    try: cls = {0: bAtom, 1: fAtom}[N % 2]
    except KeyError: cls = Atom # e.g. if N is a distribution
    return cls(**what)

class Substance (Object): pass
# should really have
# Element to carry abundance &c. data, subclass of Substance
# Atom to describe per-atom data, subclass of Molecule
# issue: does isotopic data belong to Element or Nucleus ?
# Ion as peer of Atom and Molecule

class Heats (Object):
    # should really deal with molar vs volume vs specific
    def _lazy_get_fusion_(self,         ig): return  self.melt
    def _lazy_get_freeze_(self,         ig): return -self.melt
    def _lazy_get_vaporization_(self,   ig): return  self.boil
    def _lazy_get_vaporisation_(self,   ig): return  self.boil
    def _lazy_get_condense_(self,       ig): return -self.boil

class Temperatures (Object):
    __upinit = Object.__init__
    __zero = 0 * Kelvin
    def __init__(self, melt=None, boil=None, **what):
        assert melt is None or boil is None or melt + self.__zero <= boil
        if melt is not None: what['melt'] = melt
        if boil is not None: what['boil'] = boil
        self.__upinit(**what)

    def __str__(self):
        bok = {}
        for k in self.__dict__.keys():
            if k != 'borrow' and k[0] != '_' and k[-1] != '_':
                bok[k] = getattr(self, k)
        return str(bok)

    def _lazy_get_fusion_(self,         ig): return self.melt
    def _lazy_get_freeze_(self,         ig): return self.melt
    def _lazy_get_vaporization_(self,   ig): return self.boil
    def _lazy_get_vaporisation_(self,   ig): return self.boil
    def _lazy_get_condense_(self,       ig): return self.boil

class Isotope (Substance):
    """Single actual isotope.

    Well, OK, if constructor's N is a distribution (and you remove an
    assert or move it to the function below), you'll get a
    distribution.  But my intent is that we don't do that; though we
    might do the equivalent for a Nucleus, for the sake of an
    Element's atom.

    This class is not directly exposed, but accessed via a function of
    the same name that caches results and thus ensures that equal
    instances are the same object.\n"""

    __upinit = Substance.__init__
    def __init__(self, Q, N):
        self.__names = () # names displaced by uses of .nominate()
        assert isinstance(Q, int) and isinstance(N, int)
        self.__upinit(protons=Q, neutrons=N)

        try: el = Element.byNumber[self.protons]
        except IndexError: pass
        else:
            if el is not None:
                el[Q+N] = self

    def nominate(self, name, symbol):
        Element.bySymbol[symbol] = Element.byName[name] = self
        # So we record the attempt even if we fail to put it into effect ...

        if self.__names: self.__names.append(self.__name)
        else: self.__names = [ self.name ]
        self.__names.insert(0, self.symbol)

        try: del self.name
        except AttributeError: pass
        self.__name, self.symbol = name, symbol

        return self # convenience for the places this is used ...

    def _lazy_get_quarks_(self, ignored):
        """Twople of counts of (up, down) quarks the isotope is 'really' made of."""
        p, n = self.protons, self.neutrons
        return 2 * p + n, p + 2 * n

    def _lazy_get_name_(self, ignored):
        if self.__names: return '%s (a.k.a. %s)' % (self.__name, ', '.join(self.__names))
        return '%s[%s]' % (self.element.name, self.massnumber)

    def _lazy_get_symbol_(self, ignored):
        return '%s[%s]' % (self.element.symbol, self.massnumber)

    def _lazy_get_fullsymbol_(self, ignored):
        return '<sup>%d</sup><sub>%d</sub>%s' % (self.massnumber, self.protons, self.symbol)

    def _lazy_get__lazy_hash_(self, ignored):
        return hash(self.__class__) ^ hash(self.protons) ^ hash(self.neutrons)

    def _lazy_get_atom_(self, ignored):
        return atom(self.protons, self.neutrons, self.name, self.symbol,
                    "Atom of isotope %s" % self.fullsymbol)

    def _lazy_get_electrons_(self, ignored): return self.protons
    def _lazy_get_massnumber_(self, ignored): return self.protons + self.neutrons
    def _lazy_get_element_(self, ignored):
        el = Element.byNumber[self.protons]
        if el is None:
            el = Element(None, None, self.protons, None)
            # may cause problems if that element is subsequently constructed nicely !
        return el

    def __cmp__(self, other):
        return self.protons - other.protons or self.neutrons - other.neutrons
    def __repr__(self): return self.name
    def __str__(self): return self.symbol

full = {}
def Isotope(Q, N, cls=Isotope, rack=full):
    """Unique instance of (hidden) Isotope class.

    Takes two positional parameters (do not pass more, or keywords):
      * the number of protons (atomic number)
      * the number of neutrons (nominal atomic mass minus atomic number).

    As long as no-one ever passes more than two, calls with the same
    two parameters shall get the same object back.

    The currently known instances can be accessed by calling
    Isotopes(), or the ones that arise in nature (i.e. have a non-zero
    abundance) by calling naturalIsotopes().
    """
    try: ans = rack[(Q, N)]
    except KeyError: ans = rack[(Q, N)] = cls(Q, N)
    return ans

Isotopes = full.values
naturalIsotopes = lambda : [x for x in Isotopes() if getattr(x, 'abundance', None)]
del full

# Local function, del'd below:
def Noble(n):
    """Element number for noble gas number n (counting with He at 0).

    If we take each noble gas as the last element in its cycle, the
    successive cycles of the periodic table have lengths (breaking
    after each group VIII element) 2, 8, 8, 18, 18, 32, 32, 50, 50,
    72, 72, ...; let L be the list whose entries these are; then
    L[2*i-1] == 2 * (i+1)**2 == L[2*i].  Summing according to Noble(i)
    = Noble(i-1) + L[i] then suffices to determine the element numbers
    of the noble gasses.

    At least for natural n,
     * Noble(n) = 2*sum(((j+3)/2)**2 for j in range(n+1));
     * Noble(2*i) = 2 +4*sum(j**2 for j in range(2, i+2))
     * Noble(2*i+1) = Noble(2*i) +2*(i+2)**2

    Polynomial.PowerSum(2) tells me sum(i**2 for i in range(z)) is
    z*(z -1)*(2*z -1)/6, which is 2*1*3/6 = 1 at z = 2, so sum(j**2
    for j in range(2, i+2)) = (i+2)*(i+1)*(2*i+3)/6 -1, whence

     * Noble(2*i) = 2*(i+2)*(i+1)*(2*i+3)/3 -2
       = 4*i*i*i/3 +6*i*i +26*i/3 +2,
       = ((2*i+3)**3 -(2*i +3) -12)/6
     * Noble(2*i+1) = 4*i*i*i/3 +6*i*i +26*i/3 +2 +2*(i+2)**2
       = 4*i*i*i/3 +6*i*i +26*i/3 +2 +2*i*i +8*i +8
       = 4*i*i*i/3 +8*i*i +50*i/3 +10
       = (i+1)*((2*i+5)**2 +5)/3
       = ((2*i +4)**3 +2*(2*i +4) -12)/6

    In each case, with s = +2 if n+3 is even else -1, we have
     * Noble(n) = ((n +3)**3 +s*(n +3))/6 -2

    and the division by 6 is always exact, for whole n, since:
     * for odd m = n+3, m**3 -m is odd-odd = even
     * for even m = n+3, m**3 +2*m is even+even = even; and
     * mod 3, m**3 == m and either m-m or m+2*m is a multiple of 3.

    We can restate L's rule as L[n] = 2*((n+3)/2)**2, not to be
    confused with (n+3)**2 / 2, due to rounding rules.  Applied to
    negative n, this gives L[-1] == 2, L[-2] == 0 == L[-3], L[-4] ==
    L[-1] == 2 == L[0] == L[-5], L[-6] == L[1] == 8 == L[2] == L[-7];
    in general L[-5-n] == L[n] for whole n.  (Note that, due to
    rounding oddities, (1-i)/2 = -(i/2) for natural i.  So, for n >=
    -3, -((n+3)/2) = (-2-n)/2 = ((-5-n)+3)/2.  As lambda n: 5-n is
    self-inverse, the same holds for -5-n >= -3, i.e. n <= -2; hence
    for all whole n.)  With L[0] == 2 == Noble(0), using Noble(i-1) =
    Noble(i) -L[i], we can interpolate [Noble(-i) for i in naturals] =
    [2, 0, -2, -2, -2, -4, -6, -14, -22, ...], in agreement with the
    polynomials.\n"""

    m = n + 3
    return (m*m + (-1 if m % 2 else 2)) * m / 6 - 2

class Element (Substance):
    """Mixture of isotopes. """

    __upinit = Substance.__init__
    def __init__(self, name, symbol, Z, A, T=None, **what):
        """Initialise an Element. """
        self.__isotopes = {}
        if name is not None: what['name'] = name
        if symbol is not None: what['symbol'] = symbol
        what['atomic number'] = Z
        if A is not None: what['relative atomic mass'] = A
        if T is not None: what['temperature'] = T
        self.__upinit(**what)
        self.Z = Z
        if A is not None: self.A = A

        # Handle storage in Element.by* lookups:
        while len(Element.byNumber) <= Z: Element.byNumber.append(None)
        Element.bySymbol[self.symbol] = Element.byName[self.name] = Element.byNumber[Z] = self

        try: alias = what['alias']
        except KeyError: pass
        else:
            for sym in alias:
                if len(sym) < 3 or (len == 3 and # it looks like a pig latin name:
                                    sym == sym.capitalise() and sym[0] in 'UBTQPHSOE' and
                                    all(x in 'nubtqphsoe' for x in sym[1:])):
                    assert not Element.bySymbol.has_key(sym)
                    Element.bySymbol[sym] = self
                else:
                    assert not Element.byName.has_key(sym)
                    Element.byName[sym] = self

        try: alias = what['arcanum']
        except KeyError: pass
        else:
            assert not Element.byName.has_key(alias)
            Element.byName[alias] = self

        if Z > 100:
            # Elements 101 onwards have had pig latin names until IUPAC settles
            # all name disputes.
            if name is not None:
                sym = self._lazy_get_name_('name')
                Element.byName[sym] = self
            if symbol is not None:
                sym = self._lazy_get_symbol_('symbol')
                Element.bySymbol[sym] = self

    bySymbol, byName = {}, {}
    byNumber = [ None ] * 104

    def _lazy_get__nom_(self, ig,
                        latin=( 'nil', 'un', 'bi', 'tri', 'quad',
                                'pent', 'hex', 'sept', 'oct', 'enn' )):
        row, Z = (), self.Z
        while Z:
            Z, d = divmod(Z, 10)
            row = latin[d:d+1] + row
        return row

    def _lazy_get_name_(self, ig):
        name = ''.join(self._nom)
        if name[-1] == 'i': return name + 'um'
        return name + 'ium'

    def _lazy_get_symbol_(self, ig):
        return ''.join(x[0] for x in self._nom).capitalize()

    def __getitem__(self, key):
        try: ans = self.__isotopes[key]
        except KeyError:
            assert key == int(key)
            ans = Isotope(self.Z, int(key) - self.Z) # which calls __setitem__ for us
        return ans

    def __setitem__(self, key, value):
        assert key == int(key)
        try: old = self.__isotopes[key]
        except KeyError: self.__isotopes[key] = value
        else:
            if value is not old:
                raise ValueError(key, 'setting the same isotope repeatedly')

    def __len__(self): return len(self.__isotopes)
    def isotopes(self): return self.__isotopes.copy()
    def _lazy_get_composition_(self, ignored):
        bok = {}
        for iso in self.__isotopes.values():
            try:
                bun = iso.abundance
                if not bun: raise AttributeError
            except AttributeError: pass
            else: bok[iso] = bun
        return bok

    def _lazy_get_margin_(self, ignored):
        sum = count = 0
        for k, v in self.composition.items():
            sum, count = sum + v.best, count + (k.protons + k.neutrons) * v
        return self.A * sum / count - 1

    def __repr__(self): return self.name
    def __str__(self): return self.symbol

    def _lazy_get_atom_(self, ignored, Q=Quantity, S=Sample):
        # aggregate atom of isotopes, unless self has exactly one natural isotope.
        if len(self) == 1: return self.__isotopes.values()[0].atom
        bok, what, base = {}, {}, ()

        try: A = self.A
        except AttributeError: pass
        else:
            if isinstance(A, Q): A = A._scalar
            base = (A,) # use as weights if no bok; else as a base
            what['best'] = A

        for iso in self.__isotopes.values():
            n = iso.neutrons
            what['high'] = max(what.get('high', n), n)
            what['low']  = min(what.get('low',  n), n)
            try:
                bun = iso.abundance
                if not bun: raise AttributeError
            except AttributeError: pass
            else: bok[n] = bun

        if bok: base = (bok,) + base
        try: A = S(*base, **what)
        except TypeError:
            raise AttributeError("I don't know enough about myself to describe my atom", self)

        return atom(self.Z, A, self.name, self.symbol, "%s atom" % self.name)

    # Support for groups I through VIII:
    def _lazy_get_period_(self, ig, N=Noble):
        """Conventional Period number of the element.

        This places H and He in period 1, Li through Ne in period 2, and so on.\n"""

        i = 0
        while N(i) < self.Z: i += 1
        return i + 1

    from study.snake.lazy import Lazy
    class Group (Lazy):
        """A column of the periodic table of the elements.

        For my purposes, columns are numbered leftwards from 0 at group VIII,
        except that Groups I and II get negative values -1 and -2, respectively.
        See http://www.chaos.org.uk/~eddy/bits/elements.html for why.

        Groups I through VIII are named classically; each other column of the
        table reuses the name of its first element.  Each group has a .leader
        element, which is the first in the group (except that H is ignored).\n"""

        def __init__(self, number): self.__num, self.__seq = number, []
        def __repr__(self): return self.name

        def _lazy_get_name_(self, ig, N=Noble,
                            latin=('VIII', 'VII', 'VI', 'V', 'IV', 'III', 'II', 'I')):
            """Classical name, else symbol of first element in group"""
            i = self.__num
            if i < 6: return 'Group ' + latin[i]
            return self.leader.symbol + '-Group'

        def _lazy_get_leader_(self, ig, N=Noble):
            """First element in this group, ignoring H"""
            g = self.__num
            # Special case groups I through VIII:
            if g <= 0: return Element.byNumber[2 - g]
            # Deliberately use F as group I's leader, regardless of whether H is in it:
            if g < 6: return Element.byNumber[10 - g]

            # Otherwise, find the first period to include a member of this group.
            i = 2
            while 2 * i**2 < g + 2: i += 1
            # Period 2*j has length 2*(j+1)**2, so
            # the first member of this group is in period 2*(i-1);
            # N(2*(i-1) -1) marks the end of that period.
            assert i >= 3
            assert N(2*i -3) > N(2*i -4) + g + 2
            assert N(2*i -4) <= N(2*i -5) + g + 2
            return Element.byNumber[N(2*i -3) - g]

        def __getitem__(self, key, N=Noble):
            if key < 0: raise IndexError, key

            if not self.__seq: # first time
                if self.__num == 1 or self.__num == -1:
                    # Hydrogen is schizophrenic:
                    self.__seq.append(Element.byNumber[1])
                self.__seq.append(self.leader)

            P = self.__seq[-1].period
            if self.__num < 0: P -= 1
            while len(self.__seq) <= key:
                # this'll IndexError for us if we run out of elements:
                self.__seq.append(Element.byNumber[N(P) - self.__num])
                P += 1

            return self.__seq[key]

        # __getslice__ ? TODO: support slices as input to __getitem__.

        def __len__(self):
            try:
                i = len(self.__seq)
                while True:
                    self[i] # IndexError to break out of loop
                    i += 1
            except IndexError: pass
            return i

    del Lazy
    def Group(n, G=Group, cache=[None]*8):
        assert n > -3
        while len(cache) <= n + 2: cache.append(None)
        ans = cache[n+2]

        if ans is None:
            ans = cache[n+2] = G(n)

        return ans

    def _lazy_get_group_(self, ig, N=Noble, G=Group):
        P = self.period
        if P > 1:
            tail = self.Z - N(P-2)
            if tail < 3:
                assert tail > 0
                return G(-tail)

        return G(N(P-1) - self.Z)

    del Group
del Noble

Float, About = Quantity.fromDecimal, Quantity.within

def NASelement(name, symbol, Z, A, isos=None, abundance=None, melt=None, boil=None, density=None, **what):
    """Create an Element based on my Nuffield Advanced Data book's data.

    Required arguments:
      name -- string giving the full name of the element.
      symbol -- string giving the (one or two letter) symbol of the element.
      Z -- atomic number.

    Optional arguments:
      A -- molar mass * mol / g; a.k.a. atomic mass / AMU (default, None, means
           unknown).
      isos -- description of isotopes (see below); default is None.
      abundance -- relative terrestrial abundance, scaled to make Si score 100;
                   default is None, indicating an artificial element.
      melt -- melting temperature / K
      boil -- boiling temperature / K
      density -- in g/cc at 298K; or a tuple (d, T), d in g/cc at T as liquid

    plus any further keyword arguments, to taste.  See Element's constructor for
    further details; it receives a suitably scaled abundance.

    I have, rather arbitrarily, supposed that the phase change temperatures
    given in the NAS data book generally have an error bar of +/- half a Kelvin,
    except where marked as 'uncertain' (five K) or 'highly uncertain' (fifty K
    if the cited value's last two digits are zeros, otherwise ten K).  Roughly
    as arbitrarily, where several forms of the elment are listed, I've used the
    lowest melting point and highest boiling point, ignoring any phase changes
    between forms and listing any sublimation (typically relevant only to one
    form, not the one whose melting and boiling are used) separately as sublime.

    The description of isotopes, if given, should either be a list of atomic
    mass numbers for which an isotope is known (for artificial elements and
    those natural radioactives whose isotopic composition varies wildly) or a
    mapping from known atomic mass numbers to relative abundances (or to None
    for those radioactive isotopes which normally have negligible abundance).
    If the sum of these relative abundances isn't 1, they'll be (fudged to make
    it 100 - because the NAS book is a bit off on some elements - and then)
    scaled down to make it 1.

    Both the element's terrestrial abundance and the relative abundance of its
    isotopes will be given an error bar, if they don't already have one, to
    accord with the limited precision indicated in the NAS table. """

    if abundance is None: what['abundance'] = None # artificial elements
    else:
        try: abundance.width
        except AttributeError: # need an error bar (guess: two decimal places of precision)
            abundance = Quantity.fromSigFigs(abundance, 2)
        # NAS data book gives abundances relative to Silicon = 100, but notes
        # that Silicon's true abundance is believed to be 27.72 %
        what['abundance'] = abundance * .2772

    try: A.width
    except AttributeError: # give it an error bar
        try: isos[:] # radioactive/artificial elements
        except TypeError: A = Float(A, 4) # real ones
        else: A = About(A, .5 * max(1, max(isos) - min(isos)))

    if density is not None:
        if not isinstance(density, Quantity):
            try: density, temp = density
            except TypeError: temp = 298 * Kelvin
            else: temp *= Kelvin
            density = Float(density, 2, units=gram / cc, measured=temp)
        what['density'] = density

    temp = {}
    if melt is not None:
        try: melt.width
        except AttributeError: melt = Float(melt, 1)
        temp['melt'] = melt * Kelvin

    if boil is not None:
        try: boil.width
        except AttributeError: boil = Float(boil, 1)
        temp['boil'] = boil * Kelvin

    try: temp['sublime'] = what['sublime']
    except KeyError: pass
    else: del what['sublime']
    if temp: T = Temperatures(**temp)
    else: T = None

    ans = Element(name, symbol, Z, A, T, **what)

    try: isos[:]
    except TypeError:
        try: isos.update
        except AttributeError: pass # no information
        else:
            # dictionary: { isotope: relative abundance }
            weights = [ x for x in isos.values() if x ]
            total = sum(weights)
            if total == 1: fix, scale = None, 1 # weights given as fractions
            else:
                # Otherwise, assume given as percentages; but the NAS table
                # has several entries that don't sum accurately to 100.
                scale = .01
                if total == 100: fix = None
                else: # bodge: blur the non-tiny weights to make it all sum right ...
                    assert 80 < total < 120, "Perhaps these aren't percentages after all"
                    fix = 1 + (100 - total) * Quantity.below(2) \
                          / sum(x for x in weights if x > 1)

            # Perhaps we can improve on this ...
            for k, v in isos.items():
                iso = Isotope(Z, k - Z)
                if v:
                    unit = 1
                    if v < 1:
                        while unit > v: unit = unit * .1
                        unit = unit * .1
                    if fix and v > 1: v = v * fix # bodge
                    v = About(v, unit * .01)
                    iso.abundance = v * scale
    else:
        # sequence: known isotopes
        for k in isos:
            Isotope(Z, k - Z)

    return ans

atom(1, 0, 'Hydrogen', 'H', 'The simplest atom; the most abundant form of matter',
     mass=About(1673.43, .08, harpo * gram),
     nucleus=proton)
atom(1, 1, 'Deuterium', 'D', '(Ordinary) Heavy Hydrogen',
     nucleus=nucleus(1, 1, 'deuteron', doc="Deuterium's nucleus",
                     # roughly the sum of proton and neutron:
                     mass = 2.01355321271 * AMU,
                     # roughly the *difference* between proton and neutron:
                     magneticmoment = 0.433073457e-26 * Joule / Tesla))
atom(1, 2, 'Tritium', 'T', 'Radioactive Heavy Hydrogen')
atom(2, 2, 'Helium', 'He', 'Second most abundant form of matter',
     nucleus=nucleus(2, 2, 'alpha', doc="Helium's nucleus"))

Hydrogen = NASelement('Hydrogen', 'H', 1, Float(1.0079, 5), {1: 99.985, 2: .015, 3: None}, .57, 14, 20, (.07, 20))
Helium = NASelement('Helium', 'He', 2, 4.0026, {3: 1.3e-4, 4: 100}, 1.3e-6, boil=4, density=(.12, 4))
Lithium = NASelement('Lithium', 'Li', 3, 6.939, {6: 7.42, 7: 92.58}, 2.9e-2, 454, 1604, .53)
Beryllium = NASelement('Beryllium', 'Be', 4, 9.0122, {9: 1}, 2.6e-3, 1556, Float(2750, -1), 1.85)
Boron = NASelement('Boron', 'B', 5, About(10.811, 1.5e-3), {10: 19.7, 11: 80.3}, 1.3e-3, 2300, 4200, 2.55)
Carbon = NASelement('Carbon', 'C', 6, About(12.0111, 2.5e-5), {12: 98.89, 13: 1.11, 14: None},
                    .14, 3823, Float(5100, -1), 3.53, sublime=Centigrade(About(3700, 25)))
Nitrogen = NASelement('Nitrogen', 'N', 7, 14.0067, {14: 99.63, 15: .37}, 9e-2, 63, 77, (.81, 77))
Oxygen = NASelement('Oxygen', 'O', 8, Float(15.994, 4),
                    # Third most abundant atom in the universe
                    {16: 99.759, 17: .037, 18: .204}, 2.1e-2, 54, 90, (1.14, 90))
Fluorine = NASelement('Fluorine', 'F', 9, 18.9984, {19: 1}, .4, 53, 85, (1.11, 73))
Neon = NASelement('Neon', 'Ne', 10, About(20.183, 1.5e-3), {20: 90.92, 21: .26, 22: 8.82}, 3.1e-8, 25, 27, (1.21, 27))
Sodium = NASelement('Sodium', 'Na', 11, 22.9898, {23: 1}, 12.5, 371, 1163, .97, arcanum='Natrium')
Magnesium = NASelement('Magnesium', 'Mg', 12, 24.312, {24: 78.60, 25: 10.11, 26: 11.29}, 9.2, 923, 1390, 1.74)
Aluminium = NASelement('Aluminium', 'Al', 13, 26.9185, {27: 1}, 35.8, 932, 2720, 2.7, alias=('Aluminum',))
Aluminium[26].halflife = Float(.7, 2, 6, year,
                                cite="http://space.newscientist.com/article/dn11366-saturn-moons-mysterious-heat-traced-to-early-fever.html")
Silicon = NASelement('Silicon', 'Si', 14, Float(28.086, 3), {28: 92.18, 29: 4.71, 30: 3.12}, 100, 1683, Float(2950, -1), 2.33)
Phosphorus = NASelement('Phosphorus', 'P', 15, 30.9738, {31: 1}, 5.2, 317, 554, 1.82, sublime=704*Kelvin)
Sulphur = NASelement('Sulphur', 'S', 16, About(32.064, 1.5e-3), {32: 95, 33: .76, 34: 4.22, 36: .01}, .23, Float(392, -1), 718, 1.96, alias=('Sulfur',))
Chlorine = NASelement('Chlorine', 'Cl', 17, Float(35.453, 3), {35: 75.53, 37: 24.47}, .14, 172, 239, (1.56, 239))
Argon = NASelement('Argon', 'Ar', 18, 39.9480, {36: .34, 38: .063, 40: 99.6}, 1.8e-5, 84, 87, (1.4, 85), alias=('A',))
Potassium = NASelement('Potassium', 'K', 19, 39.102, {39: 93.22, 40: .12, 41: 6.77}, # components sum to 100.11, not 100
                       11.4, 336, 1039, .86, arcanum='Kalium')
Calcium = NASelement('Calcium', 'Ca', 20, 40.08, {40: 96.97, 42: .64, 43: .15, 44: 2.06, 46: .003, 48: .19}, 16, 1123, 1765, 1.55)
Scandium = NASelement('Scandium', 'Sc', 21, 44.956, {45: 1}, 2.2e-3, About(1673, 10), About(2750, 10), 2.99)
Titanium = NASelement('Titanium', 'Ti', 22, 47.9, {46: 7.99, 47: 7.32, 48: 73.99, 49: 5.46, 50: 5.25}, 1.4, 1950, 3550, 4.54)
Vanadium = NASelement('Vanadium', 'V', 23, 50.942, {50: .25, 51: 99.75}, 6.6e-2, 2190, 3650, 6.11)
Chromium = NASelement('Chromium', 'Cr', 24, Float(51.996, 3), {50: 4.31, 52: 83.76, 53: 9.55, 54: 2.38}, 4.4e-2, 2176, 2915, 7.19)
Manganese = NASelement('Manganese', 'Mn', 25, 54.938, {55: 1}, .44, 1517, 2314, 7.42)
Iron = NASelement('Iron', 'Fe', 26, Float(55.847, 3), {54: 5.84, 56: 91.68, 57: 2.17, 58: .31}, 22, 1812, 3160, 7.86, arcanum='Ferrum')
Cobalt = NASelement('Cobalt', 'Co', 27, 58.9332, {59: 1}, .01, 1768, 3150, 8.9)
Nickel = NASelement('Nickel', 'Ni', 28, 58.71, {58: 67.76, 60: 26.16, 61: 1.25, 62: 3.66, 64: 1.16}, 3.5e-2, 1728, 3110, 8.9)
Copper = NASelement('Copper', 'Cu', 29, Float(63.54, 3), {63: 69.1, 65: 30.9}, 3.1e-2, 1356, 2855, 8.94, arcanum='Cuprum', resistivity=Float(16.8, 1) * nano * Ohm * metre)
# resistivity: http://www.irregularwebcomic.net/3295.html
Zinc = NASelement('Zinc', 'Zn', 30, 65.37, {64: 48.89, 66: 27.81, 67: 4.11, 68: 18.56, 70: .62}, 5.8e-2, 693, 1181, 7.13)
Gallium = NASelement('Gallium', 'Ga', 31, 69.72, {69: 60.2, 71: 39.8}, 6.6e-3, 303, Float(2510, -1), 5.91)
Germanium = NASelement('Germanium', 'Ge', 32, 72.59, {70: 20.55, 72: 27.37, 73: 7.67, 74: 36.74, 76: 7.67}, 3.1e-3, 1210, Float(3100, -2), 5.32)
Arsenic = NASelement('Arsenic', 'As', 33, 74.9216, {75: 1}, 2.2e-3, sublime=Float(886, 1, None, Kelvin), density=5.73)
Selenium = NASelement('Selenium', 'Se', 34, 78.96, {74: .89, 76: 9.02, 77: 7.58, 78: 23.52, 80: 49.82, 82: 9.19}, 4e-5, 490, 958, 4.79)
Bromine = NASelement('Bromine', 'Br', 35, About(79.909, .001), {79: 50.52, 81: 49.48}, 7.1e-4, 266, 331, (3.12, 266))
Krypton = NASelement('Krypton', 'Kr', 36, 83.8, {78: .35, 80: 2.27, 82: 11.56, 83: 11.55, 84: 56.9, 86: 17.37}, 4.3e-8, 116, 120, (2.16, 120))
Rubidium = NASelement('Rubidium', 'Rb', 37, 85.47, {85: 72.15, 87: 27.85}, .14, 312, 974, 1.53)
Strontium = NASelement('Strontium', 'Sr', 38, 87.62, {84: .56, 86: 9.86, 87: 7.02, 88: 82.56}, .13, 1043, 1640, 2.58)
Yttrium = NASelement('Yttrium', 'Y', 39, 88.905, {89: 1}, 1.2e-2, About(1773, 10), Float(3500, -2), 4.4)
Zirconium = NASelement('Zirconium', 'Zr', 40, 91.22, {90: 51.46, 91: 11.23, 92: 17.11, 94: 17.4, 96: 2.8}, 9.7e-2, 2125, 4650, 6.53)
Niobium = NASelement('Niobium', 'Nb', 41, 92.9060, {93: 1}, 1.1e-2, 2770, 5200, 8.55, alias=('Columbium', 'Cb'))
Molybdenum = NASelement('Molybdenum', 'Mo', 42, 95.94, {92: 15.86, 94: 9.12, 95: 15.7, 96: 16.5, 97: 9.45, 98: 23.75, 100: 9.62}, 6.6e-3, Float(2890, -1), Float(5100, -1), 10.22)
Technetium = NASelement('Technetium', 'Tc', 43, 99, [99], None, Float(2400, -2), Float(4900, -2), 11.5)
Ruthenium = NASelement('Ruthenium', 'Ru', 44, 101.07, {96: 5.46, 98: 1.87, 99: 12.63, 100: 12.53, 101: 17.02, 102: 31.6, 104: 18.87}, 1.8e-6, Float(2700, -2), Float(4000, -2), 12.41)
Rhodium = NASelement('Rhodium', 'Rh', 45, 102.905, {103: 1}, 4.4e-7, 2239, Float(4000, -2), 12.41)
Palladium = NASelement('Palladium', 'Pd', 46, 106.4, {102: 1, 104: 11, 105: 22.2, 106: 27.3, 108: 26.7, 110: 11.8}, 4.4e-6, 1823, Float(3400, -1), 12.02)
Silver = NASelement('Silver', 'Ag', 47, About(107.87, 1.5e-3), {107: 51.35, 109: 48.65}, 4.4e-5, 1234, Float(2450, -1), 10.5, arcanum='Argentum')
Cadmium = NASelement('Cadmium', 'Cd', 48, 112.4, {106: 1.22, 108: .88, 110: 12.39, 111: 12.75, 112: 24.07, 113: 12.26, 114: 28.86, 116: 7.58}, 6.6e-5, 594, 1038, 8.65)
Indium = NASelement('Indium', 'In', 49, 114.82, {113: 4.23, 115: 95.77}, 4.4e-5, 429, About(2320, 10), 7.31)
Tin = NASelement('Tin', 'Sn', 50, 118.69, {112: .95, 114: .65, 115: .34, 116: 14.24, 117: 7.57, 118: 24.01, 119: 8.58, 120: 32.97, 122: 4.71, 124: 5.98}, 1.8e-2, 505, Float(2960, -1), 7.31, arcanum='Stannum')
Antimony = NASelement('Antimony', 'Sb', 51, 121.7550, {121: 57.25, 123: 42.75}, 4.4e-4, 903, 1910, 6.68, arcanum='Stibium')
Tellurium = NASelement('Tellurium', 'Te', 52, 127.6, {120: .09, 122: 2.46, 123: .87, 124: 4.61, 125: 6.99, 126: 18.71, 128: 31.79, 130: 34.49}, 8.8e-7, 723, 1260, 6.25)
Iodine = NASelement('Iodine', 'I', 53, 126.9044, {127: 1}, 1.3e-4, 387, 456, 4.94)
Xenon = NASelement('Xenon', 'Xe', 54, 131.3,
                   {124: .013, 126: .09, 128: 1.92, 129: 26.44, 130: 4.08, 131: 21.18, 132: 26.89, 134: 10.4, 136: 8.87}, # components sum to 99.883, not 100
                   5.3e-10, 161, 165, (3.52, 164))
Caesium = NASelement('Caesium', 'Cs', 55, 132.905, {133: 1}, 3.1e-3, 302, 958, 1.87, alias=('Cesium',))
Barium = NASelement('Barium', 'Ba', 56, 137.34, {130: .101, 132: .097, 134: 2.42, 135: 6.59, 136: 7.81, 137: 11.32, 138:71.66}, .57, 983, 1910, 3.5)
Lanthanum = NASelement('Lanthanum', 'La', 57, 138.91, {138: .09, 139: 99.91}, 8.1e-3, 1193, 3640, 6.19)
Cerium = NASelement('Cerium', 'Ce', 58, 140.12, {136: .193, 138: .23, 140: 88.48, 142: 11.07}, .02)
Praseodymium = NASelement('Praseodymium', 'Pr', 59, 140.907, {141: 1}, 2.4e-3)
Neodymium = NASelement('Neodymium', 'Nd', 60, 144.24, {142: 27.13, 143: 12.2, 144: 23.87, 145: 8.29, 146: 17.18, 148: 5.72, 150: 5.6}, 1.1e-2)
Promethium = NASelement('Promethium', 'Pm', 61, 145, [145])
Samarium = NASelement('Samarium', 'Sm', 62, 150.35, {144: 3.16, 147: 15.07, 148: 11.27, 149: 13.82, 150: 7.47, 152: 26.63, 154: 22.53}, 2.8e-3)
Europium = NASelement('Europium', 'Eu', 63, 151.96, {151: 47.77, 153: 52.23}, 4.7e-4)
Gadolinium = NASelement('Gadolinium', 'Gd', 64, 157.25, {152: .2, 154: 2.15, 155: 14.7, 156: 20.47, 157: 15.68, 158: 24.9, 160: 21.9}, 2.1)
Terbium = NASelement('Terbium', 'Tb', 65, 158.924, {159: 1}, 4e-4)
Dysprosium = NASelement('Dysprosium', 'Dy', 66, 162.5, {156: .05, 158: .09, 160: 2.29, 161: 18.88, 162: 25.53, 163: 24.97, 164: 28.18}, 2e-3)
Holmium = NASelement('Holmium', 'Ho', 67, 164.93, {165: 1}, 5.1e-4)
Erbium = NASelement('Erbium', 'Er', 68, 167.26, {162: .14, 164: 1.56, 166: 33.41, 167: 22.94, 168: 27.07, 170: 14.88}, 1.1e-3)
Thulium = NASelement('Thulium', 'Tm', 69, 168.934, {169: 1}, 8.8e-5)
Ytterbium = NASelement('Ytterbium', 'Yb', 70, 173.04, {168: .14, 170: 3.03, 171: 14.31, 172: 21.82, 173: 16.13, 174: 31.84, 176: 12.73}, 1.2e-3)
Lutetium = NASelement('Lutetium', 'Lu', 71, 174.97, {175: 97.4, 176: 2.6}, 3.3e-4)
Hafnium = NASelement('Hafnium', 'Hf', 72, 178.49, {174: .16, 176: 5.21, 177: 18.56, 178: 27.1, 179: 13.75, 180: 35.22}, 2e-3, Float(2495, -1), Float(5500, -1), 13.3)
Tantalum = NASelement('Tantalum', 'Ta', 73, 180.948, {180: .01, 181: 99.99}, 9.2e-4, 3270, Float(5700, -2), 16.6)
Tungsten = NASelement('Tungsten', 'W', 74, 183.85, {180: .14, 182: 26.4, 183: 14.4, 184: 30.6, 186: 28.4}, 3e-2, 3650, Float(5800, -2), 19.35, arcanum='Wolfram')
Rhenium = NASelement('Rhenium', 'Re', 75, 186.2, {185: 37.07, 187: 62.93}, 4.4e-8, 3453, Float(5900, -2),21.02)
Osmium = NASelement('Osmium', 'Os', 76, 190.2,
                    {188: 13.3, 189: 16.1, 190: 26.4, 192: 41}, # components sum to 96.8, not 100
                    2.2e-6, Float(3000, -2), Float(4500, -2), 22.57)
Iridium = NASelement('Iridium', 'Ir', 77, 192.2, {191: 38.5, 193: 61.5}, 4.4e-7, 2727, Float(4400, -2),
                     22.42)
Platinum = NASelement('Platinum', 'Pt', 78, 195.09,
                      {190: .01, 192: .78, 194: 32.9, 195: 33.8, 196: 25.2, 198: 7.2}, # components sum to 99.89, not 100
                      2.2e-6, 2043, Float(4100, -2), 21.45)
Gold = NASelement('Gold', 'Au', 79, 196.967, {197: 1}, 2.2e-6, 1336, About(2980, 10), 19.32, arcanum='Aurum')
Mercury = NASelement(
    'Mercury', 'Hg', 80, 200.592,
    {196: .15, 198: 10.02, 199: 16.84, 200: 23.13, 201: 13.22, 202: 29.80, 204: 6.85},
    2.2e-4, Float(234.3, 1), Float(629.7, 1),
    arcanum='Hydrargyrum', alias=('Quick-silver', 'Quicksilver'),
    heat = Heats(melt = Float(2.29, 2, None, kilo * Joule / mol),
                 boil = Float(59.11, 2, None, kilo * Joule / mol),
                 capacity = Float(27.953, 3, None, Joule / mol / Kelvin,
                                  at = Centigrade(25))),
    density = Float(13595.1, 1, None, kg / metre**3,
                    """Density of merucry.

This is equivalently Atmosphere / .76 / metre / Earth.surface.g, since a 76 cm
column of mercury balances one atmosphere's pressure.  (Note that Eart.surface.g
is equivalently m/m.weight for any mass m.)

I have also seen its value given (using the British gallon) as 136.26 lb /
gallon, which conflicts with the value given here (135.9 pound / gallon).  The
NAS table used for most other densities gives 13.53 g/cc at 298K, also
conflicting with the 13.595 g/cc here.
"""),
    __doc__ = """Mercury

http://www.du.edu/~jcalvert/phys/mercury.htm
says:

  The name hydrargyrum, 'water silver', was given by Pliny from Greek roots for
  the common name, and is the source of its chemical symbol, Hg. In German, it
  is called Quecksilber, from its usual ancient name, and in French it is
  mercure, from which the English 'mercury' is derived. This, no doubt, comes
  from the fancies of late medieval alchemy, where it was represented by the
  symbol for the god Mercury, ...

and provides lots of further (but, in places, inconsistent) physical data.
""")
Mercury.density.observe(torr * tonne / kg.weight / metre)
Thallium = NASelement('Thallium', 'Tl', 81, 204.37, {203: 29.5, 205: 70.5}, 1.3e-3, 577, 1740, 11.85)
Lead = NASelement('Lead', 'Pb', 82, 207.19,
                  {202: .5, 204: 1.4, 206: 25.1, 207: 21.7, 208: 52.3}, # components sum to 101, not 100
                  7e-3, 601, 2024, 11.35, arcanum='Plumbum')
Bismuth = NASelement('Bismuth', 'Bi', 83, 208.98, {209: 1}, 8.8e-5, 545, 1832, 9.75)
Polonium = NASelement('Polonium', 'Po', 84, 210, {210: 1}, .13, 527, 1235, 9.32)
Astatine = NASelement('Astatine', 'At', 85, 210, [206, 215], None, About(575, 10), Float(650, -1))
Radon = NASelement('Radon', 'Rn', 86, 222, [222, 220], None, About(202, 10), About(211, 10), (4.4, 211), alias=('Emanation', 'Em'))
Francium = NASelement('Francium', 'Fr', 87, 223, [223], None, Float(300, -2), About(950, 10))
Radium = NASelement('Radium', 'Ra', 88, 226.05, [226, 228, 224, 223], 5.7e-9, 973, Float(1800, -2), 5)
Actinium = NASelement('Actinium', 'Ac', 89, 227, [227, 228], 1.3e-15, About(1470, 10), Float(3600, -2), 10.07)
Thorium = NASelement('Thorium', 'Th', 90, 232.038, {230: 0, 232: 1}, 5.1e-3, 1968, Float(4500, -2), 11.66)
Protactinium = NASelement('Protactinium', 'Pa', 91, 231, {231: 1}, 3.5e-10, 1500, 4300, 15.37)
Uranium = NASelement('Uranium', 'U', 92, 238.03, {234: .0057, 235: .7196, 238: 99.276}, 1.8e-3, 1406, 4200, 18.95)
Neptunium = NASelement('Neptunium', 'Np', 93, 237, [237, 239], None, 913, 3500)
Plutonium = NASelement('Plutonium', 'Pu', 94, 242, [238, 239, 242], 19.84)
Americium = NASelement('Americium', 'Am', 95, 243, [243])
Curium = NASelement('Curium', 'Cm', 96, 247, [247])
Berkelium = NASelement('Berkelium', 'Bk', 97, 249, [249])
Californium = NASelement('Californium', 'Cf', 98, 251, [251])
Einsteinium = NASelement('Einsteinium', 'Es', 99, 254, [254])
Fermium = NASelement('Fermium', 'Fm', 100, 253, [253])
Mendelevium = NASelement('Mendelevium', 'Md', 101, 256, [256])
Nobelium = NASelement('Nobelium', 'No', 102, 254, [254])
Lawrencium = NASelement('Lawrencium', 'Lr', 103, 257, [257])
Rutherfordium = NASelement('Rutherfordium', 'Rf', 104, 261, [ 260 ],
                           alias=('Kurchatovium', 'Ku'))
Dubnium = NASelement('Dubnium', 'Db', 105, 262, [ 261 ])
Seaborgium = NASelement('Seaborgium', 'Sg', 106, 266, [ 263, 266 ])
Bohrium = NASelement('Bohrium', 'Bh', 107, 264, [ 262, 266, 267 ])
Hassium = NASelement('Hassium', 'Hs', 108, 269, [ 265 ])
Meitnerium = NASelement('Meitnerium', 'Mt', 109, 268, [ 266 ])
Darmstadtium = NASelement('Darmstadtium', 'Ds', 110, 281, [ 269, 271 ])
Roentgenium = NASelement('Roentgenium', 'Rg', 111, 272, [ 272 ])
# Elements 11[2-68] have also been reported, with decays
# 118 (.9 ms, 2006) alpha -> 116 alpha -> 114 -> ?
# 115 (90 ms, 2004) -> 113 (1.2 s) -> ?

# and a few synonyms ...
protium = Hydrogen[1].nominate('protium', 'p')
Deuterium = Hydrogen[2].nominate('Deuterium', 'D') # abundance 8.5e-5 (relative to Si = 100)
Tritium = Hydrogen[3].nominate('Tritium', 'T')
Ionium = Thorium[230].nominate('Ionium', 'Io')
Thoron = Radon[220].nominate('Thoron', 'Tn')

deuteron = Deuterium.atom.nucleus
triton = Tritium.atom.nucleus
alpha = Helium[4].atom.nucleus

# Nuclear reactions in stars (nucleosynthesis):
# http://web.missouri.edu/~speckan/witch-stuff/Research/chapter2/node8.html
# (With some adjustments, suggested to author as possibly corrections.)
# Main sequence, temperature > 12 MK:
#  p-p chain a: 
#   H[1] + H[1] -> positron + neutrino + H[2]
#   H[2] + H[1] -> gamma + He[3]
#   He[3] + He[3] -> H[1] + H[1] + He[4]
#  p-p chain b:
#   He[3] + He[4] -> gamma + Be[7]
#   Be[7] + electron -> Li[7] + neutrino
#   Li[7] + H[1] -> He[4] + He[4]
#  p-p chain c:
#   Be[7] + "&mu;" -> gamma + B[8]
#   B[8] -> positron + neutrino + Be[8]
#   Be[8] -> He[4] + He[4]
#  C-N cycle (dominates for stellar mass > about 1.2 * Sun.mass):
#   N[15] + H[1] -> He[4] + C[12]
#   C[12] + H[1] -> gamma + N[13]
#   N[13] -> positron + neutrino + C[13] # sole source of C[13]
#   C[13] + H[1] -> gamma + N[14]
#   N[14] + H[1] -> gamma + O[15]
#   O[15] -> positron + neutrino + N[15]
#  Extending that to the C-N-O cycle:
#   N[15] + H[1] -> gamma + O[16]
#   O[16] + H[1] -> gamma + F[17]
#   F[17] -> positron + neutrino + O[17]
#   O[17] + H[1] -> He[4] + N[14]
#  O-F extras:
#   O[17] + H[1] -> gamma + F[18]
#   F[18] -> positron + neutrino + O[18]
#   O[18] + H[1] -> He[4] + N[15]
#   O[18] + H[1] -> gamma + F[19]
#   F[19] + H[1] -> He[4] + O[16]
# http://web.missouri.edu/~speckan/witch-stuff/Research/chapter2/node12.html
# The core Helium burning (CHeB) phase, which ends the red giant phase:
#  Triple-alpha process, temperature > c. 100 MK)
#   He[4] + He[4] -> Be[8] # NB: Be[8] has a half-life of c. 7e-16s, limiting next step !
#   Be[8] + He[4] -> C[12](excited)
#   C[12](excited) -> gamma + gamma + C[12]
#  This can continue as:
#   C[12] + He[4] -> gamma + O[16]
#   O[16] + He[4] -> gamma + Ne[20]
#   Ne[20] + He[4] -> gamma + Mg[24]
#  The C-N cycle leaves N[14] from which to seed:
#   N[14] + He[4] -> gamma + F[18] # thence to O[18] as above
#   O[18] + He[4] -> gamma + Ne[22]
# Asymptotic Giant Branch phase; He exhausted, second red-giant phase:
#  H-burning shell (outside He-shell):
#   p-p chains and C-N cycle, as earlier
#  He-burning shell (s-process actions on p-p and C-N products):
#   C[13] + He[4] -> neutron + O[16]
#   Ne[22] + He[4] -> neutron + Mg[25]
#   neutron-addition then pushes elements above Fe[56] (s(low)-process and r(apid)-process).
#  Extra C[13] production (see sole source, above) when H[1] gets into the
#  C-rich core; and "hot bottom burning" (stars with mass > Sun.mass * 4, T > 50
#  MK in H-shell) does more.

del Object, Sample, Float, About, kilo, nano, harpo, \
    Joule, Tesla, Ohm, Kelvin, Centigrade, gram, kg, tonne, metre, mol, torr, \
    cc, year, Particle, Boson, Fermion, AMU, NASelement
