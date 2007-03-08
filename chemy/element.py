"""Repository of data about the chemical elements and their isotopes.

The problem: we have two paths for isotopic mixing.  That is: an element's
composition describes it as a mix of atoms of various isotopes, each of which
has a composition comprising a definite number of neutrons, along with the
appropriate number of protons and electrons; but equally, e.g. from the point of
view of a chemical species (via its molecule), the element's atom has a definite
arrangement of electrons around a nucleus which is a mixture of possible
variants.  I chose to take the former description seriously and fake the latter.

chemistry.mercury will need tweaked; it references atom.

See also: basEddy.elements !
"""

_rcs_id_ = """
$Id: element.py,v 1.2 2007-03-08 23:35:42 eddy Exp $
"""
from particle import *

class Nucleus (Particle): _namespace = 'Nucleus.item'
class bNucleus (Boson, Nucleus): 'Bosonic nucleus'
class fNucleus (Fermion, Nucleus): 'Fermionic nucleus'
def nucleus(Q, N, name, **what):
    what.update({'name': name,
                 'constituents': {Nucleon.item.proton: Q, Nucleon.item.neutron: N}})

    try: klaz = {0: bNucleus, 1: fNucleus}[(Q + N) % 2]
    except KeyError: klaz = Nucleus
    return apply(klaz, (), what)

class Atom (Particle):
    _namespace = 'Atom.item'
class bAtom (Boson, Atom): 'Bosonic atom'
class fAtom (Fermion, Atom): 'Fermionic atom'
def atom(Q, N, name, symbol, doc, **what):
    ndoc = "%s %d's nucleus" % (name, Q+N)
    try: n = what['nucleus']
    except KeyError: n = what['nucleus'] = nucleus(Q, N, ndoc)
    else:
        try:
            if not n.__dict__['__doc__']: raise KeyError
        except KeyError: n.__doc__ = ndoc

    what.update({'name': name, 'symbol': symbol, 'doc': doc,
                 'constituents': {n: 1, Lepton.item.electron: Q}})
    try: klaz = {0: bAtom, 1: fAtom}[N % 2]
    except KeyError: klaz = Atom
    return apply(klaz, (), what)

atom(1, 0, 'Hydrogen', 'H', 'The simplest atom; the most abundant form of matter',
     mass=Quantity(sample(1673.43, .08), harpo * gram),
     nucleus=Nucleon.item.proton)
atom(1, 1, 'Deuterium', 'D', '(Ordinary) Heavy Hydrogen',
     nucleus=nucleus(1, 1, 'deuteron', doc="Deuterium's nucleus",
                     # roughly the sum of proton and neutron:
                     mass = 2.01355321271 * AMU,
                     # roughly the *difference* between proton and neutron:
                     magneticmoment = 0.433073457e-26 * Joule / Tesla))
atom(1, 2, 'Tritium', 'T', 'Radioactive Heavy Hydrogen')
atom(2, 2, 'Helium', 'He', 'Second most abundant form of matter',
     nucleus=nucleus(2, 2, 'alpha', doc="Helium's nucleus"))


class Isotope (Object):
    """Single actual isotope.

    Well, OK, if constructor's N is a distribution, you get a distribution.
    But my intent is that we don't do that; though we might do the equivalent for
    a Nucleus, for the sake of an Element's atom.
    """

    __upinit = Object.__init__
    def __init__(self, Q, N):
        self.__names = () # names displaced by uses of .nominate()
        assert type(Q) is type(1) is type(N)
        self.__upinit(protons=Q, neutrons=N)

    def nominate(self, name, symbol):
        Element.bysymbol[symbol] = Element.byname[name] = self
        # So we record the attempt even if we fail to put it into effect ...

        if self.__names: self.__names.append(self.__name)
        else: self.__names = [ self.symbol, self.name ]
        self.__names.insert(0, self.symbol)

        try: del self.name
        except AttributeError: pass
        self.__name, self.symbol = name, symbol

    def _lazy_get_name_(self, ignored):
        if self.__names: return '%s (a.k.a. %s)' % (self.__name, ', '.join(self.__names))
        return '%s[%s]' % (self.element.name, self.massnumber)

    def _lazy_get_symbol_(self, ignored):
        return '%s[%s]' % (self.element.symbol, self.massnumber)

    def _lazy_get_fullsymbol_(self, ignored):
        return '<sup>%d</sup><sub>%d</sub>%s' % (self.massnumber, self.protons, self.electron.symbol)

    def _lazy_get__lazy_hash_(self, ignored):
        return hash(self.__class__) ^ hash(self.protons) ^ hash(self.neutrons)

    def _lazy_get_nucleus_(self, ignored):
        return Nucleus('%s<sup>+%d</sup>' % (self.symbol, self.protons),
                       doc='Nucleus of the %s atom' % self.name,
                       constituents={proton: self.protons, neutron: self.neutrons})

    def _lazy_get_constituents_(self, ignored):
        return Particle(self.name, self, constituents={self.nucleus: 1, electron: self.electrons}).constituents

    def _lazy_get_electrons_(self, ignored): return self.protons
    def _lazy_get_massnumber_(self, ignored): return self.protons + self.neutrons
    def _lazy_get_element_(self, ignored): return Element.bynumber[self.protons]

    def __repr__(self): return self.name
    def __str__(self): return self.symbol

all = {}
def Isotope(Q, N, klaz=Isotope, rack=all):
    try: ans = rack[(Q,N)]
    except KeyError: ans = rack[(Q,N)] = klaz(Q, N)
    return ans

Isotopes = all.values
naturalIsotopes = lambda : filter(lambda x: getattr(x, 'abundance', None), Isotopes())
del all

class Element (Object):
    """Mixture of isotopes.
    """

    __upinit = Object.__init__
    def __init__(self, name, symbol, Z, A, **what):
        """Initialise an Element.
        """
        self.__isotopes = {}
        what.update({'name': name, 'symbol': symbol,
                     'atomic number': Z, 'relative atomic mass': A})
        apply(self.__upinit, (), what)
        self.Z, self.A = Z, A

        # Handle storage in Element.by* lookups:
        while len(Element.bynumber) <= Z: Element.bynumber.append(None)
        Element.bysymbol[symbol] = Element.byname[name] = Element.bynumber[Z] = self

        try: alias = what['alias']
        except KeyError: pass
        else:
            for sym in alias:
                if len(sym) < 3:
                    assert not Element.bysymbol.has_key(sym)
                    Element.bysymbol[sym] = self
                else:
                    assert not Element.byname.has_key(sym)
                    Element.byname[sym] = self

        try: alias = what['arcanum']
        except KeyError: pass
        else:
            assert not Element.byname.has_key(alias)
            Element.byname[alias] = self

    bysymbol, byname = {}, {}
    bynumber = [ None ] * 104

    def __getitem__(self, key):
        try: ans = self.__isotopes[key]
        except KeyError:
            assert key == int(key)
            ans = self.__isotopes[key] = Isotope(self.Z, int(key) - self.Z)
        return ans

    def __setitem__(self, key, value):
        assert key == int(key)
        try: old = self.__isotopes[key]
        except KeyError: self.__isotopes[key] = value
        else:
            if value is not old:
                raise ValueError(key, 'setting the same isotope repeatedly')

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

    def _lazy_get_atom_(self, ignored):
        # aggregate atom of isotopes, unless self has exactly one natural isotope.
        pass

def NASelement(name, symbol, Z, A, isos=None, abundance=None, **what):
    """Create an Element based on my Nuffield Advanced Data book's data.

    Required arguments:
      name -- string giving the full name of the element.
      symbol -- string giving the (one or two letter) symbol of the element.
      Z -- atomic number.
      A -- molar mass * mol / g; a.k.a. atomic mass / AMU.

    Optional arguments:
      isos -- description of isotopes (see below); default is None.
      abundance -- relative terrestrial abundance, scaled to make Si score 100;
      default is None, indicating an artificial element.

    plus any further keyword arguments, to taste.  See Element's constructor for
    further details; it receives a suitably scaled abundance.

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
        except AttributeError: # better give it an error bar
            unit = 1
            while unit > abundance: unit = unit * .1
            abundance = abundance + tophat * unit * .1
        # NAS data book gives abundances relative to Silicon = 100, but notes
        # that Silicon's true abundance is believed to be 27.72 %
        what['abundance'] = abundance * 2.772e-3

    try: A.width
    except AttributeError: # give it an error bar
        try: isos[:] # radioactive elements
        except TypeError: A = A + tophat * .0001 # real ones
        else: A = A + tophat * max(1, max(isos) - min(isos))

    ans = apply(Element, (name, symbol, Z, A), what)

    try: isos[:]
    except TypeError:
        try: isos.update
        except AttributeError: pass
        else:
            # dictionary
            weights = filter(None, isos.values())
            total = reduce(lambda x,y: x+y, weights, 0.)
            # The NAS table has several entries that don't sum accurately to 100.
            if total == 1: fix, scale = None, 1
            else:
                scale = .01
                if total == 100: fix = None
                else: # bodge: blur the non-tiny weights to make it all sum right ...
                    fix = 1 + (100 - total) * (tophat + .5) * 2 \
                          / reduce(lambda x,y: x+y, filter(lambda x: x > 1, weights), 0)

            for k, v in isos.items():
                ans[k] = Isotope(Z, k - Z)
                if v:
                    unit = 1
                    if v < 1:
                        while unit > v: unit = unit * .1
                        unit = unit * .1
                    if fix is not None and v > 1: v = v * fix # bodge
                    v = v + unit * tophat * .01
                    ans[k].abundance = v * scale
    else:
        # sequence
        for k in isos:
            ans[k] = Isotope(Z, k - Z)

    return ans

Hydrogen = NASelement('Hydrogen', 'H', 1, 1.0079 + 1e-5 * tophat, {1: 99.985, 2: .015, 3: None}, .57)
Helium = NASelement('Helium', 'He', 2, 4.0026, {3: 1.3e-4, 4: 100}, 1.3e-6)
Lithium = NASelement('Lithium', 'Li', 3, 6.939, {6: 7.42, 7: 92.58}, 2.9e-2)
Beryllium = NASelement('Beryllium', 'Be', 4, 9.0122, {9: 1}, 2.6e-3)
Boron = NASelement('Boron', 'B', 5, 10.811 + 3e-3 * tophat, {10: 19.7, 11: 80.3}, 1.3e-3)
Carbon = NASelement('Carbon', 'C', 6, 12.0111 + 5e-5 * tophat, {12: 98.89, 13: 1.11, 14: None},
                    .14, sublime=Centigrade(3700))
Nitrogen = NASelement('Nitrogen', 'N', 7, 14.0067, {14: 99.63, 15: .37}, 9e-2)
Oxygen = NASelement('Oxygen', 'O', 8, 15.994 + 1e-4 * tophat, {16: 99.759, 17: .037, 18: .204}, 2.1e-2)
Fluorine = NASelement('Fluorine', 'F', 9, 18.9984, {19: 1}, .4)
Neon = NASelement('Neon', 'Ne', 10, 20.183 + 3e-3 * tophat, {20: 90.92, 21: .26, 22: 8.82}, 3.1e-8)
Sodium = NASelement('Sodium', 'Na', 11, 22.9898, {23: 1}, 12.5, arcanum='Natrium')
Magnesium = NASelement('Magnesium', 'Mg', 12, 24.312, {24: 78.60, 25: 10.11, 26: 11.29}, 9.2)
Aluminium = NASelement('Aluminium', 'Al', 13, 26.9185, {27: 1}, 35.8, alias=('Aluminum',))
Silicon = NASelement('Silicon', 'Si', 14, 28.086 + 1e-3 * tophat, {28: 92.18, 29: 4.71, 30: 3.12}, 100)
Phosphorus = NASelement('Phosphorus', 'P', 15, 30.9738, {31: 1}, 5.2)
Sulphur = NASelement('Sulphur', 'S', 16, 32.064 + 3e-3 * tophat, {32: 95, 33: .76, 34: 4.22, 36: .01}, .23, alias=('Sulfur',))
Chlorine = NASelement('Chlorine', 'Cl', 17, 35.453 + 1e-3 * tophat, {35: 75.53, 37: 24.47}, .14)
Argon = NASelement('Argon', 'Ar', 18, 39.9480, {36: .34, 38: .063, 40: 99.6}, 1.8e-5, alias=('A',))
Potassium = NASelement('Potassium', 'K', 19, 39.102, {39: 93.22, 40: .12, 41: 6.77},
                       11.4, arcanum='Kalium') # components sum to 100.11, not 100
Calcium = NASelement('Calcium', 'Ca', 20, 40.08, {40: 96.97, 42: .64, 43: .15, 44: 2.06, 46: .003, 48: .19}, 16)
Scandium = NASelement('Scandium', 'Sc', 21, 44.956, {45: 1}, 2.2e-3)
Titanium = NASelement('Titanium', 'Ti', 22, 47.9, {46: 7.99, 47: 7.32, 48: 73.99, 49: 5.46, 50: 5.25}, 1.4)
Vanadium = NASelement('Vanadium', 'V', 23, 50.942, {50: .25, 51: 99.75}, 6.6e-2)
Chromium = NASelement('Chromium', 'Cr', 24, 51.996 + 1e-3 * tophat, {50: 4.31, 52: 83.76, 53: 9.55, 54: 2.38}, 4.4e-2)
Manganese = NASelement('Manganese', 'Mn', 25, 54.938, {55: 1}, .44)
Iron = NASelement('Iron', 'Fe', 26, 55.847 + 3e-3 * tophat, {54: 5.84, 56: 91.68, 57: 2.17, 58: .31}, 22, arcanum='Ferrum')
Cobalt = NASelement('Cobalt', 'Co', 27, 58.9332, {59: 1}, .01)
Nickel = NASelement('Nickel', 'Ni', 28, 58.71, {58: 67.76, 60: 26.16, 61: 1.25, 62: 3.66, 64: 1.16}, 3.5e-2)
Copper = NASelement('Copper', 'Cu', 29, 63.54 + 1e-3 * tophat, {63: 69.1, 65: 30.9}, 3.1e-2, arcanum='Cuprum')
Zinc = NASelement('Zinc', 'Zn', 30, 65.37, {64: 48.89, 66: 27.81, 67: 4.11, 68: 18.56, 70: .62}, 5.8e-2)
Gallium = NASelement('Gallium', 'Ga', 31, 69.72, {69: 60.2, 71: 39.8}, 6.6e-3)
Germanium = NASelement('Germanium', 'Ge', 32, 72.59, {70: 20.55, 72: 27.37, 73: 7.67, 74: 36.74, 76: 7.67}, 3.1e-3)
Arsenic = NASelement('Arsenic', 'As', 33, 74.9216, {75: 1}, 2.2e-3)
Selenium = NASelement('Selenium', 'Se', 34, 78.96, {74: .89, 76: 9.02, 77: 7.58, 78: 23.52, 80: 49.82, 82: 9.19}, 4e-5)
Bromine = NASelement('Bromine', 'Br', 35, 79.909 +.002 * tophat, {79: 50.52, 81: 49.48}, 7.1e-4)
Krypton = NASelement('Krypton', 'Kr', 36, 83.8, {78: .35, 80: 2.27, 82: 11.56, 83: 11.55, 84: 56.9, 86: 17.37}, 4.3e-8)
Rubidium = NASelement('Rubidium', 'Rb', 37, 85.47, {85: 72.15, 87: 27.85}, .14)
Strontium = NASelement('Strontium', 'Sr', 38, 87.62, {84: .56, 86: 9.86, 87: 7.02, 88: 82.56}, .13)
Yttrium = NASelement('Yttrium', 'Y', 39, 88.905, {89: 1}, 1.2e-2)
Zirconium = NASelement('Zirconium', 'Zr', 40, 91.22, {90: 51.46, 91: 11.23, 92: 17.11, 94: 17.4, 96: 2.8}, 9.7e-2)
Niobium = NASelement('Niobium', 'Nb', 41, 92.9060, {93: 1}, 1.1e-2, alias=('Columbium', 'Cb'))
Molybdenum = NASelement('Molybdenum', 'Mo', 42, 95.94, {92: 15.86, 94: 9.12, 95: 15.7, 96: 16.5, 97: 9.45, 98: 23.75, 100: 9.62}, 6.6e-3)
Technetium = NASelement('Technetium', 'Tc', 43, 99, [99])
Ruthenium = NASelement('Ruthenium', 'Ru', 44, 101.07, {96: 5.46, 98: 1.87, 99: 12.63, 100: 12.53, 101: 17.02, 102: 31.6, 104: 18.87}, 1.8e-6)
Rhodium = NASelement('Rhodium', 'Rh', 45, 102.905, {103: 1}, 4.4e-7)
Palladium = NASelement('Palladium', 'Pd', 46, 106.4, {102: 1, 104: 11, 105: 22.2, 106: 27.3, 108: 26.7, 110: 11.8}, 4.4e-6)
Silver = NASelement('Silver', 'Ag', 47, 107.87 + 3e-3 * tophat, {107: 51.35, 109: 48.65}, 4.4e-5, arcanum='Argentum')
Cadmium = NASelement('Cadmium', 'Cd', 48, 112.4, {106: 1.22, 108: .88, 110: 12.39, 111: 12.75, 112: 24.07, 113: 12.26, 114: 28.86, 116: 7.58}, 6.6e-5)
Indium = NASelement('Indium', 'In', 49, 114.82, {113: 4.23, 115: 95.77}, 4.4e-5)
Tin = NASelement('Tin', 'Sn', 50, 118.69, {112: .95, 114: .65, 115: .34, 116: 14.24, 117: 7.57, 118: 24.01, 119: 8.58, 120: 32.97, 122: 4.71, 124: 5.98}, 1.8e-2, arcanum='Stannum')
Antimony = NASelement('Antimony', 'Sb', 51, 121.7550, {121: 57.25, 123: 42.75}, 4.4e-4, arcanum='Stibium')
Tellurium = NASelement('Tellurium', 'Te', 52, 127.6, {120: .09, 122: 2.46, 123: .87, 124: 4.61, 125: 6.99, 126: 18.71, 128: 31.79, 130: 34.49}, 8.8e-7)
Iodine = NASelement('Iodine', 'I', 53, 126.9044, {127: 1}, 1.3e-4)
Xenon = NASelement('Xenon', 'Xe', 54, 131.3, {124: .013, 126: .09, 128: 1.92, 129: 26.44, 130: 4.08, 131: 21.18, 132: 26.89, 134: 10.4, 136: 8.87},
                   5.3e-10) # components sum to 99.883, not 100
Caesium = NASelement('Caesium', 'Cs', 55, 132.905, {133: 1}, 3.1e-3, alias=('Cesium',))
Barium = NASelement('Barium', 'Ba', 56, 137.34, {130: .101, 132: .097, 134: 2.42, 135: 6.59, 136: 7.81, 137: 11.32, 138:71.66}, .57)
Lanthanum = NASelement('Lanthanum', 'La', 57, 138.91, {138: .09, 139: 99.91}, 8.1e-3)
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
Hafnium = NASelement('Hafnium', 'Hf', 72, 178.49, {174: .16, 176: 5.21, 177: 18.56, 178: 27.1, 179: 13.75, 180: 35.22}, 2e-3)
Tantalum = NASelement('Tantalum', 'Ta', 73, 180.948, {180: .01, 181: 99.99}, 9.2e-4)
Tungsten = NASelement('Tungsten', 'W', 74, 183.85, {180: .14, 182: 26.4, 183: 14.4, 184: 30.6, 186: 28.4}, 3e-2, arcanum='Wolfram')
Rhenium = NASelement('Rhenium', 'Re', 75, 186.2, {185: 37.07, 187: 62.93}, 4.4e-8)
Osmium = NASelement('Osmium', 'Os', 76, 190.2,
                    {188: 13.3, 189: 16.1, 190: 26.4, 192: 41}, # components sum to 96.8, not 100
                    2.2e-6, density = 22.5 * gram / cc, melt = Centigrade(2700)) # boil > 5300
Iridium = NASelement('Iridium', 'Ir', 77, 192.2, {191: 38.5, 193: 61.5}, 4.4e-7,
                     density = 22.42 * gram / cc)
Platinum = NASelement('Platinum', 'Pt', 78, 195.09,
                      {190: .01, 192: .78, 194: 32.9, 195: 33.8, 196: 25.2, 198: 7.2}, # components sum to 99.89, not 100
                      2.2e-6, density = 21.37 * gram / cc)
Gold = NASelement('Gold', 'Au', 79, 196.967, {197: 1}, 2.2e-6, arcanum='Aurum')
Mercury = NASelement('Mercury', 'Hg', 80, 200.59, {196: .15, 198: 10.02, 199: 16.84, 200: 23.13, 201: 13.22, 202: 29.80, 204: 6.85}, 2.2e-4, arcanum='Hydrargyrum')
Thallium = NASelement('Thallium', 'Tl', 81, 204.37, {203: 29.5, 205: 70.5}, 1.3e-3)
Lead = NASelement('Lead', 'Pb', 82, 207.19, {202: .5, 204: 1.4, 206: 25.1, 207: 21.7, 208: 52.3},
                  7e-3, arcanum='Plumbum') # components sum to 101, not 100
Bismuth = NASelement('Bismuth', 'Bi', 83, 208.98, {209: 1}, 8.8e-5)
Polonium = NASelement('Polonium', 'Po', 84, 210, {210: 1}, .13)
Astatine = NASelement('Astatine', 'At', 85, 210, [206, 215])
Radon = NASelement('Radon', 'Rn', 86, 222, [222, 220], alias=('Emanation', 'Em'))
Francium = NASelement('Francium', 'Fr', 87, 223, [223])
Radium = NASelement('Radium', 'Ra', 88, 226.05, [226, 228, 224, 223], 5.7e-9)
Actinium = NASelement('Actinium', 'Ac', 89, 227, [227, 228], 1.3e-15)
Thorium = NASelement('Thorium', 'Th', 90, 232.038, {230: 0, 232: 1}, 5.1e-3)
Protactinium = NASelement('Protactinium', 'Pa', 91, 231, {231: 1}, 3.5e-10)
Uranium = NASelement('Uranium', 'U', 92, 238.03, {234: .0057, 235: .7196, 238: 99.276}, 1.8e-3)
Neptunium = NASelement('Neptunium', 'Np', 93, 237, [237, 239])
Plutonium = NASelement('Plutonium', 'Pu', 94, 242, [238, 239, 242])
Americium = NASelement('Americium', 'Am', 95, 243, [243])
Curium = NASelement('Curium', 'Cm', 96, 247, [247])
Berkelium = NASelement('Berkelium', 'Bk', 97, 249, [249])
Californium = NASelement('Californium', 'Cf', 98, 251, [251])
Einsteinium = NASelement('Einsteinium', 'Es', 99, 254, [254])
Fermium = NASelement('Fermium', 'Fm', 100, 253, [253])
Mendelevium = NASelement('Mendelevium', 'Md', 101, 256, [256])
Nobelium = NASelement('Nobelium', 'No', 102, 254, [254])
Lawrencium = NASelement('Lawrencium', 'Lr', 103, 257, [257])
# Kurchatovium = NASelement('Kurchatovium', 'Ku', 104, ...)

# and a few synonyms ...
Deuterium = Hydrogen[2]
Deuterium.nominate('Deuterium', 'D') # abundance 8.5e-5 (relative to Si = 100)
Tritium = Hydrogen[3]
Tritium.nominate('Tritium', 'T')
Ionium = Thorium[230]
Ionium.nominate('Ionium', 'Io')
Thoron = Radon[220]
Thoron.nominate('Thoron', 'Tn')

_rcs_log_ = """
$Log: element.py,v $
Revision 1.2  2007-03-08 23:35:42  eddy
Cross-reference partial duplicate.

Initial Revision 1.1  2004/02/17 00:12:43  eddy
"""
