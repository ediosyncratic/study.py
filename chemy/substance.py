"""Messing around with chemistry.

$Id: substance.py,v 1.2 2004-02-17 00:12:27 eddy Exp $
"""

from elements import *
# should really have
# Element to carry abundance &c. data, subclass of Substance
# Atom to describe per-atom data, subclass of Molecule
# issue: does isotopic data belong to Element or Nucleus ?
# Ion as peer of Atom and Molecule

# Properties of some substances:
class Substance (Object): pass
class Gas (Substance):
    def _lazy_get__amupokt_(self, ignored, amuk=Nucleon.amuk): # AMU*P/k/T
        try: T = self.temperature
        except AttributeError: T = Centigrade(0)
        try: P = self.pressure
        except AttributeError: P = Atmosphere
        return amuk * P / T
        
    def _lazy_get_density_(self, ignored):
        return self.RMM * self._amupokt

    def _lazy_get_RMM_(self, ignored): # relative molecular mass
        return self.density / self._amupokt

water = Substance(
	density = .999973 * kilogramme / litre, # at 277.13K, when density is maximal
	freezes = 273.150 * Kelvin)
milk = Substance(
    density = 10.5 * pound / gallon)
IcePoint = water.freezes
mercury = Substance(
    atom=atom(80,
              qSample({ 116: .15, 118: 10.02, 119: 16.84,
                        120: 23.13, 121: 13.22, 122: 29.80,
                        124: 6.85 }, best=120.59),
              'Mercury', 'Hg',
              "Quicksilver's mercurial atom",
              etymology = 'hydrargyrum'),
    # Atmosphere / .76 / metre / Earth.surface.g = density
    # 135.9 pound / gallon (disagrees with the following !)
    density = 13595.1 * kilogramme / metre**3)

air = Gas(
    RMM = 1.6 * Nitrogen.A + .4 * Oxygen.A, # close enough ...
    sound = Object(speed = 331.36 * metre / second))
kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)

_rcs_log = """
  $Log: substance.py,v $
  Revision 1.2  2004-02-17 00:12:27  eddy
  New class, Gas; air is one.  New module elements used in place of particle.

  Initial Revision 1.1  2002/10/08 21:30:18  eddy
"""
