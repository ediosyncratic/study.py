# -*- coding: iso-8859-1 -*-
"""Messing around with chemistry.

Allegedly, Oxygen liquifies at -297 F, i.e. 90 K.

$Id: substance.py,v 1.4 2006-08-08 21:22:09 eddy Exp $

Ethyl alcohol boils at 78.4°C (173°F), so it would be all right for room
thermometers, and has been widely used for that purpose for many years.  The
alcohol is colored red (usually) so it can be seen easily.  Amyl alcohol
(1-pentanol) melts at -78.9°C and boils at 138.1°C, so it can be used to replace
mercury in laboratory thermometers that must read to 110°C.  Its coefficient of
cubical expansion is 0.902e-3 / K, so beta' = 0.874e-3.

"""

from elements import *
from basEddy.quantity import Quantity, tophat
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

class Heats (Object):
    # should really deal with molar vs volume vs specific
    def _lazy_get_fusion_(self,         ig): return  self.melt
    def _lazy_get_freeze_(self,         ig): return -self.melt
    def _lazy_get_vaporization_(self,   ig): return  self.boil
    def _lazy_get_vaporisation_(self,   ig): return  self.boil
    def _lazy_get_condense_(self,       ig): return -self.boil

class Temperatures (Object):
    def _lazy_get_fusion_(self,         ig): return self.melt
    def _lazy_get_freeze_(self,         ig): return self.melt
    def _lazy_get_vaporization_(self,   ig): return self.boil
    def _lazy_get_vaporisation_(self,   ig): return self.boil
    def _lazy_get_condense_(self,       ig): return self.boil

water = Substance(
	density = Quantity(1 -27e-6 +tophat * micro, kilogramme / litre,
                           """Density of water.
at 277.13K, when density is maximal.\n"""),
        heat = Heats(
    capacity = Quantity(4.2 + tophat * .1, Joule / gram / Kelvin,
                        "The specific heat capacity of water")),
        temperature = Temperatures(
    triple = Quantity(273.16, Kelvin,
                      "Triple point of water (by definition of the Kelvin)."),
    melt = Quantity(273.150, Kelvin,
                      "Freezing point of water (at one atmosphere)."),
    boil = Quantity(373.150, Kelvin, "Melting point of water (at one atmosphere).")))
IcePoint = water.temperature.freeze
milk = Substance(
    density = 10.5 * pound / gallon)
mercury = Substance(
    atom=atom(80,
              qSample({ 116: .15, 118: 10.02, 119: 16.84,
                        120: 23.13, 121: 13.22, 122: 29.80,
                        124: 6.85 }, best=120.592),
              'Mercury', 'Hg',
              "Quicksilver's mercurial atom",
              etymology = 'hydrargyrum'),
    temperature = Temperatures(freeze = Centigrade(-38.9),
                               boil = Centigrade(356.58)),
    heat = Heats(melt = Quantity(2.29 + tophat * .01, kilo * Joule / mol),
                 boil = Quantity(59.11 + tophat * .01, kilo * Joule / mol),
                 capacity = Quantity(27.953 + tophat * .001, Joule / mol / Kelvin,
                                     at = Centigrade(25))),
    density = Quantity(13595.1 + tophat * .1, kilogramme / metre**3,
                       """Density of merucry.

This is equivalently Atmosphere / .76 / metre / Earth.surface.g, since a 76 cm
column of mercury balances one atmosphere's pressure.  (Note that Eart.surface.g
is equivalently m/m.weight for any mass m.)

I have also seen its value given (using the British gallon) as 136.26 lb /
gallon, which conflicts with the value given here (135.9 pound / gallon)."""),
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

mercury.density.observe(torr * tonne / kg.weight / metre)

air = Gas(
    RMM = 1.6 * Nitrogen.A + .4 * Oxygen.A, # close enough ...
    sound = Object(speed = Quantity(mach)))
air.sound.speed.observe(331.36 * metre / second) # duno where I got this one ...

kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)

_rcs_log = """
  $Log: substance.py,v $
  Revision 1.4  2006-08-08 21:22:09  eddy
  Forgot to reivse IcePoint.

  Revision 1.3  2006/08/08 21:19:49  eddy
  Assorted details from jcalvert at du.edu, mostly about mercury.
  Use mach for sound speed, specify water's boiling, add some error bars,
  introduce classes for heats and temperatures of phase changes, etc.

  Revision 1.2  2004/02/17 00:12:27  eddy
  New class, Gas; air is one.  New module elements used in place of particle.

  Initial Revision 1.1  2002/10/08 21:30:18  eddy
"""
