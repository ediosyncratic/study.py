# -*- coding: iso-8859-1 -*-
"""Messing around with chemistry.

Ethyl alcohol boils at 78.4°C (173°F), so it would be all right for room
thermometers, and has been widely used for that purpose for many years.  The
alcohol is colored red (usually) so it can be seen easily.  Amyl alcohol
(1-pentanol) melts at -78.9°C and boils at 138.1°C, so it can be used to replace
mercury in laboratory thermometers that must read to 110°C.  Its coefficient of
cubical expansion is 0.902e-3 / K, so beta' = 0.874e-3.

$Id: substance.py,v 1.6 2007-03-24 15:02:05 eddy Exp $
"""
from elements import * # q.v.
from value.quantity import Quantity, tophat

# Properties of some substances:
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
milk = Substance(density = 10.5 * pound / gallon)

air = Gas(RMM = 1.6 * Nitrogen.A + .4 * Oxygen.A, # close enough ...
          sound = Object(speed = Quantity(mach)))
air.sound.speed.observe(331.36 * metre / second) # duno where I got this one ...

kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)
