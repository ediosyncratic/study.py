"""Messing around with chemistry.

$Id: substance.py,v 1.1 2002-10-08 21:30:18 eddy Exp $
"""

from particle import *
# should really have
# Element to carry abundance &c. data, subclass of Substance
# Atom to describe per-atom data, subclass of Molecule
# issue: does isotopic data belong to Element or Nucleus ?
# Ion as peer of Atom and Molecule

# Properties of some substances:
class Substance (Object): pass

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

air = Substance(
	sound = Object(speed = 331.36 * metre / second))
kerosene = Substance(density = 8 * pound / gallon)
alcohol = Substance(density = 8 * pound / gallon)
petrol = Substance(density = 7.5 * pound / gallon)

_rcs_log = """
  $Log: substance.py,v $
  Revision 1.1  2002-10-08 21:30:18  eddy
  Initial revision

"""
