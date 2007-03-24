# -*- coding: iso-8859-1 -*-
"""Galaxies

$Id: galaxy.py,v 1.3 2007-03-24 16:10:46 eddy Exp $
"""

from value.units import Quantity, tophat, \
     kilo, arc, year, second, metre
from space.home import MilkyWay
from space.common import Orbit, Discovery
from space.body import Galaxy, Group

# data from http://www.daviddarling.info/encyclopedia/G/Galaxy.html
def MilkyOrbit(r): return Orbit(MilkyWay, r * kilo * year.light, None)

# Magnitudes are absolute.
SagittariusDE = Galaxy("Sagittarius Dwarf Elliptical",
                       orbit = MilkyOrbit(78),
                       magnitude=-13.4,
                       radius=5 * kilo * year.light, # diameter "> 10,000 ?"
                       discovery=Discovery("Astronomers", 1994))

LMC = Galaxy("Large Magellanic Cloud",
             orbit = MilkyOrbit(160),
             magnitude=-18.1, radius = 10 * kilo * year.light,
             discovery=Discovery("prehistoric", -1e4))

SMC = Galaxy("Small Magellanic Cloud",
             orbit = MilkyOrbit(180),
             magnitude = -16.2, radius = 7.5 * kilo * year.light,
             discovery=Discovery("prehistoric", -1e4))

UrsaMinorD = Galaxy("Ursa Minor Dwarf",
                    orbit=MilkyOrbit(220),
                    magnitude = -8.9, radius = 500 * year.light,
                    discovery=Discovery("Astronomers", 1954))

DracoD = Galaxy("Draco Dwarf",
                orbit = MilkyOrbit(270),
                magnitude = -8.8, radius = 250 * year.light,
                discovery=Discovery("Astronomers", 1954))

SculptorD = Galaxy("Sculptor Dwarf",
                   orbit = MilkyOrbit(285),
                   magnitude = -11.1, radius = 500 * year.light,
                   discovery = Discovery("Astronomers", 1938))

SextansD = Galaxy("Sextans Dwarf",
                  orbit = MilkyOrbit(290),
                  magnitude = -9.5, radius = 1.5 * kilo * year.light,
                  discovery = Discovery("Astronomers", 1990))

CarinaD = Galaxy("Carina Dwarf",
                 orbit = MilkyOrbit(330),
                 magnitude = -9.3, radius = 250 * year.light,
                 discovery = Discovery("Astronomers", 1977))

FornaxD = Galaxy("Fornax Dwarf",
                 orbit = MilkyOrbit(450),
                 magnitude = -13.2, radius = 1500 * year.light,
                 discovery = Discovery("Astronomers", 1938))

LeoI = Galaxy("Leo I",
              orbit = MilkyOrbit(670),
              magnitude = -9.6, radius = 250 * year.light,
              discovery = Discovery("Astronomers", 1950))

LeoII = Galaxy("Leo II",
               orbit = MilkyOrbit(830),
               magnitude = -11.9, radius = 500 * year.light,
               discovery = Discovery("Astronomers", 1950))

del MilkyOrbit

# See also: http://www.daviddarling.info/encyclopedia/L/LocalGroup.html
# Data from apod:
LocalGroup = Group('Local Group',
                   speed=Quantity(627 + 44 * tophat, kilo * metre / second,
                                  """Speed of Local Group

Our local group of galaxies is moving at somewhere between 600 and 650 km/s
relative to the cosmic microwave background, in a direction described by
astronomers as toward a position given in terms of co-ordinates (l,b) whose
values are given as attributes here.

Most of our uncertainty about this arises from our ignorance of our motion
relative to the Local Group: we know our own velocity relative to the background
radiation far more accurately.

See also: http://antwrp.gsfc.nasa.gov/apod/ap030209.html
""",
                                  l = (273 + 6 * tophat) * arc.degree,
                                  b = (30 + 6 * tophat) * arc.degree))

del Quantity, tophat, kilo, arc, year, second, metre, \
    Orbit, Discovery, Galaxy, Group
