# -*- coding: iso-8859-1 -*-
"""Kuiper Belt and Oort Cloud objects of our Solar system.

$Id: Kuiper.py,v 1.2 2005-03-12 15:35:49 eddy Exp $
"""

from basEddy.units import *
from space.home import Sun, KLplanet, KLsurface
from space.common import Orbit, Spin, Discovery, Spheroid
from space.rock import NASAmoon, NASAshell
from space.body import Planet

Pluto = KLplanet('Pluto',
                 KLsurface(.23, .05, Spin(6 * day + 9 * hour, 118),
                           flattening = 0, material="CH4 ice"),
                 Orbit(Sun, (5936 + .1 * tophat) * giga * metre,
                       Spin(250 * year, 17.13), .253),
                 .0025, 1.1, atmosphere="trace CH4")
Pluto.mass.observe(15e21 * kilogramme)
Pluto.surface.spin.period.observe(6.3867 * day)
Pluto.surface.radius.observe(mega * (1.195 + .001 * tophat) * metre) # NASA
Pluto.orbit.spin.period.observe(248.5 * 365.242198781 * day)
Pluto.orbit.spin.period.observe(248.02 * year) # NASA
Pluto.orbit.radius.observe(giga * (5915.80 + .1 * tophat) * metre) # NASA

Charon = NASAmoon("Charon", Pluto, Discovery("Christy", 1978), 19.6, 6.39, NASAshell(593), "ice")

Quaoar = Planet('Quaoar', Spheroid(800 * mile),
                # I haven't yet found radius ... but its eccentricity is low
                Orbit(Sun, Quantity(4 + tophat, giga * mile), None, 0),
                magnitude=18.5,
                discovery=Discovery("Chadwick Trujillo and Michael Brown, of Caltech", 2002,
                                    telescope = "Palomar Oschin Schmidt",
                                    note = """2002 LM60, a.k.a. Quaoar

Drs. Trujillo and Brown first observed this Kuiper Belt object, then (July 5,
August 1) had the Hubble Space Telescope take a closer look, to determine true
angular size, 40 * milli * arc.second.  They named it after the creator-god of
the Tongva, the original inhabitants of the Los Angeles basin.\n"""))

# Even further out: beyond Kuiper, in the "inner Oort Cloud"
ape = Quantity(900, AU)
ape.observe(130 * tera * metre)
ape.observe(84 * giga * mile)
peri = Quantity(76 + 14 * tophat, AU)
Sedna = Planet('Sedna', Spheroid((950 + 300 * tophat) * mile,
                                 # surface temperature is "about" -400 F.
                                 temperature=Fahrenheit(-400 + 20 * tophat),
                                 # it "likely rotates once every approximately 40 days"
                                 #' suggesting that it's tidally locked to a moon
                                 spin=Spin(Quantity(40 + 10 * tophat, day))),
               # orbit comes as close as
               Orbit(Sun,
                     .5 * (ape.best + peri.best) + (ape.high - peri.low) * tophat,
                     Spin(10.5 * kilo * year),
                     perihelion=peri, apehelion=ape),
               discovery=Discovery(
    "Michael Brown (Caltech), Chadwick Trujillo (Gemini Observatory), David Rabinowitz (Yale)",
    2004, telescope="Samuel Oschin", observatory="Palomar",
    note="""2003 VB12, a.k.a. Sedna

First identified as an 'inner' Oort cloud object in early November 2003, the
astronomers subsequently traced it back to 2001 in archived data.
Named after the Inuit goddess of the sea, from whom all sea creatures were
created; she's said to live at the bottom of the Arctic Ocean.\n"""))
# Also: Sedna, eccentric 10,500 year orbit, 8 to 84 giga miles radius (putting
# it in Sun.Bode[9:13]); between 800 and 1100 miles across; may have a moon.
del ape, peri

del Orbit, Spin, Discovery, Sun, KLplanet, KLsurface, Spheroid, Planet

_rcs_log = """
$Log: Kuiper.py,v $
Revision 1.2  2005-03-12 15:35:49  eddy
Chuck in Charon while we're at it.

Initial Revision 1.1  2005/03/12 15:25:57  eddy
"""
