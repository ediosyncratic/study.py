# -*- coding: iso-8859-1 -*-
"""Kuiper Belt and Oort Cloud objects of our Solar system.

$Id: Kuiper.py,v 1.4 2005-03-12 17:28:41 eddy Exp $
"""

from basEddy.units import *
from space.home import Sun, Earth, AU, KLplanet, KLsurface
from space.outer import Neptune
from space.common import Orbit, Spin, Discovery, Spheroid
from space.rock import NASAmoon, NASAshell
from space.body import Planet, Body, Ring

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

# I take Kupier and Oort from a diagram in New Scientist [2004/Dec/25th, p46]
# using a logarithmic scale for radius.  Like Asteroid, they're rather
# imprecise.  Separate nasa.gov pages refined the Kuiper orbit: "it has a rather
# sharp edge at 50 AU" though some of its objects do stray further out.
# The radius distributions probably shouldn't be uniform ...

Kuiper = Ring("The Kuiper Belt", Sun, Neptune.orbit.radius, 50 * AU,
              #' Guess: Pluto's tilt is ordinary among them
              2 * Pluto.orbit.spin.tilt,
              # Let Orbit guess eccentricity, don't use Ring's default, 0.
              None,
              __doc__ = """The Kuiper Belt

From the orbit of Neptune out to roughly 100 AU from the Sun, the solar system
is surrounded by a belt of rocks, of various sizes (some consider Pluto to be
merely a large one of them), known as the Kuiper Belt.  When these rocks stray
into the inner solar system, as some of the more eccentric ones sometimes do,
they are known as comets.

Originally postulated by Gerard Kuiper in 1951, a year after Jan Oort had
postulated his cloud (much further out).  The first conclusive observation of a
Kuiper Belt object (unless you count Pluto) was in 1992; in the next decade more
thatn 500 more were found..  High resolution CCD cameras combined with powerful
computers are making it much easier to detect such objects.\n""")

Oort = Ring("The Oort Cloud", Sun, 11 * kilo * AU, 100 * kilo * AU,
            None, # special case tilt: all possible values
            None, # use Orbit's guess at eccentricity, not Ring's default, 0
            __doc__ = """The Oort Cloud

The solar system is surrounded by a roughly spherical (except for the hole in
its middle) cloud of interstellar debris, stretching from roughly 11 kAU out to
around 100 kAU (that's about 1.6 light-years).

Named after a Dutch astronomer, Jan Oort, who first asserted its existence, back
in 1950.\n""")

# Notional boundary of the solar system (after Asimov):
Terminus = Sun.orbiter(Quantity(2 + .2 * Sample.tophat, year.light),
                       __doc__ = """Nominal outer boundary of the Solar system.

Since the nearest other star is 4.3 light years away, anything within about 2
light years can be thought of as `within' our Solar system.  Of course, there
may be `brown dwarf' star(oid)s within that distance, and some in the outer
reaches might be more sensibly thought of as having their own little systems
meandering between the realms of our Sun and its nearest peers; but, for my
coarse purposes, it's useful to have a marker orbit.\n""")
# it'd be kinda interesting to extrapolate Bode's law out this far ... if I knew it.

Gliese710 = Body(
    'Gliese 710',
    __doc__ = """Gliese 710

According to http://www.xs4all.nl/~mke/Gliese710.htm this is a red dwarf headed
our way at 50,400 km/hr, 50 times the size of Earth, 100,000 times as massive
and due to arrive in about 1.4 mega years.

Apparently, we're also due (not quite so close, but nearer than Proxima
Centauri, our current nearest neighbour) visits from Barnard's star (10,000
years hence) and Alpha Centauri (A/B).\n""",
    mass = 1e5 * Earth.mass,
    closestapproach = 4e4 * AU)

del Orbit, Spin, Discovery, Sun, Earth, KLplanet, KLsurface, Neptune, Spheroid, Planet, Body, Ring

_rcs_log = """
$Log: Kuiper.py,v $
Revision 1.4  2005-03-12 17:28:41  eddy
Also need Ring.  Added comment from ../planets.py about Oort/Kuiper sources.

Revision 1.3  2005/03/12 16:34:30  eddy
Add the Kuiper and Oort objects, plus other outer details.

Revision 1.2  2005/03/12 15:35:49  eddy
Chuck in Charon while we're at it.

Initial Revision 1.1  2005/03/12 15:25:57  eddy
"""
