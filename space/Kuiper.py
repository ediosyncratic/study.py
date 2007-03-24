# -*- coding: iso-8859-1 -*-
"""Kuiper Belt and Oort Cloud objects of our Solar system.

$Id: Kuiper.py,v 1.9 2007-03-24 22:42:21 eddy Exp $
"""

from study.value.units import Sample, Quantity, tophat, \
     tera, giga, mega, kilo, metre, mile, day, hour, year, kg, \
     Fahrenheit, Centigrade
from home import Sun, Earth, AU, KLplanet, KLsurface
from outer import Neptune
from common import Orbit, Spin, Discovery, Spheroid
from rock import NASAmoon, NASAshell
from body import Planet, Object, Ring, Shell

Pluto = KLplanet('Pluto',
                 KLsurface(.23, .05, Spin(6 * day + 9 * hour, 118),
                           flattening = 0, material="CH4 ice",
                           temperature=Centigrade(-233 + 10 * tophat)),
                 Orbit(Sun, (5936 + .1 * tophat) * giga * metre,
                       Spin(250 * year, 17.13), .253),
                 .0025, 1.95 + .3 * tophat, # K&L gave 1.1 g/cc; solstation gave 1.8 to 2.1
                 atmosphere="trace CH4",
                 discovery=Discovery('Clyde Tombaugh', 1930,
                                     date="1930 February 18 or 23",
                                     location="Lowell observatory, Flagstaff, Arizona",
                                     story="""Discovery of Pluto

Lowell, among others, noticed that the orbits of Neptune and Uranus wobbled,
hinting at another planet further out.  Lowell predicted the planet's orbit and
set in motion a project to find it - which continued after his death and lead to
successful discovery.

The telescope which took the discovery pictures was only installed in 1929 (on
'Mars Hill' no less).
"""))
# Always keeps > 18 AUs from Neptune, due to that 17 degree tilt out of the ecliptic:
# perihelion happens at max tilt.  Orbital period is neatly 1.5 times Neptune's.
Pluto.mass.observe(15e21 * kg)
Pluto.surface.spin.period.observe(6.3867 * day)
Pluto.surface.radius.observe(mega * (1.195 + .001 * tophat) * metre) # NASA
# but http://www.solstation.com/stars/kuiper.htm gives 2320 km diameter, i.e. r=1.16 Mm
Pluto.orbit.spin.period.observe(248.5 * 365.242198781 * day)
Pluto.orbit.spin.period.observe(248.02 * year) # NASA
Pluto.orbit.radius.observe(giga * (5915.80 + .1 * tophat) * metre) # NASA

Charon = NASAmoon("Charon", Pluto, Discovery("Christy", 1978), 19.64, 6.39, NASAshell(593), "ice")
# but http://www.solstation.com/stars/kuiper.htm gives surface radius 635 km

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
              __doc__ = """The Edgeworth-Kuiper Belt

From the orbit of Neptune out to roughly 100 AU from the Sun, the solar system
is surrounded by a belt of rocks, of various sizes (some consider Pluto to be
merely a large one of them), known as the Kuiper Belt.  When these rocks stray
into the inner solar system, as some of the more eccentric ones sometimes do,
they are known as comets.  However, most inhabitants of this zone of the solar
system follow roughly circular orbits fairly close to the plane of the ecliptic.

Originally postulated by Gerard Kuiper in 1951, a year after Jan Oort had
postulated his cloud (much further out); but fore-shadowed by Kenneth Edgeworth
in 1943 and 1949.  The first conclusive observation of a Kuiper Belt object
(unless you count Pluto) was in 1992; in the next decade more thatn 500 more
were found.  High resolution CCD cameras combined with powerful computers are
making it much easier to detect such objects.\n""")

Oort = Shell("The Oort Cloud", Sun,
             (5 + 8 * tophat) * 11 * kilo * AU, # from Bode index 17 to 20
             mass=(9 + 14 * tophat) * 20 * Earth.mass,
             # estimates from about 40 times Earth's mass to greater than that of Jupiter
            __doc__ = """The Oort Cloud

The solar system is surrounded by a roughly spherical (except for the hole in
its middle) cloud of interstellar debris, stretching from roughly 11 kAU out to
around 100 kAU (i.e. about 1.6 light-years).

Named after a Dutch astronomer, Jan Oort, who first asserted its existence, back
in 1950.\n""")

Heliosphere = Shell("The Heliosphere", Sun, (.5 + tophat) * 230 * AU,
                    # Out to Bode index c. 12
                    Termination = Shell("Sol's Termination Shock", Sun,
                                        (82.5 + 15 * tophat) * AU, # "75-90"
                                        # c. Bode index 10
                                        __doc__ = "Where solar wind falls below sound speed."),
                    Heliopause = Shell("Heliopause", Sun,
                                       (11 + tophat) * 10 * AU, # "about 110"
                                       # c. Bode index 11
                                       __doc__ = "Where solar wind ions meet galactic ions"),
                    BowShock = Shell("Sol's Bow Shock", Sun, (23 + tophat) * 10 * AU, # 'near 230'
                                     # c. Bode index 11.6
                                     __doc__="The Bow Wave of the Heliosphere"),
                    __doc__ = """Our Sun's sphere of influence.

The Sun's magnetic field and particles from the solar wind continue outwards
into space.  The Termination Shock is defined to be where the solar wind's speed
falls to below the speed of sound (in it).  At the Heliopause, the Sun's ions
run into ions from those from the rest of our galaxy, the Milky Way.  This
results in a pressure wave in the interstellar gas, essentially like the bow
wave of a boat in water; this causes a bow shock.

See also: http://antwrp.gsfc.nasa.gov/apod/ap020624.html
and a picture of the bow shock of a young star, LL Orionis, in Orion at
http://antwrp.gsfc.nasa.gov/apod/ap020313.html
""")

# Notional boundary of the solar system (after Asimov):
Terminus = Shell("Our Solar System's Edge", Sun, Quantity(2 + .2 * Sample.tophat, year.light),
                 # Roughly Bode index 20.7
                 __doc__ = """Nominal outer boundary of the Solar system.

Since the nearest other star is 4.3 light years away, anything within about 2
light years can be thought of as `within' our Solar system.  Of course, there
may be `brown dwarf' star(oid)s within that distance, and some in the outer
reaches might be more sensibly thought of as having their own little systems
meandering between the realms of our Sun and its nearest peers; but, for my
coarse purposes, it's useful to have a marker orbit.\n""")

del Orbit, Spin, Discovery, Sun, Earth, AU, KLplanet, KLsurface, Neptune, \
    Spheroid, Planet, Object, Ring, Sample, Quantity, tophat, \
    tera, giga, mega, kilo, metre, mile, day, hour, year, kg, Fahrenheit
