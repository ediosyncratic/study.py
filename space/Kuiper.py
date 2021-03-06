# -*- coding: iso-8859-1 -*-
"""Kuiper Belt and Oort Cloud objects of our Solar system.

See also:
http://space.newscientist.com/article/dn13029-voyager-2-probe-reaches-solar-system-boundary.html
See study.LICENSE for copyright and license information.
"""

from study.value.units import Quantity, \
     zetta, tera, giga, mega, kilo, metre, second, hour, day, year, kg, \
     Kelvin, Fahrenheit, Centigrade
from study.value.archaea import mile

from home import Sun, Earth, AU, KLplanet, KLsurface
from outer import Neptune
from common import Orbit, Spin, Discovery, Spheroid, Surface
from rock import NASAmoon, NASAshell
from body import Object, Ring, Shell, Planetoid, MinorPlanet, DwarfPlanet

Float = Quantity.fromDecimal
About = Quantity.within

Pluto = KLplanet('Pluto',
                 KLsurface(.23, .05, Spin(6 * day + 9 * hour, 118),
                           flattening = 0, material="CH4 ice",
                           temperature=Centigrade(About(-233, 5))),
                 Orbit(Sun, Float(5936, 1, 9, metre),
                       Spin(250 * year, 17.13), .253),
                 .0025,
                 Quantity.flat(1.8, 2.1), # according to Solstation (K&L gave 1.1 g/cc)
                 DwarfPlanet, # in place of Planet
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
Pluto.surface.radius.observe(Float(1.195, 3, 6, metre)) # NASA
# but http://www.solstation.com/stars/kuiper.htm gives 2320 km diameter, i.e. r=1.16 Mm
Pluto.orbit.spin.period.observe(248.5 * 365.242198781 * day)
Pluto.orbit.spin.period.observe(248.02 * year) # NASA
Pluto.orbit.radius.observe(Float(5915.80, 1, 9, metre)) # NASA

Charon = NASAmoon("Charon", Pluto, Discovery("Christy", 1978), 19.64, 6.39, NASAshell(593), "ice")
# but http://www.solstation.com/stars/kuiper.htm gives surface radius 635 km

Makemake = DwarfPlanet('Makemake',
                       # from Wikipedia
                       surface=Spheroid(.95 * mega * metre,
                                        .75 * mega * metre,
                                        .65 * mega * metre),
                       orbit=Orbit(Sun, 45.791 * AU,
                                   Spin(113183 * day, 28.96),
                                   0.159),
                       mass=Float(4, 0, 21, kg))

# From Wikipedia:
Haumea = DwarfPlanet('Haumea',
                     # not clear, but Wikipedia's "dimensions" appear to be
                     # diameters; hence the /2 here:
                     surface = Spheroid(1.96/2 * mega * metre,
                                        1.518/2 * mega * metre,
                                        .996/2 * mega * metre),
                     orbit = Orbit(Sun, 43.335 * AU,
                                   Spin(Float(104234, 1, None, day),
                                        Float(28.19, 2)),
                                   0.18874),
                     mass = Float(2.1, 1, 21, 2 * kg),
                     albedo = About(.7, .1),
                     temperature = About(32, 3, Kelvin))

Hiiaka = Planetoid("Hi'iaka",
                   orbit=Orbit(Haumea,
                               About(49.5, .4, mega * metre),
                               Spin(About(49.12, .03, day),
                                    About(234.8, .3)),
                               About(.05, .003)),
                   mass=Float(4, 0, 20, kg),
                   surface=Spheroid(Float(155, 0, 3, metre)))
Namaka = Planetoid('Namaka',
                   orbit=Orbit(Haumea,
                               39.3 * mega * metre,
                               Spin(About(34.7, .1, day),
                                    #' tilt is given relative to Hi'iaka
                                    About(39, 6))),
                   mass = Float(8, 0, 19, kg),
                   surface=Spheroid(85 * kilo * metre))

Quaoar = MinorPlanet('Quaoar',
                   surface=Spheroid(800 * mile),
                   # I haven't yet found radius ... but its eccentricity is low
                   orbit=Orbit(Sun, Float(4, 0, 9, mile), None, 0),
                   magnitude=18.5,
                   discovery=Discovery("Chadwick Trujillo and Michael Brown, of Caltech",
                                       2002,
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
peri = About(76, 7, AU)
Sedna = MinorPlanet('Sedna',
                    surface=Spheroid(About(950, 150, mile),
                                     # surface temperature is "about" -400 F.
                                     temperature=Fahrenheit(About(-400, 10)),
                                     # it "likely rotates once every approximately 40 days"
                                     #' suggesting that it's tidally locked to a moon
                                     spin=Spin(About(40, 5, day))),
                    orbit=Orbit(Sun,
                                Quantity.flat(peri.low, ape.high,
                                              .5 * (ape.best + peri.best)),
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

# Data on Eris and Dysnomia from Wikipedia (2007/July/7):
Eris = DwarfPlanet('Eris',
                   surface=Surface(Quantity.fromSpread(1.3, .1, .3, mega * metre),
                                   # But alleged equatorial radius is only 1.2 Mm ...
                                   .8 * metre / second**2,
                                   Spin(About(10, 2, hour)), # > 8
                                   albedo=About(0.86, .07),
                                   material="CH4 ice",
                                   temperature = Quantity.flat(30, 55, None, Kelvin)),
                   orbit=Orbit(Sun, 67.6681 * AU, Spin(203500 * day, 44.187), .4417,
                               apehelion=97.56 * AU,
                               perihelion=37.77 * AU),
                   mass = About(16.6, .2, zetta * kg),
                   discovery=Discovery("Mike Brown, Chad Trujillo, David Rabinowitz", 2003,
                                       telescope = "Palomar Oschin",
                                       note="""Originally called 2003 UB313.

First noticed on January 5, 2005, in images dating from October 21, 2003.

Initially nick-named Xena (after a warrior princess in an eponymous television
show), Eris forced astronomers to come to a decision as to whether to classify
it as a planet or to declassify Pluto as a planet; for more on this, see
http://www.chaos.org.uk/~eddy/when/2006/planet.html

Given the conflicts that resulted, the name Eris seems entirely apt: Eris was
the ancient Greek deity (a vicious one, a companion of Ares) associated with
discord.  She wasn't invited to the wedding of Peleus and Thetis (Achilles'
parents), in spite at which she threw a golden apple in among the celebrants,
inscribed 'To the most beautiful' - leading to a legendary conflict among the
goddesses there present as to which deserved that title.  In the end, the three
goddesses who wouldn't back down agreed to let Paris, son of King Priam of Troy,
adjudicate.  Each candidate did her best to sway his judgement her way;
ultimately, one of them (I'm guessing Aphrodite) promissed him the most
beautiful wife in the world - Helen - if he'd give her the prize.  Paris took
that offer, got Helen and thereby precipitated the ten-year siege known as the
Trojan war, which led to his death and the fall of Troy.  Eris, goddess of
discord, must have been delighted at how well her trouble-making succeeded.

Modern types have a tendency to identify Eris with chaos, celebrating the
transformative power of chaos, but it should be noted that the ancient Greeks
had a perfectly good word for that - chaos - and Eris wasn't particularly
associated with it, aside from it tending to be a consequence of her vicious
trouble-making.  As Steve Linley put it, Eris is identified as 'warmonger, the
cause of conflict, the source of competitive rivalry. Any attempt to euphemise,
mollify or glamorise her nature is naive.'  For further details, see:
 * http://www.pantheon.org/articles/e/eris.html
 * http://homepage.mac.com/cparada/GML/Eris.html
"""))

Dysnomia = Planetoid(name="Dysnomia",
                     surface=Spheroid(75 * kilo * metre), # < 150 km
                     orbit=Orbit(Eris,
                                 About(37.37, .15, mega * metre),
                                 Spin(About(15.774, .002, day),
                                      About(142, 3)),
                                 Quantity.below(.013)),
                     discovery=Discovery("M. E. Brown, M. A. van Dam, A. H. Bouchez, D. Le Mignant",
                                         2005, telescope="Keck",
                                         note="""2005, September 10 images.

Initially nicknamed Gabrielle after Xena's side-kick.
Discovered using laser guide star adaptive optics.
Its discoverers translate 'Dysnomia' as 'lawlessness', punning on the name, Lucy
Lawless, of the actress who played Xena.

http://www.pantheon.org/articles/d/dysnomia.html
"""))

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
             About(5, 4, 11 * kilo * AU), # from Bode index 17 to 20
             mass=About(9, 7, 20 * Earth.mass),
             # estimates from about 40 times Earth's mass to greater than that of Jupiter
            __doc__ = """The Oort Cloud

The solar system is surrounded by a roughly spherical (except for the hole in
its middle) cloud of interstellar debris, stretching from roughly 11 kAU out to
around 100 kAU (i.e. about 1.6 light-years).

Named after a Dutch astronomer, Jan Oort, who first asserted its existence, back
in 1950.\n""")

Heliosphere = Shell("The Heliosphere", Sun, Quantity.below(230, AU),
                    # Out to Bode index c. 12
                    Termination = Shell("Sol's Termination Shock", Sun,
                                        Quantity.flat(75, 90, None, AU),
                                        # c. Bode index 10
                                        __doc__ = "Solar wind falls below sound speed."),
                    Heliopause = Shell("Heliopause", Sun,
                                       Float(11, 0, 1, AU), # "about 110"
                                       # c. Bode index 11
                                       __doc__ = """Solar wind meets interstellar medium.

Theory predicts the solar wind should be deflected sideways when it meets the
interstellar medium.  See Stagnation for what Voyager 1 found instead.
"""),
                    Stagnation = Shell("Stagnation region", Sun,
                                       Quantity.flat(113, 121, None, AU),
                                       __doc__ = """Solar wind stops.

Voyager 1 observed the solar wind speed to drop away to nothing at about 113
AU and remain so for at least 7.5 AU - we're still (2012) waiting for more
data on this, and don't have a good model for what's going on.
"""),
                    BowShock = Shell("Sol's Bow Shock", Sun,
                                     Float(23, 0, 1, AU), # 'near 230'
                                     # c. Bode index 11.6
                                     __doc__="The Bow Wave of the Heliosphere"),
                    __doc__ = """Our Sun's sphere of influence.

The Sun's magnetic field and particles from the solar wind continue outwards
into space.  The Termination Shock is defined to be where the solar wind's
speed falls to below the speed of sound (in it).  At the Heliopause, the Sun's
ions were expected to be deflected sideways due to meeting the interstellar
medium (a mixture of atoms, molecules and ions).  This results in a pressure
wave in the interstellar gas, essentially like the bow wave of a boat in
water; this causes a bow shock.  However, Voyager 1 has found a stagnation
region roughly where the heliopause was expected; instead of being deflected
sidways, the solar wind apparently simply stops; this isn't presently (2012)
well understood !

See also: 
 * http://antwrp.gsfc.nasa.gov/apod/ap020624.html
 * http://arstechnica.com/science/2012/09/missing-voyager-1-yet-to-find-the-boundary-line-of-the-solar-system/
 * and a picture of the bow shock of a young star, LL Orionis, in Orion:
   http://antwrp.gsfc.nasa.gov/apod/ap020313.html
""")

# Notional boundary of the solar system (after Asimov):
Terminus = Shell("Our Solar System's Edge", Sun,
                 Quantity.flat(1.9, 2.1, None, year.light),
                 # Roughly Bode index 20.7
                 __doc__ = """Nominal outer boundary of the Solar system.

Since the nearest other star is 4.3 light years away, anything within about 2
light years can be thought of as `within' our Solar system.  Of course, there
may be `brown dwarf' star(oid)s within that distance, and some in the outer
reaches might be more sensibly thought of as having their own little systems
meandering between the realms of our Sun and its nearest peers; but, for my
coarse purposes, it's useful to have a marker orbit.\n""")

del Orbit, Spin, Discovery, Sun, Earth, AU, KLplanet, KLsurface, Neptune, \
    Spheroid, Object, Ring, Quantity, Float, About, \
    zetta, tera, giga, mega, kilo, metre, second, mile, hour, day, year, kg, \
    Kelvin, Fahrenheit, Centigrade
