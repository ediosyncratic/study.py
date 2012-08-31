# -*- coding: iso-8859-1 -*-
"""The Inner Planets of our Solar system

Available source for more data:
http://solarsystem.nasa.gov/planets/profile.cfm?Object=Mars&Display=Facts

See study.LICENSE for copyright and license information.
"""

from study.value.units import Object, Quantity, \
     mega, year, day, hour, minute, metre, kg, bar, arc, Centigrade
from home import Sun, Earth, KLplanet, KLsurface
from common import Orbit, Spin, Discovery
Float = Quantity.fromDecimal

Mercury = KLplanet('Mercury',
                   KLsurface(.382, .38, Spin(58 * day + 16 * hour, 0),
                             flattening = 0, material = "silicates",
                             temperature=Centigrade(Quantity.within(110, 290))),
                   Orbit(Sun, Float(57.91, 2, 9, metre),
                         Spin(.241 * year, 7.005), .2056),
                   .0553, 5.43, atmosphere="trace Na",
                   discovery=Discovery("prehistoric", -1e4,
                                       etymology="""Latin: Mercurius.

From Latin, named for the messenger of the goods; so called because it moves so
fast.  Compare the element hydrargyrum.
"""))

Mercury.surface.temperature.document("""Mercury's wildly varying surface temperature

As Mercury spins only slightly (less than a factor of two) faster than it
orbits, its surface spends long periods alternately directly exposed to the
Sun's heat and utterly hidden from it.  Without an atmosphere to re-distribute
the heat, the surface thus gets un-endurably hot when exposed to the sun -
rising to 400 Celsius - and unendurably cold - falling to -180 Celsius, below
100 K.  See http://antwrp.gsfc.nasa.gov/apod/ap020716.html
""")
Mercury.mass.observe(0.33022e24 * kg)
Mercury.surface.spin.period.observe(58.6462 * day)
Mercury.surface.radius.observe(2.57 * mega * metre)
Mercury.surface.radius.observe(Float(2.4397, 4, 6, metre)) # NASA
Mercury.orbit.spin.period.observe(86.96 * day)
Mercury.orbit.precession = Float(.420, 3, None, arc.second / year)

Venus = KLplanet('Venus',
                 # tilt of 177, i.e. nearly 180, means retrograde rotation ...
                 KLsurface(.949, .9,
                            Spin(Float(243.015, 2, None, day), 177.3),
                            # But note that the *atmosphere* goes round every 96 hours !
                            flattening = 0, material="basalt, granite?"),
                 Orbit(Sun, Float(108.21, 2, 9, metre),
                       Spin(.615 * year, 3.394), .0068),
                 .815, 5.24,
                 Atmosphere = Object(pressure = 90 * bar, composition = { "CO2": .97 }),
                 discovery=Discovery("prehistoric", -1e4,
                                     etymology="""Latin: Venus.

From Latin, named for the goddess of love.  Other civilizations have variously
named it for love or war deities.
"""))
Venus.mass.observe(4.8690e24 * kg)
Venus.surface.radius.observe(6.3 * mega * metre)
Venus.surface.radius.observe(Float(6.0518, 4, 6, metre)) # NASA
Venus.orbit.spin.period.observe(224.68 * day)

# Earth goes here ...

def load_martian(): # lazy satellite loader
    import asteroid # for Phobos and Deimos

Mars = KLplanet('Mars',
                KLsurface(.532, .38, Spin(24 * hour + 37 * minute, 25.2),
                          flattening = .0052, material = "basalt, clays"),
                Orbit(Sun, Float(227.94, 2, 9, metre),
                      Spin(1.881 * year, 1.85), .0933),
                .1074, 3.94,
                Atmosphere = Object(pressure = .07 * bar, composition = { "CO2": .95 }),
                discovery=Discovery("prehistoric", -1e4,
                                    etymology="""Latin: Mars

The Romans named it after their god of war, thanks to its reddish colour.
"""),
                satelload=load_martian)
del load_martian

Mars.mass.observe(0.64191e24 * kg)
Mars.surface.spin.period.observe(1.02595675 * day)
Mars.surface.radius.observe(3.43 * mega * metre)
Mars.surface.radius.observe(Float(3.397, 3, 6, metre)) # NASA
Mars.orbit.spin.period.observe(686.95 * day)

Barsoom = Object(__doc__="""Barsoomian units of measurement.

In Edgar Rice Burroughs' books of adventures on Barsoom (known to Earthlings
as Mars), some units of measurement - I am thus far only aware of length and
time - are accounted for clearly enough to be mapped to Earthling units.

In 'Thuvia, Maid of Mars' [0], Burroughs provides a table of length units and
a glossary including both these (but with ad spelt od) and some units of
time.  The text accompanying the table is lamentably not self-consistent,
giving the ad as 11.694 inches but claiming that the haad, which is 200 ad
(2338.8 inches), is 2339 feet; and that the karad, 100 haad, is one degree of
the circumference of Mars at its equator, for which the haad would need to be
1945.2 feet.  Although the text with the table describes the ad (and the
glossary describes the od) as a Martian foot, usage elsewhere - e.g. Chapter
III of [1] The Chessmen of Mars - favours the sofad as foot.  Taking the sofad
to be 11.694 inches yields, via ad and haad, a karad that is a respectable
approximation to one degree of Mars's equator.  This then makes a reasonable
resolution of the confusion, also followed by a fan site [2].  The ratios
among the units, given by the table in 'Thuvia', are:

     10 sofads = 1 ad
    200 ads    = 1 haad
    100 haads  = 1 karad
    360 karads = 1 circumference of Mars at equator.

The glossary at the end of 'Thuvia' also gives tal as a 'Martian second', xat
as a 'Martian minute' and xode as 'Martian hour', but with no further
details.  In 'The Chessmen of Mars' [3] (nearly half way through chapter II),
I see a unit of time called zode (not xode) used, which I read the text to
indicate as a tenth of a day, although the phrasing is unclear.  The next
chapter rather more clearly identifies eight zodes with 'a trifle over
nineteen and a half Earth hours', which matches well with one zode being a
tenth of a Martian day.  In the sixth chapter, Danger, of 'The Master Mind of
Mars' [4], five xats are given to equal about 15 minutes; dividing the zode by
three minutes I get almost fifty, so infer that there are 50 xat in a zode.  A
later chapter of The Master Mind of Mars, 'Xaxa', gives one tal to be roughly
one second (while the zode is confirmed as two and a half hours); as the xat
is a little over 177 seconds, I conjecture that the tal is in fact a little
shorter than a second, with two hundred of them in a xat.

[0] http://freeread.com.au/ebooks00/fr100046.txt
    The table is given part way through Chapter VI, The Jeddak of Lothar.
    The glossary is an appendix.
[1] http://freeread.com.au/ebooks00/fr100047.txt
[2] http://www.erblist.com/abg/maps.html
[3] http://freeread.com.au/ebooks00/fr100047.txt
[4] http://gutenberg.net.au/ebooks01/0100201.txt
""",
                 karad = Mars.surface.circumference / 360,
                 zode = Mars.day / 10)
Barsoom.also(haad = Barsoom.karad / 100,
             xat = Barsoom.zode / 50)
Barsoom.also(ad = Barsoom.haad / 200,
             tal = Barsoom.xat / 200)
Barsoom.sofad = Barsoom.ad / 10

del Orbit, Spin, Discovery, Sun, KLplanet, KLsurface, \
    Object, Quantity, Float, mega, year, day, hour, minute, metre, kg, bar
