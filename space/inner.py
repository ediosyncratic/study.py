# -*- coding: iso-8859-1 -*-
"""The Inner Planets of our Solar system

$Id: inner.py,v 1.2 2005-03-13 18:47:23 eddy Exp $
"""

from basEddy.units import Object, tophat, \
     giga, mega, year, day, hour, minute, metre, kg, bar
from space.home import Sun, Earth, KLplanet, KLsurface
from space.common import Orbit, Spin, Discovery

Mercury = KLplanet('Mercury',
                   KLsurface(.382, .38, Spin(58 * day + 16 * hour, 0),
                             flattening = 0, material = "silicates"),
                   Orbit(Sun, (57.91 + .01 * tophat) * giga * metre,
                         Spin(.241 * year, 7.005), .2056),
                   .0553, 5.43, atmosphere="trace Na",
                   discovery=Discovery("prehistoric", -1e4,
                                       etymology="""Latin: Mercurius.

From Latin, named for the messenger of the goods; so called because it moves so
fast.  Compare the element hydrargyrum.
"""))

Mercury.mass.observe(0.33022e24 * kg)
Mercury.surface.spin.period.observe(58.6462 * day)
Mercury.surface.radius.observe(2.57 * mega * metre)
Mercury.surface.radius.observe(mega * (2.4397 + .0001 * tophat) * metre) # NASA
Mercury.orbit.spin.period.observe(86.96 * day)

Venus = KLplanet('Venus',
                 # tilt of 177, i.e. nearly 180, means retrograde rotation ...
                 KLsurface(.949, .9, Spin((243.015 + tophat * .01) * day, 177.3),
                           flattening = 0, material="basalt, granite?"),
                 Orbit(Sun, (108.21 + .01 * tophat) * giga * metre,
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
Venus.surface.radius.observe(mega * (6.0518 + .0001 * tophat) * metre) # NASA
Venus.orbit.spin.period.observe(224.68 * day)

# Earth goes here ...

Mars = KLplanet('Mars',
                KLsurface(.532, .38, Spin(24 * hour + 37 * minute, 25.2),
                          flattening = .0052, material = "basalt, clays"),
                Orbit(Sun, (227.94 + .01 * tophat) * giga * metre,
                      Spin(1.881 * year, 1.85), .0933),
                .1074, 3.94,
                Atmosphere = Object(pressure = .07 * bar, composition = { "CO2": .95 }),
                discovery=Discovery("prehistoric", -1e4,
                                    etymology="""Latin: Mars

The Romans named it after their god of war, thanks to its reddish colour.
"""))
Mars.mass.observe(0.64191e24 * kg)
Mars.surface.spin.period.observe(1.02595675 * day)
Mars.surface.radius.observe(3.43 * mega * metre)
Mars.surface.radius.observe(mega * (3.397 + .001 * tophat) * metre) # NASA
Mars.orbit.spin.period.observe(686.95 * day)

del Orbit, Spin, Discovery, Sun, KLplanet, KLsurface, \
    Object, tophat, giga, mega, year, day, hour, minute, metre, kg, bar

_rcs_log = """
$Log: inner.py,v $
Revision 1.2  2005-03-13 18:47:23  eddy
Clean up import/export, include a missing tophat.

Initial Revision 1.1  2005/03/12 15:17:22  eddy
"""
