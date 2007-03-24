# -*- coding: iso-8859-1 -*-
"""The Outer Planets of our Solar system

Note that each outer planet's system of moons has a total mass of roughly 1e-4
times the planet's mass: http://www.physorg.com/news69517584.html

$Id: outer.py,v 1.5 2007-03-24 16:20:15 eddy Exp $
"""

from value.units import tophat, giga, mega, metre, day, hour, minute, year, kg
from space.home import Sun, KLplanet, KLsurface
from space.common import Orbit, Spin, Discovery
tyr = 365.242198781 * day
gassy = "H2, He, CH4, NH3, etc."

def load_jovian(): # lazy satellite loader
    import space.jovian

Jupiter = KLplanet('Jupiter',
                   KLsurface(11.19, 2.54, Spin(9 * hour + 50 * minute, 3.1),
                             flattening = .0648),
                   Orbit(Sun, (778.2 + .01 * tophat) * giga * metre,
                         Spin(11.862 * year, 1.305), .0481),
                   317.89, 1.33, atmosphere=gassy,
                   discovery=Discovery("prehistoric", -1e-4,
                                       etymology="""Latin: Jupiter

The Romans, like the Greeks, named this planet (which we now know to be the
largest and most massive) after the king of their gods.
"""),
                   aliases=("Zeus", "Jove"),
                   satelload=load_jovian)
del load_jovian

Jupiter.mass.observe(1898.8e24 * kg)
Jupiter.surface.radius.observe(72 * mega * metre)
Jupiter.surface.radius.observe(mega * (71.492 + .001 * tophat) * metre) # NASA
Jupiter.surface.spin.period.observe(0.41354 * day)
Jupiter.orbit.spin.period.observe(11.862 * tyr)
Jupiter.orbit.radius.observe(giga * (778.30 + .1 * tophat) * metre) # NASA

def load_saturnalia(): # lazy satellite loader
    import space.saturnalia

Saturn = KLplanet('Saturn',
                  KLsurface(9.41, 1.07, Spin(10 * hour + 14 * minute, 26.7),
                            flattening = .1076),
                  Orbit(Sun, (1431 + .1 * tophat) * giga * metre,
                        Spin(29.57 * year, 2.49), .051),
                  95.18, .70, atmosphere=gassy,
                  discovery=Discovery("prehistoric", -1e-4,
                                      etymology="""Latin: Saturn

Father of Jupiter (who killed him, quite likely by swallowing), known to the
Greeks as Cronos, whose brothers and sisters were Titans, after whom many of
Saturn's moons are named.

Saturn is the most distant planet that can be observed from Earth with the naked
eye.  It was the first gas giant whose rings got noticed.
"""),
                  aliases=("Cronos",),
                  satelload=load_saturnalia)
del load_saturnalia

Saturn.mass.observe(568.5e24 * kg)
Saturn.surface.radius.observe(60.5 * mega * metre)
Saturn.surface.radius.observe(mega * (60.268 + .001 * tophat) * metre) # NASA
Saturn.surface.spin.period.observe(0.4375 * day)
Saturn.orbit.spin.period.observe(29.458 * tyr)
Saturn.orbit.radius.observe(giga * (1429.39 + .1 * tophat) * metre) # NASA

def load_uranic(): # lazy satellite loader
    import space.uranic

Uranus = KLplanet('Uranus',
                  KLsurface(3.98, .9, Spin(15 * hour + 34 * minute, 97.9),
                            flattening = .03),
                  Orbit(Sun, (2886 + .1 * tophat) * giga * metre,
                        Spin(84.75 * year, .773), .047),
                  14.5, 1.30, atmosphere=gassy, discovery=Discovery("Herschel", 1781),
                  satelload=load_uranic)
del load_uranic

Uranus.mass.observe(86.625e24 * kg)
Uranus.surface.radius.observe(26.7 * mega * metre)
Uranus.surface.radius.observe(mega * (25.559 + .001 * tophat) * metre) # NASA
Uranus.surface.spin.period.observe(0.72 * day) # NASA
Uranus.orbit.spin.period.observe(84.012 * tyr)
Uranus.orbit.spin.period.observe(83.75 * year) # NASA (suspiciously 1 year off K&L)
Uranus.orbit.radius.observe(giga * (2875.04 + .1 * tophat) * metre) # NASA

def load_neptunous(): # lazy satellite loader
    import space.neptunous

Neptune = KLplanet('Neptune',
                   KLsurface(3.81, 1.2, Spin(18 * hour + 26 * minute, 29.6),
                             flattening = .026),
                   Orbit(Sun, (4529 + .1 * tophat) * giga * metre,
                         Spin(167 * year, 1.77), .007),
                   17.24, 1.76, atmosphere=gassy,
                   discovery=Discovery("Galle", 1846,
                                       __doc__="""Neptune's discovery

Two young mathematicians, Adams and le Verrier, independently predicted that
there must be a planet further out than Uranus, to account for observed motions;
they were mostly ignored, but le Verrier persuaded Galle to check.  On September
23rd, 1846, Galle found Neptune pretty much exactly where he'd been told to
look.  A mere 17 days later, Lassell found Triton orbiting it.\n"""),

                   satelload=load_neptunous)
del load_neptunous

Neptune.mass.observe(102.78e24 * kg)
Neptune.surface.radius.observe(24.9 * mega * metre)
Neptune.surface.radius.observe(mega * (24.764 + .001 * tophat) * metre) # NASA
Neptune.surface.spin.period.observe(0.67 * day) # NASA
Neptune.orbit.spin.period.observe(164.798 * tyr)
Neptune.orbit.spin.period.observe(163.72 * year) # NASA
Neptune.orbit.radius.observe(giga * (4504.45 + .1 * tophat) * metre) # NASA

del Orbit, Spin, Discovery, Sun, KLplanet, KLsurface, \
    tophat, giga, mega, metre, day, hour, minute, year, kg, tyr, gassy
