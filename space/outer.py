# -*- coding: iso-8859-1 -*-
"""The Outer Planets of our Solar system

Note that each outer planet's system of moons has a total mass of roughly 1e-4
times the planet's mass: http://www.physorg.com/news69517584.html

See study.LICENSE for copyright and license information.
"""

from study.value.units import Quantity, giga, mega, metre, day, hour, minute, year, kg
from home import Sun, KLplanet, KLsurface
from common import Orbit, Spin, Discovery
tyr = 365.242198781 * day
gassy = "H2, He, CH4, NH3, etc."
Float = Quantity.fromDecimal

def load_jovian(): # lazy satellite loader
    import jovian

Jupiter = KLplanet('Jupiter',
                   KLsurface(11.19, 2.54, Spin(9 * hour + 50 * minute, 3.1),
                             flattening = .0648),
                   Orbit(Sun, Float(778.2, 2, 9, metre),
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
Jupiter.surface.radius.observe(Float(71.492, 3, 6, metre)) # NASA
Jupiter.surface.spin.period.observe(0.41354 * day)
Jupiter.orbit.spin.period.observe(11.862 * tyr)
Jupiter.orbit.radius.observe(Float(778.30, 1, 9, metre)) # NASA

def load_saturnalia(): # lazy satellite loader
    import saturnalia

Saturn = KLplanet('Saturn',
                  KLsurface(9.41, 1.07, Spin(10 * hour + 14 * minute, 26.7),
                            # But its spin is very uncertain, thanks to
                            # Enceladus' geysers: see, e.g.,
                            # http://www.physorg.com/news93793251.html
                            flattening = .1076),
                  Orbit(Sun, Float(1431, 1, 9, metre),
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
Saturn.surface.radius.observe(Float(60.268, 3, 6, metre)) # NASA
Saturn.surface.spin.period.observe(0.4375 * day)
Saturn.orbit.spin.period.observe(29.458 * tyr)
Saturn.orbit.radius.observe(Float(1429.39, 1, 9, metre)) # NASA

def load_uranic(): # lazy satellite loader
    import uranic

Uranus = KLplanet('Uranus',
                  KLsurface(3.98, .9, Spin(15 * hour + 34 * minute, 97.9),
                            flattening = .03),
                  Orbit(Sun, Float(2886, 1, 9, metre),
                        Spin(84.75 * year, .773), .047),
                  14.5, 1.30, atmosphere=gassy,
                  discovery=Discovery("William Herschel", 1781,
                                      etymology="""Latin: Uranus

In Roman mythology, Uranus was the father of Saturn.
""",
                                      origin="""Initially named Georgium Sidus.

Uranus was the first planet discovered by science.  Previously, folk had
espied the Sun, Mercury, Venus, the Moon, Mars, Jupiter and Saturn (albeit
naming them diversely by culture); these had been known since antiquity, as
they were visible to anyone (e.g. shepherds) who spent any timeout and about
by night.  Herschel studied the motions of Jupiter and Saturn and noticed
slight deviations, in the latter's movement, from what he could predict using
Newton's theory, given the known heavenly bodies.  The deviations fitted with
what would arise from there being another planet beyond Saturn, so he worked
out where that planet would be and looked there; sure enough, it was there.

Herschel, a German immigrant to England, initially honoured the German king of
England by naming the newly-discovered planet after him, as George's star (in
Latin, Georgium Sidus).  This name was less popular outside George's earthly
realm.  Johann Bode suggested the name Uranus - as the next star out from
Jupiter was named for Jupiter's father, Saturn, so the next star out from it
is named for Saturn's father, Uranus.  The name Georgium Sidus remained as an
archaism in some contexts until 1850 but has now been entirely supplanted by
Bode's suggestion.

http://www.irregularwebcomic.net/3207.html#note4
"""),
                  satelload=load_uranic)
del load_uranic

Uranus.mass.observe(86.625e24 * kg)
Uranus.surface.radius.observe(26.7 * mega * metre)
Uranus.surface.radius.observe(Float(25.559, 3, 6, metre)) # NASA
Uranus.surface.spin.period.observe(0.72 * day) # NASA
Uranus.orbit.spin.period.observe(84.012 * tyr)
Uranus.orbit.spin.period.observe(83.75 * year) # NASA (suspiciously 1 year off K&L)
Uranus.orbit.radius.observe(Float(2875.04, 1, 9, metre)) # NASA

def load_neptunous(): # lazy satellite loader
    import neptunous

Neptune = KLplanet('Neptune',
                   KLsurface(3.81, 1.2, Spin(18 * hour + 26 * minute, 29.6),
                             flattening = .026),
                   Orbit(Sun, Float(4529, 1, 9, metre),
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
Neptune.surface.radius.observe(Float(24.764, 3, 6, metre)) # NASA
Neptune.surface.spin.period.observe(0.67 * day) # NASA
Neptune.orbit.spin.period.observe(164.798 * tyr)
Neptune.orbit.spin.period.observe(163.72 * year) # NASA
Neptune.orbit.radius.observe(Float(4504.45, 1, 9, metre)) # NASA

del Orbit, Spin, Discovery, Sun, KLplanet, KLsurface, Float, \
    Quantity, giga, mega, metre, day, hour, minute, year, kg, tyr, gassy
