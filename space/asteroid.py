# -*- coding: iso-8859-1 -*-
"""The Asteroids, and Mars' two mooons.

See also: http://www.johnstonsarchive.net/astro/asteroidmoons.html and links
therefrom.

$Id: asteroid.py,v 1.3 2005-03-13 15:21:30 eddy Exp $
"""

from basEddy.units import Sample, ton, tera, mega, mile, Quantity, year, tophat
from space import Planets, D
from space.body import Asteroid, Ring, Planetoid
from space.common import Discovery, Orbit
from space.rock import NASAmoon, NASAshell

# Mars' moons are just captured asteroids ...
from space.inner import Mars
tmp = Discovery("A. Hall", 1877, location="Washington", date="""August, 1877

Hall discovered Deimos on August 11th and Phobos on August 17th.
He named them after the horses that Roman mythology said pull Mars' chariot.
""")
Phobos = NASAmoon("Phobos", Mars, tmp, 9.38, 0.32,
                  NASAshell(13.4, 11.2, 9.2), "carbonaceous", 1.08e-4, 1.9,
                  etymology="Greek: Phobos (= English: Flight)")
Deimos = NASAmoon("Deimos", Mars, tmp, 23.46, 1.26,
                  NASAshell(7.5, 6.1, 5.2), "carbonaceous", 1.80e-5, 1.76,
                  etymology="Greek: Deimos (= English: Fear)")
del Mars, tmp

Asteroids = Ring("The Asteroid Belt", D.Sun, D.AU, 5 * D.AU,
                 # at least as much tilt as any inner planet
                 max(map(lambda x: abs(x.orbit.spin.tilt), Planets.inner)),
                 # Let Orbit guess eccentricity (don't use Ring's default, 0)
                 None,
                 __doc__ = """The Asteroid Belt

Most asteroids are found between the orbits of Mars and Jupiter.  Some live
closer in; some cross Earth's orbit, others lie entirely within.  There are also
quite a few that inhabit Jupiter's Lagrange points.\n""")

# Some asteroids:
def IArock(name, when, period, maxdiam, mass, miss,
           blur=(1+.01*Sample.tophat), Tton=tera*ton.US, Mmile=mega*mile,
           Q=Quantity, bar=Sample.tophat, yr=year, ml=mile,
           find=Discovery, rock=Asteroid, sol=D.Sun):
    """Asteroids described by Asimov in From Earth to Heaven.

See p. 210, table 32.\n"""

    if not isinstance(when, find):
        when = find(None, when)

    # pity about not knowing eccentricities ...
    return rock(name, sol.orbiter(Q(period + .01 * bar, yr)), mass * Tton * blur,
                maxdiameter=(maxdiam + bar) * ml,
                periterrion=miss*Mmile*blur, # closest approach to Terra
                discovery=when)

#' When I know Ceres' mass, I can make it an Asteroid() ...
Ceres = Planetoid('Ceres',
                  orbit=Orbit(D.Sun, Quantity(2.77 + .01 * tophat, D.AU)),
                  discovery=Discovery("Piazzi", 1801,
                                      day="January 1st 1801",
                                      __doc__="""The discovery of Ceres.

A team of astronomers set out, in 1800, to look for a planet between Mars and
Jupiter, as anticipated by the Titius-Bode law (see Sun.Bode); starting on the
first day of the new year, Piazzi noticed a moving star which was, in due
course, recognised as what they were looking for, even though it was a bit
smaller than they expected and a few more showed up soon enough.\n"""),
                  __doc__="""The first asteroid discovered.

See Ceres.discovery and Sun.Bode for further details.  Piazzi actually named
this asteroid Ceres Ferdinandea, but it's now called Ceres.\n"""),

Albert = IArock('Albert', 1911, 4 + Sample.tophat, 3, 300, 20),
Eros = IArock('Eros', Discovery("C.G. Witt", 1898, location="Berlin",
                                date="1898, August 13",
                                etymology="Greek: Eros - god of love"), 1.76, 15, 15000, 14),
Amor = IArock('Amor', 1932, 2.67, 10, 12000, 10),
# Amor is also the name of a class of Mars-crossers that don't cross Earth's orbit.
Apollo = IArock('Apollo', 1932, 1.81, 2, 100, 7),
Icarus = IArock('Icarus', 1949, 1.12, 1, 12, 4),
Adonis = IArock('Adonis', 1936, 2.76, 1, 12, 1.5),
Hermes = IArock('Hermes', 1937, 1.47, 1, 12, .2)

Asteroids.borrow([ Ceres, Albert, Eros, Amor, Apollo, Icarus, Adonis, Hermes ])

del Planets, D, Asteroid, Ring, Planetoid, Discovery, Orbit, NASAmoon, NASAshell, IArock
del Sample, ton, tera, mega, mile, Quantity, year, tophat

_rcs_log = """
$Log: asteroid.py,v $
Revision 1.3  2005-03-13 15:21:30  eddy
Moved Planets from home to __init__;
may as well get Sun and AU via D while we're at it.

Revision 1.2  2005/03/12 17:56:42  eddy
Missed imports and a punctuation glitch.

Initial Revision 1.1  2005/03/12 16:28:45  eddy
"""
