# -*- coding: iso-8859-1 -*-
"""The Asteroids, and Mars' two mooons.

See also:
  http://www.johnstonsarchive.net/astro/asteroidmoons.html
  http://www.nasm.si.edu/research/ceps/etp/asteroids/
  http://cfa-www.harvard.edu/iau/lists/InnerPlot.html
  http://seds.lpl.arizona.edu/nineplanets/nineplanets/asteroids.html
  http://aa.usno.navy.mil/ephemerides/asteroid/astr_alm/asteroid_ephemerides.html
  http://aa.usno.navy.mil/hilton/asteroid_masses.htm
and links therefrom.
"""

from study.value.units import Sample, Quantity, tera, mega, year, km, kg
from study.value.archaea import ton, mile

from inner import Mercury, Venus, Mars
from home import Sun, AU, Earth
from body import Asteroid, DwarfAster, Ring
from common import Discovery, Orbit, Spheroid
from rock import NASAmoon, NASAshell

# Rough stab at tilt of the asteroid belt - at least as much as any inner planet:
tilted = max(map(lambda x: abs(x.orbit.spin.tilt), [ Mercury, Venus, Earth, Mars ]))
del Mercury, Venus, Earth # keep Mars for its moons:

# Mars' moons are just captured asteroids ...
tmp = Discovery("A. Hall", 1877, location="Washington", date="""August, 1877

Hall discovered Deimos on August 11th and Phobos on August 17th.
He named them after the horses that Roman mythology said pull Mars' chariot.
Kepler predicted their existence (on entirely bogus grounds) in 1610 and
Jonathan Swift's famous novel Gulliver's Travels discussed them over 150 years
before their discovery: see http://antwrp.gsfc.nasa.gov/apod/ap010902.html

They're probably captured asteroids.  Note that Phobos is doomed:
http://antwrp.gsfc.nasa.gov/apod/ap010818.html
""")
Phobos = NASAmoon("Phobos", Mars, tmp, 9.38, 0.32,
                  NASAshell(13.4, 11.2, 9.2), "carbonaceous", 1.08e-4, 1.9,
                  etymology="Greek: Phobos (= English: Fear)")
Deimos = NASAmoon("Deimos", Mars, tmp, 23.46, 1.26,
                  NASAshell(7.5, 6.1, 5.2), "carbonaceous", 1.80e-5, 1.76,
                  etymology="Greek: Deimos (= English: Flight or Panic)")
del Mars, tmp

Asteroids = Ring("The Asteroid Belt", Sun, AU, 5 * AU, tilted,
                 # Let Orbit guess eccentricity (don't use Ring's default, 0)
                 None,
                 __doc__ = """The Asteroid Belt

Most asteroids are found between the orbits of Mars and Jupiter.  Some live
closer in; some cross Earth's orbit, others lie entirely within.  There are also
quite a few that inhabit Jupiter's Lagrange points.\n""")

# Some asteroids:
def IArock(name, when, period, maxdiam, mass, miss,
           ton=ton.US, yr=year, ml=mile, Q=Quantity.fromDecimal,
           find=Discovery, rock=Asteroid, sol=Sun):
    """Asteroids described by Asimov in From Earth to Heaven.

See p. 210, table 32.\n"""

    if not isinstance(when, find):
        when = find(None, when)

    # pity about not knowing eccentricities ...
    return rock(name,
                sol.orbiter(Q(period, 2, None, yr)),
                Q(1, 2, 12, mass * ton),
                maxdiameter=Q(maxdiam, 0, None, ml),
                periterrion=Q(1, 2, 6, miss * ml), # closest approach to Terra
                discovery=when)

Ceres = DwarfAster('Ceres',
                   Orbit(Sun, Quantity.fromDecimal(413.9, 1, 6, km), None),
                   Quantity.fromDecimal(.87, 2, 21, kg),
                   surface = Spheroid(Quantity.fromDecimal(466, 0, None, km)),
                   number = 1,
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
this asteroid Ceres Ferdinandea, but it's now called Ceres.\n""")

more_data = """
A few asteroids and comets are listed below for comparison. (distance is the mean distance to the Sun in thousands of kilometers; masses in kilograms). 
No.  Name     Distance/Mm Radius/Mm Mass/kg  Discoverer   Date
---- ---------  --------  ------  -------  ----------  -----
   2 Pallas       414500     261  3.18e20   Olbers      1802
   3 Juno         399400     123     ?      Harding     1804
   4 Vesta        353400     265  3.0e20    Olbers      1807
  10 Hygiea       470300     215  9.3e19    De Gasparis 1849
  15 Eunomia      395500     136  8.3e18    De Gasparis 1851
  52 Europa       463300     156     ?      Goldschmidt 1858
 243 Ida          428000      35     ?      ?           1880?
 433 Eros         172800      33x13x13      Witt        1989
 511 Davida       475400     168     ?      Dugan       1903
 911 Agamemnon    778100      88     ?      Reinmuth    1919
 951 Gaspra       330000       8     ?      Neujmin     1916
1566 Icarus       161269       0.7   ?      Baade       1949
1862 Apollo       220061       0.7   ?      Reinmuth    1932
2060 Chiron      2051900      85     ?      Kowal       1977
2062 Aten         144514       0.5   ?      Helin       1976
2212 Hephaistos   323884       4.4   ?      Chernykh    1978
3554 Amun         145710       ?     ?      Shoemaker   1986
"""

Albert = IArock('Albert', 1911, Sample.flat(3.5, 4.5), 3, 300, 20)
Eros = IArock('Eros', Discovery("C.G. Witt", 1898, location="Berlin",
                                date="1898, August 13",
                                etymology="Greek: Eros - god of love"), 1.76, 15, 15000, 14)
Amor = IArock('Amor', 1932, 2.67, 10, 12000, 10)
# Amor is also the name of a class of Mars-crossers that don't cross Earth's orbit.
Apollo = IArock('Apollo', 1932, 1.81, 2, 100, 7)
Icarus = IArock('Icarus', 1949, 1.12, 1, 12, 4)
Adonis = IArock('Adonis', 1936, 2.76, 1, 12, 1.5)
Hermes = IArock('Hermes', 1937, 1.47, 1, 12, .2)

Asteroids.borrow([ Ceres, Albert, Eros, Amor, Apollo, Icarus, Adonis, Hermes ])

del Sun, AU, Asteroid, DwarfAster, Ring, Discovery, Orbit, NASAmoon, NASAshell, IArock, \
    Sample, ton, tera, mega, mile, Quantity, year, km, kg
