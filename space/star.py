# -*- coding: iso-8859-1 -*-
"""Some data on stars.

Sources:
  http://www.solstation.com/stars.htm and links therefrom

  http://www.solstation.com/stars/alp-cent3.htm
  comes with good links, and yas more data I haven't finished plundering ;^)

$Id: star.py,v 1.3 2005-03-16 23:23:05 eddy Exp $
"""

from basEddy.units import Quantity, tophat, kilo, mega, giga, micro, year, day
from space.body import Object, Star
from space.common import Discovery, Spheroid, Spin, Orbit
from space.home import Sun, AU

class Group (Object): pass
class Cluster (Group): pass
def Stjerne(name, typ, dist, locn, mass, size, lum, spin=None,
            Sp=Spin, Sf=Spheroid, Sol=Sun, ly=year.light, dy=day, S=Star, **what):
    """Initialize a Star based (mostly) on solstation.com's data."""

    if spin is None: skin = Sf(Sol.surface.radius * size)
    else: skin = Sf(Sol.surface.radius * size, spin=Sp(dy * spin))

    what.update({'type': typ, 'distance': dist * ly,
                 'ICRS2000_0': locn, # 'ICRS 2000.0' data
                 'mass': mass * Sol.mass,
                 'surface': skin,
                 'bright': lum * Sol.bright })

    return apply(S, (name,), what)

del Sun, Object, Star, day, Spheroid

Gliese710 = Stjerne('Gliese 710', 'K5-M1 V', 63.0 + tophat * .1,
                    '18:19:50.8-1:56:19.0',
                    Quantity(.5 + .2 * tophat, best=.42), # but xs4all.n./~mke gave 1e5 * Earth.mass
                    (4.2 + .1 * tophat) / 100, .67 + .1 * tophat, # 'possibly 67 percent'
                    aliases=('NSV 10635', 'Gl 710', 'Hip 89825', 'BD-01 3474',
                             'HD 168442', 'HD 168442', 'U449', 'Vys/McC 63'),
                    __doc__ = """Gliese 710

According to http://www.xs4all.nl/~mke/Gliese710.htm this is a red dwarf headed
our way at 50,400 km/hr, 50 times the size of Earth, 100,000 times as massive
and due to arrive in about 1.4 mega years.  However, solstation reports that
astronomers don't expect it to disturb the Oort cloud enough to 'create a
substantial increase in the long-period comet flux at Earth's orbit'.

Apparently, we're also due (not quite so close, but nearer than Proxima
Centauri, our current nearest neighbour) visits from Barnard's star (10,000
years hence) and Alpha Centauri (A/B).\n""",
                 # but http://www.solstation.com/stars2/gl710.htm
                 # gives "within 1.1 ly (0.34 pc)"; i.e. c. 7e4 AU
                 closestapproach = 4e4 * AU)

AlphaCentauri = Group('Alpha Centauri',
                      aliases=('Rigel Centaurus',),
                      discovery=Discovery('prehistoric', (-2 + tophat * 3) * kilo,
                                          etymology="Arabic: Rigil Kentaurus = the foot of the Centaur",
                                          __doc__="""Alpha Centauri has been known since ancient times.

It's the fourth brightest star in the night sky as well as the brightest star in
Constellation Centaurus; it's been known about for millennia.\n"""),
                      A = Stjerne('Alpha Centauri A', 'G2 V', 4.36 + tophat * .01,
                                  '14:39:36.5-62:50:02.3', 1.095 + .01 * tophat,
                                  1.56 + .08 * tophat, 1.23 + .01 * tophat,
                                  aliases=('Alf Cen A', 'HR 5459', 'Gl 559 A',
                                           'Hip 71683', 'HD 128620', 'CP(D)-60 5483',
                                           'SAO 252838', 'FK5 538', 'LHS 50'),
                                  discovery=Discovery('Nicholas Louis de La Caille', 1752,
                                                      location='Cape Hope',
                                                      story="""Discovery that Alpha Centauri was two stars

The Abbé [Abbot] Nicholas Louis de La Caille (1713-1762) was at the Cape of Good
Hope, the southernmost point of Africa, in 1752 studying the stars of the
southern hemisphere with just an half-inch (8x) refractor.  He noticed that
Alpha Centauri was actually two stars.\n""")),
                      B = Stjerne('Alpha Centauri B', 'K0-1 V', 4.36 + tophat * .01,
                                  '14:39:35.1-60:50:13.8', .907 + .001 * tophat,
                                  .485 + .07 * tophat, .865 + .01 * tophat,
                                  aliases=('Alf Cen B', 'HR 5460', 'Gl 559 B',
                                           'Hip 71681', 'HD 128621', 'LHS 51')),
                      C = Stjerne('Proxima Centauri', 'M5.5 Ve', 4.22 + tophat * .01,
                                  '14:29:42.95-62:40:46.14', .123 + .01 * tophat,
                                  micro * (86.5 + 67 * tophat), # highly variable !
                                  .145 + .01 * tophat, 31.5 + 3 * tophat,
                                  aliases=('V645 Centauri', 'Gl 551', 'Hip 70890', 'LHS 49',
                                           'Alf Cen C'),
                                  age=giga * year * (5.5 + tophat),
                                  discovery=Discovery('Robert Thorburn Ayton Innes', 1915,
                                                      location='Cape Hope',
                                                      story="""Proxima Centauri's discovery

Although Alpha Centauri was known to be double at least as early as 1752, the
third member of the group was not discovered until 1915.  Like de La Caille
before him, Robert Thorburn Ayton Innes (1861-1933) of Edinburgh, Scotland was
also observing from Cape Hope, probably with the 7-inch refractor at the Royal
Observatory.\n""")))

ProximaCentauri = AlphaCentauri.C
AlphaCentauri.B.discovery = AlphaCentauri.A.discovery

# There should be a better way to describe mutual orbit ...
frac = AlphaCentauri.A.mass / (AlphaCentauri.A.mass + AlphaCentauri.B.mass)
AlphaCentauri.A.orbit = Orbit(AlphaCentauri,
                              (23.7 + .1 * tophat) * AU * (1 - frac),
                              Spin(year * (79.90 + .01), 79.23),
                              0.519)
AlphaCentauri.B.orbit = Orbit(AlphaCentauri,
                              (23.7 + .1 * tophat) * AU * frac,
                              Spin(year * (79.90 + .01), 79.23),
                              0.519)
del frac
# i.e. they're 23.7 AU apart and mutually orbiting.

# Source says Proxima might be hyperbolic, i.e. eccentricity may exceed 1; but,
# if elliptical, seems to have period about half a mega-year.
# Using (1-e.e)**1.5 = 2*pi*L/T*(L/G/M)**.5 gives
# 1-e.e = L * (2*pi/T)**(2./3) / (G*M)**(1./3)
# If we assume L (the semi-latus rectum) is the R we see, that comes out at
# 1.59, making e.e negative; but all we know is R is somewhere between L/(1+e)
# and L/(1-e); exploiting L/(1-e) >= R, we can infer 1+e >= 1.59, used below.
AlphaCentauri.C.orbit = Orbit(AlphaCentauri,
                              year.light * (.2 + .02 * tophat), # 'roughly a fifth of a light year'
                              # Source also gives 'about 13,000 AUs' which is .20566 ly.
                              Spin(mega * year * (.5 + .05 * tophat)), # 'around half a million years'
                              # Not sure what upper bound to guess, but lower bound is .59:
                              .8 + .42 * tophat)

del Stjerne, Discovery, Spin, Orbit, Quantity, tophat, kilo, mega, giga, micro, year, AU

_rcs_log = """
$Log: star.py,v $
Revision 1.3  2005-03-16 23:23:05  eddy
Moved Gliese710 from Kuiper, tidied up by adding Stjerne to package solstation data.

Revision 1.2  2005/03/14 00:01:01  eddy
Import/export clean-up.

Initial Revision 1.1  2005/03/13 23:55:49  eddy
"""
