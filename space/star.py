# -*- coding: iso-8859-1 -*-
"""Some data on stars.

Sources:
  http://www.solstation.com/stars/alp-cent3.htm
  comes with good links, and yas more data I haven't finished plundering ;^)

$Id: star.py,v 1.2 2005-03-14 00:01:01 eddy Exp $
"""

from basEddy.units import tophat, kilo, mega, giga, micro, year, day
from space.body import Object, Galaxy, Star
from space.common import Discovery, Spheroid, Spin, Orbit
from space.home import MilkyWay, Sun, AU

class Group (Object): pass
class Cluster (Group): pass

AlphaCentauri = Group('Alpha Centauri',
                      # locations are 'ICRS 2000.0'
                      aliases=('Rigel Centaurus',),
                      discovery=Discovery('prehistoric', (-2 + tophat * 3) * kilo,
                                          etymology="Arabic: Rigil Kentaurus = the foot of the Centaur",
                                          __doc__="""Alpha Centauri has been known since ancient times.

It's the fourth brightest star in the night sky as well as the brightest star in
Constellation Centaurus; it's been known about for millennia.\n"""),
                      A = Star('Alpha Centauri A', 'G2 V', 4.36,
                               aliases=('Alf Cen A', 'HR 5459', 'Gl 559 A',
                                        'Hip 71683', 'HD 128620', 'CP(D)-60 5483',
                                        'SAO 252838', 'FK5 538', 'LHS 50'),
                               discovery=Discovery('Nicholas Louis de La Caille', 1752,
                                                   location='Cape Hope',
                                                   story="""Discovery that Alpha Centauri was two stars

The Abbé [Abbot] Nicholas Louis de La Caille (1713-1762) was at the Cape of Good
Hope, the southernmost point of Africa, in 1752 studying the stars of the
southern hemisphere with just an half-inch (8x) refractor.  He noticed that
Alpha Centauri was actually two stars.\n"""),
                               location='14:39:36.5-62:50:02.3',
                               bright=Sun.bright * (1.56 + .08 * tophat),
                               mass=Sun.mass * (1.095 + .01 * tophat),
                               surface=Spheroid(Sun.surface.radius * (1.23 + .01 * tophat))),
                      B = Star('Alpha Centauri B', 'K0-1 V', 4.36,
                               aliases=('Alf Cen B', 'HR 5460', 'Gl 559 B',
                                        'Hip 71681', 'HD 128621', 'LHS 51'),
                               location='14:39:35.1-60:50:13.8',
                               mass=Sun.mass * (.907 + .001 * tophat),
                               bright=Sun.bright * (.485 + .07 * tophat),
                               surface=Spheroid(Sun.surface.radius * (.865 + .01 * tophat))),
                      C = Star('Proxima Centauri', 'M5.5 Ve', 4.22,
                               location='14:29:42.95-62:40:46.14',
                               aliases=('V645 Centauri', 'Gl 551', 'Hip 70890', 'LHS 49',
                                        'Alf Cen C'),
                               mass=Sun.mass * (.123 + .01 * tophat),
                               bright=Sun.bright * micro * (86.5 + 67 * tophat), # highly variable !
                               age=giga * year * (5.5 + tophat),
                               surface=Spheroid(Sun.surface.radius * (.145 + .01 * tophat),
                                                spin=Spin(day * (31.5 + 3 * tophat))),
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

del MilkyWay, Sun, Object, Galaxy, Star

_rcs_log = """
$Log: star.py,v $
Revision 1.2  2005-03-14 00:01:01  eddy
Import/export clean-up.

Initial Revision 1.1  2005/03/13 23:55:49  eddy
"""
