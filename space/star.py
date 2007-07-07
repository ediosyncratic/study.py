# -*- coding: iso-8859-1 -*-
"""Some data on stars.

Sources:
  http://www.solstation.com/stars.htm and links therefrom

  http://www.solstation.com/stars/alp-cent3.htm
  comes with good links, and yas more data I haven't finished plundering ;^)

See also:
  millisecond pulsar:
  http://antwrp.gsfc.nasa.gov/apod/ap020220.html
  Big fat database of stars:
  http://antwrp.gsfc.nasa.gov/apod/ap990426.html

$Id: star.py,v 1.7 2007-07-07 18:27:18 eddy Exp $
"""

from study.value.units import Quantity, tophat, kilo, mega, giga, micro, year, day
from body import Object, Star, Group
from common import Discovery, Spheroid, Spin, Orbit
from home import Sun, AU

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

notes = """
Near neighbours

Star			R(Sol)/light-year	(X,Y,Z)/light-year	Type	Luminosity/Sun.lum	Mass/Sun.mass	Radius/Sun.R
Sol			0			(0,0,0)			G2	1.0			1.0		1.0
Proxima Centauri	4.3			(-1.6,-1.2,-3.8)	M5e	.00006			0.1		-
&alpha; Centauri A	4.4			(-1.7,-1.4,-3.8)	G2	1.3			1.1		1.23
&alpha; Centauri B	4.4			(-1.7,-1.4,-3.8)	G2	.36			.89		.87
Barnard's star		5.9			(-.1,-5.9,-.5)		M5	.00044			.15		.12
Wolf 359		7.6			(-7.2,2.1,1.0)		M8e	.00002			.2		.04
Lalande 21185		8.1			(-6.3,1.7,4.8)		M2	.0052			.35		.35
Sirius A		8.7			(-1.6,8.2,-2.5)		A1	23.0			2.31		1.8
Sirius B		8.7			(-1.6,8.2,-2.5)		DA	.0028			.98		.022
UV Ceti A		8.9			(7.7,3.4,-2.8)		M6e	.00006			.12		.05
UV Ceti B		8.9			(7.7,3.4,-2.8)		M6e	.00004			.10		.04
Ross 154		9.5			(1.8,-8.5,-3.8)		M5e	.0004			.31		.12
Ross 248		10.3			(7.4,-.7,7.1)		M6e	.00011			.25		.07
&epsilon; Eridani	10.7			(6.4,8.4,-1.8)		K2	.3			.8		.9
Luyten 789-6		10.8			(9.7,-3.7,-2.9)		M6	.00012			.25		.08
Ross 128		10.8			(-10.8,.7,.2)		M5	.00033			.31		.1
61 Cygni A		11.2			(6.3,-6.1,7.0)		K5	.063			.59		.7
61 Cygni B		11.2			(6.3,-6.1,7.0)		K7	.040			.50		.8
&epsilon; Indi		11.2			(5.3,-3.0,-9.4)		K5	.13			.71		1.0
Procyon A		11.4			(-4.7,10.3,1.1)		F5	7.6			1.77		1.7
Procyon B		11.4			(-4.7,10.3,1.1)		DF	.0005			.63		.01
+59&deg; 1915 A		11.5			(1.1,-5.7,9.9)		M4	.0028			.4		.28
+59&deg; 1915 B		11.5			(1.1,-5.7,9.9)		M5	.0013			.4		.20
Groombridge 34A		11.5			(8.4,.5,8.0)		M2	.0058			.38		.38
Groombridge 34B		11.5			(8.4,.5,8.0)		M4	.0004			-		.11
Lacaille 9352		11.7			(9.2,-2.3,-6.9)		M2	.012			.47		.57
&tau; Ceti		11.9			(10.3,4.9,-3.3)		G8	.44			.82		1.67
Luyten BD		12.2			(-4.4,11.3,1.1)		M4	.0014			.38		.16
LET 118			12.5			(11.4,3.4,-3.8)		M5e	-			-		-
Lacaille 8760		12.5			(7.3,-6.4,-7.9)		M1	.025			.54		.82
Kapteyn's Star		12.7			(1.9,8.8,-9.0)		M0	.004			.44		.24
Kruger 60 A		12.8			(6.3,-2.7,10.8)		M4	.0017			.27		.51
Kruger 60 B		12.8			(6.3,-2.7,10.8)		M6	.00044			.16		-
Ross 614 A		13.1			(-1.5,13.0,-.6)		M5e	.0004			.14		.14
Ross 614 B		13.1			(-1.5,13.0,-.6)		-	.00002			.08		-
BD -12&deg; 4523	13.1			(-5.0,-11.8,-2.8)	M5	.0013			.38		.22
van Maanen's Star	13.9			(13.6,2.8,1.2)		DG	.00017			-		-
Wolf 424 A		14.2			(-13.9,-1.9,2.3)	M6e	.00014			-		.09
Wolf 424 B		14.2			(-13.9,-1.9,2.3)	M6e	.00014			-		.09
G158-27			14.4			(14.3,.2,-2.0)		M7	.00005			-		-
CD -37&deg; 15492	14.5			(11.5,.1,-8.8)		M3	.00058			.39		.4
Groombridge 1616	15.0			(-8.6,4.6,11.4)		K7	.04			.56		.5
CD -46&deg; 11540	15.1			(-1.6,-10.2,-11.0)	M4	.003			.44		.25
CD -49&deg; 13515	15.2			(7.9,-6.0,-11.5)	M3	.00058			.37		.34
CD -44&deg; 11909	15.3			(-1.3,-10.9,-10.7)	M5	.00063			.34		.15
Luyten 1159-16		15.4			(13.1,7.3,3.4)		M8	.00023			-		-
Lalande 25372		15.7			(-13.6,-6.6,4.1)	M2	.0076			-		.40
BD +68&deg; 946		15.8			(-.6,-5.8,14.7)		M3	.0044			.35		.39
Luyten 145-141		15.8			(-6.8,.5,-14.3)		DA	.0008			-		-
Ross 780		15.8			(14.6,-4.5,-4.0)	M5	.0016			.39		.23
&Omicron; Eridani A	15.9			(7.1,14.1,-2.1)		K0	.33			.81		.7
&Omicron; Eridani B	15.9			(7.1,14.1,-2.1)		DA	.0027			.43		.018
&Omicron; Eridani C	15.9			(7.1,14.1,-2.1)		M4e	.00063			.21		.43
BD +20&deg; 2465	16.1			(-13.6,6.6,5.5)		M4	.0036			.44		.28
Altair			16.6			(7.4,-14.6,2.5)		A7	10.0			1.9		1.2
70 Ophiuchi A		16.7			(.2,-16.7,.7)		K1	.44			.89		1.3
70 Ophiuchi B		16.7			(.2,-16.7,.7)		K6	.083			.68		.84
AC +79&deg; 3888	16.8			(-3.2,.2,16.5)		M4	.0009			.35		.15
BD +43&deg; 4305	16.9			(11.5,-3.9,11.8)	M5e	.0021			.26		.24
Stein 2051 A		17.0			(3.5,8.1,14.6)		M5	.0008			-		-
Stein 2051 B		17.0			(3.5,8.1,14.6)		DC	.0003			-		-
WX Ursa Majoris A	17.5			(-12.2,3.1,12.1)	M2	-			-		-
WX Ursa Majoris B	17.5			(-12.2,3.1,12.1)	M8	-			-		-
36 Ophiuchi A		17.7			(-3.3,-15.5,-7.9)	K2	.26			.77		.90
36 Ophiuchi B		17.7			(-3.3,-15.5,-7.9)	K1	.26			.76		.82
36 Ophiuchi C		17.7			(-3.3,-15.5,-7.9)	K6	.09			.63		.90
HR 7703 A		18.4			(7.9,-12.6,-10.9)	K3	.20			.76		.80
HR 7703 B		18.4			(7.9,-12.6,-10.9)	M5	.0008			.35		.14
&sigma; Draconis	18.5			(2.5,-5.9,17.3)		K0	.4			.82		.28
YZ Canis Minoris	18.5			(-7.8,16.7,1.2)		M4	-			-		-
&delta; Pavonis		18.6			(3.8,-6.4,-17.0)	G6	1.0			.98		1.07
1&deg; 4774		18.6			(18.6,-1.1,.7)		M2	.0001			-		-
Luyten 347-14		18.6			(4.3,12.3,13.3)		M7	.0001			.26		.08
-21&deg; 1377		18.7			(-.6,17.3,-7.0)		M1	.016			.46		.59
Luyten 97-12		18.9			(-3.4,6.3,-17.5)	D	.0003			-		-
Luyten 674-15		19.1			(-.96,15.0,-7.0)	M	-			-		-
&eta; Cassiopeia A	19.2			(10.1,2.1,16.2)		G0	1.0			.85		.84
&eta; Cassiopeia B	19.2			(10.1,2.1,16.2)		M0	.03			.52		.07
Luyten 205-128		19.2			(-.8,-10.3,-16.2)	M	.0002			.14		-
HD 36395		19.2			(2.6,19.0,-1.2)		M1	.02			.51		.69
40&deg; 9712		19.3			(-8.8,-11.5,-12.7)	M4	.003			.44		.29
Ross 986		19.3			(-4.3,-14.4,12)		M5	-			-		-
Ross 47			19.4			(1.7,18.9,4.2)		M6	.0008			.35		.17
Wolf 294		19.4			(-3.6,15.8,10.7)	M4	.008			.49		.46
LP 658-2		19.6			(.6,19.5,-1.4)		DK
+53&deg; 1320 A		19.6			(-8.8,7.9,15.6)		M0
+53&deg; 1320 B		19.6			(-8.8,7.9,15.6)		M0
VB10 A			19.6			(6.2,-18.5,1.7)		M4	-			.39		.43
VB10 B			19.6			(6.2,-18.5,1.7)		M5	.007			-		.008
-45&deg; 13677		19.9			(7.5,-11.8,-14.1)	M0	.00002
82 Eridani		20.3			(9.6,11.2,-13.9)	G5
Wolf 630 A		20.3			(-5.8,-19.2,-2.9)	M4	-			.38		-
Wolf 630 B		20.3			(-5.8,-19.2,-2.9)	M5	-			.38		-
Wolf 630 C		20.3			(-5.8,-19.2,-2.9)	-	-			-		-
Wolf 630 D		20.3			(-5.8,-19.2,-2.9)	M4	-			-		-
-11&deg; 3759		20.4			(-15.7,-12.3,-4.4)	M4	-			-		-
&beta; Hydri		20.5			(4.4,.4,-20.0)		G1	-			-		1.66
+45 Fu46 A		21.0			(-3.1,-14.4,15.0)	M3	-			.31		-
+45 Fu46 A		21.0			(-3.1,-14.4,15.0)	-	-			.25		-
+19&deg; 5116 A		21.0			(19.5,-3.4,7.1)		M4
+19&deg; 5116 B		21.0			(19.5,-3.4,7.1)		M6

Types: O,B,A,F,G,K,M; O0 brightest, M9 dimmest; but white dwarfs have type D*
"""
