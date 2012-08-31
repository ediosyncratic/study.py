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
See study.LICENSE for copyright and license information.
"""

from study.value.units import Quantity, kilo, mega, giga, micro, year
from body import Star, System
from common import Discovery, Spin, Orbit
from home import AU, Sun
from constellation import CanisMinor, Cassiopeia, Centaur, Cetus, Cygnus, \
     Draco, Eridanus, Hydra, Indus, Ophiuchus, Pavonis, UrsaMajor
Float = Quantity.fromDecimal

Solar = System("The Solar System", star=Sun)
del Sun

Gliese710 = Star('Gliese 710',
                 __doc__ = """Gliese 710

According to http://www.xs4all.nl/~mke/Gliese710.htm this is a red dwarf
headed our way at 50,400 km/hr, 50 times the size of Earth, 100,000 times as
massive and due to arrive in about 1.4 mega years.  However, solstation
reports that astronomers don't expect it to disturb the Oort cloud enough to
'create a substantial increase in the long-period comet flux at Earth's
orbit'.

Apparently, we're also due (not quite so close, but nearer than Proxima
Centauri, our current nearest neighbour) visits from Barnard's star (10,000
years hence) and &alpha; Centauri (A/B).\n""")

Gliese710.Solstation('K5-M1 V', Float(63, 1),
                     '18:19:50.8-1:56:19.0',
                     Quantity.flat(.4, .6, .42),
                     # but xs4all.n./~mke gave 1e5 * Earth.mass
                     Float(4.2, 1, -2),
                     Float(.67, 1), # 'possibly 67 percent'
                     aliases=('NSV 10635', 'Gl 710', 'Hip 89825', 'BD-01 3474',
                              'HD 168442', 'HD 168442', 'U449', 'Vys/McC 63'),
                     # http://www.solstation.com/stars2/gl710.htm
                     # gives "within 1.1 ly (0.34 pc)"; i.e. c. 7e4 AU
                     closestapproach = 4e4 * AU)

Centaur.Alpha = System("&alpha; Centauri",
                        __doc__="""&alpha; Centauri has been known since ancient times.

It's the fourth brightest star in the night sky as well as the brightest star
in constellation Centaurus; it's been known about for millennia.\n""",
                        aliases=("Rigil Kentaurus",),
                        discovery=Discovery(
        'prehistoric', Quantity.flat(-3.5, -.5) * kilo,
        etymology="Arabic: Rigil Kentaurus = the foot of the Centaur"),
                       A = Star('&alpha; Centauri A',
                                aliases=('Alf Cen A', 'HR 5459', 'Gl 559 A',
                                         'Hip 71683', 'HD 128620', 'CP(D)-60 5483',
                                         'SAO 252838', 'FK5 538', 'LHS 50')),
                       B = Star('&alpha; Centauri B',
                                aliases=('Alf Cen B', 'HR 5460', 'Gl 559 B',
                                         'Hip 71681', 'HD 128621', 'LHS 51')),
                       C = Star('&alpha; Centauri C',
                                aliases=('V645 Centauri', 'Gl 551', 'Hip 70890', 'LHS 49',
                                         'Alf Cen C', 'Proxima Centauri')))

Centaur.Alpha.A.Solstation('G2 V', Float(4.36, 2),
                            '14:39:36.5-62:50:02.3', Float(1.095, 2),
                            Quantity.flat(1.52, 1.6), Float(1.23, 2),
                            discovery=Discovery('Nicholas Louis de La Caille', 1752,
                                                location='Cape Hope',
                                                story="""Separation

The Abbé [Abbot] Nicholas Louis de La Caille (1713-1762) was at the Cape of
Good Hope, the southernmost point of Africa, in 1752 studying the stars of the
southern hemisphere with just a half-inch (8x) refractor.  He noticed that
&alpha; Centauri was actually two stars.\n"""))

Centaur.Alpha.B.Solstation('K0-1 V', Float(4.36, 2),
                           '14:39:35.1-60:50:13.8', Float(.907, 3),
                           Quantity.flat(.45, .52), Float(.865, 2),
                           discovery = Centaur.Alpha.A.discovery)

# There should be a better way to describe mutual orbit ...
frac = Centaur.Alpha.A.mass / (Centaur.Alpha.A.mass + Centaur.Alpha.B.mass)
whole = Float(23.7, 1, None, AU)
Centaur.Alpha.A.orbit = Orbit(Centaur.Alpha,
                              (1 - frac) * whole,
                              Spin(year * (79.90 + .01), 79.23),
                              0.519)
Centaur.Alpha.B.orbit = Orbit(Centaur.Alpha,
                              frac * whole,
                              Spin(year * (79.90 + .01), 79.23),
                              0.519)
del frac, whole
# i.e. they're 23.7 AU apart and mutually orbiting.

Centaur.Proxima = Centaur.Alpha.C
Centaur.Alpha.C.Solstation('M5.5 Ve', Float(4.22, 2),
                           '14:29:42.95-62:40:46.14', Float(.123, 2),
                           Quantity.flat(53, 120) * micro, # highly variable !
                           Float(.145, 2), Quantity.flat(30, 33),
                           discovery=Discovery('Robert Thorburn Ayton Innes', 1915,
                                               location='Cape Hope',
                                               story="""Proxima Centauri's discovery

Although &alpha; Centauri was known to be double at least as early as 1752,
the third member of the group was not discovered until 1915.  Like de La
Caille before him, Robert Thorburn Ayton Innes (1861-1933) of Edinburgh,
Scotland was also observing from Cape Hope, probably with the 7-inch refractor
at the Royal Observatory.\n"""),
                           age=giga * year * Float(5.5, 0),
                           orbit=Orbit(Centaur.Alpha,
                                       year.light * Float(.20566, 2),
                                       # 'roughly a fifth of a light year';
                                       # source also gives 'about 13,000 AUs'
                                       # which is .20566 ly.
                                       Spin(Float(1, 1, 6, year / 2)),
                                       # 'around half a million years'
                                       # Not sure what upper bound to guess, but lower bound is .59:
                                       Quantity.flat(.59, 1,
                                                      doc="""Some uncertainty.

Source says Proxima's orbit might be hyperbolic, i.e. eccentricity may exceed
1; but, if elliptical, seems to have period about half a mega-year.  Using
(1-e.e)**1.5 = 2*pi*L/T*(L/G/M)**.5 gives
        1-e.e = L * (2*pi/T)**(2./3) / (G*M)**(1./3)
If we assume L (the semi-latus rectum) is the R given, that comes out at 1.59,
making e.e negative; but all we know is R is somewhere between L/(1+e) and
L/(1-e); exploiting L/(1-e) >= R, we can infer 1+e >= 1.59, used above.
""")))

Centaur.Proxima.NeighbourTable(4.3, (-1.6,-1.2,-3.8), 'M5e', .00006, 0.1)
Centaur.Alpha.A.NeighbourTable(4.4, (-1.7,-1.4,-3.8), 'G2', 1.3, 1.1, 1.23)
Centaur.Alpha.B.NeighbourTable(4.4, (-1.7,-1.4,-3.8), 'G2', .36, .89, .87)
Barnard = Star("Barnard's star")
Barnard.NeighbourTable(5.9, (-.1,-5.9,-.5), 'M5', .00044, .15, .12)

Wolf359 = Star("Wolf 359")
Wolf359.NeighbourTable(7.6, (-7.2,2.1,1.0), "M8e", .00002, .2, .04)
Lalande21185 = Star("Lalande 21185")
Lalande21185.NeighbourTable(8.1, (-6.3,1.7,4.8), "M2", .0052, .35, .35)
Sirius = System("Sirius", aliases=("The Dog Star",),
                A = Star("Sirius A"), B = Star("Sirius B"))
Sirius.A.NeighbourTable(8.7, (-1.6,8.2,-2.5), "A1", 23.0, 2.31, 1.8)
Sirius.B.NeighbourTable(8.7, (-1.6,8.2,-2.5), "DA", .0028, .98, .022)
Cetus.UV = System("UV Ceti", A = Star("UV Ceti A"), B = Star("UV Ceti B"))
Cetus.UV.A.NeighbourTable(8.9, (7.7,3.4,-2.8), "M6e", .00006, .12, .05)
Cetus.UV.B.NeighbourTable(8.9, (7.7,3.4,-2.8), "M6e", .00004, .10, .04)
Ross154 = Star("Ross 154")
Ross154.NeighbourTable(9.5, (1.8,-8.5,-3.8), "M5e", .0004, .31, .12)
Ross248 = Star("Ross 248")
Ross248.NeighbourTable(10.3, (7.4,-.7,7.1), "M6e", .00011, .25, .07)
Eridanus.epsilon = Star("&epsilon; Eridani")
Eridanus.epsilon.NeighbourTable(10.7, (6.4,8.4,-1.8), "K2", .3, .8, .9)
Luyten789_6 = Star("Luyten 789-6")
Luyten789_6.NeighbourTable(10.8, (9.7,-3.7,-2.9), "M6", .00012, .25, .08)
Ross128 = Star("Ross 128")
Ross128.NeighbourTable(10.8, (-10.8,.7,.2), "M5", .00033, .31, .1)
Cygnus.s61 = System("61 Cygni",
                    A = Star("61 Cygni A"),
                    B = Star("61 Cygni B"))
Cygnus.s61.A.NeighbourTable(11.2, (6.3,-6.1,7.0), "K5", .063, .59, .7)
Cygnus.s61.B.NeighbourTable(11.2, (6.3,-6.1,7.0), "K7", .040, .50, .8)
Indus.epsilon = Star("&epsilon; Indi")
Indus.epsilon.NeighbourTable(11.2, (5.3,-3.0,-9.4), "K5", .13, .71, 1.0)
Procyon = System("Procyon", A = Star("Procyon A"), B = Star("Procyon B"))
Procyon.A.NeighbourTable(11.4, (-4.7,10.3,1.1), "F5", 7.6, 1.77, 1.7)
Procyon.B.NeighbourTable(11.4, (-4.7,10.3,1.1), "DF", .0005, .63, .01)
p59deg1915 = System("+59&deg; 1915",
                    A = Star("+59&deg; 1915 A"),
                    B = Star("+59&deg; 1915 B"))
p59deg1915.A.NeighbourTable(11.5, (1.1,-5.7,9.9), "M4", .0028, .4, .28)
p59deg1915.B.NeighbourTable(11.5, (1.1,-5.7,9.9), "M5", .0013, .4, .20)
Groombridge34 = System("Groombridge 34",
                       A = Star("Groombridge 34 A"),
                       B = Star("Groombridge 34 B"))
Groombridge34.A.NeighbourTable(11.5, (8.4,.5,8.0), "M2", .0058, .38, .38)
Groombridge34.B.NeighbourTable(11.5, (8.4,.5,8.0), "M4", .0004, None, .11)
Lacaille9352 = Star("Lacaille 9352")
Lacaille9352.NeighbourTable(11.7, (9.2,-2.3,-6.9), "M2", .012, .47, .57)
Cetus.tau = Star("&tau; Ceti")
Cetus.tau.NeighbourTable(11.9, (10.3,4.9,-3.3), "G8", .44, .82, 1.67)
LuytenBD = Star("Luyten BD")
LuytenBD.NeighbourTable(12.2, (-4.4,11.3,1.1), "M4", .0014, .38, .16)
LET118 = Star("LET 118")
LET118.NeighbourTable(12.5, (11.4,3.4,-3.8), "M5e")
Lacaille8760 = Star("Lacaille 8760")
Lacaille8760.NeighbourTable(12.5, (7.3,-6.4,-7.9), "M1", .025, .54, .82)
Kapteyn = Star("Kapteyn's Star")
Kapteyn.NeighbourTable(12.7, (1.9,8.8,-9.0), "M0", .004, .44, .24)
Kruger60 = System("Kruger 60", A = Star("Kruger 60 A"), B = Star("Kruger 60 B"))
Kruger60.A.NeighbourTable(12.8, (6.3,-2.7,10.8), "M4", .0017, .27, .51)
Kruger60.B.NeighbourTable(12.8, (6.3,-2.7,10.8), "M6", .00044, .16)
Ross614 = System("Ross 614", A = Star("Ross 614 A"), B = Star("Ross 614 B"))
Ross614.A.NeighbourTable(13.1, (-1.5,13.0,-.6), "M5e", .0004, .14, .14)
Ross614.B.NeighbourTable(13.1, (-1.5,13.0,-.6), None, .00002, .08)
BDn12deg4523 = Star("BD -12&deg; 4523")
BDn12deg4523.NeighbourTable(13.1, (-5.0,-11.8,-2.8), "M5", .0013, .38, .22)
vanMaanen = Star("van Maanen's Star")
vanMaanen.NeighbourTable(13.9, (13.6,2.8,1.2), "DG", .00017)
Wolf424 = System("Wolf 424", A = Star("Wolf 424 A"), B = Star("Wolf 424 B"))
Wolf424.A.NeighbourTable(14.2, (-13.9,-1.9,2.3), "M6e", .00014, None, .09)
Wolf424.B.NeighbourTable(14.2, (-13.9,-1.9,2.3), "M6e", .00014, None, .09)
G158_27 = Star("G158-27")
G158_27.NeighbourTable(14.4, (14.3,.2,-2.0), "M7", .00005)
CDn37deg15492 = Star("CD -37&deg; 15492")
CDn37deg15492.NeighbourTable(14.5, (11.5,.1,-8.8), "M3", .00058, .39, .4)
Groombridge1616 = Star("Groombridge 1616")
Groombridge1616.NeighbourTable(15.0, (-8.6,4.6,11.4), "K7", .04, .56, .5)
CDn46deg11540 = Star("CD -46&deg; 11540")
CDn46deg11540.NeighbourTable(15.1, (-1.6,-10.2,-11.0), "M4", .003, .44, .25)
CDn49deg13515 = Star("CD -49&deg; 13515")
CDn49deg13515.NeighbourTable(15.2, (7.9,-6.0,-11.5), "M3", .00058, .37, .34)
CDn44deg11909 = Star("CD -44&deg; 11909")
CDn44deg11909.NeighbourTable(15.3, (-1.3,-10.9,-10.7), "M5", .00063, .34, .15)
Luyten1159_16 = Star("Luyten 1159-16")
Luyten1159_16.NeighbourTable(15.4, (13.1,7.3,3.4), "M8", .00023)
Lalande25372 = Star("Lalande 25372")
Lalande25372.NeighbourTable(15.7, (-13.6,-6.6,4.1), "M2", .0076, None, .40)
BDp68deg946 = Star("BD +68&deg; 946")
BDp68deg946.NeighbourTable(15.8, (-.6,-5.8,14.7), "M3", .0044, .35, .39)
Luyten145_141 = Star("Luyten 145-141")
Luyten145_141.NeighbourTable(15.8, (-6.8,.5,-14.3), "DA", .0008)
Ross780 = Star("Ross 780")
Ross780.NeighbourTable(15.8, (14.6,-4.5,-4.0), "M5", .0016, .39, .23)
Eridanus.Omicron = System("&Omicron; Eridani",
                          A = Star("&Omicron; Eridani A"),
                          B = Star("&Omicron; Eridani B"),
                          C = Star("&Omicron; Eridani C"))
Eridanus.Omicron.A.NeighbourTable(15.9, (7.1,14.1,-2.1), "K0", .33, .81, .7)
Eridanus.Omicron.B.NeighbourTable(15.9, (7.1,14.1,-2.1), "DA", .0027, .43, .018)
Eridanus.Omicron.C.NeighbourTable(15.9, (7.1,14.1,-2.1), "M4e", .00063, .21, .43)
BDp20deg2465 = Star("BD +20&deg; 2465")
BDp20deg2465.NeighbourTable(16.1, (-13.6,6.6,5.5), "M4", .0036, .44, .28)
Altair = Star("Altair")
Altair.NeighbourTable(16.6, (7.4,-14.6,2.5), "A7", 10.0, 1.9, 1.2)
Ophiuchus.s70 = System("70 Ophiuchi",
                       A = Star("70 Ophiuchi A"),
                       B = Star("70 Ophiuchi B"))
Ophiuchus.s70.A.NeighbourTable(16.7, (.2,-16.7,.7), "K1", .44, .89, 1.3)
Ophiuchus.s70.B.NeighbourTable(16.7, (.2,-16.7,.7), "K6", .083, .68, .84)
ACp79deg3888 = Star("AC +79&deg; 3888")
ACp79deg3888.NeighbourTable(16.8, (-3.2,.2,16.5), "M4", .0009, .35, .15)
BDp43deg4305 = Star("BD +43&deg; 4305")
BDp43deg4305.NeighbourTable(16.9, (11.5,-3.9,11.8), "M5e", .0021, .26, .24)
Stein2051 = System("Stein 2051", A = Star("Stein 2051 A"), B = Star("Stein 2051 B"))
Stein2051.A.NeighbourTable(17.0, (3.5,8.1,14.6), "M5", .0008)
Stein2051.B.NeighbourTable(17.0, (3.5,8.1,14.6), "DC", .0003)
UrsaMajor.WX = System("WX Ursa Majoris",
                      A = Star("WX Ursa Majoris A"),
                      B = Star("WX Ursa Majoris B"))
UrsaMajor.WX.A.NeighbourTable(17.5, (-12.2,3.1,12.1), "M2")
UrsaMajor.WX.B.NeighbourTable(17.5, (-12.2,3.1,12.1), "M8")
Ophiuchus.s36 = System("36 Ophiuchi",
                       A = Star("36 Ophiuchi A"),
                       B = Star("36 Ophiuchi B"),
                       C = Star("36 Ophiuchi C"))
Ophiuchus.s36.A.NeighbourTable(17.7, (-3.3,-15.5,-7.9), "K2", .26, .77, .90)
Ophiuchus.s36.B.NeighbourTable(17.7, (-3.3,-15.5,-7.9), "K1", .26, .76, .82)
Ophiuchus.s36.C.NeighbourTable(17.7, (-3.3,-15.5,-7.9), "K6", .09, .63, .90)
HR7703 = System("HR 7703", A = Star("HR 7703 A"), B = Star("HR 7703 B"))
HR7703.A.NeighbourTable(18.4, (7.9,-12.6,-10.9), "K3", .20, .76, .80)
HR7703.B.NeighbourTable(18.4, (7.9,-12.6,-10.9), "M5", .0008, .35, .14)
Draco.sigma = Star("&sigma; Draconis")
Draco.sigma.NeighbourTable(18.5, (2.5,-5.9,17.3), "K0", .4, .82, .28)
CanisMinor.YZ = Star("YZ Canis Minoris")
CanisMinor.YZ.NeighbourTable(18.5, (-7.8,16.7,1.2), "M4")
Pavonis.delta = Star("&delta; Pavonis")
Pavonis.delta.NeighbourTable(18.6, (3.8,-6.4,-17.0), "G6", 1.0, .98, 1.07)
p1deg4774 = Star("1&deg; 4774")
p1deg4774.NeighbourTable(18.6, (18.6,-1.1,.7), "M2", .0001)
Luyten347_14 = Star("Luyten 347-14")
Luyten347_14.NeighbourTable(18.6, (4.3,12.3,13.3), "M7", .0001, .26, .08)
m21deg1377 = Star("-21&deg; 1377")
m21deg1377.NeighbourTable(18.7, (-.6,17.3,-7.0), "M1", .016, .46, .59)
Luyten97_12 = Star("Luyten 97-12")
Luyten97_12.NeighbourTable(18.9, (-3.4,6.3,-17.5), "D", .0003)
Luyten674_15 = Star("Luyten 674-15")
Luyten674_15.NeighbourTable(19.1, (-.96,15.0,-7.0), "M")
Cassiopeia.eta = System("&eta; Cassiopeia",
                        A = Star("&eta; Cassiopeia A"),
                        B = Star("&eta; Cassiopeia B"))
Cassiopeia.eta.A.NeighbourTable(19.2, (10.1,2.1,16.2), "G0", 1.0, .85, .84)
Cassiopeia.eta.B.NeighbourTable(19.2, (10.1,2.1,16.2), "M0", .03, .52, .07)
Luyten205_128 = Star("Luyten 205-128")
Luyten205_128.NeighbourTable(19.2, (-.8,-10.3,-16.2), "M", .0002, .14)
HD36395 = Star("HD 36395")
HD36395.NeighbourTable(19.2, (2.6,19.0,-1.2), "M1", .02, .51, .69)
p40deg9712 = Star("40&deg; 9712")
p40deg9712.NeighbourTable(19.3, (-8.8,-11.5,-12.7), "M4", .003, .44, .29)
Ross986 = Star("Ross 986")
Ross986.NeighbourTable(19.3, (-4.3,-14.4,12), "M5")
Ross47 = Star("Ross 47")
Ross47.NeighbourTable(19.4, (1.7,18.9,4.2), "M6", .0008, .35, .17)
Wolf294 = Star("Wolf 294")
Wolf294.NeighbourTable(19.4, (-3.6,15.8,10.7), "M4", .008, .49, .46)
LP658_2 = Star("LP 658-2")
LP658_2.NeighbourTable(19.6, (.6,19.5,-1.4), "DK")
p53deg1320 = System("+53&deg; 1320",
                    A = Star("+53&deg; 1320 A"),
                    B = Star("+53&deg; 1320 B"))
p53deg1320.A.NeighbourTable(19.6, (-8.8,7.9,15.6), "M0")
p53deg1320.B.NeighbourTable(19.6, (-8.8,7.9,15.6), "M0")
VB10 = System("VB10", A = Star("VB10 A"), B = Star("VB10 B"))
VB10.A.NeighbourTable(19.6, (6.2,-18.5,1.7), "M4", None, .39, .43)
VB10.B.NeighbourTable(19.6, (6.2,-18.5,1.7), "M5", .007, None, .008)
m45deg13677 = Star("-45&deg; 13677")
m45deg13677.NeighbourTable(19.9, (7.5,-11.8,-14.1), "M0", .00002)
Eridanus.s82 = Star("82 Eridani")
Eridanus.s82.NeighbourTable(20.3, (9.6,11.2,-13.9), "G5")
Wolf630 = System("Wolf 630",
                 A = Star("Wolf 630 A"),
                 B = Star("Wolf 630 B"),
                 C = Star("Wolf 630 C"),
                 D = Star("Wolf 630 D"))
Wolf630.A.NeighbourTable(20.3, (-5.8,-19.2,-2.9), "M4", None, .38)
Wolf630.B.NeighbourTable(20.3, (-5.8,-19.2,-2.9), "M5", None, .38)
Wolf630.C.NeighbourTable(20.3, (-5.8,-19.2,-2.9))
Wolf630.D.NeighbourTable(20.3, (-5.8,-19.2,-2.9), "M4")
m11deg3759 = Star("-11&deg; 3759")
m11deg3759.NeighbourTable(20.4, (-15.7,-12.3,-4.4), "M4")
Hydra.beta = Star("&beta; Hydri")
Hydra.beta.NeighbourTable(20.5, (4.4,.4,-20.0), "G1", None, None, 1.66)
p45Fu46 = System("+45 Fu46", A = Star("+45 Fu46 A"), B = Star("+45 Fu46 B"))
p45Fu46.A.NeighbourTable(21.0, (-3.1,-14.4,15.0), "M3", None, .31)
p45Fu46.B.NeighbourTable(21.0, (-3.1,-14.4,15.0), None, None, .25)
p19deg5116 = System("+19&deg; 5116",
                    A = Star("+19&deg; 5116 A"),
                    B = Star("+19&deg; 5116 B"))
p19deg5116.A.NeighbourTable(21.0, (19.5,-3.4,7.1), "M4")
p19deg5116.B.NeighbourTable(21.0, (19.5,-3.4,7.1), "M6")


del Discovery, Spin, Orbit, Star, System, \
    Quantity, Float, kilo, mega, giga, micro, year, AU
