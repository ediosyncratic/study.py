# -*- coding: iso-8859-1 -*-
"""The moons and rings of Saturn.

$Id: saturnalia.py,v 1.1 2005-03-12 15:51:02 eddy Exp $
"""

from basEddy.units import mega, metre, km, tophat
from space.common import Discovery
from space.body import Ring, Hoop
from space.rock import NASAmoon, NASAshell, NASAtrojan, SAOmoon
from space.outer import Saturn

_tmp = Discovery("Herschel", 1789)
Mimas = NASAmoon("Mimas", Saturn, _tmp, 185.52, 0.94,
                 NASAshell(209, 196, 191), "ice", 0.375, 1.14)
Enceladus = NASAmoon("Enceladus", Saturn, _tmp, 238.02, 1.37,
                     NASAshell(256, 247, 245), "pure ice", 0.73, 1.12)
_tmp = Discovery("Cassini", 1684)
Tethys = NASAmoon("Tethys", Saturn, _tmp, 294.66, 1.89,
                  NASAshell(536, 528, 526), "ice", 6.22, 1.00)
Dione = NASAmoon("Dione", Saturn, _tmp, 377.40, 2.74,
                 NASAshell(560), "ice", 10.52, 1.44)
Rhea = NASAmoon("Rhea", Saturn, Discovery("Cassini", 1672),
                527.04, 4.52, NASAshell(764), "ice", 23.1, 1.24)
Titan = NASAmoon("Titan", Saturn, Discovery("Huygens", 1655),
                 1221.83, 15.94, NASAshell(2575), "cloudy atmosphere", 1345.5, 1.881)
Hyperion = NASAmoon("Hyperion", Saturn, Discovery("Bond, Lassell", 1848),
                    1481.1, 21.28, NASAshell(180, 140, 113), "dirty ice")
Iapetus = NASAmoon("Iapetus", Saturn, Discovery("Cassini", 1671), 3561.3, 79.33,
                   NASAshell(718), "ice/carbonaceous", 15.9, 1.02)
Phoebe = NASAmoon("Phoebe", Saturn, Discovery("Pickering", 1898),
                  12952, -550.48, NASAshell(110), "carbonaceous?",
                  note="Suspected Kuiper Belt object")
Janus = NASAmoon("Janus", Saturn, Discovery("Dollfus", 1966, confirmed=1980, source="Voyager 1"),
                 151.47, 0.69, NASAshell(97, 95, 77), "ice?", 0.0198, 0.65)
Epimetheus = NASAmoon("Epimetheus", Saturn, Discovery("Fountain et al.", 1980, source="Voyager 1"),
                      151.42, 0.69, NASAshell(69, 55, 55), "ice?", .0055, .63)
Helene = NASAtrojan("Helene", Discovery("Laques, Lecacheux", 1980, source="Voyager 1"),
                    Dione, '+', NASAshell(18, 16, 15), "ice?")
# had to guess which of these leads and which follows ...
Telesto = NASAtrojan("Telesto", Discovery("Smith et al.", 1980, source="Voyager 1"),
                     Tethys, '-', NASAshell(15, 12.5, 7.5), "ice?")
Calypso = NASAtrojan("Calypso", Discovery("Pascu et al.", 1980, source="Voyager 1"),
                     Tethys, '+', NASAshell(15, 8, 8), "ice?")
Atlas = NASAmoon("Atlas", Saturn, Discovery("Terrile", 1980, source="Voyager 1"),
                 137.67, 0.60, NASAshell(18.5, 17.2, 13.5), "ice?")
_tmp = Discovery("Collins", 1980, source="Voyager 1")
Prometheus = NASAmoon("Prometheus", Saturn, _tmp,
                      139.35, 0.61, NASAshell(74, 50, 34), "ice?", .001, .27)
Pandora = NASAmoon("Pandora", Saturn, _tmp,
                   141.70, 0.63, NASAshell(55, 44, 31), "ice?", .001, .42)
Pan = NASAmoon("Pan", Saturn, Discovery("Showalter", 1990),
               133.58, 0.58, NASAshell(10))

_glad, _kav = Discovery("Gladman", 2000), Discovery("Gladman, Kavelaars", 2000)
SAOmoon(Saturn, _glad, "S1", 23076, 1310.60)
SAOmoon(Saturn, _glad, "S2", 15172, 685.89)
SAOmoon(Saturn, _kav, "S3", 17251, 826.06)
SAOmoon(Saturn, _kav, "S4", 18231, 924.58)
SAOmoon(Saturn, _glad, "S5", 11339, 447.77)
SAOmoon(Saturn, _kav, "S6", 11465, 453.05)
SAOmoon(Saturn, _kav, "S7", 20144, 1068.06)
SAOmoon(Saturn, _kav, "S8", 15676, 730.84)
SAOmoon(Saturn, _kav, "S9", 18486, 939.90)
SAOmoon(Saturn, _kav, "S10", 17452, 860.03)
SAOmoon(Saturn, Discovery("Holman", 2000), "S11", 17874, 888.54)
SAOmoon(Saturn, _glad, "S12", 19747, 1038.11)

Ring("Saturn's D Ring", Saturn, 67 * mega * metre, 74.5 * mega * metre)
Ring("Saturn's C Ring", Saturn, 74.5 * mega * metre, 92 * mega * metre,
     Columbo=Hoop("Columbo Gap", Saturn, 77.8 * mega * metre, width=100 * km),
     Maxwell=Hoop("Maxwell Gap", Saturn, 87.5 * mega * metre, width=270 * km))
Ring("Saturn's B Ring", Saturn, 92 * mega * metre, 117.5 * mega * metre)
Ring("The Cassini Division", Saturn, 117.5 * mega * metre, 122.2 * mega * metre,
     width = 4.7 * mega * metre,
     note="gap between rings, not actually a ring; and not actually quite empty, either",
     Huygens=Hoop("Huygens Gap", 117.68 * mega * metre, width=(362.5 + tophat * 77.5) * km)) # width "285-440" km
Ring("Saturn's A Ring", Saturn, 122.2 * mega * metre, 136.8 * mega * metre,
     Encke=Hoop("Encke Division", 133.57 * mega * metre, width=325 * km),
     Keeler=Hoop("Keeler Gap", 136.53 * mega * metre, width=35 * km))
Ring("Saturn's F Ring", Saturn, 140.0775 * mega * metre, 140.3425 * mega * metre,
     # radius 140.21 Mm, width 30 to 500 km
     width = (265 + tophat * 235) * km)
Ring("Saturn's E Ring", Saturn, 180 * mega * metre, 480 * mega * metre)
# Average thickness: c. 100 m.  If all gathered together, they'd form a body
# only 500 km in diameter (and the NASA book uses "diameter", in some of its
# data tables, as a synonym for "radius" - d'oh).  Diagrams also show an
# "unnamed" object in orbit at 118 Mm, shepherding the B ring.

del Discovery, Ring, Hoop, NASAmoon, NASAshell, NASAtrojan, SAOmoon, _glad, _kav, _tmp, mega, metre, km, tophat

_rcs_log = """
$Log: saturnalia.py,v $
Revision 1.1  2005-03-12 15:51:02  eddy
Initial revision

"""
