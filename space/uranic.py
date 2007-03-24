# -*- coding: iso-8859-1 -*-
"""The moons and rings of Uranus.

$Id: uranic.py,v 1.3 2007-03-24 22:42:21 eddy Exp $
"""

from study.value.units import mega, metre, km, tophat
from common import Discovery
from body import Ring
from rock import NASAmoon, NASAshell, SAOmoon
from outer import Uranus

_tmp = Discovery("Lassell", 1851)
Ariel = NASAmoon("Ariel", Uranus, _tmp, 191.02, 2.52,
                 NASAshell(581, 578, 578), "dirty ice", 13.53, 1.67)
Umbriel = NASAmoon("Umbriel", Uranus, _tmp, 266.30, 4.14,
                   NASAshell(585), "dirty ice", 11.72, 1.4)
_tmp = Discovery("Herschel", 1787)
Titania = NASAmoon("Titania", Uranus, _tmp, 435.91, 8.70,
                   NASAshell(789), "dirty ice", 35.27, 1.71)
Oberon = NASAmoon("Oberon", Uranus, _tmp, 583.52, 13.46,
                  NASAshell(761), "dirty ice", 30.14, 1.63)
Miranda = NASAmoon("Miranda", Uranus, Discovery("Kuiper", 1948), 129.39, 1.41,
                   NASAshell(240, 234, 233), "dirty ice", .659, 1.2)
_tmp = Discovery("Terrile", 1986, source="Voyager 2")
Cordelia = NASAmoon("Cordelia", Uranus, _tmp, 49.77, .34, NASAshell(13))
Ophelia = NASAmoon("Ophelia", Uranus, _tmp, 53.79, .38, NASAshell(15))
Bianca = NASAmoon("Bianca", Uranus, Discovery("Voyager 2", 1986), 59.17, .43, NASAshell(21))
_tmp = Discovery("Synnott", 1986, source="Voyager 2")
Cressida = NASAmoon("Cressida", Uranus, _tmp, 61.78, .46, NASAshell(31))
Desdemona = NASAmoon("Desdemona", Uranus, _tmp, 62.68, 0.47, NASAshell(27))
Juliet = NASAmoon("Juliet", Uranus, _tmp, 64.35, .49, NASAshell(42))
Portia = NASAmoon("Portia", Uranus, _tmp, 66.09, 0.51, NASAshell(54))
Rosalind = NASAmoon("Rosalind", Uranus, _tmp, 69.94, .56, NASAshell(27))
Belinda = NASAmoon("Belinda", Uranus, _tmp, 75.26, .62, NASAshell(33))
Puck = NASAmoon("Puck", Uranus, Discovery("Synnott", 1985), 86.01, .76, NASAshell(77), "carbonaceous?")
_tmp = Discovery("Gladman et al.", 1997)
Caliban = NASAmoon("Caliban", Uranus, _tmp, 7169, -579.47, NASAshell(30))
Sycorax = NASAmoon("Sycorax", Uranus, _tmp, 12214, -1283.27, NASAshell(60))

Prospero = SAOmoon(Uranus, Discovery("Holman et al.", 1999), "U", 16665, 2037.14, "Prospero")
Setebos = SAOmoon(Uranus, Discovery("Kavelaars et al.", 1999), "U", 17879, 2273.34, "Setebos")
Stephano = SAOmoon(Uranus, Discovery("Gladman et al.", 1999), "U", 7979, 673.56, "Stephano")

NASAmoon("S/1986 U10", Uranus, Discovery("Karkoschka", 1999), 76.4, .64, NASAshell(40))

Ring("Uranus' 1986 U2R", Uranus, 38 * mega * metre, 41 * mega * metre, width=2.5 * mega * metre)
# From NASA I had 33 to 41 Mm radius; can't remember if measured off diagram or taken from text.
# USGS gives 1986 U2R at radius 38 Mm, width "2500?" km.
# NASA book seems to have indicated widths between 22 and 93 km for the rest; I've used USGS data.
Ring("Uranus' 6 Ring", Uranus, 41.84 * mega * metre, 41.842 * mega * metre, width=2 * (1 + tophat) * km)
Ring("Uranus' 5 Ring", Uranus, 42.23 * mega * metre, 42.233 * mega * metre, width=(2.5 + tophat) * km)
Ring("Uranus' 4 Ring", Uranus, 42.58 * mega * metre, 42.583 * mega * metre, width=(2.5 + tophat) * km)
Ring("Uranus' alpha Ring", Uranus, 44.72 * mega * metre, 44.82 * mega * metre, width=(9.5 + 5 * tophat) * km)
Ring("Uranus' beta Ring", Uranus, 45.67 * mega * metre, 45.68 * mega * metre, width=(9.5 + 5 * tophat) * km)
Ring("Uranus' eta Ring", Uranus, 47.19 * mega * metre, 47.192 * mega * metre, width=2 * (.5 + tophat) * km)
Ring("Uranus' gamma Ring", Uranus, 47.63 * mega * metre, 47.633 * mega * metre, width=(2.5 + 3 * tophat) * km)
Ring("Uranus' delta Ring", Uranus, 48.29 * mega * metre, 48.296 * mega * metre, width=6 * (1 + tophat) * km)
Ring("Uranus' 1986 U1R", Uranus, 50.02 * mega * metre, 50.022 * mega * metre, width=(1.5 + tophat) * km)
Ring("Uranus' epsilon Ring", Uranus, 51 * mega * metre, 51.1 * mega * metre, width=20 * (3 + 4 * tophat) * km)

del Discovery, Ring, NASAmoon, NASAshell, SAOmoon, mega, metre, km, tophat, _tmp
