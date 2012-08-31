# -*- coding: iso-8859-1 -*-
"""The moons and rings of Neptune

It's also accompanied in its orbit by a host of trojans:
http://www.space.com/scienceastronomy/070130_st_neptune_trojans.html
The ones found to date are at its forward Lagrange point, L4; and the New
Horizons probe shall pass near its trailing one, L5, en route to Pluto.

See study.LICENSE for copyright and license information.
"""

from study.value.units import mega, metre, km
from common import Discovery
from body import Ring, Hoop
from rock import NASAmoon, NASAshell
from outer import Neptune

Triton = NASAmoon("Triton", Neptune, Discovery("Lassell", 1846),
                  354.76, -5.88, NASAshell(1353), "methane ice",
                  note="Suspected Kuiper Belt object")
Nereid = NASAmoon("Nereid", Neptune, Discovery("Kuiper", 1949),
                  5513.4, 360.14, NASAshell(170), "dirty ice",
                  note="Suspected Kuiper Belt object")
_tmp = Discovery("Terrile", 1989, source="Voyager 2")
Naiad = NASAmoon("Naiad", Neptune, _tmp, 48.23, .29, NASAshell(29))
Thalassa = NASAmoon("Thalassa", Neptune, _tmp, 50.07, .31, NASAshell(40))
_tmp = Discovery("Synnott", 1989, source="Voyager 2")
Despina = NASAmoon("Despina", Neptune, _tmp, 52.53, .33, NASAshell(74))
Galatea = NASAmoon("Galatea", Neptune, _tmp, 61.95, .43, NASAshell(79))
Proteus = NASAmoon("Proteus", Neptune, _tmp, 117.65, 1.12, NASAshell(218, 208, 201))
Larissa = NASAmoon("Larissa", Neptune, Discovery("Reitsema et al.", 1989, source="Voyager 2"),
                   73.55, .55, NASAshell(104, 89))
# Plus three more, with diameters of 30 to 40 km, discovered in 2003
# http://www.space.com/scienceastronomy/neptune_moons_030113.html

# rings discovered in the mid-1980s
Galle = Ring("Galle", Neptune, 41.7 * mega * metre, 41.86 * mega * metre, id="1989 N3R", width=15 * km)
leVerrier = Ring("le Verrier", Neptune, 48.85 * mega * metre, 52.15 * mega * metre, id="1989 N2R", width=15 * km)
Lassel = Ring("Lassel", Neptune, 52.3 * mega * metre, 55 * mega * metre)
Arago = Ring("Arago", Neptune, 56.4 * mega * metre, 57.4 * mega * metre)
Adams = Ring("Adams", Neptune, 62.7 * mega * metre, 62.86 * mega * metre, id="1989 N1R", # USGS:width < 50 km
             # USGS gives three arcs for that, all at radius 62.9 Mm, width unspecified
             leading=Hoop("Liberté", Neptune, 62.9 * mega * metre),
             equidistant=Hoop("Egalité", Neptune, 62.9 * mega * metre),
             following=Hoop("Fraternité", Neptune, 62.9 * mega * metre),
             Courage=Hoop("Courage", Neptune, 62.9 * mega * metre))

del NASAmoon, NASAshell, Ring, Hoop, Discovery, mega, metre, km, _tmp
