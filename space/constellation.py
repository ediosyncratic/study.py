# -*- coding: iso-8859-1 -*-
"""Constellations.

These are arbitrary groupings of apparent stars (some of which may be galaxies)
within the sky, loosely related by position in the sky as seen from Earth
(though their distances from Earth may be vastly different).  None the less,
they play a part in the naming of stars and galaxies, serving as name-space
objects.
"""
from body import Constellation

Centaur = Constellation("Centaurus")
Cetus = Constellation("Cetus")
Eridanus = Constellation("Eridanus")
Cygnus = Constellation("Cygnus", aliases=("The Swan",))
Indus = Constellation("Indus")
Ophiuchus = Constellation("Ophiuchus")
Cassiopeia = Constellation("Cassiopeia")
UrsaMajor = Constellation("Ursa Majoris", aliases=("The Great Bear", "The Plough"))
Draco = Constellation("Draco", aliases=("The Dragon",))
CanisMinor = Constellation("Canis Minor")
Pavonis = Constellation("Pavonis")
Hydra = Constellation("Hydra")

del Constellation
