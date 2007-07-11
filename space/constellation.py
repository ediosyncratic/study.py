# -*- coding: iso-8859-1 -*-
"""Constellations.

These are arbitrary groupings of apparent stars (some of which may be galaxies,
clusters or several stars too close together to distinguish by eye) within the
sky, loosely related by position in the sky as seen from Earth (though their
distances from Earth may be vastly different).  None the less, they play a part
in the naming of stars and galaxies, serving as name-space objects.
"""
from body import Constellation

CanisMinor = Constellation("Canis Minor")
Cassiopeia = Constellation("Cassiopeia")
Centaur = Constellation("Centaurus")
Cetus = Constellation("Cetus")
Cygnus = Constellation("Cygnus", aliases=("The Swan",))
Draco = Constellation("Draco", aliases=("The Dragon",))
Eridanus = Constellation("Eridanus")
Hydra = Constellation("Hydra")
Indus = Constellation("Indus")
Ophiuchus = Constellation("Ophiuchus")
Pavonis = Constellation("Pavonis")
UrsaMajor = Constellation("Ursa Majoris", aliases=("The Great Bear", "The Plough"))

del Constellation
