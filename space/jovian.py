# -*- coding: iso-8859-1 -*-
"""The moons of Jupiter.

$Id: jovian.py,v 1.1 2005-03-12 15:44:15 eddy Exp $
"""

from basEddy.units import metre, km, mega
from space.rock import NASAmoon, NASAshell, NamedOrbit, SAOmoon
from space.common import Discovery
from space.body import Ring
from space.outer import Jupiter

_tmp = Discovery("Galileo Galilei", 1610, location="Padua",
                 date="1610 January 8",
                 etymology="""Greek mythology

Galileo named Jupiter's satellites after some of Jupiter's many lovers:

 Io -- daughter of Inachus, an Arcadian river god; c.f. Themisto.  (Jove turned
       her into a cow, poor dear, to try to hide her from his wife Hera, who
       knew what he was up to, saw through the disguise and invented the gadfly
       to torment her.)

 Europa -- daughter of Agenor, king of Tyre. (Jove seduced her disguised as a
           bull: she climbed on his back and he swam off to Crete with her,
           where she bore him several children, including the famous king Minos;
           see Pasiphaë.)

 Ganymede -- cupbearer to the gods of Olympus.  (Abducted to there by Jove in
             the form of an eagle.  Apparently Hera didn't get jealous of boys.)

 Callisto -- Lycaon's daughter.  (Jove turned her into a bear in yet another
             attempt to keep his infidelities from Hera's attention.)
""",
                 story="""Galileo's world-changing observation of Jovian moons.

On January 8th, 1610, Galileo Galilei pointed a telescope at the night sky and
observed that Jupiter had some little companions; that night, he saw Ganymede
and Callisto; the following night, he added Europa and Io (USGS infers that he
saw a combined image of these two the first night and merely separated them on
the second night).  This was the decisive moment in history when the geocentric
(and anthropocentric) view of the universe became irreparably untennable.

It is (USGS assures me) probable that Simon Marius discovered the same moons at
about the same time, maybe as much as a month earlier: but Galileo published
first.
""")
Io = NASAmoon("Io", Jupiter, _tmp, 422, 1.77,
              NASAshell(1830, 1819, 1815), "silicates, sulphur, SO2", 893.3, 3.53)
Europa = NASAmoon("Europa", Jupiter, _tmp, 671, 3.55,
                  NASAshell(1565), "ice", 479.7, 2.99)
Ganymede = NASAmoon("Ganymede", Jupiter, _tmp, 1070, 7.15,
                    NASAshell(2634), "dirty ice", 1482, 1.94)
Callisto = NASAmoon("Callisto", Jupiter, _tmp, 1883, 16.69,
                    NASAshell(2403), "dirty ice", 1076, 1.851)
Amalthea = NASAmoon("Amalthea", Jupiter,
                    Discovery("E.E. Barnard", 1892, date="1892 September 9",
                              location="Mt. Hamilton",
                              etymology="""Greek Mythology.

Amalthea was a naiad, who nursed baby Jove.  Her pet goat fed him.
Flammarion suggested her name for this moon.
"""),
                    181, 0.50, NASAshell(131, 73, 67), "rock, sulphur")
Himalia = NASAmoon("Himalia", Jupiter,
                   Discovery("C.D. Perrine", 1904, date="1904 December 4",
                             location="Mt. Hamilton",
                             etymology="""Greek Mythology.

Himalia was a nymph from Rhodes.  She bore Jove three sons.
"""),
                   11480, 250.57, NASAshell(85), "carbonaceous")
Elara = NASAmoon("Elara", Jupiter,
                 Discovery("C.D. Perrine", 1905, date="1905 January 3",
                           location="Mt. Hamilton",
                           etymology="""Greek Mythology.

Elara, daughter of King Orchomenus.  Zeus was the father of her baby, the giant
Tityus (not to be confused with Bode, q.v.).
"""),
                 11737, 259.65, NASAshell(40), "carbonaceous")
Pasiphae = NASAmoon("Pasiphae", Jupiter,
                    Discovery("P.J. Melotte", 1908, discovery="1908 January 27",
                              location="Greenwich",
                              etymology="""Greek Mythology.

Pasiphaë was married to Minos, king of Crete.  Not shy of cuckolding his own
son, Jove appeared to her in the form of a bull - just like he'd done to her
mother in law - and left her with a baby minotaur to look after.
"""),
                    23500, -735, NASAshell(18))
Sinope = NASAmoon("Sinope", Jupiter,
                  Discovery("S.B. Nicholson", 1914, date="1914 July 21",
                            location="Mt. Hamilton",
                            etymology="""Greek Mythology.

Sinope's daddy was a river god, Asopus.  She managed to trick both Zeus and
Apollo with their own promises, when they came a-letching after her.  Zeus
'granted her' perpetual virginity for this.
"""),
                  23700, -758, NASAshell(14))
Lysithea = NASAmoon("Lysithea", Jupiter,
                    Discovery("S.B. Nicholson", 1938, date="1938 July 6",
                              location="Mt. Wilson",
                              etymology="""Greek Mythology.

Lysithea, a.k.a. Semele, was either Kadmos' daughter and Dionysos' mother (but
see also Thyone), or Evenus' daughter and Helenus' mother; either way, Zeus was
the bairn's father.
"""),
                    11720, 259.22, NASAshell(12), "carbonaceous")
Carme = NASAmoon("Carme", Jupiter,
                 Discovery("S.B. Nicholson", 1938, date="1938 July 30",
                           location="Mt. Wilson",
                           etymology="""Greek Mythology.

A nymph, attendant on Artemis; and mother (by Jove) of Britomartis.
"""),
                 22600, -692, NASAshell(15))
Ananke = NASAmoon("Ananke", Jupiter,
                  Discovery("S.B. Nicholson", 1951, date="1951 September 28",
                            location="Mt. Wilson",
                            etymology="""Greek Mythology.

Goddess of fate and necessity; mother (by Jove) of Adrastea.
"""),
                  21200, -631, NASAshell(10))
Leda = NASAmoon("Leda", Jupiter,
                Discovery("C.T. Kowal", 1974, date="1974 September 11",
                          location="Palomar",
                          etymology="""Greek Mythology.

Mother (by Jove, in the form of a swan) of Pollux and Helen.
('ang on - what about Castor, allegedly twin to Pollux ?)
"""),
                11094, 238.72, NASAshell(5))
Thebe = NASAmoon("Thebe", Jupiter,
                 Discovery("S. Synnott and Voyager Team", 1980, date="1979 March 5",
                           location="Voyager 1",
                           etymology="""Greek Mythology.

Grand-daughter (via an Egyptian king) of Io; mother (by Jove) of Aigyptos.
The Egyptian city of Thebes was named after her.
"""),
                 222, 0.67, NASAshell(55, 45), "rock?")
Adrastea = NASAmoon("Adrastea", Jupiter,
                    Discovery("D. Jewitt, E. Danielson", 1979, date="1979 July",
                              location="Voyager 2",
                              etymology="""Greek Mythology.

Cretan nymph who looked after baby Zeus.
Not to be confused with Ananke's daughter (presumably).
"""),
                    129, 0.30, NASAshell(13, 10, 8), "rock?")
Metis = NASAmoon("Metis", Jupiter,
                 Discovery("S. Synnott and Voyager Team", 1980, date="1979 March 4",
                           location="Voyager 1",
                           etymology="""Greek Mythology.

Zeus' first wife.  When she got pregnant, Zeus swallowed her.
Athena was subsequently born from his fore-head.
"""),
                 128, 0.29, NASAshell(20), "rock?")

Callirrhoe = NamedOrbit('Callirrhoe', Jupiter, 24103, 758.76,
                        aliases=('1999 J1',), discovery=Discovery(
    "J.V. Scotti, T.B. Spahr, R.S. McMillan, J.A. Larson, J. Montani, A.E. Gleason, T. Gehrels",
    1999, date="1999 October 19", location="Spacewatch",
    etymology="""Greek Mythology.

Daughter of Achelous, a river god.  Step-daughter of Zeus.
"""))

_tmp = Discovery("S.S. Shepard et al.", 2000, # well, they look pretty much like one event ...
                 location="Mauna Kea", date="2000 November, 23 and 25")
Themisto = NamedOrbit('Themisto', Jupiter, 7507, 130.02, aliases("1975 J1", '2000 J1'),
                      discovery=Discovery("C.T. Kowal, E. Roemer", 1975, date="1975 September 30",
                                          location="Palomar"), rediscovery=_tmp,
                      etymology="""Greek Mythology.

Daughter of Arcadian river god Inachus (c.f. Io), mother (by Jove) of Ister.
""")
Kalyke = NamedOrbit('Kalyke', Jupiter, 23746, 750.81, discovery=_tmp,
                    aliases=("2000 J2",), etymology="""Greek Mythology

Nymph, mother (by Jove) of Endymion.
""")
Iocaste = NamedOrbit('Iocaste', Jupiter, 20210, 585.17, discovery=_tmp,
                     aliases=("2000 J3",), etymology="""Greek Mythology

Wife of Laius, King of Thebes, and mother (allegedly by Jove) of Agamedes.
Rather more famously, she was mother (by Laius) of Oedipus.

Laius was rather terrified of losing his wife's affection if they ever had a
son, so tried to have the son she bore him killed; but he survived and was
raised ignorant of his parentage, under the name Oedipus.  One day, out on the
road between adventures, he met a road hog who was rash enough to pick a fight
with him - and lose, fatally.  Arriving shortly thereafter in Thebes, Oedipus
found the throne vacant, married the recently widowed Queen and thereby made it
into Sigmund Freud's index.  Thebes had rotten luck for a while, until the sages
declared that the gods were angry, the old servant who'd done an incomplete job
of bumping off the baby remembered details, everyone realized what had happened,
Iocaste killed herself, Oedipus tore his eyes out and nobody lived happily after
that at all.
""")
Erinome = NamedOrbit('Erinome', Jupiter, 22972, 713.52, discovery=_tmp,
                     aliases=("2000 J4",), etymology="""Greek Mythology

Daughter of Celes.  Venus compelled her to fall in love with Jove.
""")
Harpalyke = NamedOrbit('Harpalyke', Jupiter, 21336, 633.68, discovery=_tmp,
                       aliases=("2000 J5",), etymology="""Greek Mythology

Daughter of Clymenus, who also made her his wife; she wasn't happy about that,
killed the sone she bore him, cooked the corpse and served it up as his dinner.
He hanged himself and she got turned into a night bird, Chalkis - though the
latter is allegedly another of Jove's ways of showing his gratitude to
girlfriends (c.f. Io, Callisto).
""")
Isonoe = NamedOrbit('Isonoe', Jupiter, 23074, 718.71, discovery=_tmp,
                    aliases=("2000 J6",), etymology="""Greek Mythology

A Danaid, mother (by Jove) of Orchomenos.
""")
Praxidike = NamedOrbit('Praxidike', Jupiter, 21010, 619.19, discovery=_tmp,
                       aliases=("2000 J7",), etymology="""Greek Mythology

Goddess of punishment, mother (by Jove) of Klesios.
""")
Megaclite = NamedOrbit('Megaclite', Jupiter, 23618, 743.13, discovery=_tmp,
                       aliases=("2000 J8",), etymology="""Greek Mythology

Daughter of Macareus, mother (by Jove) of Thebe and Locrus.  We can only hope
this isn't the same Thebe whose sone Aigyptos claimed to be a son of Zeus, too.
""")
Taygete = NamedOrbit('Taygete', Jupiter, 22304, 682.68, discovery=_tmp,
                     aliases=("2000 J9",), etymology="""Greek Mythology

Daughter of Atlas, one of the Pleiades (who, him or her ?), mother (by Jove) of
Lakedaimon.
""")
Chaldene = NamedOrbit('Chaldene', Jupiter, 20174, 587.62, discovery=_tmp,
                      aliases=("2000 J10",), etymology="""Greek Mythology

Mother (by Jove) of Solymos.
""")
SAOmoon(Jupiter, _tmp, "J11", 12557, 286.95)

# USGS names several 2001/Dec/{9,11} Mauna Kea discoveries, also by Sheppard et al. (but no 2000 J11)

Ring("Jovian Halo Ring", Jupiter, 100 * mega * metre, 122.8 * mega * metre, 10, thickness = 20 * mega * metre)
Ring("Jovian Main Ring", Jupiter, 122.8 * mega * metre, 129.2 * mega * metre, thickness = 30 * km)
Ring("Jovian Disk Ring", Jupiter, 129.2 * mega * metre, 214.2 * mega * metre)

del Ring, Discovery, SAOmoon, NamedOrbit, NASAmoon, NASAshell, metre, km, mega, _tmp

_rcs_log = """
$Log: jovian.py,v $
Revision 1.1  2005-03-12 15:44:15  eddy
Initial revision

"""
