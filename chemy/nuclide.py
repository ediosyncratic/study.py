"""Decay data relating to unstable isotopes of elements.

I'm not entirely clear where I found much of this data; it may have come from
Kaye and Laby.  In any case, I suspect I mis-understood the source, so the
result is somewhat suspect.  In places it conflicts with my Nuffield Advanced
Science data book (NAS); where the latter enabled me to make sense of what I'd
got, I adapted to suit it.

See study.LICENSE for copyright and license information.
"""

from element import Isotope, alpha
from particle import eV, Photon, proton, neutron, electron
from study.value.units import milli, second, minute, day, year
from decay import ratedDecay

def decay(what, halflife, *modes):
    what.decays = ratedDecay(what, halflife,
                             *[(m[0], m[1] * eV) + tuple(m[2:]) for m in modes])

def photon(erg):
    return Photon(energy= erg * eV)

positron = electron.anti

# NAS table NUC, pp 37--41 lists conflicting data; not sure whence these come !
# In places I've adjusted data to match what NAS said
decay(Isotope(1, 2).atom.nucleus, 12.35 * year,
      (1, 18.62e3, electron, Isotope(2, 1).atom.nucleus))

decay(Isotope(2, 4).atom.nucleus, .81 * second,
      # NAS gave energy as 5e6
      (1, 3.51e6, electron, Isotope(3, 3).atom.nucleus))
decay(Isotope(2, 6).atom.nucleus, .122 * second,
      # 88% beta- 9.7; 12% n; 88% gamma .981 ???
      (.98, 9.7e6 -.981e6, electron, photon(.981e6), neutron, Isotope(3, 4).atom.nucleus),
      (.02, 9.7e6, electron, neutron, Isotope(3, 4).atom.nucleus))

decay(Isotope(3, 5).atom.nucleus, .844 * second,
      (1, 13.1e6, electron, Isotope(4, 4).atom.nucleus))
decay(Isotope(3, 6).atom.nucleus, .178 * second,
      # NAS gives 13.6 MeV beta-, followed 75% of the time by a .76 MeV neutron
      # 65% beta- 13.6; 35% n, 2 alpha ?? [the 2 alpha shall be (3,5)'s decay via (4,4)]
      (.65, 13.6e4, electron, Isotope(4, 5).atom.nucleus),
      (.35, 0, neutron, Isotope(3, 5).atom.nucleus))
decay(Isotope(3, 8).atom.nucleus, 8.5e-3 * second,
      # 39% beta- 20.4; 61% n
      (.39, 20.4e6, electron, Isotope(4, 7).atom.nucleus),
      (.61, 0, neutron, Isotope(3, 7).atom.nucleus)) # ???

Isotope(4, 2).atom.nucleus.halflife = .4 * second # unspecified decay
decay(Isotope(4, 3).atom.nucleus, 53.3 * day,
      # 100% K; 10% gamma .477 ???
      (1, 47.7e3, positron, Isotope(3, 4).atom.nucleus))
decay(Isotope(4, 4).atom.nucleus, 7e-17 * second, (1, 0, alpha, alpha))
decay(Isotope(4, 6).atom.nucleus, 1.6e6 * year, # NAS gives 2.5e6 * year
      (1, 556e3, electron, Isotope(5, 5).atom.nucleus))
decay(Isotope(4, 7).atom.nucleus, 13.6 * second,
      # NAS gives 11.5 MeV beta-, followed 32% of the time by a 2.14 MeV gamma
      # 57% beta- 11.5; 33% gamma 2.12; 5% gamma 6.79; 2% gamma 5.85
      (.57, 11.5e6, electron, Isotope(5, 6).atom.nucleus),
      (.33, 2.12e6, Isotope(4, 7).atom.nucleus),
      (.05, 6.79e6, Isotope(4, 7).atom.nucleus),
      (.02, 5.85e6, Isotope(4, 7).atom.nucleus))
decay(Isotope(4, 8).atom.nucleus, .0114 * second,
      # NAS gives 12 Mev beta- plus a photon
      (1, 0, electron, neutron, Isotope(5, 6).atom.nucleus))

decay(Isotope(5, 3).atom.nucleus, .77 * second,
      # 93% beta+ 13.7; 100% 2 alpha
      (.93, 13.7e6, positron, Isotope(4, 4).atom.nucleus),
      (.07, 1.6e6, alpha, Isotope(3, 1))) # ???
decay(Isotope(5, 7).atom.nucleus, .02 * second,
      # 97% beta- 13.37; 2% alpha; 1% gamma 4.439
      (.97, 13.37e6, electron, Isotope(6, 6).atom.nucleus),
      (.01, 13.37e6 -4.439e6, electron, photon(4.439e6), Isotope(6, 6).atom.nucleus),
      (.02, 0, alpha, Isotope(3, 5).atom.nucleus))
decay(Isotope(5, 8).atom.nucleus, 17e-3 * second,
      # 92% beta- 13.44; 8% gamma 3.68
      (.92, 13.44e6, electron, Isotope(6, 7).atom.nucleus),
      (.08, 13.44e6 -3.68e6, electron, photon(3.68e6), Isotope(6, 7).atom.nucleus))
decay(Isotope(5, 9).atom.nucleus, 16e-3 * second,
      # 87% beta- 14.5; 90% gamma 6.09; 9% gamma 6.73
      (.87, 14.5e6, electron, Isotope(6, 8).atom.nucleus),
      (.9, 14.5e6 -6.09e6, electron, photon(6.09e6), Isotope(6, 8).atom.nucleus),
      (.09, 14.5e6 -6.73, electron, photon(6.73), Isotope(6, 8).atom.nucleus))

decay(Isotope(6, 3).atom.nucleus, .127 * second,
      # NAS gives 8.2 MeV p followed by 0.05 MeV alpha
      # beta+; 100% p
      (1, 0, positron, proton, Isotope(4, 4).atom.nucleus))
decay(Isotope(6, 4).atom.nucleus, 19.15 * second,
      # 99% beta+ 1.87; 99% gamma .718; 1% gamma 1.022
      (.99, 1.87e6, positron, photon(.718e6), Isotope(5, 5).atom.nucleus),
      (.01, .848e6, positron, photon(.718e6), photon(1.022e6), Isotope(5, 5).atom.nucleus))
decay(Isotope(6, 5).atom.nucleus, 20.38 * minute,
      (1, .96e6, positron, Isotope(5, 6).atom.nucleus))
decay(Isotope(6, 8).atom.nucleus, 5730 * year,
      (1, .156e6, electron, Isotope(7, 7).atom.nucleus))
decay(Isotope(6, 9).atom.nucleus, 2.45 * second,
      # 32% beta- 9.77; 68% beta- 4.47; 68% gamma 5.299
      (.32, 9.77e6, electron, Isotope(7, 8).atom.nucleus),
      (.68, 4.47e6, electron, photon(5.299e6), Isotope(7, 8).atom.nucleus))
decay(Isotope(6, 10).atom.nucleus, .75 * second,
      # NAS gives only beta- followed by neutron
      # 84% beta- 5.4; 100% n ???
      (.84, 5.4e6, electron, neutron, Isotope(7, 8).atom.nucleus),
      (.16, 0, neutron, Isotope(6, 9).atom.nucleus))

decay(Isotope(7, 5).atom.nucleus, .011 * second,
      # NAS only gives the beta-, followed 3% of the time by 3*alpha
      # 94% beta+ 16.3; 4% alpha; 2% gamma 4.439
      (.94, 16.3e6, positron, Isotope(6, 6).atom.nucleus),
      (.04, 0, alpha, Isotope(5, 3).atom.nucleus),
      (.02, 4.439e6, Isotope(7, 5).atom.nucleus))
decay(Isotope(7, 6).atom.nucleus, 9.96 * minute,
      (1, 1.19e6, positron, Isotope(6, 7).atom.nucleus))
decay(Isotope(7, 9).atom.nucleus, 7.14 * second,
      # 26% beta- 10.42; 68% beta- 4.29; 69% gamma 6.129; 5% gamma 7.115; 1% gamma .871
      (.32, 10.42e6, electron, Isotope(8, 8).atom.nucleus),
      (.68, 10.42e6 -6.13e6, electron, photon(6.13e6), Isotope(8, 8).atom.nucleus))
decay(Isotope(7, 10).atom.nucleus, 4.17 * second,
      # NAS gives 8.68 MeV beta-, followed 95% of the time by 1.21 MeV neutron
      # 2% beta- 8.7; 50% beta- 3.3; 95% n; 3% gamma .871 ???
      (.02, 8.68e6, electron, Isotope(8, 9).atom.nucleus),
      (.5, 3.3e6, electron, neutron, Isotope(8, 8).atom.nucleus))
decay(Isotope(7, 11).atom.nucleus, .63 * second,
      # NAS gives 9.4 MeV beta- followed by 1.98 MeV gamma
      # 100% beta- 9.6; 100% 1.982; 72% gamma .82; 72% gamma 1.65
      (1, 9.6e6, electron, Isotope(8, 10).atom.nucleus))

decay(Isotope(8, 5).atom.nucleus, 8.7e-3 * second,
      # NAS gives beta+ always followed by 6.97 MeV proton
      # beta+ 16.7; 12% p
      (.88, 16.7e6, positron, Isotope(7, 6).atom.nucleus),
      (.12, 16.7e6, positron, proton, Isotope(6, 6).atom.nucleus))
decay(Isotope(8, 6).atom.nucleus, 70.6 * second,
      # 1% beta+ 4.12; 99% beta+ 1.81; 99% gamma 2.313
      (.01, 4.12e6, positron, Isotope(7, 7).atom.nucleus),
      (.99, 1.81e6, photon(2.313e6), positron, Isotope(7, 7).atom.nucleus))
decay(Isotope(8, 7).atom.nucleus, 122.1 * second,
      (1, 1.73e6, positron, Isotope(7, 8).atom.nucleus))
# NAS has more data ...

what = Isotope(110, 159).atom.nucleus
what.decays = ratedDecay(what, .27 * milli * second,
                         (1, None, alpha, Isotope(108, 157).atom.nucleus))

del what, Isotope, alpha, eV, Photon, proton, neutron, electron, positron, \
    ratedDecay, decay, photon, second, minute, day, year, milli
