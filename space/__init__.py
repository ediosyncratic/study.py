# -*- coding: iso-8859-1 -*-
"""Data about stars, planets and other clutter in space.

This module exports two objects: a namespace, D, whose attributes are all the
heavenly bodies and related data of this package; and an immutable sequence,
Planets, whose entries are the nine planets of our Solar system.  For
convenience, Planets also has attributes inner, outer and final: inner is a
tuple of the four Earthoid inner planets, outer is a tuple of the four gas
giants and final is the Kuiper belt object that is commonly considered a planet.
Both D and Planets use lazy evaluation to avoid loading data you don't yet want.

Sources:
    Kaye & Laby: Tables of physical and chemical constants
    Patrick Moore: The new atlas of the universe
    Isaac Asimov: From Earth to Heaven
    Ronald Greeley, Raymond Batson: The Compact NASA Atlas of the Solar System (CUP, ISBN 0 521 80633X)
    Johnston's Archive: http://www.johnstonsarchive.net/astro/
    USGS: http://planetarynames.wr.usgs.gov/append8.html for radii, append7.html for discoveries
    NASA Astronomy Pictures Of the Day, http://antwrp.gsfc.nasa.gov/apod/*, listed as '/apod/...' below

Note (from the whatwg discussion on location): analogous to longitude
and latitude, the tidy way to encode altitude is as
   (radial co-ordinate / planet surface radius).evaluate(log) * radian
which has some interesting ramifications.  It would be best to
actually use gravitational potentials (and reverse the ratio) rather
than radii, since this provides the means to correct for the planet
not being spherical due to spin; sea level is a surface of constant
gravitational potential, and is our normal zero-point for altitude.

$Id: __init__.py,v 1.2 2005-03-13 15:11:28 eddy Exp $
"""

from basEddy import Lazy
class NameSpace (Lazy):
    # Home:
    def _lazy_get_Earth_(self, ig):
        from space.home import Earth
        return Earth
    def _lazy_get_Moon_(self, ig):
        from space.home import Moon
        return Moon

    # The Big Picture:
    def _lazy_get_MilkyWay_(self, ig):
        from space.home import MilkyWay
        return MilkyWay
    def _lazy_get_Universe_(self, ig):
        from space.home import Universe
        return Universe
    def _lazy_get_Sun_(self, ig):
        from space.home import Sun
        return Sun

    # Some units of measurement:
    def _lazy_get_AU_(self, ig):
        from space.home import AU
        return AU
    def _lazy_get_parsec_(self, ig):
        from space.home import parsec
        return parsec
    def _lazy_get_Month_(self, ig):
        from space.home import Month
        return Month

    # The Earthoid Planets:
    def _lazy_get_Mercury_(self, ig):
        from space.inner import Mercury
        return Mercury
    def _lazy_get_Venus_(self, ig):
        from space.inner import Venus
        return Venus
    def _lazy_get_Mars_(self, ig):
        from space.inner import Mars
        return Mars

    # The Gas Giants
    def _lazy_get_Jupiter_(self, ig):
        from space.outer import Jupiter
        return Jupiter
    def _lazy_get_Saturn_(self, ig):
        from space.outer import Saturn
        return Saturn
    def _lazy_get_Uranus_(self, ig):
        from space.outer import Uranus
        return Uranus
    def _lazy_get_Neptune_(self, ig):
        from space.outer import Neptune
        return Neptune

    # Jupiter's satellites:
    def _lazy_get_Io_(self, ig):
        from space.jovian import Io
        return Io
    def _lazy_get_Europa_(self, ig):
        from space.jovian import Europa
        return Europa
    def _lazy_get_Ganymede_(self, ig):
        from space.jovian import Ganymede
        return Ganymede
    def _lazy_get_Callisto_(self, ig):
        from space.jovian import Callisto
        return Callisto
    def _lazy_get_Amalthea_(self, ig):
        from space.jovian import Amalthea
        return Amalthea
    def _lazy_get_Himalia_(self, ig):
        from space.jovian import Himalia
        return Himalia
    def _lazy_get_Elara_(self, ig):
        from space.jovian import Elara
        return Elara
    def _lazy_get_Pasiphae_(self, ig):
        from space.jovian import Pasiphae
        return Pasiphae
    def _lazy_get_Sinope_(self, ig):
        from space.jovian import Sinope
        return Sinope
    def _lazy_get_Lysithea_(self, ig):
        from space.jovian import Lysithea
        return Lysithea
    def _lazy_get_Carme_(self, ig):
        from space.jovian import Carme
        return Carme
    def _lazy_get_Ananke_(self, ig):
        from space.jovian import Ananke
        return Ananke
    def _lazy_get_Leda_(self, ig):
        from space.jovian import Leda
        return Leda
    def _lazy_get_Thebe_(self, ig):
        from space.jovian import Thebe
        return Thebe
    def _lazy_get_Adrastea_(self, ig):
        from space.jovian import Adrastea
        return Adrastea
    def _lazy_get_Metis_(self, ig):
        from space.jovian import Metis
        return Metis
    def _lazy_get_Callirrhoe_(self, ig):
        from space.jovian import Callirrhoe
        return Callirrhoe
    def _lazy_get_Themisto_(self, ig):
        from space.jovian import Themisto
        return Themisto
    def _lazy_get_Kalyke_(self, ig):
        from space.jovian import Kalyke
        return Kalyke
    def _lazy_get_Iocaste_(self, ig):
        from space.jovian import Iocaste
        return Iocaste
    def _lazy_get_Erinome_(self, ig):
        from space.jovian import Erinome
        return Erinome
    def _lazy_get_Harpalyke_(self, ig):
        from space.jovian import Harpalyke
        return Harpalyke
    def _lazy_get_Isonoe_(self, ig):
        from space.jovian import Isonoe
        return Isonoe
    def _lazy_get_Praxidike_(self, ig):
        from space.jovian import Praxidike
        return Praxidike
    def _lazy_get_Megaclite_(self, ig):
        from space.jovian import Megaclite
        return Megaclite
    def _lazy_get_Taygete_(self, ig):
        from space.jovian import Taygete
        return Taygete
    def _lazy_get_Chaldene_(self, ig):
        from space.jovian import Chaldene
        return Chaldene

    # Saturn's satellites:
    def _lazy_get_Mimas_(self, ig):
        from space.saturnalia import Mimas
        return Mimas
    def _lazy_get_Enceladus_(self, ig):
        from space.saturnalia import Enceladus
        return Enceladus
    def _lazy_get_Tethys_(self, ig):
        from space.saturnalia import Tethys
        return Tethys
    def _lazy_get_Dione_(self, ig):
        from space.saturnalia import Dione
        return Dione
    def _lazy_get_Rhea_(self, ig):
        from space.saturnalia import Rhea
        return Rhea
    def _lazy_get_Titan_(self, ig):
        from space.saturnalia import Titan
        return Titan
    def _lazy_get_Hyperion_(self, ig):
        from space.saturnalia import Hyperion
        return Hyperion
    def _lazy_get_Iapetus_(self, ig):
        from space.saturnalia import Iapetus
        return Iapetus
    def _lazy_get_Phoebe_(self, ig):
        from space.saturnalia import Phoebe
        return Phoebe
    def _lazy_get_Janus_(self, ig):
        from space.saturnalia import Janus
        return Janus
    def _lazy_get_Epimetheus_(self, ig):
        from space.saturnalia import Epimetheus
        return Epimetheus
    def _lazy_get_Helene_(self, ig):
        from space.saturnalia import Helene
        return Helene
    def _lazy_get_Telesto_(self, ig):
        from space.saturnalia import Telesto
        return Telesto
    def _lazy_get_Calypso_(self, ig):
        from space.saturnalia import Calypso
        return Calypso
    def _lazy_get_Atlas_(self, ig):
        from space.saturnalia import Atlas
        return Atlas
    def _lazy_get_Prometheus_(self, ig):
        from space.saturnalia import Prometheus
        return Prometheus
    def _lazy_get_Pandora_(self, ig):
        from space.saturnalia import Pandora
        return Pandora
    def _lazy_get_Pan_(self, ig):
        from space.saturnalia import Pan
        return Pan

    # Uranus' satellites
    def _lazy_get_Ariel_(self, ig):
        from space.uranic import Ariel
        return Ariel
    def _lazy_get_Umbriel_(self, ig):
        from space.uranic import Umbriel
        return Umbriel
    def _lazy_get_Titania_(self, ig):
        from space.uranic import Titania
        return Titania
    def _lazy_get_Oberon_(self, ig):
        from space.uranic import Oberon
        return Oberon
    def _lazy_get_Miranda_(self, ig):
        from space.uranic import Miranda
        return Miranda
    def _lazy_get_Cordelia_(self, ig):
        from space.uranic import Cordelia
        return Cordelia
    def _lazy_get_Ophelia_(self, ig):
        from space.uranic import Ophelia
        return Ophelia
    def _lazy_get_Bianca_(self, ig):
        from space.uranic import Bianca
        return Bianca
    def _lazy_get_Cressida_(self, ig):
        from space.uranic import Cressida
        return Cressida
    def _lazy_get_Desdemona_(self, ig):
        from space.uranic import Desdemona
        return Desdemona
    def _lazy_get_Juliet_(self, ig):
        from space.uranic import Juliet
        return Juliet
    def _lazy_get_Portia_(self, ig):
        from space.uranic import Portia
        return Portia
    def _lazy_get_Rosalind_(self, ig):
        from space.uranic import Rosalind
        return Rosalind
    def _lazy_get_Belinda_(self, ig):
        from space.uranic import Belinda
        return Belinda
    def _lazy_get_Puck_(self, ig):
        from space.uranic import Puck
        return Puck
    def _lazy_get_Caliban_(self, ig):
        from space.uranic import Caliban
        return Caliban
    def _lazy_get_Sycorax_(self, ig):
        from space.uranic import Sycorax
        return Sycorax
    def _lazy_get_Prospero_(self, ig):
        from space.uranic import Prospero
        return Prospero
    def _lazy_get_Setebos_(self, ig):
        from space.uranic import Setebos
        return Setebos
    def _lazy_get_Stephano_(self, ig):
        from space.uranic import Stephano
        return Stephano

    # Neptune's satellites
    def _lazy_get_Triton_(self, ig):
        from space.neptunous import Triton
        return Triton
    def _lazy_get_Nereid_(self, ig):
        from space.neptunous import Nereid
        return Nereid
    def _lazy_get_Naiad_(self, ig):
        from space.neptunous import Naiad
        return Naiad
    def _lazy_get_Thalassa_(self, ig):
        from space.neptunous import Thalassa
        return Thalassa
    def _lazy_get_Despina_(self, ig):
        from space.neptunous import Despina
        return Despina
    def _lazy_get_Galatea_(self, ig):
        from space.neptunous import Galatea
        return Galatea
    def _lazy_get_Proteus_(self, ig):
        from space.neptunous import Proteus
        return Proteus
    def _lazy_get_Larissa_(self, ig):
        from space.neptunous import Larissa
        return Larissa
    def _lazy_get_Galle_(self, ig):
        from space.neptunous import Galle
        return Galle
    def _lazy_get_leVerrier_(self, ig):
        from space.neptunous import leVerrier
        return leVerrier
    def _lazy_get_Lassel_(self, ig):
        from space.neptunous import Lassel
        return Lassel
    def _lazy_get_Arago_(self, ig):
        from space.neptunous import Arago
        return Arago
    def _lazy_get_Adams_(self, ig):
        from space.neptunous import Adams
        return Adams

    # Moons of Mars:
    def _lazy_get_Phobos_(self, ig):
        from space.asteroid import Phobos
        return Phobos
    def _lazy_get_Deimos_(self, ig):
        from space.asteroid import Deimos
        return Deimos

    # Asteroids:
    def _lazy_get_Asteroids_(self, ig):
        from space.asteroid import Asteroids
        return Asteroids
    def _lazy_get_Ceres_(self, ig):
        from space.asteroid import Ceres
        return Ceres
    def _lazy_get_Albert_(self, ig):
        from space.asteroid import Albert
        return Albert
    def _lazy_get_Eros_(self, ig):
        from space.asteroid import Eros
        return Eros
    def _lazy_get_Amor_(self, ig):
        from space.asteroid import Amor
        return Amor
    def _lazy_get_Apollo_(self, ig):
        from space.asteroid import Apollo
        return Apollo
    def _lazy_get_Icarus_(self, ig):
        from space.asteroid import Icarus
        return Icarus
    def _lazy_get_Adonis_(self, ig):
        from space.asteroid import Adonis
        return Adonis
    def _lazy_get_Hermes_(self, ig):
        from space.asteroid import Hermes
        return Hermes

    # Few Kuiper belt objects:
    def _lazy_get_Pluto_(self, ig):
        from space.Kuiper import Pluto
        return Pluto
    def _lazy_get_Charon_(self, ig):
        from space.Kuiper import Charon
        return Charon
    def _lazy_get_Quaoar_(self, ig):
        from space.Kuiper import Quaoar
        return Quaoar
    def _lazy_get_Sedna_(self, ig):
        from space.Kuiper import Sedna
        return Sedna

    # The outer reaches ...
    def _lazy_get_Kuiper_(self, ig):
        from space.Kuiper import Kuiper
        return Kuiper
    def _lazy_get_Oort_(self, ig):
        from space.Kuiper import Oort
        return Oort
    def _lazy_get_Terminus_(self, ig):
        from space.Kuiper import Terminus
        return Terminus
    def _lazy_get_Gliese710_(self, ig):
        from space.Kuiper import Gliese710
        return Gliese710

class PlanetList (Lazy):
    def __len__(self): return 9
    def __getitem__(self, ind):
        if ind < 0 or ind > 8: raise IndexError, ind
        if ind < 4: return self.inner[ind]
        if ind < 8: return self.outer[ind - 4]
        assert ind == 8
        return self.final

    def __repr__(self):
        return repr(list(self.inner + self.outer + (self.final,)))

    def __getslice__(self, at, to=None, step=None):
        if step is None:
            step = 1
            if to is None: at, to = 0, at

        if step > 0:
            if to > len(self): to = len(self)
            while at < 0: at = at + step
        elif step < 0:
            if to < 0: to = -1
            while at > len(self): at = at + step
        else:
            raise ValueError, 'Zero step makes for a bad sequence ...'

        return map(self.__getitem__, range(at, to, step))

    def _lazy_get_inner_(self, ignored):
        from space.inner import Mercury, Venus, Earth, Mars
        return ( Mercury, Venus, Earth, Mars )

    def _lazy_get_outer_(self, ignored):
        from space.outer import Jupiter, Saturn, Uranus, Neptune
        return ( Jupiter, Saturn, Uranus, Neptune )

    def _lazy_get_final_(self, ignored):
        # With hind-sight, perhaps Pluto should never have been declared a planet ...
        from space.Kuiper import Pluto
        return Pluto

D = NameSpace()
Planets = PlanetList()

del NameSpace, PlanetList, Lazy

_rcs_log = """
$Log: __init__.py,v $
Revision 1.2  2005-03-13 15:11:28  eddy
Moved PlanetList and Planets from home.py and large comment from common.py;
added NameSpace and its instance, D; documented it and Planets.

Initial Revision 1.1  2005/03/12 12:58:26  eddy
"""
