# -*- coding: iso-8859-1 -*-
"""Data about stars, planets and other clutter in space.

Exported objects:
  D -- a namespace; attributes are all the heavenly bodies and related data of
       this package
  Planets -- immutable sequence listing the eight planets of our Solar system;
             has attributes inner (the four Earthoid inner planets) and outter
             (the four gas giants).
Each uses lazy evaluation to avoid loading data you don't yet want.

Modules:
  asteroid -- the asteroids, including Mars's moons
  Bode -- experiments with generalizing the Titius-Bode law
  body -- internal classes describing heavenly bodies
  common -- internal classes describing miscellaneous things
  galaxy -- some galaxies
  hole -- description of a black hole (Schwarzschild solution)
  home -- universe, local group, milky way, Sun, Earth and Moon
  inner -- Mercury, Venus and Mars; also borrows Earth off home
  jovian -- Jupiter's moons
  Kuiper -- the outer solar system
  ladder -- description of a synchronously orbiting space elevator
  neptunous -- Neptune's moons
  outer -- Jupiter, Saturn, Uranus and Neptune
  rock -- internal classes mapping particular data sources to the classes in
          body and common
  saturnalia -- Saturn's moons
  star -- assorted stars
  uranic -- Uranus's moons
  zoom -- description of motion at constant accelleration

Sources:
    Kaye & Laby: Tables of physical and chemical constants
    Patrick Moore: The new atlas of the universe
    Isaac Asimov: From Earth to Heaven
    Ronald Greeley, Raymond Batson: The Compact NASA Atlas of the Solar System (CUP, ISBN 0 521 80633X)
    Johnston's Archive: http://www.johnstonsarchive.net/astro/
    USGS: http://planetarynames.wr.usgs.gov/append8.html for radii, append7.html for discoveries
    NASA Astronomy Pictures Of the Day, http://antwrp.gsfc.nasa.gov/apod/*, listed as '/apod/...' below

Note (from the whatwg discussion on location): analogous to longitude and
latitude, the tidy way to encode altitude is as
   (radial co-ordinate / planet surface radius).evaluate(log) * radian
which has some interesting ramifications.  It would be best to actually use
gravitational potentials (and reverse the ratio) rather than radii, since this
provides the means to correct for the planet not being spherical due to spin;
sea level is a surface of constant gravitational potential, and is our normal
zero-point for altitude.  Since this depends logarithmically on radius, it may
make a good replacement for Bode-style description.

See also:
    Destroy the Earth -- http://www.exitmundi.nl/exitmundi.htm
    Extrasolar Planets -- http://antwrp.gsfc.nasa.gov/apod/ap010817.html

Note: brightness of stars is given as 'magnitude' on a logarithmic scale where
five units is a factor of 100; i.e. the unit of magnitude is 100**.2, just over
2.5.

Todo: replace all the source-specific constructors.  Instead, have minimal
constructors for objects whose classes support 'fill in my details' methods
specific to the relevant sources.  Causes all manner of grief: sub-bands of
Saturn's rings need to know that's what they are before they know their radii or
those of the bands in which they're implanted.  Has some nice effects, though: a
minimal structure can be built that fills itself in on demand by imporing
modules that just cause the side effects of refining data.  Requires total
revolution.  Also demands that I find the assorted sources again so as to record
each separately, as the present data is a mish-mash of them all.

$Id: __init__.py,v 1.9 2007-07-07 18:28:02 eddy Exp $
"""

from study.snake.lazy import Lazy
class NameSpace (Lazy):
    # Home:
    def _lazy_get_Earth_(self, ig):
        from home import Earth
        return Earth
    def _lazy_get_Moon_(self, ig):
        from home import Moon
        return Moon

    # The Big Picture:
    def _lazy_get_Universe_(self, ig):
        from home import Universe
        return Universe
    def _lazy_get_MilkyWay_(self, ig):
        from home import MilkyWay
        return MilkyWay
    def _lazy_get_Sun_(self, ig):
        from home import Sun
        return Sun

    # Some units of measurement:
    def _lazy_get_AU_(self, ig):
        from home import AU
        return AU
    def _lazy_get_parsec_(self, ig):
        from home import parsec
        return parsec
    def _lazy_get_Month_(self, ig):
        from home import Month
        return Month

    # The Earthoid Planets:
    def _lazy_get_Mercury_(self, ig):
        from inner import Mercury
        return Mercury
    def _lazy_get_Venus_(self, ig):
        from inner import Venus
        return Venus
    def _lazy_get_Mars_(self, ig):
        from inner import Mars
        return Mars

    # The Gas Giants
    def _lazy_get_Jupiter_(self, ig):
        from outer import Jupiter
        return Jupiter
    def _lazy_get_Saturn_(self, ig):
        from outer import Saturn
        return Saturn
    def _lazy_get_Uranus_(self, ig):
        from outer import Uranus
        return Uranus
    def _lazy_get_Neptune_(self, ig):
        from outer import Neptune
        return Neptune

    # Jupiter's satellites:
    def _lazy_get_Io_(self, ig):
        from jovian import Io
        return Io
    def _lazy_get_Europa_(self, ig):
        from jovian import Europa
        return Europa
    def _lazy_get_Ganymede_(self, ig):
        from jovian import Ganymede
        return Ganymede
    def _lazy_get_Callisto_(self, ig):
        from jovian import Callisto
        return Callisto
    def _lazy_get_Amalthea_(self, ig):
        from jovian import Amalthea
        return Amalthea
    def _lazy_get_Himalia_(self, ig):
        from jovian import Himalia
        return Himalia
    def _lazy_get_Elara_(self, ig):
        from jovian import Elara
        return Elara
    def _lazy_get_Pasiphae_(self, ig):
        from jovian import Pasiphae
        return Pasiphae
    def _lazy_get_Sinope_(self, ig):
        from jovian import Sinope
        return Sinope
    def _lazy_get_Lysithea_(self, ig):
        from jovian import Lysithea
        return Lysithea
    def _lazy_get_Carme_(self, ig):
        from jovian import Carme
        return Carme
    def _lazy_get_Ananke_(self, ig):
        from jovian import Ananke
        return Ananke
    def _lazy_get_Leda_(self, ig):
        from jovian import Leda
        return Leda
    def _lazy_get_Thebe_(self, ig):
        from jovian import Thebe
        return Thebe
    def _lazy_get_Adrastea_(self, ig):
        from jovian import Adrastea
        return Adrastea
    def _lazy_get_Metis_(self, ig):
        from jovian import Metis
        return Metis
    def _lazy_get_Callirrhoe_(self, ig):
        from jovian import Callirrhoe
        return Callirrhoe
    def _lazy_get_Themisto_(self, ig):
        from jovian import Themisto
        return Themisto
    def _lazy_get_Kalyke_(self, ig):
        from jovian import Kalyke
        return Kalyke
    def _lazy_get_Iocaste_(self, ig):
        from jovian import Iocaste
        return Iocaste
    def _lazy_get_Erinome_(self, ig):
        from jovian import Erinome
        return Erinome
    def _lazy_get_Harpalyke_(self, ig):
        from jovian import Harpalyke
        return Harpalyke
    def _lazy_get_Isonoe_(self, ig):
        from jovian import Isonoe
        return Isonoe
    def _lazy_get_Praxidike_(self, ig):
        from jovian import Praxidike
        return Praxidike
    def _lazy_get_Megaclite_(self, ig):
        from jovian import Megaclite
        return Megaclite
    def _lazy_get_Taygete_(self, ig):
        from jovian import Taygete
        return Taygete
    def _lazy_get_Chaldene_(self, ig):
        from jovian import Chaldene
        return Chaldene

    # Saturn's satellites:
    def _lazy_get_Mimas_(self, ig):
        from saturnalia import Mimas
        return Mimas
    def _lazy_get_Enceladus_(self, ig):
        from saturnalia import Enceladus
        return Enceladus
    def _lazy_get_Tethys_(self, ig):
        from saturnalia import Tethys
        return Tethys
    def _lazy_get_Dione_(self, ig):
        from saturnalia import Dione
        return Dione
    def _lazy_get_Rhea_(self, ig):
        from saturnalia import Rhea
        return Rhea
    def _lazy_get_Titan_(self, ig):
        from saturnalia import Titan
        return Titan
    def _lazy_get_Hyperion_(self, ig):
        from saturnalia import Hyperion
        return Hyperion
    def _lazy_get_Iapetus_(self, ig):
        from saturnalia import Iapetus
        return Iapetus
    def _lazy_get_Phoebe_(self, ig):
        from saturnalia import Phoebe
        return Phoebe
    def _lazy_get_Janus_(self, ig):
        from saturnalia import Janus
        return Janus
    def _lazy_get_Epimetheus_(self, ig):
        from saturnalia import Epimetheus
        return Epimetheus
    def _lazy_get_Helene_(self, ig):
        from saturnalia import Helene
        return Helene
    def _lazy_get_Telesto_(self, ig):
        from saturnalia import Telesto
        return Telesto
    def _lazy_get_Calypso_(self, ig):
        from saturnalia import Calypso
        return Calypso
    def _lazy_get_Atlas_(self, ig):
        from saturnalia import Atlas
        return Atlas
    def _lazy_get_Prometheus_(self, ig):
        from saturnalia import Prometheus
        return Prometheus
    def _lazy_get_Pandora_(self, ig):
        from saturnalia import Pandora
        return Pandora
    def _lazy_get_Pan_(self, ig):
        from saturnalia import Pan
        return Pan

    # Uranus' satellites
    def _lazy_get_Ariel_(self, ig):
        from uranic import Ariel
        return Ariel
    def _lazy_get_Umbriel_(self, ig):
        from uranic import Umbriel
        return Umbriel
    def _lazy_get_Titania_(self, ig):
        from uranic import Titania
        return Titania
    def _lazy_get_Oberon_(self, ig):
        from uranic import Oberon
        return Oberon
    def _lazy_get_Miranda_(self, ig):
        from uranic import Miranda
        return Miranda
    def _lazy_get_Cordelia_(self, ig):
        from uranic import Cordelia
        return Cordelia
    def _lazy_get_Ophelia_(self, ig):
        from uranic import Ophelia
        return Ophelia
    def _lazy_get_Bianca_(self, ig):
        from uranic import Bianca
        return Bianca
    def _lazy_get_Cressida_(self, ig):
        from uranic import Cressida
        return Cressida
    def _lazy_get_Desdemona_(self, ig):
        from uranic import Desdemona
        return Desdemona
    def _lazy_get_Juliet_(self, ig):
        from uranic import Juliet
        return Juliet
    def _lazy_get_Portia_(self, ig):
        from uranic import Portia
        return Portia
    def _lazy_get_Rosalind_(self, ig):
        from uranic import Rosalind
        return Rosalind
    def _lazy_get_Belinda_(self, ig):
        from uranic import Belinda
        return Belinda
    def _lazy_get_Puck_(self, ig):
        from uranic import Puck
        return Puck
    def _lazy_get_Caliban_(self, ig):
        from uranic import Caliban
        return Caliban
    def _lazy_get_Sycorax_(self, ig):
        from uranic import Sycorax
        return Sycorax
    def _lazy_get_Prospero_(self, ig):
        from uranic import Prospero
        return Prospero
    def _lazy_get_Setebos_(self, ig):
        from uranic import Setebos
        return Setebos
    def _lazy_get_Stephano_(self, ig):
        from uranic import Stephano
        return Stephano

    # Neptune's satellites
    def _lazy_get_Triton_(self, ig):
        from neptunous import Triton
        return Triton
    def _lazy_get_Nereid_(self, ig):
        from neptunous import Nereid
        return Nereid
    def _lazy_get_Naiad_(self, ig):
        from neptunous import Naiad
        return Naiad
    def _lazy_get_Thalassa_(self, ig):
        from neptunous import Thalassa
        return Thalassa
    def _lazy_get_Despina_(self, ig):
        from neptunous import Despina
        return Despina
    def _lazy_get_Galatea_(self, ig):
        from neptunous import Galatea
        return Galatea
    def _lazy_get_Proteus_(self, ig):
        from neptunous import Proteus
        return Proteus
    def _lazy_get_Larissa_(self, ig):
        from neptunous import Larissa
        return Larissa
    def _lazy_get_Galle_(self, ig):
        from neptunous import Galle
        return Galle
    def _lazy_get_leVerrier_(self, ig):
        from neptunous import leVerrier
        return leVerrier
    def _lazy_get_Lassel_(self, ig):
        from neptunous import Lassel
        return Lassel
    def _lazy_get_Arago_(self, ig):
        from neptunous import Arago
        return Arago
    def _lazy_get_Adams_(self, ig):
        from neptunous import Adams
        return Adams

    # Moons of Mars:
    def _lazy_get_Phobos_(self, ig):
        from asteroid import Phobos
        return Phobos
    def _lazy_get_Deimos_(self, ig):
        from asteroid import Deimos
        return Deimos

    # Asteroids:
    def _lazy_get_Asteroids_(self, ig):
        from asteroid import Asteroids
        return Asteroids
    def _lazy_get_Ceres_(self, ig):
        from asteroid import Ceres
        return Ceres
    def _lazy_get_Albert_(self, ig):
        from asteroid import Albert
        return Albert
    def _lazy_get_Eros_(self, ig):
        from asteroid import Eros
        return Eros
    def _lazy_get_Amor_(self, ig):
        from asteroid import Amor
        return Amor
    def _lazy_get_Apollo_(self, ig):
        from asteroid import Apollo
        return Apollo
    def _lazy_get_Icarus_(self, ig):
        from asteroid import Icarus
        return Icarus
    def _lazy_get_Adonis_(self, ig):
        from asteroid import Adonis
        return Adonis
    def _lazy_get_Hermes_(self, ig):
        from asteroid import Hermes
        return Hermes

    # Few Kuiper belt objects:
    def _lazy_get_Pluto_(self, ig):
        from Kuiper import Pluto
        return Pluto
    def _lazy_get_Charon_(self, ig):
        from Kuiper import Charon
        return Charon
    def _lazy_get_Quaoar_(self, ig):
        from Kuiper import Quaoar
        return Quaoar
    def _lazy_get_Sedna_(self, ig):
        from Kuiper import Sedna
        return Sedna
    def _lazy_get_Eris_(self, ig):
        from Kuiper import Eris
        return Eris
    def _lazy_get_Dysnomia_(self, ig):
        from Kuiper import Dysnomia
        return Dysnomia

    # The outer reaches ...
    def _lazy_get_Kuiper_(self, ig):
        from Kuiper import Kuiper
        return Kuiper
    def _lazy_get_Oort_(self, ig):
        from Kuiper import Oort
        return Oort
    def _lazy_get_Terminus_(self, ig):
        from Kuiper import Terminus
        return Terminus

    # Stars ...
    def _lazy_get_Gliese710_(self, ig):
        from star import Gliese710
        return Gliese710
    def _lazy_get_ProximaCentauri_(self, ig):
        from star import ProximaCentauri
        return ProximaCentauri
    def _lazy_get_AlphaCentauri_(self, ig):
        from star import AlphaCentauri
        return AlphaCentauri

class PlanetList (Lazy):
    def __len__(self): return 9
    def __getitem__(self, ind):
        if ind < 0 or ind > 7: raise IndexError, ind
        if ind < 4: return self.inner[ind]
        return self.outer[ind - 4]

    def __repr__(self):
        return repr(self.inner + self.outer)

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
        from inner import Mercury, Venus, Earth, Mars
        return ( Mercury, Venus, Earth, Mars )

    def _lazy_get_outer_(self, ignored):
        from outer import Jupiter, Saturn, Uranus, Neptune
        return ( Jupiter, Saturn, Uranus, Neptune )

D = NameSpace()
Planets = PlanetList()

del NameSpace, PlanetList, Lazy

notes = """

Telesto precedes and Calypso follows Tethys as the trio circles Saturn.

Cosmic debates:
 Scale of Universe:
http://antwrp.gsfc.nasa.gov/debate/debate96.html
 Nature of Universe:
http://antwrp.gsfc.nasa.gov/debate/debate98.html

Gagarin's flight:
http://antwrp.gsfc.nasa.gov/apod/ap010414.html

Arthur C Clarke's home page
http://library.thinkquest.org/27864/data/clarke/acchome.html

Nice description (and picture) of the layers:
http://liftoff.msfc.nasa.gov/academy/space/atmosphere.html

Solar wind: 250 to 400 miles / second; light / (450 to 750)
http://antwrp.gsfc.nasa.gov/apod/ap970217.html

Solar wind:
http://www-spof.gsfc.nasa.gov/Education/wsolwind.html

Kepler:
http://antwrp.gsfc.nasa.gov/apod/ap010114.html
Brahe:
http://antwrp.gsfc.nasa.gov/apod/ap010107.html
Romer's determination of ligth's speed:
http://antwrp.gsfc.nasa.gov/apod/ap010102.html

Lunar Perigee: 221,797 miles
http://antwrp.gsfc.nasa.gov/apod/ap960731.html

http://www.astro.ubc.ca/people/scott/faq_email.html

Solar system moves at a velocity of 370.6 +/- 0.4 km/s towards galactic coordinates (l,b)=(264.31+/-0.17,48.05+/-0.10), which corresponds to RA=11h12m, Dec=-7.2.

Local group moves at velocity 627 +/- 22 km/s towards (l,b)=(276+/-3,30+/-3)
(error mostly ignorance of our motion relative to local group)

Cosmic background temperature: 2.728 +/- .002 K.

http://www.astro.ubc.ca/people/scott/faq_email.html
Cosmic microwave dates from red-shift 1100 +/- 100
The best temperature for the CMB is currently 2.728+/-0.004K, where this uncertainty represents a statistical 95% confidence region.
 k is not known to infinite precision, but is 1.380658(12)e23 J/K

there are about 412 CMB photons per cubic cm (with an uncertainty of about 1); the energy density is the equivalent of 0.261 electron Volts per cubic cm (again uncertain by about +/-1 in the last decimal place); the equivalent mass density is 4.66e-31 kilogrammes per cubic metre; the peak of the spectrum is at a frequency of 160.4 GHz (uncertain by about +/-0.1); and the peak intensity of the background is about 385 MJy/Sr (that's MegaJanskys per Steradian, which is not a unit you meet everyday!).

 there are about 400 CMB photons in every cubic centimeter of the Universe, ... representing a flux of 3.14e-6 W/m/m

http://antwrp.gsfc.nasa.gov/apod/ap990111.html
alleges Earth's preihelion and apehelion are  147 and 150 Gm
implying smaller eccentricity and radius than the data I've been using.

Big Ear's Wow ! signal:
http://www.bigear.org/wowmenu.htm
http://antwrp.gsfc.nasa.gov/apod/ap980917.html

Asteroid 3753 orbiting The Sun synchronized with Earth, but in a
kidney-bean-shaped relative motion.
http://antwrp.gsfc.nasa.gov/apod/ap970618.html

75 rem in a month => radiation sickness
500 rem in a month => death
for `month' read: time period short compared to
cell repair and replacement cycles of the human body

and compare some of the bigger orbital radii with 1 AU ...

http://www.johnstonsarchive.net/astro/asteroidmoons.html

look at field of a constant-density spherically-symmetric body
at what size or density (or what criterion) does it collapse to a black hole ?

Handy astronomical dictionary:
http://imagine.gsfc.nasa.gov/docs/dictionary.html#perihelion
with lots of interesting links ;^)

Data on Eros (includes implied mass):
http://antwrp.gsfc.nasa.gov/apod/ap000803.html

Add this to your sources.list
deb http://ftp.no.debian.org/debian experimental main contrib

See also: http://www.codeville.org/
pythonic version control system.

chording single-hand key-thing (replaces board ;^)
http://www.handykey.com/site/twiddler2.html

Ancient disk drive:
http://www.siliconvalley.com/mld/siliconvalley/11743962.htm

--

 From Robert Zubrin's 1999 'Entering Space'
ISBN 0-87477-975-8

Planet		Orbital Radius/AU	`Velocity to orbit'.s/km
Jupiter		5.2			29.5
Saturn		9.5			14.8
Uranus		19.2			12.6
Neptune		30.1			14.2

Saturn's moons:
Moon		Orbital Radius/Mm	Radius/km
Mimas		185.6			195
Enceladus	238.1			255
Tethys		294.7			525
Dione		377.5			560
Rhea		527.2			765
Titan		1221.6			2575
Hyperion	1483.0			143
Iapetus		3560.1			720
Phoebe		12950			100

Jupiter's moons:
Moon		Orbital Radius/Mm	Radius/km	Radiation.day/rem
Metis		127.96			20		18000
Adrastea	128.98			10		18000
Amalthea	181.3			105		18000
Thebe		221.9			50		18000
Io		421.6			1815		3600
Europa		670.9			1569		540
Ganymede	1070			2631		8
Callisto	1883			2400		.01
Leda		11094			8
Hmalia		11480			90
Lysithea	11720			20
Elara		11737			40
Ananke		21200			15
Carme		22600			22
Pasiphae	23500			35
Sinope		23700			20

"""
