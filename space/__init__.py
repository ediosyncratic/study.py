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

$Id: __init__.py,v 1.3 2007-03-24 12:20:58 eddy Exp $
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
    def _lazy_get_Universe_(self, ig):
        from space.home import Universe
        return Universe
    def _lazy_get_MilkyWay_(self, ig):
        from space.home import MilkyWay
        return MilkyWay
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
        from space.inner import Mercury, Venus, Earth, Mars
        return ( Mercury, Venus, Earth, Mars )

    def _lazy_get_outer_(self, ignored):
        from space.outer import Jupiter, Saturn, Uranus, Neptune
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

Big fat database of stars:
http://antwrp.gsfc.nasa.gov/apod/ap990426.html

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

-- 

Near neighbours

Star			R(Sol)/light-year	(X,Y,Z)/light-year	Type	Luminosity/Sun.lum	Mass/Sun.mass	Radius/Sun.R
Sol			0			(0,0,0)			G2	1.0			1.0		1.0
Proxima Centauri	4.3			(-1.6,-1.2,-3.8)	M5e	.00006			0.1		-
&alpha; Centauri A	4.4			(-1.7,-1.4,-3.8)	G2	1.3			1.1		1.23
&alpha; Centauri B	4.4			(-1.7,-1.4,-3.8)	G2	.36			.89		.87
Barnard's star		5.9			(-.1,-5.9,-.5)		M5	.00044			.15		.12
Wolf 359		7.6			(-7.2,2.1,1.0)		M8e	.00002			.2		.04
Lalande 21185		8.1			(-6.3,1.7,4.8)		M2	.0052			.35		.35
Sirius A		8.7			(-1.6,8.2,-2.5)		A1	23.0			2.31		1.8
Sirius B		8.7			(-1.6,8.2,-2.5)		DA	.0028			.98		.022
UV Ceti A		8.9			(7.7,3.4,-2.8)		M6e	.00006			.12		.05
UV Ceti B		8.9			(7.7,3.4,-2.8)		M6e	.00004			.10		.04
Ross 154		9.5			(1.8,-8.5,-3.8)		M5e	.0004			.31		.12
Ross 248		10.3			(7.4,-.7,7.1)		M6e	.00011			.25		.07
&epsilon; Eridani	10.7			(6.4,8.4,-1.8)		K2	.3			.8		.9
Luyten 789-6		10.8			(9.7,-3.7,-2.9)		M6	.00012			.25		.08
Ross 128		10.8			(-10.8,.7,.2)		M5	.00033			.31		.1
61 Cygni A		11.2			(6.3,-6.1,7.0)		K5	.063			.59		.7
61 Cygni B		11.2			(6.3,-6.1,7.0)		K7	.040			.50		.8
&epsilon; Indi		11.2			(5.3,-3.0,-9.4)		K5	.13			.71		1.0
Procyon A		11.4			(-4.7,10.3,1.1)		F5	7.6			1.77		1.7
Procyon B		11.4			(-4.7,10.3,1.1)		DF	.0005			.63		.01
+59&deg; 1915 A		11.5			(1.1,-5.7,9.9)		M4	.0028			.4		.28
+59&deg; 1915 B		11.5			(1.1,-5.7,9.9)		M5	.0013			.4		.20
Groombridge 34A		11.5			(8.4,.5,8.0)		M2	.0058			.38		.38
Groombridge 34B		11.5			(8.4,.5,8.0)		M4	.0004			-		.11
Lacaille 9352		11.7			(9.2,-2.3,-6.9)		M2	.012			.47		.57
&tau; Ceti		11.9			(10.3,4.9,-3.3)		G8	.44			.82		1.67
Luyten BD		12.2			(-4.4,11.3,1.1)		M4	.0014			.38		.16
LET 118			12.5			(11.4,3.4,-3.8)		M5e	-			-		-
Lacaille 8760		12.5			(7.3,-6.4,-7.9)		M1	.025			.54		.82
Kapteyn's Star		12.7			(1.9,8.8,-9.0)		M0	.004			.44		.24
Kruger 60 A		12.8			(6.3,-2.7,10.8)		M4	.0017			.27		.51
Kruger 60 B		12.8			(6.3,-2.7,10.8)		M6	.00044			.16		-
Ross 614 A		13.1			(-1.5,13.0,-.6)		M5e	.0004			.14		.14
Ross 614 B		13.1			(-1.5,13.0,-.6)		-	.00002			.08		-
BD -12&deg; 4523	13.1			(-5.0,-11.8,-2.8)	M5	.0013			.38		.22
van Maanen's Star	13.9			(13.6,2.8,1.2)		DG	.00017			-		-
Wolf 424 A		14.2			(-13.9,-1.9,2.3)	M6e	.00014			-		.09
Wolf 424 B		14.2			(-13.9,-1.9,2.3)	M6e	.00014			-		.09
G158-27			14.4			(14.3,.2,-2.0)		M7	.00005			-		-
CD -37&deg; 15492	14.5			(11.5,.1,-8.8)		M3	.00058			.39		.4
Groombridge 1616	15.0			(-8.6,4.6,11.4)		K7	.04			.56		.5
CD -46&deg; 11540	15.1			(-1.6,-10.2,-11.0)	M4	.003			.44		.25
CD -49&deg; 13515	15.2			(7.9,-6.0,-11.5)	M3	.00058			.37		.34
CD -44&deg; 11909	15.3			(-1.3,-10.9,-10.7)	M5	.00063			.34		.15
Luyten 1159-16		15.4			(13.1,7.3,3.4)		M8	.00023			-		-
Lalande 25372		15.7			(-13.6,-6.6,4.1)	M2	.0076			-		.40
BD +68&deg; 946		15.8			(-.6,-5.8,14.7)		M3	.0044			.35		.39
Luyten 145-141		15.8			(-6.8,.5,-14.3)		DA	.0008			-		-
Ross 780		15.8			(14.6,-4.5,-4.0)	M5	.0016			.39		.23
&Omicron; Eridani A	15.9			(7.1,14.1,-2.1)		K0	.33			.81		.7
&Omicron; Eridani B	15.9			(7.1,14.1,-2.1)		DA	.0027			.43		.018
&Omicron; Eridani C	15.9			(7.1,14.1,-2.1)		M4e	.00063			.21		.43
BD +20&deg; 2465	16.1			(-13.6,6.6,5.5)		M4	.0036			.44		.28
Altair			16.6			(7.4,-14.6,2.5)		A7	10.0			1.9		1.2
70 Ophiuchi A		16.7			(.2,-16.7,.7)		K1	.44			.89		1.3
70 Ophiuchi B		16.7			(.2,-16.7,.7)		K6	.083			.68		.84
AC +79&deg; 3888	16.8			(-3.2,.2,16.5)		M4	.0009			.35		.15
BD +43&deg; 4305	16.9			(11.5,-3.9,11.8)	M5e	.0021			.26		.24
Stein 2051 A		17.0			(3.5,8.1,14.6)		M5	.0008			-		-
Stein 2051 B		17.0			(3.5,8.1,14.6)		DC	.0003			-		-
WX Ursa Majoris A	17.5			(-12.2,3.1,12.1)	M2	-			-		-
WX Ursa Majoris B	17.5			(-12.2,3.1,12.1)	M8	-			-		-
36 Ophiuchi A		17.7			(-3.3,-15.5,-7.9)	K2	.26			.77		.90
36 Ophiuchi B		17.7			(-3.3,-15.5,-7.9)	K1	.26			.76		.82
36 Ophiuchi C		17.7			(-3.3,-15.5,-7.9)	K6	.09			.63		.90
HR 7703 A		18.4			(7.9,-12.6,-10.9)	K3	.20			.76		.80
HR 7703 B		18.4			(7.9,-12.6,-10.9)	M5	.0008			.35		.14
&sigma; Draconis	18.5			(2.5,-5.9,17.3)		K0	.4			.82		.28
YZ Canis Minoris	18.5			(-7.8,16.7,1.2)		M4	-			-		-
&delta; Pavonis		18.6			(3.8,-6.4,-17.0)	G6	1.0			.98		1.07
1&deg; 4774		18.6			(18.6,-1.1,.7)		M2	.0001			-		-
Luyten 347-14		18.6			(4.3,12.3,13.3)		M7	.0001			.26		.08
-21&deg; 1377		18.7			(-.6,17.3,-7.0)		M1	.016			.46		.59
Luyten 97-12		18.9			(-3.4,6.3,-17.5)	D	.0003			-		-
Luyten 674-15		19.1			(-.96,15.0,-7.0)	M	-			-		-
&eta; Cassiopeia A	19.2			(10.1,2.1,16.2)		G0	1.0			.85		.84
&eta; Cassiopeia B	19.2			(10.1,2.1,16.2)		M0	.03			.52		.07
Luyten 205-128		19.2			(-.8,-10.3,-16.2)	M	.0002			.14		-
HD 36395		19.2			(2.6,19.0,-1.2)		M1	.02			.51		.69
40&deg; 9712		19.3			(-8.8,-11.5,-12.7)	M4	.003			.44		.29
Ross 986		19.3			(-4.3,-14.4,12)		M5	-			-		-
Ross 47			19.4			(1.7,18.9,4.2)		M6	.0008			.35		.17
Wolf 294		19.4			(-3.6,15.8,10.7)	M4	.008			.49		.46
LP 658-2		19.6			(.6,19.5,-1.4)		DK
+53&deg; 1320 A		19.6			(-8.8,7.9,15.6)		M0
+53&deg; 1320 B		19.6			(-8.8,7.9,15.6)		M0
VB10 A			19.6			(6.2,-18.5,1.7)		M4	-			.39		.43
VB10 B			19.6			(6.2,-18.5,1.7)		M5	.007			-		.008
-45&deg; 13677		19.9			(7.5,-11.8,-14.1)	M0	.00002
82 Eridani		20.3			(9.6,11.2,-13.9)	G5
Wolf 630 A		20.3			(-5.8,-19.2,-2.9)	M4	-			.38		-
Wolf 630 B		20.3			(-5.8,-19.2,-2.9)	M5	-			.38		-
Wolf 630 C		20.3			(-5.8,-19.2,-2.9)	-	-			-		-
Wolf 630 D		20.3			(-5.8,-19.2,-2.9)	M4	-			-		-
-11&deg; 3759		20.4			(-15.7,-12.3,-4.4)	M4	-			-		-
&beta; Hydri		20.5			(4.4,.4,-20.0)		G1	-			-		1.66
+45 Fu46 A		21.0			(-3.1,-14.4,15.0)	M3	-			.31		-
+45 Fu46 A		21.0			(-3.1,-14.4,15.0)	-	-			.25		-
+19&deg; 5116 A		21.0			(19.5,-3.4,7.1)		M4
+19&deg; 5116 B		21.0			(19.5,-3.4,7.1)		M6

Types: O,B,A,F,G,K,M; O0 brightest, M9 dimmest; but white dwarfs have type D*

"""

_rcs_log = """
$Log: __init__.py,v $
Revision 1.3  2007-03-24 12:20:58  eddy
More data (in strings, for later processing), tidying, demoted Pluto.

Revision 1.2  2005/03/13 15:11:28  eddy
Moved PlanetList and Planets from home.py and large comment from common.py;
added NameSpace and its instance, D; documented it and Planets.

Initial Revision 1.1  2005/03/12 12:58:26  eddy
"""
