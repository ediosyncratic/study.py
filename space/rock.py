# -*- coding: iso-8859-1 -*-
"""Functions to aid in constructing moons, asteroids and other clutter.

$Id: rock.py,v 1.1 2005-03-12 15:33:57 eddy Exp $
"""

from basEddy.units import *
from space.body import Planetoid, Planet
from space.common import Spheroid, Orbit, Spin

# Packaging data taken from NASA atlas, mostly pp 325--327
def NASAshell(major, minor=None, minim=None):
    major =  major * km
    if minor is not None: minor = minor * km
    if minim is not None: minim = minim * km
    return Spheroid(major, minor, minim) # should really be a Surface ...

def NASAorbit(planet, sma, per, tilt = 90 * (.5 + tophat), **what):
    # no eccentricities supplied ... but they are all bound orbits ...
    # no tilt supplied, aside from retrograde or not ...
    if per < 0: tilt, per = tilt + 90, -per
    # no error bars supplied, but all periods gave two decimal places
    # sma is really radius / (1 - eccentricity), but endure it and let per's error-bar infect it
    return apply(Orbit, (planet, sma * mega * metre, Spin(day * (per + .01 * tophat), tilt)), what)

def NASAdata(what, found, mass, rho, skin, shell):
    what['discovery'] = found
    if mass is not None: what['mass'] = mass * 1e20 * kilogramme
    if rho  is not None: what['density'] = rho * gram / cc
    if skin is not None: shell.also(material=skin)

def NASAtrojan(name, found, boss, phase, shell, skin=None, mass=None, rho=None, **what):
    NASAdata(what, found, mass, rho, skin, shell)
    what.update({ 'boss': boss })

    # there may be a better way to encode this ...
    ans = apply(Planet, (name, shell, boss.orbit), what)
    boss.Lagrange[phase].append(ans)
    return ans

def NASAmoon(name, planet, found, sma, per, shell, skin=None, mass=None, rho=None, **what):
    NASAdata(what, found, mass, rho, skin, shell)
    return apply(Planet, (name, shell, NASAorbit(planet, sma, per)), what)

def NamedOrbit(name, planet, sma, per, **what):
    # No data bon the bodies themselves, but good data on name and discovery
    what['orbit'] = NASAorbit(planet, sma, per, name=name)
    return appply(Planetoid, (name,), what)

def SAOmoon(planet, found, nom, sma, per, name=None):
    # Names are catalogue numbers and we have no data on the bodies themselves ...
    assert planet.name[0] == nom[0] # the rest of nom is a number, with maybe a trailing #
    if name is None: name = 'S/%d %s' % (found.year, nom)
    return Planetoid(name, orbit=NASAorbit(planet, sma, per, name=name), discovery=found)

_rcs_log = """
$Log: rock.py,v $
Revision 1.1  2005-03-12 15:33:57  eddy
Initial revision

"""
