# -*- coding: iso-8859-1 -*-
"""Functions to aid in constructing moons, asteroids and other clutter.

See study.LICENSE for copyright and license information.
"""

from study.value.units import Quantity, mega, day, km, metre, kg, litre
from body import Planetoid, Planet
from common import Spheroid, Orbit, Spin

# Packaging data taken from NASA atlas, mostly pp 325--327
def NASAshell(major, minor=None, minim=None, Sp=Spheroid, u=km):
    major =  major * u
    if minor is not None: minor = minor * u
    if minim is not None: minim = minim * u
    return Sp(major, minor, minim) # should really be a Surface ...

def NASAorbit(planet, sma, per, tilt = Quantity.below(90),
              Float=Quantity.fromDecimal,
              u=day, Mm=mega*metre, O=Orbit, S=Spin, **what):
    # no eccentricities supplied ... but they are all bound orbits ...
    # no tilt supplied, aside from retrograde or not ...
    if per < 0: tilt, per = tilt + 90, -per
    # no error bars supplied, but all periods gave two decimal places
    # sma is really radius / (1 - eccentricity), but endure it and let per's error-bar infect it
    return O(planet, sma * Mm, S(Float(per, 2, None, u), tilt), **what)

def NASAdata(what, found, mass, rho, skin, shell, m=kg, d=kg/litre):
    what['discovery'] = found
    if mass is not None: what['mass'] = mass * 1e20 * m
    if rho  is not None: what['density'] = rho * d
    if skin is not None: shell.also(material=skin)

def NASAtrojan(name, found, boss, phase, shell, skin=None, mass=None, rho=None, P=Planet, **what):
    NASAdata(what, found, mass, rho, skin, shell)
    what.update(boss=boss)

    # there may be a better way to encode this ...
    ans = P(name, shell, boss.orbit, **what)
    boss.Lagrange[phase].append(ans)
    return ans

def NASAmoon(name, planet, found, sma, per, shell, skin=None, mass=None, rho=None, P=Planet, **what):
    NASAdata(what, found, mass, rho, skin, shell)
    return P(name, shell, NASAorbit(planet, sma, per), **what)

def NamedOrbit(name, planet, sma, per, P=Planetoid, **what):
    # No data bon the bodies themselves, but good data on name and discovery
    what['orbit'] = NASAorbit(planet, sma, per, name=name)
    return P(name, **what)

def SAOmoon(planet, found, nom, sma, per, name=None, P=Planetoid):
    # Names are catalogue numbers and we have no data on the bodies themselves ...
    assert planet.name[0] == nom[0] # the rest of nom is a number, with maybe a trailing #
    if name is None: name = 'S/%d %s' % (found.year, nom)
    return P(name, orbit=NASAorbit(planet, sma, per, name=name), discovery=found)

del Planetoid, Planet, Spheroid, Orbit, Spin, Quantity, mega, day, km, metre, kg, litre
