"""Tools and data for studying various scientific and mathematical topics.

Sub-packages:
 chemy -- fundamental physics through to chemistry
 code  -- tools for encoding and decoding
 maths -- assorted mathematical tools
 space -- the solar system and some of the rest of the universe
 value -- imprecise numbers and units of measurement, combined

While this code does roughly what I want, and thus proves that the general
approach is valid, there are assorted architectural flaws.  Most prominent among
these is that the object to represent an entity is created from the data I have
from some particular source, or a melding of sources, where I now consider it
would be better to create the naked object and feed it data subsequently - via,
for each data source, a method which knows how to handle that source's data; or
via some general-purpose method which most source-specific methods call after
massaging the source's data (e.g. adding units, scalings and error bars; or
combining things the source gives to obtain the attributes we actually want).
However, this requires some fairly far-reaching re-design, so I'll make do with
what I have in the mean time ;-)

$Id: __init__.py,v 1.3 2007-03-25 07:56:07 eddy Exp $
"""
import chemy
import code
import maths
import space
import value
