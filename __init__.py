"""Tools (and data) for studying various topics.

Sub-packages:
 cache -- remembering things to save working them out again
 chemy -- fundamental physics through to chemistry
 crypt -- tools for encoding and decoding
 maths -- assorted mathematical tools
 parse -- parsers, serializers and objects to manage the data in between
 snake -- general pythonic helper infrastructure
 space -- the solar system and some of the rest of the universe
 value -- imprecise numbers and units of measurement, combined

While the Quantity-based code (value, chemy and space) does roughly what I want,
and thus proves that the general approach is valid, there are assorted
architectural flaws.  Most prominent among these is that the object to represent
an entity is created from the data I have from some particular source, or a
melding of sources, where I now consider it would be better to create the naked
object and feed it data subsequently - via, for each data source, a method which
knows how to handle that source's data; or via some general-purpose method which
most source-specific methods call after massaging the source's data (e.g. adding
units, scalings and error bars; or combining things the source gives to obtain
the attributes we actually want).  In particular, the data should not be part of
this package: it should be contained in data files digested by the code in this
package.  However, this requires some fairly far-reaching re-design, so I'll
make do with what I have in the mean time ;-)
"""
# TODO: PEP 8 conformance
# rename class-parameters to cls (from mode, klaz, etc.)
# prefer .startswith(), .endswith() over comparison w/ relevant chunks of strings
# purge any type(blah) == what checks in favour of insinstance
# purge CVS cruft (and switch to git)
# use __all__ ?

# TODO: reStructuredText conversion
# Modify doc-strings to use reStructuredText; in each module, once
# converted, specify global __docformat__ = 'restructuredtext'

# Basics:
import snake
import cache

# Extensions:
import crypt
import maths
import parse

# The Quantity-based universe:
import value
import chemy
import space
