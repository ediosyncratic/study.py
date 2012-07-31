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

While the Quantity-based code (value, chemy and space) does roughly what I
want, and thus proves that the general approach is valid, there are assorted
architectural flaws; see value.quantity's doc-string's TODO.  However, fixing
that shall require some fairly far-reaching re-design, so I'll make do with
what I have in the mean time ;-)
"""

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

TODO = """There are many things that could be improved.

 * Have I finished the conversion to use of interpolators ?
   No.

 * Replace the Lazy-based machinery with study.cache.property types.

 * PEP 8 conformance.
   - Prefer .startswith(), .endswith() over comparison w/ relevant chunks of strings
   - use __all__ ?
   - kill tabs !

 * Break up archaea into a sub-package - it's huge.  Replace the ad hoc
   classification with a registration mechanism for the diverse namespaces (by
   nationality, by name of unit) causing mere creation of the unit object to
   get it registered in relevant namespaces.

 * Convert documentation to use reStructuredText. Modify doc-strings to use
   reStructuredText; in each module, once converted, specify global
   __docformat__ = 'restructuredtext'

 * Add a DateTime type that knows about (and its instances can represent
   themselves in any of) several different calendar systems; equip it with at
   least some knowledge of which calendars were in use in which jurisdictions
   at what dates.  (Start with solar calendars and leave lunar ones for later
   !)  Take account of differences in when different cultures deemed the
   transitions between days to happen, as far as possible.  Use the Julian day
   and UTC as underlying time system.  Integrate with Quantity's time values
   (e.g. for differences and times within each day).  c.f. the datetime module.

 * New quantity-type; see value.quantity's doc-string.
"""
