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

LICENSE = """\
Eddy's python package to support the study of science and mathematics.
Copyright (C) 1998--2012, Edward Welbourne (eddy [at] chaos [dot] org [dot] uk)

This package is free software: you can redistribute it and/or modify it under
the terms of version 3 of the GNU General Public License as published by the
Free Software Foundation.

This package is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this package (see the file GPL-3 in the top-level study/ directory).  If not,
see <http://www.gnu.org/licenses/>.
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
   - don't use __myname__ (e.g. __quantity__ etc.; currently violated extensively)
   - don't use type(x) == blah comparisons; prefer isinstance(x, blah)
   - check for any isinstance(x, string); check against basestring instead

 * Cure other archaisms:
   - s/raise $T, $v/raise $T($v)/
   - s/except $T, $v/except $T as $v/
   - See http://www.python.org/dev/peps/pep-0290/

 * Break up archaea into a sub-package - it's huge.  Replace the ad hoc
   classification with a registration mechanism for the diverse namespaces (by
   nationality, by name of unit) causing mere creation of the unit object to
   get it registered in relevant namespaces.  See:
   http://docutils.sourceforge.net/rst.html

 * Break out randomness from maths as random.

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

 * Covnert to python 3 - worth doing on a separate branch !
   - there is a "2to3" utility to help with this
   - convert .next() to .__next__() on iterators
   - s/for $var in $iterable: yield $var/yield from $iterable/g
   - can have (tunnels or) keyword-only arguments after *args; PEP 3102
   - can modify variables in outer scopes using nonlocal; PEP 3104
   - when catching one exception to raise a different, can inherit the
     original's context using: raise Type from value (PEP 344)
   - print stuff -> print(stuff); PEP 3105
   - changes in .keys(), .values(), .items(); PEP 3106
   - can no longer use tuple-parameter unpack, e.g. lambda (k, v): k
   - study PEP 3141, a type hierarchy for numbers
"""
