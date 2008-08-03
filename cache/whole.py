"""Cacheing data about integers.

This module provides classes to manage caches of data about the integers.  It
presumes that you have some way of computing some information about integers for
which it is worth saving the answers to disk (for example, because the cost of
re-computation is high, possibly due to needing answers from smaller integers to
arrive at answers for bigger ones).  The cache managed by this system just
remembers the information you computed, in python modules importable with
execfile, organized in a directory hierarchy, with an __init__.py in each
directory to manage the hierarchy itself.  There may be gaps in such caches, a
system may use several of them with distinct roots and a system may remember
several (up to about 26) distinct types of information in a single cache.

The primary use case motivating the initial implementation of this
infrastructure is tracking information about primeness and least common factors
for the naturals (non-negative integers); the intent here is to be generic, but
it is possible some design features should be refactored out to maths.prime in
order to make these classes function truly generically.

Nodes in the hierarchy have names of form
        \([NP]\)\?\([0-9a-z]+\)\([A-Z]+\)\([0-9a-z]+\)\(\.py\)\?
wherein:
 * the optional initial [NP] (for negative, positive) is only present if the
   parent directory's span includes both negative and positive integers; this
   only ever arises when the parent directory is the root of a cache
 * the \([A-Z]\) could in fact be \([^0-9a-z]+\) and provides
   application-specific information about the types of data, about integers,
   that are present in this node and any descendants
 * the final \(\.py\)\? is only present for leaf nodes, i.e. files
 * the two \([0-9a-z]+\) are interpreted via int(,36) as naturals; the first is
   the start-point (with sign indicated by [NP] if present, else by ancestor in
   the hierarchy if any has such a prefix, defaulting to positive if no ancestor
   does) and the second is the length (measured away from zero)
 * the start-point in the preceding is always indicated relative to that of the
   node's parent directory, measured away from zero (with sign implied by
   nearest ancestor having [NP] prefix, if any, else tacitly positive).

Thus N0X42.py would be a file providing application-specific information of
category 'X'; it could only appear in the root directory of its hierarchy, and
would relate to the integers from 0 down to minus one hundred and fourty-six
(i.e. 4*36 + 2).  In all likelihood, there would be something in the same cache
root directory whose name would start P1X, providing the same kind of
information for positive integers up to some limit.

$Id: whole.py,v 1.2 2008-08-03 21:34:46 eddy Exp $
"""

# TODO: refactor much of maths.prime.cache into here.
