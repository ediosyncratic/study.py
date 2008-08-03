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

$Id: whole.py,v 1.1 2008-08-03 21:12:49 eddy Exp $
"""

