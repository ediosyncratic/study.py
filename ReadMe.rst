Eddy's pythonic study package
=============================

This package has evolved since 1998 from my tinkerings in python.  There are
huge improvements that could be made (search for ``TODO`` in the source files;
you may even find some ``FIXME`` comments, too) but it's hobby work, so don't
expect it to be made "finished" any time soon.  Patches would be welcome if
anyone finds this fun enough to play with to make it worth patching.

Most documentation is in doc-strings, where it belongs; you should get useful
information by printing the ``.__doc__`` of whatever object you're playing with,
be it Planck's constant, the Polynomial class or the top-level study package
object itself - a good place to start, if you want to learn more about this
package.  There's also a partial description of the package `on my web-site`__.

__ StudyPy_
.. _StudyPy: http://www.chaos.org.uk/~eddy/dev/study.py.html

Coding style
------------

`PEP 8`_ contains ample sound advice.  I even agree with most of it; and the
rest is eminently sensible, too.  Some of my deviations from it are deliberate -
outlined in Style.rst_ - but this package contains plenty that are mere
accidents of history.  Some simply predate the advice, or the language feature
that makes the advice sound (or my hearing of either).  Most of these are slated
for fixing - see the "PEP 8 conformance" item in ``study.TODO`` - but shall take
a lot of work to fix, simply by virtue of there being quite a lot of code and,
especially, doc-strings to revise.

.. _PEP 8: http://www.python.org/dev/peps/pep-0008/
.. _Style.rst: Style.rst

So, if you're thinking of submitting a patch to cure such ugliness, don't be
shy - but it's probably prudent to ask me about it *before* you expend large
amounts of effort, only to discover I preferred it the old way.  In any case,
please make such clean-up changes in commits separate from any more meaningful
code changes.

History
-------

The first toy (April 1998) was an early version of ``study.maths.ratio``'s
``Rational`` and ``approximate()``.  The next (Jan 1999) was a first stab at
what has since become the ``Quantity`` class, initially just supporting a
numeric value and its units; it soon grew the ability to track error bars on its
numeric values.  Right at the outset, ``Quantity`` included the SI units, some
derived units, and a few silly archaic units (that now live in
``study.value.archaea``).  The next innovation was the first incarnation of
``Lazy`` (now in ``study.snake.lazy`` and deprecated in favour of
``study.cache.property``).  This sets the pattern for the three main areas of
development: mathematical toys, scientific quantities and pythonic support
infrastructure.

All of this happened in RCS; later, I migrated it to CVS.  The way I'd been
using RCS made this easy, as it happened.  I had all my RCS files in a directory
tree mirroring the source tree, whose top-level directory had a symlink named
RCS pointing at the top of the RCS tree; each sub-directory then had
an ``RCS -> ../RCS/name`` symlink using its own ``name``.  The resulting RCS
tree was thus a ready-made CVS module, that I merely had to move into my
``CVSROOT``.  There's consequently no obvious sign of when I did that, but I can
guess it was at the same time (2001/Autumn) as I converted my web-site to CVS.

In the RCS/CVS era of history, I could re-arrange the directory tree at whim,
without making any changes visible in the commit history, aside from the
consequent changes to how modules import one another.  I think the last major
re-structuring was 614f6ac in 2007/March, although a few relatively minor moves
may still be hidden from the version-control history between then and the summer
of 2010, when I imported the whole lot from CVS into git.  So checking out
historical revisions before summer 2010 may, and before spring 2007 certainly
shall, give you a source tree that probably doesn't even import cleanly.
