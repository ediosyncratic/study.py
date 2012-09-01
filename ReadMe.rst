Eddy's pythonic study package
=============================

This package has evolved since 1998 from my tinkerings in python.  There
are huge improvements that could be made (search for TODO in the source
files; you may even find some FIXME comments, too) but it's hobby work,
so don't expect it to be made "finished" any time soon.  Patches would
be welcome if anyone finds this fun enough to play with to make it worth
patching.

Most documentation is in doc-strings, where it belongs; you should get
useful information by printing the __doc__ of whatever object you're
playing with, be it Planck's constant, the Polynomial class or the
top-level study package object itself - a good place to start, if you
want to learn more about this package.

Coding style
------------

`PEP 8`__ contains ample sound advice.  I even agree with most of it;
and the rest is eminently sensible, too.  Some of my deviations from it
are deliberate - outlined below - but this package contains plenty that
are mere accidents of history.  Some simply predate the advice, or the
language feature that makes the advice sound (or my hearing of
either).  Most of these are slated for fixing, see the "PEP 8
conformance" item in study.TODO, but shall take a lot of work to fix,
simply by virtue of there being quite a lot of code and, especially,
doc-strings to revise.

__ PEP8_
.. _PEP8: http://www.python.org/dev/peps/pep-0008/

So, if you're thinking of submitting a patch to cure such ugliness,
don't be shy - but it's probably prudent to ask me about it *before* you
expend large amounts of effort, only to discover I preferred it the old
way.  In any case, please make such clean-up changes in commits separate
from any more meaningful code changes.

I don't entirely agree with PEP 8 - and anyone wanting me to accept any
patches to my branches of this package would do well to respect my
ediosyncracies as to coding style, if only to keep the patches
small.  If I have any patches for python's standard library, I'll abide
by PEP 8; if any of this code is ever invited into the standard library,
I'll convert it; but, aside from that, this package follows my slightly
different coding conventions.

I'm a bit stingier with vertical space than PEP 8 would be: I use fewer
blank lines and, where I can get away with putting a single-statement
block on the same line as the control structure to which it's
subordinate, I generally do.  (However, I entirely agree with PEP 8
about not putting several statements on one line separated by
semicolons.)  I think I'm currently using a right margin of 78
characters; I might increase it to 80, to match PEP 8's advice.

For horizontal space, I *mostly* agree with PEP 8, but do leave spaces
at start and end of { key: value } dictionary displays and, when doing
arithmetic inside an array index, I tend to discard horizontal spaces,
e.g. seq[1+i].  I sometimes - e.g. when a block of methods have similar
one-line implementations - use spacing to align corresponding parts of
successive lines, but only when the lines are in any case very similar
in form.

I use class-private names (starting, but not ending, with double
underscores) more extensively than PEP 8 believes in, making very little
use of single-underscore names (in newer code; my older lazy attribute
infrastructure uses them extensively).  There are various places where I
have used names with double underscores at start and end that I shall
change to use single underscores, when time permits, in accordance with
PEP 8.

I am quite fussy about avoiding clutter in name-spaces: I factor out
tool functions and tunnel them in, as defaults of arguments never
passed, for other functions to use them, then del the tool after its
client.  Usually I do this with a private method as the client, but
sometimes I just document that no extra parameters should be passed to
the method.  In the latter case, I should probably deploy @accepting
from study.snake.decorate to prevent passing of extras.

Some class names are all lowercase, rather than using the CapWords
pattern: these are generally classes I intend for use as if they were
functions (e.g. several decorators, particularly those related to
properties).  I am not a fan of variable names that append an underscore
to a word which would otherwise be a keyword; I prefer to avoid use of
the keyword, instead.  For variables that are expected to be classes, I
tend to use the name cls, as PEP 8 suggests.

I tend to end doc-strings in """... final words.\\n""" so that they
print cleanly; I'm a bit skeptical of the recommended practice of
leaving a blank line and indentation at the end of a doc-string.  A good
doc-gen tool might talk me round, though.

History
-------

The first toy (April 1998) was an early version of study.maths.ratio's
Rational and approximate().  The next (Jan 1999) was a first stab at
what has since become the Quantity class, initially just supporting a
numeric value and its units; it soon grew the ability to track error
bars on its numeric values.  Right at the outset, Quantity included the
SI units, some derived units, and a few silly archaic units (that now
live in study.value.archaea).  The next innovation was the first
incarnation of Lazy (now in study.snake.lazy and deprecated in favour of
study.cache.property).  This sets the pattern for the three main areas
of development: mathematical toys, scientific quantities and pythonic
support infrastructure.

All of this happened in RCS; later, I migrated it to CVS.  The way I'd
been using RCS made this easy, as it happened: I had all my RCS files in
a directory tree mirroring the source tree, whose top-level directory
had a symlink named RCS pointing at the top of the RCS tree; each
sub-directory then had an RCS -> ../RCS/name symlink using its own
name.  In this era of history, I could re-arrange the directory tree at
whim, without making any changes visible in the commit history, aside
from the consequent changes to how modules import one another.  I think
the last major re-structuring was 614f6ac in 2007/March, although a few
relatively minor moves may still be hidden from the version-control
history between then and the summer of 2010, when I imported the whole
lot into git.  So checking out historical revisions before summer 2010
may, and before spring 2007 certainly shall, give you a source tree that
probably doesn't even import cleanly.
