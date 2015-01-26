Eddy's almost-pythonic coding style
===================================

I don't *entirely* agree with PEP 8 - so anyone wanting me to accept any patches
to my branches of this package would do well to respect my idiosyncrasies as to
coding style, if only to keep the patches small.  If I have any patches for
python's standard library, I'll abide by PEP 8; if any of this code is ever
invited into the standard library, I'll convert it; but, aside from that, this
package follows my slightly different coding conventions.

Spacing
-------

I'm a bit stingier with vertical space than PEP 8 would be: I use fewer blank
lines and, where I can get away with putting a single-statement block on the
same line as the control structure to which it's subordinate, I generally
do.  (However, I entirely agree with PEP 8 about not putting several statements
on one line separated by semicolons.)  I have used right margins of 80 and 78 at
various times in the history of this package; I'm now back to using 80, to match
PEP 8's advice, but some old code and (especially) doc-strings may use 78.

For horizontal space, I *mostly* agree with PEP 8, but do leave spaces at start
and end of { key: value } dictionary displays and, when doing arithmetic inside
an array index, I tend to discard horizontal spaces, e.g. ``seq[1+i]``.  I
sometimes - e.g. when a block of methods have similar one-line implementations -
use spacing to align corresponding parts of successive lines, but only when the
lines are in any case very similar in form.

I routinely break up longer files using the form-feed character, ``\f``, on a
line of its own.  It's a space (i.e. ``'\f'.isspace()`` is ``True``), so python
ignores such lines; but my editor (``emacs``) recognises it as a page boundary
(this is indeed what it means in ASCII) for purposes of some commands that make
it easier for me to navigate around the file.  You might even find that some
printers shall honour these as page breaks, too.  I typically put one class or
function, or a group of related small classes and functions, onto each
page.  The file's documentation and file-global ``import`` statements (sometimes
with other globals) form the first page; subsequent pages are apt to ``import``
some names at start and ``del`` them at end; and there's often a one-line (or,
at least, short) last page on which I ``del`` any globals that I don't intend to
export.

Names
-----

I use class-private names (starting, but not ending, with double underscores)
more extensively than PEP 8 believes in, making very little use of
single-underscore names (in newer code; my older lazy attribute infrastructure
uses them extensively).  There are various places where I have used names with
double underscores at start and end that I shall change to use single
underscores, when time permits, in accordance with PEP 8.

I am quite fussy about avoiding clutter in name-spaces (hence the ``del`` uses
mentioned above): I factor out tool functions and tunnel them in, as defaults of
arguments never passed, for other functions to use them, then ``del`` the tool
after its client.  Usually I do this with a private method as the client, but
sometimes I just document that no extra parameters should be passed to the
method.  In the latter case, I should probably deploy ``@accepting`` from
``study.snake.decorate`` to *prevent* passing of extras.

Some class names are all lowercase, rather than using the CapWords pattern:
these are generally classes I intend for use as if they were functions
(e.g. several decorators, particularly those related to properties).  I am not a
fan of variable names that append an underscore to a word which would otherwise
be a keyword; I prefer to avoid use of the keyword, instead.  For variables that
are expected to be classes, I tend to use the name ``cls``, as PEP 8 suggests.

Ediosyncrasies
--------------

All of the above can be considered idiosyncratic, but the following are
particularly so and I might be a bit more willing to change them ...

I tend to end doc-strings in ``"""... final words.\n"""`` so that (in an
interactive session) they print cleanly; I'm a bit skeptical of the recommended
practice of leaving a blank line and indentation at the end of a doc-string.  A
good doc-gen tool might talk me round, though.

I have had a tendency to spell in/equality comparisons with ``-1`` in terms of
adding one to the value to be compared and using the result as a boolean, via
the usual implicit comparison with zero.  (This habit arises from a murky past
in which I worked on a system where the signedness of some abstract types, for
which ``-1`` was sometimes used as a sentinal value, was
indeterminate.  Compilers warn about signed/unsigned comparison if one tests
against ``-1`` when the type is unsigned.  Neither test works properly if the
unsigned type's size is less than that of ``int``; but we knew the types in
question were big enough, so the ``+1`` test avoided the warning and worked in
all the same cases as comparing with ``-1``.  This is all irrelevant to python,
of course.)  To the few who are used to that practice, it's just as readable;
but it confuses everyone else !
