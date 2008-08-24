"""Helper function for printing.

  printmenu -- prints an unordered list in multi-column format

$Id: show.py,v 1.3 2008-08-24 20:53:24 eddy Exp $
"""
from row import transpose
try:
    from os import environ
    try: wide = int(environ['COLUMNS'])
    finally: del environ
except (KeyError, ValueError, ImportError): wide = 72

def printmenu(menu=None, width=wide):
    """Pretty-prints the known errors.

    Arguments are optional (but omitting the first makes this a no-op):

      menu -- a sequence of strings

      width -- width of available display (default: int(os.environ['COLUMNS'])
               if available, else 72): the output will be formatted in a table
               with as many columns as the display permits.  Any sufficiently
               low value (e.g. 0) will force single-column output, which ignores
               width.

    Single-column output will respect the order of the given sequence; for
    multi-column output, the strings are sorted by length so that the strings in
    each column are all of similar length; this is a display-space-saving
    optimisation.\n"""
    if not menu: return

    def lines(fmt, seq):
        print reduce(lambda x, y: x + '\n' + y,
                     map(lambda r, f=fmt: f % r, seq[1:]),
                     fmt % seq[0])
        return

    siz, wide = max(map(len, menu)), '%%%ds'

    if 2 * (1 + siz) > (width or 0):
        # single-column output; if we can't do it well, we can at least do it cheaply.
        return lines(wide % siz, menu)

    menu.sort(lambda a,b: cmp(len(a), len(b)))
    def span(nr, seq=menu):
        # if we display menu in nr rows, going down each column then stepping
        # right, span(nr) will be the width of the resulting top row.
        sum, at = 0, len(seq)-1
        while at >= 0:
            sum = sum + len(seq[at]) + 1 # space after each item
            at = (at / nr) * nr - 1 # last item in each column is longest
        if sum: return sum - 1 # ... but we don't need the final space
        return 0

    tall = len(menu) / (width / (siz + 1)) # rough estimate
    if span(tall - 1) <= width:
        tall = tall - 1
        while span(tall - 1) <= width: tall = tall - 1
    else: # (I don't belive this can happen, but here's the right thing to do:)
        while span(tall) > width: tall = tall + 1

    # chop menu up into a list of columns, each of length tall.
    lo, cols = 0, []
    while lo < len(menu):
        hi = lo + tall
        cols.append(menu[lo:hi])
        lo = hi

    # formats of the columns (wide % n is a format string, for integer n):
    formats = map(lambda w, f=wide: f % w, map(lambda c: len(c[-1]), cols))

    def tidy(seq, flip=transpose):
        """Minor tidy-up *after* transpose.

        In effect, transpose reads cols as a rectangle, substituting None into
        any gaps left by cols of varying length: tidy takes out any instances of
        None at the ends of the resulting rows (safe in the knowledge that these
        are gap-fillers inserted as above, and that there will be at most one in
        each row since only the last column might be shorter than the rest). """

        if not seq[-1]: return tuple(seq[:-1]) + ('',)
        return tuple(seq)

    return lines(reduce(lambda x,y: x + ' ' + y, formats),
                 map(tidy, flip(cols)))

del transpose, wide
