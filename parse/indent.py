"""Canonicalisation of indentation.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import Cached, lazyprop

from study.cache.sequence import LazySeq
class TabStop (LazySeq):
    """A pattern of indentation offsets.

    A TabStop object encodes the positions in a line at which a tab character
    can end.  If the cursor is n characters from the right margin when a tab
    is encountered, then the tab moves the cursor to the first tab-stop
    position (strictly) greater than n.  This class supports three ways of
    expressing the pattern of tab-stop positions:
     * a positive integer, each multiple of which is a tab-stop position;
     * a function encoding a (possibly infinite) sequence; or
     * an iteratable.
    In each case, a strictly increasing sequence of values is obtained (on
    demand) and cached.

    An integer n is treated (via the following) as the function lambda i:
    n*i. A function f is treated as the iterator that yields, successively:
    f(0), f(1), ...  All three cases are thus reduced to iterators.  The
    resulting iterator is then filtered to make it strictly increasing and
    positive: each yield that is strictly positive and greater than all
    earlier yields is kept, any other yields are discarded.  The values from
    this iterator are then cached as tab-stop positions.\n"""

    # A few tool functions, deleted below:
    def from_iter(seq):
        n, c = 0, []
        for m in seq:
            if m > n:
                yield m
                n = m
                c.append(m)

        while c:
            for m in c: yield n + m
            n += m

        raise ValueError('No positive tab-stops provided', seq)

    def from_func(func):
        i = n = 0
        while True:
            m = func(i)
            if m > n:
                yield m
                n = m
            i += 1

    def from_numb(numb):
        n = 0
        while True:
            n += numb
            yield n

    @staticmethod
    def __monotonic(tabstop, func=from_func, numb=from_numb, sequ=from_iter):
        try: it = iter(tabstop)
        except TypeError: it = () # (definitely isn't tabstop)
        if it is not tabstop:
            try: tabstop[:]
            except TypeError:
                if callable(tabstop):
                    return func(tabstop)
                elif tabstop > 0:
                    return numb(tabstop)
                else:
                    raise ValueError(
          'Expected positive integer, sequence, function or iterator',
          tabstop)

        return sequ(tabstop)
    del from_func, from_numb, from_iter

    __upinit = LazySeq.__init__
    def __init__(self, tabstop=8):
        """Initialize the tab-stops."""
        self.__upinit(self.__monotonic(tabstop))

    def advance(self, end, start=0):
        """Indicate what to type to advance from one column to another.

        Required argument, end, is the column at which one wishes to type the
        next character after the spacing; optional second argument, start
        (defaults to 0), is the cursor position prior to this typing.

        Returns a tuple (t, s) such that, if the cursor is initially at column
        start (i.e., absent backspacing, the last character typed was: either
        at column start-1; or, for start = 0, a newline), t tabs followed by s
        spaces will leave the cursor at column end (i.e. ready to type a
        character at that position).\n"""
        if start > end:
            raise ValueError('', start, end)

        i = t = 0
        while self[i] <= start: i += 1
        while self[i+1] <= end:
            i += 1
            t += 1
        return t, end - self[i]

    def tab(self, prior):
        """Returns first entry in self greater than prior.

        This is the column you'll be in after, with cursor initially at column
        prior, you type a tab character.\n"""
        for m in self:
            if m > prior:
                return m

        raise ValueError('Insufficient tab-stop positions', prior)

del LazySeq

class Indent (object):
    """Indentation canonicaliser, a.k.a. de/tabifier.

    Associates, with each position between characters in a text (also with
    start and end), a 'column number'; this is zero at the start of the text
    and immediately after each '\n' in the text; the column number after a
    character in the text otherwise depends on both the character and the
    column number before it; most characters simply increase the column number
    by one; the backspace character BSP decreases it by one (this may yield a
    negative column number); the delete character DEL undoes the change made
    by the preceding character; each TAB character ends at the smallest column
    number, greater than that at which it starts, in a specified sequence of
    'tab stops'; see the TabStop class for details.

    See also the .expandtabs([width]) method of strings; it will often give
    you a more light-weight way to achieve what you need.\n"""

    def __init__(self, tabstop=TabStop()):
        """Initialize indentation canonicaliser.

        Single optional argument, tabstop, is either an instance of TabStop
        (q.v.) or an input suitable to be passed to its constructor.  If
        omitted, the default TabStop() is used.\n"""

        if isinstance(tabstop, TabStop): self.__tabs = tabstop
        else: self.__tabs = TabStop(tabstop) # error if unsuitable param


class Text (str):
    """Like a string object, but tracking indentation at end.
    """
