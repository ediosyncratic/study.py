"""Tool for use by table-parsers.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop, Cached

class Table (Cached):
    def __init__(self): self.rows, self.caption = [], None
    __caption = 'caption'
    def open_caption(self):
        assert not hasattr(self, 'caption')
        self.__text, self.__cell = '', self.__caption
    def open_row(self): self.__currow, self.__cell = [], 0
    def open_cell(self):
        self.__end_cell()
        self.__text, self.__cell = '', self.__cell + 1
    def add_data(self, text): self.__text += text

    def __end_cell(self):
        try:
            if self.__cell is self.__caption: return
        except AttributeError: return
        assert self.__cell > 0
        try: self.__text
        except AttributeError: pass
        else: self.close_cell()

    def __tidy_text(self):
        # Eliminate extraneous space from text:
        lines = map(str.rstrip, self.__text.split('\n'))
        del self.__text
        # Any common indentation (except possibly on first line):
        indents = map(lambda txt: txt[:-len(txt.lstrip())],
                      filter(None, lines[1:]))
        if indents:
            cut = min(map(len, indents))
            while cut > 0:
                seq = iter(indents)
                dent = seq.next()[:cut]
                for it in seq:
                    if it[:cut] != dent: break
                else: # they all agree :-)
                    trim = lambda x, c=cut: x[c:]
                    if lines[0] and lines[0][:cut] != dent:
                        lines[1:] = map(trim, lines[1:])
                    else: lines = map(trim, lines)
                    break
                cut -= 1

        # Leading and trailing blank lines:
        while not lines[0]:  lines = lines[1:]
        while not lines[-1]: lines = lines[:-1]

        # Re-assemble what remains:
        return '\n'.join(lines)

    def close_cell(self):
        self.__currow.append(self.__tidy_text())
        assert self.__cell == len(self.__currow), (self.__cell, self.__currow)

    def close_row(self):
        self.__end_cell()
        assert not self.rows or len(self.rows[0]) == len(self.__currow)
        self.rows.append(tuple(self.__currow))
        del self.__currow, self.__cell

    def close_caption(self):
        assert self.__cell is self.__caption
        self.caption = self.__tidy_text()
        del self.__cell

    @lazyprop
    def width(self, ignored=None):
        if self.rows: return len(self.rows[0])
        raise AttributeError("I don't yet know my width :-(")

    def __len__(self): return len(self.rows)

    class Transform (object):
        """Base-class for Table.close()'s transform.

        Callers of Table.close() needn't use an instance of a class
        derived from this (python isn't static-typed), but an instance
        of this class provides the simplest way to transform the data of
        a table.  Since this base-class's instances have no internal
        state, all methods are static or class methods; a real
        implementation would be likely to have real methods and some
        internal state, e.g. remembering the table's caption.\n"""

        def __init__(self, caption):
            """The easiest factory is a constructor.

            In general, Table.close() accepts a factory callable, that
            is passed the table's caption and returns something that
            behaves like an instance of this class; naturally, this
            class itself meets that criterion.

            A more sophisticated factory might be a function that
            inspects the caption to decide among several classes to
            instantiate, for example.\n"""

            return

        @classmethod
        def first(cls, headings):
            """Create the initial result sequence.

            This is called with the first row of the table, typically
            the headings of the columns.  Must return an object with an
            append method that works like list.append; this method will
            be passed the returns from .each(), q.v.  The return from
            .first() will in turn be passed to .package() when complete.

            This basic version returns a list containing just one entry,
            .each(headings), treating the first row just the same way
            each later row shall be treated.  Where the table's first
            row really is a set of headings, it may be more apt to
            return an empty list and record information from the
            headings, for use in processing of each later row.

            For example, a more sophisticated transform might identify
            which columns are of interest and prepare an index sequence,
            for later use in extracting the interesting fields from each
            row; or the first row might be used to construct a new class
            to be used to digest each subsequent row.\n"""

            return [ cls.each(headings) ]

        @staticmethod
        def each(row):
            """Process a single row of the table.

            Input is a tuple containing the texts of the cells of a row
            of the table.  The transform object has previously seen the
            first (presumably headings) row of the table; it can apply
            column-appropriate transformations to the entries in each
            row (e.g. converting strings that represent numbers to
            numbers), select which columns are of interest and build an
            object packaging the entries from these columns suitably to
            represent the row.

            Anything goes.  This basic version simply .strip()s each
            entry in the row and returns the results as a tuple.\n"""

            return tuple(map(lambda x: x.strip(), row))

        @staticmethod
        def package(all):
            """Final processing of the table data.

            After .first()'s return has .append()ed one return from
            .each() per row (after the first) of the table, .first()'s
            return is passed through .package() and the result is used
            to represent the table.

            This basic version simply turns the mutable list, used to
            accumulate the rows, into a tuple; one could equally use a
            frozenset instead of a tuple.  A more sophisticated version
            might have .each() return (key, object) twoples to later be
            converted by .package() into a dictionary, using one
            column's entry as the key and the others to build the
            object.\n"""

            return tuple(all)

    def close(self, factory=Transform):
        """Close the table and return an object representing it.

        Single argument, factory, must be a callable, which will be
        passed a single argument: None if the table had no caption
        element, otherwise the (possibly empty) text of the last caption
        element (earlier captions shall have been discarded).  It must
        return an object with methods corresponding to those of an
        instance of Transform (q.v.), which shall be used to transform
        the table's data into a final object representing the
        table.  The factory may arrange for the transform object to
        remember the caption, for later use on the object returned by
        the .package() method of the transform.\n"""

        try: self.__currow
        except AttributeError: pass
        else: assert False, ('Rows should be explicitly closed', self.__currow)

        transform = factory(self.caption)
        seq = iter(self.rows)
        all = transform.first(seq.next())
        for it in seq: all.append(transform.each(it))
        return transform.package(all)
