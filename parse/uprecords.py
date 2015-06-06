"""Parser for uptimed's database of past uptimes.

The data record how long a computer ran between booting and shutting down
(whether gracefully or otherwise).

See study.LICENSE for copyright and license information.
"""

from study.cache.property import lazyprop
from study.snake.sequence import iterable
import time, datetime

class Record (tuple):
    def __init__(self, seq):
        self.ran, self.start, self.note = seq
        self.end = self.start + self.ran

    def overlaps(self, other):
        return self.start < other.end and other.start < self.end

    @lazyprop
    def date(self, delta=datetime.timedelta):
        return str(delta(seconds=self.ran))

    @lazyprop
    def duration(self, fmt=time.strftime):
        return fmt('%c', self.start)

    @lazyprop
    def days(self): return self.hours / 24
    @lazyprop
    def hours(self): return self.ran / 36e2

    def __sane(self, end=None, time=time.time):
        if end is None: end = time()
        if self.end > end:
            raise ValueError("Run-interval extends past credible end-time",
                             self, end)

    def __check(self, other, merge, was=None):
        if not self.overlaps(other): return False
        if not merge or self.start != other.start or self.note != other.note:
            raise ValueError("Overlapping ranges", self, other)
        other.__sane(was) # self is safe, but need to check other !
        if other.ran > self.ran:
            self.ran = other.ran
            self.end = self.start + self.ran
        return True

    def __resort(self, most):
        # Move self (possibly) up most's .__less chain:
        if self is most: return most # already at top

        run = most
        # We can be sure self is *in* the .__less chain:
        while run.__less is not self: run = run.__less
        # Nothing to do if still below next record up:
        if run.ran >= self.ran: return most

        try: run.__less = self.__less # snip self out of chain
        except AttributeError: del run.__less # self was last in chain

        if self.ran >= most.ran: # moved up to first place:
            self.__less = most
            return self

        run = most
        # This definitely ends before run gets to where we changed .__less,
        # above; so no need to worry that .__less might run out:
        while run.__less.ran > self.ran: run = run.__less
        assert run.ran > self.ran
        # Insert self after run:
        self.__less = run.__less
        run.__less = self
        return most

    def insert(self, last, most, merge=False):
        """Insert self into the lists sorted by date and duration.

        Required arguments, last and most, are the heads of lists of
        Record()s; last is the most recent in the .__prior-linked list with
        .start decreasing; most has the longest duration in the .__less-linked
        list with .ran decreasing.  Optional argument, merge, defaults to
        False; if it is true, self is allowed to have the same start-time and
        note as an existing record, with which it shall be merged.  Other than
        this, overlapping ranges are not allowed; nor is a range whose
        end-point is in the future.

        Returns the heads of the two lists, i.e. last and most, except that
        self may have replaced either (or both) of them.\n"""

        if last is None:
            assert most is None
            self.__sane()
            return self, self

        if last.__check(self, merge):
            # Merged into old last entry instead of inserting anew.
            # May have increased .ran, so maybe revise .__less order:
            return last, last.__resort(most)

        if self.start >= last.start:
            self.__sane()
            last.__sane(self.start)
            self.__prior, last = last, self
        else:
            run = last
            try:
                while self.start < run.__prior.start:
                    run, was = run.__prior, run.start
                    if run.__check(self, merge, was):
                        # Merged into old entry instead of inserting anew, as above.
                        return last, run.__resort(most)

            except AttributeError: pass # no .__prior
            else: self.__prior = run.__prior
            self.__sane(run.start) # check self ended before next
            run.__prior = self

        if self.ran > most.ran: self.__less, most = most, self
        else:
            run = most
            try:
                while self.ran < run.__less.ran:
                    run = run.__less
            except AttributeError: pass # no .__less
            else: self.__less = run.__less
            run.__less = self

        return last, most

    @iterable
    def bydate(self):
        try:
            while True:
                yield self
                self = self.__prior
        except AttributeError: pass

    @iterable
    def bylength(self):
        try:
            while True:
                yield self
                self = self.__less
        except AttributeError: pass

del time, datetime, lazyprop, iterable

class History (object):
    def __init__(self, file="/var/spool/uptimed/records"):
        """Reads records from the database.

        Single argument, file, is optional: it names the file to read.  Its
        default is the file uptimed uses as its database.\n"""

        last = most = None
        count = 0
        for it in self.__parse(file):
            last, most = it.insert(last, most)
            count += 1

        self.latest, self.longest, self.count = last, most, count

    @staticmethod
    def __parse(file):
        with open(file) as fd:
            for line in fd:
                ran, start, note = line.rstrip().split(':', 2)
                yield Record((int(ran), int(start), note))

    def __len__(self): return self.count

    def merge(self, other):
        """Transfer other's entries into self's lists.\n"""
        last, most = self.latest, self.longest
        for run in other.bydate():
            last, most = run.insert(last, most, True)
        self.latest, self.longest = last, most
        return self

    def bydate(self):
        "Iterator over records of increasing age"
        return self.latest.bydate()

    def bylength(self):
        "Iterator over records of increasing duration"
        return self.longest.bylength()
