"""Code to manipulate /usr/share/dict/'s word-list files.

See study.LICENSE for copyright and license information.
"""
from study.crypt.characters import Counter

class WordList (dict):
    def __init__(self, src="/usr/share/dict/words"):
        self.__file = src
        self.__count()

    class Unique (Counter):
        __upinit = Counter.__init__
        def __init__(self):
            self.__upinit()
            self.__unique = set()

        __upgest = Counter.ingest
        def ingest(self, text):
            text = text.lower().strip()
            for word in text.split():
                if word not in self.__unique:
                    self.__unique.add(word)
                    self.__upgest(word)

    def __count(self, Bok=Unique):
        ws = Bok()
        ws.digest(self.__file)
        seq = map(lambda (ch, n): (n, ch), ws.items())
        seq.sort()
        seq.reverse()
        self.__freq = tuple(seq)
    del Unique

    def frequent(self, count=None):
        """Reports the order of decreasing frequency of characters.

        Returns a string that starts with the most frequently-used
        character and works its way down towards less frequently-used
        characters.  Note that this counts letter frequency of unique
        words, after lower-casing, in the word-list.  This is not the
        same as letter frequencies in normal bodies of text, where some
        words appear much more frequently than others, hence the letters
        in those words occur more often.

        Optional argument, count, is a number of characters to return;
        only this many most frequent characters are reported;
        .frequent(n) is equivalent to .frequent()[:n] but may save some
        computation.\n"""

        if count is None: seq = self.__freq
        else: seq = self.__freq[:count]
        return ''.join(map(lambda (n, ch): ch, seq))

    def select(self, checker):
        """Returns an iterator over words accepted by a given test.

        Single argument is a callable, typically the .search method of an
        instance of re; each line from the word-list is passed to it in turn;
        for each of those for which it returns a true value, the iterator
        yields a two-ple of the line and checker's return (the latter is
        included so that you don't need to call checker again to get its
        answer when, for example, it's the match object from a regex's
        .search).  Note that each line (except possibly the last) shall
        include a trailing newline.\n"""

        fd = open(self.__file)
        try:
            for line in fd:
                ans = checker(line)
                if ans:
                    yield line, ans
        finally: fd.close()

del Counter
