"""Calculation of letter-frequencies in text.

Provides class Counter, which extends dict with some methods for counting
frequencies of tokens.  Instances are, after running methods appropriately,
suitable for use as the symbol-frequency argument required by the constructor of
the Huffman class (q.v.) provided by Huffman.py in this directory.
"""

class Counter (dict):
    """Count frequencies of data.

    Extends the build-in mapping class dict with the following methods:

      .ingest(seq) -- count frequencies of entries in seq.
      .engest(seq) -- as .ingest, but process HTML character entities.
      .digest(filename [, entities]) -- read input from file.
      .scan(dir [, regex [, entities]]) -- scan directory tree for input.

    For example, .scan('~/public_html', False, re.compile('\.(x?html|txt)$'))
    can be used to discover the frequencies with which a web-site's text content
    uses characters.  The Counter object can subsequently be used to construct
    an optimal Huffman code for this text content.\n"""

    def ingest(self, text):
        """Count the frequencies in a given sequence of tokens.

        Single argument is a sequence, e.g. a string, whose entries are to be
        included in self's distribution.  For the case where HTML character
        entities are to be parsed, see .engest().\n"""

        for ch in text: self[ch] = self.get(ch, 0) + 1

    def engest(self, text):
        """Count character frequencies, handling character entities.

        Single argument is a string (or sequence of characters), whose entries
        are to be included in self's distribution, save that each sequence of
        characters matching /&\S*?;/ is to be treated as a single
        'character'.  (This regular expression - an arbitrary sequence of
        non-space characters, enclosed in '&' on the left and ';' on the right -
        is more general than the actual lexical form of character entities, so
        may match some texts that are not character entities.)

        Note that no attempt is made to identify when two such texts happen to
        indicate the same character (formally: unicode code-point),
        e.g. '&otimes;' and '&#8855;' are treated as distinct
        characters.  Likewise, if unicode strings are either .ingest()ed or
        .engest()ed, literal unicode characters are not identified with
        character entities that happen to encode them.\n"""

        tmp = ''
        for ch in text:
            if tmp:
                if ch.isspace():
                    self.ingest(tmp)
                    tmp = ''
                else:
                    tmp += ch
                    if ch == ';':
                        self[tmp] = self.get(tmp, 0) + 1
                        tmp = ''
            elif ch == '&': tmp = ch
            else: self[ch] = self.get(ch, 0) + 1

    def digest(self, file, entities=False):
        """Count frequencies of characters in a file.

        Required argument, file, is the name (suitable for passing to open(),
        q.v.) of a file whose contents are to be included in self's
        distribution.  Optional second argument, entities, defaults to False,
        selects whether to process HTML character entities: if true, the file's
        contents are .engest()ed, else (the default) .ingest()ed.\n"""

        if entities: eat = self.engest
        else: eat = self.ingest
        fd = open(file)
        try:
            for it in fd: eat(it)
        finally: fd.close()

    import os
    def scan(self, dir, entities=False, pattern=None,
             walk=os.walk, join=os.path.join):
        """Scan a directory tree for files to .digest()

        Required argument, dir, is the name of a directory under which to scan,
        recursively, for files.  Note that any symlinks to directories shall be
        ignored (to avoid problems with non-tree structures that might result
        otherwise).  Symlinks to files are followed.

        Optional arguments (do *not* pass more than three arguments in all):
         entities -- as for .digest(), q.v.; is passed as second argument to
                     each call to .digest().
         pattern -- None (default) to .digest() all files; else an object with a
                    .search() method (e.g. a regular expression from the re
                    module); the name of each file (excluding directory path) is
                    passed to this .search() method and the file is .digest()ed
                    if the return value is true (as happens when re.search()
                    succeeds).

        Thus if pattern is re.compile('\.x?html$'), all .html and .xhtml files
        shall be processed.\n"""

        if pattern is None: prune = lambda s: s
        else: prune = lambda s, c=pattern.search: filter(c, s)

        for d, ss, fs in walk(dir):
            for x in ('CVS', '.git'): # any more ?
                if x in ss: ss.remove(x) # skip version-control subdirs.

            for name in prune(fs):
                self.digest(join(d, name), entities)
    del os
