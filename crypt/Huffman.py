"""Implementing Huffman coding.

Provides:
  Huffman -- class implementing Huffman encodings
  alphabet -- default symbol set used by Huffman (q.v.) for encoded data
"""
from study.snake.lazy import Lazy
alphabet = ''.join(filter(lambda c: len(repr(c)) < 4 and not c.isspace() and c != "'",
                          map(chr, range(32, 126))))

class Huffman (Lazy):
    """Implementation of Huffman encoding.

    See __init__ for constructor documentation.  In particular: whether the
    messages to be encoded are strings or sequences of some other type of token
    depends on the frequency distribution passed to it; and whether the message
    is encoded one token at a time or in blocks is controlled by its blocksize
    parameter, N.  In the following, a 'message block' may be a single token or
    a block of tokens, depending on this last.

    Methods:
      encode(message) -- encodes a message as a string
      encode(text) -- decodes a string to recover the message

    Lazily computed attributes:
      .mapping -- { message blocks : encoded string fragment, ... }
      .reverse -- reverse of .mapping
      .length -- weighted average number of output characters per input token
      .entropy -- entropy of the encoded strings, using the number of symbols in
                  the encoded string alphabet as base for logarithms; this
                  ensures that .entropy >= .length always.

    Also supports representation by depicting .mapping as a table.\n"""

    def __init__(self, P, symbols=alphabet, N=1, blank=None, tail=None):
        """Initialize a Huffman encoder.

        First argument, P, is a mapping (e.g. a Counter object from this
        directory's characters.py, q.v.) from symbols to their relative
        frequencies; if a sequence is supplied instead, it is interpreted as a
        mapping from its indices to its entries.  If every key of P is a
        character (i.e. a single-character string, whether 8-bit or unicode),
        the 'messages' to be encoded are taken to be strings; otherwise, they
        are sequences (e.g. lists or tuples) of the tokens used as keys.  (If
        what you really want is to encode non-string sequences of single
        characters, either ''.join() them or include a key of P with frequency
        zero - so that it'll be ignored - which isn't a single character.)  In
        principle, P is normalized to yield a probability distribution, but this
        is not actually necessary, as long as it has no negative values.

        Optional arguments:

          symbols -- the alphabet to use for the encoded message (default: the
                     nicely printable characters - see below).

          N -- the number (default: 1) of input symbols to be handled in each
               block: only messages which are a multiple of this block size can
               be encoded by the resulting Huffman encoder.

          blank -- None (the default), to enforce the message length limit
                   detailed above for N; or a key (or index) of P with which a
                   message may be padded to make up its length to a multiple of
                   the block size; in the latter case, the decoder shall always
                   strip up to N-1 of these blanks from each message.

          tail -- entry in symbols which is apt to be lost from the end of an
                  encoded text (e.g. because it's the blank of a down-stream
                  Huffman); when decoding, enough of these shall be appended, if
                  needed.  Default, None, means to omit such bodgery.

        The compressed form is, in all cases, a string; if you pass symbols='01'
        you can subsequently read each eight output characters as a binary
        number (see base.binary in this sub-package) and, via chr, convert it to
        an octet stream; or you could pass ''.join(map(chr, range(256))) to get
        directly to such a stream (albeit probably less efficiently, at least if
        len(P) < 256).  Likewise, you could use symbols='01' and read each six
        characters as a single base-64 token and use base64-encoding; this is
        somewhat more robust than the full octet range, for various purposes
        (e.g. it can be transmitted over media on which a parity bity is needed
        to detect transmission errors).

        However, since we're actually working in python, the default used here
        is so chosen as to ensure that any encoded string's repr is only two
        bytes longer than it (due to a single quote at start and end - but no
        character in between needs to be represented by an escape sequence); the
        encoded strings can be saved to file as python strings and robustly
        transmitted over channels which use parity checks; they can also be
        split into lines of some convenient length by the insertion of '\n'
        characters without danger of being changed by any software that strips
        or reformats spaces (since it contains no spaces); when the '\n' breaks
        are subsequently removed, the result should decode safely.  These
        constraints allow us 92 symbols, yielding a symbol set a bit over 8%
        more efficient than using base64 encoding.\n"""

        # Block size:
        if N != int(N) or N < 1:
            raise ValueError(N, 'Block size should be a positive integer')
        self.__block_size = N

        # Output symbols:
        if len(symbols) < 2:
            raise ValueError(symbols, 'Need at least two output symbols')
        self.__symbols = symbols

        if tail is not None:
            if tail not in symbols:
                raise ValueError(tail,
                                 'Tail padding must be a valid output symbol',
                                 symbols)
            self.__tail = tail

        # Check P is a mapping or sequence:
        try: P.items() # mapping
        except AttributeError:
            try: P[:] # sequence
            except: raise ValueError(P,
                                     'Probability distribution must be mapping or sequence')
            # Convert P to a mapping:
            bok = {}
            for i in range(len(P)):
                bok[i] = P[i] # raises exception if P is not a sequence ...

            self.__distribution = bok
        else: self.__distribution = P

        # Are our symbols strings or are we working with sequences ?
        self.__str = len(filter(lambda x: not(isinstance(x, basestring) and len(x) == 1),
                                P.keys())) == 0

        # Verify no negative weights:
        bad = filter(lambda (x,y): y < 0, P.items())
        if bad: raise ValueError('Negative token frequencies', dict(bad))

        # Input padding:
        if blank is not None:
            if not self.__distribution.has_key(blank):
                raise ValueError(blank,
                                 'Invalid blank character specified: not in symbol set')
            self.__blank = blank

    def encode(self, message):
        """Encode (compress) a message.

        Single parameter is the message; this is either a string or a sequence
        of tokens, see constructor documentation.  Returns a string.\n"""
        rem = len(message) %  self.__block_size
        if rem:
            try: pad = self.__blank
            except AttributeError:
                raise ValueError(self.__block_size,
                                 'Cannot pad message to multiple of block size: no blank')
            if self.__str:
                message = message + (self.__block_size - rem) * pad
            else:
                message = tuple(message) + (self.__block_size - rem) * (pad,)
        elif not self.__str:
            message = tuple(message)

        txt, n, code = '', self.__block_size, self.mapping
        try:
            while message:
                message, txt = message[n:], txt + code[message[:n]]
        except KeyError:
            raise ValueError('Unable to encode', message, code)

        return txt

    def decode(self, txt):
        """Decode (uncompress) a message.

        Single parameter is a string, the encoded message.  Return value is a
        string or tuple of tokens; see constructor documentation.\n"""
        code = self.reverse
        if self.__str: message = ''
        else: message = ()
        try:
            while txt:
                i = 1
                while not code.has_key(txt[:i]) and i < len(txt): i = 1 + i
                if not code.has_key(txt[:i]) and i >= len(txt):
                    try: pad = self.__tail
                    except AttributeError: pass
                    else:
                        cut = max(map(len, code.keys()))
                        while i < cut and not code.has_key(txt):
                            txt, i = txt + tail, i + 1

                txt, message = txt[i:], message + code[txt[:i]] # KeyError if bad txt
        except KeyError:
            raise ValueError(txt, 'Unable to decode this message', code)

        assert len(message) % self.__block_size == 0

        try: pad, n = self.__blank, self.__block_size - 1
        except AttributeError: pass
        else: message = message[:-n] + message[-n:].rstrip(pad)

        return message

    def __repr__(self):
        """Representation.

        The representation used here is simply a table, in two rows; the first
        row shows the (blocks of) tokens recognised by the encoding, the second
        shows the string encoding each below it.\n"""
        try: ans = self.__repr
        except AttributeError:
            its = self.mapping.items()
            its.sort(lambda (k,v), (h,u): cmp(u, v) or cmp(h, k))
            if not self.__str:
                if self.__block_size == 1:
                    its = map(lambda ((k,), v): (k, v), its)
                its = map(lambda (k, v): (str(k), v), its)
            fmt = map(lambda (k, v): '%%%ds' % max(len(k), len(v)), its)
            ans = '\n'.join(map(lambda x, f=' | '.join(fmt): f % x,
                                map(lambda *args: args, *its)))
            self.__repr = ans

        return ans

    def _lazy_get__block_map_(self, ig, possible=lambda (k, v): v):
        """Mapping from possible blocks to their probabilities."""
        if self.__str:
            bok = { '': 1 }
            def add(seq, item): return seq + item
        else:
            bok = { (): 1 }
            def add(seq, item): return seq + (item,)

        N, dist = self.__block_size, filter(possible, self.__distribution.items())
        while N > 0:
            N -= 1
            old = filter(possible, bok.items())

            bok = {}
            for k, v in old:
                for s, q in dist:
                    bok[add(k, s)] = q * v

        return bok # { block => probability }

    def _lazy_get_reverse_(self, ig):
        """.reverse maps encoded string fragments to decoded blocks.\n"""
        bok = {}
        for k, v in self.mapping.items():
            bok[v] = k
        return bok # { 'code' => block }, block is tuple or string of length .__block_size

    def _lazy_get_length_(self, ig):
        """Expected output length per input token."""
        P = self._block_map
        code = self.mapping
        all = tot = 0
        for k, v in P.items():
            if v:
                all, tot = all + v * len(code[k]), tot + v

        return all * 1. / tot / self.__block_size

    from math import log
    def _lazy_get_entropy_(self, ig, ln=log):
        """Entropy per symbol.

        Uses the number of distinct output symbols as base for logarithms; this
        should ensure that length >= entropy; if the difference is large, it may
        be worth increasing the block size.

        In case the weights of our distribution don't add up to exactly 1, we
        adjust: we're computing sum(: log(k/p).p/k :) for some k we don't know
        until the end; this is sum(: log(k/p).p/k :) = sum(: log(1/p).p :)/k +
        log(k).sum(p)/k, with k = sum(p). """

        all = tot = 0 # sum, k
        for v in self.__distribution.values():
            all, tot = all - v * ln(v), tot + v

        return (all / tot + ln(tot)) / ln(len(self.__symbols))

    del log

    # Computing an optimal encoding (Huffman's algorithm):
    class Leaf:
        def __init__(self, key, weight):
            self.key, self.weight = key, weight

        def mark(self, stem, bok, sym):
            assert not bok.has_key(self.key), (self, bok)
            bok[self.key] = stem

    class Tree:
        def __init__(self, kids):
            self.kids = tuple(kids)
            self.weight = sum(map(lambda x: x.weight, kids))

        def mark(self, stem, bok, sym):
            i = len(self.kids)
            assert i <= len(sym)
            while i > 0:
                i = i - 1
                self.kids[i].mark(stem + sym[i], bok, sym)

    from study.snake.sequence import Ordered
    def _lazy_get_mapping_(self, ig, Leaf=Leaf, Tree=Tree, List=Ordered):
        """.mapping maps (blocks of) tokens to encoded string fragments.

        Computing this is the heart of the Huffman encoding.\n"""
        P, sym = self._block_map, self.__symbols
        forest = List(attr='weight')
        for k, v in P.items():
            if v: forest.append(Leaf(k, v))

        while len(forest) > 1:
            forest[:len(sym)] = [ Tree(forest[:len(sym)]) ]

        code = {}
        if forest:
            forest[0].mark('', code, sym)

        return code # { block => 'code' }, block is tuple or string of length .__block_size

    del Leaf, Tree, Ordered

del Lazy
