"""Implementing Huffman coding.

$Id: Huffman.py,v 1.10 2008-06-13 07:37:51 eddy Exp $
"""
from study.snake.lazy import Lazy

class Huffman (Lazy):
    def __init__(self, P, N=1, blank=None, symbols='01', tail=None):
        """Initialize a Huffman encoder.

        First argument, P, is a mapping from symbols to their relative
        frequencies (which must not be negative); if a (not too long) sequence
        is supplied instead, it is interpreted as a mapping from its indices
        (each represented as a digit, letter or punctuator) to its entries.
        (TODO: support non-string keys; encode sequences of keys).  In
        principle, P is normalized to yield a probability distribution, but this
        is not actually necessary.

        Optional arguments:

          N -- the number of such symbols to be handled in each block: only
               messages which are a multiple of this block size can be encoded
               by the resulting Huffman encoder (default: 1).

          blank -- None (the default), to enforce the message length limit
                   detailed above for N; or a key (or index) of P with which a
                   message may be padded to make up its length to a multiple of
                   the block size; in the latter case, the decoder shall always
                   strip up to N-1 of these blanks from each message.

          symbols -- the alphabet to use for the encoded message (default: '01').

          tail -- entry in symbols which is apt to be lost from the end of an
                  encoded text (e.g. because it's the blank of a down-stream
                  Huffman); when decoding, enough of these shall be appended, if
                  needed.  Default, None, means to omit such bodging.
        """

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
            # Concoct representation for P's indices:
            import string
            keys = string.digits
            for more in ( string.lowercase, string.uppercase, string.punctuation, ' ' ):
                if len(P) > len(keys):
                    keys = keys + more
                else: break

            if len(P) > len(keys):
                raise ValueError(P, 'Sequence too long for fall-back handling', keys)

            # Convert P to a mapping:
            bok = {}
            for i in range(len(P)):
                bok[keys[i]] = P[i] # raises exception if P is not a sequence ...

            self.__distribution = bok
        else: self.__distribution = P

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
        rem = len(message) %  self.__block_size
        if rem:
            try: pad = self.__blank
            except AttributeError:
                raise ValueError(self.__block_size,
                                 'Cannot pad message to multiple of block size: no blank')
            message = message + (self.__block_size - rem) * pad

        txt, n, code = '', self.__block_size, self.mapping
        try:
            while message:
                message, txt = message[n:], txt + code[message[:n]]
        except KeyError:
            raise ValueError(message,
                             'Unable to encode this message',
                             code)
        return txt

    def decode(self, txt):
        message, code = '', self.reverse
        try:
            while txt:
                i = 1
                while not code.has_key(txt[:i]) and txt[i:]: i = 1 + i
                if not code.has_key(txt[:i]) and not txt[i:]:
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
        try: ans = self.__repr
        except AttributeError:
            its = self.mapping.items()
            its.sort(lambda (k,v), (h,u): cmp(u, v) or cmp(h, k))
            fmt = map(lambda (k, v): '%%%ds' % max(len(k), len(v)), its)
            ans = '\n'.join(map(lambda x, f=' | '.join(fmt): f % x,
                                map(* [ lambda *args: args ] + its)))
            self.__repr = ans

        return ans

    def _lazy_get__block_map_(self, ig, possible=lambda (k, v): v):
        """Mapping from possible blocks to their probabilities. """
        bok = { '': 1 }
        N, dist = self.__block_size, filter(possible, self.__distribution.items())
        while N > 0:
            N = N - 1
            old = filter(possible, bok.items())

            bok = {}
            for s, q in dist:
                for k, v in old:
                    bok[k + s] = q * v

        return bok

    def _lazy_get_reverse_(self, ig):
        bok = {}
        for k, v in self.mapping.items():
            bok[v] = k
        return bok

    def _lazy_get_length_(self, ig):
        """Expected output length per input token."""
        P = self._block_map
        code = self.mapping
        all = tot = 0
        for k, v in P.items():
            all, tot = all + v * len(code[k]), tot + v

        return all * 1. / tot / self.__block_size

    from math import log
    def _lazy_get_entropy_(self, ig, ln=log):
        """Entropy per symbol.

        Uses the number of distinct output symbols as base for logarithms; this
        should ensure that length >= entropy.

        In case the weights of our distribution don't add up to exactly 1, we
        adjust: we're computing sum(: log(k/p).p/k :) for some k we don't know
        until the end; this is sum(: log(k/p).p/k :) = sum(: log(1/p).p :)/k +
        log(k).sum(p)/k, with k = sum(p). """

        all = tot = 0 # sum, k
        for v in self.__distribution.values():
            all, tot = all - v * ln(v), tot + v

        return (all / tot + ln(tot)) / ln(len(self.__symbols))

    del log

    # Computing an optimal encoding:
    class Leaf:
        def __init__(self, key, weight):
            self.key, self.weight = key, weight

        def mark(self, stem, bok, sym):
            bok[self.key] = stem

    class Tree:
        def __init__(self, *kids):
            self.kids = kids
            self.weight = sum(map(lambda x: x.weight, kids))

        def mark(self, stem, bok, sym):
            i = min(len(sym), len(self.kids))
            while i > 0:
                i = i - 1
                self.kids[i].mark(stem + sym[i], bok, sym)

    from study.snake.sequence import Ordered
    def _lazy_get_mapping_(self, ig, Leaf=Leaf, Tree=Tree, List=Ordered):
        P, sym = self._block_map, self.__symbols
        forest = List(key='weight')
        for k, v in P.items():
            if v: forest.append(Leaf(k, v))

        while len(forest) > 1:
            forest[:len(sym)] = [ Tree(* forest[:len(sym)]) ]

        code = {}
        if forest:
            forest[0].mark('', code, sym)

        return code

    del Leaf, Tree, Ordered
