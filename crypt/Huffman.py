"""Implementing Huffman coding.
"""

_rcs_id = "$Id: Huffman.py,v 1.3 2006-05-18 11:13:20 eddy Exp $"
from basEddy import Lazy

class Huffman (Lazy):
    def __init__(self, P, N=1, blank=None, symbols='01'):
        """Initialize a Huffman encoder.

        First argument, P, is a mapping from symbols to their relative
        frequencies; if a sequence is supplied instead, it is interpreted as a
        mapping from the strings naming its indices to its entries.  (FIXME:
        broken for sequence longer than two-ple; need to support string messages
        but also sequences of tokens from P's vocabulary).  In principle, P is
        reduced to a probability distribution: it'll be normalized if
        appropriate.  Present implementation presumes that P's keys are all
        strings.

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
        """

        if N != int(N) or N < 1:
            raise ValueError(N, 'Block size should be a positive integer')
        self.__block_size = N

        if len(symbols) < 2:
            raise ValueError(symbols, 'Need at least two output symbols')
        self.__symbols = symbols

        # Check P is a mapping or sequence:
        try: keys = P.keys()
        except AttributeError:
            try: P[:]
            except: raise ValueError(P,
                                     'Probability distribution must be mapping or sequence')
            else:
                keys = map(str, range(len(P)))
                for i in keys:
                    P[i] # raises exception if P is not a sequence ...

        self.__distribution = P
        if blank is not None:
            if blank not in keys:
                raise ValueError(blank,
                                 'Invalid blank character specified: not in symbol set')
            self.__blank = blank

    def encode(self, message):
        message = self.__pad(message) # ValueError if trouble
        txt, n, code = '', self.__block_size, self.mapping
        while message:
            message, txt = message[n:], txt + code[message[:n]]
        return txt

    def decode(self, txt):
        message, code = '', self.reverse
        while txt:
            i = 1
            while not code.has_key(txt[:i]) and txt[i:]: i = 1 + i
            txt, message = txt[i:], message + code[txt[:i]] # KeyError if bad txt

        try: pad, n = self.__blank, self.__block_size - 1
        except AttributeError: pass
        else: message = message[:-n] + message[-n:].rstrip(pad)

        return message

    def __pad(self, message):
        rem = len(message) %  self.__block_size
        if rem:
            try: pad = self.__blank
            except AttributeError:
                raise ValueError(self.__block_size,
                                 'Cannot pad message to multiple of block size: no blank')
            message = message + (self.__block_size - rem) * pad

        return message

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

    def weigh(x, y):
        gap = x.weight - y.weight
        if gap < 0: return -1
        elif gap > 0: return 1
        return 0

    def _lazy_get_mapping_(self, ig, Leaf=Leaf, Tree=Tree, weigh=weigh):
        P, sym = self._block_map, self.__symbols
        forest = []
        for k, v in P.items():
            if v: forest.append(Leaf(k, v))

        while len(forest) > 1:
            forest.sort(weigh)
            forest = [ apply(Tree, forest[:len(sym)]) ] + forest[len(sym):]

        code = {}
        if forest:
            forest[0].mark('', code, sym)

        return code

    del Leaf, Tree, weigh

_rcs_log = """
 $Log: Huffman.py,v $
 Revision 1.3  2006-05-18 11:13:20  eddy
 Support for more than two output symbols.  Re-purpose .__symbols name.
 Check validity of blank, if supplied, in constructor.
 Strip dangling padding on decode.

 Revision 1.2  2006/05/18 10:20:03  eddy
 Shuffle parts into tidier order.

 Initial Revision 1.1  2006/05/18 10:13:16  eddy
"""
