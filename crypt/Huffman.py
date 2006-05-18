"""Implementing Huffman coding.
"""

_rcs_id = "$Id: Huffman.py,v 1.1 2006-05-18 10:13:16 eddy Exp $"
from basEddy import Lazy

class Huffman (Lazy):
    def __init__(self, P, N=1, blank=None):
        """Initialize a Huffman encoder.

        First argument, P, is a mapping from symbols to their frequencies; if a
        sequence is supplied instead, it is interpreted as a mapping from the
        strings naming its indices to its entries.  (FIXME: broken for sequence
        longer than two-ple; need to support string messages but also sequences
        of tokens from P's vocabulary).  Present implementation presumes that
        P's keys are all strings.

        Optional second argument is the number of such symbols to be handled in
        each block: only messages which are a multiple of this block size can be
        encoded by the resulting Huffman encoder.  Optional third argument,
        blank, is a key (or index) of P with which a message may be padded to
        make up its length to a multiple of the block size."""

        if blank is not None: self.__blank = blank
        if N != int(N) or N < 1:
            raise ValueError(N, 'Block size should be a positive integer')
        self.__block_size = N
        self.__distribution = P
        try: self.__symbols = P.keys()
        except AttributeError:
            try: P[:]
            except: raise ValueError(P,
                                     'Probability distribution must be mapping or sequence')
            else:
                self.__symbols = map(str, range(len(P)))
                for i in self.__symbols:
                    P[i] # raises exception if P not a sequence ...

    def encode(self, message):
        message = self.__pad(message) # raises error if trouble
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
        return message

    def __pad(self, message):
        rem = len(message) %  self.__block_size
        if rem:
            try: pad = self.__blank
            except AttributeError:
                raise ValueError(self.__block_size,
                         'Cannot pad message to multiple of block size: no blank specified')
            if pad not in self.__symbols:
                del self.__blank
                raise ValueError(pad,
                                 'Invalid blank character specified: not in symbol set')
            message = message + (self.__block_size - rem) * pad

        return message

    def _lazy_get__block_map_(self, ig):
        """Mapping from possible blocks to their probabilities. """
        bok = { '': 1 }
        N = self.__block_size
        while N > 0:
            N = N - 1
            old = filter(lambda (k, v): v, bok.items())

            bok = {}
            for s, q in self.__distribution.items():
                if q:
                    for k, v in old:
                        bok[k + s] = q * v

        return bok

    class Leaf:
        def __init__(self, key, weight):
            self.key, self.weight = key, weight

        def mark(self, stem, bok):
            bok[self.key] = stem

    class Tree:
        def __init__(self, a, b):
            self.kids = (a, b)
            self.weight = a.weight + b.weight

        def mark(self, stem, bok):
            # This Huffman only emits binary outputs - TODO: generalize
            self.kids[0].mark(stem + '0', bok)
            self.kids[1].mark(stem + '1', bok)

    def weigh(x, y):
        gap = x.weight - y.weight
        if gap < 0: return -1
        elif gap > 0: return 1
        return 0

    def _lazy_get_mapping_(self, ig, Leaf=Leaf, Tree=Tree, weigh = weigh):
        P = self._block_map
        forest = []
        for k, v in P.items():
            if v: forest.append(Leaf(k, v))

        while len(forest) > 1:
            forest.sort(weigh)
            forest = forest[2:] + [ Tree(forest[0], forest[1]) ]

        code = {}
        if forest:
            forest[0].mark('', code)

        return code

    del Leaf, Tree, weigh

    def _lazy_get_reverse_(self, ig):
        bok = {}
        for k, v in self.mapping.items():
            bok[v] = k
        return bok

    def _lazy_get_length_(self, ig):
        """Expected length per token."""
        P = self._block_map
        code = self.mapping
        all = tot = 0
        for k, v in P.items():
            all, tot = all + v * len(code[k]), tot + v

        return all * 1. / tot / self.__block_size

    from math import log
    def _lazy_get_entropy_(self, ig, ln=log, ln2 = log(2)):
        """Entropy per symbol.

        In case the weights of our distribution don't add up to exactly 1, we
        adjust: we're computing sum(: log(k/p).p/k :) for some k we don't know
        until the end; this is sum(: log(k/p).p/k :) = sum(: log(1/p).p :)/k +
        log(k).sum(p)/k, with k = sum(p)
        """
        all = tot = 0 # sum, k
        for v in self.__distribution.values():
            all, tot = all - v * ln(v), tot + v

        return (all / tot + ln(tot)) / ln2

    del log

_rcs_log = """
 $Log: Huffman.py,v $
 Revision 1.1  2006-05-18 10:13:16  eddy
 Initial revision

"""
