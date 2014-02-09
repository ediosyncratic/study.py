"""Solving the `eight queens' problem.

Given are an 8 X 8 chessboard and 8 queens which are hostile to each other.
Find a position for each queen (a configuration) such that no queen may be taken
by any other queen (i.e. such that every row, column, and diagonal contains at
most one queen).

Consequently, each row contains exactly one queen, as does each column; so the
mapping from rows to columns is a permutation.  There are twelve essentially
distinct solutions (once one takes accounts of the symmetries of a chess board,
allowing black and white squares to swap), one of which is symmetric under a
half turn of the board: [2, 4, 1, 7, 0, 6, 3, 5].

The problem naturally generalizes to other sizes than 8, although 8 is the
natural size for a standard chess board.  This module provides a class Solution,
which extends permute.Permutation; adding a .solve(size) iterator to iterate
over all solutions of the given size; and adding a .unique(size) to do the same
but skip equivalent solutions: two solutions are deemed equivalent if some
symmetry of the chess board maps one onto another.

See study.LICENSE for copyright and license information.
"""

from permute import Permutation
class Solution (Permutation):
    def __repr__(self):
        try: ans = self.__repr
        except AttributeError:
            ans = self.__repr = '\n'.join(map(
                lambda i, n=len(self)-1: ' ' * i + '#' + ' ' * (n-i), self))
        return ans

    @staticmethod
    def __peaceful(seq):
        i = len(seq) - 1
        while i > 0:
            j, n = i, seq[i]
            while j:
                j -= 1
                if seq[j] - n in (j - i, i - j): # same diagonal
                    return False
            i -= 1
        return True

    @classmethod
    def solve(cls, size):
        """Iterates over all solutions to the `n queens' problem.

        They are explored in lexical order.  Value yielded at each step is a
        Solution object - this is a Permutation with a custom repr() as a
        picture.\n"""
        return cls.all(size).filter(cls.__peaceful)

    @classmethod
    def unique(cls, size):
        """Like all, q.v., but skips essentially equivalent solutions."""
        a = ()
        for r in cls.solve(size):
            if r not in a:
                yield r
                a = a + cls.__equivalents(r, size)

    def entwist(seq, r, n): # tool used by __equivalents
        seq.append(r)
        r = map(lambda i: n-i, r) # top-bottom reflection
        if r not in seq: seq.append(r[:]) # copy before reversing !
        r.reverse() # left-right reflection
        if r not in seq:
            seq.append(r)
            r = map(lambda i: n-i, r) # top-bottom reflection
            if r not in seq: seq.append(r)
            # r.reverse() would restore its initial value

    @staticmethod
    def __equivalents(r, n, add=entwist):
        row = []
        add(row, r, n)
        r = r.inverse # diagonal reflection
        if r not in row:
            add(row, r, n)
        return tuple(row)

    del entwist

del Permutation
# backwards compatibility:
def Iterator(size=8): return Solution.solve(size)
def Unique(size=8): return Solution.unique(size)
