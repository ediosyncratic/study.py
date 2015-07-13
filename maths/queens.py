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
            ans = self.__repr = '\n'.join([''] + [
                    ' ' * i + '#' + ' ' * (len(self) - 1 - i) for i in self ])
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
        """Like all, q.v., but skips essentially equivalent solutions.

        Reflecting or rotating the board doesn't give an interestingly different
        solution.\n"""
        old = set()
        for r in cls.solve(size):
            if r not in old:
                yield r
                cls.__add_equivalents(old, r, size)

    def rectangle(seen, r, flip): # tool used by __add_equivalents
        """Record horizontal and vertical reflections, plus half-turn.

        These are the symmetries of a rectangle.  Arguments are the set to which
        to add equivalents of r, the permutation r and a function, flip, that
        does top-bottom reflection in the same sense that reversed() does
        left-right reflection.  Note that neither of these operations can give
        the permutation we started with (aside from the () and (0,) trivial
        cases), although perhaps their combination (half turn) could.\n"""
        seen.add(r)
        m = flip(r) # top-bottom reflection
        assert m in ((), (0,)) or m not in seen
        seen.add(m)
        m = reversed(m) # left-right reflection
        if m != r: # half-turn might have taken us back to r
            assert m not in seen
            seen.add(m)
            seen.add(flip(m)) # top-bottom reflection
            # The last's reversed() is the r we started with

    @staticmethod
    def __add_equivalents(seen, r, n, add=rectangle):
        """Grind through the equivalents of r, adding them to seen.

        This function looks after the diagonal reflection that, with the other
        symmetries of a rectangle, generates the full set.\n"""
        assert r not in seen
        def flip(seq, m=n-1): return seq.map(lambda i, k=m: k - i)
        add(seen, r, flip)
        if n > 1: # trivial otherwise
            i = r.inverse # diagonal reflection
            if i not in seen: add(seen, i, flip)

    del rectangle

del Permutation
# backwards compatibility:
def Iterator(size=8): return Solution.solve(size)
def Unique(size=8): return Solution.unique(size)
