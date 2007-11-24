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
natural size for a standard chess board.  This module provides a plain Iterator
over all solutions of a given size and a Unique iterator which skips equivalent
solutions: two solutions are deemed equivalent if some symmetry of the chess
board maps one onto another.

$Id: queens.py,v 1.6 2007-11-24 17:45:19 eddy Exp $
"""

import permute
class Solution(permute.Permutation):
    def __repr__(self):
        try: ans = self.__repr
        except AttributeError:
            ans = self.__repr = '\n'.join(map(
                lambda i, n=len(self)-1: ' ' * i + '#' + ' ' * (n-i), self))
        return ans

    def peaceful(self):
        i = len(self) - 1
        while i:
            j, n = i, self[i]
            while j:
                j -= 1
                if self[j] - n in (j - i, i - j): # same diagonal
                    return False
            i -= 1
        return True

def Iterator(size=8):
    """Iterates over all solutions to the `n queens' problem.

    They are explored in lexical order.  Value yielded at each step is a
    Solution object - this is a Permutation with a custom repr() as a picture
    (and a method, peaceful, which shall return True).\n"""

    for it in permute.Iterator(size, Solution):
        if it.peaceful(): yield it

    raise StopIteration

def Unique(size=8):
    """Like Iterator, q.v., but skips essentially equivalent solutions."""
    def entwist(seq, r, n):
        seq.append(r)
	r = map(lambda i: n-i, r) # top-bottom reflection
	if r not in seq: seq.append(r[:]) # copy before reversing !
	r.reverse() # left-right reflection
	if r not in seq:
	    seq.append(r)
	    r = map(lambda i: n-i, r) # top-bottom reflection
	    if r not in seq: seq.append(r)
            # r.reverse() would restore its initial value

    def equivalents(r, n, add=entwist):
        row = []
        add(row, r, n)
        r = r.inverse # diagonal reflection
        if r not in row:
            add(row, r, n)
        return tuple(row)

    a = ()
    for r in Iterator(size):
	if r not in a:
	    yield r
	    a = a + equivalents(r, size)

    raise StopIteration
