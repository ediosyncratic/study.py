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
natural size for a standard chess board.

$Id: queens.py,v 1.4 2007-11-24 15:25:40 eddy Exp $
"""

import permute
class Permutation(permute.Permutation):
    def __repr__(self):
        try: ans = self.__repr
        except AttributeError:
            ans = self.__repr = '\n'.join(map(
                lambda i, n=len(self)-1: ' ' * i + '#' + ' ' * (n-i), self))
        return ans

    def queenly(self):
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
    Permutation object but with a custom repr() as a picture (and a method,
    queenly, which shall return True).\n"""

    for it in permute.Iterator(size, Permutation):
        if it.queenly(): yield it

    raise StopIteration

def unique(size=8):
    def entwist(r, seq, n):
	s = map(lambda i: n-i, r)
	if s not in seq: seq.append(s[:]) # copy before reversing !
	s.reverse()
	if s not in seq:
	    seq.append(s)
	    s = map(lambda i: n-i, s)
	    if s not in seq: seq.append(s)

    u, a = [], []
    for r in map(list, Iterator(size)):
	if r not in a:
	    u.append(tuple(r))
	    new = [ r ]
	    entwist(r, new, size)

	    r = list(permute.invert(r))
	    if r not in new:
		new.append(r)
		entwist(r, new, size)

	    a = a + new

    return u
