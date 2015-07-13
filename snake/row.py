"""Miscellaneous tools to help with sequences.

  transpose -- transpose(x)[i][j] is x[j][i], when x is a sequence of sequences
  unique(seq) -- copy sequence, skipping duplicates
  deltas(seq [, step]) -- list of differences between entries in sequence
  median(seq) -- find entry, with as many >= it as <= it, in seq (or a mapping's keys)

See study.LICENSE for copyright and license information.
"""

def transpose(rows):
    """Transposes a sequence of sequences.

    Only argument is a sequence of sequences.  Result is a list of tuples.  Each
    inner sequence of the input is effectively padded with None entries to the
    length of the longest, so that we may read the input as a `rectangular'
    array: output[i][j] is input[j][i] for all relevant i, j.  (Contrast with
    the built-in zip(), which truncates to the shortest input instead of padding
    to the longest.)

    The first inner sequence of the input provides the first members of the
    tuples in the result list; the first tuple of the result list consists of
    the first members of the inner sequences in the input.  For `first', read
    nth(n) for any n, provided you do so throughout.

    Transposing a tuple of lists will yield a list of tuples preserving the
    `position in tuple' and `position in list' of each entry. """

    # (Even in python some useful one-liners require more than a moment's thought.-)
    return map(lambda *args: args, *rows)
# I can understand why a language designer might be shy of built-in functional tools.

def unique(seq):
    """Returns seq with repetitions eliminated.

    Result is of the same type as seq - seq.__class__ must be callable,
    accepting a list as input - and has the same entries, with the same order of
    first appearance; however, duplicates are eliminated.\n"""
    bok, row = set(), []
    for it in seq:
        if it not in bok:
            bok.add(it)
            row.append(it)
    return seq.__class__(row)

def deltas(seq, step=1):
    """Return list of differences within a sequence.

    Required argument, seq, is a sequence of values between which subtraction is
    feasible.  Optional argument, step, defaults to 1.  Returns the list
    (: seq[i+step] - seq[i] &larr; i :).\n"""
    return [y - x for x, y in zip(seq[:-step], seq[step:])]

def median(seq):
    """Find an entry of seq with as many >= it as <= it.

    Returns the formal median of the sequence; if the sequence were sorted, this
    would be the one in the middle.  For a sequence of even length, when the two
    in the middle aren't equal, their mean is used.\n"""
    row = list(seq)
    row.sort()
    n, i = divmod(len(row), 2)
    if i: return row[n]
    i, j = row[n], row[n-1]
    if i == j: return i
    return 0.5 * (i+j)
