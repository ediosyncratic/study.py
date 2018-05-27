"""Shortest-first lexical iterators.

See study.LICENSE for copyright and license information.
"""

def lexicon(symbols=''.join(chr(n) for n in [9, 10] + range(32, 127)), follow=''):
    """Iterate strings in length-first lexical order.

    Optional parameters:
      symbols -- the sequence of symbols to use (default: see below)
      follow -- string to start after (default: '')

    The default value for symbols is a string of the characters with
    ASCII codes 9 (horizontal tab), 10 (new line) and from 32 (space)
    through 126 (tilde), in ASCII code order.  Any sequence type will
    do for symbols; its entries must be of a type that can be added to
    slices of follow, which is how the yields of this iterator shall
    be constructed.

    Shorter strings come before longer; strings of equal length are
    ordered by the first character in which they differ; the the one
    with the earlier entry in symbols at that position is before the
    other.

    If an entry in follow is not in symbols, the iteration shall vary
    any later entries until everything after the alien is the last
    entry in symbols; it'll then replace the alien (which shall never
    be seen again) and each of these later entries with the first
    entry in symbols, as if the alien sorted earlier than all entries
    in symbols.

    For example, lexicon('01', '101') starts with '110', '111',
    '0000', '0001', '0010', '0011', '0100', ...

    Note that this iterator calls itself recursively, to a depth equal
    to the length of the strings it yields; iterating it until it
    yields a serialisation of the complete works of Shakespeare may
    exhaust your computer's resources.\n"""
    seq = iter(symbols)
    if follow:
        stem, end = follow[:-1], follow[-1]
        for ch in seq:
            if ch == end:
                break
        else: # end is an alien
            seq = iter(symbols)
    else:
        stem = follow[:]

    for ch in seq:
        yield stem + ch

    for stem in lexicon(symbols, stem):
        for ch in symbols:
            yield stem + ch
