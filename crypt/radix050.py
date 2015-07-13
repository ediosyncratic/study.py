"""Code to perform `radix 050' encoding and decoding.

This encoding recognises fourty characters: space, a through z, dollar,
full-stop, question-mark, 0 through 9.  These are numbered 0 to 39 in the order
given and a string is then read as a number in base fourty (aka octal 050).
Letters are handled case-insensitively.  This provides for strings of up to six
characters (and a few seven-character ones) to be represented as 32-bit integers
(strings earlier than t84qkg don't use the top bit).

This module exports two functions:
 encode(string) -- yields a number.
 decode(number) -- yields a string.

Bill (at LSL) found that this is the wrong way to approach the decoding,
though: it is actually done by cutting the int, seen as a sequence of bits,
into chunks, then decoding each chunk separately.  So I should probably at
least support that reading !

See study.LICENSE for copyright and license information.
"""

_charset = " abcdefghijklmnopqrstuvwxyz$.?0123456789"
from sys import maxint
from study.maths.natural import sqrt
rootmaxint = int(sqrt(2*(maxint+1)))
del sqrt, maxint
# Assert: any negative (non-long) int x satisfies -x / rootmaxint < rootmaxint / 2

def decode(number, wordsize=None, halfword=rootmaxint):
    """Decodes a string from `radix 050' form.

    Required argument, number, should be a positive integer (however, negative
    integers down to -maxint will be suitably interpreted as signed readings
    of unsigned ints: 2*(1+maxint) will be added).  This number is expressed
    in base 40 (aka octal 050) and thereby read as a string.

    Optional argument wordsize is the number of bits in the wordsize that's
    actually been used to store the number: only relevant if the number was
    read in as a signed int and thus may turn out to be negative.  If supplied
    (and not None), (1<<(wordsize-1))-1 is used in place of maxint in the
    work-around for negative integers mentioned above.

    Letters are emitted lower-case.\n"""

    if wordsize is not None: halfword = 1 << (wordsize / 2)

    if number < 0:
        # coerce signed int to unsigned ...
        if -number / halfword < halfword / 2: number += halfword**2
        else: raise ValueError('Huge negative: not a radix050 "string"', number, halfword)

    row = []
    while number:
        number, here = divmod(number, 40)
        row.append(_charset[here])

    return ''.join(row)
del rootmaxint

def encode(text):
    """Encodes a string in `radix 050' form.

    Argument, text, is a string.  It should only use the characters known to
    the radix 050 encoding (alphanumeric, space, $, . and ?) - all others
    will be treated as spaces (but I reserve the right to treat them as some
    other character instead - probably `?').  The string is read as a radix
    050 (i.e. fourty) number using these characters as digits (with space as
    0, letters as 1 through 26, $, . and ? as 27, 28 and 29, each digit as
    its value plus 30).  Note that leading spaces are ignored (just like
    leading zeros on a number).\n"""

    number = 0
    import string
    for ch in text:
        try: here = string.index(_charset, string.lower(ch))
        except ValueError: here = 0 # perhaps use ? (29) in place of ch.
        number = number * 40 + here

    return number
