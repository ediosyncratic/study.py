"""Playing with different number bases.

Contrast study.crypt.base64 for MIME's base-64 encoding, which builds on this.

See study.LICENSE for copyright and license information.
"""

class Base:
    """Handling of varied number base for python.

    Represents a base by a sequence of `digits' to use in that representation.
    Note that
      Base((zip(string.digits) + zip(string.lowercase, string.uppercase))[:n])
    relates strings to numbers the same way as the python int() function does
    when given second argument n; see intbase, below.\n"""

    # TODO: (subclass to) handle exponent suffix, optionally with constraints
    # such as: exponent must be a multiple of three; use case of exponent marker
    # to indicate exact value.  Aim to replace study.value.qSample's bodgery.
    # Alternate subclass: handle 'thousand separator' suitably.

    def __init__(self, digits='0123456789',
                 offset=0, signif=None,
                 plus=('', '+'), minus='-', fracsep='.',
                 ignore=',', prefix=('',), doc=None):
        """Set up a base.

        Where the following refers to a 'character' you may equally supply a
        string or other sequence of characters; for encoding, its first
        character shall be used, but decoding shall deem all others in the
        string equivalent to it (e.g. to treat letters case-insensitively).  See
        below for examples.

        Arguments are all optional: default set-up is for decimal.
          digits -- sequence of characters to be used as digits, in increasing order
          offset -- integer to be added to a digit to get its representation's
                    index in digits
          signif -- default number of significant figures to display (default is
                    (the smaller of 100 and) whatever encodes the smallest
                    representable fractional error on 1, i.e. C's DBL_EPSILON)
          plus -- character indicating positive value (default: ('', '+'))
          minus -- character indicating negative value (default: '-')
          fracsep -- marker character between the whole and fractional part of a number
          ignore -- character to be ignored in parsing (default ','); see
                    Conway13 for an exotic use.
          prefix -- sequence of strings; only a text beginning with one of these
                    shall be decoded (skipping over this prefix); the first
                    shall be used as prefix on every encoded value.
          doc -- a documentation string for the base (or None).\n"""

        self.__digits, self.__ignore = digits, ignore
        self.__plus, self.__minus, self.__fracsep = plus, minus, fracsep
        self.__base, self.__offset = len(digits), offset
        if doc is not None: self.__doc__ = doc

        if signif is None:
            # Compute minimal representable fractional error:
            signif, err, frac = 0, 1, 1. / self.__base
            while 1 + err != 1 and signif < 100: err, signif = err * frac, 1 + signif

        self.__sigfig = signif

    def __unique(self, char):
        while len(char) > 1: char = char[0]
        return char

    def __encode(self, index):
        return self.__unique(self.__digits[int(index) + self.__offset])

    # TODO: replace signif (passed to encode) with a test function which, given
    # top and bottom of an interval, says whether the digit sequence denoting
    # that interval is precise enough.
    def encode(self, number, signif=None):
        if signif is None: signif = self.__sigfig

        if number < 0:
            sign = self.__unique(self.__minus)
            number = -number
        elif number > 0: sign = self.__unique(self.__plus)
        else: sign = ''

        shunt = 0
        while True:
            whole = long(number)
            if number == whole or shunt >= signif: break
            shunt, number = 1 + shunt, self.__base * number

        if number > .5 + whole: whole = 1 + whole # round up.
        result = ''

        while shunt > 0:
            whole, digit = divmod(whole, self.__base)
            if digit + self.__offset >= self.__base:
                whole, digit = whole + 1, digit - self.__base
            result, shunt = self.__encode(digit) + result, shunt - 1

        if result: result = self.__unique(self.__fracsep) + result

        while whole:
            whole, digit = divmod(whole, self.__base)
            if digit + self.__offset >= self.__base:
                whole, digit = whole + 1, digit - self.__base
            result = self.__encode(digit) + result

        return sign + result

    def __decode(self, digit):
        ind = - self.__offset
        for it in self.__digits:
            if digit in it:
                return ind
            ind += 1

        raise ValueError, ('Bad digit', digit, self.__digits)

    def decode(self, given):
        if given and (given[0] in self.__plus or given[0] in self.__minus):
            negate, given = given[0] in self.__minus, given[1:]
        else: negate = False
        result = 0

        while given and given[0] not in self.__fracsep:
            left, given = given[0], given[1:]
            if left not in self.__ignore:
                left = self.__decode(left)
                result = result * self.__base + left

        if given: given, unit = given[1:], 1.
        while given:
            left, given = given[0], given[1:]
            if left in self.__ignore: continue # perverse
            left, unit = self.__decode(left), unit / self.__base
            result = result + left * unit

        if negate: return -result
        return result

    def intlen(self, i):
        """len(self.encode(1L << (8*i)))

        Number of characters this base needs to represent an integer encoded in
        i octets (that is, 8-bit bytes)."""
        return len(self.encode(1L << (8*i)))
    # Also want to compute good k,n,d for which k + (i * n)/d >= intlen(i) with
    # the difference growing very slowly; c.f. ratio.{refine,approximate}

binary = Base('01', doc="Binary")
signal = Base('T01', offset=1, doc="""Signed ternary.

Base three, represented using digits 0, 1 and T for zero, plus one and minus
one.  Thus 1T stands for two, 10 stands for three, 11 stands for four and five
is represented as 1TT, i.e. nine minus three minus one.

Note: 3**5 is 243, aka binary 11110011, representable in 8 bits with 13, aka
binary 1101, free tokens ...

2**27 is 134217728, 3**17 is 129140163, aka binary 111101100101000010111000011.
2**485 > 3**306, 2**1539 > 3**971, 2**2593 > 3**1636, ... 2**75235 > 3**47468,
all giving longer initial sequences of 1s for the power of 3 in binary.

All things considered, 3**5 and 2**8 are the pair to use; 8 bits is a familiar
unit, 5 trits packed in a byte should work reasonably well.
""")
octal = Base('01234567', prefix=('0',), doc="Octal")
decimal = Base(doc="Decimal")
hexadecimal = Base(( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                     'aA', 'bB', 'cC', 'dD', 'eE', 'fF' ),
                   prefix=('0x', '0X'), doc="Hexadecimal")
radix050 = Base((' ', 'aA', 'bB', 'cC', 'dD', 'eE', 'fF', 'gG', 'hH', 'iI',
                 'jJ', 'kK', 'lL', 'mM', 'nN', 'oO', 'pP', 'qQ', 'rR', 'sS',
                 'tT', 'uU', 'vV', 'wW', 'xX', 'yY', 'zZ', '$', '.', '?',
                 '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'),
                doc="""Radix 050 (forty)

This encoding has some peculiar history I've forgotten; it uses space as zero,
letters of the alphabet as digits for 1 through 26, '$.?' as the remaining
digits up to 29 and the usual decimal digits for 30 through 39.  It is actually
base forty, as a result; but 'forty' has traditionally been written in octal
when speaking of this base, as 050, with the result that it is commonly called
'base fifty'.
""")

rfc1924 = Base('0123456789' +
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
               'abcdefghijklmnopqrstuvwxyz' +
               '!#$%&()*+-;<=>?@^_`{|}~')

import string
def intbase(n,
            seq=list(string.digits) + [
        ''.join(s) for s in zip(string.lowercase, string.uppercase)]):
    """Returns the base comensurate with int(string, n)

    Python's int() built-in, when given a second argument, takes that as base,
    using the letters of the alphabet, case-insensitively, as extra digits
    beyond 9, if needed.  For n from 2 to 36, intbase(n).decode(string) is thus
    the same as int(string, n) and intbase(n).encode does the encoding to match;
    it uses lower-case where it uses letters (you can .upper() the result if you
    want upper-case).\n"""
    if n < 2 or n > 36:
        raise ValueError, 'int() base must be >= 2 and <= 36'
    return Base(seq[:n], doc="""Base %d.

    This is an instance of study.crypt.base.Base, generated by
    study.crypt.base.intbase(%d).  It uses the letters of the alphabet (without
    distinguishing lower-case from upper-case, and using lower-case when
    encoding) to encode and decode numbers in the base which, when written in
    base ten, is represented by %d.  Its .decode(x) is equivalent to python's
    built-in int(x, %d).\n""" % (n, n, n, n))
del string

def Conway13(number,
             en=Base('0123456789+-.').encode,
             de=Base(ignore='+-.').decode):
    """The Conway Base 13 function (approximately).

    Caricatures the meanest hairiest function I know how to integrate.  Encodes
    using base 13, then judiciously ignores parts of the encoding when decoding
    as base 10.  To do it properly for infinite precision you'd need to deal
    with some complications, but this is only meant as a caricature ;->\n"""

    return de(en(number))
