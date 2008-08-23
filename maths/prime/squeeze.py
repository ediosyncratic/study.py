"""Compressing data on primes and factors.

I should be able to keep cache files smaller by using a compressed format
for the lists of primes and of factors.

The information on primes is already in a fairly-well compressed format as a
string, exploiting the generalized octet formalism; we can save the bz2.compress
of this if shorter, for example.  However, bz2.compress gives strings in which
many entries are non-printable, so they take up four bytes in the repr that
we'll actually save to file, which defeats the purpose of compressing in the
first place; the initial uncompressed string suffers the same deficiency.
However, we can use base 64 encoding to restrict the characters used to the
limited repertoire of characters that contribute only one byte to the repr where
they appear in a string.

  There are 94 characters which contribute only one character to the repr of a
  string in which they appear.  In practice, I'd want to ditch the single quote
  and space from this list.  However, using 94 characters, or a few less, would
  require either a custom compression algorithm (targeting this repertoire) or a
  very much harder conversion (from base 256 to base 94 or so) to encode the
  compressed string as printable bytes.  It would give us smaller files, but
  only by the factor log(64)/log(94) = 0.915; i.e. it'd save us less than 9%
  relative to to base64, at the expense of significantly more effort.

The information on factors, on the other hand, is a sequence of prime indices,
each at least the number, n, of primes in our octet-type.  We can say very
definite things about how much of the time each index occurs, so a custom
compression algorithm should be feasible.  For natural i > n, only a fraction
product(: (p-1)/p &larr;p |primes[n:i]) of the indices are at least i; and
1/primes[i] of these are i.  This is the data we'd need to construct a Huffman
code; except that the symbol set is infinite, which complicates matters !  (For
the n = 7 case, using primes up to 17 for the octet, the sum of proportions for
primes[7:92] is just over 0.5.)

For any given file recording factors of a range of naturals, however, the set of
relevant primes is finite; it comprises only those primes whose squares are at
most the highest natural in the range.  Our symbol set is thus limited to
primes[n:m] for some m with primes[m]**2 greater than the end of our range.
However, this would require working out a fresh Huffman coding for each file of
factor information.  That's a rather tiresome business, so instead we could
chose some specific sequence of values of m for which we work out a Huffman
coding, then use the one for the least selected m whose primes[m]**2 exceeds the
end of our range.

Since this involves building our own compression algorithm, we can chose our own
symbol set (rather than the usual two-symbol set); as remarked above, we have 92
or so symbols that contribute only one character to an output string, so we'll
want to restrict to these.  Using this character set, I find that the common
factor list from the naturals, coprime to the primes up to 17, up to the product
of the primes up to 17, allow compression by a factor of about 3, using the
compressor derived from primes[7:7+92*92]; this is the compressor I'd end up
using for the first 14912 generalized octets, covering the primes up to
7612725120, a little over 7 * 2**30.\n"""

from bz2 import compress, decompress

def expand(txt, d=decompress): return d(txt)
def squash(txt, c=compress):
    ans = enc(c(txt))
    if len(repr(ans)) < len(repr(txt)): return ans
    raise ValueError, "I'm sorry Dave, I can't do that"

del compress, decompress

from study.crypt.Huffman import Huffman, alphabet
import re

class Huff (Huffman):
    __upinit, __base = Huffman.__init__, len(alphabet)
    def __init__(self, primes, skip, bound):
        """Initialize a Huffman encoder for least proper factor blocks.

        Requires three arguments:
          primes -- the list of all primes
          skip -- the primes accounted for by your octet type
          bound -- the largest natural in the block to be encoded

        Computes a Huffman encoding for sequences, whose entries are either None
        (denoting primes) or least proper factors (which are always prime),
        describing ranges of naturals up to at least the given bound.  In
        practice, the encoding used always uses all the primes less than or
        equal to the first non-skipped prime (usually 19) times some power of
        the length of the alphabet used for the encoding (this is 92, at
        present).\n"""

        bok, rest, N, stop = {}, 1., 0, None
        for p in primes:
            if p in skip: continue
            elif stop is None:
                stop, base = p, self.__base
                while stop * stop < bound:
                    stop *= base
                    N += 1
            elif p > stop: break

            bok[p] = rest / p
            rest *= 1 - 1. / p

        bok[None] = rest
        self.__upinit(bok)
        self.order = N

    assert '\n' not in alphabet
    __enc = Huffman.encode
    __dec = Huffman.decode
    # Add and ignore newlines in ciphertext to make its lines <= 80 long:

    def encode(self, message, chop=re.compile('.{,80}').findall):
        return '\n'.join(chop(self.__enc(message)))

    def decode(self, text):
        return self.__dec(''.join(text.split('\n')))

del Huffman, alphabet, re
