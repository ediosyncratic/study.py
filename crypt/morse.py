"""The Morse code.

Provides two python dictionaries, encoding and decoding.  Keys of encoding are
single (upper case) letters, as are values of decoding.  Values of encoding and
keys of decoding are strings representing the morse code for the corresponding
letter: dot is encoded as '.', dash as '-'.

Provides two functions, encode and decode, which perform the implied
conversions. """

_rcs_id = "$Id: morse.py,v 1.1 2005-10-08 16:01:01 eddy Exp $"

encoding = {
    'A': '.-',
    'B': '-...',
    'C': '-.-.',
    'D': '-..',
    'E': '.',
    'F': '..-.',
    'G': '--.',
    'H': '....',
    'I': '..',
    'J': '.---',
    'K': '-.-',
    'L': '.-..',
    'M': '--',
    'N': '-.',
    'O': '---',
    'P': '.--.',
    'Q': '--.-',
    'R': '.-.',
    'S': '...',
    'T': '-',
    'U': '..-',
    'V': '...-',
    'W': '.--',
    'X': '-..-',
    'Y': '-.--',
    'Z': '--..',
    # Seen in use on the uncyclopedia:
    "'": '.----.' }
decoding = {}
for key, val in encoding.items(): decoding[val] = key

def encode(text):
    # should really pre-process {'.': 'stop', ',': 'comma', '-': 'dash', ...}
    return ' '.join(map(lambda x, g=encoding.get: g(x, ' '), text.upper()))

def decode(message):
    ans = ''.join(map(lambda x, g=decoding.get: g(x, ' '), message.split(' ')))
    return ' '.join(ans.split()) # tidy up spacing


_rcs_log = """
 $Log: morse.py,v $
 Revision 1.1  2005-10-08 16:01:01  eddy
 Initial revision

"""
