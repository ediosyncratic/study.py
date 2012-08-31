"""Encoding and decoding tests in various ways.

Fragments available:
 base -- representing numbers as strings in various bases
 characters -- counting letter (and other token) frequencies
 dequote -- undoing the 'quoted-printable' encoding used in e-mails
 Huffman -- text-compressor using the Huffman coding
 morse -- using dots and dashes to represent text
 radix050 -- an archaic base-fourty text encoding
 salty -- access to system crypt()

Attribute:
 english -- letter frequencies in English text

See also:
 urllib -- standard python module, provides {un,}quote{,_plus} for %xx {en,de}coding.
See study.LICENSE for copyright and license information.
"""

# Taken from Information Theory, Inference, and Learning Algorithms (D.J.C. MacKay) p.100:
english = {
    ' ': 0.1928, 'e': 0.0913, 't': 0.0706, 'o': 0.0689,
    'i': 0.0599, 'n': 0.0596, 'a': 0.0575, 's': 0.0567, 'r': 0.0508,
    'l': 0.0335, 'u': 0.0334, 'h': 0.0313,
    'd': 0.0285, 'c': 0.0263, 'm': 0.0235,
    'p': 0.0192, 'f': 0.0173, 'y': 0.0164, 'g': 0.0133, 'b': 0.0128, 'w': 0.0119,
    'k': 0.0084, 'x': 0.0073, 'v': 0.0069,
    'q': 0.0008, 'z': 0.0007, 'j': 0.0006 }
