"""The Morse code.

Provides two python dictionaries, encoding and decoding.  Keys of encoding are
single (upper case) letters, as are values of decoding.  Values of encoding and
keys of decoding are strings representing the morse code for the corresponding
letter: dot is encoded as '.', dash as '-'.

Provides two functions, encode and decode, which perform the implied
conversions.

See study.LICENSE for copyright and license information.
"""

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

def decipher(message):
    # like decode, but when there are no spaces.
    row = [ ( '', message ) ]
    while filter(lambda x: x[1], row):
        old = row
        row = []
        for it in old:
            txt, code = it
            if code:
                for (t, c) in encoding.items():
                    if code[:len(c)] == c:
                        row.append((txt + t, code[len(c):]))
                # NB we discard it if no initial segment of code matches an encoding.
            else: row.append(it)

    return map(lambda it: it[0], row)
