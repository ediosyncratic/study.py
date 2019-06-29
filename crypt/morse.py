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
    return ' '.join(encoding.get(x, ' ') for x in text.upper())

def decode(message):
    ans = ''.join(decoding.get(x, ' ') for x in message.split(' '))
    return ' '.join(ans.split()) # tidy up spacing

def decipher(message, prefix='',
             code=tuple(sorted(encoding.items(),
                               lambda a, b: cmp(len(b[1]), len(a[1])) or cmp(a[0], b[0])))):
    """Find candidate decodings of a text.

    If there aren't spaces between the morse tokens, the encoding is
    ambiguous.  This gives you an interable over the candidates.

    Should normally be passed just one argument, the morse-encoded
    text (after demangling, if appropriate).  Can be passed an
    optional second parameter (present to allow a recursive
    implementation), which is simply added as a prefix to every
    candidate reading of the code; this defaults to empty.  Do not
    pass more than two arguments.
    """
    if message and message[0].isspace():
        prefix, message = prefix + ' ', message.lstrip()
    if message:
        for t, m in code:
            if message.startswith(m):
                for it in decipher(message[len(m):], prefix + t):
                    yield it
    else:
        yield prefix

def demangle(message, mangles={u'\u2013': '--', u'\u2014': '---', u'\u2026': '...'}):
    """Undo possible mangling from 'smart text' and similar cleverness.

    The author may have typed a sequence of dashes or dots, only to
    have their too-clever-by-half 'authoring tool' decide that they
    meant a long dash or ellipsis.  This decodes such mangled strings
    back to a reasonable guess at what the author intended.
    """
    # Example: https://www.johnanderikaspeak.com/an/2011/11/30/1107/
    # May be unicode encoded in UTF
    try: message = message.decode('utf-8')
    except UnicodeDecodeError: pass
    for k, v in mangles.items():
        message = message.replace(k, v)
    return message.encode()
