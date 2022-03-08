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

def encode(text, sep=' ', fallback=' '):
    """Performs Morse encoding.

    Required first parameter is the text to encode. This should
    consist of letters and spaces.

    Optional second parameter (default: a single space) is the
    separator to place between Morse tokens. Using an empty separator
    results in highly ambiguous results.

    Optional third parameter, fallback (default: a single space), is
    the character to use in place of any character not in the Morse
    repertoire. A valud of None will simply propagate the unencoded
    character.
    """
    # should really pre-process {'.': 'stop', ',': 'comma', '-': 'dash', ...}
    return sep.join(encoding.get(x, x if fallback is None else fallback) for x in text.upper())

def decode(message, sep=' ', fallback=' '):
    ans = ''.join(decoding.get(x, x if fallback is None else fallback) for x in message.split(sep))
    return ' '.join(ans.split()) # tidy up spacing

def decipher(message, prefix='',
             code=tuple(sorted(encoding.items(),
                               lambda a, b: cmp(len(b[1]), len(a[1])) or cmp(a[0], b[0])))):
    """Find candidate decodings of a text.

    If there aren't spaces between the morse tokens, the encoding is
    ambiguous.  This gives you an iterable over the candidates.  Note
    that there may be shockingly many candidates, even for quite a
    short text, even when space separators are included: the Morse
    codes for most letters can be reinterpreted as sequences of Morse
    codes for several others.

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
