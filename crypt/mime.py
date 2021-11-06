"""MIME-related encodings

See study.LICENSE for copyright and license information.
"""

def quotedPrintableDecode(text):
    """Decode tokens starting with '=' to recover original.

    If '=' is followed by a newline, these two characters are simply
    removed.  Otherwise, the first two characters after any '=' (or
    fewer if the string ends or another '=' appears sooner) must be
    hex digits (i.e. either one of the familiar decimal digits '0'
    through '9' or a letter earlier in the Latin alphabet than G) or a
    ValueError is raised.
    """
    parts = text.split('=')
    text = parts.pop(0)
    for part in parts:
        if part.startswith('\n'):
            text += part[1:]
        elif part:
            text += chr(int(part[:2], 16)) + part[2:]
    return text

def quotedPrintableEncode(text, avoid='', cols=70, upper=False):
    r"""Encode text using quoted printable.

    Single required argument is the text to encode.  Optional
    arguments:

      avoid -- a collection of characters to always encode; empty string by default.
      cols -- maximum separation between newlines in the result; default 70

    By default, all bytes < 32 (space) or > 126 (tilde) are encoded,
    with the exception of '\n', as is (inevitably) '=' itself.  Any
    characters supplied in avoid are encoded in addition to these; in
    particular, you can include '\n' in avoid if you want it encoded,
    too.  (Note that any receiver of quoted-printable data needs to be
    able to cope with '=', '\n' and hex digits.  None the less, you
    can encode them if you so chose.)  The resulting lines (or line,
    if you're encoding '\n' itself) are split, with '=\n', whenever
    they would otherwise have length > cols.

    Note that the output shall use '=', the usual decimal digits and
    some letters
    """

    res, size = '', 0
    while text:
        b, text = text[:1], text[1:]
        if b in avoid or b > '~' or b == '=' or (b < ' ' and b != '\n'):
            t = hex(ord(b))[2:]
            # hex() returns lower-case by default.
            if upper: t = t.upper()
            b = '=' + (2 - len(t)) * '0' + t
        if b == '\n': size = 0
        else: size += len(b)
        # If b isn't the end of a line, we need space for either
        # the next character or the '=' that introduces the
        # line-split that shall precede that next character.
        if size + (1 if text and not text.startswith('\n') else 0) > cols:
            res += '=\n'
            size = len(b)
        res += b
    return res
