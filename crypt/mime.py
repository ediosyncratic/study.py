"""MIME-related encodings

See study.LICENSE for copyright and license information.
"""

def quotedPrintableDecode(text):
    parts = text.split('=')
    text = parts.pop(0)
    for part in parts:
        if part.startswith('\n'):
            text += part[1:]
        else:
            text += chr(int(part[:2], 16)) + part([2:])
    return text
