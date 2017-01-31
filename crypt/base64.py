"""Base 64 encoding (as used by MIME, with quirks).

Note that python's standard library has a base64 module which provides
b64encode() and b64decode(), probably more efficiently than this -
which is mostly a pedagogic exercise.

See study.LICENSE for copyright and license information.
"""

class Base64 (object):
    def __init__(self, tail=None, pad=None):
        if tail is not None:
            self.__tail = tail
            if pad is None:
                try: pad = self.__tails[tail]
                except KeyError: pass

        if pad is not None:
            self.__pad = pad
    __tail = __pad = None # Fall-backs

    from study.crypt.base import Base
    __latin1 = Base(''.join(chr(i) for i in range(256)))
    __tails = {'+/': '=', '-_': '=', '._': '-', '~-': '=',
               # Variants without padding:
               '+,': '', '.-': '', '_:': '', '!-': ''}
    import string
    def b64(tail, # Last two digits - known variants are listed in __tails
              base62 = string.uppercase + string.lowercase + string.digits,
              B=Base):
        return B(base62 + tail)
    del Base, string

    def __fripperies(self, pad, tail, base=b64):
        if tail is None:
            tail = self.__tail or '+/'

        if len(tail) != 2:
            raise ValueError(
                'Should be just two characters for variable part of base 64', tail)
        if any(ch.isalnum() for ch in tail):
            raise ValueError(
                'Letter or digit re-used in last two characters for base 64', tail)

        if pad is None:
            if self.__pad:
                pad = self.__pad
            else:
                try: pad = self.__tails[tail]
                except KeyError: pad = '='

        if self.__tail is None:
            self.__tail = tail

        return base(tail), pad
    del b64

    def encode(self, text, pad=None, tail=None):
        """Encode in base 64
        """
        b64, pad = self.__fripperies(pad, tail)
        cipher = ''
        while len(text) >= 3:
            here, text = self.__latin1.decode(text[:3]), text[3:]
            cipher += b64.encode(here)

        if text:
            assert(len(text) in (1, 2))
            copy, shift = divmod((3 - len(text)) * 8, 6)
            here = self.__latin1.decode(text) << shift
            cipher += b64.encode(here) + copy * pad

        return cipher

    def scan(self, cipher, pad=None, tail=None):
        """Messy heuristics to discover pad and tail for decode.

        Use at your own risk.
        """
        cipher = ''.join(cipher.split()) # discard any spacing
        if not cipher: return pad, tail
        punk = set(ch for ch in cipher if not ch.isalnum())
        if len(punk) > 3:
            raise ValueError('Too much weird in cipher text', ''.join(punk))
        n, last = len(cipher) % 4, ''
        if n:
            n = 4 - n # amount of omitted padding
        elif cipher[-1] in punk: # maybe padded
            last = cipher[-1]
            n = len(cipher) - cipher.find(last)
            if len(set(cipher[-n:])) > 1:
                n = 0 # not (valid) padding
            elif pad == last:
                punk.discard(pad)
                cipher = cipher[:-n]
            elif len(punk) == 3:
                pad = last
                punk.discard(pad)
                cipher = cipher[:-n]
            else:
                if pad is None: pad = last # tentatively
                n = 0

        if len(punk) == 2:
            known = [x for x in self.__tails if punk.issuperset(known)]
            if known:
                assert len(known) == 1, known
                tail = known[0]

        elif punk:
            assert len(punk) == 1, punk
            ch = punk.pop()
            known = [x for x in self.__tails if ch in x]
            nice = [k for k in known if self.__tails[k] != pad]
            if nice:
                known = nice
                assert not (pad and any(k for k in known if pad not in k)), (known, pad)
            elif pad:
                nice = [k for k in known if pad not in k]
                if nice: known = nice
            if len(known) == 1:
                tail = known[0]

        if pad != last or (pad and not n):
            pad = self.__tails[tail]
        return pad, tail

    def decode(self, cipher, pad=None, tail=None):
        text, cipher = '', ''.join(cipher.split()) # discard any spacing
        b64, pad = self.__fripperies(pad, tail)
        if pad: cipher = cipher.rstrip(pad)

        while len(cipher) >= 4:
            here, cipher = b64.decode(cipher[:4]), cipher[4:]
            here = self.__latin1.encode(here)
            if len(here) < 3: text += '\0' * (3 - len(here))
            text += here

        if cipher:
            shift = ((4 - len(cipher)) * 6) % 8
            here = b64.decode(cipher) << shift
            text += self.__latin1.encode(here)

        return text
