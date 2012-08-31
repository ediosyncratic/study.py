"""Implement suitably infinite values.

See study.LICENSE for copyright and license information.
"""

try: PosInf = float('Infinity')
except ValueError:
    try: from fpconst import PosInf
    except ImportError:
        import struct
        # From PEP 754's proposed implementation:
        be = struct.pack('i', 1)[0] != '\x01' # big-endian ?
        if be: PosInf = struct.unpack('d', '\x7F\xF0\x00\x00\x00\x00\x00\x00')[0]
        else:  PosInf = struct.unpack('d', '\x00\x00\x00\x00\x00\x00\xf0\x7f')[0]
        del be
        # note that judicious use of endian-flags of struct could make be redundant.

class Infinite:
    __slots__ = ( '__sign', )
    def __init__(self, sign=0): self.__sign = cmp(sign, 0)
    def __eq__(self, other, big=PosInf):
        if not self.__sign: return False
        if isinstance(other, Infinite):
            return self.__sign is other.__sign
        if self.__sign < 0: return -other == big
        return other == big

    def __lt__(self, other, big=PosInf):
        if self.__sign < 0:
            if isinstance(other, Infinite):
                return other.__sign > 0
            return -other < big
        return False

    def __gt__(self, other, big=PosInf):
        if self.__sign > 0:
            if isinstance(other, Infinite):
                return other.__sign < 0
            return other < big
        return False

    def __le__(self, other): return self < other or self == other
    def __ge__(self, other): return self > other or self == other
    def __ne__(self, other): return self.__sign and not self == other
    def __nonzero__(self): return True
    def __neg__(self): return Infinite(-self.__sign)
    def __pos__(self): return self
    def __abs__(self):
        if self.__sign: return Infinite(1)
        return 0

class Aleph0 (Infinite):
    """A value to use as infinity.

    See PEP 754: http://www.python.org/dev/peps/pep-0754/
    and PEP 326: http://www.python.org/dev/peps/pep-0326/

    I used a variant on the latter, but it would be nice to have a proper aleph
    null.\n"""

    __slots__ = ()
    __upinit = Infinite.__init__
    def __init__(self): self.__upinit(1)

    def __coerce__(self, other, big=PosInf):
        # if isinstance(other, Infinite): ??? shouldn't happen ?
        try: return big, float(other)
        except OverflowError: return None # give up :-(

    def __repr__(self): return 'Aleph0'

    from sys import maxint
    def __int__(self, val=maxint):
        """Best (machine) integer approximation ...

        For use as (for example) the length of an infinite sequence, self have
        to have a valid __int__, returning a value with nb_int filled in its
        type object.  This isn't ideal, but it's the best I can do ...\n"""

        return val
    del maxint

Aleph0 = Aleph0()

del PosInf
