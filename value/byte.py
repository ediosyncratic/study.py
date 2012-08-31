"""Description of bytes, including the kilo = 1024 twist ...

... but note kibi, mibi etc. should obsolete these; see quantity.py

See also: units.py
See study.LICENSE for copyright and license information.
"""
from quantity import Quantity, qSample, _quantifier_dictionary

class bQuantity (Quantity):
    # TODO: fold back into Quantity somehow
    # FIXME: probably doesn't work at present !  e.g. _quantade_split_() doesn't exist.
    def __quantity__(self, what, units):
        try: order = units['bit']
        except KeyError: order = 0
        if order: return self.__class__(what, units)
        return Quantity(what, units)

    def _lazy_get__unit_str_(self, key):
        order = self._unit_order('bit')
        factor, mul = self._quantade_split_(order)

        if factor < 1: factor, mul = 1, ''
        tail = self / pow(factor * byte, order)
        num, uni = tail._str_frags()

        if order > 0:
            if order == 1: here = mul + 'byte'
            else: here = '%sbyte^%d^' % (mul, order)

            if uni:
                ind = uni.find('/')
                if ind < 0: uni = uni + '.' + here
                elif ind > 0: uni = uni[:ind] + '.' + here + uni[ind:]
                else: uni = here + uni

            else: uni = here

        elif order < 0:
            if order == -1: uni = '%s/%sbyte' % (uni, mul)
            else: uni = '%s/%sbyte^%d^' % (uni, mul, -order)

        self._number_str, self._unit_str = num, uni

        if key == '_unit_str': return uni
        elif key == '_number_str': return num
        else: raise ValueError, key

    _lazy_get__number_str_ = _lazy_get__unit_str_

bit = Quantity.base_unit('bit', 'bit',
                         """The definitive unit of binary data.

A single binary digit is capable of exactly two states, known as 0 and 1.
A sequence of n binary digits thus has pow(2, n) possible states.
""")

_name = 'byte'
byte = bQuantity(qSample(best=8), # but no actual sample
                bit,
                """The standard unit of memory on a computer.

Whereas the bit is the *natural* unit of binary data, in practice one normally
manipulates data in larger chunks.  These may vary from machine to machine, but
one mostly deals with multiples of 8 bits (these days) - these suffice for 256
distinct values.  An 8-bit byte is also known as an `octet'.

Groups of (typically 2, 4, 8 and sometimes higher powers of two) bytes tend to
have special significance to the machine, and form the building blocks out of
which one builds data structures.

On the large scale, one tends to measure amounts of data in units of
kibibytes, mibibytes, gibibytes, ... (commonly called kilobytes, megabytes,
gigabytes, etc.) in which the factor by which each is a multiple of its
predecessor is 1024, rather than the 1000 normally used in kilo, mega, giga
and so on.  So I also define Kib, Mib, Gib, along with the full swath of
positive-exponent quantifiers applied to byte, as kibibyte
etc. above.  Indeed, it is mainly in order to do this that I bother defining
the byte ...
""",
                _name)

_row = filter(lambda x: x > 0, _quantifier_dictionary.keys())
_row.sort()
for _key in _row:
    if _key % 3: continue # skip deka, hecto
    _nom = '%sbyte' % _quantifier_dictionary[_key]
    exec '%s = Quantity.unit(1024, %s, fullname="%s")' % (_nom, _name, _nom)
    _name = _nom

del _nom, _name, _key, _row, _quantifier_dictionary, Quantity, qSample
Kb, Mb, Gb = kilobyte, megabyte, gigabyte
