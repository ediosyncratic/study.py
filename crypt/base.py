"""Playing with different number bases.

Note: 3**5 is 243, aka 11110011, representable in 8 bits with 13, aka 1101, free
tokens ...
2**27 is 134217728, 3**17 is 129140163, aka 111101100101000010111000011.
2**485 > 3**306, 2**1539 > 3**971, 2**2593 > 3**1636, ... 2**75235 > 3**47468,
all giving longer initial sequences of 1s for the power of 3.

All things considered, 3**5 and 2**8 are the pair to use; 8bits is a familiar unit,
5 signs packed in a byte sounds cool.

$Id: base.py,v 1.1 2001-12-03 19:11:59 eddy Exp $
"""

class Base:
    """Handling of varied number base for python.

    Represents a base by a sequence of `digits' to use in that representation.
    """

    def __init__(self, digits='0123456789',
		 offset=0, signif=None,
		 plus='+', minus='-', point='.',
		 ignore=','):
	self.__digits, self.__ignore = digits, ignore
	self.__plus, self.__minus, self.__point = plus, minus, point
	self.__base, self.__offset = len(digits), offset

	if signif is None:
	    err, signif = 1e-6, 0
	    while err < 1 and signif < 100:
		err, signif = self.__base * err, 1 + signif

	self.__sigfig = signif

    def __unique(self, char):
	while len(char) > 1: char = char[0]
	return char

    def __encode(self, index):
	return self.__unique(self.__digits[int(index) + self.__offset])

    def encode(self, number, signif=None):
	if signif is None: signif = self.__sigfig

	if number < 0:
	    sign = self.__unique(self.__minus)
	    number = -number
	elif number > 0: sign = self.__unique(self.__plus)
	else: sign = ''

	shunt = 0
	while 1:
	    whole = long(number)
	    if number == whole or shunt >= signif: break
	    else:
		shunt = 1 + shunt
		try: number = self.__base * number
		except OverflowError: number = long(self.__base) * number

	if number > .5 + whole: whole = 1 + whole # round up.
	result = ''

	while shunt > 0:
	    whole, digit = divmod(whole, self.__base)
	    if digit + self.__offset >= self.__base:
		whole, digit = whole + 1, digit - self.__base
	    result, shunt = self.__encode(digit) + result, shunt - 1

	if result: result = self.__unique(self.__point) + result

	while whole:
	    whole, digit = divmod(whole, self.__base)
	    if digit + self.__offset >= self.__base:
		whole, digit = whole + 1, digit - self.__base
	    result = self.__encode(digit) + result

	return sign + result

    def __decode(self, digit):
	last = - self.__offset
	for it in self.__digits:
	    if digit in it: break
	    last = last + 1
	else:
	    raise ValueError, ('Bad digit', digit, self.__digits)

	return last

    def decode(self, given):
	if given and (given[0] in self.__plus or given[0] in self.__minus):
		sign, given = given[0], given[1:]
	else:	sign = self.__unique(self.__plus)
	result = 0

	while given and given[0] not in self.__point:
	    left, given = given[0], given[1:]
	    if left in self.__ignore: continue
	    left = self.__decode(left)

	    try:	result = result * self.__base + left
	    except OverflowError:
			result = result * long(self.__base) + left

	if given: given, unit = given[1:], 1.
	while given:
	    left, given = given[0], given[1:]
	    if left in self.__ignore: continue
	    left, unit = self.__decode(left), unit / self.__base
	    result = result + left * unit

	if sign in self.__minus: return -result
	return result

binary = Base('01')
signal = Base('T01', offset=1)
octal = Base('01234567')
decimal = Base()
hexadecimal = Base(( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		     'aA', 'bB', 'cC', 'dD', 'eE', 'fF' ))
radix050 = Base((' ', 'aA', 'bB', 'cC', 'dD', 'eE', 'fF', 'gG', 'hH', 'iI',
		 'jJ', 'kK', 'lL', 'mM', 'nN', 'oO', 'pP', 'qQ', 'rR', 'sS',
		 'tT', 'uU', 'vV', 'wW', 'xX', 'yY', 'zZ', '$', '.', '?',
		 '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'))

def Conway13(number, en=Base('0123456789+-.').encode, de=Base(ignore='+-.').decode):
    """The Conway Base 13 function (approximately).

    Caricatures the meanest hairiest function I know how to integrate. """

    return de(en(number))

_rcs_log = """
 $Log: base.py,v $
 Revision 1.1  2001-12-03 19:11:59  eddy
 Initial revision

"""
