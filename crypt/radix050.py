"""Code to perform `radix 050' encoding and decoding.

This encoding recognises fourty characters: space, a through z, dollar,
full-stop, question-mark, 0 through 9.  These are numbered 0 to 39 in the order
given and a string is then read as a number in base fourty (aka octal 050).
Letters are handled case-insensitively.  This provides for strings of up to six
characters (and a few seven-character ones) to be represented as 32-bit integers
(strings earlier than t84qkg don't use the top bit).

This module exports two functions:

 encode(string) -- yields a number.

 decode(number) -- yields a string.

Bill has found that this is the wrong way to approach the decoding, though: it
is actually done by cutting the int, seen as a sequence of bits, into chunks,
then decoding each chunk separately.

$Id: radix050.py,v 1.1 2007-03-08 23:56:35 eddy Exp $
""" # ' deconfuse font-lock

_charset = " abcdefghijklmnopqrstuvwxyz$.?0123456789"
_rootmaxint = 1
_bigint = 1
try:
	while 1:
		_rootmaxint = _rootmaxint * 2
		_bigint = _bigint * 4

except OverflowError: pass
# Assert: any x representable by a (non-long) int satisfies abs(x / _rootmaxint) < _rootmaxint

def decode(number):
	"""Decodes a string from `radix 050' form.

	Argument, number, should be a positive integer (however, negative
	integers down to -2147483647 will be suitably interpreted as signed
	readings of unsigned ints: pow(2,32) will be added).  This number is
	expressed in base 40 (aka octal 050) and thereby read as a string.

	Letters are emitted lower-case.
	"""

	if number < 0:
		# coerce signed int to unsigned ...
		if -number / _rootmaxint < _rootmaxint:
			number = pow(2L, 32) + number

		else: raise ValueError, number	# huge, negative: not a radix050 string.

	row = []
	while number:
		number, here = divmod(number, 40)
		row.append(_charset[int(here)])

	return reduce(lambda x,y: y+x, row, '')

def encode(text):
	"""Encodes a string in `radix 050' form.

	Argument, text, is a string.  It should only use the characters known to
	the radix 050 encoding (alphanumeric, space, $, . and ?) - all others
	will be treated as spaces (but I reserve the right to treat them as some
	other character instead - probably `?').  The string is read as a radix
	050 (i.e. fourty) number using these characters as digits (with space as
	0, letters as 1 through 26, $, . and ? as 27, 28 and 29, each digit as
	its value plus 30).  Note that leading spaces are ignored (just like
	leading zeros on a number).
	"""

	number = 0
	import string
	for ch in text:

		try: here = string.index(_charset, string.lower(ch))
		except ValueError: here = 0	# perhaps use ? in place of ch.

		try: number = number * 40 + here
		except OverflowError: number = number * 40L + here

	return number
