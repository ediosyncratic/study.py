"""Interaction with the system crypt() utility.
"""

def encrypt(text):
    """Calls crypt() on a text, with a suitable salt.

    Takes one argument, the text to be crypt()ed.
    Returns a string, the result of crypt()ing the given text.

    This is actually an initialiser which, the first time it's called, replaces
    itself in tools' namespace with something more efficient which has had all
    its initialisation done already.  This subsequently delegates to its
    replacement. """

    global encrypt # I'm what gets called the first time: I over-write myself.

    fd = encrypt.func_defaults
    if fd is None or len(fd) is not 2:
	# I'm encrypt: replace myself with someone more efficient.
	import string
	from crypt import crypt
	from whrandom import whrandom
	gen = whrandom()

	def get(c=gen.choice, r = string.digits + string.letters + '/.'):
	    """Generates a salt for use as crypt's second argument."""
	    # not to be *called* until gen.seed() has been called ...
	    return c(r) + c(r)

	try:
	    top = 1
	    while 1: top = top + top
	except OverflowError: pass
	def unsign(val, top=top):
	    # coerces a negative int to a positive one
	    if val < 0: return top + val + top
	    return val
	def mash(obj, un=unsign):
	    # forces hash-value positive
	    return un(hash(obj))
	def bash(a, b): return (a & 0xff) ^ (b >> 8)

	# (the hash()es of functions change from session to session)
	x, y, z = mash(encrypt) ^ mash(bash), mash(get) ^ mash(mash), mash(crypt) ^ mash(unsign)
	# (gen.seed() really takes unsigned char arguments)
	while (x|y|z) >> 8: x, y, z = bash(y, z), bash(z, x), bash(x, y)
	gen.seed(x, y, z) # ... so get() is now ready for use.

	def encrypt(t, c=crypt, g=get):
	    """Calls crypt() on a text, with a suitable salt.

	    Takes one argument, the text to be crypt()ed.
	    Returns a string, the result of crypt()ing the given text.

	    Will not behave as advertised if called with more than one argument. """

	    return c(t, g())

    return encrypt(text)

