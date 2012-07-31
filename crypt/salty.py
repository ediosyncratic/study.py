"""Interaction with the system crypt() utility.
"""

def encrypt(text):
    """Calls crypt() on a text, with a suitable salt.

    Takes one argument, the text to be crypt()ed.
    Returns a string, the result of crypt()ing the given text.

    This is actually an initialiser which, the first time it's called,
    replaces itself in the module's' namespace with something more efficient
    which has had all its initialisation done already.  This subsequently
    delegates to its replacement.\n"""

    global encrypt # I'm what gets called the first time.
    # I replace myself with someone more efficient:

    import string
    from crypt import crypt
    import random as gen

    def get(c=gen.choice, r=string.digits + string.letters + '/.'):
        """Generates a salt for use as crypt's second argument."""
        # not to be *called* until gen.seed() has been called ...
        return c(r) + c(r)

    from sys import maxint
    def unsign(val, top=maxint+1):
        # coerces a negative int to a positive one
        if val < 0: return top + val + top
        return val
    def mash(obj, un=unsign):
        # forces hash-value positive
        return un(hash(obj))
    def bash(a, b): return (a & 0xff) ^ (b >> 8)

    # (the hash()es of functions change from session to session)
    gen.seed((mash(encrypt), mash(bash), mash(get),
              mash(mash), mash(crypt), mash(unsign)))
    # ... so get() is now ready for use.

    from study.snake.decorate import overriding
    @overriding(encrypt) # Carry doc-string and signature over to replacement
    def encrypt(t, c=crypt, g=get): return c(t, g())

    # Delegate to replacement explicitly, for this first call:
    return encrypt(text)
