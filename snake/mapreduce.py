"""An implementation of MapReduce.

This isn't *quite* as simple as just reduce(join, map(xfrm, data)), since the
map needn't produce only one output per input and its outputs are expected to be
key-value pairs; the reduce then combines values from all instances of each key,
associating the combined value with that key.  This implementation is not meant
to be especially performant: it exists for the pedagogic purpose of illustrating
what MapReduce does.
"""

def MapReduceDict(src, each=lambda x: (x,), join=lambda v, p=(): p + (v,)):
    """Simplest possible pythonic MapReduce.

    This returns a dictionary that maps keys to joined values, joining those
    values two at a time.  Takes three arguments:

      src -- an iterable
      each -- map each entry in src to an iterable over key-value pairs
      join -- combine a prior value with a new one for the same key

    Thus for each item in src, we iterate each(item)'s key-value pairs; the
    first time a given key is seen, join(value) is stored in the result
    dictionary; whenever that key recurs, join(value, prior) is called to
    combine the new value with the prior value, which is over-written by the
    return.  Defaults are provided for each and join: the default for each uses
    each item in src as a single key-value pair to use; that for join collects
    up all of the values seen into a tuple.  Returns the resulting mapping from
    keys to the result of joining values.\n"""

    bok = {}
    for item in src:
        for key, value in each(item):
            try: prior = bok.get(key)
            except KeyError: bok[key] = value
            else: bok[key] = join(value, prior)

    return bok

# TODO: think of a way to pipeline it through iterables all the way.
