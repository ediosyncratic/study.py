"""Cache files, directories and their managment.

A cache object knows the range of integer values it spans.  A prime cache object
also knows the range of prime indices it spans.

 * A cache can be asked, for any given value or index within its range, to
   supply an actual object describing the target (and a range about it); on
   failure, it returns a gap object describing the widest contiguous interval
   about this target not covered under this cache node.  If passed an optional
   gap object (describing such an interval about the integer) it instead returns
   the intersection of this gap object with the one it would have returned.

 * An object returned from a cache, when not a gap object, represents from the
   cache file.  It records the range of integers spanned, as an object of type
   Interval, but only specified relative to the cache file's directory's base
   offset; if the object describes a prime cache file, its index ranges are
   likewise relative to the directory's base.  The recursion out of the search
   for the object computes the offsets and the cache root object returns a tuple
   of the object and its offset(s).  It equally does this for a gap object.

 * Files and directories follow a common naming scheme, encoding the range of
   values relative to parent.  Within a directory, lexicographic sort order
   should match numeric order, so the directory decides how many digits there
   are in each name; names are of form [PQ]+[0-9a-z]+\+[0-9a-z]+.py, in which
   the numbers are encoded using base 36; they indicate the range of integers
   the named thing spans; in [PQ]+, P indicates the object provides some
   information on primes, Q indicates some information on factors.  Since the
   start and length of each cache block is a multiple of the cache's extended
   octet modulus, the factor of this is always implicit.  The first number
   (immediately following the leading [PQ]+) is the difference between the start
   of the cache object's range and that of the directory it's in; the second
   number (following the + character) is the length of the range.  Each number
   is padded with ehough leading 0s to make it as long as all the others in the
   same position as it for other names in the same directory, so as to ensure
   clean sorting.

 * Directories have an __init__.py that records (at least) the maximum depth of
   directory hierarchy below them (each is 1 higher than the max of its
   children); and, in a prime cache, their range of indices.  Each prime cache
   file (not bothering to record depth 0 below it) records its own range of
   indices within this.  Again, ranges of indices of files or sub-directories in
   a directory are relative to the start-index of the directory's range.

 * Each cache root records a list of the primes it uses to define its
   generalized octets; there's a file (somewhere in this cache) which describes
   the interval from 0 to some multiple of their product; when asking it about
   the early part of its range, we need to be sure we don't forget the special
   case of this base list.  Since the top-level directory doesn't get to set its
   own name, its __init__.py also has to record the range of values it spans.

 * Cache root directories may also remember sporadics, in so far as we've
   discovered any; that is, a list of primes beyond the end of the covered range
   and, in a factor cache, a mapping from integers to one proper factor each.
   The latter is not worth recording, however, if the factor is one of the
   primes used for the octet type of this cache.  It may make some sense to
   record these sporadics relative to the top of the number range covered by the
   cache.

 * The cache root __init__.py may eventually record a cache format version, for
   future-prooofing purposes !  However, until the need for that is realised, we
   can leave it out and have it default to 0 if not found :-)

 * At run-time, the only nodes held persistently in memory describe some
   directories, to facilitate searching.  Parents refer to children only via
   weakrefs, so they can fall out of cache naturally; but children need their
   parents, so use proper references to them.  Parents probably remember the
   result of digesting os.listdir(), once they know it, again via weakref; it
   may remember the live children via this, in which case this is the only place
   it needs a weakref.

TODO: what difference does it make to the cache objects if they're a modifiable
cache ?  It affects whether things can be added, renamed, etc.

TODO: I clearly need to think some more about the class hierarchy.  The entries
in a directory need to look something like CacheNode (i.e. represent intervals
and support comparison as such) and support a 'load-me' method (which looks to
see if it's a file or a sub-directory).  The root's constructor can be as
Interval(0, None) with a path-name and the machinery to read (and sort) its
directory; every subordinate node needs to know who its parent is, what its
relative name is and be an interval.  Mapping names to paths can be mediated by
a .path(*args) method which delegates upwards to the root, which actually uses
os.path.join; so leaves can self.path(), implicitly adding self.__name, and
directories can self.path('__init__.py'), either adding self.__name or (for
root) simply slapping it on the root directory.  So root doesn't descend from
directory; I guess we need a base class shared by leaves and sub-directories;
which has a base-class shared with roots.  Still, root and sub-directory deserve
to have a common (probably mix-in) base-class that mediates directory listing
and parsing; the .__up of any leaf or sub-directory is an instance of this.
(Note: this is a good example of where classic single-inheritance falls down,
although ruby's version of it copes.)

ReadCacheDir: can parse directories
CacheNode: intervalish, has some conception of weakly-linked loaded object
  CacheRootNode (ReadCacheDir): knows its directory; resolves path
  CacheSubNode: knows its interval, parent and relative name
    CacheSubDir (ReadCacheDir): delegates path
    CacheFile: simple file

A directory node always reads its __init__.py but only parses its listing when
needed; a file node only reads its file when needed.  The directory thus wants
to use a weakref for its Ordered listing (and any listings filtered therefrom)
while a file wants to use a weakref to its pseudo-module object.

$Id: cache.py,v 1.1 2008-06-26 07:36:59 eddy Exp $
"""

def read_cache(name, bok=None, glo={}):
    if bok is None: bok = {}
    execfile(name, glo, bok) # may raise IOError, ParseError
    return bok

def write_cache(name, __doc__=None, **what):
    fd = open(name, 'w')
    try:
        if __doc__is not None:
            fd.write('"""%s"""\n\n', __doc__)
        for k, v in what.items:
            fd.write('%s = %s\n' % (k, repr(v)))
    finally: fd.close()
    # potentially: return size of file

import os
from study.snake.interval import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr

class ReadCacheDir (object):
    def __init__(self):
        # TODO: clean up
        for k, v in read_cache(self.path('__init__.py')).items():
            if k[0] != '_' or k == '__doc__':
                setattr(self, k, v)

    @weakattr
    def listing(self, ig=None, get=os.listdir, seq=Ordered,
                pat=re.compile(r'^([PQ]+)([0-9a-z]+)\+([0-9a-z]+)(\.py)?$')):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                start, span = int(got.group(2), 36), int(got.group(3), 36)
                # TODO: what should we do when modifiable ?
                if got.group(4): klaz = ReadCacheFile
                else: klaz = ReadCacheSubDir
                ans.append(klaz(self, name, start, span, got.group(1)))

        return ans

    @weakattr
    def primes(self, ig=None):
        return filter(lambda i: i.prime, self.listing)

    @weakattr
    def factors(self, ig=None):
        return filter(lambda i: i.factor, self.listing)

class ReadCacheRoot (ReadCacheDir, Interval):
    __rcinit = ReadCacheDir.__init__
    __gapinit = Interval.__init__
    def __init__(self, path):
        self.__dir = path
        self.__rcinit()
        self.__gapinit(0, None)

    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)
    # May need __setattr__ to intercept some of __gapinit's setattr()

class ReadCacheSubNode (Interval):
    __gapinit = Interval.__init__
    def __init__(self, parent, name, start, span, types):
        self.__gapinit(start, span)
        self.__up, self.__name = parent, name
        self.prime, self.factor = 'P' in types, 'Q' in types

    def path(self, *tail): return self.__up.path(self.__name, *tail)

class ReadCacheSubDir (ReadCacheDir, ReadCacheNode):
    __rcinit = ReadCacheDir.__init__
    __upinit = ReadCacheSubNode.__init__
    def __init__(self, parent, name, start, span, types):
        self.__upinit(parent, name, start, span, types)
        self.__rcinit()
    # May need __setattr__ to intercept some of __gapinit's setattr()

class ReadCacheFile (ReadCacheNode):
    @weakattr
    def content(self, ig=None):
        return read_cache(self.path())


del Interval Ordered, weakattr
from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

del os
