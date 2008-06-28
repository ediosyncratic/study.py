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

(Note: this is a good example of where classic single-inheritance falls down,
although ruby's version of it copes.)

$Id: cache.py,v 1.2 2008-06-28 07:36:26 eddy Exp $
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
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr, lazyattr, lazyprop
class Gap (Interval):
    __upinit = Interval.__init__
    def __init__(self, *what):
        self.__upinit(*what)
        self.prime = self.factor = False

class ReadCacheDir (object):
    def __init__(self, Range=Interval):
        for k, v in read_cache(self.path('__init__.py')).items():
            if k == 'indices':
                # saved to file as (start, span) twople.
                setattr(self, k, Range(v[0], v[1])
            elif k[0] != '_' or k == '__doc__':
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

        assert ans[0].start == 0, 'Directory lacks initial sub-range'
        assert self.start + ans[-1].stop == self.stop, \
               'Directory lacks final sub-range'

        return ans

    @weakattr
    def primes(self, ig=None):
        return filter(lambda i: i.prime, self.listing)

    @weakattr
    def factors(self, ig=None):
        return filter(lambda i: i.factor, self.listing)

    def get_primes(self, value, gap=None, index=None):
        """Find a chunk or gap enclosing a designated integer.

        Like get_factor (q.v.) except that it returns data on primes, and allows
        value to be None, in which its (otherwise ignored and optional) third
        argument, index, must be supplied: in this case, index must be a natural
        and the effect is as if primes[index] had been passed as value.\n"""

        if not self.prime:
            assert isinstance(self, ReadCacheRoot)
            return gap

    def __bubble(self, tuple):
        kid, off = tuple[:2]
        off += self.start
        try: return kid, off, tuple[2] + self.indices.start
        except (AttributeError, IndexError): assert not kid.prime
        return kid, off

    def __prime_gap(self, what):
        """Returns interval of primes spanning what.

        Sole argument describes a natural (interpreted as a range of length one)
        or a range of naturals, relative to the start of the interval self
        describes; return is a range r for which primes[r.start] is not greater
        than the start of the given range and primes[r.start] is greater than
        every natural in the range; and r shall be the narrowest range for which
        self can be sure this holds true.  If self has no information from which
        to determine such a range, AttributeError is raised instead.\n"""
        pass

    def get_factors(self, value, gap=None, Range=Gap):
        """Find a chunk or gap enclosing a designated integer.

        Arguments:
          value -- integer to be found in cache (required)
          gap -- optional Gap around value else (default) None

        Required first argument, value, is a natural number.  The return shall
        describe an interval in which that natural falls.  The return is always
        a tuple: its first member is either a Gap or a ReadCacheFile; the second
        is a natural, the offset of the range described by the first relative to
        the start of the range described by self; if present, the third is the
        offset of the index-range of the first relative to the start of self's
        index-range.  In the following, this tuple's first two entries are
        called object and offset; and object.start <= value - offset <
        object.stop shall always hold true.

        When the cache contains data covering the interval requested, object
        shall be a ReadCacheFile; its .content contains the namespace of its
        cache file, which provides factor information, and its .next and .prev
        attributes may be used to access the following or preceding chunk, if
        available in this cache.

        Otherwise, object shall be a Gap (recognizable as such by its lack of a
        .content), describing a gap in self's range of covered values; if the
        optional second argument gap is supplied, the returned Gap is its
        intersection with the widest range of naturals about value in which self
        has no data.  When gap is None, it is treated as if it were Gap(0,
        None).  When object is a Gap, it may have an Interval as .indices,
        indicating a range of prime indices spanning (but possibly wider than)
        the indices of primes in the gap, relative to self.indices.start (if
        known).\n"""

        if not self.factor:
            assert isinstance(self, ReadCacheRoot)
            return gap

        try: ind = self.factors.index(value)
        except ValueError, what:
            ind = what[2] # where in .factors our gap would be inserted
            assert ind > 0, 'Directory lacks initial or final sub-range'
            base = self.factors[ind-1].stop
            assert base is not None, 'Badly sorted intervals'
            kid = Range(base, self.factors[ind].start - base)
            if gap is not None:
                kid = gap.trim(kid)
                kid = Range(kid.start, kid.stop - kid.start)
            try: kid.indices = self.__prime_gap(kid)
            except AttributeError: pass
        else:
            kid = self.factors[ind]
            if isinstance(kid, ReadCacheDir):
                return kid.__bubble(kid.get_factors(value - kid.start,
                                                    gap - kid.start))
            assert isinstance(kid, ReadCacheFile)

        return kid, 0, 0

class ReadCacheRoot (ReadCacheDir, Interval):
    __rcinit = ReadCacheDir.__init__
    __gapinit = Interval.__init__
    def __init__(self, path):
        self.__dir = path
        self.__rcinit() # __init__.py must specify self.stop
        self.__gapinit(0, self.stop)

    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)
    # May need __setattr__ to intercept some of __gapinit's setattr()

    @lazyprop
    def prime(self, ig=None): return len(self.primes) > 0
    @lazyprop
    def factor(self, ig=None): return len(self.factors) > 0

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

    @lazyattr
    def next(self, ig=None):
        pass # TODO: implement me

    @lazyattr
    def prev(self, ig=None):
        pass # TODO: implement me


del Interval, Ordered, weakattr, lazyattr, lazyprop
from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

del os
