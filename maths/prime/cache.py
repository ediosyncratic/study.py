"""Cache files, directories and their managment.

A cache object knows the range of integer values it spans.  A prime cache object
also knows the range of prime indices it spans.  Maybe a factor cache object
shall, too.

Reading caches:

 * A cache can be asked, for any given value or index within its range, to
   supply an actual object describing the target (and a range about it); on
   failure, it returns a gap object describing the widest contiguous interval
   about this target not covered under this cache node.  If passed an optional
   gap object (describing such an interval about the target) it instead returns
   the intersection of this gap object with the one it would have returned.

 * The returned Interval instance (whether gap or cache file) describes its
   range of integers relative to its .parent's range; however, it also has a
   .span giving the absolute range.  It may likewise have a .indices giving its
   range of indices (for prime cache requests it normally shall).  Cache files
   describing incompletely sieved chunks of the naturals may also exist; these
   are apt (but not sure) to know their index start-point and perhaps even an
   upper bound on their number of primes.

 * Files and directories follow a common naming scheme, encoding the range of
   values relative to parent.  Within a directory, lexicographic sort order
   should match numeric order, so the directory decides how many digits there
   are in each name; names are of form [PF]+[0-9a-z]+\+[0-9a-z]+.py, in which
   the numbers are encoded using base 36; they indicate the range of integers
   the named thing spans; in [PF]+, P indicates the object provides some
   information on primes, F indicates some information on factors (note
   upper-case to distinguish from the subsequent base-36 number which might
   start with p or f).  Since the start and length of each cache block is a
   multiple of the cache's extended octet modulus, the factor of this is always
   implicit.  The first number (immediately following the leading [PF]+) is the
   difference between the start of the cache object's range and that of the
   directory it's in; the second number (following the + character) is the
   length of the range.  The first number is padded with ehough leading 0s to
   make it as long as all the first numbers of other names in the same
   directory, so as to ensure clean sorting (partitioned by [PF]+).

 * Directories have an __init__.py that records (at least) the maximum depth of
   directory hierarchy below them (each is 1 higher than the max of its
   children); and, in a prime cache, their range of indices.  Each completed
   prime cache file (not bothering to record depth 0 below it) records its own
   range of indices within this.  Again, ranges of indices of files or
   sub-directories in a directory are relative to the start-index of the
   directory's range.

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
   parents, so use proper references to them.  Parents likewise remember the
   result of digesting os.listdir(), once they know it, via weakref; this result
   is a list of child nodes which haven't initially evaluated their weakref
   attributes.

Writing caches:

 * Cache roots must support locking so that only one process tries to modify a
   cache at a time, and others don't try to read a cache while it's being
   modified.
 * Use one modifiable cache supported by a sequence of read-only caches
   - Files, once created, only change when renamed, which may prompt meta-data
     changes (base offset for ranges of indices and values have changed) but not
     changes to their meaning.
   - Directories in the modifiable cache are wont to be reshuffled: when a
     directory has too many entries, separate them into sub-directories.

TODO: what difference does it make to the cache objects if they're a modifiable
cache ?  It affects whether things can be added, renamed, etc.

(Note: this is a good example of where classic single-inheritance falls down,
although ruby's version of it copes.)

$Id: cache.py,v 1.15 2008-07-13 15:35:38 eddy Exp $
"""
import os
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr, lazyattr, lazyprop

class Node (Interval):
    """Base class for cache nodes
    """

    from errno import EWOULDBLOCK as __BLOCKED

    class Bok (dict): pass # for weakref's sake
    def load(bok=None, glo={}, Range=Interval, Dict=Bok):
        # TODO: need a lock-check on all ancestors
        # TODO: want to be able to GET a URL instead of reading a file

        if bok is None: bok = Bok()
        if not self.lock(True):
            raise IOError(self.__BLOCKED, 'File temporarily unreadable',
                          self._content_file)
        try: execfile(self._content_file, glo, bok) # may raise IOError, ParseError
        finally: self.unlock(True)

        try: self.depth = bok.pop('depth')
        except KeyError: assert isinstance(self, CacheFile)

        try: gap = bok.pop('indices') # saved to file as twople
        except KeyError: pass
        else: self.__indices = Range(gap[0], gap[1])

        return bok
    del Bok

    @weakattr
    def content(self, ig=None): return self.load()

    @lazyattr
    def span(self, ig=None):
        """Absolute interval of naturals spanned by this Node.

        Relies on derived classes to define ._span(range), which converts:
          * from a range relative to the same base-point as self, measured in
            units of this cache's octet
          * to an actual range of naturals.

        This is achieved in two phases: SubNode recursively adds directory
        offsets to an interval, initially self, going up the directory hierarchy
        to the root, which does the needed re-scaling.\n"""
        return self._span(self)

    @lazyprop
    def _indices(self, ig=None): # Gap may have it set
        """Range of prime indices, relative to context."""
        try: return self.__indices
        except AttributeError:
            if not self.prime: raise
        self.content # evaluate to force .load()ing (fails for Gap)
        return self.__indices # should now succeed

    @staticmethod
    def __repr(val):
        """Decides how to display values

        While meta-data is mostly terse, the actual tuples of factors and
        strings encoding prime octets are very long, so should be split across
        many lines.  It's always OK to return repr(val), albeit this may be
        painfully long, but where something nicer is viable and reliable, it
        should be used.  Nicer forms should try to put the data on separate
        lines from any start and end details.\n"""

        if isinstance(val, tuple) and len(val) > 20:
            # Like tuple's repr, but using ',\n' in place of ', ':
            return '(\n' + ',\n'.join(map(repr, val)) + '\n)'

        if isinstance(val, basestring) and len(val) > 80 && '\n' in val:
            if isinstance(val, unicode): lead = 'u'
            else: lead = ''
            if "'" in val:
                if '"' not in val:
                    return lead + '"""\n%s"""' % val
            else: return lead + "'''\n%s'''" % val
            # The strings that'll match this all end in a newline already:
            # squeeze.eighty.findall() always ends in an empty string.

        return repr(val)

    def save(**what):
        assert self.prime or self.factor
        if not self.lock(False, True):
            raise IOError(self.__BLOCKED, 'File temporarily unwritable',
                          self._content_file)
        try:
            fd = open(self._content_file, 'w')
            try:
                if self.__doc__ is not self.__class__.__doc__:
                    fd.write('"""%s"""\n\n', self.__doc__)

                if not isinstance(self, CacheFile):
                    what['depth'] = self.depth

                try: gap = self._indices # save as tuple if present
                except AttributeError: assert not self.prime
                else: what['indices'] = (gap.start, len(gap))

                for k, v in what.items:
                    fd.write('%s = %s\n' % (k, self.__repr(v)))

            finally: fd.close()
        finally: self.unlock(False, True)
        # potentially: return size of file

class SubNode (Node):
    __gapinit = Node.__init__
    def __init__(self, parent, start, span, types):
        self.__gapinit(start, span)
        self.__up = parent
        self.prime, self.factor = 'P' in types, 'F' in types

    def lock(self, read=False, write=False):
        return self.root.lock(read, write)
    def unlock(self, read=False, write=False):
        return self.root.unlock(read, write)

    @property
    def parent(self, ig=None): return self.__up # read-only access
    @property
    def root(self, ig=None): return self.__up.root

    @lazyprop
    def indices(self, ig=None):
        return self.__up.indices.start + self._indices

    def _span(self, gap): return self.__up._span(gap + self.__up.start)

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, start, span, indices=None):
        self.__upinit(parent, start, span, '')
        if indices is not None: self._indices = indices

from study.snake.sequence import WeakTuple
class CacheDir (object):
    @lazyattr
    def _content_file(self, ig=None):
        return self.path('__init__.py')

    import re
    @lazyattr
    def __listing(self, ig=None, get=os.listdir, seq=Ordered,
                pat=re.compile(r'^([PF]+)([0-9a-z]+)\+([0-9a-z]+)(\.py)?$')):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                start, span = int(got.group(2), 36), int(got.group(3), 36)
                if got.group(4): klaz = CacheFile
                else: klaz = CacheSubDir
                ans.append((start, span, name, got.group(1), klaz))

        assert ans[0][0] == 0, 'Directory lacks initial sub-range'
        assert self.start + ans[-1][0] +ans[-1][1] == self.stop, \
               'Directory lacks final sub-range'

        return ans
    del re

    class WeakSeq (WeakTuple):
        __upinit = WeakTuple.__init__
        def __init__(self, cdir, getseq):
            def get(ind, s=cdir, g=getseq):
                start, size, name, mode, klaz = g(s)[ind]
                return klaz(s, name, start, size, mode)
            self.__upinit(get)
            self.__who, self.__att = cdir, getseq

        def __len__(self): return len(self.__att(self.__who))

    @lazyattr
    def listing(self, s, ig=None, W=WeakSeq):
        return W(self, lambda x: x.__listing)

    @lazyattr
    def primes(self, ig=None, W=WeakSeq, nice=lambda i: 'P' in i[3]):
        return W(self, lambda x: x.__listing.filter(nice))

    @lazyattr
    def factors(self, ig=None, W=WeakSeq, nice=lambda i: 'F' in i[3]):
        return W(self, lambda x: x.__listing.filter(nice))

    del WeakSeq

    @lazyprop
    def depth(self, ig=None): # but usuall we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1

    @staticmethod
    def __findex(seq, index):
        lo, lon, hi, hin = 0, seq[0]._indices, len(seq) - 1, seq[-1]._indices
        # Take care to reference ._indices on as few nodes as possible.
        if lon.stop > index: return lo
        if hin.start <= index: return hi
        while hi > lo + 1:
            assert lon.stop < index < hin.start
            # linear-interpolate an estimate at the prime with this index:
            try: mid = seq.index(seq[lo].stop +
                                 (seq[hi].start - seq[lo].stop) *
                                 (index - lon.stop) *
                                 1./(hin.start - lon.stop), lo, hi)
            except ValueError, what: mid = what.args[0]
            assert lo < mid < hi
            midn = seq[mid]._indices
            if midn.start > index: hi, hin = mid, midn
            elif midn.stop <= index: lo, lon = mid, midn
            elif index in midn: return mid
            else: raise ValueError(mid)

        assert hi == lo + 1
        assert seq[lo].stop < seq[hi].start # it's a gap
        assert lon.stop <= index < hin.start
        raise ValueError(hi)

    def __locate(self, seq, value, gap, index=None, Range=Gap, Hole=Interval):
        try:
            if value is None:
                assert index is not None and self.prime
                assert index in self._indices
                assert seq is self.primes
                ind = self.__findex(seq, index)
            else:
                ind = seq.index(value)

        except ValueError, what:
            ind = what.args[0] # where in seq our gap would be inserted
            assert ind > 0, 'Directory lacks initial or final sub-range'

            try:
                dex = None # in case all else fails
                dex = self._indices
                lox = seq[ind-1]._indices.stop
                assert lox is not None, 'Badly sorted intervals'
                dex = Hole(lox, seq[ind]._indices.start - lox)
            except AttributeError: pass # missing ._indices

            try: dex = dex.trim(gap.indices - self.indices.start)
            except AttributeError: pass # missing .indices

            lo = seq[ind-1].stop
            assert lo is not None, 'Badly sorted intervals'
            kid = Hole(lo, seq[ind].start - lo)
            if gap is not None: kid = kid.trim(gap)

            kid = Range(self, kid.start, len(kid), dex)

        else:
            kid = seq[ind]
            if isinstance(kid, CacheDir):
                if value is not None: value -= kid.start
                else: index -= kid._indices.start
                return kid.__locate(seq, value, gap, index)

            assert isinstance(kid, CacheFile)

        return kid

    def _get_primes(self, value, gap=None, index=None):
        """Internals for CacheRoot.get_primes

        Same args as get_primes, but value is divided by root's octet's
        .modulus\n"""

        return self.__locate(self.primes, value, gap, index)

    def _get_factors(self, value, gap=None):
        """Internals for CacheRoot.get_factors

        Same args as get_factors, but value (if given) is divided by root's
        octet's .modulus\n"""

        return self.__locate(self.factors, value, gap)

del WeakTuple

from lockdir import LockableDir

class CacheRoot (CacheDir, LockableDir, Node):
    __gapinit = Node.__init__
    __dirinit = LockableDir.__init__
    def __init__(self, path):
        self.__dir = path
        self.content # evaluate in order to force a .load()
        self.__gapinit(0, self.stop)
        self.__dirinit()

    @property
    def root(self, ig=None): return self
    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)

    def get_factors(self, value, gap=None):
        """Find a chunk or gap enclosing a designated integer.

        Arguments:
          value -- integer to be found in cache (required)
          gap -- optional Gap around value else (default) None

        Required first argument, value, is a natural number.  The return shall
        describe an interval in which that natural falls; if the return is obj,
        then obj.start <= value < obj.stop; if this cache has least proper
        factor data for the given natural, obj is an instance of CacheFile;
        otherwise, it is an instance of Gap. (recognizable as such by its lack
        of a .content).  In the latter case, if the optional second argument gap
        is supplied, the returned Gap is its intersection with the widest range
        of naturals about value in which self has no data.  When gap is None, it
        is treated as if it were Gap(self, 0, None).

        The return has, when available, a .indices attribute describing the
        range of prime indices it spans; however, the returned index interval
        may (e.g. in a factor-only cache, or when describing a gap) be wider
        than the actual interval described by the return, if this is the best
        available information.

        See also: get_primes().\n"""

        if gap is None: gap = Range(self, 0, None)
        if not self.factor: return gap
        return self._get_factors(self, value / self.octet.modulus, gap)

    def get_primes(self, value, gap=None, index=None, Range=Gap, Hole=Interval):
        """Find a cache file or gap enclosing a designated integer.

        Like get_factors (q.v.) except that it returns data on primes, and
        allows value to be None, in which case its (otherwise ignored and
        optional) third argument, index, must be supplied: in this case, index
        must be a natural and the effect is as if primes[index] had been passed
        as value - no-one can be expected to know primes[index] without calling
        this function to get the cache object that tells us !\n"""

        if gap is None: gap = Range(self, 0, None, Hole(0, None))

        if not self.prime: return gap
        if value is not None: value /= self.octet.modulus
        return self._get_primes(self, value, gap, index)

    __load = Node.load
    from octet import OctetType
    def load(self, bok=None, mode=OctetType):
        bok = self.__load(bok)
        self.stop = bok.pop('top')
        self.octet = mode(bok.pop('octet'))
        return bok
    del OctetType

    __save = Node.save
    def save(self, **what):
        what['top'] = self.stop
        what['octet'] = self.octet.primes
        return self.___save(**what)

    def _span(self, gap, Range=Interval):
        step = self.octet.modulus
        return Range(gap.start * step, len(gap) * step)

    @lazyprop
    def prime(self, ig=None): return len(self.primes) > 0
    @lazyprop
    def factor(self, ig=None): return len(self.factors) > 0
    @lazyprop
    def indices(self, ig=None): return self._indices

del LockableDir

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, parent, name, start, span, types):
        self.__gapinit(parent, start, span, types)
        self.__name = name

    def path(self, *tail): return self.parent.path(self.__name, *tail)

class CacheSubDir (CacheDir, CacheSubNode):
    pass

class CacheFile (CacheSubNode):
    @lazyattr
    def _content_file(self, ig=None): return self.path()

    @property
    def depth(self, ig=None): return 0

    def __spanner(self, val):
        up = self
        try:
            while val not in up:
                val += up.start
                up = up.parent

        except AttributeError: return None # root lacks this value

        if self.prime:
            assert not self.factor, 'I should be a file !'
            ans = up._get_prime(val)
        else:
            assert self.factor
            ans = up._get_factor(val)

        if isinstance(ans, CacheFile): return ans
        return None

    @lazyattr
    def next(self, ig=None):
        """Successor node to this one, if in this cache; else None."""
        return self.__spanner(len(self))

    @lazyattr
    def prev(self, ig=None):
        """Prior node to this one, if in this cache; else None."""
        return self.__spanner(-1)

del Interval, Ordered, weakattr, lazyattr, lazyprop
from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

class WriteCacheDir (CacheDir):
    def addnode(self, child):
        pass

    def move(self, foster):
        pass

class WriteCacheRoot (WriteCacheDir, CacheRoot):
    pass

class WriteCacheSubDir (WriteCacheDir, CacheSubDir):
    pass

class WriteCacheFile (CacheFile):
    pass

del os
