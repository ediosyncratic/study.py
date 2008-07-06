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

$Id: cache.py,v 1.3 2008-07-06 12:11:59 eddy Exp $
"""

import os
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr, lazyattr, lazyprop

class Node (Interval):
    """Base class for cache nodes
    """

    class Bok (dict): pass # for weakref's sake
    def _read(name, bok=None, glo={}, Range=Interval, Dict=Bok):
        # TODO: need a lock-check on all ancestors

        if bok is None: bok = Bok()
        execfile(name, glo, bok) # may raise IOError, ParseError

        try: self.depth = bok.pop('depth')
        except KeyError: assert isinstance(self, CacheFile)

        try: gap = bok.pop('indices') # saved to file as twople
        except KeyError: pass
        else: self.__indices = Range(gap[0], gap[1])

        return bok
    del Bok

    @lazyprop
    def _indices(self, ig=None): # Gap may have it set
        """Range of prime indices, relative to context."""
        try: return self.__indices
        except AttributeError:
            if not self.prime: raise
        self.content # evaluate to force loading (fails for Gap)
        return self.__indices # should now succeed

    def _write(name, **what):
        fd = open(name, 'w')
        try:
            if self.__doc__ is not self.__class__.__doc__:
                fd.write('"""%s"""\n\n', self.__doc__)

            if not isinstance(self, CacheFile):
                what['depth'] = self.depth

            if self.prime:
                try: gap = self._indices # save as tuple if present
                except AttributeError: pass
                else: what['indices'] = (gap.start, len(gap))

            for k, v in what.items:
                fd.write('%s = %s\n' % (k, repr(v)))

        finally: fd.close()
        # potentially: return size of file

class SubNode (Node):
    __gapinit = Node.__init__
    def __init__(self, parent, start, span, types):
        self.__gapinit(start, span)
        self.__up = parent
        self.prime, self.factor = 'P' in types, 'F' in types

    @property
    def octet(self, ig=None): return self.__up.octet
    def path(self, *tail): return self.__up.path(*tail)

    @lazyprop
    def indices(self, ig=None):
        return self.__up.indices.start + self._indices

    @lazyattr
    def span(self, ig=None): return self._span(self)
    def _span(self, gap): return self.__up._span(gap + self.start)

    def _spanner(self, val): # for use by CacheFile
        up = self
        try:
            while val not in up:
                val += up.start
                up = up.__up

        except AttributeError: return None # root lacks this value

        if self.prime:
            assert not self.factor, 'I should be a file !'
            ans = up._get_prime(val)
        else:
            assert self.factor
            ans = up._get_factor(val)

        if isinstance(ans, CacheFile): return ans
        return None

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, start, span):
        self.__upinit(parent, start, span, '')

class CacheDir (object):
    @weakattr
    def content(self, ig=None):
        return self._read(self.path('__init__.py'))

    @weakattr
    def listing(self, ig=None, get=os.listdir, seq=Ordered,
                pat=re.compile(r'^([PF]+)([0-9a-z]+)\+([0-9a-z]+)(\.py)?$')):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                start, span = int(got.group(2), 36), int(got.group(3), 36)
                # TODO: what should we do when modifiable ?
                if got.group(4): klaz = CacheFile
                else: klaz = CacheSubDir
                ans.append(klaz(self, name, start, span, got.group(1)))

        assert ans[0].start == 0, 'Directory lacks initial sub-range'
        assert self.start + ans[-1].stop == self.stop, \
               'Directory lacks final sub-range'

        return ans

    @lazyprop
    def depth(self, ig=None): # but usuall we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1

    @weakattr
    def primes(self, ig=None):
        return self.listing.filter(lambda i: i.prime)

    @weakattr
    def factors(self, ig=None):
        return self.listing.filter(lambda i: i.factor)

    @staticmethod
    def __findex(seq, index):
        lo, lon, hi, hin = 0, seq[0].indices, len(seq) - 1, seq[-1].indices
        # Take care to reference .indices on as few nodes as possible.
        if lon.stop > index: return lo
        if hin.start <= index: return hi
        while hi > lo + 1:
            if lon.stop == index: return lo + 1
            assert lon.stop < index < hin.start
            # linear-interpolate an estimate at the prime with this index:
            try: mid = seq.index(seq[lo].stop +
                                 (seq[hi].start - seq[lo].stop) *
                                 (index - lon.stop) *
                                 1./(hin.start - lon.stop), lo, hi)
            except ValueError, what: mid = what.args[0]
            assert lo < mid < hi
            midn = seq[mid].indices
            if midn.start > index: hi, hin = mid, midn
            elif midn.stop <= index: lo, lon = mid, midn
            else: return mid

        assert hi == lo + 1
        assert seq[lo].stop < seq[hi].start # it's a gap
        assert lon.stop <= index < hin.start
        return hi

    def __locate(self, seq, value, gap, index=None, Range=Gap):

        try:
            if value is None:
                assert index is not None and self.prime
                assert index in self.indices
                assert seq is self.primes
                ind = self.__findex(seq, index)
            else:
                ind = seq.index(value)

        except ValueError, what:
            ind = what.args[0] # where in .factors our gap would be inserted
            assert ind > 0, 'Directory lacks initial or final sub-range'
            base = seq[ind-1].stop
            assert base is not None, 'Badly sorted intervals'
            kid = Range(base, seq[ind].start - base)
            if gap is not None:
                kid = gap.trim(kid)
                kid = Range(kid.start, len(kid))
            # ? TODO: provide kid._indices ?
        else:
            kid = seq[ind]
            if isinstance(kid, CacheDir):
                ans = kid._get_factors(value - kid.start, gap - kid.start)
                if isinstance(ans, CacheFile): return ans
                return ans + kid.start
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

class CacheRoot (CacheDir, Node):
    __gapinit = Node.__init__
    def __init__(self, path):
        self.__dir = path
        self.content # evaluate in order to force a _read
        self.__gapinit(0, self.stop)

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
        is treated as if it were Gap(0, None).

        See also: get_primes().\n"""

        # TODO: any special handling needed for value <= self.octet.primes[-1] ?
        if gap is None: gap = Range(self, 0, None)
        if not self.factor: return gap
        return self._get_factors(self, value / self.octet.modulus, gap)

    def get_primes(self, value, gap=None, index=None, Range=Gap):
        """Find a cache file or gap enclosing a designated integer.

        Like get_factors (q.v.) except that it returns data on primes, and
        allows value to be None, in which case its (otherwise ignored and
        optional) third argument, index, must be supplied: in this case, index
        must be a natural and the effect is as if primes[index] had been passed
        as value.\n"""

        # TODO: any special handling needed for value <= self.octet.primes[-1] ?
        if gap is None: gap = Range(self, 0, None)
        if not self.prime: return gap
        if value is not None: value /= self.octet.modulus
        return self._get_primes(self, value, gap, index)

    __upread = Node._read
    from octet import OctetType
    def _read(self, name, bok=None, mode=OctetType):
        bok = self.__upread(name, bok)
        self.stop = bok.pop('top')
        self.octet = mode(bok.pop('octet'))
        return bok
    del OctetType

    __upwrite = Node._write
    def _write(self, name, **what):
        what['top'] = self.stop
        what['octet'] = self.octet.primes
        return self._write(name, **what)

    def _span(self, gap, Range=Interval):
        assert self.start == 0
        step = self.octet.modulus
        return Range(gap.start * step, len(gap) * step)

    @lazyprop
    def prime(self, ig=None): return len(self.primes) > 0
    @lazyprop
    def factor(self, ig=None): return len(self.factors) > 0
    @lazyprop
    def indices(self, ig=None): return self._indices

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, parent, name, start, span, types):
        self.__gapinit(parent, start, span, types)
        self.__name = name

    __path = SubNode.path
    def path(self, *tail): return self.__path(self.__name, *tail)

class CacheSubDir (CacheDir, CacheSubNode):
    pass

class CacheFile (CacheSubNode):
    @weakattr
    def content(self, ig=None):
        return self._read(self.path())

    @property
    def depth(self, ig=None): return 0

    @lazyattr
    def next(self, ig=None):
        """Successor node to this one, if in this cache; else None."""
        return self._spanner(len(self))

    @lazyattr
    def prev(self, ig=None):
        """Prior node to this one, if in this cache; else None."""
        return self._spanner(-1)

del Interval, Ordered, weakattr, lazyattr, lazyprop
from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

del os
