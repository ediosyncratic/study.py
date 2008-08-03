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

 * The primes object needs to be able to take a (possibly only partially) sieved
   block and turn it into a node in the cache hierarchy.  It knows the sieved
   block's range of naturals, so can pass in an interval describing this and get
   back a WriteCacheFile representing it; see _WriteCacheDir.locate().

TODO: what difference does it make to the cache objects if they're a modifiable
cache ?  It affects whether things can be added, renamed, etc.

(Note: this is a good example of where classic single-inheritance falls down,
although ruby's version of it copes.)

TODO (later): what adaptations would be needed to make a general-purpose 'cache
of data about naturals' of which the following could be a specialization ?

$Id: cache.py,v 1.38 2008-08-03 09:40:44 eddy Exp $
"""
import os
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr, lazyattr, lazyprop

class Node (Interval):
    """Base class for cache nodes
    """

    from errno import EWOULDBLOCK as __BLOCKED

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

    def _indices(self, ig=None): # Gap may have it set
        """Range of prime indices, relative to context."""
        try: return self.__indices
        except AttributeError:
            if not self.prime: raise
        self.content # evaluate to force .load()ing (fails for Gap)
        return self.__indices # should now succeed
    _indices = lazyprop('_indices', _indices)

    class Bok (dict): pass # for weakref's sake
    def load(self, bok=None, glo={}, Range=Interval, Dict=Bok):
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

class WriteNode (Node):
    def save(self, **what):
        assert self.prime or self.factor
        if not self.lock(write=True):
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
        finally: self.unlock(write=True)
        # potentially: return size of file

    @staticmethod
    def __repr(val):
        """Decides how to display values

        While meta-data is mostly terse, the actual strings encoding tuples of
        factors and prime octets are very long, so should be split across many
        lines.  It's always OK to return repr(val), albeit this may be painfully
        long, but where something nicer is viable and reliable, it should be
        used.  Nicer forms should try to put the data on separate lines from any
        start and end details.  The actual display hacks used are:
         * an overt tuple is displayed as normal but using '\n' rather than
           space after the commas between items, with '\n' between opening and
           closing parentheses and adjacent values; likewise for lists.
         * any overt string with newlines in it and a length over 80 characters
           is displayed as a triply-quoted string, unescaping newlines in its
           contents.  Note that self.__doc__ (if any) is also handled as a
           triply-quoted string, but separately from this hack.
        More may be added as experience illuminates what's worth doing.\n"""

        if isinstance(val, tuple) and len(val) > 20:
            # Like tuple's repr, but using ',\n' in place of ', ':
            return '(\n' + ',\n'.join(map(repr, val)) + '\n)'

        if isinstance(val, list) and len(val) > 20:
            # Like list's repr, but using ',\n' in place of ', ':
            return '[\n' + ',\n'.join(map(repr, val)) + '\n]'

        if isinstance(val, basestring) and len(val) > 80 and '\n' in val:
            txt = repr(val).replace('\\n', '\n')
            if txt[0] == 'u': head, txt = txt[0], txt[1:]
            else: head = ''
            assert txt[0] == txt[-1]
            txt, tail = txt[1:-1], 3 * txt[-1]
            head += tail + '\n'
            return head + txt + tail
            # The strings that'll match this all end in a newline already:
            # squeeze.eighty.findall() always ends in an empty string.

        return repr(val)

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

    def indices(self, ig=None):
        return self.__up.indices.start + self._indices
    indices = lazyprop('indices', indices)

    def _span(self, gap): return self.__up._span(gap + self.__up.start)

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, start, span, indices=None):
        self.__upinit(parent, start, span, '')
        if indices is not None: self._indices = indices

class WriteSubNode (SubNode, WriteNode):
    __upsave = WriteNode.save
    def save(self, **what):
        self.__upsave(**what)
        run = self
        while run is not self.root:
            run = run.parent
            run.changed = True

from study.snake.sequence import WeakTuple
class _CacheDir (object):
    @lazyattr
    def _content_file(self, ig=None):
        return self.path('__init__.py')

    import re
    @lazyattr
    def __listing(self, ig=None, get=os.listdir, seq=Ordered,
                pat=re.compile(r'^([0-9a-z]+)([PF]+)([0-9a-z]+)(\.py)?$')):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                start, span = int(got.group(1), 36), int(got.group(3), 36)
                assert not got.group(4) or len(got.group(2)) == 1, 'mixed file'
                ans.append((start, span, name, got.group(2), got.group(4)))

        return ans
    del re

    def _onchange(self):
        # All lazyattr: so del raises no AttributeError when absent.
        del self.__listing, self.listing, self.primes, self.factors, self.depth

    @staticmethod
    def _child_class_(isfile):
        if isfile: return CacheFile
        return CacheSubDir

    class WeakSeq (WeakTuple):
        __upinit = WeakTuple.__init__
        def __init__(self, cdir, getseq):
            def get(ind, s=cdir, g=getseq):
                start, size, name, mode, isfile = g(s)[ind]
                klaz = s._child_class_(isfile)
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

    def depth(self, ig=None): # but usually we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1
    depth = lazyprop('depth', depth)

    @staticmethod
    def __findex(seq, index):
        lo, lon, hi, hin = 0, seq[0]._indices, len(seq) - 1, seq[-1]._indices
        # Take care to reference ._indices on as few nodes as possible.
        # FIXME: drop implicit assumption that seq[0] and seq[-1] abut self's ends
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

    def __locate(self, seq, value, gap, index=None,
                 Range=Gap, Hole=Interval):
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
            # FIXME: no longer believe in reaching all the way to the ends
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
            if isinstance(kid, _CacheDir):
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

from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

class _WriteCacheDir (_CacheDir):
    changed = False # see tidy() and WriteSubNode.save()
    # set on instances when they do change; cleared by tidy.

    # FIXME: in a mixed cache, when adding nodes at start or end, both prime and
    # factor data must reach to the adjusted end-point; so we can't add a single
    # file at start or end, only in a gap in the middle.  This implies a need to
    # add prime and factor nodes simultaneously, possibly several of each.
    # Resolution: drop the requirement that a directory must contain data all
    # the way up to its ends.

    # An unfinished sieve node is a factor file, of sorts.  However, it should
    # probably only be added in a gap or as the root's tail.

    def locate(self, range, prime=False):
        """Find where to create a new file to be added.

        Required first argument is a range of naturals, in units of
        self.root.octet.modulus, relative to the start of the interval self
        describes.  Optional second argument, prime, defaults to False,
        indicating that the file to be created shall contain factor information;
        pass prime=True to get a file that shall contain prime information.

        Returns a new WriteFile in self or a sub-directory of self, such that
        saving data describing the given interval via this WriteFile shall
        extend the cache.  Once a coherent set of such additions is complete,
        the root of the cache should be told to .tidy() itself.\n"""
        # TODO: implement
        raise NotImplementedError

    def tidy(self):
        if not self.changed: return # nothing to do
        for node in self.listing:
            if isinstance(node, _WriteCacheDir) and node.changed:
                node.tidy()
        self._onchanged()
        # TODO: implement
        raise NotImplementedError
        del self.changed # to expose the class value, False
        assert not self.changed

    @staticmethod
    def _child_class_(isfile): # configure __listing
        if isfile: return WriteCacheFile
        return WriteCacheSubDir

    @lazyattr
    def __namelen(self, ig=None, fmt=nameber.encode):
        # len(self) is an upper bound on the .start of children
        return len(fmt(len(self)))

    def __child_name(self, child, fmt=nameber.encode):
        """Determine name of a child.

        Single argument is an interval of the naturals, encoded as by a SubNode
        of self; that is, relative to the start of self's range, in units of the
        .octet.modulus of our .root node.  Returns a name matching the regex of
        __listing and padded with enough leading 0s to ensure lexical sorting.\n"""

        name = fmt(child.start)
        gap = self.__namelen - len(name)
        if gap > 0: name = '0' * gap + name

        assert child.prime or child.factor
        if child.prime: name += 'P'
        if child.factor: name += 'F'

        name += fmt(len(child))
        if isinstance(child, CacheFile): name += '.py'
        return name

del nameber

from lockdir import LockableDir

class CacheRoot (_CacheDir, LockableDir, Node):
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

    def _span(self, gap, Range=Interval):
        step = self.octet.modulus
        return Range(gap.start * step, len(gap) * step)

    @lazyattr
    def prime(self, ig=None): return len(self.primes) > 0
    @lazyattr
    def factor(self, ig=None): return len(self.factors) > 0
    @property
    def indices(self, ig=None): return self._indices

    __load = Node.load
    from octet import OctetType
    def load(self, bok=None, mode=OctetType):
        bok = self.__load(bok)
        self.stop = bok.pop('top')
        self.octet = mode(bok.pop('octet'))
        return bok
    del OctetType

del LockableDir

class WriteCacheRoot (_WriteCacheDir, CacheRoot, WriteNode):
    __save = WriteNode.save
    def save(self, **what):
        what['top'] = self.stop
        what['octet'] = self.octet.primes
        return self.___save(**what)

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, parent, name, start, span, types):
        self.__gapinit(parent, start, span, types)
        self.__name = name

    def path(self, *tail): return self.parent.path(self.__name, *tail)

class CacheSubDir (_CacheDir, CacheSubNode):
    pass

class WriteCacheSubDir (_WriteCacheDir, CacheSubDir, WriteSubNode):
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
        return None # don't return a gap

    @lazyattr
    def next(self, ig=None):
        """Successor node to this one, if in this cache; else None."""
        return self.__spanner(len(self))

    @lazyattr
    def prev(self, ig=None):
        """Prior node to this one, if in this cache; else None."""
        return self.__spanner(-1)

    __load = CacheSubNode.load
    def load(self, bok=None):
        bok = self.__load(bok)
        assert not self.prime  or filter(lambda k: k[:5] == 'prime',  bok.keys())
        assert not self.factor or filter(lambda k: k[:6] == 'factor', bok.keys())
        return bok

class WriteCacheFile (CacheFile, WriteSubNode):
    __save = WriteSubNode.load
    def save(self, **what):
        assert not self.prime  or filter(lambda k: k[:5] == 'prime',  what.keys())
        assert not self.factor or filter(lambda k: k[:6] == 'factor', what.keys())
        return self.__save(**what)

del Node, SubNode, CacheSubNode, WriteNode, WriteSubNode
del Interval, weakattr, lazyattr, lazyprop

class oldCache (object):
    """Iterator over an old-style cache.

    Earlier versions of this study package included a cruder study.maths.primes
    module, without factor information, with its own cache directory; this class
    makes it possible to digest one of those into a form which can be saved
    (with the help of octet.Chunker) into a new-style prime cache.  An instance
    only provides prime data; you'll still need to sieve using the new system in
    order to build up factor data, but this makes as much use of old data as
    possible.

    An instance is an iterator over primes found in the old cache, possibly
    yielding None after it has completed the contiguous range of the old cache;
    thereafter, it may leave out some primes.  The range of primes for which
    data is yielded may be limited by supplying start and stop parameters to the
    constructor (q.v.).\n"""

    def __iter__(self): return self
    def __init__(self, cdir, start=0, stop=None, os=os, List=Ordered):
        """Digest an old-style prime-only cache directory.

        Required arguments:
          cdir -- path name of an old-style cache directory

        Optional arguments:
          start -- inclusive lower bound on naturals (default: 0)
          stop -- exclusive upper bound on naturals or (default) None, meaning
                  unbounded\n"""

        row, self.__sparse = [], List(unique=True)
        for name in os.listdir(cdir):
            if name[:1] == 'c' and name[-3:] == '.py' and '-' in name[1:-3]:
                try: lo, hi = map(int, name[1:-3].split('-'))
                except ValueError:
                    print 'ignored malformed name', name, 'in old cache', cdir
                else: row.append((lo, hi, os.path.join(cdir, name)))
        row.sort()

        self.__src = self.__primes(row, start, stop)

    def next(self): return self.__src.next()

    def __file(self, lo, hi, fd):
        quote, num = '', 0
        while True:
            if quote:
                off, bs = line.find(quote), 0
                while off > bs:
                    if line[off-1-bs] == '\\': bs += 1
                    elif bs % 2: off, bs = line.find(quote, off+1), 0
                    else: break

                if off < 0:
                    line = fd.readline()
                    num += 1
                    continue
                else:
                    off += len(quote)
                    line, quote = line[off:], ''
                    if not line.strip():
                        line = fd.readline()
                        num += 1
            else:
                line = fd.readline()
                num += 1

            if line.strip() == 'block = [':
                line = fd.readline()
                num += 1
                break
            elif line[:9] == 'sparse = ':
                self.__sparse += eval(line[8:])
            elif line[:5] == 'to = ': assert hi == int(line[4:]), (line, num)
            elif line[:5] == 'at = ': assert lo == int(line[4:]), (line, num)
            elif line[:3] in ('"""', "'''"):
                quote, line = line[:3], line[3:]
            elif line[:1] in ('"', "'"):
                quote, line = line[:1], line[1:]
            else:
                assert False, ('Unexpected line in old cache file', line, num)
                off, bs = line.find('"'), 0
                if off < 0 or "'" in line[:off]: off = line.find("'")
                while off > bs:
                    if line[off-1-bs] == '\\': bs += 1
                    elif bs % 2:
                        old = off+1
                        off, bs = line.find('"', old), 0
                        if off < 0 or "'" in line[old:off]:
                            off = line.find("'", old)
                    else: break

                if off >= 0:
                    # ignore line[:off]
                    if line[off] == line[off+1] == line[off+2]:
                        quote, line = line[off:off+3], line[off+3:]
                    else: quote, line = line[off], line[off+1:]
                # else simply ignore line

        while line.strip() != ']':
            if line:
                yield eval(line) # it should eval to a tuple
                line = fd.readline()
                num += 1
            else:
                assert False, ('Premature end of old cache file', num)
                break

        while line:
            line = fd.readline()
            num += 1
            assert not line.strip(), ('Stray content at end of old cache file',
                                      line, num)
        raise StopIteration

    def __primes(self, row, start, stop):
        it = 0
        for (lo, hi, name) in row:
            fd = open(name)
            # print "Processing", name
            try:
                for seq in self.__file(lo, hi, fd):
                    if seq[-1] < start: pass
                    elif stop is None or seq[-1] < stop:
                        for it in seq: yield it
                    else:
                        for it in seq:
                            if it < start: pass
                            elif stop is None or it < stop: yield it
                            else: raise StopIteration # discarding __sparse

            finally: fd.close()
            # print "Finished", name

        # Now deal with sparse.  First trim it:
        if stop is not None:
            try: ind = self.__sparse.index(stop)
            except ValueError, what: ind = what.args[0]
            if what >= 0: del self.__sparse[what:]

        if it < start: it = start # we never yielded anything !
        # Find how much, if any, of self.__sparse belongs after it:
        try: ind = self.__sparse.index(it)
        except ValueError, what:
            ind = what.args[0] # where Ordered.append would insert it
        else:
            if ind >= 0: ind += 1 # don't repeat it

        if ind >= 0: # -1 means "after last entry"
            if ind > 0: del self.__sparse[:ind]
            if self.__sparse:
                yield None
                for it in self.__sparse: yield it

        raise StopIteration

del os, Ordered
