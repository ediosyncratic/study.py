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

$Id: cache.py,v 1.11 2008-07-11 12:16:54 eddy Exp $
"""

import os
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from study.snake.property import weakattr, lazyattr, lazyprop

class Node (Interval):
    """Base class for cache nodes
    """

    class Bok (dict): pass # for weakref's sake
    def load(bok=None, glo={}, Range=Interval, Dict=Bok):
        # TODO: need a lock-check on all ancestors
        # TODO: want to be able to GET a URL instead of reading a file

        if bok is None: bok = Bok()
        execfile(self._content_file, glo, bok) # may raise IOError, ParseError

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
        # potentially: return size of file

class SubNode (Node):
    __gapinit = Node.__init__
    def __init__(self, parent, start, span, types):
        self.__gapinit(start, span)
        self.__up = parent
        self.prime, self.factor = 'P' in types, 'F' in types

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

class LockableDir (object):
    """Lock management base-class.

    This isolates lock management from the rest of cache directory management.
    The locking implemented here is recursive: if you already hold a lock,
    locking it again is a successful no-op and the matching unlock (which is
    required) shall also be a no-op.  The locking is also conservative: if in
    doubt about whether it can succeed, it fails.  On failure, it endeavours to
    report the process ID of the contending process and the file that implied
    the contention, to facilitate manual intervention in the event of stale
    locks.

    Locking a directory for reading means locking it so that the current process
    may read it.  LIkewise, locking for writing means locking so as to be able
    to write.  A directory which is being written is not safe to read, except
    (if they're careful) by whoever is writing it; so a directory locked by
    someone else for reading cannot be locked for writing; and a directory
    locked by someone else for writing cannot be locked at all.  However, two
    processes may safely lock a directory for reading.  Thus the locking
    semantics needed match with those of POSIX flock (3) with writing as an
    exclusive lock and reading as a shared lock.

    It is left to derived classes to ensure that sub-directories lock and unlock
    their parents in suitable ways; and to implement .path(leafname) as the name
    of a file in the directory to be locked.\n"""

    def __init__(self):
        # Am *I* holding read/write locks ?  Not yet.
        self.__read = self.__write = self.__mode = 0

    def __del__(self):
        # I'm not sure this assertion is reliable, but let's give it a try:
        assert self.__read <= 0 and self.__write <= 0, 'why not ?'
        while (self.__read > 0 or self.__write > 0 and
               self.unlock(self.__read > 0, self.__write > 0)):
            pass

    import fcntl, errno
    def unlock(self, read=False, write=False,
               BLOCKS=errno.EWOULDBLOCK,
               EXCLUDE=fcntl.LOCK_EX, SHARE=fcntl.LOCK_SH):
        """Release locks.

        Arguments are as for .lock(), q.v.  Every call to .lock() should be
        matched by a call to .unlock() with the same arguments, except that when
        both are True they may be cleared by distinct calls to unlock().
        Typical usage should look like:

            if dir.lock(True):
                try: # ... do stuff ...
                finally: dir.unlock(True)

        with matching args to lock and unlock.\n"""

        # Shouldn't even ask to unlock if not actually locked:
        if write: assert self.__write > 0
        if read: assert self.__read > 0

        if write and self.__write == 1: unlock = EXCLUDE
        else: unlock = 0
        elif read and self.__read == 1: unlock |= SHARE

        if unlock:
            try: ok = self.__lock(clear=unlock)
            except IOError, what:
                assert what.errno == BLOCKS
                assert not "I didn't expect unlocking to be able to fail !"
                # so I may have failed to handle this failure properly ...
                return False
            if not ok: return False

        if write and self.__write > 0: self.__write -= 1
        if read and self.__read > 0: self.__read -= 1
        return True

    def lock(self, read=False, write=False, block=False,
             BLOCKS=errno.EWOULDBLOCK,
             EXCLUDE=fcntl.LOCK_EX, SHARE=fcntl.LOCK_SH):
        """See if this process can lock this directory.

        Arguments, read and write, are optional booleans (defaulting to False)
        selecting the kind of lock desired; at least one of them should be
        specified True.\n"""

        assert read or write, 'Fatuous call'

        if read and self.__read == 0: mode = SHARE
        else: mode = 0
        if write and self.__write == 0: mode |= EXCLUDE

        if mode:
            try: ok = self.__lock(block, mode)
            except IOError, what:
                assert what.errno == BLOCKS
                return False
            if not ok: return False

        if read: self.__read += 1
        if write: self.__write += 1
        return True

    __pid = os.getpid()
    def __lock(self, block=False, lock=0, clear=0,
               lock=fcntl.flock, exist=os.path.exists,
               touch=lambda n: open(n, 'w').close(),
               remove=os.remove, rename=os.rename,
               open=os.open, close=os.close, write=os.write,
               fdopen=os.fdopen,
               ENOENT=errno.ENOENT, NOBLOCK=os.O_NONBLOCK,
               EXCLUDE=fcntl.LOCK_EX, SHARE=fcntl.LOCK_SH,
               NOW=fcntl.LOCK_NB, UNLOCK=fcntl.LOCK_UN):

        if ((lock & EXCLUDE) or self.__write > 0) and not (clear & EXCLUDE):
            flag, mode = EXCLUDE, 'w'
        elif ((lock & SHARE) or self.__read > 0) and not (clear & SHARE):
            flag, mode = SHARE, 'r'
        elif not self.__mode: return True # Nothing to do
        else: flag, mode = UNLOCK, ''

        if self.__mode == flag: return True # Nothing to do
        if block: block = 0
        else: block = NOW

        # TODO: this needs further work

        # Should the lock file have my pid in it ?
        if flag & EXCLUDE: content = True # yes, and that's a change
        elif self.__mode & EXCLUDE: content = False # no, and that's a change
        else: content = None # no, but it currently doesn't anyway

        if content is None: # No need to change content:
            try: self.__fd
            except AttributeError: # initialize
                assert not self.__mode
                try:
                    fd = open(self.__file, NOBLOCK, mode)
                    fo = fdopen(fd, mode)
                except IOError, what:
                    if what.errno == ENOENT and mode == 'r':
                        touch(self.__file)
                        fd = open(self.__file, NOBLOCK, mode)
                    else: raise

                self.__fd, self.__mode = fo, UNLOCK

            lock(self.__fd, flag | block)

            old = None
        else:
            try: old = self.__fd
            except AttributeError: old = None

            tmpfile = self.__file + '.%d' % self.__pid
            fd = fo = None
            try:
                if content:
                    fd = open(tmpfile, NOBLOCK, 'w')
                    fo = fdopen(fd, 'w')
                    fo.write(str(self.__pid))
                    fo.flush()
                else:
                    touch(tmpfile)
                    fd = open(tmpfile, NOBLOCK, 'r')
                    fo = fdopen(fd, 'r')
                lock(fd, flag | block)
                was = self.__check()
                assert content or (was and int(was) == self.__pid), \
                       (content, was, self.__pid)
                rename(tmpfile, self.__file)
            except:
                try:
                    if fo is not None: fo.close()
                    elif fd is not None: close(fd)
                finally: remove(tmpfile)
                raise

            self.__fd = fo

        self.__mode = flag
        if old is not None: old.close()

    # Prepare tool for __check:
    rival = getattr(os, 'getsid', None)
    if rival is None: # No getsid; can't tell if pid is live or not.
        def rival(pair, pid=__pid): return pair[0] != pid
    else: # getsid available: be more agressive about ignoring stale locks:
        def rival(pair, pid=__pid, sid=rival, ESRCH=errno.ESRCH,
                  remove=os.remove, OSError=os.error):
            """Test whether a (pid, file) pair indicates contention.

            Sole argument, pair, is a (pid, file) pair; if the pid is self's
            pid, or indicates a dead process, then we can ignore it; otherwise
            it indicates a rival process holding or trying to acquire the lock.

            When the given pair's pid is dead, we try to remove the lockfile
            named by the second member of the pair, to save wasted testing in
            later checks.  However, if deletion fails, we print a warning and
            ignore the error: if the file no longer exists, it's no problem; if
            it's a temporary of a dead process, it shall never be renamed to
            become a live lock, so is no problem; otherwise, it's a live lock
            and __lock()'s attempt to over-write it (immediately after calling
            __check) shall fail, so there's no point failing here.\n"""

            if pair[0] == pid: return False
            try: sid(pair[0])
            except IOError, what:
                if what.errno == ESRCH:
                    # Stale lock file: tidy away, if possible.
                    try: remove(pair[1])
                    except OSError:
                        print 'Failed to remove stale lock file', pair[1]
                    return False
                # else: some other error; process exists; honour its lock file.
            return True

    import re
    def __check(self,
                exist=os.path.exists, get=os.path.listdir,
                sid=rival, BLOCKS=errno.EWOULDBLOCK,
                pat=re.compile(r'^\.lock\.(\d+)')):
        """Check lock state.

        Raises IOError if someone else currently holds the lock file or it's
        empty (unheld) but someone else is in the act of locking it.  Should
        only be called after creating our own temporary file in preparation for
        locking it, so that anyone else in the act of locking shall fail along
        with us.\n"""

        ids = []
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                ids.append((int(got.group(1)), self.path(name)))

        if exist(self.__file):
            fd = open(self.__file)
            got = fd.read()
            if got: ids = (int(got), self.__file)
        else: got = ''

        ids = filter(sid, ids)
        if ids:
            raise IOError(BLOCKS,
                          'Lock contention: ' +
                          ', '.join(map('pid %d (%s)'.__mod__, ids)),
                          ids[0][1])
        return got

    del rival, re, fcntl, errno

    @lazyattr
    def __file(self, ig=None): return self.path('.lock')

class CacheDir (LockableDir):
    @lazyattr
    def _content_file(self, ig=None):
        return self.path('__init__.py')

    import re
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

    del re

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

class CacheRoot (CacheDir, Node):
    __gapinit = Node.__init__
    __dirinit = CacheDir.__init__
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

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, parent, name, start, span, types):
        self.__gapinit(parent, start, span, types)
        self.__name = name

    def path(self, *tail): return self.parent.path(self.__name, *tail)

class CacheSubDir (CacheDir, CacheSubNode):
    __gapinit = CacheSubNode.__init__
    __dirinit = CacheDir.__init__
    def __init__(self, *args, **what):
        self.__gapinit(*args, **what)
        self.__dirinit()

class CacheFile (CacheSubNode):
    @weakattr
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
    def passdown(self, child):
        pass

class WriteCacheRoot (WriteCacheDir, CacheRoot):
    pass

class WriteCacheSubDir (WriteCacheDir, CacheSubDir):
    pass

class WriteCacheFile (CacheFile):
    pass

del os
