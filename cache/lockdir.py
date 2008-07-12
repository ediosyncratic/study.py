"""Lockable directory base-class.

Used by cache.py but isolated due to size !

$Id: lockdir.py,v 1.4 2008-07-12 11:56:51 eddy Exp $
"""

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
    of a file in the directory to be locked.  This name must be amenable to
    having a suffix like .42 added (for various values of 42) and still name a
    file in the same directory.\n"""

    def __init__(self):
        # Am *I* holding read/write locks ?  Not yet.
        self.__read = self.__write = self.__mode = 0

    def __del__(self):
        """When garbage-collected, unlock.

        Cache directories shall mostly be held only via weakref, so shall tend
        to get garbage-collected; so they need to release their locks.  They
        should probably only ever get garbage-collected after all locks on them
        have been released anyway, but better safe than sorry ...

        Unlocking is done methodically via calls to .unlock() rather than by
        bypassing the count-downs and calling .__lock(UNLOCK), so that derived
        classes can do complex things with directory hierarchies, e.g. locking
        parent directories, without ill effects as long as they've correctly
        over-ridden lock and unlock.\n"""

        # I'm not sure this assertion is reliable, but let's give it a try:
        assert self.__read <= 0 and self.__write <= 0, 'why not ?'

        while (self.__read > 0 or self.__write > 0 and
               self.unlock(self.__read > 0, self.__write > 0)):
            pass

    from study.snake.property import lazyattr

    @lazyattr
    def __file(self, ig=None): return self.path('.lock')

    del lazyattr
    import fcntl, errno

    def unlock(self, read=False, write=False,
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

        if write and self.__write == 1: clear = EXCLUDE
        else: clear = 0
        elif read and self.__read == 1: clear |= SHARE

        if clear:
            w = self.__write > 0 and not (clear & EXCLUDE)
            r = self.__read > 0 and not (clear & SHARE)
            if not self.__lock(r, w):
                assert not "I didn't expect UNlocking to be able to fail !"
                # ... and I may have failed to handle such failure properly ...
                return False

        if write and self.__write > 0: self.__write -= 1
        if read and self.__read > 0: self.__read -= 1
        return True

    def lock(self, read=False, write=False):
        """See if this process can lock this directory.

        Arguments, read and write, are optional booleans (defaulting to False)
        selecting the kind of lock desired; at least one of them should be
        specified True.\n"""

        assert read or write, 'Fatuous call'

        if read and self.__read == 0: r = True
        if write and self.__write == 0: w = True

        if r or w:
            if not self.__lock(r, w): return False

        if read: self.__read += 1
        if write: self.__write += 1
        return True

    def __lock(self, read=False, write=False,
               EXCLUDE=fcntl.LOCK_EX, SHARE=fcntl.LOCK_SH, UNLOCK=fcntl.LOCK_UN,
               BLOCKS=errno.EWOULDBLOCK):
        """Lock management.

        Requires two boolean arguments, read and write; each should be True
        precisely if we need the so-named kind of access turned on.  If both are
        set, write takes precedence.

        The two bit-fields are made from the flags LOCK_SH and LOCK_EX provided
        by the fcntl module.  If clear is non-zero, the locking it indicates is
        released or, if .__read and .__write indicate the need for it, changed
        to the other kind of locking.

        The details are handled by internal __lockf(); this method is just a
        wrapper round it to digest arguments and handle the anticipated
        exception.\n"""

        if write: flag, mode = EXCLUDE, 'w'
        elif read: flag, mode = SHARE, 'r'
        elif not (self.__mode & ~UNLOCK): return True # Nothing to do
        else: flag, mode = UNLOCK, ''

        if self.__mode == flag: return True # Nothing to do

        try: return self.__lockf(flag, mode)
        except IOError, what:
            if what.errno == BLOCKS: return False
            raise

    import os
    __pid = os.getpid()
    def __lockf(self, flag, mode,,
                touch=lambda n: open(n, 'w').close(),
                exist=os.path.exists, remove=os.remove, rename=os.rename,
                fdopen=os.fdopen, open=os.open, close=os.close,
                write=os.write, fsync=os.fsync, flock=fcntl.flock,
                ENOENT=errno.ENOENT, NOBLOCK=os.O_NONBLOCK,
                EXCLUDE=fcntl.LOCK_EX, SHARE=fcntl.LOCK_SH,
                NOW=fcntl.LOCK_NB, UNLOCK=fcntl.LOCK_UN):
        """Perform actual lock file management.

        When locked for writing, the lock file contains the process id of the
        locking process (which also owns an exclusive lock on it).  Otherwise,
        it is empty (or non-existent).

        When changing the content, a temporary file with the content is first
        prepared (and, if necessary, opened and locked), then renamed in to
        replace the prior file (if any).  Checking for contention (see __check)
        is done between preparing the new file and renaming, so that contending
        processes can see one anothers' temporary files in order to detect the
        contention.\n"""

        # Do we need to change content of lock file ?
        if flag & EXCLUDE: content = True # need to write my pid in it
        elif self.__mode & EXCLUDE: content = False # clear my pid from it
        elif not exist(self.__file): content = False # need to create it
        else: content = None # no change

        try: old = self.__fd
        except AttributeError: old = None

        if content is None:
            if flag & UNLOCK: del self.__fd
            else:
                if old is None: # initialize
                    assert (flag & UNLOCK) == 0 == (self.__mode & ~UNLOCK)
                    fd = open(self.__file, NOBLOCK, mode)
                    fo = fdopen(fd, mode)
                    self.__fd = fo

                flock(self.__fd.fileno(), flag | NOW)
                old = None # don't close self.__fd

        else:
            tmpfile = self.__file + '.%d' % self.__pid
            fd = fo = None
            try:
                if content:
                    fd = open(tmpfile, NOBLOCK, 'w')
                    fo = fdopen(fd, 'w')
                    fo.write(str(self.__pid))
                    fo.flush()
                    fsync(fd)
                else:
                    touch(tmpfile)
                    if mode:
                        fd = open(tmpfile, NOBLOCK, mode)
                        fo = fdopen(fd, mode)

                if fd is not None:
                    assert fo is not None and fd == fo.fileno()
                    flock(fd, flag | NOW)
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

            if fo is not None: self.__fd = fo
            elif old is not None: del self.__fd

        self.__mode = flag
        # This may raise IOError, so do it after we've reached a sane state.
        if old is not None: old.close()
        return True

    # Tool for __check:
    def rival(pair, pid=__pid,
              open=os.open, close=os.close, NOBLOCK=os.O_NONBLOCK,
              flock=fcntl.flock, contend=fcntl.LOCK_SH | fcntl.LOCK_NB):
        if pair[0] != pid:
            try:
                fd = None
                try:
                    fd = open(pair[1], NOBLOCK, 'w')
                    flock(fd, contend)
                finally:
                    if fd is not None: close(fd)
            except IOError:
                return True

        return False

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

    del rival, re, fcntl, errno, os
