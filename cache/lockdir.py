"""Lockable directory base-class.

Used by whole.py but isolated because intrinsically independent.

See study.LICENSE for copyright and license information.
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
    may read it.  Likewise, locking for writing means locking so as to be able
    to write.  A directory which is being written is not safe to read, except
    (if they're careful) by whoever is writing it; so a directory locked by
    someone else for reading cannot be locked for writing; and a directory
    locked by someone else for writing cannot be locked at all.  However, two
    processes may safely lock a directory for reading.  Thus the locking
    semantics needed match with those of POSIX flock (3) with writing as an
    exclusive lock and reading as a shared lock.

    It is left to derived classes to ensure that sub-directories lock and unlock
    their parents in suitable ways; and to implement .path() as the name of the
    directory itself, with .path(leafname) being the name of a file in the
    directory to be locked.  This last name must be amenable to having a suffix
    like .42 added (for various values of 42) and still name a file in the same
    directory.\n"""

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

        # I'm not sure this precondition is reliable, but let's give it a try:
        finished = self.__read <= 0 and self.__write <= 0

        while ((self.__read > 0 or self.__write > 0) and
               self.unlock(self.__read > 0, self.__write > 0)):
            pass

        assert finished, 'Apparently we did need this method, after all'

    def unlock(self, read=False, write=False):
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

        r =  read and self.__read  == 1
        w = write and self.__write == 1
        if (r or w) and not self.__lock(not r and self.__read > 0,
                                        not w and self.__write > 0):
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
        specified True.  Specifying which are true in keyword form is likely
        clearer for anyone reading your code.\n"""

        assert read or write, 'Fatuous call'

        r =  read and self.__read  == 0
        w = write and self.__write == 0
        if r or w:
            if not self.__lock(r, w): return False

        if read: self.__read += 1
        if write: self.__write += 1
        return True

    # The rest of this class is private and doesn't mess with __read, __write.
    from property import lazyprop

    @lazyprop
    def __file(self): return self.path('.lock')

    del lazyprop
    import fcntl, errno

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

        if write: flag = EXCLUDE
        elif read: flag = SHARE
        elif not (self.__mode & ~UNLOCK): return True # Nothing to do
        else: flag = UNLOCK

        if self.__mode == flag: return True # Nothing to do

        try: return self.__lockf(flag)
        except IOError, what:
            if what.errno == BLOCKS:
                print 'Failed (un)lock: ', what.filename, what.args
                return False
            raise

    import os
    __pid = os.getpid()
    def __lockf(self, flag,
                touch=lambda n: open(n, 'w').close(),
                exist=os.path.exists, remove=os.remove, rename=os.rename,
                fdopen=os.fdopen, open=os.open, close=os.close,
                write=os.write, fsync=os.fsync, flock=fcntl.flock,
                ENOENT=errno.ENOENT, OSError=os.error,
                modes={
        fcntl.LOCK_EX: (os.O_WRONLY | os.O_NONBLOCK | os.O_CREAT, 'w'),
        fcntl.LOCK_SH: (os.O_RDONLY | os.O_NONBLOCK, 'r'),
        fcntl.LOCK_UN: ()}, NOW=fcntl.LOCK_NB,
                EXCLUDE=fcntl.LOCK_EX, UNLOCK=fcntl.LOCK_UN):
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

        # Remember old __fd so we can .close() it when we're done with it:
        try: old = self.__fd
        except AttributeError: old = None

        def open(file, mode=modes[flag], open=open, fdopen=fdopen):
            # An IndexError here indicates flag was UNLOCK: we shouldn't be
            # open()ing anything in that case.
            fd = open(file, mode[0], 0666)
            fo = fdopen(fd, mode[1])
            return fd, fo

        if content is None:
            if flag & UNLOCK: del self.__fd # closed later as old.close()
            else:
                if old is None: # initialize
                    assert (flag & UNLOCK) == 0 == (self.__mode & ~UNLOCK)
                    self.__fd = open(self.__file)[1]
                else:
                    old = None # don't close self.__fd

                flock(self.__fd.fileno(), flag | NOW)
        else:
            tmpfile = self.__file + '.%d' % self.__pid
            fd = fo = None
            try:
                if content:
                    assert flag & EXCLUDE
                    fd, fo = open(tmpfile)
                    fo.write(str(self.__pid))
                    fo.flush()
                    fsync(fd)
                else:
                    touch(tmpfile)
                    if flag & ~UNLOCK:
                        fd, fo = open(tmpfile)

                if fd is not None:
                    assert fo is not None and fd == fo.fileno()
                    flock(fd, flag | NOW)
                was = self.__check()
                assert not (self.__mode & EXCLUDE) or \
                       (was and int(was) == self.__pid), \
                       (content, was, self.__pid)
                rename(tmpfile, self.__file)

            except:
                try:
                    if fo is not None: fo.close()
                    elif fd is not None: close(fd)
                finally:
                    if exist(tmpfile): remove(tmpfile)
                raise

            if fo is not None: self.__fd = fo
            elif old is not None: del self.__fd

        self.__mode = flag
        # This may raise IOError, so do it after we've reached a sane state.
        if old is not None: old.close()
        return True

    # Tool for __check:
    def rival(pair, pid=__pid,
              open=os.open, close=os.close, remove=os.unlink,
              WRITE=os.O_WRONLY | os.O_NONBLOCK, ENOENT=errno.ENOENT,
              flock=fcntl.flock, contend=fcntl.LOCK_EX | fcntl.LOCK_NB):
        if pair[0] != pid:
            try:
                fd = None
                try:
                    fd = open(pair[1], WRITE, 0666)
                    flock(fd, contend)
                finally:
                    if fd is not None: close(fd)

                remove(pair[1]) # transient lock-file of a dead process
            except IOError, what:
                return what.errno != ENOENT

        return False

    import re
    def __check(self,
                exist=os.path.exists, get=os.listdir,
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
            if got: ids = [(int(got), self.__file)]
        else: got = ''

        ids = [x for x in ids if sid(x)]
        if ids:
            raise IOError(BLOCKS,
                          'Lock contention: ' +
                          ', '.join('pid %d (%s)' % x for x in ids),
                          ids[0][1])
        return got

    del rival, re, fcntl, errno, os
