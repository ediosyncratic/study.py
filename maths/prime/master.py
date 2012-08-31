"""Master object controlling primes list.

See study.LICENSE for copyright and license information.
"""
import os
from study.maths.prime import cache

class Master (object):

    # Tool functions (not methods) used by __init__:
    def realabs(name, real=os.path.realpath, abspath=os.path.abspath):
        return real(abspath(name))

    def writedir(name,
                 exist=os.path.exists, makedirs=os.makedirs,
                 real=realabs, WriteRoot=cache.WriteRoot):
        name = real(name)
        if not exist(name): makedirs(name) # may raise OSError

        root = WriteRoot(name)
        if root.lock(write=True):
            try: 
                if not root.lock(read=True):
                    assert False, 'Ouch - broken lockery'
            finally: root.unlock(write=True)
            return root

        raise IOError

    def readroot(name, kind, CacheRoot=cache.CacheRoot):
        r = CacheRoot(name)
        if not getattr(r, kind): raise AttributeError
        # Provoke early OSError if use of this directory is problematic (but
        # being locked is no problem):
        if r.lock(read=True): r.unlock(read=True)
        return r

    def readpath(names, write, seen, kind,
                 readroot=readroot, real=realabs,
                 exist=os.path.exists, OSError=os.error):
        if write in seen: write, seen = None, [ real(write) ]
        else: write, seen = real(write), []

        out = []
        for name in names:
            if not exist(name): continue
            name = real(name)
            if name in seen: continue
            elif name == write: write = None
            seen.append(name)
            try: out.append(readroot(name))
            except (IOError, OSError, AttributeError,): pass

        if write is not None:
            # default position for non-writable write-root is as first read-root:
            try: out.insert(0, readroot(write))
            except (IOError, OSError, AttributeError,): pass

        return tuple(out)

    del readroot, realabs
    from study.snake.sequence import Ordered
    def __init__(self,
                 pwrite=None,
                 fwrite=None,
                 pread=None,
                 fread=None,
                 memsize=0x1000000, # 16 MB total
                 disksize=0x100000, # 1 MB / file
                 pathsep=os.pathsep,
                 env=os.environ,
                 study=None,
                 home=os.curdir,
                 # Tunnels:
                 join=os.path.join, OSError=os.error,
                 writedir=writedir, readpath=readpath,
                 List=Ordered):
        """Initialize master object.

        All arguments are optional:
          pwrite: modifiable prime cache directory or (default) None to use
                  $STUDY_PRIME_DIR; else 'prime/' sub-dir of study
          pread: path of read-only cache directories or (default) None to use
                 $STUDY_PRIME_PATH if set, else ''
          fwrite: modifiable factor cache directory or (default) None to use
                  $STUDY_FACTOR_DIR if set, else 'factor/' sub-dir of study
          fread: path of read-only factor cache directories or (default) None to
                 use $STUDY_FACTOR_PATH if set, else ''
          memsize: limit on (crudely-estimated) in-memory size (default: 16 MB)
                   of various data objects used in the primes infrastructure
                   (the OctetType object, the largest Huffman encoder for
                   factors).  May be violated by objects created in response to
                   existing cache files.  Note that several objects of this size
                   are apt to be created, in any case.
          disksize: approximate limit on size of cache files (default: 1 MB),
                    actual cache files may be a few kB over this limit.
          pathsep: separator used in the read-only paths, pread and fread,
                   between directory names (default: os.pathsep).
          env: environment (default: os.environ), a mapping in which to look up
               the defaults for [pf]{read,write}
          study: resource directory for the study package or (default) None to
                 use env['STUDYRC'] if set, else '.study' sub-directory of home;
                 used in defaults for pwrite and fwrite.
          home: parent directory in which to look for study if env['HOME'] is
                unset (default: os.curdir, at the time this module was loaded).

        Notes:

          * This API is very likely to change (it has far too many parameters),
            for example to take most of the data from a configuration file in
            $STUDYRC (by default).
          * Use of key-word calling is encouraged (if only so as to ensure that
            you notice any change to the API, by provoking an error until you
            change your calls); in any case, passing more positional arguments
            than the ten described above may lead to surprises and brokenness.
          * memsize only affects decisions made by this master prime object (and
            its servants): objects needed in order to interact with existing
            caches shall be created even if they violate the memsize constraint
            wildly.
          * All defaults taken from os are read when python loads the module
            containing this class; for example, setting os.pathsep after that
            shall not affect the default used for pathsep (but changes in
            os.environ shall take effect as long as os.environ is the same
            mapping object as was saved here as the default for env).
          * All directory names are mapped to canonical absolute paths when the
            instance is created: any resolution of symbolic links is unaffected
            by subsequent changes in the file-system; changes to the environment
            variables won't change which cache directories are in use.
          * If the value of either write directory, as specified above, is empty
            then the relevant sub-directory of study is used.
          * The write directories are read-locked for the lifetime of the Master
            object, so as to prevent any other process from using them as write
            directory (this may be excessive, so may change at a later
            revision).
          * If a write directory is unwritable (because this process lacks
            necessary privileges, or because it is in use by another process) it
            is added to the read paths as if it had been included (as first
            entry, if not already present) in the read path corresponding to the
            write category for which it was specified; otherwise, if it was
            present in either path, it shall be removed from that path.
          * Any directory, in either read path, that provides data of the kind
            sought by the other shall in fact be included at the end of the
            other, if not already present in it.\n"""

        self.__disk, self.__ram = disksize, memsize
        self.__chunks = List(unique=None) # treat attempted duplication as error

        if pwrite and fwrite: pass
        elif study is None: study = join(env.get('HOME', home), '.study')

        if pread is None: pread = env.get('STUDY_PRIME_PATH', '')
        if fread is None: fread = env.get('STUDY_FACTOR_PATH', '')
        seen, pread, fread = [], pread.split(pathsep), fread.split(pathsep)

        if pwrite is None: pwrite = env.get('STUDY_PRIME_DIR', None)
        if not pwrite: pwrite = join(study, 'prime')
        try: self.__prime_root = writedir(pwrite)
        except (OSError, IOError): pass
        else: seen.append(pwrite)

        if fwrite is None: fwrite = env.get('STUDY_FACTOR_DIR', None)
        if not fwrite: fwrite = join(study, 'factor')
        try: self.__factor_root = writedir(fwrite)
        except (OSError, IOError): pass
        else: seen.append(fwrite)

        self.__prime_path = readpath(pread + fread, pwrite, seen, 'prime')
        self.__factor_path = readpath(fread + pread, fwrite, seen, 'factor')

    del writedir, readpath, Ordered

    def __del__(self):
        try: self.__prime_root.unlock(read=True)
        except AttributeError: pass
        try: self.__factor_root.unlock(read=True)
        except AttributeError: pass

    # TODO: write methods that put all those directory tree objects to use

del os, cache
