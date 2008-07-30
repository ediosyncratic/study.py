"""Master object controlling primes list.

$Id: master.py,v 1.3 2008-07-30 19:27:32 eddy Exp $
"""
import os, cache

class Master (object):

    # Tool functions (not methods) used by __init__:
    def realabs(name, real=os.path.realpath, abspath=os.path.abspath):
        return real(abspath(name))

    def writedir(name,
                 exist=os.path.exists, makedirs=os.makedirs,
                 real=realabs, WriteCacheRoot=cache.WriteCacheRoot):
        name = real(name)
        if not exist(name): makedirs(name) # may raise OSError

        root = WriteCacheRoot(name)
        if root.lock(write=True):
            if not root.lock(read=True): assert False, 'Ouch - broken lockery'
            root.unlock(write=True)
            return root

        raise IOError

    def readroot(name, kind, CacheRoot=cache.CacheRoot):
        r = CacheRoot(name)
        if not getattr(r, kind): raise AttributeError
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
    def __init__(self,
                 pwrite=None,
                 fwrite=None,
                 pread=None,
                 fread=None,
                 memsize=0x1000000, # 16 MB
                 disksize=0x100000, # 1 MB
                 pathsep=os.pathsep,
                 env=os.environ,
                 study=None,
                 home=os.curdir,
                 join=os.path.join, OSError=os.error,
                 writedir=writedir, readpath=readpath):
        """Initialize master object.

        All arguments are optional:
          pwrite: modifiable prime cache directory or (default) None to use
                  $STUDY_PRIME_DIR; else '~/.study/prime/'
          pread: path of read-only cache directories or (default) None to use
                 $STUDY_PRIME_PATH if set, else ''
          fwrite: modifiable factor cache directory or (default) None to use
                  $STUDY_FACTOR_DIR if set, else '~/.study/factor/'
          fread: path of read-only factor cache directories or (default) None to
                 use $STUDY_FACTOR_PATH if set, else ''
          memsize: limit on (crudely-estimated) in-memory size (default: 16 MB)
                   of various data objects used in the primes infrastructure
                   (the OctetType object, the largest Huffman encoder for
                   factors).  May be violated by objects created in response to
                   existing cache files.
          disksize: approximate limit on size of cache files (default: 1 MB),
                    actual cache files may be a few kB over this limit.
          pathsep: separator used in the read-only paths, pread and fread,
                   between directory names (default: os.pathsep).
          env: environment (default: os.environ), a mapping in which to look up
               the defaults for [pf]{read,write}
          study: resource directory for the study package or (default) None to
                 use env['STUDYRC'] if set, else '~/.study'; used in place of
                 '~/.study' in the fall-back defaults above.
          home: fall-back to use for '~', in the default for study, if
                env['HOME'] is unset (default: os.curdir).

        Use of key-word calling is encouraged and passing more than nine
        positional arguments may lead to surprises and brokenness.  Note that
        memsize only affects decisions made by this master prime object (and its
        servants): objects needed in order to interact with existing caches
        shall be created even if they violate the memsize constraint wildly.

        If the value of either write directory, as specified above, is empty
        then the relevant sub-directory of study is used.  If a write directory
        is unwritable (because this process lacks necessary privileges, or
        because it is locked by another process) it is added to the read paths
        as if it had been included (as first entry, if not already present) in
        the read path corresponding to the write category for which it was
        specified; otherwise, if it was present in either path, it shall be
        removed from that path.  All directory names are mapped to canonical
        absolute paths when the instance is created, so any resolution of
        symbolic links is unaffected by subsequent changes in the file-system.
        Any directory in either read path that provides data of the kind sought
        by the other shall in fact be included at the end of the other, if not
        already present in it.

        Note that all defaults taken from os are read when the module containing
        this class is first loaded; for example, setting os.pathsep after that
        shall not affect the default used for pathsep (but changes in os.env
        shall take effect as long as os.env is the same mapping object as was
        saved here as env).\n"""

        self.__disk, self.__ram = disksize, memsize

        if pwrite and fwrite: pass
        elif study is None: study = join(env.get('HOME', home), '.study')

        if pread is None: pread = env.get('STUDY_PRIME_PATH', '')
        if fread is None: fread = env.get('STUDY_FACTOR_PATH', '')
        seen, pread, fread = [], pathsep.split(pread), pathsep.split(fread)

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

    del writedir, readpath
