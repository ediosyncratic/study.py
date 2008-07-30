"""Master object controlling primes list.

$Id: master.py,v 1.1 2008-07-30 07:26:16 eddy Exp $
"""
import os, cache

class Master (object):

    def writedir(name,
                 exist=os.path.exists,
                 makedirs=os.makedirs,
                 WriteCacheRoot=cache.WriteCacheRoot):
        if not exist(name): makedirs(name) # may raise OSError
        root = WriteCacheRoot(name)
        if root.lock(write=True):
            if not root.lock(read=True):
                assert 'Ouch - broken lockery'
            root.unlock(write=True)
            return root

        raise IOError

    def readpath(names, seen, kind,
                 CacheRoot=cache.CacheRoot):
        out = []
        for name in names:
            if name in seen: continue
            seen.append(name)
            try:
                r = CacheRoot(name)
                if not getattr(r, kind): continue
                if r.lock(read=True): r.unlock(read=True)
            except (IOError, OSError): continue
            else: out.append(r)

        return tuple(out)

    def __init__(self,
                 pwrite=None,
                 fwrite=None,
                 pread=None,
                 fread=None,
                 memsize=0x1000000, # 16 MB
                 disksize=0x100000, # 1 MB
                 pathsep=os.pathsep,
                 writedir=writedir,
                 readpath=readpath,
                 env=os.environ,
                 study=None,
                 home=os.curdir,
                 OSError=os.error,
                 join=os.path.join,
                 real=os.path.realpath,
                 abspath=os.path.abspath,
                 exist=os.path.exists):
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
        pread = map(real, map(abspath, filter(exist, pathsep.split(pread))))
        if fread is None: fread = env.get('STUDY_FACTOR_PATH', '')
        fread = map(real, map(abspath, filter(exist, pathsep.split(fread))))
        seen = []

        if pwrite is None: pwrite = env.get('STUDY_PRIME_DIR', None)
        if not pwrite: pwrite = join(study, 'prime')
        pwrite = real(abspath(pwrite))
        try: self.__prime_root = writedir(pwrite)
        except (OSError, IOError):
            if pwrite not in pread: pread.insert(0, pwrite)
        else: seen.append(pwrite)

        if fwrite is None: fwrite = env.get('STUDY_FACTOR_DIR', None)
        if not fwrite: fwrite = join(study, 'factor')
        fwrite = real(abspath(fwrite))
        try: self.__factor_root = writedir(fwrite)
        except (OSError, IOError):
            if fwrite not in fread: fread.insert(0, fwrite)
        else: seen.append(fwrite)

        self.__prime_path = readpath(pread + fread, seen[:], 'prime')
        self.__factor_path = readpath(fread + pread, seen, 'factor')

    del writedir, readpath
