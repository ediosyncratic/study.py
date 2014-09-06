"""Lazy module loading hierarchy.

The idea here is to instantiate LazyMod on a directory and get back a
module-like object except that, on trying to access a non-existent attribute of
that object, or any derived in the following way from it, we attempt to find a
sub-module to import.  If the attribute name matches a *.py file in the
directory, we load it; if it matches a sub-directory we load it as a LazyMod,
recursively.  The constructor takes a filename, defaulting to __init__.py, that
is loaded to give the 'raw' namespace of the object loaded; this is propagated
to recursively-generated instances.  Thus, if a directory hieararchy has a bunch
of (say) config.py files, one per directory, to tell tools about the contents of
the directory, loading the top-level directory as a LazyMod root will let you
automagically reference root.foo.bar.baz, loading 'foo/bar/baz/config.py' to
populate this object's namespace.  The result is thus much the same as a python
package hierarcy, but without the need to explicitly import anything and with
all the imports done lazily.

To reload one of these lazily-loaded attributes, e.g. following an update to its
source file, just del the relevant attribute.  Next time it's accessed, it'll be
lazily reloaded.
"""
from __builtin__ import __class__ as modbase
# module is actually in __builtins__, but we can't reference it as such !

class Module (modbase):
    """Lazily-loaded module hierarchy.

    Instantiate the root of the hierarchy using Module.Root(); don't use the
    Module() constructor itself, that's reserved for internal use by the class,
    lazily constructing a sub-object hierarchy off the instance returned by
    Root().\n"""

    @classmethod
    def Root(cls, directory, name=None, src='__init__.py'):
        """Instantiate the root of a lazily-loaded module hierarchy.

        Required argument, directory, is the root directory of the
        hierarchy.  Next positional argument, name, is optional; if given (and
        not None), it is used as the __name__ of the module object.  Third
        positional argument, src, is the name of a file (typically with a '.py'
        extension) to serve as the module loaded from each directory to
        represent its 'package' object within that directory; the default is
        '__init__.py'.  (This name may even include path separators.)  The
        default for name obtained by stripping the extension (if any) from the
        last component, that doesn't start with '__', of the result of
        path-joining directory to src.  If all path components considred by that
        start with '__' then the name 'root' is used.

        Attribute lookup on the returned module object shall autoload submodules
        and subpackages (using the same src file in each) whenever attribute
        lookup would otherwise fail and a relevant file exists.\n"""

        if name is None:
            name = cls.__base_name(src)
            if name is None:
                name = cls.__base_name(directory)
                if name is None: name = 'root'

        return cls(name, directory, src).__load_self()

    __updelat = modbase.__delattr__
    def __delattr__(self, key):
        try: del self.__cache[key]
        except KeyError: pass

        return self.__updelat(key)

    __upsetat = modbase.__setattr__
    def __setattr__(self, key, value):
        if isinstance(value, Module) and value.__name__ == self.__name__ + '.' + key:
            # This is exactly what a lazy-lookup would have given us: clear any
            # prior attribute for key (maybe in __dict__) and move to __cache.
            try: delattr(self, key)
            except AttributeError: pass
            self.__cache[key] = value
        else:
            self.__upsetat(key, value)

    def __getattr__(self, key):
        try: return self.__cache[key]
        except KeyError: pass

        try: ans = self.__load_sub(key)
        except IOError: pass
        else:
            self.__cache[key] = ans
            return ans

        raise AttributeError('No such attribute, submodule or sub-directory',
                             key, self.__name__)

    def __repr__(self):
        return """<%s '%s', lazily loaded from '%s' in '%s'>""" % (
            self.__class__.__name__, self.__name__, self.__src, self.__dir)

    # Only implementation internals below.
    import sys
    @staticmethod
    def __load_file(path, into, indir, repack,
                    modmap=sys.modules, searchpath=sys.path):
        """Load python code into the namespace of an object.

        The object should be based on the builtin module type. It is returned,
        after its .__dict__ has been populated from the loaded file.\n"""

        # Fake up participation in the module protocol:
        name = into.__name__ # cached in case into.__name__ gets changed
        try: prior, restore = modmap[name], True
        except KeyError: restore = False
        modmap[name] = into
        searchpath.insert(0, indir)

        # TODO: can we replace what the system uses as <type module> when doing
        # its imports ?  See .__repack()'s documentation.

        try:
            fd = open(path)
            try: exec fd in into.__dict__
            finally: fd.close()
        finally: # Restore module protocol status quo:
            try: ind = searchpath.index(indir)
            except ValueError: print 'Someone already pruned sys.path for me :-!\n', path
            else: del searchpath[:ind+1] # also pop anything added during loading.

            if restore: modmap[name] = prior
            # Should we, in fact, leave this in place ?
            # It might mitigate the issue __repack() works round.
            else: del modmap[name]

        return repack(into)
    del sys

    def __load_self(self):
        try: return self.__load_file(self.__path__, self, self.__dir, self.__repack)
        except IOError: return self # Directory with nothing specific to load

    def __load_raw(self, name, path, base=modbase):
        inst = base(name, """%s '%s'

Lazily loaded from file %s by '%s' '%s'.
""" % (base.__name__, name, path, self.__class__.__name__, self.__name__))
        inst.__path__ = path
        return self.__load_file(path, inst, self.__dir, self.__repack)

    def __repack(self, mod, base=modbase):
        """Repackage sub-modules imported by a sub-module.

        Looks at mod's namespace, skipping any '__'-started names, for module
        objects that it would make sense to replace with Module objects.  For
        now, only sub-modules of self are considered; it may make sense to also
        apply this to sub-modules of other Module objects.

        If we could replace the builtin module(), a method of self - using
        self.__sub_{raw,mod} when appropriate, and maybe even the matching
        methods of other things in self's tree when suitable, falling back on
        the builtin module() otherwise - would let us handle imports in the
        loaded text gracefully.  At present, we just get raw module objects when
        the loading does an import, breaking the lazy loading via those.  Which
        is why we need repack.  That and the fact that __load_file() saved to
        mod.__dict__ even what belongs in mod.__cache.\n"""

        assert isinstance(mod, base)
        assert mod is self or not isinstance(mod, Module)

        try: repacking = not mod.__repacked
        except AttributeError: mod.__repacked = False
        else:
            if repacking:
                print 'Wimping out of recursive repack', mod.__name__, mod.__file__
                # When we re-wind from the recursion, all should be well anyway.
            # else: nothing to do, job has already been done.
            return mod

        ismod = isinstance(mod, Module)
        for k in dir(mod):
            if k.startswith('__'): continue

            submod = getattr(mod, k)
            if isinstance(submod, Module):
                if ismod and mod.__name__ + '.' + k == submod.__name__:
                    # __load_file stored to mod.__dict__; move to mod.__cache:
                    setattr(mod, k, submod)
                # In any case, submod's own loading did any needed repacking.
                assert submod.__repacked

            elif isinstance(submod, base):
                submod = self.__as_sub_mod(submod)
                # That's None if replacement doesn't make sense or no suitable
                # replacement is available.  Otherwise, one way or another, it
                # came from __load_sub(), so submod has been subjected to any
                # needed repack()ing.
                if submod is not None:
                    # Over-write the raw module attribute with its replacement:
                    setattr(mod, k, submod)
                    assert submod.__repacked

        mod.__repacked = True
        return mod

    import os
    @staticmethod
    def __base_name(name, split=os.path.split, prune=os.path.splitext):
        """Implements Root()'s default for name."""
        path, name = split(name)
        while name.startswith('__'):
            if path: path, name = split(path)
            else: return None

        return prune(name)[0]

    @staticmethod
    def __in_dir(stem, name,
                 abspath=os.path.abspath,
                 seps=tuple(x for x in (os.path.sep, os.path.altsep) if x)):
        if not name.startswith(stem):
            stem = abspath(stem)
            if not name.startswith(stem):
                name = abspath(name)
                if not name.startswith(stem): return None

        if name[len(stem)] in seps: return name[len(stem) + 1:]
        return None

    @staticmethod
    def __get_source(mod):
        # Some modules have .__file__, others have .__path__ !
        try: path = mod.__file__
        except AttributeError: path = mod.__path__
        # A .pyc file's source is the .py from which it's compiled:
        if path.endswith('.pyc'): path = path[:-1]
        return path

    def __as_sub_mod(self, other,
                     split=os.path.split, prune=os.path.splitext):
        """Suggest a suitable replacement for a raw module object.

        Single parameter, other, is an attribute value of a module object
        (either raw or lazy-loading) being lazy-loaded; if it's a raw module
        that could be represented as a lazy-loaded submodule of self, return
        that replacement; otherwise, return None to keep the original.\n"""

        try: name = self.__get_source(other)
        except AttributeError: return None

        # TODO: .__in_subdir(node.__dir, name) for node in self's tree
        relpath = self.__in_dir(self.__dir, name)
        if not relpath: return None
        # OK, it is (or looks like) a sub-module:

        path, name = split(relpath)
        seq = [] if name == self.__src else [ prune(name)[0] ]
        while path:
            path, name = split(path)
            seq.insert(0, name)

        seq = iter(seq)
        try: nom = seq.next()
        except StopIteration: return None # Don't believe an apparent self-reference

        # Must do the __load_sub() directly, not via getattr(), for the first as
        # other may have come as an attribute of self, in which case getattr is
        # just going to give us other here.  This also ensures we repack submod,
        # so that any module objects within *it* are taken care of.
        try: submod = self.__load_sub(nom)
        except IOError: return None # False positive ?
        # But now we can rely on getattr(), as *its* calls to __load_sub() shall
        # do a suitable __repack() where needed.
        try:
            for nom in seq: submod = getattr(submod, nom)
        except AttributeError: return None # False positive ?

        newpath = self.__get_source(submod)
        if self.__in_dir(self.__dir, newpath) != relpath:
            print 'Wimping out of replacing', other, 'with', submod
            return None

        return submod

    def __load_sub(self, key,
                   join=os.path.join,
                   exists=os.path.exists,
                   isdir=os.path.isdir):
        name = self.__name__ + '.' + key
        path = join(self.__dir, key + '.py') # TODO: also check for .pyc ?
        if exists(path):
            return self.__load_raw(name, path)

        path = path[:-3] # sub-directory: drop the .py extension
        if isdir(path):
            return self.__class__(name, path, self.__src).__load_self()

        raise IOError

    __upinit = modbase.__init__
    def __init__(self, name, directory, src, join=os.path.join):
        """Internal constructor; use Root() to construct your root object."""
        self.__path__ = join(directory, src)
        self.__upinit(name, # and a doc-string:
                      """%s '%s'

Loaded lazily from '%s'
in '%s'
""" % (self.__class__.__name__, name, src, directory))
        self.__dir, self.__src = directory, src
        self.__cache = {}

    del os

del modbase
