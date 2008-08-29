"""Cacheing data about integer-bounded ranges of the number line.

This module provides classes to manage caches of data about the naturals,
integers or even reals, in so far as the cached data can be delineated by ranges
of integers.  It presumes that you have some way of computing the data for which
it is worth saving the answers to disk (for example, because the cost of
re-computation is high, e.g. due to needing answers from nearer zero to arrive
at answers for further from it).  The cache managed by this system just
remembers the information you computed, in python modules importable with
execfile, organized in a directory hierarchy, with an __init__.py in each
directory to manage the hierarchy itself.  There may be gaps in such caches, a
system may use several of them with distinct roots and a system may remember
several (up to about 26) distinct types of information in a single cache.

For guidance on how to adapt this module's classes to your particular
application, see the template in the .Adaptation attribute of this module.  The
primary use case motivating the initial implementation of this infrastructure is
tracking information about primeness and least common factors for the naturals
(non-negative integers); the intent here is to be generic, but it is possible
some design features should be refactored out to maths.prime.cache in order to
make these classes function truly generically.


Hierarchy
=========

Actual data is stored in files, but it's not nice to put too many files into one
directory, so a hierarchy of directories is built up, to hold the data.  The
classes defined here model that hierarchy with objects.  The directory hierarchy
may be extensive, so the object hierarchy to model it is only built (on demand)
as needed and only weakly remembered, so that python's garbage collection can
keep the amount of memory used under control.  The individual data files are apt
to be large, so their contents are, likewise, only read as needed and only
weakly remembered.

An application interacts with each cache via an object, of a class derived from
CacheRoot, describing the root of its directory hierarchy.  The application can
ask this object (via its .locate() method, q.v.) to find the (object
representing a) file in whose integer-bounded range of the number line some
particular number falls.  In so far as the application has extended the classes
here to reflect other data (typically read from the cache files) which increase
monotonically with position in the number line, the root object can also find
files based on a value of such a datum.  Where the cache does not contain a file
with the data required by such a search, the root's .locate() instead returns a
'gap' object describing a range around the target sought, in which data are
lacking.  The application can consult several caches for data in this way,
refining the 'gap' at each consultation.

If none of the consulted caches has the data sought, the 'gap' object the
application is left with describes the range of values for which none of them
have data.  The application is then left to compute the data, for some interval
of the number line within this gap (or all of it), and can then ask a root to
allot a file to it, in which to save the data it has computed, thereby extending
that root's cache.  For this, it needs a modifiable root object, derived from
CacheRoot via WriteCacheRoot.  An application is presumed to normally have one
such modifiable cache, in which it saves the data it computes, backed up by
possibly several read-only caches that save it the need to compute data for the
ranges they know about.

Details:

 * Within a cache, each sub-directory or file describes the range for which it
   holds data only relative to the start-point of the range of the directory in
   which it resides, so as to keep numbers small (and hence names, of files and
   directories, short).  The numbers in question are also remembered base 36
   (using 'a' through 'z' as the digits from ten to thirty-five) to help keep
   names short.  The initial numeric part of each name is padded with 0s to make
   it the same length as the longest, so as to ensure that directory listings
   show files in their numeric order.

 * Each directory in a cache contains an __init__.py file in which the
   application can store relevant data, such as summary data covering the whole
   range of the directory; this can be used to save un-necessary loading of
   subordinate directories and files in order to discover such data when
   consulting the cache later.

 * Where some application-specific data increases monotonically along the number
   line, the application can record each directory's range of values for that
   datum in the directory's __init__.py; which makes it possible for the
   application to save the matching data of subordinate files and directories
   only relative to the start-point of the directory's range; as for spans of
   the number line, this can enable some saving in disk space.

 * The root directory shall typically use its __init__.py to record data
   relevant to the whole cache.  Since the root's name is not controlled by this
   cache code, this name does not encode the span of the cache, so the root
   directory generally records this span in its __init__.py file.

 * Directories support locking so as to prevent clashes and confusion when two
   processes are accessing the same cache.  Locking a directory for reading may
   be done by several processes at one time; but locking a directory for writing
   is incompatible with any other process locking it for either reading or
   writing.  Write locking only applies to the directory it's done to; but read
   locking propagates up via parent directories to the root of the cache, so as
   to ensure no process re-writing an ancestor directory removes a directory
   being read.


Naming
======

Nodes in the hierarchy have names matching regex
        (N|P|)([0-9a-z]+)([A-Z]+)([0-9a-z]+)(\.py|)
wherein:
 * the final (\.py|) is only non-empty for leaf nodes, i.e. files
 * the ([A-Z]+) could in fact be ([^0-9a-z]+) and provides application-specific
   information about the types of data, about integers, that are present in this
   node and any descendants
 * the initial (N|P|) - for negative, positive - is empty unless the parent
   directory's span includes both negative and positive values; this only ever
   arises when the parent directory is the root of a cache;
 * the cache-root class (based on CacheRoot) determines the default sign;
   derived classes may over-ride the positive default set by CacheRoot; any
   child of root with N or P prefix over-rides any such default; all other cache
   nodes inherit sign from their parents;
 * the two ([0-9a-z]+) are interpreted via int(,36) as naturals, measured away
   from zero, in the direction indicated by the node's sign; the first gives the
   start-point (relative to a context-dependent origin, see next point) and the
   second gives the length;
 * when (N|P|) is empty, the start-point in the preceding is given relative to
   that of the node's parent directory; otherwise, the start-point is absolute,
   in the specified direction.

Thus N0X42.py would be a file providing application-specific information of
category 'X'; it could only appear in the root directory of its hierarchy, it
would relate to values between 0 (included) down to (ecluded) minus one hundred
and fourty-six (i.e. 4*36 + 2).  In all likelihood, there would be something in
the same cache root directory whose name would start P1X (for an integer cache;
but P0X in a real cache), providing the same kind of information for positive
values up to some limit.

Files should generally be named for the exact range about which they hold data;
but a file describing work-in-progress to discover data about some range should
be named for that range even if its data are incomplete.  The ranges of files
containing different types of data may overlap.  Sub-directories of any given
directory shall always have distinct ranges, except while the parent is
write-locked and the exception is required for re-arringement of the
sub-directories.  Each directory's range must subsume the range of each of its
children; it should normally be exactly the union of the ranges of its children,
although the root directory (whose range is not encoded in its name) can have,
as its nominal span, the full range of integers for which its data are
potentially of interest.


Hierarchical tidiness
=====================

The depth of a cache file is 0; the depth of a cache directory is one more than
the depth of its child with greatest depth.  The root directory of each cache
shall avoid variations in depth among its children in so far as is practical.
Each non-root directories shall ensure that all of its children have equal
depth.  Each directory endeavours to keep its number of children close to
twelve.

When it comes to simply growing, outwards from zero, an isolated cache of only
one kind of data, with no data borrowed from other caches, the root has to
endure two adjacent depths some of the time; its deeper children are all
complete and tidy, as are all but the last of the shallower ones; there is a
bleeding-edge of last nodes with between 8 and 20 children, at each depth, all
others being tidy.  When a new node is added, it is always a new last file so it
is added to the depth=1 last node; if a non-root last node finds it has 20
children, it keeps its first 12 children and ceases being a last node by passing
these to its parent, to turn into a new last node; this may take that parent up
to 20 children, in which case it does likewise, unless it's the root.  When the
root has 20 children all of equal depth, it collects 12 into a sub-node of one
greater depth, retaining the other 8 as direct children; successive additions to
the last of these cause it to push new children of their depth into the root;
when it has n < 8 deeper nodes and 20-n > 12 shallower ones, it collects 12
shallow ones into a fresh deeper node; when it has 7 or more deeper nodes and 8
or more shallower ones, it can collect the shallow ones into a new deeper node
and get itself back to having only one depth of child.

The situation is more complex when borrowing from another cache, or when we have
borrowed from a cache in the past, that we no longer see.  If we're borrowing
from a simple cache, as above, then our cache only saves data past the end of
the other, so the dynamics are as before.  This creates a cache that covers a
range, albeit one distant from zero.  If we borrow from several such caches, not
necessarily including the ones that they referenced while being built, we have a
read-cache with holes in it and we want to grow a write-cache to fill the holes.
This write-cache then ends up covering several disjoint ranges.  It could be
structured as above, but I chose to place gaps in the intervals between
directories.

The situation is further complicated in a mixed cache, with some diverse types
of application-specific information.  An interval may be covered by the types
taken together without being covered entirely by any one of them.  New nodes
added to extend one type may fall in the middle of the range of values for all
types taken together.  Nodes of different types might only partially over-lap -
e.g. if files of different types have different lengths, or files of two types
have equal lengths but those of each type start at the mid-point of the other
type's files.

Applications shall be obliged to be able to split data of any types at any
integer, on request from CacheDir.tidy(); however, this shall endeavour to avoid
exercising this right.  If applications have limitations on the values at which
they can split, they should identify a mapping from the integers to possible
split-points and wrap the cache in an object that maps the integers the cache
infrastructure knows about (the end-points of .span attributes of Node()s) to
their corresponding split-points.  If applications have types of data whose
potential split-points are offset from one another, they can wrap the cache in
such a way as to add type-specific offsets to the data to synchronise their
split-points; if this is not possible, the application should keep the distinct
types of data in separate caches.

Policy:
  * each child of a root describes a contiguous range that doesn't straddle zero
    (but may start at it);
  * a sub-directory which is not the first or last child of its parent, or which
    has zero as a boundary value, shall have as close to twelve children as
    circumstances permit, without splitting or uniting files to silly sizes;
  * other directories shall be more liberal about numbers of children, but aim
    to keep within the range from eight to twenty or, when even that is not
    practical, within the range from six to thirty-two; these liberal
    directories shall arise in chains at the boundaries of contiguous ranges.

When two ranges, each contiguous, have expanded towards each other far enough
that they meet, unifying them shall involve a messy interaction, where changes
ripple through them to ensure tidiness in the internal child nodes.  This should
be mediated by the nearer-zero node preserving such tidiness as it has, with its
neighbour transfering nodes into it as if the nearer-zero node were simply
having nodes added to it after the manner of simple growth - albeit these
additions may be done in bulk, rather than one at a time.

$Id: whole.py,v 1.17 2008-08-29 04:12:42 eddy Exp $
"""

Adaptation = """
# This is a template for extending the classes provided here.

from study.cache import whole

class Node (whole.Node):
    # optionally extend _load_
    pass
class WriteNode (Node, whole.WriteNode):
    # optionally extend _save_
    pass

class CacheFile (Node, whole.CacheFile):
    # optionally extend _load_ some more
    pass
class WriteCacheFile (WriteNode, whole.WriteCacheFile):
    # optionally extend _save_ some more
    pass
# Optionally complicate further with files for each cached data type

weaklisting = whole.CacheDir.weaklisting
class CacheDir (Node):
    @staticmethod
    def _child_class_(isfile, mode):
        # possibly complicate further for mode
        if isfile: return CacheFile
        return CacheSubDir

    # optionally extend _load_ some more
    # optionally extend _onchange_, _gap_
    # define any @weaklisting attributes, typically one per cached data type
del weaklisting

class WriteCacheDir (WriteNode, CacheDir):
    @staticmethod
    def _child_class_(isfile, mode):
        # possibly complicate further for mode
        if isfile: return WriteCacheFile
        return WriteCacheSubDir

    # optionally extend _save_ some more

class CacheSubDir (CacheDir, whole.CacheSubDir):
    pass
class WriteCacheSubDir (WriteCacheDir, whole.WriteCacheSubDir):
    pass

class CacheRoot (CacheDir, whole.CacheRoot):
    # optionally: over-ride sign, span
    pass
class WriteCacheRoot (WriteCacheDir, whole.WriteCacheRoot):
    pass

# (Note: this is a good example of where classic single-inheritance falls down,
# although ruby's variation on that theme would be able to cope.)
"""

from study.snake.regular import Interval
from property import Cached, lazyattr, lazyprop
from weak import weakattr
from errno import EWOULDBLOCK

class Node (Cached):
    """Base-class for nodes in a hierarchy of cached data about integers.

    Derived classes should implement attribute _cache_file (absolute path of
    file to be loaded by load; use __init__.py in directories), implement lock
    and unlock methods (with optional boolean arguments (read, write) each
    defaulting False, raising IOError or returning False on failure), extend
    load and implement a (typically lazy) attribute span, describing the range
    of integers this Node describes.  If span is None it means the full range of
    all integers; otherwise, it should be an Interval or the result of negating
    an Interval (a Span with stride -1).\n"""

    @weakattr
    def content(self, ig=None): return self._load_()

    class Bok (dict): pass # for weakref's sake
    def _load_(self, bok=None, glo={}, Dict=Bok, BLOCKED=EWOULDBLOCK):
        """Loads self._cache_file and returns the result as a mapping.

        Sole optional argument, bok, is a mapping object into which the
        cache-file's namespace is to be imported; if not supplied, an empty
        mapping of a class based on dict (so amenable to weakref) is used; in
        either case, this is the object returned on success.  May raise IOError
        or ParseError.

        Derived classes extending this should take the same optional argument,
        pass it down to base-class load and do any processing of the result,
        e.g. extracting data for use in application-specific attributes, on the
        way back out.\n"""
        # TODO: want to be able to GET a URL instead of reading a file

        if bok is None: bok = Bok()
        if not self.lock(True):
            raise IOError(BLOCKED, 'File temporarily unreadable',
                          self._cache_file)
        try: execfile(self._cache_file, glo, bok) # may raise IOError, ParseError
        finally: self.unlock(True)

        try: self.depth = bok.pop('depth')
        except KeyError: assert isinstance(self, CacheFile)

        return bok
    del Bok

class WriteNode (Node):
    """Extends Node with write-functionality.

    Derived classes should extend save.\n"""
    def _save_(self, formatter=None, **what):
        """Saves a given namespace to a module.

        First argument, formatter, is None (to use a default) or a function
        taking a key, a value and an optional replacement for repr; it should
        output a string to save to file, that shall be used to record the
        key-value pair.  If None, a default is used which formats the pair as a
        simple assignment statement with the key on the left and the value's
        repr() on the right, followed by a newline.

        All other arguments should be given in keyword form.  The name 'depth'
        shall be ignored if given (it is over-written by an attribute of the
        same name) on nodes not descended from CacheFile (whose depth is 0).
        The name '__doc__' shall be replaced by self.__doc__ if this is not the
        same object as self.__class__.__doc__; any __doc__ obtained by either of
        these means shall be saved as doc-string in the module.

        Derived classes may introduce special handling for some keys; to do so,
        they need to hijack the formatter they get and tunnel it through one
        they pass on in its place, that implements the special handling and falls
        back on the given formatter or, if None:
            lambda k, v: '%s = %s\n' % (k, repr(v))
        using the repr optionally passed to them by their caller.  Derived
        classes should document the special handling they implement.  This class
        provides special handling for the keys 'depth' and '__doc__': see above.

        Derived classes may also introduce special formatting of values by
        passing a replacement for repr to their client's (or further derived
        class's) formatter, via their re-formatting wrapper.  If doing so, they
        must take care to use any repr supplied to their wrapper, when *it* is
        called (by a base class), rather than the built-in, as the foundation on
        which they build their modified repr.\n"""

        if formatter is None:
            formatter = lambda k, v: '%s = %s\n' % (k, repr(v))

        if not self.lock(write=True):
            raise IOError(self.__BLOCKED, 'File temporarily unwritable',
                          self._cache_file)
        try:
            fd = open(self._cache_file, 'w')
            try:
                doc = what.pop('__doc__', None)
                if self.__doc__ is not self.__class__.__doc__:
                    doc = self.__doc__
                if doc: fd.write('"""%s"""\n\n', self.__doc__)

                if not isinstance(self, CacheFile):
                    fd.write('depth = %d\n' % self.depth)
                what.pop('depth', None) # don't let what over-ride that.

                for k, v in what.items():
                    fd.write(formatter(k, v))

            finally: fd.close()
        finally: self.unlock(write=True)

        run = self.parent
        while run is not None:
            run.changed = True
            run = run.parent

        # potentially: return size of file

    __BLOCKED = EWOULDBLOCK

del EWOULDBLOCK, Cached, weakattr

class SubNode (Node):
    def __init__(self, parent, types, sign, start, reach,
                 Range=Interval):
        if sign: self.__sign = sign
        self.__span = Range(start, reach), 
        self.__type, self.__up = types, parent

    # read-only access to data members
    @property
    def parent(self, ig=None): return self.__up
    @property
    def types(self, ig=None): return self.__type
    @property
    def sign(self, ig=None):
        up = self.__up.sign
        try: return self.__sign * up
        except AttributeError: return up

    @lazyattr
    def span(self, ig=None):
        if self.__sign:
            assert self.__up.span is None or 0 in self.__up.span
            ans = self.__span
        else:
            assert self.__up.span is not None
            ans = self.__span + self.__up.span.start

        if self.sign < 0: return -ans
        return ans

    # Chase parent to its root:
    @property
    def root(self, ig=None): return self.__up.root

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, sign, start, span):
        self.__upinit(parent, '', start, span)

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, name, parent, types, sign, start, span):
        self.__gapinit(parent, types, sign, start, span)
        self.__name = name

    def path(self, *tail): return self.parent.path(self.__name, *tail)

class WriteSubNode (CacheSubNode, WriteNode):
    # TODO: needs to know how to rename itself when its range expands
    pass

class CacheFile (CacheSubNode):
    @lazyattr
    def _cache_file(self, ig=None): return self.path()

    @property
    def depth(self, ig=None): return 0

    def lock(self, read=False, write=False):
        return self.parent.lock(read, write)
    def unlock(self, read=False, write=False):
        return self.parent.unlock(read, write)

    def __spanner(self, val):
        up = self
        try:
            while val not in up:
                val += up.start
                up = up.parent

        except AttributeError: return None # root lacks this value

        ans = up.locate(val, types=self.types)
        if isinstance(ans, CacheFile): return ans
        return None # no such data available under my root

    @lazyattr
    def next(self, ig=None):
        """Successor node to this one, if in this cache; else None."""
        return self.__spanner(self.span.stop)

    @lazyattr
    def prev(self, ig=None):
        """Prior node to this one, if in this cache; else None."""
        return self.__spanner(self.span.start - 1)

class WriteCacheFile (CacheFile, WriteSubNode):
    # TODO: support being expanded or split.
    pass

from lockdir import LockableDir

class LockDir (LockableDir):
    """Implement lock propagation.

    This extends LockableDir's un/locking so that read locks propagate up to
    root (whose .parent must be None) while write locks are only set on the
    directories to which they are directly addressed.  This lets one process
    modify parts of a cache while another is reading other parts, as long as
    their activities don't overlap.\n"""

    __lock, __unlock = LockableDir.lock, LockableDir.unlock
    def lock(self, read=False, write=False):
        if not read or self.parent is None:
            return self.__lock(read, write)
        elif self.parent.lock(read, False):
            if self.__lock(read, write): return True
            self.parent.unlock(read, False)
        return False

    def unlock(self, read=False, write=False):
        if not read or self.parent is None: ok = True
        else: ok = self.parent.unlock(read, False)
        return self.__unlock(read, write) and ok

del LockableDir

import os
from weak import WeakTuple
TYPES = 2 # index into each entry of __listing at which type string is held

class CacheDir (LockDir):
    @lazyattr
    def _cache_file(self, ig=None):
        return self.path('__init__.py')

    from study.snake.sequence import Ordered
    import re
    @lazyattr
    def __listing(self, ig=None, get=os.listdir, seq=Ordered,
                  pat=re.compile(r'^(N|P|)([0-9a-z]+)([A-Z]+)([0-9a-z]+)(\.py|)$'),
                  signmap={ 'N': -1, 'P': +1, '': None }):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                try: sign = signmap(got.group(1))
                except KeyError:
                    assert False, "Bad sign; shouldn't have matched regex"
                    sign = None

                start, size = int(got.group(2), 36), int(got.group(4), 36)
                start *= self.sign # ensure sensible sort order in __listing
                if sign is not None: start *= sign
                mode, isfile = got.group(3), got.group(5)
                # Be sure to match WeakSeq:
                ans.append((start, size, mode, sign, name, isfile))

        return ans
    del re, Ordered

    def depth(self, ig=None): # but usually we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1
    depth = lazyprop('depth', depth)

    def _onchange_(self):
        """Update attributes after directory contents have changed.

        When the contents of the directory get changed (see WriteCacheDir), this
        method is called to ensure attributes correctly reflect the contents of
        the directory.  Note that changes to self.span are handled separately.

        Where possible, attributes that depend on directory contents should be
        lazy.  All lazy attributes (notably including every @weaklisting) can
        simply be deleted, since lazyattr doesn't raise any error when the
        attribute is absent; so deletion shall remove the attribute if present,
        ready to be recomputed next time it's needed.  Any non-lazy attributes,
        however, need to be updated.

        Derived classes should extend this method, calling relevant base class
        version and deleting any attributes they add.\n"""

        del self.__listing, self.listing, self.depth

    @staticmethod
    def _child_class_(isfile, mode):
        """Child node classes for use by @weaklisting attributes.

        Required arguments:
          isfile -- true if the child is to describe a file, false if it is to
                    describe a directory
          mode -- the '[A-Z]+' part of the child's name (may be ignored)

        Returns a class to use, taking the same constructor arguments as for
        CacheFile and CacheSubDir, to instantiate a child of self.  When isfile
        is true, this class must have CacheFile as a base; when isfile is false,
        the class must have CacheSubDir as a base.

        Derived classes should over-ride this @staticmethod to provide classes
        with suitable @weaklisting (and related) attributes for their specific
        applications.\n"""

        if isfile: return CacheFile
        return CacheSubDir

    class WeakSeq (WeakTuple):
        __upinit = WeakTuple.__init__
        def __init__(self, cdir, getseq):
            def get(ind, s=cdir, g=getseq):
                # be sure to match order in __listing(self, ...), above:
                start, size, mode, sign, name, isfile = g(s)[ind]
                start /= cdir.sign # undo __listing's sorting hack
                if sign is not None: start /= sign
                klaz = s._child_class_(isfile, mode)
                assert issubclass(klaz, CacheDir._child_class_(isfile, mode))
                return klaz(name, s, mode, sign, start, size)
            self.__upinit(get)
            self.__who, self.__att = cdir, getseq

        def __len__(self): return len(self.__att(self.__who))

    @lazyattr
    def listing(self, ig=None, W=WeakSeq):
        return W(self, lambda s: s.__listing)

    del WeakSeq

    class WeakSubSeq (WeakTuple):
        """For filtered sub-lists of .listing

        We don't want to force re-loading of every entry in listing just to
        check whether its types match some requirement; so, instead, we rely on
        the fact that .listing is in sync with .__listing; filter the latter to
        find the entry we want, then take the corresponding entry from the
        former.  This ensures that all @weaklistings re-uses the same objects as
        entries, rather than creating duplicate objects.

        Required arguments:
          cdir -- CacheDir object
          getseq -- function to fetch its private __listing attribute
          test -- test to apply to types strings, the [A-Z]+ part of file-names.

        This member class can't refer to .__listing directly, so we have to be
        given getseq to fetch it; our resulting WeakTuple holds the members of
        .listing with indices matching those in cdir.__listing for whose types
        field the given test returns true.\n"""

        __upinit = WeakTuple.__init__
        def __init__(self, cdir, getseq, test):
            def get(ind, s=cdir, g=getseq, f=test):
                j = 0
                for it in g(s):
                    if f(it[TYPES]):
                        if ind: ind -= 1
                        else:
                            ans = s.listing[j]
                            assert ans.types == it[TYPES] and f(ans.types)
                            return ans
                    j += 1

                raise IndexError
            self.__upinit(get)
            self.__who, self.__att, self.__test = cdir, getseq, test

        def __len__(self): return len(filter(self.__test, self.__att(self.__who)))

    @staticmethod
    def weaklisting(picker, W=WeakSubSeq, L=lazyattr):
        """Lazy WeakTuple decorator for a filter on types.

        The [A-Z]+ portion of each cache file or directory name describes the
        type of data it contains.  This class supports a lazy attribute which
        packages the parsed directory listing; using other classes of this
        module it can convert entries in this to suitable SubNode objects.  The
        lazy attribute listing yields a full list of such objects, as a
        WeakTuple.  However, derived classes may want only a sub-set of that
        listing, also as a WeakTuple.  This decorator helps them implement that.

        Single argument, picker, is a function which takes a type string,
        matching '[A-Z]+', and returns a true result if sub-nodes of that type
        are to be included in the WeakTuple.

        Example: a derived class, for which type 'F' data provides factor
        information about naturals, could use:
           @CacheDir.weaklisting
           def factors(types): 'F' in types
        to provide its instances with a lazily-evaluated .factors attribute,
        whose value is a tuple of weakly referenced sub-node objects describing
        directory entries in whose names 'F' appeared in the type
        component.\n"""

        def get(self, ig=None, test=picker, S=W):
            return S(self, lambda s: s.__listing, test)

        return L(get, doc=picker.__doc__)

    del WeakSubSeq

    @staticmethod
    def _gap_(before, after, limit, Hole=Gap, Range=Interval):
        """Gap object for use by locate.

        Given arguments:
          before -- extant cache node before the gap, or None
          after -- extant cache node after the gap, or None
          limit -- None or an existing Gap

        Returns a Gap object describing the interval between two nodes.  If
        limit is not None, the returned Gap's interval is a sub-interval of the
        one it describes.

        Derived classes should extend this method, calling their base-class's
        implementation and adding application-specific attributes to the gap
        describing such properties of it as they know how to infer from before
        and after, which shall be instances of some class or classes returned by
        self._child_node_().  Interval attributes should, where relevant, be
        sub-intervals of those on limit, if present.\n"""

        assert before is None or after is None or before.parent == after.parent
        parent = None

        if before is None: start is None
        else: start, parent = before.stop, before.parent

        if after is None: stop is None
        else: stop, parent = after.start, after.parent

        if limit is not None:
            assert isinstance(getattr(limit, 'span', None), Range)
            # TODO - special case: before, after, gap don't all have same sign !

            if start is None: start = limit.span.start
            elif limit.span.start is None: pass
            elif limit.span.start > start: start = limit.span.start

            if stop is None: stop = limit.stop
            elif limit.span.stop is None: pass
            elif limit.span.stop < stop: stop = limit.span.stop

            if parent is None: parent = limit.parent

        assert parent is not None
        return Hole(parent, start, stop)

    @staticmethod
    def _bchop(row, value, attr):
        lo, hi = 0, len(row)
        loa, hia = getattr(row[lo], attr), getattr(row[hi], attr)
        if loa.start > value: raise IndexError(None, row[lo])
        elif hia.stop <= value: raise IndexError(row[hi], None)
        if loa.stop > value: return row[lo]
        if hia.start <= value: return row[hi]
        assert hi > lo

        while lo + 1 < hi:
            assert loa.stop <= value < hia.start
            # linear-interpolate a "mid-point" between those:
            mid = int(lo + 1 + (hi - lo - 1) * 
                      (value - loa.stop) * 1./ (hia.start - loa.stop))
            assert lo < mid < hi
            mida = getattr(row[mid], attr)
            if mida.start > value: hi, hia = mid, mida
            elif mida.stop <= value: lo, loa = mid, mida
            else: return row[mid]

        assert hi == lo + 1
        assert row[lo].span.stop < row[hi].span.start # it's a gap
        assert loa.stop <= value < hia.start
        raise IndexError(row[lo], row[hi])

    def locate(self, value, attr='span', gap=None, seq='listing', types=None):
        """Find sub-node or gap containing a specified entry.

        Do not pass more than five arguments (beyond self) to this method.
        Required argument:
          value -- what to locate
        Optional arguments:
          attr -- name, default 'span', of the attribute to which value relates.
                  Must be an Interval-valued attribute, whose ranges of values
                  for nodes with disjoint spans are disjoint and in the same
                  relative order as the spans.
          gap -- a Gap object, or None indicating the full range of integers
          seq -- name, default 'listing', of some @weaklisting attribute for
                 which each entry in the list has the attribute named by attr
          types -- None, to use seq, else an [A-Z]+ string to ignore seq and use
                   the sub-list of .listing whose types include all those listed
                   in types.

        Aims to locate a CacheFile in which the natural with attr equal to value
        would lie, if there is such a natural; however, if the cache contains no
        such file, a Gap object shall be returned, describing an interval in
        which this cache lacks relevant data.  In the latter case, the Gap
        object is obtained by calling self._gap_ with suitable arguments, using
        gap as limit.

        The attribute named by seq (when not superseded by specifying types)
        should be an attribute of self and of all CacheDir descendants of self;
        its value on each such node should be a list (typically the @weaklisting
        filtering on child nodes that shall have attribute attr) in which each
        entry must have attribute attr, with the entries in self.seq and each
        descendant CacheDir's .seq are sorted in increasing order on attr.\n"""

        if types is None: row = getattr(self, seq)
        elif not types: row = self.listing
        else:
            row, i = [], 0
            for it in self.__listing:
                for t in types:
                    if t not in it[TYPES]: break
                else:
                    row.append(self.listing[i])
                i += 1

        try: kid = self._bchop(row, value, attr)
        except IndexError, what:
            return self._gap_(*(what.args + (gap,)))

        if isinstance(kid, CacheDir):
            return kid.locate(value, attr, gap, seq, types)

        assert isinstance(kid, CacheFile)
        return kid

del WeakTuple, LockDir, Interval, lazyprop

class CacheRoot (Node, CacheDir):
    def __init__(self, path): self.__dir = path
    parent = span = None
    sign = +1
    @property
    def root(self, ig=None): return self
    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)

del os

from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

class WriteCacheDir (CacheDir):
    def newfile(self, span, types):
        """Find where to create a new file to be added.

        Required arguments:
          span -- the range of integers to be described by the file
          types -- an [A-Z]+ text specifying application-specific types of
                   information about integers to be stored in the new file.

        Raises ValueError if the given span overlaps any existing file.
        Otherwise, returns a self._child_class_(True, types) instance in the
        hierarchy under self, into which the client may save its data of the
        given types for the given range of integers.  The application should
        subsequently call self.tidy() to ensure consistency of the resulting
        cache.

        This base-class implementation may instead raise an IndexError whose
        .args is a twople, (before, after), each entry in which is a node on the
        relevant side of the sought file, if any, else None; derived classes
        WriteCacheRoot and WriteCacheDir overload this method to deal with the
        various possibilities in that case.\n"""

        assert span.step in (-1, +1), 'I require a contiguous range'
        assert span > -1 or span < 1, 'Only a cache root can straddle 0'
        assert self.span is None or self.span.subsumes(span)
        lo = self.locate(span.start, types=types)
        hi = self.locate(span.start + len(span) - 1)
        if isinstance(lo, CacheFile) or isinstance(hi, CacheFile):
            raise ValueError(span, 'starts or ends in existing file')
        if span.start < hi.span.start or lo.span.stop < span.stop:
            raise ValueError(span, 'straddles an existing file')

        row, i = [], 0
        for it in self.__listing:
            if it[5]: # isfile
                for t in types:
                    if t not in it[TYPES]: break
                else:
                    row.append(self.listing[i])
            else: row.append(self.listing[i])

        kid = self._bchop(row, value, 'span') # may IndexError(before, after)
        if isinstance(kid, CacheDir):
            assert isinstance(kid, WriteCacheDir)
            return kid.newfile(span, types)

        # Extend the existing file, or ask me for a file for the extra part !
        assert isinstance(kid, WriteCacheFile)
        return kid

    changed = False # see tidy() and WriteNode._save_()
    def tidy(self):
        """Tidy up subordinate nodes.

        Ideally, each directory contains a dozen children, all of equal depth.
        This function sees how near that ideal it can bring this directory.\n"""

        change = False
        for node in self.listing:
            if isinstance(node, WriteCacheDir) and \
                   (node.changed or len(node.__listing) != 12):
                if node.tidy(): change = True

        if self.change or change:
            self._onchange_()
        elif len(self.__listing) == 12:
            return False # nothing to do

        # TODO: implement
        raise NotImplementedError

        del self.changed # to expose the class value, False
        assert not self.changed
        return True

    @staticmethod
    def _child_class_(isfile, mode): # configure __listing
        if isfile: return WriteCacheFile
        return WriteCacheSubDir

    @lazyattr
    def __namelen(self, ig=None, fmt=nameber.encode):
        # len(self.span) is an upper bound on the .span.start of children
        return len(fmt(len(self.span)))

    def __child_name(self, span, isfile, types, fmt=nameber.encode):
        """Determine name of a child.

        Required arguments:
          span -- range of integers to be described by the new node
          isfile -- true for a file, false for a directory
          types -- [A-Z]+ string indicating application-specific types of data
                   to be stored in the new node.

        Returns a name matching the regex of __listing and padded with enough
        leading 0s to make lexical sorting match numeric order.\n"""

        name = fmt(span.start - self.span.start)
        gap = self.__namelen - len(name)
        if gap > 0: name = '0' * gap + name

        assert len(types) > 0
        name += types
        name += fmt(len(span))
        if isfile: name += '.py'
        return name

del nameber

class WriteCacheRoot (WriteNode, CacheRoot, WriteCacheDir):
    _child_class_ = WriteCacheDir._child_class_
    __newfile = WriteCacheDir.newfile # q.v. for documentation
    def newfile(self, span, types):
        try: return self.__newfile(span, types)
        except IndexError, what: before, after = what.args
        klaz = self._child_class_(True, types)

        # before and after may be subdirectories, in which case adding to their
        # relevant end may be suitable, if they abut span; otherwise, I'd beter
        # be depth 1, so I can tidily add a simple file, or the root, where I'm
        # allowed untidiness.
        if self.depth == 1:
            # name?, self, types, sign?, span.start, len(span)
            if self.span is None or (-1 in self.span and +1 in self.span):
                # self straddles zero, so we must set sign
                if span > -1: sign = self.sign
                else: sign = -self.sign
            else: sign = None

        # TODO: implement
        raise NotImplementedError

        return klaz(name, self, types, sign, start, reach)

class CacheSubDir (CacheSubNode, CacheDir):
    pass

class WriteCacheSubDir (WriteSubNode, CacheSubDir, WriteCacheDir):
    _child_class_ = WriteCacheDir._child_class_
    __newfile = WriteCacheDir.newfile # q.v. for documentation
    def newfile(self, span, types):
        try: return self.__newfile(span, types)
        except IndexError, what: before, after = what.args
        klaz = self._child_class_(True, types)

        # If self is the before or after to which a parent delegated, its
        # matching before or after shall be None; it needs to grow to embrace
        # span.

        if self.depth == 1:
            pass

        # TODO: implement
        raise NotImplementedError

        return klaz(name, self, types, None, start, reach)

del lazyattr
