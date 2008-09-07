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

$Id: whole.py,v 1.24 2008-09-07 09:29:31 eddy Exp $
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
    # optionally extend _ontidy_, _gap_
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

        self.parent._onchange_()
        # potentially: return size of file

    __BLOCKED = EWOULDBLOCK

del EWOULDBLOCK, Cached, weakattr

class SubNode (Node):
    def __init__(self, parent, types, start, reach, sign=None,
                 Range=Interval):
        """Initialize a sub-node with type and span data.

        Required arguments:
          parent -- parent node
          types -- string of [A-Z]+ type indicators
          start -- offset into parent's .span at which self starts
          reach -- length of interval self describes

        Optional argument, sign, defaults to None, meaning parent.span doesn't
        straddle 0; otherwise, it should be +1 or -1 to indicate which side of 0
        self is.  Do not supply any further parameters.\n"""

        assert reach > 0
        if sign:
            assert sign in (+1, -1)
            self.__sign = sign
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
        ans = self.__span
        if self.sign < 0: ans = -ans
        try: self.__sign
        except AttributeError:
            assert self.__up.span is not None
            return ans + self.__up.span.start

        assert self.__up.straddles0
        return ans

    # Chase parent to its root:
    @property
    def root(self, ig=None): return self.__up.root

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, start, span, sign):
        self.__upinit(parent, '', start, span, sign)

    def straddles0(self, ig=None):
        return self.span is None or (-1 in self.span and +1 in self.span)

class CacheSubNode (SubNode):
    __gapinit = SubNode.__init__
    def __init__(self, name, parent, types, start, span, sign):
        self.__gapinit(parent, types, start, span, sign)
        self.__name = name

    def path(self, *tail): return self.parent.path(self.__name, *tail)

    @lazyattr
    def straddles0(self, ig=None):
        """CacheSubnodes do not straddle zero.

        They may start at it, but never contain values on both sides of it.
        This could simply be a property, except that I want to lazilly assert,
        at most once in the object's life-time, that it really doesn't straddle
        zero.\n"""
        assert self.span is not None
        assert -self.sign not in self.span
        return False

class WriteSubNode (CacheSubNode, WriteNode):
    def _extend_(self, span):
        # needs to know how to rename itself when its range expands
        raise NotImplementedError # TODO: implement

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
    # TODO: support being (expanded (see _extend_) or) split.
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
                if sign is not None: # ensure sensible sort order
                    start *= sign * self.sign
                mode, isfile = got.group(3), got.group(5)
                # Be sure to match WeakSeq:
                ans.append((start, size, mode, sign, name, isfile))
                # WriteCacheDir.newfile relies on isfile being last

        return ans
    del re, Ordered

    def depth(self, ig=None): # but usually we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1
    depth = lazyprop('depth', depth)

    def _ontidy_(self):
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
                if sign is not None: # undo __listing's sorting hack
                    start /= sign * cdir.sign
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
    def _gap_(before, after, limit=None, Hole=Gap, Range=Interval):
        """Gap object for use by locate.

        Required arguments:
          before -- extant cache node one one side of the gap, or None
          after -- extant cache node on the other side of the gap, or None
        Optional argument:
          limit -- None (default) or an existing Gap
          Hole -- callable, taking the same parameters of Gap and returning an
                  instance of Gap; defaults to Gap, but derived classes might
                  wish to replace it with a class based on Gap.

        At most one of before and after shall be None; when neither is, they
        share a common .parent, each is an entry in its .listing and before's
        index therein is less than after's.  In particular, each shall be an
        instance of some class returned by the ._child_node_() of .parent; and
        .parent's class is the one whose _gap_ implementation is used.

        Returns a Gap object describing the interval between the given nodes.
        If limit is not None, the returned Gap's interval is a sub-interval of
        the one it describes.

        Derived classes should extend this method, calling their base-class's
        implementation (optionally with a class derived from Gap as Hole) and
        adding application-specific attributes to the returned gap, describing
        such properties of it as they know how to infer from before and after.
        Interval attributes should, where relevant, be sub-intervals of any
        corresponding attribute on limit.\n"""

        # First, work out absolute start, stop
        if after is None: stop = None
        else:
            parent = after.parent
            if after.span.step * parent.sign < 0:
                assert after.span.stop is not None
                stop = after.span.stop - after.span.step
            else: stop = after.span.start

        if before is None:
            assert after is not None # => nor is parent, nor is stop
            if parent.span is None: start = None
            else: start = parent.span.start
        else:
            parent = before.parent
            if before.sign * parent.sign < 0:
                start = before.span.start - before.span.step
            else: start = before.span.stop
            assert start is not None
            if stop is None and parent.span is not None:
                stop = parent.span.stop # which may still be None

        assert parent is not None
        assert parent.span.step == parent.sign
        # Clip to limit:
        if limit is not None:
            assert isinstance(getattr(limit, 'span', None), Range)
            lo, hi = limit.span.start, limit.span.stop
            if limit.span.step * parent.sign < 0: lo, hi = hi, lo

            if start is None: start = limit.span.start
            elif lo is None: pass
            elif (lo - start) * parent.sign > 0: start = lo

            if stop is None: stop = hi
            elif hi is None: pass
            elif (hi - stop) * parent.sign < 0: stop = hi

        assert start is None or stop is None or (stop - start) * parent.sign > 0
        # Now orient away from zero (if possible):
        if start is None:
            assert parent.span is None
            assert stop is not None
            start, stop, sign = stop - parent.sign, None, -parent.sign
        elif stop is None:
            assert parent.span.stop is None
            sign = parent.sign
        elif stop * parent.sign <= 1 && start * parent.sign < 0:
            sign = -parent.sign
            start, stop = stop - sign, start - sign
        else:
            sign = parent.sign

        assert start is not None
        # Fix up relative to parent:
        if parent.straddles0:
            sign /= parent.sign
        else:
            assert sign == parent.sign
            sign = None
            start -= parent.span.start
            if stop is not None:
                stop -= parent.span.start

        # Finally, create our Gap object:
        return Hole(parent, start, stop, sign)

    @staticmethod
    def _bchop(row, value, attr):
        """Find where in a sequence some monotonic attribute takes a given value.

        Required arguments:
          row -- sequence of cache nodes
          value -- the value sought
          attr -- the name of an attribute

        Uses binary chop, with linear interpolation, and endeavours to access as
        few nodes in the sequence as possible, since the sequence may be weakly
        stored, hence accessing entries may involve non-trivial object creation.
        If a node in the sequence is found, in whose range of values, of the
        given attr, our sought value lies, then this node's index in row is
        returned.  Otherwise, an IndexError is raised, whose .args is a twople,
        (before, after), in which: either before is None or it's the index of a
        node whose range of attr is entirely less than value; either after is
        None or it's the index of a node whose range of attr is entirely greater
        than value; if either is None then the other is 0 or len(row)-1; and if
        neither is None they are consecutive (but possibly reversed) indices in
        row.\n"""

        lo, hi = 0, len(row) - 1
        loa, hia = getattr(row[lo], attr), getattr(row[hi], attr)
        if hia > loa: sign = +1
        else:
            assert hia < loa
            sign, lo, hi, loa, hia = -1, hi, lo, hia, loa

        if loa.min > value: raise IndexError(None, lo)
        elif hia.max < value: raise IndexError(hi, None)
        if loa.max >= value: return lo
        if hia.min <= value: return hi
        assert 0 < sign * (lo - hi)

        while 1 < sign * (lo - hi):
            assert loa.max < value < hia.min
            # linear-interpolate a "mid-point" between those:
            mid = int(lo + sign + (hi - lo - sign) * 
                      (value - loa.max - 1) * 1./ (hia.min - loa.max - 1))
            assert (sign < 0 and lo > mid > hi) or (sign > 0 and lo < mid < hi)
            mida = getattr(row[mid], attr)
            if mida.min > value: hi, hia = mid, mida
            elif mida.max < value: lo, loa = mid, mida
            else: return mid

        assert sign * (hi - lo) == 1
        assert loa.max < value < hia.min
        raise IndexError(lo, hi)

    def locate(self, value, attr='span', gap=None, seq='listing', types=None):
        """Find sub-node or gap containing a specified entry.

        Do not pass more than five arguments (beyond self) to this method.
        Required argument:
          value -- what to locate
        Optional arguments:
          attr -- name, default 'span', of the attribute to which value relates.
                  Must be an Interval-valued attribute, whose ranges of values
                  for nodes with disjoint spans are disjoint and abut (i.e. the
                  .stop of one is the .start of the other) if the spans do.
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
        object is obtained by calling self._gap_ (q.v.) with suitable arguments,
        using gap as limit.  This enables derived classes to equip the returned
        object with relevant attributes unknown to this base-class.

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

        if not row: # hopeless !
            if gap is None:
                raise ValueError, 'No gap limits range on empty cache directory'
            return gap

        try: kid = self._bchop(row, value, attr)
        except IndexError, what:
            b, a = what.args
            if (None not in (a, b) and a < b) or \
                   (b is None and a + 1 == len(row)) or \
                   (a is None and b == 0):
                a, b = b, a
            return self._gap_(row[b], row[a], gap)

        if isinstance(kid, CacheDir):
            return kid.locate(value, attr, gap, seq, types)

        assert isinstance(kid, CacheFile)
        return kid

del WeakTuple, LockDir, Interval, lazyprop

class CacheRoot (Node, CacheDir):
    def __init__(self, path): self.__dir = path
    parent = span = None
    sign = +1
    @lazyattr
    def straddles0(self, ig=None):
        return self.span is None or (-1 in self.span and +1 in self.span)
    @property
    def root(self, ig=None): return self
    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)


from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

class WriteCacheDir (CacheDir):
    def abuts(lo, gap, hi): # Tool function for newfile.
        """Determines whether lo or hi can be grown to include gap.

        Arguments:
          lo -- a span which might abut gap.start
          gap -- a span
          hi -- a span which might abut gap.stop

        Abutting across 0 doesn't count; a node can't be grown across it.\n"""
        ar, op, ep = gap.start, gap.stop, gap.step
        if op == 0:            hia = False
        elif hi.step * ep > 0: hia = (op - hi.start) * ep >= 0
        else:                  hia = (op - hi.stop)  * ep >  0
        if lo.step * ep > 0:    loa = (ar - lo.stop)  * ep <  1
        elif ar * ep in (0, 1): loa = False
        else:                   loa = (ar - lo.start) * ep <= 1
        return loa, hia

    def newfile(self, span, types, abutting=abuts):
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

        row, i = [], 0
        for it in self.__listing:
            if it[-1]: # isfile; only include if types match
                for t in types:
                    if t not in it[TYPES]: break
                else:
                    row.append(self.listing[i])
            else: # directory: always include
                row.append(self.listing[i])

        sign = span.step * self.span.step
        def getargs(what, r=len(row), swap=sign<0):
            b, a = what.args
            # before, after: indices into row, possibly in reverse order:
            if (None not in (a, b) and a < b) or \
                   (b is None and a + 1 == r) or \
                   (a is None and b == 0):
                a, b = b, a
            # Now we know their order in row,
            # put them into their order relative to span:
            if swap: return a, b
            return b, a

        endfile = midfile = hi = None
        if span.stop is not None:
            try: end = span.last
            except AttributeError:
                ep = span.step
                if ep is None: ep = 1
                q, r = divmod(span.stop - span.start, ep)
                if not r: q -= 1
                end = span.start + q * ep

            try: hi = self._bchop(row, end, 'span')
            except IndexError, what:
                hi = (b, a) = getarts(what)

                if b is not None:
                    b = row[b]
                    assert b.span is not None
                    if b.span.step * span.step < 0:
                        wide = span.step * (span.start - b.span.start) < 1
                    else if b.span.stop is None:
                        assert False, "How did that happen ?"
                        wide = True
                    else:
                        wide = span.step * (span.start - b.span.stop) < 0

                    if wide: midfile = b
            else:
                if isinstance(row[hi], CacheFile):
                    endfile = row[hi]

        try: lo = self._bchop(row, span.start, 'span')
        except IndexError, what:
            lo = (b, a) = getargs(what)
            if a is not None and span.stop is not None:
                a = row[a]
                assert a.span is not None
                if a.span.step * span.step > 0:
                    wide = span.step * (span.stop - a.span.start) > 0
                elif a.span.stop is None:
                    assert False, "How did that happen ?"
                    wide = True
                else:
                    wide = span.step * (span.stop - a.span.stop) > 1

                if wide: midfile = b
        else:
            if isinstance(row[lo], CacheFile):
                endfile = row[lo]

        if endfile is not None:
            raise ValueError(span, 'starts or ends in existing file', endfile)

        if midfile is not None:
            raise ValueError(span, 'straddles an existing file', midfile)

        # Set ind, the first and last interesting indices in row:
        ind = [ None, None ]
        if isinstance(hi, tuple): ind[1] = hi[0]
        else: ind[1] = hi
        if isinstance(lo, tuple): ind[0] = lo[1]
        else: ind[0] = lo

        # Now swap from span's order to row's:
        if sign < 0: ind.reverse()
        if ind[0] is None: ind[0] = 0
        if ind[1] is None: ind[1] = len(row) - 1

        fom, tom = ind[0], ind[1] # fra-og-med, til-og-med
        if tom > fom:
            raise ValueError('Interval spans several subordinate nodes',
                             span, row[fom:tom+1])
        elif tom < fom:
            assert lo == (tom, fom) == hi
            assert fom == 1 + tom

            tom, fom = row[tom], row[fom]
            # tab, fab: True if tom, fom abut span, not across zero:
            if sign < 0: fab, tab = abutting(fom.span, span, tom.span)
            else:        tab, fab = abutting(tom.span, span, fom.span)

            assert not tab or not fab or tom.span.sign * fom.span.sign > 0
            # Grow the one further from zero, given a choice:
            if (tom.span.start - fom.span.start) * tom.span.sign > 0 and \
               tab and isinstance(tom, CacheDir): kid = tom
            elif fab and isinstance(fom, CacheDir): kid = fom
            elif tab and isinstance(tom, CacheDir): kid = tom
            else:
                # Delegate to derived class:
                raise IndexError(tom, fom)
        else:
            kid = row[tom]

        assert isinstance(kid, WriteCacheSubDir), \
               'I should have raised ValueError above' # endfile or midfile

        kid = kid._extend_(span, types)
        return kid.newfile(span, types)

    del abuts

    __changed = False # see tidy() and WriteNode._save_()
    def _onchange_(self): self.__changed = True
    def tidy(self):
        """Tidy up subordinate nodes.

        Ideally, each directory contains a dozen children, all of equal depth.
        This function sees how near that ideal it can bring this directory.\n"""

        change = False
        for node in self.listing:
            if isinstance(node, WriteCacheDir) and \
                   (node.__changed or len(node.__listing) != 12):
                if node.tidy(): change = True

        if self.__changed or change:
            self._ontidy_()
        elif len(self.__listing) == 12:
            return False # nothing to do

        raise NotImplementedError # TODO: implement

        del self.__changed # to expose the class value, False
        assert not self.__changed
        return True

    @staticmethod
    def _child_class_(isfile, mode): # configure __listing
        if isfile: return WriteCacheFile
        return WriteCacheSubDir

    @lazyattr
    def __namelen(self, ig=None, fmt=nameber.encode):
        if not self.straddles0:
            # len(self.span) is an upper bound on the .span.start of children
            return len(fmt(len(self.span)))
        # Child names use absolute values for starts
        return len(fmt(max(abs(self.listing[0].last),
                           abs(self.listing[-1].last))))

    def child_name(self, span, isfile, types, fmt=nameber.encode):
        """Determine name of a child.

        Required arguments:
          span -- range of integers to be described by the new node
          isfile -- true for a file, false for a directory
          types -- [A-Z]+ string indicating application-specific types of data
                   to be stored in the new node.

        Returns a name matching the regex of __listing and padded with enough
        leading 0s to make lexical sorting match numeric order.\n"""

        assert span >= 0 or span <= 0
        assert span.step > 0 or span <= 0
        assert span.step < 0 or span >= 0
        assert span.start * span.step >= 0

        if self.straddles0:
            if span.step < 0: sign = 'N'
            else: sign = 'P'
            name = fmt(span.start * span.step)
        else:
            name = fmt(span.start - self.span.start)

        gap = self.__namelen - len(name)
        if gap > 0: name = '0' * gap + name
        if self.straddles0: name = sign + name

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

        if self.straddles0:
            # We must set sign, and use absolute start:
            if span > -1: sign = self.sign
            else: sign = -self.sign
            start = abs(span.start)
            assert sign * self.sign * start == span.start
        else:
            if span.step * self.sign < 0: span = span.reversed()
            sign, start = None, span.start - self.span.start

        klaz = self._child_class_(True, types)
        return klaz(self.child_name(span, True, types),
                    self, types, sign, start, len(span))

class CacheSubDir (CacheSubNode, CacheDir):
    pass

class WriteCacheSubDir (WriteSubNode, CacheSubDir, WriteCacheDir):
    _child_class_ = WriteCacheDir._child_class_

    __onchange = WriteCacheDir._onchange_
    def _onchange_(self):
        self.__onchange()
        self.parent._onchange_()

    def _extend_(self, span=None, types='',
                 rename=os.rename, mkdir=os.mkdir):
        """Replace self with an expanded sub-directory.

        Optional arguments:
          span -- None or a range of integers
          types -- string of type letters, [A-Z]+ (default: '')

        If span is not None, the new sub-directory's range of integers shall
        subsume both span and self.span; otherwise, its span shall be that of
        self.  The new sub-directory shall have the union of types and
        self.types as its list of types.  All children of self shall be suitably
        renamed into the new directory and self shall be removed.

        Returns an instance of self.parent._child_class_(False, t) where t is
        the union of types and self.types (in alphabetic order).  Derived
        classes should over-load this method, taking the returned instance and
        adding appropriate attributes, based on those of self.\n"""

        ts = {}
        for t in self.types: ts[t] = None
        for t in types: ts[t] = None
        ts = ts.keys()
        ts.sort()
        types = ''.join(ts)

        # Is it sufficient to simply rename self ?
        if self.span.subsumes(span):
            if types == self.types: return self
            alias = True
        else:
            assert self.sign == self.span.step
            assert self.sign * self.span.start >= 0
            # Union (but filling in gaps to make regular):
            span = self.span.meet(span)
            if span.step * self.sign < 0: span = span.reversed()
            alias = span.start == self.span.start

        if alias:
            # Simple rename
            name = self.parent.child_name(span, False, types)
            rename(self.name, name)
            self.parent._onchange_()
            klaz = self.parent._child_class_(False, types)
            if self.parent.straddles0:
                start, sign = span.start, self.sign * self.parent.sign
            else: start, sign = span.start - self.parent.start, None
            return klaz(self.parent, types, start, len(span), sign)

        # Heigh ho - self.span.start has to change.  Create new directory, move
        # each child into it, renaming to adjust offsets as it goes.

        klaz = self.parent._child_class_(False, types)
        sign = self.sign * self.parent.sign
        if self.parent.straddles0: start = span.start * self.parent.sign
        else:
            assert sign == +1
            start, sign = self.sign * (span.start - self.parent.start), None
            assert start > 0

        name = self.parent.child_name(span, True, types)
        mkdir(name)
        self.parent._onchange_()
        peer = klaz(self.parent, types, span.start, len(span), sign)

        # FIXME: peer doesn't get to extend its attributes until this base class
        # method returns and derived classes sort that out; but children
        # probably need that to have happened already before we move them into
        # it.

        raise NotImplementedError # TODO: implement

    __newfile = WriteCacheDir.newfile # q.v. for documentation
    def newfile(self, span, types):
        if span.step * self.sign < 0: span = span.reversed()
        try: return self.__newfile(span, types)
        except IndexError, what: before, after = what.args

        assert self.depth == 1
        assert self.sign < 0 or span >= self.start
        assert self.sign > 0 or span <= self.start

        klaz = self._child_class_(True, types)
        return klaz(self.child_name(span, True, types),
                    self, types, None,
                    span.start - self.span.start, len(span))

del lazyattr, os
