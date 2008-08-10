"""Cacheing data about integers.

This module provides classes to manage caches of data about the integers.  It
presumes that you have some way of computing some information about integers for
which it is worth saving the answers to disk (for example, because the cost of
re-computation is high, possibly due to needing answers from smaller integers to
arrive at answers for bigger ones).  The cache managed by this system just
remembers the information you computed, in python modules importable with
execfile, organized in a directory hierarchy, with an __init__.py in each
directory to manage the hierarchy itself.  There may be gaps in such caches, a
system may use several of them with distinct roots and a system may remember
several (up to about 26) distinct types of information in a single cache.

The primary use case motivating the initial implementation of this
infrastructure is tracking information about primeness and least common factors
for the naturals (non-negative integers); the intent here is to be generic, but
it is possible some design features should be refactored out to maths.prime in
order to make these classes function truly generically.

Nodes in the hierarchy have names of form
        \([NP]\)\?\([0-9a-z]+\)\([A-Z]+\)\([0-9a-z]+\)\(\.py\)\?
wherein:
 * the optional initial [NP] (for negative, positive) is only present if the
   parent directory's span includes both negative and positive integers; this
   only ever arises when the parent directory is the root of a cache
 * the \([A-Z]\) could in fact be \([^0-9a-z]+\) and provides
   application-specific information about the types of data, about integers,
   that are present in this node and any descendants
 * the final \(\.py\)\? is only present for leaf nodes, i.e. files
 * the two \([0-9a-z]+\) are interpreted via int(,36) as naturals; the first is
   the start-point (with sign indicated by [NP] if present, else by ancestor in
   the hierarchy if any has such a prefix, defaulting to positive if no ancestor
   does) and the second is the length (measured away from zero)
 * the start-point in the preceding is always indicated relative to that of the
   node's parent directory, measured away from zero (with sign implied by
   nearest ancestor having [NP] prefix, if any, else tacitly positive).

Thus N0X42.py would be a file providing application-specific information of
category 'X'; it could only appear in the root directory of its hierarchy, it
and would relate to the integers from 0 down to minus one hundred and fourty-six
(i.e. 4*36 + 2).  In all likelihood, there would be something in the same cache
root directory whose name would start P1X, providing the same kind of
information for positive integers up to some limit.

$Id: whole.py,v 1.4 2008-08-10 16:04:22 eddy Exp $
"""
import os
from errno import EWOULDBLOCK
from study.snake.regular import Interval
from study.snake.sequence import Ordered
from property import Cached, lazyattr, lazyprop
from weak import weakattr
# TODO: refactor much of maths.prime.cache into here.
# Move assorted compact data read from disk into a _cache mapping from which
# attributes can be derived.  Let load and save mediate maintenance of those;
# generic classes just manage "range of naturals" data derived from names.

class Node (Cached):
    """Base-class for nodes in a hierarchy of cached data about naturals.

    Derived classes should implement attribute _cache_file (absolute path of
    file to be loaded by load; use __init__.py in directories), implement lock
    and unlock methods (with optional boolean arguments (read, write) each
    defaulting False, raising IOError or returning False on failure), extend
    load and implement a (typically lazy) attribute span, describing the range
    of naturals this Node describes.\n"""

    @lazyattr
    def _cache(self, ig=None): return {}

    @weakattr
    def content(self, ig=None): return self.load()

    class Bok (dict): pass # for weakref's sake
    def load(self, bok=None, glo={}, Dict=Bok, BLOCKED=EWOULDBLOCK):
        """Loads self._cache_file and returns the result as a mapping.

        Sole optional argument, bok, is a mapping object into which the
        cache-file's namespace is to be imported; if not supplied, an empty
        mapping of a class based on dict (so amenable to weakref) is used; in
        either case, this is the object returned on success.  May raise IOError
        or ParseError.

        Derived classes extending this should take the same optional argument,
        pass it down to base-class load and do any processing of the result,
        e.g. extracting data into self._cache, on the way back out.\n"""
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
    def save(self, formatter=None, **what):
        """Saves a given namespace to a module.

        First argument, formatter, is None (to use a default) or a function
        taking a key-value pair and outputting a string to save to file, that
        shall be used to record that key-value pair.  If None, a default is used
        which formats the pair as a simple assignment statement with the key on
        the left and the value's repr() on the right, followed by a newline.
        Derived classes may introduce special handling for some keys; only keys
        with no special handling shall be passed to formatter.  This class
        provides special handling for 'depth' and '__doc__' (see below).

        All other arguments should be given in keyword form.  The name 'depth'
        shall be ignored if given (it is over-written by an attribute of the
        same name) nodes not descended from CacheFile (whose depth is 0).  The
        name '__doc__' shall be replaced by self.__doc__ if this is not the same
        object as self.__class__.__doc__; any __doc__ obtained by either of
        these means shall be saved as doc-string in the module.\n"""

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

                if formatter is None:
                    for k, v in what.items():
                        fd.write('%s = %s\n' % (k, repr(v)))
                else:
                    for k, v in what.items():
                        fd.write(formatter(k, v))

            finally: fd.close()
        finally: self.unlock(write=True)
        # potentially: return size of file

    __BLOCKED = EWOULDBLOCK

class SubNode (Node):
    def __init__(self, parent, start, reach, types,
                 Span=Interval):
        self.__span, self.__type, self.__up = Span(start, reach), types, parent

    def lock(self, read=False, write=False):
        return self.root.lock(read, write)
    def unlock(self, read=False, write=False):
        return self.root.unlock(read, write)

    # read-only access to data members
    @property
    def types(self, ig=None): return self.__type
    @lazyattr
    def span(self, ig=None): return self.__span + self.__up.span.start
    @property
    def parent(self, ig=None): return self.__up
    # Chase parent to its root:
    @property
    def root(self, ig=None): return self.__up.root

class Gap (SubNode):
    __upinit = SubNode.__init__
    def __init__(self, parent, start, span):
        self.__upinit(parent, start, span, '')

class WriteSubNode (SubNode, WriteNode):
    __upsave = WriteNode.save
    def save(self, formatter=None, **what):
        self.__upsave(formatter, **what)
        run = self
        while run is not self.root:
            run = run.parent
            run.changed = True

from weak import WeakTuple
class CacheDir (object):
    @lazyattr
    def _cache_file(self, ig=None):
        return self.path('__init__.py')

    import re
    @lazyattr
    def __listing(self, ig=None, get=os.listdir, seq=Ordered,
                pat=re.compile(r'^([0-9a-z]+)([A-Z]+)([0-9a-z]+)(\.py)?$')):
        """The (suitably sorted) list of contents of this directory.

        Ignores entries not matching the forms of cache file names.\n"""
        ans = seq()
        for name in get(self.path()):
            got = pat.match(name)
            if got is not None:
                start, span = int(got.group(1), 36), int(got.group(3), 36)
                ans.append((start, span, name, got.group(2), got.group(4)))

        return ans
    del re

    def depth(self, ig=None): # but usually we'll read this from __init__.py
        return max(self.listing.map(lambda x: x.depth)) + 1
    depth = lazyprop('depth', depth)

    def _onchange(self):
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
                start, size, name, mode, isfile = g(s)[ind]
                klaz = s._child_class_(isfile, mode)
                assert issubclass(klaz, CacheDir._child_class_(isfile, mode))
                return klaz(s, name, start, size, mode)
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
        the fact that .listing is in sync with .__listing; so filter the latter
        to find the entry we want, then take the corresponding entry from the
        former.  This ensures that all @weaklistings re-uses the same objects as
        entries, rather than creating duplicate objects.\n"""
        __upinit = WeakTuple.__init__
        def __init__(self, cdir, getseq, test):
            def get(ind, s=cdir, g=getseq, f=test):
                j = 0
                for it in g(s):
                    if f(it[3]):
                        if ind: ind -= 1
                        else:
                            assert f(s.listing[j].types)
                            return s.listing[j]
                    j += 1

                raise IndexError
            self.__upinit(get)
            self.__who, self.__att, self.__test = cdir, getseq, test

        def __len__(self): return len(filter(self.__test, self.__att(self.__who)))

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

        def get(self, ig=None, test=lambda r, p=picker: p(r[3]), S=W):
            return S(self, lambda s: s.__listing, test)

        return L(get, doc=picker.__doc__)

    del WeakSubSeq

    @staticmethod
    def _gap_(before, after, limit, Range=Gap):
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
            assert hasattr(limit, 'range') # and it's an instance of Interval

            if start is None: start = limit.range.start
            elif limit.range.start is None: pass
            elif limit.range.start > start: start = limit.range.start

            if stop is None: stop = limit.stop
            elif limit.range.stop is None: pass
            elif limit.range.stop < stop: stop = limit.range.stop

            if parent is None: parent = limit.parent

        assert parent is not None
        return Range(parent, start, stop)

    def bchop(row, value, attr):
        lo, hi = 0, len(row)
        loa, hia = getattr(row[lo], attr), getattr(row[hi], attr)
        if loa.start > value: raise ValueError(None, row[lo])
        elif hia.stop <= value: raise ValueError(row[hi], None)
        if loa.stop > value: return row[lo]
        if hi.start <= value: return row[hi]
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
        raise ValueError(row[lo], row[hi])

    def locate(self, value, attr='span', gap=None, seq='listing', types=None,
               chop=bchop):
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
                    if t not in it[3]: break
                else:
                    row.append(self.listing[i])
                i += 1

        try: kid = chop(row, value, attr)
        except ValueError, what:
            return self._gap_(*what.args)

        if isinstance(kid, CacheDir):
            return kid.locate(value, attr, gap, seq, types)

        assert isinstance(kid, CacheFile)
        return kid

    del bchop
del WeakTuple

from study.crypt.base import intbase
nameber = intbase(36) # its .decode is equivalent to int(,36) used above.
del intbase

class WriteCacheDir (CacheDir):
    def newfile(self, range, types):
        """Find where to create a new file to be added.

        Required arguments:
          range -- the range of integers to be described by the file
          types -- an [A-Z]+ text specifying application-specific types of
                   information about integers to be stored in the new file.

        Returns a self._child_class_(True, types) instance in the hierarchy
        under self, into which the client may save its data of the given types
        for the given range of integers.  The application should subsequently
        call self.tidy() to ensure consistency of the resulting cache.\n"""

        # TODO: implement
        raise NotImplementedError

    changed = False # see tidy() and WriteSubNode.save()
    def tidy(self):
        """Tidy up subordinate nodes.

        Ideally, each directory contains a dozen children, all of equal depth.
        This function endeavours to make that true of this directory.

        When it comes to simply growing an isolated cache, containing only one
        type of data, with no data borrowed from other caches, the top-level
        node has to endure two adjacent depths some of the time; the deeper ones
        are all complete and tidy as are all but the last of the shallower ones;
        there is a bleeding-edge of last nodes with between 8 and 20 children,
        at each depth, all others being tidy.  When a new node is added, it is
        always a new last file so it is added to the depth=1 last node; if a
        non-root last node finds it has 20 children, it keeps its first 12
        children and ceases being a last node by passing these to its parent, to
        turn into a new last node; this may take that parent up to 20 children,
        in which case it does likewise, unless it's the root.  When the root has
        20 children all of equal depth, it collects 12 into a sub-node of one
        greater depth, retaining the other 8 as direct children; successive
        additions to the last of these cause it to push new children of their
        depth into the root; when it has n < 8 deeper nodes and 20-n > 12
        shallower ones, it collects 12 shallow ones into a fresh deeper node;
        when it has 7 or more deeper nodes and 8 or more shallower ones, it can
        collect the shallow ones into a new deeper node and get itself back to
        having only one depth of child.

        The situation is more complex when borrowing from another cache, or when
        we have borrowed from a cache in the past, that we no longer see.  If
        we're borrowing from a simple cache, as above, then our cache only saves
        data past the end of the other, so the dynamics are as before.  This
        creates a cache that covers a range of the integers, albeit one distant
        from zero.  If we borrow from several such caches, not necessarily
        including the ones that they referenced while being built, we have a
        read-cache with holes in it and we want to grow a write-cache to fill
        the holes.  This write-cache then ends up covering several disjoint
        ranges of the integers.  It could be structured as above, but I chose to
        place gaps in the intervals between directories.

        The situation is further complicated in a mixed cache, with some diverse
        types of application-specific information.  An interval may be covered
        by the types taken together without being covered entirely by any one of
        them.  New nodes added to extend one type may fall in the middle of the
        range of values for all types taken together.

        Policy:
         * each child of a root is tidy and describes a contiguous block of
           integers;
         * each root is untidy only in so far as that makes necessary;
         * each such contiguous block potentially has, at low end and at high
           end, a sub-directory of boundary nodes whose sole untidiness is that
           they may have anywhere between 8 and 20 sub-nodes (but all subnodes
           are of equal depth);
         * as the leaf at depth zero, for each type of data, such a chain may
           include one node with only partial data of that kind;
         * The nominal range of naturals described by a node shall always
           subsume the union of the nominal ranges of its children;
         * The nominal range of naturals described by a node may, at each end,
           stretch beyond the end of its actual data, so long as it does not
           overlap its sibling on the relevant side.

        When two chunks, each contiguous, have expanded towards each other far
        enough that they meet, unifying them shall involve a messy interaction,
        where changes ripple through them to ensure tidiness in the internal
        child nodes.  This should be mediated by the nearer-zero node preserving
        such tidiness as it has, with its neighbour transfering nodes into it as
        if the nearer-zero node were simply having nodes added to it after the
        manner of simple growth.\n"""
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

from lockdir import LockableDir

class CacheRoot (CacheDir, LockableDir, Node):
    __gapinit = Node.__init__
    __dirinit = LockableDir.__init__
    def __init__(self, path):
        self.__dir = path
        self.content # evaluate in order to force a .load()
        self.__gapinit(0, self.stop)
        self.__dirinit()

    parent = None
    @property
    def root(self, ig=None): return self
    def path(self, *tail): return self.__path(tail)
    def __path(self, tail, join=os.path.join): return join(self.__dir, *tail)

    __load = Node.load
    def load(self, bok=None):
        bok = self.__load(bok)
        self.stop = bok.pop('top')
        return bok

del LockableDir

class WriteCacheRoot (_WriteCacheDir, CacheRoot, WriteNode):
    __save = WriteNode.save
    def save(self, **what):
        what['top'] = self.stop
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
    # TODO: needs to know how to rename itself when its range expands
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
