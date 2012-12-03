"""Basic Voronoi decomposition.

Thanks to Paul Endresen for explaining the bisection tree algorithm for doing
nearest-neighbour searches efficiently.  Any flaws in the implementation are my
fault, but the clever idea behind it is one he told me.

See study.LICENSE for copyright and license information.
"""
from study.maths.vector import Vector

class Catchment (set):
    """Subdivide space according to which of a set of points is nearest.

    This is Voronoi's decomposition of the space; see class Voronoi for the
    description of the regions into which the space is thus decomposed.  Records
    the result of ingesting its constructor's argument (see .__init__()) as
    .centres; has one method, .nearest(vec [, elide]), that finds the nearest in
    .centres (but not in elide, if passed) to the given vec.\n"""

    __upnew = set.__new__
    def __new__(cls, centres, count=1):
        if count < 1: raise ValueError('Demanding unrealistic subdivision', count)
        return cls.__upnew(cls, map(cls.__vectorise, centres))

    __upinit = set.__init__
    def __init__(self, centres, count=1):
        """Set up data for computation of catchment-regions.

        Required first argument, centres, should be an iterable, whose entries
        are points in some vector space; if they are not Vector instances, they
        should be sequences acceptable to Vector.fromSeq().  Optional second
        argument, count, is an upper bound on the number of points to include in
        each leaf of the bisection tree built internally; it must be at least 1
        - its default (and recommended) value.

        Stores, in self as a set, Vector (see study.maths.vector) instances
        (made using Vector.fromSeq where needed) representing the entries in
        centres.\n"""

        self.__upinit(centres)

        # Check all in the same space:
        each = iter(self)
        try: dims = each.next().dimension # StopIteration if no centres, which would be silly
        except StopIteration, what:
            raise apply(StopIteration, what.args + (self,))
        for it in each:
            d = it.dimension
            if len(d) != len(dims) or d != dims:
                raise ValueError('Mismatched dimension', dims, d, it)
        self.__dim = tuple(dims)
        self.__root, self.__ineach = self.__btree(tuple(self), count), count

    @staticmethod
    def __vectorise(val, V=Vector):
        return val if isinstance(val, V) else V.fromSeq(val)

    # Support for .__btree()
    class BTree (object):
        """Bisection tree.

        A coordinate-aligned cuboid box that knows how to be split in two.  Has,
        as its .box attribute, a pair of vectors (low, high); the cuboid it
        describes has .box[0][ind] <= vec[ind] <= .box[1][ind] for each vec in
        the cuboid and each full-rank index, ind.  Supports 'vec in self'
        testing based on this.

        If it's been .split(), it has child notes .left and .rite and its .cut
        records the coordinate at their boundary; otherwise these attributes are
        unset.  The value of .cut (when set) is a twople whose first entry is
        the index of the coordinate and whose sencond entry is the value of this
        coordinate at the boundary between the two child nodes.

        Has .parent set to the node whose .split() created it as .left or .rite,
        if this is how it was created; otherwise, doesn't have .parent set (so
        AttributeError is the easy way out of traversal up the .parent
        hierarchy).  Methods:

          .split() -- bisect self on a suitable coordinate plane.
          .locate(vec) -- find which cell a given vec is in
          .distance(vec) -- range of distance between vec and points in self.box

        Our one client, Catchment, shall set .points to a list of points in the
        box (but no method of this class pays any attention to .points).\n"""
        def __init__(self, lo, hi, up=None):
            self.box = lo, hi
            if up is not None: self.parent = up
        # TODO: make parent a weak attribute, so that garbage collection is happy.

        # Tools for use by split():
        def __child(self, lo, hi): return self.__class__(lo, hi, self)

        @staticmethod
        def __split(deep, lo, hi, only=Vector.delta):
            """Finds the right index and mid-value on which to split a box.

            Uses deep (from __depth(), below) to determine how recently each
            index has been split on; amongst the least recently used, finds the
            one for which hi-lo has the largest component, i.e. the box's width
            is biggest.  Returns a twople of this index with the vector that's
            half the box's width in this direction.\n"""
            dex, was = (hi - lo).iteritems(), 0
            while not was:
                try: big, was = dex.next()
                except StopIteration: # lo == hi
                    raise ValueError('Empty box', lo, hi)
                assert was >= 0
            at = deep(big)

            for ind, val in dex:
                assert val >= 0
                if val: # don't try to cut on a zero-width direction !
                    dp = deep(ind)
                    if at < dp or (dp == at and val < was):
                        at, was, big = dp, val, ind

            gap = hi[big] - lo[big]
            assert gap > 0
            q, r = divmod(gap, 2)
            if r: q = gap * .5 # but prefer an int if we got one
            return big, only(hi.dimension, big) * q

        def __depth(self):
            """Returns a function to compute depth below last split on an index.

            The returned function returns one more than self's depth in the tree
            when we haven't yet split on the index given.\n"""
            up, prior = self, []
            try:
                while True:
                    up = up.parent # AttributeError at root
                    assert hasattr(up, 'cut')
                    prior.append(up.cut[0])
            except AttributeError: pass

            def deep(ind, p=prior):
                try: return p.index(ind)
                except ValueError: return len(p)

            return deep

        def split(self):
            """Split self in half in one coordinate direction.

            The chosen coordinate direction is the least recently used, among
            splits in the tree down to self, preferring those with bigger width
            among those on which we haven't split previously.\n"""

            lo, hi = self.box
            ind, half = self.__split(self.__depth(), lo, hi)
            assert len(hi) == len(half), (half, ind, hi, lo)
            self.left, self.rite = self.__child(lo, hi - half), self.__child(lo + half, hi)
            self.cut = ind, self.left.box[1][ind]
            assert self.cut[1] == self.rite.box[0][ind]
            return self.left, self.rite

        def __contains__(self, vec):
            lo, hi = self.box
            return not vec.pointwise(lambda v, L, H: v < L or H < v, None, lo, hi)

        def locate(self, vec):
            """Returns the leaf node containing a given vector.

            Identifies the leaf node, in the tree under self, in which vec
            appears; or the one closest to vec, if vec is not in self.\n"""

            here = self
            while True:
                try: ind, cut = here.cut
                except AttributeError: break
                here = here.rite if vec[ind] > cut else here.left
                # For vec in self, this is equivalent to, but cheaper than:
                # here = left if vec in left else rite

            assert vec in here or not vec in self
            return here

        def distance(self, vec):
            """Returns the range of distances, in self's box, from a given vector.

            Required argument, vec, is the vector to measure from.  Returns a
            twople of the distances from vec to its nearest and furthest points
            in self.\n"""
            lo, hi = self.box
            off = vec.pointwise(lambda v, L, H: L - v if v <= L else H -v if v >= H else 0,
                                 None, lo, hi)
            far = vec.pointwise(lambda v, L, H: H - v if 2 * v < L + H else L - v,
                                 None, lo, hi)
            return off.squaresum ** .5, far.squaresum ** .5

    @classmethod
    def __btree(cls, ps, n=1, Tree=BTree):
        """Builds a BTree with at most n of the ps in each leaf of the tree.

        Saves, as each node's .points, the list of entries from ps that are in
        the node.  Points on boundaries are placed in one of the adjacent nodes,
        at each split; clients should make no assumption about which.\n"""
        root = Tree(ps[0].pointwise(lambda *xs: min(xs), None, *ps[1:]),
                    ps[0].pointwise(lambda *xs: max(xs), None, *ps[1:]))
        cls.__split(root, ps, n)
        return root
    del BTree

    @staticmethod
    def __split(root, ps, each):
        live = [ ( root, ps ) ]
        while live:
            tree, ps = live.pop()
            assert all(p in tree for p in ps)
            tree.points = frozenset(ps)

            if len(ps) > each: # else we can let tree be a leaf
                left, rite = tree.split()
                ind, cut = tree.cut
                ls, rs = [], []
                for p in ps: (rs if p[ind] > cut else ls).append(p)
                live += [ (left, tuple(ls)), (rite, tuple(rs)) ]

    # Critique: self.__root.points is a set with the same membership as self
    # (just lacking the extra methods of Catchment).  This strongly hints that
    # self and self.__root could be unified in some way, with each sub-tree as a
    # subset.  It would change the semantics of __contains__(); rename the
    # geometric one to encloses().

    # API of set:
    __upadd = set.add
    def add(self, point):
        if point.dimension != iter(self).next().dimension:
            raise ValueError('Incompatible dimension', point.dimension)
        self.__upadd(point)

        if point in self.__root:
            # We can insert it into the existing BTree:
            tree = self.__root.locate(point)
            self.__split(tree, tree.points.union((point,)), self.__ineach)

            while True: # run up to root, adding point to .points of each node:
                try: tree = tree.parent
                except AttributeError: break
                tree.points = tree.points.union((point,))

        else: # Need a new BTree:
            self.__root = self.__btree(tuple(self), self.__ineach)

    __uprm = set.discard
    def discard(self, point):
        if point not in self: return
        self.__uprm(point)
        self.__rm(point, self.__root)

    @staticmethod
    def __rm(point, root):
        assert point in root.points # which is what we need to fix
        tree, point = root.locate(point), set((point,))
        while True:
            tree.points -= point
            # TODO: unsplit if one child is empty and the other a leaf.
            try: tree = tree.parent
            except AttributeError: break

    __uppop = set.pop
    def pop(self): # ... or suppress this method as a silly one to use ?
        ans = self.__uppop()
        self.__rm(ans, self.__root)
        return ans

    # Support for .nearest()
    from itertools import chain
    @staticmethod
    def __nearleaves(where, tree, elide, byturns=chain):
        """Find the leaf nodes in which the closest point to where could lie.

        Arguments:
          where -- the point;
          tree -- the tree to traverse;
          elide -- collection of points to ignore, in .points attributes.

        Expands each split in tree so as to discard (without splitting) every
        box whose nearest point to where is further than the furthest point of
        some other box; repeatedly splits the box, whose furthest point from
        where is nearer than that of any other, so as to be able to discard as
        many futher boxes as soon as possible.  Each box retained, at the end,
        is a leaf with at least some point in it closer to where than the
        furthest point of every retained box.  The return is an iterator over
        the points in the boxes retained; these are the points that might be
        nearest to where.\n"""

        near, best = tree.distance(where)
        live, more = [ ( tree, near ) ], False
        if elide: empty = lambda ps, ig=elide: not [ p for p in ps if p not in ig ]
        else: empty = lambda ps: not ps
        if empty(tree.points):
            raise ValueError('Ignoring all points leaves none to be nearest', elide)

        # Invariants: best is the max distance from where of any point in tree's
        # box; tree is the live BTree node for which this is least, among those
        # we've seen; more is true when we've split a node since last our
        # scanning showed us tree.  We loop over the live BTree nodes, splitting
        # those we can and eliminating those in which the nearest point can't
        # be.  There are no empty() entries in live.
        while True:
            here, near = live.pop()
            if near > best: continue # we can ignore this sub-tree

            try: pair = here.left, here.rite
            except AttributeError:
                live.insert(0, (here, near)) # Move to the back of the queue
                if here is tree: # Are we done yet ?
                    if more: more = False
                    else: break # no further refinement possible

            else: # split node
                more, shift = True, here is tree
                for here in pair:
                    # if it's a leaf with no points, ignore it:
                    if empty(here.points): continue

                    near, far = here.distance(where)
                    if near < best: # else ignore here
                        # When splitting tree itself (shift is true), one of its
                        # children must be at least as good as it and we must
                        # switch to using that as tree, so that the "Are we done
                        # yet ?" does get triggered.  If splitting some other
                        # node equals tree, but doesn't beat it, then we'll
                        # still be done when we get back to tree in the loop, so
                        # no need to change which node we think is best.
                        if (far <= best if shift else far < best):
                            # Improvement (or replacing tree with equal child):
                            best, tree = far, here
                            # Reprocess promptly, as splitting it is apt to improve best:
                            live.append((here, near))
                        else: live.insert(0, (here, near)) # Back of the queue.

        assert live
        # Remaining leaves overlap, need to check all points in them.
        return byturns(*tuple(it[0].points for it in live))
    del chain

    def nearest(self, where, elide=()):
        """Find a nearest entry in self to where.

        Required first argument, where, is a vector (or tensor, with the same
        .dimension as the members of self).  Optional second argument, elide, is
        a container (we must be able to do 'p in elide' tests) of points in self
        to ignore when searching for the one nearest to where; defaults to
        ().  Returns a twople (p, d) where p is in self and d*d ==
        (where-p).squaresum is minimal among relevant p (in self but not in
        elide) - although there is no guarantee this is unique.\n"""

        ps = self.__nearleaves(self.__vectorise(where), self.__root, elide)
        centre = ps.next()
        sumsqr = (centre - where).squaresum
        for p in ps:
            if p in elide: continue
            rr = (p - where).squaresum
            if rr < sumsqr: centre, sumsqr = p, rr

        return centre, sumsqr ** .5

class Voronoi (object):
    # TODO: boundary-representation of the convex hulls, one per point.
    pass

del Vector
