"""Basic Voronoi decomposition.

Thanks to Paul Endresen for explaining the bisection tree algorithm for doing
nearest-neighbour searches efficiently.  Any flaws in the implementation are my
fault, but the clever idea behind it is one he told me.

See study.LICENSE for copyright and license information.
"""
from study.maths.vector import Vector

class Catchment (object):
    """Subdivide space according to which of a set of points is nearest.

    This is Voronoi's decomposition of the space; see class Voronoi for the
    description of the regions into which the space is thus decomposed.\n"""

    def __init__(self, centres):
        """Set up data for computation of catchment-regions.

        Single argument, centres, should be an iterable, whose entries are
        points in some vector space; if they are not Vector instances, they
        should be sequences acceptable to Vector.fromSeq().  The space is
        divided up into cells, one per point; each point's cell comprises the
        portion of the space that's closer to this point than any other.\n"""

        self.centres = tuple(map(self.__vectorise, centres))
        # Check all in the same space:
        each = iter(self.centres)
        dims = each.next().dimension
        for it in each:
            d = it.dimension
            if len(d) != len(dims) or d != dims:
                raise ValueError('Mismatched dimension', dims, d, it)
        self.__dim = tuple(dims)
        self.tree = self.__btree(self.centres)

    @staticmethod
    def __vectorise(val, V=Vector):
        if isinstance(val, V): return val
        return V.fromSeq(val)

    class BTree (object):
        def __init__(self, lo, hi): self.box = lo, hi
        @classmethod
        def __btree(cls, lo, hi): return cls(lo, hi)

        @staticmethod
        def __split(ind, lo, hi, only=Vector.delta):
            gap = hi[ind] - lo[ind]
            assert gap > 0
            q, r = divmod(gap, 2)
            if r: q = gap * .5 # but prefer an int if we got one
            return only(hi.dimension, ind) * q

        def split(self):
            lo, hi = self.box
            ind = (hi - lo).biggest
            gap = self.__split(ind, lo, hi)
            assert len(hi) == len(gap), (gap, ind, hi, lo)
            self.left, self.rite = self.__btree(lo, hi - gap), self.__btree(lo + gap, hi)
            self.cut = ind, self.left.box[1][ind]
            assert self.cut[1] == self.rite.box[0][ind]
            return self.left, self.rite

        def __contains__(self, vec):
            lo, hi = self.box
            return not vec.pointwise(lambda v, L, H: v < L or H < v, None, lo, hi)

        def locate(self, vec):
            here = self
            while True:
                try: ind, cut = here.cut
                except AttributeError: break
                here = here.rite if vec[ind] > cut else here.left
                # equivalent to, but cheaper than:
                # here = left if vec in left else rite
            assert vec in here
            return here

        def distance(self, vec):
            lo, hi = self.box
            off = vec.pointwise(lambda v, L, H: L - v if v <= L else H -v if v >= H else 0,
                                 None, lo, hi)
            far = vec.pointwise(lambda v, L, H: H - v if 2 * v < L + H else L - v,
                                 None, lo, hi)
            return off.squaresum ** .5, far.squaresum ** .5

    @staticmethod
    def __btree(ps, each=1, Tree=BTree):
        """Builds a BTree with at most each of the ps in each leaf of the tree.

        Saves, as each leaf's .points, the list of entries from ps that are in
        the leaf.  Points on boundaries are placed in one of the adjacent
        leaves; clients should make no assumption about which.\n"""
        root = Tree(ps[0].pointwise(lambda *xs: min(xs), None, *ps[1:]),
                    ps[0].pointwise(lambda *xs: max(xs), None, *ps[1:]))

        live = [ ( root, ps ) ]
        while live:
            tree, ps = live.pop()
            if len(ps) > each:
                left, rite = tree.split()
                ind, cut = tree.cut
                ls, rs = [], []
                for p in ps: (rs if p[ind] > cut else ls).append(p)
                live += [ (left, tuple(ls)), (rite, tuple(rs)) ]
            else: # sub-divided enough; record which points are in this leaf:
                assert all(p in tree for p in ps)
                tree.points = tuple(ps)

        return root
    del BTree

    from itertools import chain
    @staticmethod
    def __nearleaves(where, tree, same, byturns=chain):
        near, best = tree.distance(where)
        live, more = [ ( tree, near ) ], False

        # Invariants: best is the max distance from where of any point in tree's
        # box; tree is the live BTree node for which this is least, among those
        # we've seen; more is true when we've split a node since last our
        # scanning showed us tree.  We loop over the live BTree nodes, splitting
        # those we can and eliminating those in which the nearest point can't
        # be.

        while True:
            here, near = live.pop()
            if near > best: continue # we can ignore this sub-tree

            try: pair = here.left, here.rite
            except AttributeError:
                if here.points and (same or here.points != (where,)):
                    live.insert(0, (here, near)) # put it back
                if here is tree:
                    if more: more = False
                    else: break # we're looping
            else:
                more = True
                for here in pair:
                    try:  # if it's a leaf with no points, ignore it:
                        if not here.points: continue
                    except AttributeError: pass # not a leaf
                    near, far = here.distance(where)
                    if near < best:
                        if best < far:
                            # Put it at the back of the queue
                            live.insert(0, (here, near))
                        else:
                            best, tree = far, here
                            # Reprocess promptly, as splitting it is apt to improve best:
                            live.append((here, near))
        assert live
        # Remaining leaves overlap, need to check all points in them.
        return byturns(*tuple(it[0].points for it in live))
    del chain

    def nearest(self, where, same=True):
        """Find a nearest entry in self.centres to where.

        Required first argument, where, is a vector (or tensor, of the same kind
        as self.centres).  Optional second argument, same, indicates whether
        where itself can be used as nearest point, if it happens to be one of
        self's centres; it defaults to True - set it to False to find the
        nearest non-equal point.  Returns a twople (p, d) where p is in
        self.centres and d*d == (where-p).squaresum is minimal among p in
        self.centres (although there is no guarantee this is unique).\n"""

        ps = self.__nearleaves(self.__vectorise(where), self.tree, same)
        centre = ps.next()
        sumsqr = (centre - where).squaresum
        for p in ps:
            if not same and p == where: continue
            rr = (p - where).squaresum
            if rr < sumsqr: centre, sumsqr = p, rr

        return centre, sumsqr ** .5

class Voronoi (object):
    # TODO: boundary-representation of the convex hulls, one per point.
    pass

del Vector
