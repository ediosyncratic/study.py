"""Some graph-theoretic stuff.

$Id: graph.py,v 1.3 2000-02-19 15:37:11 eddy Exp $
"""

class Unite:
    """Builds a partition, from a discrete one, by unions.

    Instances support len() and, for indices in range(len()), a query,
    .peers(index), for the list of indices in the same part of the partition.  A
    new nodes may be added by calling .append(len()) [it is presumed that you
    know the len() without having cause to compute it; and that you want an
    assertion that you got it right]; the new node is, initially, the sole
    member of a new part.  Two nodes in distinct parts may be passed to .join()
    to combine the two parts into one; .join() should never be called with nodes
    already connected. """

    def __init__(self):
        self.__forward = []
        self.__backward = []

    def __len__(self): return len(self.__forward)
    # assert: equal to len(self.__backward)

    def peers(self, node):
        """Returns the component containing a given node.

        Result is a list of nodes in the given node's component.
        If it ever involves duplicates, something's gone wrong.
        Probably so wrong you won't see the answer ... """

        f, n, row = self.__forward, node, []
        while 1:
            row.append(n)
            n = f[n]
            assert b[n] == row[-1]
            if n is node: return row

    def append(self, ind):
        assert len(self.__forward) == ind == len(self.__backward), \
               "Unite's append requires the right value"

        self.__forward.append(ind)
        self.__backward.append(ind)

    def join(self, nod, ver):
        """Joins two disjoint components.

        Arguments are nodes, one from each component.

        The nodes must not be peers: this is *not* checked.
        They will be peers after join is called. """

        f, b = self.__forward, self.__backward
        pod, wer = f[nod], f[ver]
        assert b[pod] is nod and b[wer] is ver
        # Do the swaps:
        b[pod], b[wer] = ver, nod
        f[nod], f[ver] = wer, pod

class Find:
    def __init__(self):
        self.__up = []
        self.__count = []

    def __len__(self): return len(self.__up)
    # assert: always equal to len()s of the other lists

    def __chase(self, node):
        """Chases the __up chain from a node to its top.

        Single argument is a node (index): returns the node reached from this by
        chasing i->self.__up[i] to a fixed point.  Side-effect: changes the
        __up[] of all nodes it visits on the way to point at the answer
        returned, so as to speed this chase next time around. """

        trail = []
        while 1:
            n = self.__up[node]
            if n is node: break
            trail.append(node)
            node = n

        # All but the last item in trail need to be pointed at node:
        for n in trail[:-1]: self.__up[n] = node
        return node

    def joined(self, node, *nodes):
        """Do the edges seen thus far connect (all) the given nodes ?

        Arguments are node indices; at least one must be given.  Result is true
        precisely if all nodes are in the same connected component of the graph
        of edges thus far passed to self.join(). """

        nod = self.__chase(node)
        for vertex in nodes:
            if self.__chase(vertex) != nod:
                return None # i.e. No.

        return 1>0 # i.e. Yes

    def append(self, ind):
        assert len(self.__count) == ind == len(self.__up), \
               "Find's append requires the right value"

        # Lazily grow the lists: node is initially alone.
        self.__up.append(ind)
        # New node's sub-tree contains only one node: itself.
        self.__count.append(1)

    def join(self, node, vertex):
        """Joins two nodes.

        Returns None if nodes were already joined: else, a true value.
        Consequently its return is what `not self.join()' would have yielded
        previously. """

        nod, ver = self.__chase(node), self.__chase(vertex)
        if nod is ver: return # nothing to do

        # Who has the bigger sub-tree ?
        count = self.__count # we refer to it 5 times, may as well use local ref
        if count[nod] < count[ver]: nod, ver = ver, nod # swap

        # Hang smaller tree below bigger:
        self.__up[ver] = nod
        count[nod] = count[nod] + count[ver]

        return 1>0      # did something

    def disjoint(self):
        """Returns a sample member from each connected component. """
        ans = []
        for it in self.__up:
            if it not in ans:
                ans.append(it)

        return ans

class FindUnite (Find, Unite):
    """An implementation of the find-unite algorithm.

    Create an instance of FindUnite(); for each edge in a graph, identify the
    two ends and invoke the instance's .join(thisend, thatend).  At any stage,
    invoke .peers(node) to get a list of all nodes in the given one's connected
    component. """

    def __init__(self):
        """Initialises an empty FindUnite."""

        Unite.__init__(self)
        Find.__init__(self)

    def append(self, ind):
        Unite.append(self, ind)
        Find.append(self, ind)

    def join(self, node, vertex):
        """Joins a given pair of nodes.

        Takes two arguments: the end-nodes of an edge in your graph.  Joins
        their connected components, if disjoint, together.  Nodes not previously
        known to self are added, initially connected to nothing; they are then
        joined as for familiar nodes. """

        # Unite doesn't want to join things unless they didn't used to be.
        # Find knows whether they're already joined:
        if Find.join(self, node, vertex):
            # Find did something: get Unite in on the act.
            Unite.join(self, node, vertex)

    def partition(self):
        return map(self.peers, self.disjoint())

class Graph:
    def __init__(self):
        self.__edges, self.__nodes, self.__connect = [], [], FindUnite()

    # Read-only copies:
    def nodes(self): return tuple(self.__nodes)
    def edges(self): return tuple(self.__edges)

    def joined(self, *nodes):
        try: indices = map(self.__nodes.index, nodes)
        except ValueError: return None

        return apply(self.__connect.joined, indices)

    def __node(self, node):
        """Returns internal index of node.

        If node has been met before, an index has been recorded for it: this is
        returned.  Otherwise, the node is added to internal datastructures with
        a previously-unused index, which is returned.  For internal use only. """

        # Could be more efficient using an inverse-lookup here.
        try: return self.__nodes.index(node)
        except ValueError: pass

        ind = len(self.__nodes)
        self.__nodes.append(node)

        self.__connect.append(ind)
        return ind

    def join(self, start, stop):
        self.__edges.append((start, stop))
        self.__connect.join(self.__node(start), self.__node(stop))

    def sub(self, *nodes):
        """Returns a full sub-graph containing the given nodes. """

        all = []
        for node in nodes:
            if node not in all:
                all = all + self.peers(node)

        ans = Graph()
        for a, b in self.__edges:
            if a in all:
                assert b in all
                ans.join(a, b)
            else:
                assert b not in all

        return ans

    def peers(self, node):
        try: nod = self.__nodes.index(node)
        except ValueError: return [ node ]
        return self.__peers(nod)

    def __peers(self, nod):
        return map(lambda i, _r=self.__nodes: _r[i], self.__connect.peers(nod))

    def partition(self):
        """Returns a list of connected components of self.

        Each connected component is represented as a list (whose order is
        arbitrary) of nodes.  Each node known to self appears in exactly one
        connected component. """

        return map(self.__peers, self.__connect.disjoint())

"""
 $Log: graph.py,v $
 Revision 1.3  2000-02-19 15:37:11  eddy
 Carved everything up better.

 Revision 1.2  2000/02/19 13:37:30  eddy
 Moved all node/index translations to Graph so FindUnite's node values
 are all indices into lists.  Now to carve up FindUnite.

 Initial Revision 1.1  2000/02/19 12:18:45  eddy
"""
