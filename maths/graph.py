"""Some graph-theoretic stuff.

$Id: graph.py,v 1.2 2000-02-19 13:37:30 eddy Exp $
"""

class NodeError(KeyError):
    """Exception used by graph.py's tools to signal `no such node'."""

class Partition:
    def __init__(self): pass

class FindUnite:
    """An implementation of the find-unite algorithm.

    Create an instance of FindUnite(); for each edge in a graph, identify the
    two ends and invoke the instance's .join(thisend, thatend).  At any stage,
    invoke .peers(node) to get a list of all nodes equivalent to the one given.
    """

    def __init__(self):
        """Initialises an empty FindUnite."""

        self.__up = []
        self.__forward = []
        self.__backward = []

        # Book-keeping assistant: same length as all the above
        self.__count = []       # number of nodes `below'

    def __len__(self): return len(self.__up)
    # assert: always equal to len()s of the other lists

    def peers(self, node):
        """Returns the equivalence class of a given node.

        Result is a list of nodes equivalent to the given node.
        If it ever involves duplicates, something's gone wrong.
        Probably so wrong you won't see the answer ...

        If node is unknown to self, self knows of no nodes connected to it; so
        the result is just [ node ]. """

        f, n, row = self.__forward, node, []
        while 1:
            row.append(n)
            n = f[n]
            if n is node: return row

    def __chase(self, node):
        """Chases the __up chain from a node to its top.

        Takes a node (looks up its index), returns the node reached from it by
        chasing i->self.__up[i] to a fixed point.  In the process, it ensures
        the chase will be shorter, in future, for all the nodes chased. """

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

        Arguments are nodes; at least one must be given.  Result is true
        precisely if all nodes are in the same connected component of the graph
        of edges thus far passed to self.join(). """

        nod = self.__chase(node)
        for vertex in nodes:
            if nod != self.__chase(vertex):
                return None # i.e. No.

        return 1>0 # i.e. Yes

    def append(self, ind):
        if ind != len(self.__up):
            raise ValueError, "FindUnite's append requires the right value"

        assert len(self.__forward) == ind == len(self.__backward)
        # Lazily grow the lists: node points at itself initially in all.
        self.__forward.append(ind)
        self.__backward.append(ind)
        self.__up.append(ind)
        # New node's sub-tree contains only itself:
        self.__count.append(1)

    def join(self, node, vertex):
        """Joins a given pair of nodes.

        Takes two arguments: the end-nodes of an edge in your graph.  Joins
        their connected components, if disjoint, together.  Nodes not previously
        known to self are added, initially connected to nothing; they are then
        joined as for familiar nodes. """

        nod, ver = self.__chase(node), self.__chase(vertex)
        if nod is ver: return # nothing to do

        # Who has the bigger sub-tree ?
        count = self.__count # we refer to it 5 times, may as well use local ref
        if count[nod] < count[ver]: nod, ver = ver, nod # swap

        # Hang smaller tree below bigger:
        self.__up[ver] = nod
        count[nod] = count[nod] + count[ver]
        del count # we're done with this reference to it

        # Now combine their two-way loops:
        f, b = self.__forward, self.__backward
        pod, wer = f[nod], f[ver]
        assert b[pod] is nod and b[wer] is ver
        # Do the swaps:
        b[wer], b[pod] = nod, ver
        f[nod], f[ver] = wer, pod

class Graph:
    def __init__(self):
        self.__edges, self.__nodes, self.__equiv = [], [], FindUnite()

    # Read-only copies:
    def nodes(self): return tuple(self.__nodes)
    def edges(self): return tuple(self.__edges)

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

        self.__equiv.append(ind)
        return ind

    def newedge(self, start, stop, twoway=1>0):
        self.__edges.append((start, stop))
        self.__equiv.join(self.__node(start), self.__node(stop))

    def peers(self, node):
        try: nod = self.__nodes.index(node)
        except ValueError: return [ node ]

        return map(lambda i, _r=self.__nodes: _r[i],
                   self.__equiv.peers(nod))

    def joined(self, *nodes):
        try: indices = map(self.__nodes.index, nodes)
        except ValueError: return None

        return apply(self.__equiv.joined, indices)
        

"""
 $Log: graph.py,v $
 Revision 1.2  2000-02-19 13:37:30  eddy
 Moved all node/index translations to Graph so FindUnite's node values
 are all indices into lists.  Now to carve up FindUnite.

 Revision 1.1  2000/02/19 12:18:45  eddy
 Initial revision

"""
