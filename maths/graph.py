"""Some graph-theoretic stuff.

$Id: graph.py,v 1.1 2000-02-19 12:18:45 eddy Exp $
"""

class NodeError(KeyError):
    """Exception used by graph.py's tools to signal `no such node'."""

class FindUnite:
    """An implementation of the find-unite algorithm.

    Create an instance of FindUnite(); for each edge in a graph, identify the
    two ends and invoke the instance's .join(thisend, thatend).  At any stage,
    invoke .peers(node) to get a list of all nodes equivalent to the one given.
    """

    def __init__(self):
        """Initialises an empty FindUnite."""

        self.__indices = {}     # { arbitrary node: index }

        # Indices are into the following three sequences, each of which is a
        # lazily-grown infinite list [ 0, 1, 2, 3, ... ] and represents a
        # network of pointers among the legitimate indices: chase __up to a node
        # which is its own __up to find the identifying member of your
        # equivalence class; chasing forward runs around a loop of all nodes in
        # your equivalence class; chasing backward traverses the same loop in
        # reverse order.

        self.__up = []
        self.__forward = []
        self.__backward = []

        # Book-keeping assistant: same length as all the above
        self.__count = []       # number of nodes `below'
        self.__nodes = []       # inverse of __indices

    def __len__(self): return len(self.__nodes)
    # assert: always equal to len()s of the other lists and, indeed, of __indices

    def nodes(self): return self.__nodes[:] # a copy !
    # assert: always equal to self.__indices.keys() modulo order

    def peers(self, node):
        """Returns the equivalence class of a given node.

        Result is a list of nodes equivalent to the given node.
        If it ever involves duplicates, something's gone wrong.
        Probably so wrong you won't see the answer ...

        If node is unknown to self, self knows of no nodes connected to it; so
        the result is just [ node ]. """

        try: first = self.__indices[node]
        except KeyError: return [ node ]

        f, names = self.__forward, self.__nodes
        row, n = [ node ], f[first]
        while n is not first:
            row.append(self.__nodes[n])
            n = f[n]

        return row

    def __chase(self, node):
        """Chases the __up chain from a node to its top.

        Takes a node (looks up its index), returns the node reached from it by
        chasing i->self.__up[i] to a fixed point.  In the process, it ensures
        the chase will be shorter, in future, for all the nodes chased. """

        try: bot = self.__indices[node]
        except KeyError: raise NodeError(node)

        ans = bot
        while 1:
            a = self.__up[ans]
            if a is ans: break
            ans = a

        # short-cut future chases
        while bot is not ans:
            next, self.__up[bot] = self.__up[bot], ans
            bot = next

        return ans

    def joined(self, node, *nodes):
        """Do the edges seen thus far connect (all) the given nodes ?

        Arguments are nodes; at least one must be given.  If any of the nodes
        given is unknown to self, even if it's the only one given, the result is
        false: self doesn't join it to anything.  Otherwise, result is true
        precisely if all nodes are in the same connected component of the graph
        of edges thus far passed to self.join(). """

        try:
            nod = self.__chase(node)
            for vertex in nodes:
                if nod != self.__chase(vertex):
                    return None # i.e. No.

        except NodeError: # Unknown node
            return None

        return 1>0 # i.e. Yes

    def __newnode(self, node):
        # Create a new node:
        self.__indices[node] = ind = len(self.__nodes)
        self.__nodes.append(node)
        assert len(self.__forward) == ind == len(self.__backward)
        # Lazily grow the lists: node points at itself initially in all.
        self.__forward.append(ind)
        self.__backward.append(ind)
        self.__up.append(ind)
        # New node has only itself `below'.
        self.__count.append(1)

        # Return the index we just gave it.
        return ind

    def __root(self, node):
        """Either creates node or finds its parent.

        If node is new to self, it is allocated a number and internal
        data-structures are updated to accommodate it.  Thereafter, this number
        is self's index for that node.  Otherwise, node already exists and we
        chase i-> self.__up[i] to find the representative of its connected
        component. """

        try: return self.__chase(node)
        except NodeError:
            return self.__newnode(node)

    def join(self, node, vertex):
        """Joins a given pair of nodes.

        Takes two arguments: the end-nodes of an edge in your graph.  Joins
        their connected components, if disjoint, together.  Nodes not previously
        known to self are added, initially connected to nothing; they are then
        joined as for familiar nodes. """

        nod, ver = self.__root(node), self.__root(vertex)
        if nod is ver: return # nothing to do

        # Hang one node below the other:
        count = self.__count
        if count[nod] < count[ver]: nod, ver = ver, nod # swap

        self.__up[ver] = nod
        count[nod] = count[nod] + count[ver]
        del count # we're done with this reference to it

        # Now combine their two-way loops:
        f, b = self.__forward, self.__backward
        p, w = f[nod], f[ver]
        assert nod is b[p] and ver is b[w]
        # Do the swap:
        b[w], b[p] = nod, ver
        f[nod], f[ver] = w, p
        return

class Graph:
    def __init__(self):
        self.__edges, self.__equiv = [], FindUnite()

    def newedge(self, start, stop, twoway=1>0):
        self.__equiv.join(start, stop)
        self.__edges.append((start, stop))

"""
 $Log: graph.py,v $
 Revision 1.1  2000-02-19 12:18:45  eddy
 Initial revision

"""
