"""Some graph-theoretic stuff.

See also graphviz (http://www.graphviz.org/ and man pages for: twopi, dotty,
lefty, acyclic, lneato, nop, tred, gc, sccmap, unflatten, gvgpr, dot, neato,
ccomps and gvcolorize) for visualisation of graphs.  Things to play at:

  * .h file inclusion hierarchy; CPP sensitive ?
  * C function call hierarchy
"""

class Partition:
    """Base-class for partitions.   Home for docs.

    A partition, P, of a set, S, is a set of disjoint sub-sets of S for which S
    is the union of all P's members.  I'll refer to the members of P as `parts';
    each part is a sub-set of S, each member of S is a member of exactly one
    part.  I'll describe members of S as nodes; and the members of the part
    containing a given node as peers of that node.

    The present implementation works with S as range(n) for some integer n; the
    nodes are integers 0 to n-1.  Where doc-strings in sub-classes of Partition
    describe a parameter as a node, an IndexError will be raised if its value is
    not in range(n).  Internal datastructures use the node as an index into
    appropriate arrays of length n.

    Partitions support len(); a new node may be added by calling append(len()).
    It is presumed that the caller had some prior knowledge of len()'s value;
    the argument to append() is asserted to be len() when __debug__ is true.

    Each new node will be in a new part when created: thus initialisation of the
    nodes creates a `discrete' partition in which each node is a part and each
    part has only one node in it.  [The presence of the empty set as one of the
    parts is formally allowed but invariably a nuisance: it is more practical to
    exclude empty.]  A partition is modified by uniting two of its parts: this
    is expressed as joining two nodes, one from each part.

    A partition can be thought of as answering two primitive questions,
      * are these two nodes in the same part as one another ?
      * which nodes are peers of this one ?
    along with the inevitable `give a complete description of yourself'.  The
    implementations of Find and Unite, below, answer the primitive questions and
    suffice, together, to provide a complete description.  Each is optimised to
    answer one question and avoids answering the other: each presumes that the
    other will be consulted for the information it lacks.  Thus Unite presumes
    that you won't ask it to join two nodes unless you know they are in
    different parts: it thereby saves itself the expensive (for it) job of
    checking this. """

    def peers(self, node):
        """Returns a list of the nodes in the same connected component as the given node. """
        raise NotImplementedError

    def join(self, node, vertex):
        """Joins the connected components of the given nodes. """
        raise NotImplementedError

    def joined(self, node, *nodes):
        """Returns true iff all the given nodes are in one connected component. """
        raise NotImplementedError

class Unite (Partition):
    """Keeps track of the parts of a partition.

    Methods:
      peers(node) -- returns a list of peers of the given node.
      join(this, that) -- combines two *disjoint* parts.
    """

    def __init__(self, size=0):
        """Initialise internal datastructures."""
        self.__forward = range(size)
        self.__backward = range(size)

        # Chasing i->forward[i] leads round a loop;
        # likewise  backward, traversing the same loop in reverse.
        # Each such loop is a connected component of self.
        # assert i == backward[forward[i]] for each 0 < i < len(self)

    def __len__(self): return len(self.__forward)
    # assert: equal to len(self.__backward)

    def peers(self, node):
        """Returns the component containing a given node.

        Result is a list of nodes in the given node's component.
        If it ever involves duplicates, something's gone wrong.
        Probably so wrong you won't see the answer ... """
        # This should be implemented to yield an iterator ...

        f, n, row = self.__forward, node, []
        while True:
            row.append(n)
            n = f[n]
            assert self.__backward[n] == row[-1]
            if n is node: return row

    def append(self, ind):
        assert len(self.__forward) == ind == len(self.__backward), \
               "Unite's append requires the right value"

        # new loop with one member: backward[i] == i == forward[i]
        self.__forward.append(ind)
        self.__backward.append(ind)

    def join(self, nod, ver):
        """Joins two *disjoint* components.

        Arguments are nodes, one from each component.

        The nodes must not be peers: this is *not* checked.
        They will be peers after join is called. """

        f, b = self.__forward, self.__backward
        pod, wer = f[nod], f[ver]
        assert b[pod] is nod and b[wer] is ver
        # Do the swaps:
        b[pod], b[wer] = ver, nod
        f[nod], f[ver] = wer, pod

class Find (Partition):
    """Keeps track of `in same part' truths for a partition.

    Handles predicates relating to a partition; also keeps track of the
    collection of parts, expressed as one member of each part.

    Methods:
      joined(node, ...) -- true if all given nodes are in one part
      join(this, that) -- ensures two nodes are joined
      disjoint() -- returns a list of nodes, one from each part
      peercount(node) -- size of given node's connected component
    """

    def __init__(self, size=0):
        self.__up = range(size)
        self.__count = [ 1 ] * size

	# self.__up[i] is a member of the same connected component as i; i is
	# the representative member of the component iff i is self.__up[i], in
	# which case self.__count[i] is the size of the connected component.

    def __len__(self): return len(self.__up)
    # assert: always equal to len(self.__count)

    def __chase(self, node):
        """Returns the representative member of node's connected component.

        Single argument is a node (index): returns the node reached from this by
        chasing i->self.__up[i] to a fixed point.  Side-effect: changes the
        __up[] of all nodes it visits on the way to point at the answer
        returned, so as to speed this chase next time around. """

        trail = []
        while True:
            n = self.__up[node]
            if n is node: break
            trail.append(node)
            node = n

        # self.__up[node] is node, as is self.__up[trail[-1]] if len(trail) > 0.

        # All but the last item in trail need to point __up at node:
        for n in trail[:-1]: self.__up[n] = node
        return node

    def peercount(self, node): return self.__count[self.__chase(node)]

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
        self.__up.append(ind) # so self.__up[ind] is ind.
        # New node's sub-tree contains only one node: itself.
        self.__count.append(1) # and self.__count[ind] is 1.

    def join(self, node, vertex):
        """Joins two nodes.

        Returns None if nodes were already joined: else, a true value.
        Consequently its return is what `not self.joined()' would have yielded
        previously. """

        nod, ver = self.__chase(node), self.__chase(vertex)
        if nod is ver: return # nothing to do

        # Who has the bigger sub-tree ?
        count = self.__count
        n, v = count[nod], count[ver]
        if n < v: nod, ver = ver, nod # swap

        # Hang smaller tree below bigger:
        self.__up[ver], count[nod] = nod, n + v

        return 1>0      # did something

    def disjoint(self):
        """Returns a list containing one sample member from each connected component. """
        # the sample members being the fixed-points of __up

        return filter(lambda x, _u=self.__up: x is _u[x], range(len(self.__up)))

class FindUnite (Find, Unite):
    """An implementation of the find-unite algorithm.

    Provides for arbitrary graph-partitioning, provided the nodes of the graph
    are labelled with an initial sub-sequence of the natural numbers, i.e. 0, 1,
    ..., n for some natural n, with no omissions.  Graph, below, provides such a
    packaging for a graph with arbitrary python objects as nodes.

    Create an instance of FindUnite(); for each edge in a graph, identify the
    two ends and invoke the instance's .join(thisend, thatend).  At any stage,
    invoke .peers(node) to get a list of all nodes in the given one's connected
    component. """

    def __init__(self, size=0):
        """Initialises an empty FindUnite."""

        Find.__init__(self, size)
        Unite.__init__(self, size)

    def append(self, ind):
        Find.append(self, ind)
        Unite.append(self, ind)

    def join(self, node, vertex):
        """Joins a given pair of nodes.

        Takes two arguments: the end-nodes of an edge in your graph.  Joins
        their connected components, if disjoint, together.  Nodes not previously
        known to self are added, initially connected to nothing; they are then
        joined as for familiar nodes. """

        # Unite doesn't want to join things if they're already joined.
        # Find knows whether they're already joined:
        if Find.join(self, node, vertex):
            # Find did something: get Unite in on the act.
            Unite.join(self, node, vertex)

    def partition(self):
        """Returns a list of disjoint lists describing the partition.

        Each entry in the list returned is a list of nodes representing a
        connected component of the graph described by the FindUnite object. """

        return map(self.peers, self.disjoint())

class Graph:
    """Represents a network of nodes joined by edges.

    Creation:

      Graph([node, ...]) -- creates a new graph with, optionally, the given
                            objects as (initially) unconnected nodes.

    Command:

      join(this, that) -- adds an edge from this to that, optionally adding each
                          as a node in the process.

    None of the following modifies the graph: whereas join() will add any
    unfamiliar arguments as nodes of the graph, the queries (below) will
    interprete any unrecognised node as being outside the graph and connected to
    nothing but itself - but will promptly forget they ever heard of the given
    node.

    Attributes:

      nodes -- a tuple containing all known nodes.

      edges -- a tuple containing all known edges, each of which is represented
               by a 2-tuple of nodes.

      partition -- a list of connected components of self.  Each connected
                   component is represented as a list (whose order is arbitrary)
                   of nodes; each such list is equal, subject to shuffling
                   order, to the .peers(n) of each node, n, in the list.  Each
                   node of the graph appears in exactly one of the connected
                   components.

    Queries:

      peers(node) -- returns a list of all nodes in the same connected component
                     as the given node (order is arbitrary).

      peercount(node) -- returns the value len(peers(node)) would produce: the
                         number of nodes in the connected component in which the
                         given node appears.

      joined(node, ...) -- tests whether given nodes are all in one connected
                           component.

      span(node, ...) -- returns a Graph consisting of all nodes and arcs in the
                         same connected component as at least one of the given
                         nodes.

      chop(node, ...) -- returns a Graph consisting of the given nodes and any
                         edges directly connecting these nodes.

    Note that .span() provides a sub-graph `grown outwards from' its given
    nodes, while .chop() provides a sub-graph `stripped down to only' the given
    nodes. """

    # Creation:
    def __init__(self, *nodes):
        self.__edges, self.__nodes, self.__connect = [], list(nodes), FindUnite(len(nodes))

    # Attributes:
    def __getattr__(self, key):
        # Read-only copies:
        if key == 'nodes': return tuple(self.__nodes)
        if key == 'edges': return tuple(self.__edges)
        if key == 'partition': return map(self.__peers, self.__connect.disjoint())

        raise AttributeError, key

    # Command: .join() with support from .__node()

    def __node(self, node):
        """Returns internal index of node, adding node to graph if needed.

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
        """Connects two nodes in the present graph, adding the nodes if necessary. """

        self.__edges.append((start, stop))
        self.__connect.join(self.__node(start), self.__node(stop))

    # Queries: joined(), peers() and sub-Graph()s.

    def joined(self, *nodes): # requires at least one node, in fact
        if len(nodes) < 1:
            raise ValueError('no nodes provided: how can I check whether they are joined ?')
        if len(nodes) < 2: return 1 # every node is implicitly connected to itself

        try: indices = map(self.__nodes.index, nodes)
        except ValueError: return None

        return self.__connect.joined(*indices)

    def peercount(self, node):
        try: nod = self.__nodes.index(node)
        except ValueError: return 1

        return self.__connect.peercount(nod)

    def __peers(self, nod):
        return map(lambda i, _r=self.__nodes: _r[i], self.__connect.peers(nod))

    def peers(self, node):
        try: nod = self.__nodes.index(node)
        except ValueError: return [ node ]

        return self.__peers(nod)

    # sub-Graph()s: span(), chop()

    def span(self, *nodes):
        """Returns a full sub-graph containing the given nodes. """

        # collect the union of the connected components of the given nodes:
        all = []
        for node in nodes:
            if node not in all:
                all = all + self.peers(node)

        # build a graph out of them:
        ans = Graph(*all)
        for a, b in self.__edges:
            assert (a in all) == (b in all), 'I thought we had a partition here !'
            if a in all: ans.join(a, b)

        # return that graph:
        return ans

    def chop(self, *nodes):
        """Returns a restriction sub-graph containing only the given nodes.

        Includes each edge of self whose ends are both in the restriction.  In
        particular, doesn't include linkage via nodes omitted (see .span() for
        that). """

        # build a graph using only the given nodes:
        ans = Graph(*nodes)
        for a, b in self.__edges:
            if a in nodes and b in nodes: ans.join(a, b)

        # return that graph:
        return ans
