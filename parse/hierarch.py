"""Diagrams of class hierarchies.

Understanding the relationships among the classes provided by a package or
module can be made a great deal easier with the use of a diagram depicting the
inheritance hierarchy: this can also be helpful in debugging certain types of
problem with such hierarchies (notably when multiple inheritance leads to
unresolvable ambiguities in how the python engine should access
base-classes).  Fortunately, python's introspection mechanisms make it fairly
easy to discover that hierarchy; and the graphviz package's 'dot' language makes
it easy to turn the result into a picture.

$Id: hierarch.py,v 1.2 2009-03-22 23:39:32 eddy Exp $
"""

class Diagram (object):
    """A diagram of a class hierarchy.

    Every base of a class in the diagram is tacitly also present in the diagram;
    and, of course, this applies transitively to bases of bases.  The diagram
    comprises classes linked by a directed edge from each derived class to each
    of its bases.

    When it comes to displaying the diagram, as a source file for dot, it may be
    desirable to limit the diagram in some way, for example only displaying
    classes from within a certain module.  To this end, filters may be supplied,
    see add_filter, to select which classes to include: any class which matches
    at least one filter shall be included (but, if no filters are supplied, all
    classes are included).  Since the case of being provided by a designated
    module or package is anticipated to be common, a convenience method is provided
    which turns strings into filters on the .__module__ attribute of a class either
    being a given string or starting with one of them followed by a dot.

    Since a class in one module that builds on one in another may re-use its
    name, it's necessary to be able to identify classes by more than just their
    names; typically, a module prefix should be sufficient, but alternatives may
    be provided to the constructor.

    The dot file format includes support for diverse properties of nodes and
    edges, notably colour (called color, due to en-US-ish authorship of
    graphviz; but the present author's mother tongue is English).  While further
    properties may prove worth adding, the present infrastructure only provides
    for colouring.  You can specify two mappings from regular expresions to
    colours: one for the border of the ellipse that surrounds the class name,
    one for the text of the class name itself.  Each class uses the bitwise xor
    of the colours for regular expressions that match the class's name, with any
    relevant prefix.\n"""

    def __init__(self, prefix=lambda x: x.__module__.split('.')[-1] + '.'):
	"""Initialize diagram.

	Single argument, prefix, is optional.  It determines a prefix to be
	added to the name of each class in the diagram.  The special value,
	None, is a synonym for lambda x: '', i.e. use no prefix.  The default
	takes the last component of the '.'-joined sequence that is the class's
	.__module__ and adds a '.' to the end, to join it to the class
	name. Otherwise, a callable should be supplied: it is passed the class
	and should return a string (which should typically end in a '.' to join
	it to the class name).\n"""

	self.__classes = self.__filters = self.__text = self.__border = ()
	if prefix is None: self.__prefix = lambda x: ''
	else: self.__prefix = prefix

    def add_class(self, *etc):
	"""Adds some classes, and their bases, to the diagram.

	Accepts arbitrarily many positional parameters, each of which should be
	a class to be included in the diagram, along with all of its bases.\n"""
	self.__classes += etc

    def add_filter(self, *etc):
	"""Adds filters to the base classes to be included in the diagram.

	Accepts arbitrarily many positional parameters, each of which should be
	a callable which accepts a single input (a base of a class in our
	diagram) and returns a value which can be used as a conditional; if a
	base-class, of a class in the diagram, gets a true value back from at
	least one filter, the base-class is included in the diagram.  See also
	add_module, for the special case of filtering by which module provided
	the class.  Note that, if parse_module() is used, there may be fake
	module objects and even strings in the graph: the single parameter
	passed to each filter may thus be a string; otherwise, the only
	attributes it reliably has are __name__, __bases__ and __module__; and
	the last two of these may be None.\n"""

	self.__filters += etc

    @staticmethod
    def __isinmod(m):
        def res(k, p=m):
            try: n = k.__module__
            except AttributeError: return False
            if n == p: return True
            return n.startswith(p) and n[len(p):].startswith('.')

        return res

    def add_module(self, *mods):
	"""Convenience method to filter on source modules.

	Accepts arbitrarily many positional parameters, each of which should be
	a string which names a module or package: each is used to construct a
	filter which accepts classes whose .__module__ either is the string or
	begins with it followed by a dot; these filters is then forwarded to
	self.add_filter (q.v.).\n"""
	self.add_filter(*map(self.__isinmod, mods))

    class FakeClass (object):
        def __init__(self, name, module, bases):
            self.__name__ = name
            self.__module__ = module
            self.__bases__ = bases

        @classmethod
        def __match(klaz, item, maybe):
            if item.name != maybe.__name__: return 0
            n = 3 # one point for each attribute checked

            m = maybe.__module__
            if m is None: n -= 1
            elif '.' in item.module: # in case pyclbr ever gets cleverer
                if m != item.module: return 0
                n += len(m.split('.')) - 1 # bonus points for fancy module name
            elif m.split('.')[-1] != item.module: return 0

            b = maybe.__bases__
            if b is None: n -= 1
            elif len(b) != len(item.super): return 0
            else:
                i = len(b)
                while i > 0:
                    i -= 1
                    s = item.super[i]
                    if isinstance(s, basestring):
                        if s == b[i].__name__: q = 1
                        else: return 0
                    else:
                        q = klaz.__match(item.super[i], b[i])
                        if not q: return 0
                    n += q # bonus points from base-class scores.

            return n

        @classmethod
        def __find(klaz, item, prior, module):
            """Find something in prior that looks like item, if possible.

            Required arguments:
              item -- a pyclbr.Class instance or a string.
              prior -- as for express (q.v.)
              module -- None or the full name, if known, of a module in which a
                        class has item as a base.

            Tries to find an entry in prior[item.name] that item might plausibly
            be; returns that entry if found.  Otherwise, if item is a string is
            is returned; else .express() is called to create a new object to
            represent the item.\n"""

            try: known = prior[item.name]
            except AttributeError: # item is a string
                assert isinstance(item, basestring)
                try: known = prior[item]
                except KeyError: pass
                else:
                    if len(known) == 1: return known[0]
                    # else ambiguity :-(
                return item

            except KeyError: known = ()
            else:
                if module is None:
                    def count(ks): return 0
                elif '.' in module:
                    def count(ks, ms=module.split('.')):
                        i = 0
                        try:
                            while ks[i] == ms[i]: i += 1
                        except IndexError: pass
                        return i
                else:
                    def count(ks, m=module):
                        if ks[-1] == m: return 1
                        return 0

                def mark(mod, score=count, tail=module.split('.')[-1]):
                    if mod is None: return 0
                    if '.' in mod: return score(mod.split('.'))
                    if mod == tail: return 1
                    return 0

                best, score, ms, peers = None, 0, 0, []
                for it in known:
                    q = klaz.__match(item, it)
                    if q > score:
                        best, score, ms, peers = it, q, mark(it.__module__), []
                    elif 0 < q == score: # ambiguity; check module
                        sm = mark(it.__module__)
                        if sm > ms: best, score, ms, peers = it, q, sm, []
                        elif sm == ms: peers.append(it)

                if peers:
                    print 'Arbitrailly preferring', best, 'to match', item, 'ignoring', peers
                if best is not None:
                    # If item provides more data than best, embelish best with it:
                    if best.__module__ is None and item.module:
                        best.__module__ = item.module
                    if best.__bases__ is None and item.super:
                        if '.' in item.module or ms <= 1: module = item.module
                        else: module = best.__module__
                        bases = []
                        for k in item.super:
                            bases.append(klaz.__find(k, prior, module))
                        best.__bases__ = tuple(bases)
                    return best

            ans = klaz.express(item.name, item, prior)
            prior[item.name] = known + (ans,)
            return ans

        @classmethod
        def express(klaz, name, item, prior, module=None):
            """Create a FakeClass to represent a named item from module.

            Required arguments:
              name -- name of the item.
              item -- a pyclbr.Class instance.
              prior -- mapping from names to tuple of known class and FakeClass
                       objects with the given name.
              module -- full name of the module item came from, if known.

            The fiddly part of this is identifying item.super's entries with
            class or FakeClass objects from prior, when available.\n"""

            if module is None: module = item.module
            bases = []
            for k in item.super:
                bases.append(klaz.__find(k, prior, module))

            return klaz(name, module, tuple(bases))

    from pyclbr import readmodule
    def parse_module(self, module, path=(), faker=FakeClass.express, read=readmodule):
        """Crude parser to extract module hierarchy from a module.

        Required argument, module, is a name that could be given to a simple
        import statement; optional argument path should be a sequence (default
        is empty) of entries to be added to sys.path when locating module source
        code (these are the arguments to pyclbr.readmodule).

        For each class definition found at global (i.e. module) scope in the
        parsed file, this method adds a mock-class object, describing the class,
        to our class hierarchy.  It tries to match up imported symbols with the
        objects it knows about from other sources, but cannot be relied on to
        guess right every time.  Its chances of guessing right are somewhat
        improved if you parse each module after you've introduced self to the
        classes on which that module's classes are based.\n"""

        raw, cs, prior = read(module, list(path)), [], {}
        for k in self.nodes():
            prior[k.__name__] = prior.get(k.__name__, ()) + (k,)

        for n, k in raw.items():
            # Allow that k might be a string (like entries in its super);
            # otherwise, its relevant attribues are name, module, super - but
            # module is only the last component of the full module name; and
            # super's entries are of the same kind presumed for k.
            if isinstance(k, basestring): pass # too little information
            else: self.add_class(faker(n, k, prior, module))

    del FakeClass, readmodule

    import re
    from colour import Colour
    @staticmethod
    def __colpat(pattern, colour, parse=re.compile, shade=Colour.from_gv):
	"""Digest possible encodings of a regex-to-colour mapping.

	Parameters:
	  pattern -- either a regular expression or a string.
	  colour -- either a colour.Colour or a valid colour string

	Returns a two-ple of a regular expression and a colour.Colour
	object.  If pattern is a string, it is re.compile()d to obtain a regular
	expression.  For the allowable colour strings, see the
	colour.Colour.from_text(), which shall be used to interpret them.\n"""

	if isinstance(pattern, basestring): pattern = parse(pattern)
	if isinstance(colour, basestring): colour = shade(colour)
	return pattern, colour

    del re, Colour

    def colour_text(self, pattern, colour):
	"""Specify colouring of class names.
	"""
	self.__text += (self.__colpat(pattern, colour),)

    def colour_ring(self, pattern, colour):
	"""Specify colouring of the ellipses bubbling class names.
	"""
	self.__border += (self.__colpat(pattern, colour),)

    def arcs(self):
	"""Returns a filtered set of arcs of the diagram.

	Result is a set of triples (derived, index, base) with derived being a
	class whose .__bases__[index] is base.  For each class passed to
	add_class or present as the base of a returned tuple, one such tuple is
	included for each base of the class (when no filters are given or, when
	any are ...) that satisfies at least one filter passed to add_filter
	(possibly by being provided by a module or package passed to
	add_module).\n"""

	done, ans, todo = set(), set(), set(self.__classes)
	while todo:
	    it, i = todo.pop(), -1
	    done.add(it)
	    for b in it.__bases__:
                i += 1
		if b not in done and b not in todo:
                    if self.__filters:
                        for f in self.__filters:
                            if f(b): break
                        else:
                            continue

                    if not isinstance(b, basestring):
                        todo.add(b)

                ans.add((it, i, b))

	return ans

    def nodes(self):
	"""Returns a set of classes in the diagram.

	Each member of the returned set is a class that shall apppear in the
	diagram.\n"""
	ans = set()
	for (d, i, b) in self.arcs():
	    # set.add is helpfully good at ignoring duplicates
	    ans.add(d)
	    ans.add(b)

	return ans

    def colouring(self):
	"""Returns a { classname: node-colour } mapping."""
        raise NotImplementedError # TODO

    def __name(self, k):
        if isinstance(k, basestring): return k
	return self.__prefix(k) + k.__name__

    def emit(self, fd):
	"""Emits a dot-format description of the class hierarchy.

	Single argument, fd, is the handle of a file open for writing, to which
	to write the description.\n"""

	fd.write('digraph ClassDiagram {\n' +
		 '  rankdir="RL"\n' +
		 '  node [shape=ellipse]\n')

	for (d, i, b) in self.arcs():
	    fd.write('  "%s" -> "%s" [taillabel="%d"];\n' %
		     (self.__name(d), self.__name(b), i))

	fd.write('}\n')
