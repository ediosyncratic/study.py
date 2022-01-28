"""Building a graph of links within a website.

Currently incomplete, but most of a sensible structure is in place.

See study.LICENSE for copyright and license information.
"""
from html.parser import HTMLParser
import urllib.request, urllib.parse
from urllib.error import HTTPError

class Error (Exception): pass

class Context (object):
    """The context from which a page may be linked.

    This may correspond to a directory of the URL path-space, a page,
    a section within that page or an element within such a section.
    If it is not the root of the web-site, it will know its parent,
    which shall know about this child.

    Note that a subdirectory is not a page (so cannot have sections or
    elements within it); to create the 'index' page of the directory,
    call file('') on it."""

    def __init__(self, url, parent=None, kids=set, fragment=None):
        """Set up a Context.

        Only the root of a site should be constructed using this, and
        callers should take care to avoid duplicate objects; to
        construct contexts within a site, use the relevant methods of
        the parent context: subdirectory(), page(), section(),
        element().  Each of these takes care to reuse an existing
        object if asked for one it already knows about."""
        if parent is None:
            assert kids is set
            assert fragment is None
            assert not url.endswith('/'), url

        self.__url = url
        self.__parent = parent
        self.__children = kids()
        if kids is list: # page or part thereof:
            self.__fragment = fragment
        else: # directory:
            assert kids is set
            if fragment:
                raise Error('You need the directory page, not the directory, to '
                            'apply a fragment identifier', url, fragment)

        self.href = None

    def __contains__(self, other):
        """Every context is in itself; so is every sub-context."""
        while other is not None:
            if other is self:
                return True
            other = other.__parent
        return False

    # TODO: for Qt, we actually want to group by \inmodule tags,
    # or whatever they map to in HTML, not by subdirectory.
    # Those pages are module-name.html or qmlmodule-name.html
    # They contain a QT += widgts line.
    def subdirectory(self, name):
        """Set up the child Context for a subdirectory of a site.

        Pass the name of the sub-directory; it'll be appended to
        self's URL to create the child's.  Do not include a trailing
        '/' on (or a leading one at the start of) the name."""
        assert name, self.url
        self = self.__as_dir()
        assert not self.__url.endswith('/')

        url = self.__url + '/' + name
        for child in self.__children:
            if child.url == url:
                return child

        child = Context(url, self)
        self.__children.add(child)
        return child

    @property
    def isDir(self):
        return isinstance(self.__children, set) # Otherwise it's a list.

    def page(self, name):
        """Set up a page within a directory of a site.

        Pass the page-name, e.g. 'file.html'; it'll be appended to
        self's URL to create the child's.  It may be empty for the
        'index' page of a directory."""
        self = self.__as_dir()
        assert not self.__url.endswith('/')

        url = self.__url + '/' + name
        for child in self.__children:
            if child.url == url:
                return child

        child = Context(url, self, list)
        child.level = 0
        child.tag = None
        child.__anchors = {}
        child.__dangling = {}
        self.__children.add(child)
        return child

    @property
    def dangling(self):
        # Only pages have this property.
        return self.__dangling

    def reference(self, fragment):
        """Provides a stub context for links to refer to.

        This will set up a dummy child object of a page, ready to be
        filled in when its page is parsed.
        """
        try:
            self.level
        except AttributeError:
            if self.__section() is not None:
                raise Error('This should only be called on a page or subdirectory',
                            fragment, self.url)
            self = self.page('')

        assert self.level == 0, 'Should only be called on a page.'
        assert fragment, 'Should only be called with non-empty fragment.'
        return self.__reference(self, fragment)

    def __reference(self, page, fragment, tag=None, level=None):
        anchor, dangle = page.__anchors, page.dangling
        store = anchor if tag else dangle
        try:
            child = anchor[fragment]
            assert fragment is not None
            if tag or level or child.__parent is not self:
                #print(f"Warning: duplicate fragment '{fragment}' in {self.url} vs {child.__parent.url}")
                store = None
            else:
                return child
        except KeyError:
            pass

        try:
            child = dangle[fragment]
        except KeyError:
            child = Context(self.__url, self, list, fragment)
            child.tag, child.level = tag, level
        else:
            assert fragment is not None
            assert child.tag is None
            assert child.level is None
            assert child.__parent is page
            if tag:
                # i.e. we now have the real details for this fragment
                del dangle[fragment]
                child.tag, child.level = tag, level
                # We don't add reference()-created child to the page, so no need to remove:
                assert child not in page.__children
                # Caller shall take care of adding child to self's children.
                if page is not self:
                    child.__parent = self
                    child.__url = self.__url

        if fragment and store is not None:
            store[fragment] = child

        return child

    def section(self, level, fragment=None):
        """Set up a section within a page.

        Pass the level of the section (1 for an H1, 2 for H2 and so
        on); if it has one, also pass its fragment identifier.  The
        latter, if present, will be appended to self's URL to create
        the child's; otherwise, the child inherit's self's URL.  When
        you get to the close-tag, be sure to call setTitle()."""
        assert not self.isDir
        assert level > self.__section().level

        tag = f'h{level}'
        child = self.__reference(self.__page(), fragment, tag, level)
        self.__children.append(child)
        return child

    def element(self, tag, fragment=None):
        """Set up an element within a section or element.

        Pass the tag saying what kind of element it is along with its
        fragment identifier, if it has one.  Feel free to pass the
        text of the element to setTitle() if that seems appropriate to
        the type of element."""
        assert not self.isDir

        child = self.__reference(self.__page(), fragment, tag)
        self.__children.append(child)
        return child

    def sectionAbove(self, level):
        """Returns self's first ancestor above the given level.

        When parsing, if you find the start of a section at the given
        level, you need the return from here to add your new context
        as the next child of."""
        node = self.__section()
        if node is None:
            raise Error("Asked for section of directory context", level, self.url)
        while node.level is None or node.level >= level:
            node = node.__parent
        return node

    def setTitle(self, title):
        """Record the title of a section or text of an element."""
        try:
            self.__title
        except AttributeError:
            self.__title = title
        else:
            raise Error('Called setTitle() more than once', title, self.__title, self.url)

    @property
    def title(self):
        try:
            return self.__title
        except AttributeError:
            return None

    @property
    def url(self):
        try:
            base, frag = self.__url, self.__fragment
        except AttributeError: # directories can't have __fragment
            return self.__url
        return base if frag is None else base + '#' + frag

    @property
    def parent(self):
        return self.__parent

    def __bool__(self):
        return True
    def __hash__(self):
        return hash(tuple(self.__uid()))
    def __eq__(self, other):
        return tuple(self.__uid()) == tuple(other.__uid())

    def __uid(self):
        # The URL, then a bottom-up sequence of child indices, then maybe a fragment:
        yield self.__url
        run = self
        while not (run is None or run.isDir or run.__fragment):
            parent, count = run.__parent, 0
            for kid in parent.__children:
                if kid is run: # Must use is check, not ==, which this implements
                    break
                count += 1
            yield count
            run = parent

        if not run.isDir and run.__fragment:
            yield run.__fragment

    def __as_dir(self):
        if self.isDir:
            return self
        # SiteScan.context() treats the last fragment of a path as a
        # file-name; but, of course, it might be a directory.  When
        # it's called, to get a destination for a link, it can't know.

        # We're now either creating a sub-dir or a file within self,
        # so clearly something thinks self is a directory.  Those who
        # currently refer to self - including all children - think
        # it's a file, aside from its parent which doesn't know.  We
        # need to split self into a directory node and its index-file
        # node; the children and any in-bound links should refer to
        # the latter, so let self remain that, but reparent self as a
        # child of the missing directory node, which we'll return, so
        # that our caller can add a file or subdirectory to it.

        if self.level != 0 or self.__fragment:
            raise Error("Not a directory node.", self.level, self.__fragment, self.url)

        parent = self.__parent
        assert parent, f'Root contexts should be directories ! {parent.url}'
        assert parent.isDir
        assert not parent.__url.endswith('/')
        assert self.url.startswith(parent.url + '/')
        name = self.url[len(parent.url) + 1:]
        assert '/' not in name, (name, self.url)
        if not name:
            return parent

        # For some reason, remove(self) doesn't work.
        parent.__children = set(n for n in parent.__children if n is not self)
        bridge = parent.subdirectory(name)
        bridge.__children.add(self)
        assert bridge.__url == self.__url
        self.__parent = bridge
        self.__url += '/'

        return bridge

    def __page(self):
        """Find the page this is in.

        Should not be called on a director context.
        """
        assert not self.isDir
        while self.level != 0:
            self = self.__parent
        return self

    def __section(self):
        """Find the section self is in.

        Should only be called on an element within a page; if self is
        a section or page, it is returned.  Otherwise, runs up the
        parent hierarchy until it does find a section or page."""
        assert not self.isDir
        while self.level is None:
            self = self.__parent
        return self

class LinkParser (HTMLParser):
    """HTML document list parser.

    Finds all the links from within a page and all fragment
    identifiers within the page, that may be the targets of links.  In
    the process, it populates a given Context object, describing the
    page, with a hierarchy of relevant nodes, that may be relevant to
    describing where the link comes from.

    When completed, its graph attribute describes the graph of links
    out of the page; aside from taking that (and remembering the
    Context object passed to the parser), you may as well throw away
    the LinkParser object after it has been constructed."""
    __upinit = HTMLParser.__init__
    def __init__(self, context):
        """Prepare to parse an HTML document.

        Pass the Context object of the page being parsed.  It shall
        have child nodes added to it and any in-bound fragments
        resolved.  Its .anchors shall list all actual fragments that
        do exist within the page.

        On construction, the URL indicated by the context object shall
        be accessed and its contents passed to feed().  Recognises
        links and enough of the rest of what it parses to establish
        their context.

        The attempt to access the URL may lead to an urllib error
        being thrown.
        """
        assert not context.isDir
        self.__upinit()

        self.__base = context.url
        # self.__base may change if we encounter a BASE element in the HEAD.
        # It is used to resolve relative URLs.
        self.__graph = [] # [(context, URL), ...] describing links

        # Will be varied as we traverse the document
        self.current = context
        self.elementTexts = []
        with urllib.request.urlopen(context.url) as fd:
            for line in fd:
                self.feed(line.decode())

        self.close()
        assert not self.elementTexts, (self.elementTexts, context.url, self.current.url)
        del self.current
        del self.elementTexts

    @property
    def graph(self):
        """Sequence of (Context, URL) pairs.

        Every entry in the mapping indicates that there's (at least)
        one link, in the given context, to the given URL.
        """
        return tuple(self.__graph)

    def handle_starttag(self, tag, attrs):
        bok = dict(attrs)
        fragment = bok.get('id', bok.get('name') if tag == 'a' else None)

        if tag == 'base' and 'href' in bok:
            # <base href="...">
            # Configures resolving of relative URLs within this file:
            self.__base = urllib.parse.urljoin(self.__base, bok['href'])

        if tag == 'a' and 'href' in bok:
            self.current = self.current.element(tag, fragment)
            self.current.href = bok['href']
            self.current.__depth = 0
            self.__graph.append((self.current, urllib.parse.urljoin(self.__base, bok['href'])))
            title = bok.get('title')
            if self.current.title is None: # As it should be !
                if title:
                    self.current.setTitle(title)
                else:
                    # Use anchor text as "title":
                    self.elementTexts.append('')
            elif title != self.current.title:
                print("Warning: duplicate contexts with distinct titles, "
                      + f"'{title}' != '{self.current.title}")
            return

        if tag.startswith('h'):
            try:
                level = int(tag[1:])
                assert level > 0, 'There is no H0 element'
            except ValueError:
                pass # HTML, HEAD, HR, for example
            else:
                # We have a heading
                parent = self.current.sectionAbove(level)
                self.current = parent.section(level, fragment)
                self.current.__depth = 0
                if self.current.title is None: # As it should be !
                    self.elementTexts.append('')
                return

        # Is any special treatment for IMG, or other URL-referencing
        # things, worth adding ?

        if fragment:
            self.current = self.current.element(tag, fragment)
            self.current.__depth = 0
        elif tag == self.current.tag:
            self.current.__depth += 1

    def handle_data(self, data):
        stack = self.elementTexts
        data = data.strip()
        if stack and data:
            tail = stack[-1]
            stack[-1] = tail.rstrip() + ' ' + data if tail else data

    def __popText(self):
        assert self.elementTexts, ('Caller should know when collecting text !',
                                   self.current.url, self.current.tag)
        text = self.elementTexts.pop()
        # Text of an inner element is also part of the text of outer elements:
        self.handle_data(text)
        return text

    def handle_endtag(self, tag):
        if tag != self.current.tag:
            return

        if self.current.__depth > 0:
            self.current.__depth -= 1
            return

        # If the element has a title, record the text of the content as it.
        if tag.startswith('h'):
            if self.current.level:
                if self.current.title is None:
                    self.current.setTitle(self.__popText())
                del self.current.__depth
                # And *don't* change current, as this heading starts a section.
                return
            if self.current.level is not None:
                assert int(tag[1:]) == self.current.level, (tag, self.current.level)

        if tag == 'a' and self.current.title is None and self.current.href is not None:
            self.current.setTitle(self.__popText())

        del self.current.__depth
        self.current = self.current.parent


class SiteScan (object):
    """Scans web-sites to produce a graph of internal links.

    Incidentally also identifies all external links.  All the work is
    done on construction.  The resulting object's attributes and
    properties tell you about the links within and out of the
    web-site.  Each of these is a frozenset:

     * graph - (from, to) pairs of Context objects
     * broken - (page, fragment) pairs
     * external - urls of external links

    In each (from, to) of the graph, from describes the element that
    """
    # TODO: accept callables that defaults to LinkParser and Context
    # as keyword-only arguments, to allow client code to customise how
    # it parses and what gets used as nodes, by subclassing.
    def __init__(self, *roots):
        roots = tuple(r.rstrip('/') for r in roots)
        self.__toscan = set(roots)
        self.__contexts = { r: Context(r).page('') for r in self.__toscan }
        self.__roots = tuple(self.__contexts[r] for r in roots)
        self.__scanned = set()
        self.__external = set()

        broken = list()
        graphs = list()
        pages = list()
        while self.__toscan:
            url = self.__toscan.pop()
            self.__scanned.add(url)
            node = self.__context(url)
            if node is not None:
                pages.append(node)
                try:
                    graph = LinkParser(node).graph
                except HTTPError:
                    broken.append(node.url)
                for c, url in graph:
                    self.__context(url) # To make sure __toscan knows about it
                graphs.append(graph)

        # Convert the graphs into one big (context, context) graph,
        # now that self.__context should resolve all good links
        # correcly:
        union = set()
        for graph in graphs:
            for context, url in graph:
                target = self.__context(url)
                if target is not None:
                    union.add((context, target))

        self.graph = frozenset(union)

        # Now that we've resolved all URLs that we can, gather up the
        # broken internal links:
        for page in pages:
            if page.dangling:
                broken.append((page.url, page.dangling))
        self.broken = tuple(broken)

    @property
    def external(self):
        return frozenset(self.__external)

    def __context(self, url):
        """Context for the given URL, or None for an external link.

        The passed URL should be a link target found in parsing a web
        page.
        """
        try:
            return self.__contexts[url]
        except KeyError:
            pass
        if '#' in url:
            stem, fragment = url.split('#', 1)
        else:
            stem, fragment = url, None
        for node in self.__root_for(stem):
            tail = stem[len(node.url):].lstrip('/').split('/')
            last = tail.pop()
            for name in tail:
                node = node.subdirectory(name)
            if last or node.isDir:
                node = node.page(last)
            if node.url.endswith('ml') and node.url not in self.__scanned:
                self.__toscan.add(node.url)
            if fragment:
                node = node.reference(fragment)
            return node

        self.__external.add(url)
        return None

    def __root_for(self, url):
        for root in self.__roots:
            if url.startswith(root.url) or url + '/' == root.url:
                yield root
