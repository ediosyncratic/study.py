"""Parser for convenient access to and transformation of SVG data.
"""

class SVG (object):
    """Accessor for an SVG document.

    Constructor takes the name of the source file, raising IOError if it
    doesn't exist and SyntaxError if it is invalid.  The file-name specified
    to the constructor is remembered for later use when .save() is
    called.\n"""
    from xml.dom.minidom import parse
    from xml.parsers.expat import ExpatError
    def __init__(self, source, target=None, ingest=parse, fail=ExpatError):
        """Constructor.

        Required argument, source, is the path of the file containing the SVG
        to read.  Optional second argument, target, is the path to use when
        saving later; if unspecified, source is used.
        """
        if target is None: self.__path = source
        else: self.__path = target
        self.__dom = ingest(source)
        assert self.__dom.documentElement.tagName == 'svg'
    del parse, ExpatError

    def __del__(self): self.__dom.unlink()

    def id_by_tag(self, tag):
        """Iterate over nodes, of given kind, with id attribute.

        Required argument, tag, is the element type of interest.  Those which
        have no id attribute are ignored.  For each of the remainder, this
        yields a twople; the node and the value of its id attribute.\n"""
        for node in self.__dom.getElementsByTagName(tag):
            try: nom = node.attributes['id']
            except KeyError: pass
            else: yield node, nom.value

    def get_node(self, id, tag=None):
        if tag is None:
            raise NotImplementedError(
                "I haven't yet worked out how to do this")
        else:
            for node, nom in self.id_by_tag(tag):
                if nom == id: return node

        raise ValueError('No such node', id, tag, self.__path)

    import os
    def save(self, move=os.rename):
        name = self.__path + '.new'
        fd = open(name, 'w')
        try: fd.write(self.toxml())
        finally: fd.close()
        move(name, self.__path)
    del os

    def toxml(self):
        out = self.__dom.toxml('utf-8')
        # gnn ... puts mode-line comment after first newline :-(
        ind = out.find('\n')
        if ind < 0 or out[ind+1:ind+7] != '<!--*-':
            return out

        cut = out.find('-->', ind + 7) + 3
        mode = out[ind+1:cut]
        if out[cut] == '\n': cut += 1 # avoid duplicate '\n'
        return out[:ind] + mode + '\n' + out[cut:]
