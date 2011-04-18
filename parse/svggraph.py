"""Graphing data using SVG.
"""
from study.cache.property import Cached, lazyprop

class SVGgraph (Cached):
    """Manage a DOM object describing an SVG graph.

    Constructor takes the path of the file containing the SVG: if this does
    not exist, a template is used and the new object's .pristine attribute is
    set to True (it is, otherwise, False).  In this case, you must call its
    .label() and .add_curves() to configure it, before trying to add any data
    to it; you should also add some data to the curves.  The file-name
    specified to the constructor is remembered for later use when .save() is
    called.\n"""
    from xml.dom.minidom import parse
    from xml.parsers.expat import ExpatError
    def __init__(self, path, ingest=parse, fail=ExpatError):
        self.__path = path
        try: self.__dom = ingest(path)
        except (IOError, SyntaxError, fail):
            self.__dom = ingest(self.__template)
            self.pristine = True
        assert self.__dom.documentElement.tagName == 'svg'
    del parse, ExpatError

    def __del__(self): self.__dom.unlink()

    def __id_by_tag(self, tag):
        """Iterate over nodes, of given kind, with id attribute.

        Required argument, tag, is the element type of interest.  Those which
        have no id attribute are ignored.  For each of the remainder, this
        yields a twople; the node and the value of its id attribute.\n"""
        for node in self.__dom.getElementsByTagName(tag):
            try: nom = node.attributes['id']
            except KeyError: pass
            else: yield node, nom.value
        raise StopIteration

    def label(self, horiz, vert, title=None, desc=None):
        if not self.pristine:
            raise ValueError('Re-labelling existing graph', self.__path)
        # TODO: add axes, etc., based on horiz, vert
        if title is not None: # FIXME: this is pseudocode
            self.__dom.documentElement.addNode('title', title)
        if desk is not None:
            self.__dom.documentElement.addNode('desc', desc)

        del self.pristine

    def add_curves(self, **paths):
        for node, nom in self.__id_by_tag('g'):
            if nom == 'fix-vertical': break
        else: raise ValueError('No fix-vertical node', self.__path)
        node = node.getElementsByTagName('g')[0]
        assert 'matrix(1 0 0 -1 0 0)' == node.attributes['transform'].value

        create = self.__dom.createElement
        for name in paths.keys():
            kid = create('path')
            kid.setAttribute('d', "")
            kid.setAttribute('id', name)
            kid.setAttribute('stroke', paths[name])
            node.appendChild(kid)

    import os
    __template = os.path.join(os.path.split(__file__)[0],
                              'graph-template.svg')
    pristine = False

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

del Cached, lazyprop
