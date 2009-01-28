"""Handling of colour semantics.

$Id: colour.py,v 1.1 2009-01-28 08:42:51 eddy Exp $
"""

# TODO: borrow graphviz's colour parser
# see: http://vortex/doc/graphviz/html/info/attrs.html#k:color

def get_x11_rgb(src='/etc/X11/rgb.txt'):
    fd, bok = open(src), {}
    try:
	while True:
	    line = fd.readline()
	    if not line: break
	    if line[0] == '!': continue
	    row = line.split()
	    if len(row) > 3:
		try: bok[' '.join(row[3:])] = tuple(map(int, row[:3]))
		except ValueError:
		    print 'Ignoring malformed line in %s:' % src, line
		    # wilful blank line after error message.
    finally: fd.close()
    return bok
from study.cache.mapping import LazyDict

class Colour (object):
    def __init__(self, red, green, blue, alpha=None):
	if alpha is not None: self.alpha = alpha
	self.red, self.green, self.blue = red, green, blue

    alpha = 0xff # default, if not specified.

    @staticmethod
    def from_x11(txt, bok=LazyDict(fill=get_x11_rgb)):
	try: r, g, b = bok[txt]
	except KeyError: raise ValueError('Not a valid X11 colour name', txt)
	return Colour(r, g, b)

    @staticmethod
    def from_gv(txt): # Ruby got this idiom right ...
	if txt.lstrip()[0] == '#':
	    ws = txt.lstrip()[1:].split()
	    cols = []
	    for w in ws:
		while len(w) > 2:
		    cols.append(w[:2])
		    w = w[2:]
		cols.append(w)
	    if len(cols) == 3:
		r, g, b = map(lambda x: int(x, 16), cols)
		a = None
	    elif len(cols) == 4:
		r, g, b, a = map(lambda x: int(x, 16), cols)
	    else:
		raise ValueError('Malformed #... graphviz colour value', txt)

	    return Colour(r, g, b, a)

	row = txt.split('+')
	if len(row) == 3: # HSV; 0.0 <= min(row) <= max(row) <= 1.0
	    raise NotImplementedError, 'digesting graphviz HSV'
	elif len(row) > 1:
	    raise ValueError('Malformed HSV-style graphviz colour code', txt)
	else:
	    raise NotImplementedError, 'decoding graphviz colour names'

    def to_gv(self):
	raise NotImplementedError

    @staticmethod
    def from_css(txt):
	raise NotImplementedError

    def to_css(self):
	raise NotImplementedError

    @staticmethod
    def from_cmyk(cyan, magenta, yellow, black=0):
	raise NotImplementedError

    def to_cmyk(self):
	raise NotImplementedError

    @staticmethod
    def from_hsv(hue, saturation, value):
	raise NotImplementedError

    def to_hsv(self):
	raise NotImplementedError

    @property
    def rgba(self, ig=None):
	return (self.red << 24) | (self.green << 16) | (self.blue << 8) | self.alpha

    @property
    def argb(self, ig=None):
	return (self.alpha << 24) | (self.red << 16) | (self.green << 8) | self.blue

    @property
    def bgra(self, ig=None):
	return (self.blue << 24) | (self.green << 16) | (self.red << 8) | self.alpha

del get_x11_rgb, LazyDict
