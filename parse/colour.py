"""Handling of colour semantics.

See study.LICENSE for copyright and license information.
"""

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

from HTMLParser import HTMLParser
class GVParse (HTMLParser):
    def __init__(self, bok):
	self.__bok = bok

    def handle_starttag(self, tag, attrs):
	raise NotImplementedError

    def handle_startendtag(self, tag, attrs):
	raise NotImplementedError

    def handle_endtag(self, tag):
	raise NotImplementedError

    def handle_data(self, data):
	raise NotImplementedError

def get_gv_rgb(parse=GVParse,
	       local='/usr/share/doc/graphviz/html/info/colors.html',
	       x11=get_x11_rgb):
    # see http://vortex/doc/graphviz/html/info/colors.html
    bok = {}
    # Initially fill with system x11; let graphviz over-ride any over-lap.
    for k, v in x11().getitems():
	bok['/x11/' + k] = v

    try: fd = open(local)
    except IOError:
	raise NotImplementedError, 'Need URL for public version'

    try:
	parser = parse(bok)
	while True:
	    line = fd.readline()
	    if line: parser.feed(line)
	    else: break
	parser.close()
    finally: fd.close()
    return bok

from study.cache.mapping import LazyDict

class Colour (object):
    def __init__(self, red, green, blue, alpha=None):
	if alpha is not None: self.alpha = alpha
	self.red, self.green, self.blue = red, green, blue

    alpha = 0xff # default, if not specified.

    # Ruby idiom: {from,to}_otherformat() constructors:

    @staticmethod
    def from_x11(txt, bok=LazyDict(fill=get_x11_rgb)):
	try: r, g, b = bok[txt]
	except KeyError: raise ValueError('Not a valid X11 colour name', txt)
	return Colour(r, g, b)

    @staticmethod
    def from_gv(txt, scheme='x11', bok=LazyDict(fill=get_gv_rgb)):
	# see: http://vortex/doc/graphviz/html/info/attrs.html#k:color
	# TODO: is it possible to borrow graphviz's colour parser ?
	if txt == 'transparent': return Colour(0, 0, 0, 0)
	try:
	    if txt[0] != '/': r, g, b = bok['/%s/%s' % (scheme, txt)]
	    elif txt[:2] == '//': r, g, b = bok['/%s/%s' % (scheme, txt[2:])]
	    elif '/' in txt[1:]: r, g, b = bok[txt]
	    else: r, g, b = bok['/x11' + txt]
	except KeyError: pass
	else: return Colour(r, g, b)

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
	    return Colour.from_hsv(*map(float(row)))
	elif len(row) > 1:
	    raise ValueError('Malformed HSV-style graphviz colour code', txt)

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
    def from_hsv(hue, saturation, value): # ranges 0.0 <= H, S, V <= 1.0
	raise NotImplementedError

    def to_hsv(self):
	raise NotImplementedError

    # Combine attributes in some standard ways:

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
