# -*- coding: iso-8859-1 -*-
"""Extracting data from the Oslo Børs web pages.

Exports:
  report(ticker) -- get summary of current state of ticker
  update(data, svg) -- add data from summary to an svg history graph

The parser could fairly straightforwardly be adapted to parse the whole
stockList page and provide data for all stocks.  However, I only actually want
one stock at a time.
"""

# Parser for Oslo Børs ticker pages:
from urllib2 import urlopen, Request
from HTMLParser import HTMLParser

class StockPageParser (HTMLParser):
    def process(self, name, wget=urlopen, req=Request,
                url='http://www.oslobors.no/markedsaktivitet/stockOverview'):
        self.__stack, self.__keys, self.data = [], [], None
        fd = wget(req(url + '?newt__ticker=' + name, None,
                      { 'Accept-Language':
                        'en-GB;q=1.0, en;q=0.9, no-nb;0.5, no-nn;0.4' }))
        try:
            try:
                while True:
                    line = fd.readline()
                    if not line: break
                    # <bodge> work round broken attributes in generated page ...
                    ind = line.find('"colspan=') + 1
                    if ind > 0: line = line[:ind] + ' ' + line[ind:]
                    # </bodge>
                    self.feed(line)
            except:
                try: del self.__cell
                except AttributeError: pass
                raise
        finally:
            fd.close()

        self.close()
        data = self.data
        del self.__stack, self.__keys, self.data
        for k, v in data.items():
            if v == '-': del data[k]
        return data

    def handle_starttag(self, tag, attrs):
        stack = self.__stack
        n = len(stack)
        if n == 0:
            if tag == 'div':
                if dict(attrs).get('id', '') == "manamind_stockQuote_table_table":
                    stack.append(tag)
        elif n == 1:
            if tag == 'tr':
                stack.append(tag)
                self.__cell = 0
        elif n == 2:
            if tag in ('th', 'td'):
                stack.append(tag)
                bok = dict(attrs)
                self.__cell += int(bok.get('colspan', '1'))

        elif tag in ('th', 'td'):
            print tag, self.__stack

    # def handle_startendtag(tag, attrs): pass

    def handle_endtag(self, tag):
        stack = self.__stack
        while stack and tag in stack:
            if stack[-1] == 'tr':
                assert tag == 'tr', 'Row should always be closed explicitly'
                if self.data is None:
                    # finish off header row, prepare for data row
                    while len(self.__keys) < self.__cell:
                        self.__keys.append('')
                    self.data = {}
                del self.__cell
            if tag == stack.pop(): break

    def handle_data(self, data,
                    notoeng={ 'Siste': 'Last', 'Tid': 'Time',
                              'H\xf8y': 'High', 'Lav': 'Low',
                              'Kj\xf8per': 'Buy', 'Selger': 'Sell' }):
        if not data: return # not interesting
        try: i = self.__cell
        except AttributeError: return
        keys = self.__keys
        if self.data is None:
            assert self.__stack[-1] == 'th' and i >= len(keys)
            while i > len(keys): keys.append('')
            keys.append(data)
            assert keys[i] is data
        else:
            assert self.__stack[-1] == 'td'
            assert 0 <= i < len(keys), (i, keys, data, self.__stack)
            if keys[i] and data:
                key = keys[i]
                try: key = notoeng[key]
                except KeyError: pass
                else: data = '.'.join(data.split(','))
                try: key.lower()
                except UnicodeDecodeError: pass
                else: self.data[key] = self.data.get(key, '') + data

del HTMLParser, urlopen, Request
def report(ticker, parser=StockPageParser()):
    """Return summary data for the selected stock.

    Single argument, name, is the 'ticker' of the stock: for a full list of
    available tickers, see the 'All shares' table on
        http://www.oslobors.no/markedsaktivitet/stockList
    Since all tickers are all-upper-case, the given ticker shall be upper-cased
    when used, so you can use some more pleasant capitalization of it if you
    wish.

    Returns a dictionary, mapping headings from the given stock's summary table
    to data provided for each heading.\n"""
    return parser.process(ticker.upper())

del StockPageParser

# Managing an SVG graph of results
from study.cache.property import Cached, lazyprop
class StockSVG (Cached):
    from xml.dom.expatbuilder import parse
    def __init__(self, path, ingest=parse):
        self.__path = path
        self.__dom = ingest(path)
        assert self.__dom.documentElement.tagName == 'svg'
    del parse

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

    import os
    def save(self, move=os.rename):
        name = self.__path + '.new'
        fd = open(name, 'w')
        try: fd.write(self.toxml())
        finally: fd.close()
        move(name, self.__path)
    del os

    def indent(col, tab): # tool, deleted below
        """Start a new line.

        Arguments:
          col -- column to which to indent the new line
          tab -- width of each tab character used in indentation
        """
        t, s = divmod(col, tab)
        return '\n' + '\t' * t + ' ' * s

    def inwidth(text, tab): # tool, deleted below
        """Work out how deep an indent the given text implies.

        The given text, or the portion of it up to the first newline if there
        is one, is taken to be text preceding what we want our indented text
        to line up under; this function returns the column offset at which the
        indented text is to start.\n"""
        return len(text.expandtabs(tab).split('\n')[0])

    import re
    def rebreak(data, prior, tab,
                chop=re.compile(r'\b\s\s+\b'),
                dent=indent, wide=inwidth): # another tool
        """Split up a d="..." attribute nicely.

        Arguments:
          data -- the value used for the d attribute
          prior -- text prior to d=... on that line
        Indentation is determined from the content of prior.

        The xml.dom.expatbuilder parser replaces each blank (including tabs
        and newlines) in the value of an attribute with a space, wrecking any
        nice formatting one may have done.  One can see where the damage has
        been done as long as the original data only used single spaces for the
        routine separation of data items: canonicalisation makes each
        line-break and its indentation into a sequence of several adjacent
        spaces.  So this finds each of those and restores the indentation
        needed to line the following data up under the start of the attribute
        value.  If the block of consecutive spaces was longer than this
        accounts for, any surplus spaces are assumed to have originated from
        blank lines, which are duly restored. Finally, the result ends with a
        newline and indentation sufficient to line up following attributes
        under the start of d=...\n"""
        # Work out indentations:
        tail = wide(prior, tab)
        begin = dent(tail + 3, tab) # 3 is for 'd="'
        cleaned = ""
        while True: # for each many-space separator:
            was = chop.search(data)
            if not was: break
            cleaned += data[:was.start()]
            # Restore line-breaks and indentation:
            for ch in was.group(0)[len(begin):]:
                cleaned += '\n'
            cleaned += begin
            data = data[was.end():]
        return cleaned + data + dent(tail, tab)

    def rebox(elm, prior, tab, fill,
              vbox=re.compile(r'\s*(viewBox="[^"]*")\s*'),
              dent=indent, wide=inwidth):
        """Reflow the SVG element itself, with viewBox on its own line.

        The viewBox element is what changes between revisions of a graph
        updated by this script, so put it on a line of its own to make the
        difference between revisions easy to read.  At the same time, avoid
        unduly long lines.  Aside from viewBox itself (which should never be
        too long for the fill column), no attribute value contains space, so
        we can be simplistic about the re-flowing (I'd otherwise want to avoid
        splitting within an attribute value).\n"""

        lead = wide(prior, tab)
        newline = dent(lead, tab)
        box = vbox.search(elm)
        if box: lines = [ elm[:box.start()], box.group(1), elm[box.end():] ]
        else: lines = [ elm ]

        i = 0
        while i < len(lines):
            if '\n' in lines[i]: lines[i:i+1] = lines[i].split('\n')
            if i < 1: line, big = lines[i].rstrip(), fill
            else: line, big = lines[i].strip(), fill - lead

            if len(line.expandtabs(tab)) < big:
                lines[i] = line
            else:
                w = big
                while w > 0 and not line[w].isspace(): w -= 1
                while w > 0 and line[w-1].isspace(): w -= 1
                if w > 0: lines[i:i+1] = [ line[:w], line[w:] ]
                else: # no space in line[:big]; cut as soon after as possible
                    w = big
                    try:
                        while not line[w].isspace(): w += 1
                        lines[i:i+1] = [ line[:w], line[w:] ]
                    except IndexError: pass # no space in line at all
            i += 1

        return newline.join(lines)

    del indent, inwidth

    def toxml(self,
              ismode=re.compile(r'-\*-(.*)-\*-'),
              tabwidth=re.compile(r'tab-width:\s*(\d+)$'),
              fillcol=re.compile(r'fill-column:\s*(\d+)$'),
              svgelm=re.compile(r'(<svg\s+)([^>]*)>', re.MULTILINE),
              points=re.compile(r'^([\t ]+.+\b)d=("[^"]+")\s*',
                                re.MULTILINE),
              # Common settings I use in files (mode line may over-ride):
              TAB=8, # tab-width
              FILL=78, # fill-column
              # Tools from above:
              chunk=rebreak, flow=rebox):
        """Re-serialize the DOM tree to SVG.

        This restores (to the best of its ability) assorted damage the expat
        parser does to the content when round-tripping, so that (for the most
        part) what's changed is only what .update() was told to change.\n"""
        out = self.__dom.toxml('utf-8')

        # gnn ... that puts the mode-line comment after the first newline :-(
        ind = out.find('\n')
        cut = out.find('-->', ind) + 3
        mode = out[ind+1:cut]
        conf = ismode.search(mode)
        if conf:
            # Parse mode-line:
            for frag in conf.group(1).split(';'):
                tab = tabwidth.match(frag.strip())
                if tab: TAB = int(tab.group(1))

                fill = fillcol.match(frag.strip())
                if fill: FILL = int(fill.group(1))
            # Put mode-line on first line
            out = out[:ind] + mode + '\n' + out[cut:]

        # ... joins all attributes of the svg element onto one line ...
        svg = svgelm.search(out)
        assert svg, "Failed to find <svg ...> element in serialized DOM"
        out = out[:svg.start(2)] + flow(svg.group(2), svg.group(1),
                                        TAB, FILL) + out[svg.end(2):]

        # ... and kills newlines in attribute values :-(
        cleaned = ""
        while True: # for each d="..." attribute:
            was = points.search(out)
            if not was: break
            cleaned += out[:was.start(2)] + chunk(was.group(2),
                                                  was.group(1), TAB)
            out = out[was.end():]
        out = cleaned + out

        if out[-1] != '\n': return out + '\n'
        return out
    del re, rebreak, rebox

    def text_by_100(value):
            # Multiply value by 100, without converting from text:
            tail = '00'
            while tail and '.' in value:
                if '.' == value[-1]: value = value[:-1]
                else:
                    row = value.split('.')
                    row[-2], row[-1] = row[-2] + row[-1][0], row[-1][1:]
                    value = '.'.join(row)
                    tail = tail[1:]

            if tail: return value + tail
            if '.' == value[-1]: return value[:-1]
            return value

    def __revise_paths(self, dayval, data, rescale=text_by_100):
        top = None
        for line, nom in self.__id_by_tag('path'):
            for k, v in data.items():
                if k.lower() == nom:
                    value = v
                    break
            else: continue
            price = float(value)
            if top is None or price > top: top = price

            path = line.attributes['d']
            assert dayval >= int(path.value.split()[-1].split(',')[0][1:]), \
                   'Update got out-of-date "new" data'

            more, value = ' L%d,' % dayval, rescale(value)
            off = path.value.find(more)
            if off < 0: path.value += more + value
            else:
                off += len(more)
                prior = path.value[off:].split()[0]
                if prior != value:
                    print 'Warning: changing datum for', nom, \
                          'from', prior, 'to', value, 'at', more[2:-1]
                    path.value = (path.value[:off] + value +
                                  path.value[off + len(prior):])

        assert top is not None
        return top

    del text_by_100

    @lazyprop.group(2)
    def __price_axis(self, mode=None):
        price = date = None
        for node, nom in self.__id_by_tag('line'):
            if nom == 'date-axis': date = node
            elif nom == 'price-axis': price = node
        assert None not in (price, date)
        return price, date
    __price_axis, __date_axis = __price_axis

    @lazyprop.group(3)
    def __price_labels(self, mode=None):
        price = date = shunt = None
        for node, nom in self.__id_by_tag('g'):
            if nom == 'fix-vertical': shunt = node
            elif nom == 'date-labels': date = node
            elif nom == 'price-labels': price = node
        assert None not in (date, price, shunt)
        return date, price, shunt
    __date_labels, __price_labels, __fix_vertical = __price_labels

    def __rescale_price(self, top, view):
        if top > int(top): top = 1 + int(top)
        else: top = int(top)
        prior = self.__price_axis.attributes['y2']
        grow = top * 100 - int(prior.value)
        if 0 < grow:
            prior.value = str(top * 100)

            box = view.value.split()
            box[3] = str(int(box[3]) + grow)
            view.value = ' '.join(box)

            trans = self.__fix_vertical.attributes['transform']
            assert trans.value[:12] == 'translate(0 ' and trans.value[-1] == ')'
            vert = int(trans.value[12:-1]) + grow
            trans.value = 'translate(0 %d)' % vert

            # TODO: Adjust labels if necessary
            self.__price_labels

    def __rescale_date(self, dayval, when, view):
        prior = self.__date_axis.attributes['x2']
        grow = dayval - int(prior.value)
        if 0 < grow:
            prior.value = str(dayval)
            box = view.value.split()
            box[2] = str(int(box[2]) + grow)
            view.value = ' '.join(box)
            # TODO: Adjust date labels
            self.__date_labels

    import datetime, time, locale
    from study.snake.localise import withlocale
    def readdate(what, now=None, date=datetime.date, parse=time.strptime):
        try: when = parse(what, '%d %b %Y')
        except ValueError: pass
        else: return date(when.tm_year, when.tm_mon, when.tm_mday)

        if now is None: now = date.today()
        if what.endswith('.'): when = what
        else: when = what + '.'
        when = parse(when, '%d %b')
        when = date(now.year, when.tm_mon, when.tm_mday)
        if when > now: # year rolled round since page generated ?
            then = date(now.year - 1, when.month, when.day)
            if (now - then) < (when - now):
                return then
        return when

    def readtime(what, getdate=withlocale(locale.LC_TIME, "nb_NO")(readdate),
                 today=datetime.date.today, parse=time.strptime):
        now = today()
        try: return getdate(what, now)
        except ValueError: pass
        parse(what, '%H:%M') # daytime format
        # That may raise ValueError again - try other formats ?
        return now

    del datetime, time, locale, withlocale

    @lazyprop
    def startdate(self, mode=None, getdate=readdate):
        assert mode is None
        for node, nom in self.__id_by_tag('text'):
            if nom == 'date-origin':
                assert node.firstChild is node.lastChild
                return getdate(node.firstChild.nodeValue)

        raise AttributeError('No id="date-origin" text node found in SVG',
                             self.__path)

    def update(self, data, getdate=readtime):
        """Update an svg graph of stock ticker data from report().

        Sole argument, data, is a dictionary mapping headings to data, as
        obtained by report (q.v.).  Updates the SVG's DOM to extend its graph
        with the data supplied.\n"""
        for k, v in data.items():
            if k.lower() in ('date', 'time'):
                try: when = getdate(v)
                except ValueError:
                    print 'Failed to parse %s "%s" as a date' % (k, v)
                else: break
        else:
            raise ValueError('No date provided in ticker data', data)

        dayval = when.toordinal() - self.startdate.toordinal()
        assert dayval > 0
        dayval *= 10

        view = self.__dom.documentElement.attributes['viewBox']
        self.__rescale_date(dayval, when, view)
        top = self.__revise_paths(dayval, data)
        self.__rescale_price(top, view)

    del readdate, readtime
del Cached, lazyprop

