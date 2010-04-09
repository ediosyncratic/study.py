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
from study.parse.tabular import Table

class StockPageParser (HTMLParser):
    __table = None
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
                try: del self.__table
                except AttributeError: pass
                raise
        finally:
            fd.close()

        self.close()
        data = self.data
        del self.__stack, self.__keys, self.data
        return data

    def handle_starttag(self, tag, attrs, T=Table,
                        magic="manamind_stockQuote_table_table"):
        stack = self.__stack
        n = len(stack)
        if n == 0:
            if tag == 'div':
                if dict(attrs).get('id', '') == magic:
                    stack.append(tag)
                    self.__table = T()
        elif n == 1:
            if tag == 'tr':
                stack.append(tag)
                self.__table.open_row()
        elif n == 2:
            if tag in ('th', 'td'):
                stack.append(tag)
                bok = dict(attrs)
                i = int(bok.get('colspan', '1'))
                self.__table.open_cell()
                while i > 1:
                    i -= 1
                    self.__table.close_cell()
                    self.__table.open_cell()

        elif tag in ('th', 'td'):
            print 'Surprise:', tag, self.__stack

    # def handle_startendtag(tag, attrs): pass

    def handle_endtag(self, tag):
        stack = self.__stack
        while stack and tag in stack:
            top = stack.pop()
            if self.__table and top in ('th', 'td'):
                self.__table.close_cell()

            elif top == 'tr':
                assert tag == 'tr', 'Row should always be closed explicitly'
                self.__table.close_row()

            elif top == 'div':
                self.data = self.__table.close(self.__factory)
                del self.__table

            if top == tag: break

    def handle_data(self, data):
        if self.__table is None: return # not interested
        if data.strip():
            if len(self.__table) > 0: assert self.__stack[-1] == 'td'
            else: assert self.__stack[-1] == 'th'
        self.__table.add_data(data)

    class __factory (Table.Transform):
        def first(self, headings,
                  notoeng={ 'Siste': 'Last', 'Tid': 'Time',
                            'H\xf8y': 'High', 'Lav': 'Low',
                            'Kj\xf8per': 'Buy', 'Selger': 'Sell' },
                  cash=( 'Last', 'High', 'Low' ),
                  munge=lambda t: '.'.join(''.join(t.split('.')).split(',')),
                  ignore=lambda t: ''):
            heads, xfrm = [], []
            for det in headings:
                det, f = det.strip(), lambda x: x
                try: it = notoeng[det]
                except KeyError:
                    it = det
                    if it not in notoeng.values(): f = ignore
                else:
                    if it in cash: f = munge
                heads.append(it)
                xfrm.append(f)

            self.__keys, self.__xfrm = tuple(heads), tuple(xfrm)
            return []

        def each(self, row):
            # Apply any needed text transformations:
            return tuple(map(lambda f, e: f(e.strip()), self.__xfrm, row))

        def package(self, all):
            assert len(all) == 1, all
            return dict(filter(lambda (k, v): k and v and v != '-',
                               map(lambda *s: s, self.__keys, all[0])))

del HTMLParser, urlopen, Request, Table
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

    import re
    def toxml(self,
              find=re.compile(r'(\n\s+)([^"\n]+="[^"\n]+\d)\b   +\b'),
              mend=lambda m: m.expand(r'\1\2\1')):
        out = self.__dom.toxml('utf-8')
        # gnn ... puts mode-line comment after first newline :-(
        ind = out.find('\n')
        cut = out.find('-->', ind) + 3
        out = out[:ind] + out[ind+1:cut] + '\n' + out[cut:]
        # ... and turns in-data line-breaks into strings of spaces :-(
        while True:
            out, n = find.subn(mend, out)
            if n < 1: break
        if out[-1] != '\n': return out + '\n'
        return out
    del re

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

