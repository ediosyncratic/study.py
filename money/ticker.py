# -*- coding: iso-8859-1 -*-
"""Extracting data from the Oslo Børs web pages.

Exports:
  report(ticker) -- get summary of current state of ticker
  update(data, svg) -- add data from summary to an svg history graph

The parser could fairly straightforwardly be adapted to parse the whole
stockList page and provide data for all stocks.  However, I only actually want
one stock at a time.

$Id: ticker.py,v 1.5 2008-10-19 22:15:56 eddy Exp $
"""

# Parser for Oslo Børs ticker pages:
from urllib import urlopen
from HTMLParser import HTMLParser

class StockPageParser (HTMLParser):
    def process(self, name, wget=urlopen,
                url='http://www.oslobors.no/markedsaktivitet/stockOverview'):
        self.__stack, self.__keys, self.data = [], [], None
        fd = wget(url + '?newt__ticker=' + name)
        try:
            try:
                while True:
                    line = fd.readline()
                    if not line: break
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

    def handle_data(self, data):
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
                self.data[keys[i]] = self.data.get(keys[i], '') + data

del HTMLParser, urlopen
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
from study.cache.property import Cached, lazyattr
class StockSVG (Cached):
    from xml.dom.minidom import parse
    def __init__(self, path, ingest=parse):
        self.__path = path
        self.__dom = ingest(path)
        assert self.__dom.documentElement.tagName == 'svg'
    del parse

    def __del__(self): self.__dom.unlink()

    def __id_by_tag(self, tag):
        for node in self.__dom.getElementsByTagName(tag):
            try: nom = node.attributes['id']
            except KeyError: pass
            else: yield node, nom.value
        raise StopIteration

    def save(self):
        if 0: # 1 toggle when testing !
            fd = open(svg + '.new')
            try: dom.writexml(fd, '', '  ', '\n')
            finally: fd.close()
            move(svg + '.new', svg)
        else:
            print self.__dom.toxml('utf-8')

    def __revisepaths(self, dayval, data):
        top = None
        for line, nom in self.__id_by_tag('path'):
            for k, v in data.items():
                if k.lower() == nom:
                    value = v
                    break
            else: continue
            price = float(value)
            if top is None or price > top: top = price

            # Multiply value by 100
            tail = '00'
            while tail and '.' in value:
                if '.' == value[-1]: value = value[:-1]
                else:
                    row = value.split('.')
                    row[-2], row[-1] = row[-2] + row[-1][0], row[-1][1:]
                    value = '.'.join(row)
                    tail = tail[1:]
            if '.' == value[-1]: value = value[:-1]
            value = value + tail

            path = line.attributes['d']
            assert dayval >= int(path.value.split()[-1].split(',')[0][1:]), \
                   'Update got out-of-date "new" data'

            more = ' L%d,' % dayval
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

    import os, datetime, time

    def readdate(what,
                 dt=datetime,
                 date=datetime.date, parse=time.strptime):
        try: when = parse(what, '%d %b %Y') # issue: %b depends on locale
        except ValueError: pass
        else: return date(when.tm_year, when.tm_mon, when.tm_mday)

        when = parse(what, '%d %b') # issue: %b depends on locale
        # That may raise ValueError again - try other formats ?

        now = date.today()
        when = date(now.year, when.tm_mon, when.tm_mday)
        if when > now: # year rolled round since page generated ?
            then = date(now.year - 1, when.tm_mon, when.tm_mday)
            if (now - then) < (when - now):
                return then
        return when

    @lazyattr
    def startdate(self, mode=None, getdate=readdate):
        assert mode is None
        for node, nom in self.__id_by_tag('text'):
            if nom == 'date-origin':
                assert node.firstChild is node.lastChild
                return getdate(node.firstChild.nodeValue)

        raise AttributeError('No id="date-origin" text node found in SVG',
                             self.__path)

    @lazyattr.group(2)
    def __price_axis(self, mode=None):
        price = date = None
        for node, nom in self.__id_by_tag('line'):
            if nom == 'date-axis': date = node
            elif nom == 'price-axis': price = node
        assert None not in (price, date)
        return price, date
    __price_axis, __date_axis = __price_axis

    @lazyattr.group(3)
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
        self.__price_labels, self.__price_axis, self.__fix_vertical
        # TODO: Adjust price axis, and labels if that requires it

    def __rescale_date(self, when, view):
        # TODO: Adjust date axis and labels
        self.__date_labels, self.__date_axis

    def update(self, data,
               move=os.rename, date=datetime.date, getdate=readdate):
        """Update an svg graph of stock ticker data from report().

        Required argument, data, is a dictionary mapping headings to data, as
        obtained by report (q.v.).  Updates the SVG's DOM to extend its graph
        with the data supplied.\n"""
        now = date.today()
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

        top = self.__revisepaths(dayval * 10, data)
        view = self.__dom.documentElement.attributes['viewBox']

        trans = self.__fix_vertical.attributes['transform']
        assert trans.value[:12] == 'translate(0 ' and trans.value[-1] == ')'
        vert = int(trans.value[12:-1])
        if top * 100 > vert: self.__rescale_price(top, view)
        self.__rescale_date(when, view)

    del os, datetime, time, readdate
del Cached, lazyattr

