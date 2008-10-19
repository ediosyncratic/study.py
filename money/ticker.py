# -*- coding: iso-8859-1 -*-
"""Extracting data from the Oslo Børs web pages.

Exports:
  report(ticker) -- get summary of current state of ticker
  update(data, svg) -- add data from summary to an svg history graph

The parser could fairly straightforwardly be adapted to parse the whole
stockList page and provide data for all stocks.  However, I only actually want
one stock at a time.

$Id: ticker.py,v 1.2 2008-10-19 15:33:56 eddy Exp $
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
from xml.dom.minidom import parse
import os, datetime, time

def readdate(what, now,
             date=datetime.date, parse=time.strptime):
    try: when = parse(what, '%d %b %Y') # issue: %b depends on locale
    except ValueError: pass
    else: return date(when.tm_year, when.tm_mon, when.tm_mday)

    when = parse(what, '%d %b') # issue: %b depends on locale
    # That may raise ValueError again - try other formats ?
    when = date(now.year, when.tm_mon, when.tm_mday)
    if when > now: # year rolled round since page generated ?
        then = date(now.year - 1, when.tm_mon, when.tm_mday)
        if (now - then) < (when - now):
            return then
    return when

def update(data, svg,
           ingest=parse, move=os.rename,
           date=datetime.date, getdate=readdate):
    """Update an svg graph of stock ticker data from report().

    Required argument, data, is a dictionary mapping headings to data, as
    obtained by report (q.v.).  Second argument is an SVG graph in a format I
    should probably document at some point ...  Updates the SVG to extend its
    graph with the data supplied.\n"""
    now = date.today()
    for k, v in data.items():
        if k.lower() in ('date', 'time'): # Parse v as a date:
            try: when = getdate(v, now)
            except ValueError:
                print 'Failed to parse %s "%s" as a date' % (k, v)
            else: break
    else:
        raise ValueError('No date provided in ticker data', data)

    dom = ingest(svg)
    for label in dom.getElementsByTagName('text'):
        if not label.attributes.get('id', None): continue
        if label.attributes['id'].value == 'date-origin':
            assert label.firstChild is label.lastChild
            startdate = getdate(label.firstChild.nodeValue, now)
            break
    else:
        raise ValueError('No id="date-origin" text node found in SVG', svg)
    dayval = when.toordinal() - startdate.toordinal()
    assert dayval > 0
    dayval *= 10

    top = None
    for line in dom.getElementsByTagName('path'):
        nom = line.attributes.get('id', None)
        if not nom: continue
        nom = nom.value
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
                path.value = path.value[:off] + value + path.value[off+len(prior):]

    # Adjust price axis if necessary
    assert top is not None

    if 1: # 0 toggle when testing !
        fd = open(svg + '.new')
        try: dom.writexml(fd, '', '  ', '\n')
        finally: fd.close()
        move(svg + '.new', svg)
    else:
        print dom.toxml('utf-8')
    dom.unlink()

del parse, os, readdate

