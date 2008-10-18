# -*- coding: iso-8859-1 -*-
"""Extracting data from the Oslo Børs web pages.

The parser could fairly straightforwardly be adapted to parse the whole
stockList page and provide data for all stocks.  However, I only actually want
one stock at a time.

$Id: ticker.py,v 1.1 2008-10-18 15:18:21 eddy Exp $
"""

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
def report(ticker='OPERA', parser=StockPageParser()):
    """Return summary data for the selected stock.

    Single argument, name, is the 'ticker' of the stock; it defaults to 'OPERA'.
    For a full list of available tickers, see the 'All shares' table on
    http://www.oslobors.no/markedsaktivitet/stockList

    Returns a dictionary, mapping headings from the given stock's summary table
    to data provided for each heading.\n"""
    return parser.process(ticker.upper())
del StockPageParser
