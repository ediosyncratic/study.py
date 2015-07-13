"""Digesting Nordea's transaction history.

The transaction history I can save from Nordea's online banking site is
most naturally saved as a .mht pseudo-mail-message - so as to preserve
the full content as faithfully as possible - one part of which is the
actual page data (it also includes images, style sheets and
scripts).  This I can unpack using email.message_from_file() and process
using standard html-parsing tools.

The first interesting table, with caption (an icon followed by) "Account
information" (Kontoinformasjon), contains one relevant datum, "Balance
per" (Saldo pr.) dd.mm.yyyy plus irrelevant and spurious information
about today's balance (whenever the form happened to be queried).  This
table also lists the "From date" (Fra&nbsp;dato) and "To date"
(Til&nbsp;dato) for this transaction history list.  The main table has
acption "Account transactions" (Bevegelser p&aring; konto); I want all
of its data.

See study.LICENSE for copyright and license information.
"""
from HTMLParser import HTMLParser
from study.parse.tabular import Table

class NordeaParser (HTMLParser):
    def process(self, filename):
        self.__stack, self.data = [], [], []
        fd = open(filename)
        try:
            while True:
                line = fd.readline()
                if not line: break
                self.feed(line)
        finally: fd.close()
        self.close()
        data = tuple(self.data)
        del self.__stack, self.data
        return data

    def handle_starttag(self, tag, attrs, T=Table):
        stack = self.__stack
        n = len(stack)
        if n == 0:
            if tag == 'table':
                # Ignore outer table used for layout:
                if dict(attrs).get('id', '') != 'container':
                    stack.append(tag)
                    self.__table = T()
        elif n == 1:
            if tag == 'tr':
                stack.append(tag)
                self.__table.open_row()
        elif n == 2:
            if tag in ('th', 'td'):
                stack.append(tag)
                i = int(dict(attrs).get('colspan', '1'))
                self.__table.open_cell()
                # Treat content as being in the last column this cell spans:
                while i > 1:
                    i -= 1
                    self.__table.close_cell()
                    self.__table.open_cell()
                assert i == 1 # i.e. we didn't have a colspan < 1 ...

        elif n and tag in ('th', 'td'):
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

            elif top == 'table':
                self.data.append(self.__table.close(self.__factory))
                del self.__table

            if top == tag: break

    def handle_data(self, data):
        if self.__table is None: return # not interested
        if data.strip(): self.__table.check(self.__stack[-1])
        self.__table.add_data(data)

    # Classes needed for __factory:

    class RawTransform (Table.Transform):
        @staticmethod
        def check(tag): pass

    class Stub (RawTransform):
        @staticmethod
        def first(headings): return []
        @staticmethod
        def each(row): return None
        @staticmethod
        def package(all): return None

    class Transform (RawTransform):
        # Class to provide tool functions needed by Transforms.
        from datetime import date
        # Nordea uses dd.mm.yyyy format for dates:
        @staticmethod
        def _parsedate(text, date=date,
                       keys=('day', 'month', 'year')):
            return date(**dict(zip(keys, [int(x) for x in text.split('.')])))
        del date

        from decimal import Decimal
        @staticmethod
        def _parsenumber(number, D=Decimal):
            assert number == number.strip()
            # discard thousands separators, use . as decimal point
            point = number[-3]
            if point == ',': # scandinavian format
                return D('.'.join(''.join(number.split('.')).split(',')))
            if point == '.': # anglic format
                return D(''.join(number.split(',')))
            # good luck with that:
            return D(number)
        del Decimal

    class Info (Transform):
        # default first, each will do fine
        @classmethod
        def package(cls, all):
            # all[0]: ('Account:', acct-number, 'Balance per dd.mm.yyyy:', amt)
            # all[1]: ignore
            # all[2]: ('From date:', 'dd.mm.yyyy', ignore, ignore)
            assert all[2][1] == all[0][2][-11:-1]
            # all[3]: ('To date:', 'dd.mm.yyyy', empty, '&nbsp;')
            return { 'account': all[1],
                     'initial': cls._parsenumber(all[0][-1]),
                     'start': cls._parsedate(all[2][1]),
                     'end': cls._parsedate(all[3][1]) }

    class Transact (Transform):
        def check(self, tag):
            if len(self) > 0: assert tag == 'td'
            else: assert tag == 'th'

        def first(self, headings,
                  notoeng={ 'Bokf.dato': 'Booked date',
                            'Rentedato': 'Interest date',
                            'Tekst': 'Text',
                            'Buntreferanse': 'Bunch reference',
                            'Referanse': 'Reference',
                            'Ut av konto': 'Out',
                            'Inn p&aring; konto': 'In' },
                  cash=( 'Out', 'In' ),
                  ignore=lambda t: ''):

            heads, xfrm = [], []
            for det in headings:
                f = lambda x: x
                try: it = notoeng[det]
                except KeyError:
                    it = det
                    if it not in notoeng.values():
                        print "Unexpected column heading:", it
                        f = ignore
                # Even when text is in English, numbers are Scandinavian:
                if it in cash: f = self._parsenumber
                elif it[-4:] == 'date': f = self._parsedate
                heads.append(it)
                xfrm.append(f)

            self.__keys, self.__xfrm = tuple(heads), tuple(xfrm)
            return []

        class Transaction (tuple):
            # A simple named tuple type:
            __names = 'booked', 'interest', 'text', 'bunch', 'ref', 'change'
            def __new__(cls, bok): # automagically an @staticmethod
                return tuple.__new__(cls, [bok.get(n) for n in cls.__names])

            for i, nom in enumerate(__names):
                exec('\n'.join(['@property',
                                'def %s(self): return self[%d]' % (nom, i),
                                '']))
            del i, nom

            def as_dict(self, ns=__names):
                return dict(zip(ns, self))

            def __cmp__(self, other):
                """Comparison of Transaction objects.

                Earlier is less, as is more negative change.  Changes to
                ref, bunch and text are noticed, but given lower
                priority.  See also .similar()\n"""
                return cmp(self.booked, other.booked) \
                    or cmp(self.interest, other.interest) \
                    or cmp(self.change, other.change) \
                    or cmp(self.ref, other.ref) \
                    or cmp(self.bunch, other.bunch) \
                    or cmp(self.text, other.text)

            def similar(self, other):
                """Catch repetition between statements.

                If two items taken from separate statements are similar,
                it is likely they are the same item repeated; if they
                compare equal, they definitely are, but we should at
                least warn if there are others that are too similar by
                half.\n"""
                return (self.booked == other.booked and
                        self.change == other.change and
                        self.ref == other.ref)

        def each(self, row, T=Transaction,
                 short={ 'Booked date': 'booked',
                         'Interest date': 'interest',
                         'Text': 'text',
                         'Bunch reference': 'bunch',
                         'Reference': 'ref',
                         'In': 'change', 'Out': 'change' }):
            assert frozenset(short.values()) == frozenset(self.__names)
            bok = {}
            for k, v in zip(self.__keys,
                            # Apply any needed text transformations:
                            [f(e.strip()) for f, e in zip(self.__xfrm, row)]):
                assert (k != 'In' or v > 0) and (k != 'Out' or v < 0)
                bok[short[k]] = v

            return T(bok)
        del Transaction

        # default package is fine

    def __factory(self, caption,
                  cls={ 'Account information': Info,
                        'Kontoinformasjon': Info,
                        'Account transactions': Transact,
                        'Bevegelser p&aring; konto': Transact },
                  junk=Stub):
        try: return cls[caption.strip()]
        except KeyError: pass
        print 'Unexpected table -', caption, '- ignoring'
        return junk
    del Info, Transact, Stub

del HTMLParser, Table
def digest(filename, parser=NordeaParser()):
    return parser.process(filename)

del NordeaParser

