"""Doing the ORC sums.

Initial loans and start dates for interest:
 40000 June 2004
 50000 September 2004 (not more than a month after rickshaws arrived)
Interest: .005 / month
"""

from datetime import date, timedelta
from basEddy import Lazy

class Debt (Lazy):
    def __init__(self, amount, start, monthly=.005, yearly=None):
        """Construct a Debt object.

        Required arguments:
            amount -- size of debt (in NOK)
            start -- date from which it pays interest (of class date)

        Optional arguments:
            monthly -- monthly interest rate (default .5 %)
            yearly -- annual interest rate (default: None)

        If yearly is supplied, it takes precedence over monthly; a value of
        (1+yearly)**(1./12) -1 is used for monthly in this case.
        """

        if yearly is None:
            factor = 1 + monthly
        else:
            factor = (1 +yearly) ** (1./12)

        self.amount, self.start, self.factor = amount, start, factor

    def _lazy_get_current_(self, ignored):
        return int(self.__asat(self.start.today()))

    def __asat(self, when):
        then = self.start
        gap = (when.year -then.year) * 12 + when.month - then.month
        if when.day < then.day: gap = gap - 1 # latest month not complete
        if gap > 0: 
            return self.amount * self.factor ** gap
        # else interest not yet due
        return self.amount

    def __add__(self, other):
        if self.factor != other.factor:
            raise ValueError("Can't add Debts with different rates of interest")
        if self.start > other.start: self, other = other, self
        return Debt(self.__asat(other.start) + other.amount, other.start, self.factor - 1)

    def as_at(self, when):
        return int(self.__asat(when))

    def __log(self, value, hid=[]):
        try: ln = hid[0]
        except IndexError:
            from math import log
            ln = log
            hid.append(ln)

        return ln(value) / ln(self.factor)

    def when(self, amount, dpm = (365 + .97/4)/12., day = date, delta = timedelta):
        if amount < self.amount: return min(self.start, self.start.today())
        elif amount == self.amount: return self.start
        moons = self.__log(amount / self.amount)
        full = int(moons)
        yr, mn = divmod(full + self.start.month - 1, 12)
        return day(yr + self.start.year, mn + 1, self.start.day) + delta((moons - full) * dpm)

    def repay(self, amount, when):
        if when <= self.start:
            return Debt(self.amount - amount, self.start, self.factor -1)
        return Debt(self.__asat(when) - amount, when, self.factor -1)

Alix = Debt(40000, date(2004, 6, 1))
ORC = Debt(50000, date(2004, 9, 1))
Total = Alix + ORC
