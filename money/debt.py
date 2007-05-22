"""The joys of compound interest ...

$Id: debt.py,v 1.5 2007-05-22 20:31:04 eddy Exp $
"""
from datetime import date, timedelta
from study.value.lazy import Lazy

class Debt (Lazy):
    def __init__(self, amount, currency, start=date.today(),
                 monthly=None, yearly=.045 * .72,
                 daily=False):
        """Construct a Debt object.

        Required arguments:
            amount -- size of debt
            currency -- name of unit in which amount is measured

        Optional arguments:
            start -- date from which it pays interest (of class date; default today)
            monthly -- monthly interest rate (default: None)
            yearly -- annual interest rate (default: 4.5% * .72; 28% tax rebate)
            daily -- true if interest is calculated daily; false (default) for
                     monthly calculation.

        If monthly is supplied, it takes precedence over yearly; otherwise,
        (1+yearly)**(1./12) -1 is used for monthly.\n"""

        if monthly is not None:
            factor = 1 + monthly
        else:
            factor = (1 +yearly) ** (.5/6)

        self.amount, self.currency = amount, currency
        self.start, self.factor = start, factor
        self.__diem = daily

    def _lazy_get_current_(self, ignored):
        return int(self.__asat(self.start.today()))

    def __add__(self, other):
        if self.currency != other.currency:
            raise ValueError("Can't add Debts in different currencies",
                             self.currency, other.currency)
        if self.factor != other.factor:
            raise ValueError("Can't add Debts with different rates of interest",
                             self.factor, other.factor)
        if self.start > other.start: self, other = other, self
        return Debt(self.__asat(other.start) + other.amount,
                    self.currency, other.start, self.factor - 1)

    def as_at(self, when):
        """Return amount of debt at some specified date.

        Sole argument, when, is a date(year, month, day) object.
        """
        return int(self.__asat(when))

    from math import log
    def __log(self, value, ln=log):
        return ln(value) / ln(self.factor)
    del log

    dayspermonth = (365 + .97/4)/12 # Gregorian calendar's average month

    def when(self, amount, dpm = dayspermonth, day = date, delta = timedelta):
        if amount < self.amount: return min(self.start, self.start.today())
        elif amount == self.amount: return self.start
        moons = self.__log(amount / self.amount)
        if self.__diem:
            off = delta(moons * dpm)
            base = self.start
        else:
            full = int(moons)
            yr, mn = divmod(full + self.start.month - 1, 12)
            base = day(yr + self.start.year, mn + 1, self.start.day)
            off = delta((moons - full) * dpm)
        return base + off

    def __asat(self, when, dpm = dayspermonth):
        then = self.start
        if self.__diem:
            gap = (when - then).days
            if gap > 0:
                return self.amount * self.factor ** (gap / dpm)
        else:
            gap = (when.year -then.year) * 12 + when.month - then.month
            if when.day < then.day: gap -= 1 # latest month not complete
            if gap > 0: 
                return self.amount * self.factor ** gap
        # else interest not yet due
        return self.amount

    del dayspermonth

    def repay(self, amount, when):
        """Return new Debt object resulting from a repayment.

        Required arguments:
           amount -- how much repayed (number, in same currency as self.amount)
           when -- date(y, m, d) object describing date of payment
        """
        if when <= self.start:
            left, when = self.amount - amount, self.start
        else:
            left = self.__asat(when) - amount

        return Debt(left, self.currency, when, self.factor -1, daily=self.__diem)

    def rerate(self, rate, when, monthly=False):
        """Return new Debt object resulting from a change of interest rate.

        Required arguments:
          rate -- new interest rate
          when -- date(y, m, d) object describing date of change
        Optional argument:
          monthly -- true if rate is monthly, else (default) it's assumed to be yearly.
        """
        if not monthly:
            rate = (1 +rate) ** (.5/6) -1

        if when <= self.start:
            left, when = self.amount, self.start
        else:
            left = self.__asat(when)

        return Debt(left, self.currency, when, rate, daily=self.__diem)

class Mortgage (Lazy):
    """Description of a mortgage.

    See http://www.chaos.org.uk/~eddy/math/mortgage.html for theory.  Real
    interest rate: 4.25%; and .28 of that gets refunded by government from
    taxes, so .72 * .0425 is the effective interest rate.
    """

    def __init__(self, debt, monthly=None, growth=.05, duration=5):
        """Initialize a Mortgage object.

        Required first argument, debt, is a Debt object describing the money
        owed.  Optional arguments:

          monthly -- payment (in debt's currency) per month (default: None).
          growth -- annual rate of growth of monthly payment (default: 5%).
          duration -- number of years over which the mortgage is to be repayed
                      (default: 5), ignored if monthly is suupplied.

        Each argument appears as an eponymous attribute; the unsupplied or
        ignored one is lazily computed from the supplied one.  Any monthly
        'administrative fee' should not be included in monthly.\n"""
        
        self.debt, self.rate = debt, (1+growth)**(.5/6)
        if monthly is None: self.duration = duration
        else: self.monthly = monthly

    def _lazy_get_duration_(self, ig, day=date):
        debt, pay, grow = self.debt, self.monthly, self.rate
        when = debt.start
        while debt.amount > 0:
            pay = pay * grow
            yr, mn = divmod(when.month +1, 12)
            if mn < 1: yr, mn = yr - 1, mn + 12
            when = day(when.year + yr, mn, when.day)
            debt = debt.repay(pay, when)

        return when.year - self.debt.start.year + (when.month - self.debt.start.month)*.5/6

    def _lazy_get_monthly_(self, ig):
        """Compute (approximate) required monthly payment."""
        moons, debt = int(self.duration * 12 + .5), self.debt
        if -1e-4 < self.rate - debt.factor < 1e-4: return debt.amount * self.rate / moons
        return debt.amount * (debt.factor -self.rate) / (1 -(self.rate/debt.factor) ** moons)

def affordable(monthly, duration, interest=.0425 * .72, inflate=.05):
    """How big a debt can one pay off in a given time with given available cash ?

    See Mortgage for theory:
    """

    f = (1 + interest) ** (.5/6)
    h = (1 + inflate) ** (.5/6)
    moons = int(duration * 12 + .5)
    if -1e-4 < f - h < 1e-4: return moons * monthly / h
    return monthly * (1 - (h/f)**moons) / (f - h)
