# -*- coding: iso-8859-1 -*-
"""Descriptin of a Job in terms of time and money.

$Id: job.py,v 1.3 2007-03-24 16:41:09 eddy Exp $
"""
from value.lazy import Lazy

class Job (Lazy):
    # Should probably inherit dailyhours, public, weeklydays from
    # per-jurisdiction base object.
    def __init__(self, pay, period, leave, dailyhours=8, public=8, weeklydays=5):
	"""Sets up job-related data.

	Required arguments:
	  pay -- amount of money you get paid
	  period -- time period between payments
	  leave -- with how many days you get to chose to take off per year
	  
	Optional arguments:
	  dailyhours=7.5 -- hours worked per day (must be < 24)
	  public=8 -- number of public holidays per year
	  weeklydays=5 -- number of working days per week

	Only pay and period should have units; the rest are numbers.  The job
	allows public+leave holidays per year, plus 7-weeklydays per week.

	For pay and period, you can give an annual salary and year even if you
	get paid monthly; or an hourly rate and `hour'.  If period is less than
	a day, pay is presumed to be the actual pay per that much time;
	otherwise, it is presumed to be how much you are paid per that long when
	working the hours you would `normally' work during that period. """

	assert dailyhours * hour <= day
	assert weeklydays * day <= week
	assert leave + public <= year * weeklydays / week # i.e. yearlydays

	self.__pay, self.__period = pay, period
	# The remainder are numbers and may be irritatingly integer:
	self.__daily, self.__weekly = dailyhours, weeklydays
	self.__hols = public + leave
	self.leave = leave

    # time spent working per ...:
    def _lazy_get_day_(self, ignored): return self.__daily * hour
    def _lazy_get_week_(self, ignored): return self.__weekly * self.day
    def _lazy_get_year_(self, ignored):
	return self.day * (year * self.__weekly / week - self.__hols)
    def _lazy_get_month_(self, ignored): return self.year / 12
    def _lazy_get_quarter_(self, ignored): return self.year / 4

    # Rates of pay

    # relative to actual time spent working:
    def _lazy_get_rate_(self, ignored):
	time = self.__period
	rate = self.__pay / time # naive estimate
	if time < day: return rate
	# otherwise, make allowance for time off ...
	if time < week: return rate * day / self.day # eat, drink, sleep
	if 4 * time < year: return rate * week / self.week # week-end
	# more than a season; assume inputs took account of holidays
	return rate * year / self.year

    # amount of pay received per ...
    def _lazy_get_hourly_(self, ignored): return self.rate * hour
    def _lazy_get_daily_(self,  ignored): return self.rate * self.day
    def _lazy_get_weekly_(self, ignored): return self.rate * self.week
    def _lazy_get_annual_(self, ignored): return self.rate * self.year
    def _lazy_get_monthly_(self,   ignored): return self.annual / 12
    def _lazy_get_quarterly_(self, ignored): return self.annual / 4
