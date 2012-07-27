# -*- coding: iso-8859-1 -*-
"""Describing finance.

Fragments:
  debt -- description of debts and mortgages
  job -- description of a job
"""
from study.value.quantity import Quantity

quid = Quantity.base_unit(
    '£', 'Pound Sterling', """The base unit of British currency.

Used to be 20 shillings (21 shillings made a Guinea); each shilling was 12
pence, each penny was four farthings.  A florin was two shillings; a crown was
five.  Apparently a pound was also called a sovereign.  HTML supports character
entity &sterling; for the Pound Sterling.

To properly handle money within a system of units, I need support for variation
in time and space (conversion factors between different currencies vary with
time; and you'll get different exchange rates from different trading partners).
Then again, conversion factors between systems of units also show similar
variation - contrast the different nations' archaic units of length, and notice
how units of volume got re-defined by assorted legislative acts over the years.
""")

# It's clearly inadequate to treat money units as approximate multiples of one
# another: each is an exact unit in its place, it's only the conversion between
# them that's approximate.
krone = quid / Quantity.within(10, 2)

del Quantity

from debt import Debt, Mortgage
from job import Job
