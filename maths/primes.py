"""Cached prime numbers.

This module provides an object, primes, which masquerades as the infinite list
of all primes, in their proper order.  If you ask it how long it is, it'll be
finite, but if you ask for an element past the `end' you'll still get it (though
you may have to wait).  Ask for its last element and you'll get some prime: ask
whether some bigger prime is in the list and the answer will be yes, none the
less (provided you use primes.has_value(number) rather than the raw `in' test).

Building on that, this module also provides facilities for factorisation and
multiplying back together again:

  prodict(dict) -- product of pow(key, dict[key])

  factorise(numb) -- returns a dictionary, which prodict will map to numb: its
  keys are irreducible (so generally primes, but -1, at least, may also appear).

See also generic integer manipulators in natural.py, combinatorial tools in
permute.py and polynomials in polynomial.py: some day, it'd be fun to do some
stuff with prime polynomials ...

$Id: primes.py,v 1.1 2005-01-17 22:36:59 eddy Exp $
"""

checking = None
error = __name__ + ' error'
_error = 'internal ' + error

_aside_plus = """When is pow(2,n)+1 a prime ?

when n is in (1,2,3,4,8,16) and then no more, at least as far as 42.
Note that pow(2,32)+1 is 641*6700417
"""

_aside_minus = """When is pow(2,n)-1 a prime ?

Never with n even; and n=1 gives 1.  When n is in (3,5,7,13,17,19,31) and
then no more, at least as far as 47.  Observe 31=pow(2,5)-1, 17=pow(2,4)+1,
7=pow(2,3)-1, 5=pow(2,2)+1, with pow(2,1)-1=1.

So when is pow(2, pow(2,i)+pow(-1,i))-1 a prime ?  For i=1 we get 1 which
we ignore, then for i in (2,3,4,5) we get primes.  For i=6 I get a segfault.
"""

# two tools: add one, coping with overflow, and ordered insertion.
def add_one(num):
    try: return num + 1
    except OverflowError: return num + 1L

if None:
    def order_insert(row, val):
	"""Inserts val into sorted row, with val not previously in row.

	This is a straightforward implementation.  Works fine for short rows ;^)
	"""
	at = len(row) - 1
	while at > 0 and row[at] > val: at = at - 1
	row[at:at] = [ val ]

else:
    def order_insert(row, val):
	"""Inserts val into sorted row, with val not previously in row.

	This implementation uses binary chop.  For long rows I expect it to work
        faster than the straightforward implementation above.
	"""

	if not row or row[0] > val: row[:0] = [ val ]
	elif row[-1] < val: row.append(val)
	else:
	    assert val not in row
	    # so row[0] < val < row[-1], and len(row) > 1
	    bot, top = 0, len(row) - 1	# so bot < top
	    while bot + 1 < top:
		at = (bot + top) / 2	# midpoint, erring low
		assert bot < at < top
		assert row[bot] < row[at] < row[top]

		if row[at] < val: bot = at
		else: top = at

		assert row[bot] < val < row[top], 'binary chop missed'

	    assert bot + 1 == top, 'binary chop mis-terminating'
	    # so insert val after bot, before top - ie, at position top
	    row.insert(top, val)

from basEddy.item.sequence import varySeq
class lazyTuple(varySeq):
    """Evaluation on demand of a (possibly infinite) tuple.

    Some of what's in this class should be split out into _Prime, below.
    """
    def __init__(self, row=None):
	"""Initialises a lazy Tuple.

	Optional argument, row, should be a sorted list.
	"""
	varySeq.__init__(self, row)

	try: top = row[-1]
	except self._entry_error_: top = 0
	self._ask = add_one(top)

    def __getitem__(self, key):
	try: return varySeq.__getitem__(self, key)
	except self._entry_error_: pass
	while self.grow() and key > len(self._item_carrier): pass
	return varySeq.__getitem__(self, key)	# raising error if grow failed

    def has_value(self, val):
	"""Is val in self ?

	Override this in base-classes.
	"""
	while val >= self._ask and self.grow(): pass
	return varySeq.has_value(self, val)

    def grow(self):
	"""Extend self._item_carrier, return true on success.

	Over-ride this method in derived classes - don't call this one.
	It implements lazy range(infinity).
	"""
	result = self._ask
	self._item_carrier.append(result)
	self._ask = add_one(result)
	return result

class _Prime(lazyTuple):
    """List of all primes, generated as needed.

    Needs to use a paged-in-and-out item carrier.  Use that to do cacheing.
    """

    _private_doc_ = """

    From Tuple, this inherits ._item_carrier in which we store all the primes
    less than ._ask, which is the next number we're going to check to see if
    it's prime.  Any higher primes we've discovered (usually thanks to
    factorise(), see below) are kept in _sparse (in their correct order, albeit
    probably with gaps).  All currently known primes may be listed as .known().

    self._ask will be an ordinary integer as long as it can, then switch over to
    being a long one.  Entries in .known() will be ordinary integers most of the
    time, unless they are so big they must be long().  However, some entries
    which could be ordinary may still be long().

    """

    # always initialise this with at least the first entry, 2, in the list.
    def __init__(self, row=None, sparse=None):
	# implementation of .grow() requires row initially non-empty.
	if row is None: row = [ 2 ]
	elif not row: row.append(2)	# complain if it's not a list !
	lazyTuple.__init__(self, row)

	self._sqrt = 2	# Every prime less than _sqrt has square less than _ask.

	if sparse is None: self._sparse = []
	else: self._sparse = sparse

    def known(self):
	return self._item_carrier + self._sparse

    def ask(self):
	"""Returns a number about which self would like to be asked. """
	return self._ask

    def has_value(self, num):
	if num in self._item_carrier or num in self._sparse: return num
	if num < self._ask: return None
	seen = 0

	while 1:
	    for p in self._item_carrier[seen:]:
		if num % p == 0: return None
		# and p < _ask <= num
		seen = len(self._item_carrier)

	    p = self._item_carrier[-1]
	    if long(p) * p > num: return self._know(num)

	    for p in self._sparse:
		if num % p == 0: return None
		if p > num: break

	    if self.get_cache():
		if num in self._item_carrier[seen:] or num in self._sparse: return num
		if num < self._ask: return None

	    else: break

	while 1:
	    p = self.grow()
	    if num % p == 0: return None
	    if long(p) * p > num: return self._know(num)

    def grow(self):
	if self._ask < self[-1]:
	    self._ask = add_one(self[-1])
	was = self._ask

	while self[-1] < was:	# so we exit when we know.
	    if self._ask in self._sparse: self._know()
	    else:
		for p in self._item_carrier:
		    if self._ask % p == 0:
			self.__throw()
			break

		    if p < self._sqrt: continue	# skip the square root check

		    if long(p) * p > self._ask:
			# no prime less than sqrt(_ask) divides _ask
			self._know()
			# because: if no prime less than sqrt(_ask) divides it,
			# _ask is prime.
			break
		    else: self._sqrt = p + 1

	return self._item_carrier[-1]	# the new prime

    def __throw(self, other=None):
	"""Records a non-prime.

	Argument, other, defaults to self._ask: it must not be a prime.  The
	only time it matters is when it's self._ask: which is then stepped
	forward to the next integer. """
	if other in ( None, self._ask ):
	    self._ask = add_one(self._ask)

    def _know(self, other=None):
	"""Records a prime.

	Argument, other, defaults to self._ask: it must be a prime.  If it is
	self._ask, we append it and advance _ask.  Otherwise, if it isn't
	already in self or _sparse, we add it to _sparse in its proper place.
	"""

	if not other or other == self._ask:
	    # add _ask to self and advance
	    self._item_carrier.append(self._ask)
	    if self._ask in self._sparse:
		assert self._ask == self._sparse[0], \
		       'found primes array disordered at %d' % self._ask
		self._sparse = self._sparse[1:]

	    other = self._ask	# we're going to return this
	    self._ask = add_one(other)

	elif not (other in self._item_carrier or other in self._sparse):
	    if self._ask > other:
		raise _error, 'missed out a prime, %d, in the search' % other

	    order_insert(self._sparse, other)

	return other

    def factorise(self, num, gather=None):
	"""Factorises an integer.

	Argument, num, is an integer to be factorised.
	It may be long, but not real.

	Optional argument:

	    gather -- dictionary to which to add results, or None (the default)
	    to use a fresh empty dictionary: this is what factorise() returns.

	The result of factorise() is always a dictionary: its prodict() is num,
	its keys are primes, -1 or 0 and its values are positive integers.  The
	key -1 is present precisely if num is negative, in which case its
	multiplicity is 1 (not, for instance, 3 or 5).  The key 0 only appears
	when num is 0, in which case the result is {0: 1}.  Note that 1 =
	prodict({}) so I don't need to make a special case of 1 ;^)

	If num is a positive integer, factorise(num) is a dictionary whose keys
	are its prime factors: the value for each key is its multiplicity as a
	factor of num.  Thus, prodict(factorise(num)) is num.

	Zero is a special case (because it is a multiple of every number): its
	given factorisation is { 0: 1 }.  Negative values are factorised as the
	factorisation of the corresponding positive value, with one extra key,
	-1, with multiplicity 1.

	If you want to know the factorisation of the product of a sequence of
	integers, do it by using gather to collect up their several
	factorisations: it's much easier to factorise each of them in turn than
	to factorise their product !

	    out = {}
	    for num in sequence: factorise(num, out)

	See also: Factorise() (which is packaging) and prodict().
	"""

	# Can't use {} as gather's default, as we modify it !
	if gather is None: result = {}
	else:
            result = gather
            if result.get(0, 0): return result

	if num < 0:
	    result[-1] = (result.get(-1, 0) + 1) % 2
	    num = -num

	# Only accept integers !
	if num / (1L + num): raise TypeError, ('Trying to factorise a non-integer', num)

	if num == 0:
            result.clear()
            result[0] = 1
	elif num > 1:
	    # Keep track of ones we've already seen: in the cache loop,
	    # we don't want to repeat all the ones we've seen before.
	    seen = 0

	    # First, go through the known primes looking for factors:
	    while 1:
		for p in self.known()[seen:]:
		    count, num = self.__reduce(num, p)
		    if count: result[p] = count
		    # all primes less than p have already been tried.
		    if num < p: break	# assert: num == 1
		else:
		    seen = len(self)	# ignore sparse
		    if self.get_cache(): continue
		break

	    # Now go hunting for primes until the job is done:
	    while num > 1:
		p = self.grow()
		count, num = self.__reduce(num, p)
		if count: result[p] = count
		elif long(p) * p > num:
		    # num's a prime !
		    self._know(num)
		    result[num] = 1
		    # job done ;^)
		    num = 1

	return result

    def __reduce(self, n, p):
        """Returns c, m with pow(p,c) * m == n and m coprime with p."""
        c = 0	# p's multiplicity as a factor of n
        while n % p == 0:
            self.__throw(n)
            c, n = c + 1, n / p

        return c, n

    def get_cache(self):
	"""Hook-in point for cacheing of primes.

	If you can update self._item_carrier from a cache of some kind, do it
	now.  If you can't, return None, or leave it to this base method to do
	so for you.  On a successful update, return some true value: calling
	code can use this to decide whether to try again ...
	"""
	return None

def _tabulate_block(file, block):
    """Writes a sequence of numbers to a file tidily.

    Arguments:

      file -- a file handle, to which to .write() the tabulated numbers

      block -- a sequence of integers (or long integers)

    Begins each line with a space, separates numbers with comma and space,
    limits lines to 80 characters in width (thus only allowing one number per
    line once they are more than 38 characters each).  Ends in a stray comma,
    which python won't mind when it loads the array, but no newline.

    The tabulation algorithm assumes that each entry in block is bigger than all
    previous (specifically, it has at least as many digits), and that entries
    which don't need to be long aren't.  Breaking these assumptions will cause
    no harm beyond making the table messier. """

    across = 80	# Force an initial newline, etc.
    click = 9	# biggest number in current number of digits (initially 1)
    step = 3	# one each for comma and space, plus the current number of digits.

    for item in block:
	while item > click:
	    # item takes up more than step digits: increase step ...
	    try:
		click = 10 * click + 9
		step = step + 1
	    except OverflowError:
		click = 10L * click + 9
		step = step + 2		# extra one for the L !
	# That's set step correctly ...

	across = across + step
	if across > 80:
	    file.write('\n')
	    across = step

	file.write(' ' + `item` + ',')

from basEddy import Lazy
class cachePrimes(_Prime, Lazy):
    def __init__(self, row=None, sparse=None,
		 moduledir='/home/eddy/sys/py',
		 cachedir=None):
	_Prime.__init__(self, row, sparse)
	self._prime_module_dir = moduledir
	self._prime_cache_dir = cachedir
	self._step = 1024
	self._high_water = 0
	return

    def _lazy_get_prime_cache_dir_(self, ignored):
	if not self._prime_cache_dir:
	    import os
	    self._prime_cache_dir = os.path.join(self._prime_module_dir, 'primes')
	return self._prime_cache_dir

    def _do_import(self, handle):
	"""Imports data from a file, preparatory to _load()ing."""
	import sys, os
	name = 'temp_cache'
	infile = os.path.join(self._prime_module_dir, name + '.py')

	try: saved = sys.modules[name]
	except KeyError: saved = None
	else: del sys.modules[name]
	# So we've cleared sys.modules and must be sure to put it back.
	try:
	    os.link(handle, infile)
	    try:
		temp = {}
		found = None
		exec 'import ' + name in temp
		return temp[name]

	    finally:
		os.unlink(infile)
		os.unlink(infile + 'c')	# kill the .pyc file too !
	finally:
	    # Put sys.modules back the way we found it.
	    if saved: sys.modules[name] = saved
	    else:
		try: del sys.modules[name]
		except KeyError: pass

    def _load(self, found):
	"""Loads data that's been imported from a file.

	Argument, found, is the module object as which the file was imported: it
	should have integer attributes, found.at and found.to and list
	attributes found.sparse and found.block, with each member of sparse
	greater than any member of block, and at+len(block)==to.  The given
	block is used as self[at:to], while entries in sparse are added to
	_sparse in their right order.


	The bits of the file we'll examine are variables in the global scope
	with names 'at', 'to', 'sparse' and 'block'.  Most of this function
	checks things, the actual loading is quite brief ! """

	size = len(self._item_carrier)
	# Assume found contains the namespace we want to load: if not, convert KeyError.
	try:
	    # Test sanity of what we're loading:
	    assert found.to == len(found.block) + found.at, 'inconsistent load-file'

	    # Check get_cache()'s decision to call _load() was sound ...
	    assert found.at <= size, \
		'new slice, [%d:], does not meet up with what we have, [:%d]' % (
			found.at,					size)
	    assert size <= found.to, \
		   'mis-ordered loading of cache-files (%d > %d).' % (size, found.to)

	    # Expect things in found.sparse to be past the end of found.block,
	    # hence of _item_carrier: load them into _sparse (do it here to
	    # catch AttributeError if it happens).  If that expectation fails,
	    # we'll clear it out shortly.
	    for item in found.sparse:
		if item in self._item_carrier + self._sparse: continue
		if checking:
		    # only check it against what we knew before this _load() ...
		    for p in self._item_carrier + self._sparse:
			if item % p == 0: raise error, \
			'alleged prime, %d, has a factor, %d' % (item, p)
		# add item to _sparse
		self._know(item)

	except AttributeError, what:
	    raise error, 'cache-file lacks necessary variable %s: %s' % (
							str(what), handle)

	if checking:
	    test = self._item_carrier[found.at:]
	    if test != found.block[:len(test)]:
		raise error, "new block of primes doesn't match what I know already"

	# Now the actual loading !
	self._item_carrier[found.at:] = found.block
	self._ask = add_one(self._item_carrier[-1])

	# Tidy away anything in _sparse that now doesn't belong there:
	while self._sparse and self._sparse[0] in self._item_carrier:
	    self._sparse = self._sparse[1:]

	if self._sparse and self._sparse[0] < self._item_carrier[-1]:
	    raise error, 'sparse prime, %d, missed in "full" list' % self._sparse[0]

	return 1

    def _next_high(self):
	try:
	    while self._step < self._high_water / 8:
		self._step = self._step * 2
	except OverflowError: pass
	return self._high_water + self._step

    def persist(self, name='c', force=None):
	"""Records `most' of what the primes module knows in files for later reference.

	Arguments are all optional:

	  name -- name-stem for cache files. Default is 'c'.

	  force -- flag, default is false.  Normally, persist() trusts files
	  already in the cache and doesn't bother re-writing them: set force to
	  some true value to make all the files be written anew.

	Updates the cache.  Each cache file contains a sub-list of the known
	primes: small primes are recorded in files with 1024 entries, but
	subsequent sub-lists are longer (by a factor of 2 whenever the
	chunk-size gets bigger than an eighth of the number of primes in all
	earlier cache files).  Each cache-file's name expresses the range of
	indices it contains: this information is also present in the cache-file,
	along with the current value of _sparse.  The block of primes stored in
	the file is formatted to be readable on an 80-column display.  The data
	are stored in the file under the names

	  at -- index, in self, of first prime in block

	  to -- index, in self, of first prime after block

	  sparse -- current _sparse list

	  block -- self[at:to]
	"""

	import os
	name = os.path.join(self.prime_cache_dir,
			    name)	# implicitly tests isabs(name).
	tmpfile = name + '.tmp'

	new = self._next_high()
	size = len(self._item_carrier)
	while new < size:
	    outfile = name + `self._high_water` + '-' + `new` + '.py'
	    if force or not os.path.exists(outfile):
		file = open(tmpfile, 'w')
		try:
		    file.writelines([
		    '"""Persistence data for primes module."""',
		    '\nat = ', `self._high_water`,
		    '\nto = ', `new`,
		    '\nsparse = ', `_sparse`,
		    '\nblock = ['])
		    _tabulate_block(file,
				    self._item_carrier[self._high_water:new])
		    file.write('\n]\n')

		finally: file.close()

		os.rename(tmpfile, outfile)

	    self._high_water = new
	    new = self._next_high()

    def _lazy_get__caches_(self, ignored, stem='cache'):
	try:
	    import os
	    dir = self.prime_cache_dir
	    row = os.listdir(dir)
	except: return {}
	import string

	def _read_int(text):
	    import string
	    if text[-1] == 'L': return string.atol(text, 0)
	    else: return string.atoi(text, 0)

	result = {}
	lang = len(stem)
	for name in row:
	    if name[:lang] == stem and name[-3:] == '.py' and '-' in name[lang:-3]:
		result[os.path.join(dir, name)
		       ] = map(_read_int, string.split(name[lang:-3], '-'))

	return result

    def get_cache(self):
	"""Returns true if it got anything out of the caches."""
	size = len(self._item_carrier)
	for name, pair in self._caches.items():
	    if pair[0] <= size:
		del self._caches[name]
		if pair[-1] > size: return self._load(self._do_import(name))

	return None

primes = cachePrimes()
def is_prime(num): return primes.has_value(num)
def factorise(num): return primes.factorise(num)

def prodict(dict):
    """Returns the product of a factorisation dictionary.

    Argument, dict, is a dictionary whose keys are multipliable and whose values
    are powers to which one may sensibly raise these keys.  The result is the
    same as would result from

	answer = 1
	for key, val in dict.items():
	    answer = answer * pow(key, val)
	return answer

    This implementation uses reduce() and map().  This might be more efficient
    than doing it as above, but I don't know.  It's only really here to give me
    a handy way to do the inverse of factorise(), below.

    See also, in module basEddy.quantity: adddict, subdict and scaledict.  They
    and this may one day migrate elsewhere to be together.  We get:
	* prodict(adddict(a,b)) = prodict(a) * prodict(b),
	* prodict(subdict(a,b)) = prodict(a) / prodict(b),
	* prodict(scaledict(d,n)) = pow(prodict(d), n).
    """

    return reduce(lambda a,b: a*b, map(pow, dict.keys(), dict.values()), 1)

# Packaging.
from types import TupleType, ListType, FloatType, IntType, LongType
def Factorise(args=(), gather=None, cache=None):
	"""Factorises an integer or each integer in a tuple.

	Optional arguments:

	  args -- an integer, a tuple thereof or (if omitted) the empty tuple.

	  gather -- dictionary to which to add results, or None (the default) to
	  use a fresh empty dictionary: this is what Factorise() returns.

          cache -- flag, default is true.  By default, Factorise() will do a
	  primes.persist() when it is finished: this will slow you down when it
	  happens (writing a cache file can take a while once you know a lot of
	  primes).  Set cache to some false value to suppress this (and remember
	  to run primes.persist() at the end of your session).

	If args is a non-empty tuple, this returns a dictionary whose keys are
	the entries in the tuple, mapped to values which are what you get by
	passing the key to Factorise().  If args is the empty tuple, Factorise
	acts as if it'd been given the smallest natural number that the primes
	module doesn't yet know about.  If args is an integer, Factorise()
	behaves as factorise(), so prodict(Factorise(())) will be this `least
	unknown', but it'll be known by the time you've evaluated that.

	See also: prodict() and factorise().
	"""

	# Can't use {} as gather's default, as we modify it !
	if gather is None: gather = {}

	try:
	    if len(args) > 1: seq = args
	    else:
		seq = args[0]
		# Now raise the right exception if (empty or) not a sequence:
		seq[0], seq[:0]

	except (TypeError, KeyError, IndexError, AttributeError):
            # Numeric argument:
	    if args or args == 0: num = args
	    else: num = primes.ask()

	    if type(num) == FloatType:
		try: val = int(num)
		except OverflowError: val = long(num)
		if val != num: raise TypeError, 'Factorising a float !'
		num = val

	    result = primes.factorise(num, gather)

	else:
	    # We did get a sequence argument: traverse it.
	    result = gather
	    for num in seq:
                try: result[num]
                except KeyError:
		    result[num] = Factorise(num, cache=None)

	if cache: primes.persist()
	return result

class printFactor:
    """Pseudo-dictionary with which to make Factorise print answers.

    Use a printFactor as gather in Factorise: the results will be printed for
    you (you won't need to scan and print the dictionary yourself). """
    def __init__(self, dict=None):
	if dict is None: dict = {}
	self._dict = dict
    def __len__(self): return len(self._dict)
    def __getitem__(self, key): return self._dict[key]
    def __setitem__(self, key, val):
	self._dict[key] = val
	print key, '->', val
	return val
    def __delitem__(self, key): del self._dict[key]
    def __getattr__(self, key):
        return getattr(self._dict, key)

def factors(num):
    """Returns a dictionary whose keys are the factors of num.

    The value this dictionary associates with any factor is the factor's
    multiplicity as a factor of num, except for the key 1 (for which this is
    ill-defined) which gets the maximum of all the values in the dictionary.
    Thus, where defined, pow(k, factors(n)[k]) is a factor of n: for k not equal
    to 1, furthermore, pow(k, 1+factors(n)[k]) is not a factor of n.  Notice
    that factors(n)[n] = 1.

    If num is negative, -1 is listed with multiplicity 1, as is its product with
    each positive factor.  This is not wholly satisfactory, but it's not quite
    clear what's saner. """

    if num < 0:
	num = -num
	result = {-1: 1}
    else: result = {}
    fact = factorise(num)
    result[1] = max(fact.values())

    for key, val in fact.items():
	q = key
	items = result.items()	# before we enter the inner loop
	for i in range(val):
	    top = val / (1+i)	# q is pow(key, 1+i)
	    for k, v in items:
		try: r = k * q
		except OverflowError: r = long(k) * q
		result[r] = min(result[k], top)
	    try: q = q * key
	    except OverflowError: q = long(q) * key

    return result

_rcs_log = """
$Log: primes.py,v $
Revision 1.1  2005-01-17 22:36:59  eddy
Initial revision

"""
