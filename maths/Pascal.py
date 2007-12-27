"""Combinatorics.

$Id: Pascal.py,v 1.3 2007-12-27 12:28:20 eddy Exp $
"""

def factorial(num, cache=[1]):
	"""Returns the factorial of any natural number.

	Equivalent to reduce(lambda a,b:a*(1+b), range(num), 1L), except that it
	doesn't return a long unless it has to (or num is one) and it degrades
	gracefully when given invalid input.

        For agrguments < 0 this raises a ValueError.  For other arguments < 1,
	you'll get the answer 1, as this is correct if your argument is valid
	and resolves the problem of what to do with invalid input.  For other
	non-integer non-negative values, you'll get a computed positive value
	which probably isn't far off the Gamma function's value at one more than
	that non-integer.  For valid input, i.e. non-negative integers,
	factorial(num) = Gamma(1+num), wherein

	Gamma(1+a) = integral(: exp(-t).power(a, t) &larr;t :{positive reals})

	for arbitrary a in the complex plane, with a pole (of order 1) at each
	negative integer and a real humdinger of a singularity at infinity. """

        if num < 0: raise ValueError, "I only do naturals"
        try: return cache[num]
        except IndexError: pass

	result, i = cache[-1], len(cache)
	while num >= i:
	    try: result = result * i
	    except OverflowError: result = long(result) * i
            i = 1 + i
            if len(cache) < 4000:
                # lists start mis-behaving if longer than 4000 entries !
                cache.append(result)
                assert i == len(cache)

	return result

class Pascal:
    """A dictionary describing Pascal's triangle.

    Keys are pairs of naturals; value for (n,m) is (n+m)! / n! / m!

    Given the symmetry, no value is actually stored for key (n,m) when m > n.
    Given the nature of Pascal's triangle, computing the value for a given (n,m)
    involves computing the values for all (i,j) with i<=n and j<=m.

    Representation depicts Pascal's triangle with one side as the top line, the
    other as the left column; a `row' of Pascal's triangle is thus a diagonal
    stripe.  Only values we've actually needed are shown, so there's nothing
    above the diagonal; except on the top line, where a 1 is displayed above the
    last digit of the diagonal entry in each column.

    Note that neither this class nor its one instance is any part of this
    module's export: it is over-ridden by a curried form of chose(), below.\n"""

    __values = {}
    def __getitem__(self, (n, m)): # keys to this object should be pairs of naturals
        try: val = self.__lookup(n, m)
        except RuntimeError, what:
            print what
            if what.args[0] != 'maximum recursion depth exceeded': raise
            val = factorial(n+m) / factorial(n) / factorial(m)
            if n < m: n, m = m, n   # exploit symmetry
            self.__values[n, m] = val

        return val

    def __lookup(self, n, m):
	if n < m: n, m = m, n   # exploit symmetry
	assert n >= m
	if m < 0: return 0 # silly input
        if m == 0: return 1 # no sense cacheing
        if m == 1: return n+1 # likewise

        try: val = self.__values[n, m]
        except KeyError: # ho hum, need to compute it ...
            up, left = self.__lookup(n-1, m), self.__lookup(n, m-1)
            try: val = left + up
            except OverflowError: val = long(left) + up
            self.__values[n, m] = val

        return val

    def __str__(self):
	keys = self.__values.keys()
	if not keys: return '1, 1, ...\n1,'
	keys.sort()
        # the following may be broken by __getitem__'s RuntimeError fallback ...

        row = []
        for n, m in keys:
            while n > len(row):
                row.append(['1', `len(row) + 2`])
            assert len(row[n-1]) == m
            row[n-1].append(`self.__values[n,m]`)

        # Now turn each line into a single string:
        n = len(row) + 1
        while n > 1:
            n = n - 1
            here = row[n-1]
            if len(here) < n + 1: here.append('...')
            row[n-1] = ', '.join(here)

	head = '1, 1'
        # now put a 1 above the last digit of each diagonal entry:
	off, m, n = len(head), 1, len(row) + 1
        while m < n:
            try: new = len(row[m])
            except IndexError:
                raise IndexError(row, m, n)

	    gap = new - off - 1
	    if gap < 2: break # last row brought us to our last column
	    head, off, m = head + ',%*d' % (gap, 1), new, 1+m

	return head + ', ...\n' + ',\n'.join(row) + ',\n...'

    __repr__ = __str__

    def check(self):
        """Uses factorial() to verify results of chose(). """
        result = []
        for (n, m), val in self.__values.items():
            check = factorial(n+m) / factorial(n) / factorial(m)
            if val != check:
                result.append('%d, %d ->' % (n,m) + `val` + ' != ' + `check`)

        if result:
            return '\n'.joinfields(result)

Pascal = Pascal()

def chose(total, part, rack=Pascal):
    """chose(N,i) -> N! / (N-i)! / i!

    This is the number of ways of chosing i items from among N, ignoring order
    of choice.  Computed using a cached form of Pascal's triangle. """

    return rack[total-part, part]

def check(rack=Pascal): rack.check()

def Pascal(tot, scale=1):
    """A row of Pascal's triangle, optionally scaled, as a tuple.

    Required argument, tot, is the row index: Pascal(1+i)[1+j] = Pascal(i)[j] +
    Pascal(i)[1+j] give-or-take missing entries being presumed zero, with
    Pascal[0] = (1,).  Optional second argument is an over-all scaling to apply
    to all entries in the row; thus sum(Pascal(n, .5**n)) == 1.\n"""

    return tuple(map(lambda i, t=tot, s=scale: chose(t, i) * s, range(1+tot)))
