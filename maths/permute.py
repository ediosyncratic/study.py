"""Permutations and related stuff.

Provides:

   chose(N, i) -- number of ways of chosing i things from among N of them.
   factorial(n) -- number of permutations of n things, n!.
   unfixed(n) -- number of fixed-point-free permutations of n things; tends to n!/e.

   permute(seq, ind, ..., dex) -- composes permutations and applies them to seq.
   compose(ind, ..., dex) -- composes permutations (tolerates abbreviation).

   cycle(seq, [by=1]) -- shunts sequence cyclicly rightwards.
   order(seq, [cmp=cmp]) -- returns permutation s for which permute(seq,s) is sorted.
   invert(perm) -- inverts a permutation.
   sorted(seq), sort(seq) -- deploy some of the above.

   Iterator(n) -- iterator over all n! permutations of a list of length n.

See individual doc strings for further details of these; see module attribute
Theory for fuller discussion of permutations. """


Theory = ( """
Permutations
============

In what follows, I'll (orthodoxly treat any natural number as synonymous with
the collection of smaller ones, effectively n = range(n), and) write r@p for
permute(row, p);
    r@p = (: r[p[i]] <- i :len(p))

Theorem (associativity):

    When sequences r, q, p allow formation of the composites (r@q)@p and
    r@(q@p), the two are equal.

    Proof:
        assert len(r@(q@p)) is len(q@p) is len(p) is len((r@q)@p)
        for i in len(p):
            assert ((r@q)@p)[i] is (r@q)[p[i]]
                                is r[q[p[i]]]
                                is r[(q@p)[i]]
                                is (r@(q@p))[i]

        Each `is' just instanciates the definition of @ - QED.

This simple algebraic truth allows us to skip the brackets and just write r@p@q
and likewise for longer sequences of sequences, in so far as these allow
formation of the composite.  Thus permute(row, ind, ..., dex) is
row@ind@...@dex.  Note, however, that r@q might not be allowed even when r@(q@p)
is: e.g. when r's length appears as an entry in q, but its position in q doesn't
appear in p; if all other entries in q are valid indices into r, q@p's entries
will all be valid indices into r.

Definition:
    A sequence, p, is described as a permutation if it contains all the entries
    of range(len(p)).

Note that, given its length, this leaves it with no room to repeat any of its
entries.  If you ever want to discuss infinite permutations, you'll need to
define them explicitly as one-to-one mappings; but for finite permutations, the
`pidgeon-hole principle' makes the above sum up `one-to-one' neatly.
""" +

"""
Inverses
========

Given a sequence, row, and a permutation, p, of the same length, row@p is
described as `a permutation of' row: it contains the same entries as row but in
a different order; any repeated entry in row is repeated just as often in a
permutation of row; permutations of row have the same length as row.  A
permutation of a permutation is thus a permutation with the same length.

From the definition of permute, the identity permutation of any given length is
trivially a right identity: r@id == r for any sequence, r, of the given length.
Likewise, for any sequence, p, of entries in id, e.g. any permutation of id,
id@p == p; thus id is a left identity on permutations of id.  Thus, as a binary
operator on permutations of some given length, @ has, as (both left and right)
identity, the range of the given length.

Using q = order(p), p@q is a sorted permutation of p; so, when p is a
permutation, p@q is sorted and has the same entries as p, which has the same
entries as id, which is sorted; so p@q == id, making q a right-inverse for p.

Theorem:

    If @ is an associative binary operator closed on some collection, P -
    i.e. for any a, b in P we have a@b in P - having an identity, id, in P -
    i.e. for any a in P, id@a = a = a@id - and (having) right-inverses for all
    elements of P, then these right-inverses also serve as left inverses.

    Proof:
        Given p in P, consider its right-inverse, q, and q's right-inverse, d:
        so p@q = id = q@d, whence p = p@id = p@q@d = id@d = d, so q@p = id.
        QED.

Our composition, @, meets the preconditions of this, with P the collection of
permutations with the same length as p, hence order() would, on permutations,
serve as invert().  There is, however, a cheaper answer: see invert().
""" +

"""
Iteration
=========

To iterate over permutations it suffices to specify a well-ordering of
permutations - i.e. a choice of one of them as `first', combined with a `next'
opertion which will, starting from the first, iterate over all candidates.  An
obvious well-ordering to use is the one obtained from `lexicographic order', in
which one compares two sequences by finding the first entry in which they differ
and comparing those.

Among permutations of any given length, this makes the identity `less than' all
others, so it's our `first' permutation.  Note that a reverse-sorted (big values
before little ones) sequence is later than any other sequence with the same
entries.  To find the `next' permutation, in the lexicographic order, one must
change as late a chunk of the permutation as possible.  To this end, find the
longest reverse-sorted tail of the permutation: no shuffling of only that can
yield a later permutation, so our next permutation must bring in at least the
previous entry; since the previous entry (by construction) is less than the
first entry in the reverse-sorted tail, shuffling it into the tail can produce a
later entry.  A little thought will then reveal that we should swap it with the
smallest entry in the tail bigger than it, then reverse (i.e. forward-sort) the
thus-amended tail.  This is the .step() method used by the Iterator() class.
""" )


class _biNomial:
    """A dictionary describing Pascal's triangle.

    Keys are pairs of naturals; value for (n,m) is (n+m)! / n! / m!

    Given the symmetry, no value is acually stored for key (n,m) when m > n.
    Given the nature of Pascal's triangle, computing the value for a given (n,m)
    involves computing the values for all (i,j) with i<=n and j<=m.

    Representation depicts Pascal's triangle with one side as the top line, the
    other as the left column; a `row' of Pascal's triangle is thus a diagonal
    stripe.  Only values we've actually needed are shown, so there's nothing
    above the diagonal; except on the top line, where a 1 is displayed above the
    last digit of the diagonal entry in each column. """

    _values = {}
    def __getitem__(self, key):
	n, m = key	# keys to this object should be pairs of naturals
	if n < m: n, m = m, n
	# assert n >= m

	if m < 0: return 0 # silly input
	try: return self._values[n, m] # cached answer
	except KeyError: pass # ho hum, need to compute it ...

        if m > 0:
            up, left = self[n-1, m], self[n, m-1]
	    try: val = left + up
	    except OverflowError: val = long(left) + up
	else: val = 1
        self._values[n, m] = val

	return val

    def __str__(self):
	keys = self._values.keys()
	if not keys: return '1, 1, ...\n1,'
	keys.sort()
	import string

	line, row = [], []
	# presume knowledge of the order of the keys ...
	for n, m in keys:
	    # assert: n >= m
	    value = self._values[n,m]
	    if m == 0 and line:
		row.append(string.joinfields(line, ', '))
		line = []
	    line.append(`value`)
	# assert: afterwards, n has the biggest value it's had

	if line:
	    row.append(string.joinfields(line, ', '))

	head = '1, 1'
        # now put a 1 above the last digit of each diagonal entry:
	off, m = len(head), 1
        while m < n:
	    new = len(row[m])

	    gap = new - off - 1
	    if gap < 2: break # last row brought us to our last column

	    fmt = ',%' + '%dd' % gap
	    head, off, m = head + fmt % 1, new, 1+m

	return head + ', ...\n' + string.joinfields(row, ',\n') + ', ...'

    __repr__ = __str__

_binomials = _biNomial()
def chose(total, part):
    """chose(N,i) -> N! / (N-1)! / i!

    This is the number of ways of chosing i items from among N, ignoring order
    of choice.  Computed using a cached form of Pascal's triangle. """

    return _binomials[total-part, part]

def Pascal(tot):
    """Returns a row of Pascal's triangle"""
    row = []
    for it in range(1+tot): row.append(_binomials[tot-it, it])
    return tuple(row)

def factorial(num):
	"""Returns the factorial of any natural number.

	Equivalent to reduce(lambda a,b:a*(1+b), range(num), 1L), except that it
	doesn't return a long unless it has to (or num is one) and it degrades
	gracefully when given invalid input.

	If you give it an argument which is < 1, you'll get the answer 1, as
	this is correct if your argument is valid and resolves the problem of
	what to do with invalid input.  For other non-integer values, you'll get
	a computed positive value which probably isn't far off the Gamma
	function's value at one more than that non-integer.  For valid input,
	i.e. non-negative integers, factorial(num) = Gamma(1+num), wherein

	Gamma(a) = integral(positive reals| t-&gt; exp(-t). power[a-1](t) :)

	for arbitrary a in the complex plane, with a pole (of order 1) at each
	negative integer and a real humdinger of a singularity at infinity. """

	result = 1
	while num > 1:
	    try: result = result * num
	    except OverflowError: result = long(result) * num
	    num = num - 1

	return result

def _check():
    """Uses factorial() to verify results of chose(). """
    result = []
    for n, m in _binomials._values.keys():
	check = factorial(n+m) / factorial(n) / factorial(m)
	if _binomials[n,m] != check:
	    result.append('%d, %d ->' % (n,m) + `_binomials[n,m]` + ' != ' + `check`)

    if result:
        import string
        return string.joinfields(result, '\n')

def unfixed(num):
    """Returns the number of permutations of num without fixed points.

    Subtracting from num!, we get the sum over 0 < i <= num of: the number of
    ways of chosing i things to keep fixed, times the number of ways of
    permuting the remaining (num-i) items without any of these staying fixed.
    Indeed, num! = sum(: chose(num,i) * unfixed(num-i) <- i :1+num) as every
    permutation of num keeps i items fixed, for some i, while permuting the
    remaining i items without (further) fixed points.  This yields (as
    chose(num,i) = chose(num, num-i) and (1+num| num-i <- i |1+num) is a
    permutation)

	num! = sum(: chose(num, i) * unfixed(i) <- i :1+num)

    Whence, as 1 = chose(num, num)
	unfixed(num) = num! - sum(: chose(num,i) * unfixed(i) <- i :num)

    I find that unfixed(num) * e - factorial(num) gets rapidly close to 0.  We
    get a glitch in the sequence of values at 17 and 19, arising from rounding
    of the floating-point product, between the initial slide towards 0 and the
    subsequent exact zero.  Now, factorial(num) / e is
    sum(: pow(-1,i) * num! / i! <- i :natural) which is, in turn,
    sum(: pow(-1,i) * num! / i! <- i :1+num) plus something smaller than 1.

    So, consider any natural N for which, for each num in N,
	unfixed(num) = sum(: pow(-1,i)*num!/i! <- i :1+num).
    An example would be N = 0, since there is no num in 0 = {}.  In such a case,
    unfixed(N)

      = N! - sum(: chose(N,i) * sum(: pow(-1,j)*i!/j! <- j :1+i) <- i :1+N) # n=N-i+j
      = N! - sum(: sum(: N! / (n-j)! * pow(-1,j) / j! <- j :n) <- n-1 :1+N)
      = N! - sum(: N!/n! * sum(: chose(n,j)*pow(-1,j) <- j :n) <- n-1 :N)
      = N! + sum(: N!/n! * pow(-1,n) <- n-1 :N)
      = sum(: pow(-1,i) * N! / i! <- i :1+N)

    Thus unfixed has this form for num=N also, so that 1+N has the same property
    we demanded of N and, inductively, the equation holds for all num.
    Furthermore, we have unfixed(N) = N*unfixed(N-1) + pow(-1,N) which gives us
    a nice cheap way to evaluate unfixed; which clearly indicates that
    unfixed(N) must grow proportional to factorial in the long run (as witnessed
    in the clue which lead me here, exp(-1) being the constant of
    proportionality). """

    # Deal with boundary case
    if num < 0: return 0
    # Avoid computation when we can
    try: return _nd_perms[num]
    except IndexError: pass

    # can't just look it up: initialise for loop
    N, last = len(_nd_perms), _nd_perms[-1]
    if N % 2: sign = -1
    else: sign = 1

    # grow _nd_perms until it's long enough
    while num >= N:
	#    u(N) = N * u(N-1) + pow(-1,N)
	try: next = N * last + sign
	except OverflowError:
	    next = long(N) * last + sign

	_nd_perms.append(next) # method invocation: doesn't require global declaration
	N, sign, last = N+1, -sign, next

    # we should now be happy.
    return _nd_perms[num]

_nd_perms = [1]	# need a value here to seed the iteration.
# The identity permutation on empty (i.e. 0) has 0 fixed points, so qualifies ...

# well, that was pretty much number theory.
# now for some permutations, per se.

def permute(*indices):
    """Returns row permuted by a sequence of indices.

    All arguments must be sequences.  The first is the row to be permuted; each
    subsequent argument will be used to permute the row; entries in each
    argument after the first are read as indices into the preceding argument.

    Returns sequence with
      permute(row, ind, ..., dex)[i] == row[ind[...dex[i]...]],

    which may be understood as the composite of the sequences, read as
    functions (see permute.py's Theory docstring). """

    try: indices, ans = indices[:-1], indices[-1]
    except IndexError: return indices # empty tuple; no sequences given !

    while indices:
        indices, last = indices[:-1], indices[-1]
        ans = map(lambda i, _r=last: _r[i], ans)

    return ans

def compose(*perms):
    """Composes arbitrarily many permutations.

    Presumes that all arguments are permutations and interprets each as
    coinciding with the identity beyond its length - e.g. (1, 2, 0) is
    implicitly (1, 2, 0, 3, 4, 5, 6, ...) - so as to resolve any differences in
    length.  Otherwise, functionally equivalent to permute, though implemented
    with the two loops rolled out the other way (to implement the
    identity-default behaviour by catching IndexErrors). """

    try: n = max(map(len, perms))
    except ValueError: return perms # empty tuple: no permutations given
    row = list(perms)
    row.reverse()

    result = []
    for i in range(n):
        for p in row:
            try: i = p[i]
            except IndexError: pass
        result.append(i)

    return tuple(result)

def cycle(row, by=1):
    """Permutes a sequence cyclicly.

    Optional argument, by, has a default of 1: it is the position to which the
    first item in the sequence is to be moved; a portion of this length is moved
    from the end of the sequence to the start; all other entries in the list are
    moved down the list this far.

    For example: cycle((0, 1, 2, 3, 4), 2) yields (3, 4, 0, 1, 2). """

    by = -by % len(row)
    return row[by:] + row[:by]

def order(row, cmp=cmp):
    """Returns a permutation which will sort a sequence.

    The sequence permute(row, order(row)) is sorted, contains each entry of row
    exactly as often as it appears in row and has the same length as row.
    See invert(), below, for discussion of what this implies when row is a
    permutation.

    If row has the form r@q, then p = order(row) is a permutation with the same
    length as q and makes r@q@p a sorted list with this same length, making q@p
    a useful replacement for q: it contains the same entries as q, but r@(q@p)
    is sorted.  This is exploited in the recursive calls to order which model
    the qsort algorithm. """

    n = len(row)
    if n < 2: return range(n)
    pivot = row[0]
    low, mid, high = [], [ 0 ], []

    # Partition the values:
    while n > 1:
        n = n - 1
        sign = cmp(row[n], pivot)
        if sign > 0: high.append(n)
        elif sign < 0: low.append(n)
        else: mid.append(n)

    if len(low)  > 1: low  = permute(low,  order(permute(row, low),  cmp))
    if len(high) > 1: high = permute(high, order(permute(row, high), cmp))
    return low + mid + high

def invert(perm):
    """Inverts a permutation.

    Takes one argument, a permutation.  Returns a permutation which is its
    inverse: that is, if p is a permutation and q = invert(p), then p@q and q@p
    are the identity permutation, id = range(len(p)), of the same length.

    Asserting that our input was a permutation is most readilly checked by
    verifying that, when ordered, it yields the range it should. """

    n = len(perm)
    ans = [ None ] * n
    while n > 0:
        n = n - 1
        ans[perm[n]] = n

    if None in ans:
        raise ValueError, ('sequence is not a permutation', perm)
    assert permute(ans, perm) == range(len(perm)) == permute(perm, ans)
    assert ans == order(perm)
    return tuple(ans)

def sorted(row):
    return permute(row, order(row))

def sort(row):
    row[:] = sorted(row)

class Iterator:
    # for a rather elegant application, see queens.py's derived class
    """Iterator over permutations.

    Cycles through all the permutations of [0,...,n-1], a.k.a. range(n), for
    some n; does so in lexicographic order.  Attribute .live will be true until
    all permutations have been stepped over.

    Methods:

	.step() advances the iterator, setting .live when appropriate.

	.restart() resets the iterator to its initial condition

	.permute(sequence) returns the result of applying the iterator's current
	permutation to the given sequence.

    Illustrative usage::

        it = permute.Iterator(len(word))
        while it.live:
            anagram = ''.join(it.permute(word))
            if dictionary.has_key(anagram): print anagram
            it.step()

    The current permutation and its inverse are available, as tuples, under any
    name that's a non-empty initial chunk of 'permutation' or 'inverse', as
    appropriate; e.g. .p, .perm, .permutation, .i, .inv and .inverse all work.
    [Note that `permute' is *not* an initial chunk of `permutation'.]  These are
    re-computed every time they are asked for: if you intend to use either,
    particularly .inverse, several times in an iteration, you should take a copy
    of its value to save re-computation. """

    def __init__(self, size):
        """Initialise permutation iterator.

        Single argument is the size of the permutation, i.e. the length of the
        sequences it'll permute. """

        if size < 0:
            self.live = None
	    self.step = self.restart = lambda : None # no-op
        else:
            self.__row = range(size)
            self.live = 1

    def __getattr__(self, key):
	if key and self.live:
	    if key == 'permutation'[:len(key)]: return tuple(self.__row)
	    if key == 'inverse'[:len(key)]: return invert(self.__row)

        raise AttributeError, key

    def permute(self, seq):
        """Apply current permutation to a sequence."""
	if not self.live: raise AttributeError('permutation', 'exhausted iterator')
        return permute(seq, self.__row)

    def restart(self):
	try: del self.step
	except AttributeError: pass
	self.__row = range(len(self.__row))
	self.live = 1

    def step(self):
        """Advances iterator.

        Sets live to a false value if iterator was, before the call to step(),
        on the last permutation. """

        i = len(self.__row) -1
        while i > 0 and self.__row[i-1] > self.__row[i]: i = i-1

        if i < 1:
            # row is entirely in decreasing order: that's our last permutation.
            self.live = None
	    self.step = lambda : None # no-op
            return

        i, j = i-1, len(self.__row) -1
        # row[i+1:] is in decreasing order but row[i] < row[i+1]
        # Find smallest row[j] > row[i] with j > i:
        while self.__row[j] < self.__row[i]: j = j-1
        # swap i <-> j:
        self.__row[j], self.__row[i] = self.__row[i], self.__row[j]

        # row[i+1:] is still in decreasing order: reverse it
        i, j = 1+i, len(self.__row) -1
        while i < j:
            self.__row[j], self.__row[i] = self.__row[i], self.__row[j]
            i, j = 1+i, j-1

_rcs_log = """
  $Log: permute.py,v $
  Revision 1.2  2002-03-13 02:13:40  eddy
  Added Pascal, frobbed docs.  Corrected handling of Iterator(0), made
  .inverse an attribute like .permutation, xrefed new queens.py, added
  .restart() and shuffled method order.

  Initial Revision 1.1  2001/05/07 12:29:47  eddy
"""
