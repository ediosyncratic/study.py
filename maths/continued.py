"""Continued fractions.

Exports:
  real_continued(val) -- iterator over continued fraction approximation to val
  rationalize(x [, tol, depth]) -- try to approximate x = n / d for whole n, d

See also:
http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
expounding the virtues of continued fractions.

$Id: continued.py,v 1.4 2009-03-10 21:39:10 eddy Exp $
"""

def real_continued(val):
    """Returns an iterator for the continued fraction for val.

    Single argument, val, is a real number - although it need not be represented
    by a python object of type real; it suffices that plays nicely with
    divmod(val,1), 1./val and behaves as a false value, when used as a boolean,
    exactly when it is 0.  Returned iterator yields a sequence n of integers,
    for which n[0] +1/(n[1] +1/(n[2] + ...)) is a progressively more refined
    approximation to val.  Aside from n[0], abs(n[i]) is always at least 2.

    The sequence terminates when the approximation is exact, if this ever
    arises.  Note, however, that (if val is of type real) the vagaries of
    floating-point arithmetic may prevent an exact match from being noticed, or
    permit an inexact match to appear exact.  Callers may want to treat any
    sufficiently huge yield as meaning the previous yield was the end of the
    sequence.

    A small uncertainty dx in x - p = 1/(q + y), with p, q integers having x-p
    and y between -1/2 and +1/2, implies an uncertainty in y of dy; with 1 = x.y
    -p.q -p.y +q.x we have 0 = x.dy +y.dx -p.dy +q.dx = dx.(y + q) -dy.(p - x)
    so dy = dx.(y+q)/(p-x) = dx.(y+q)**2.  Going forward, if we know an upper
    bound u on abs(dx), we can infer u.(q.q +abs(q) +1/4) as an upper bound on
    abs(dy); if this ever gets above 1/2 there is little point continuing the
    sequence.  Working backwards, e.g. to work out when we can truncate while
    ensuring some upper bound u on the error in x, the upper bound we must
    impose on the possible error in y is u.(q.q -abs(q) +1/4); if this ever gets
    as high as 1/2, we can truncate.\n"""

    if not val: yield 0
    while val:
	q, r = divmod(val, 1) # r > 0 even if val < 0
	if r > .5: q, r = q+1, r-1
	yield int(q)
	if r: val = 1. / r
	else: break

def rationalize(x, tol=1e-7, depth=5):
    """Find rational approximation to a real.

    Required parameter, x, is a real number; complex is not handled and there is
    no point doing this to integers.  Optional arguments are:
      tol -- error tolerance (default: 1e-7)
      depth -- depth of search (default: 5), see below.
    Returns a twople n, d of integers for which n = d * x, give or take errors
    of order tol, raising ValueError if unable to find suitable n and d.

    Result is sought by using continued fractions; that is, by first trying to
    approximate x as s[0] + 1./(s[1] + 1./(s[2] + ...)) for some sequence s of
    integers, then unwinding this expression to obtain n and d.  The search
    aborts if it needs more than depth entries in s to get within tol of x.

    For more on the theory of continued fractions, see
    http://en.wikipedia.org/wiki/Continued_fraction\n"""

    s = real_continued(x)
    seq = [ s.next() ]
    for q in s:
	# See real_continued's uncertainty analysis for handling of tol:
	if abs(q) * tol > 1: break
	if len(seq) >= depth: raise ValueError('Hard to approximate', x)
	seq.append(q)
	tol *= (q - .5)**2

    # x == seq[0] + 1/(seq[1] + 1/(...))
    n, d = seq.pop(), 1
    while seq: n, d = d + n * seq.pop(), n
    return n, d

class Token (object):
    """Base-class for special tokens understood by Continued (q.v.).

    The constructor for Continued takes an iterator which should normally yield
    integer values.  However, it may also yield an instance of (some class
    derived from) this base-class, Token, to represent the rest of the values to
    be yielded.  The idea is that Continued may know how to do magic with the
    given Token; however, in case it doesn't, the Token should behave as an
    iterator, yielding the integers for which it stands.\n"""

    def __iter__(self): raise NotImplementedError('Derived classes should over-ride')

class Cycle (Token):
    """Token representing cyclic repetition of prior yields.

    A Cycle(*seq) object is recognized by Continued (q.v.) as a 'value' in a
    sequence of integers that means 'the rest of the values you want are just
    the entries in seq, repeated endlessly.'  The given sequence must be
    non-empty. Such a sequence is commonly used in the representation of various
    algebraic numbers, particularly square roots of rationals.  The sequence of
    values to be yielded is exposed as property .values, a tuple.\n"""

    def __init__(self, first, *rest): self.__values = (first,) + rest
    def __iter__(self):
	i = 0
	while True:
	    try: yield self.__values[i]
	    except IndexError:
		i = 0
		yield self.__values[i]
	    i += 1

    @property
    def values(self, ig=None): return self.__values

class Series (Token):
    """Token representing an arithmetic succession.

    A Series(a, b) object is recognized by Continued (q.v.) as a 'value' in a
    sequence of integers that stands for a followed, successively, by a +n*b for
    n = 1, 2, ...  Such iterators tend to represent outputs of Bessel functions
    and other irrational values.  It exposes the values of a and b as properties
    .first and .step, respectively\n"""

    def __init__(self, first, step): self.__first, self.__step = first, step
    def __iter__(self):
	n, s = self.__first, self.__step
	while True:
	    yield n
	    n += s

    @property
    def first(self, ig=None): return self.__first
    @property
    def step(self, ig=None): return self.__step

class Continued (object):
    """Continued fractions.

    A continued fraction is a sequence, n, of integers denoting the real number
    n[0] + 1/(n[1] + 1/(n[2] + 1/...)); this is rational precisely if n
    terminates, but can also represent assorted irrationals exactly.\n"""

    def __init__(self, ns):
	"""Construct a continued fraction.

	Single parameter, ns, is an iterable which, when iterated, yields the
	sequence of integers encoding the continued fraction.  If it is in fact
	an iterator, nothing else should attempt to iterate it !  The
	constructed object shall record the values an iterator over it
	yields.  These values should normally be integers.  However, if it
	yields an object of class Token (q.v.), this is taken to be the last
	yield (any further values shall be ignored) and to stand for whatever
	sequence it would yield if iterated.

	The integers used to encode the continued fraction, aside from the
	first, should be non-zero (the code might cope with zeros; but ending on
	a zero is very impolite) and should preferrably avoid 1 and -1, too.\n"""

	if iter(ns) is ns: self.__ns = self.__iterStore(ns)
	else: self.__ns = self.__digest(ns)

    class __IterStore (list):
	__upinit = list.__init__
	def __init__(self, it):
	    self.__upinit()
	    self.__src = it

	__upget = list.__getitem__
	def __getitem__(self, key):
	    try:
		while key >= len(self):
		    it = self.__src.next()
		    self.append(it)
		    if isinstance(it, Token): del self.__src # done
	    except AttributeError: raise IndexError # no __src
	    except StopIteration:
		del self.__src
		raise IndexError

	    return self.__upget(key)

	def __iter__(self):
	    i = 0
	    try:
		while True:
		    yield self[i]
		    i += 1
	    except IndexError: pass

    def __digest(self, ns):
	i, ns = 1, tuple(ns)
	# Eliminate 0 entries from ns[1:]:
	while i < len(ns):
	    if isinstance(ns[i], Token):
		ns = ns[:1+i] # discard everything after the Token
		break
	    elif ns[i]: i += 1
	    else:
		# j +1/(i +1/(0 +1/(h +1/(g +...)))) = j +1/(i+h +1/(g +...))
		try: head, last, next, tail = ns[:i-1], ns[i-1], ns[i+1], ns[i+2:]
		except IndexError: ns = ns[:i-1] # j +1/(i+infinity) = j
		else:
		    if isinstance(next, Cycle):
			rest = next.values
			next = rest[0]
			rest = rest + (next,)
			tail = (Cycle(*rest),)
		    elif isinstance(next, Series):
			next, step = next.first, next.step
			tail = (Series(next + step, step),)
		    elif isinstance(next, Token):
			i += 1
			continue # endure the 0, handle the unrecognized Token

		    ns = head + (last + next,) + tail
		    i = max(1, i-1) # NB: last+next may be zero, too !
	# NB: if ns is now empty, it denotes infinity (of both signs)
	# while i > 1: i -= 1 and weed out the ns[i] in (1, -1) ?
	return ns

    class Grinder (object):
	"""Iterator to orchestrate combination of continued fractions.

	Represents a combination (e.g. product, sum, difference, ratio) of some
	(e.g. two) continued fractions as an iterator that generates the
	integers of the continued fraction for the combined value.

	See 'Item 101B (Gosper): Continued Fraction Arithmetic' in the hakmem
	document referenced in this module's header.  This class generalizes
	Gosper's analysis to any natural number of variables.\n"""

	def __iter__(self): return self
	def __init__(self, srcs, numerator, denominator):
	    """Set up the iterator.

            Requires three arguments:
              srcs -- a tuple of iterators over the sequences representing the
                      numbers to combine as continued fractions;
              numerator -- a tuple of coefficients for the numerator and
              denominator -- a tuple of coefficients for the denominator.

            The length of each tuple of coefficients is 2**len(srcs).  The
            numbers to be combined are referred to as X[b], described by
            iterators x[b], for b in range(len(srcs)).  In each tuple of
            coefficients, the one at index i is the multiplier for a product of
            the X[b] with i&(1<<b) set; so, if cs is a tuple of coefficients,
            the actual numerator or denominator it represents is:

            P(X, cs)
              = cs[0] +cs[1]*X[0] +cs[2]*X[1] +cs[3]*X[0]*X[1] +cs[4]*X[2] +...
              = sum(map(lambda c, xs: reduce(lambda x, y: x * y, xs, c),
                        cs,
                        map(lambda i: map(lambda b, X=X: X(b),
                                          filter(lambda b, i=i: i & (1<<b),
                                                 range(len(srcs)))),
                            range(1<<len(srcs)))))

	    Note that the case len(srcs) is explicitly allowed; then self just
	    represents numerator[0] / denominator[0].  A typical term in P(X,
	    cs) can be understood as

	    T(X, cs, bs) =
	      reduce(lambda b, p, X=X: X[b] * p, bs, cs[sum(map(lambda b: 1<<b, bs))])

	    for some sorted tuple bs of distinct (so the sum in cs[...]
	    implements | on the bits) naturals < len(srcs); when srcs is empty,
	    the only candidate for bs is (). For details of how these data are
	    used, see next().\n"""

	    assert len(numerator) == 1<<len(srcs) == len(denominator)
	    self.__x = list(srcs)
	    self.__n = list(numerator)
	    self.__d = list(denominator)
	    # .__p[i] = Period of Token in which srcs[i] terminated, if any:
	    self.__p = [ None ] * len(srcs)

	    # Now digest the first term of each iterator, to deal with all
	    # entries that might be -1, 0 or 1, to spare next() having to think
	    # about them:
	    b = len(srcs)
	    while b > 0:
		b -= 1
		self.__step(b)

	def __pop(self, p):
	    """Rearrange our expression F to represent 1/(F-p)."""
	    ns = self.__n
	    self.__n = ds = self.__d
	    self.__d = map(lambda n, d, c=p: n -c*d, ns, ds)

	def edges(i, bit): # tool used by other tools
	    """Yields naturals < i in pairs, without and with the given bit."""
	    while i > 0:
		i -= 1
		if i & bit: pass
		yield i, i | bit

	def span(cs, bit, each=edges): # tool used by worst
	    """Indicates how bad the edges associated with bit are.

	    Required argument are a list of values of F at the corners of our
	    bounding cube; and a bit encoding 1<<b for some X[b]; the edges on
	    which only this X[b] varies are studied.  If any such edge sees a
	    change of sign in F's denominator, ValueError is raised; this bit
	    should be __step()ped.  Otherwise, a pair n, d of positives denoting
	    n/d is returned, for which n/d is the biggest difference between F's
	    values at the end-points of any of the edges studied.\n"""

	    t, b = 0, 1
	    for i, j in each(bit):
		(n, d), (m, e) = cs[i], cs[j]
		if d * e <= 0: raise ValueError
		n, d = abs(e * n -m * d), d * e
		if n * b > t * d: t, b = n, d

	    return t, b

	def stir(p, bit, ns, ds, pairs=edges): # tool used by __step, not method
	    """Coefficient update for X[b] with 1<<b == bit

	    Consider a pair i, j of indices into cs with j = i|bit and i =
	    j&~bit; the term in P(X, cs) with cs[j] as coefficient has X[b] as a
	    factor along with all the same X-factors as the cs[i] term, which
	    doesn't have X[b] as a factor.  Aside from these shared factors,
	    then, these two terms are cs[j]*X[b] +cs[i] and we replace X[b] by
	    p+1/X[b] then multiply every term (both in numerator and in
	    denominator, so that it cancels) by X[b].  This turns our pair of
	    terms into cs[j]*(p*X[b] +1) +cs[i]*X[b] = (cs[j]*p +cs[i])*X[b]
	    +cs[j], so we replace cs[j] and cs[i] with cs[j]*p +cs[i] and cs[j],
	    respectively.  Every index into cs either has bit set or not, so
	    shows up in exactly one such pair of indices.\n"""

	    for i, j in pairs(len(ns), bit):
		ns[i], ns[j] = ns[j], ns[i] + p * ns[j]
		ds[i], ds[j] = ds[j], ds[i] + p * ds[j]

	def clear(bit, ns, ds, pairs=edges): # tool used by __step, not method
	    """Coefficient adjustment when x[b] runs out, with 1<<b == bit.

	    As for stir, consider i, j with j = i|bit, i = j&~bit; the terms in
	    P(X, cs) with cs[i] and cs[j] as coefficient are, aside from a
	    shared product of X-factors other than X[b], cs[j]*X[b]
	    +cs[i].  Replacing X[b] simply with p, this becomes cs[j]*p +cs[i],
	    which is exactly what we replaced cs[j] with when we supposed we
	    should replace X[b] with p+1/X[b].  So we can correct our error by
	    replacing what we now have as cs[j] and cs[i] with 0 and the value
	    we computed earlier for cs[j].\n"""

	    for i, j in pairs(len(ns), bit):
		ns[i], ds[i] = ns[j], ds[j]
		del ns[j], ds[j]

	del edges

	def __step(self, b, fix=clear, mix=stir):
	    """Advance .__x[b], if not yet exhausted.

	    When we step x[b] we get some integer p = x[b].next() and x[b]
	    (hence X[b]) changes; we can write our prior value of X[b] in terms
	    of the new value X[b] represented by x[b] after iterating as p
	    +1/X[b], which we can then substitute in everywhere we had X[b] in
	    our formula for F; after a little rearrangement, see stir(),
	    we'll have mixed the coefficients up with one another and with p but
	    we get back an expression of the same form as before.

	    When we try to step x[p] and get StopIteration, matters are a little
	    more complex: this effectively says X[p] is infinite.  We thus
	    implicitly rewind to when we last stepped that iterator and work out
	    what our formula would have become if we'd substituted the final
	    .next() value, without a +1/X[b] term, in place of our prior X or
	    Y.  Fortunately, what would then have resulted can indeed be
	    inferred from what we worked out before we knew we'd reached the end
	    of our iteration; see clear().

	    We also have to take account of the possibility that .next() may
	    give us a Token; in that case, we need to record relevant
	    information from this Token and let it take over as our iterator,
	    supplying its first value as the value we should have had from
	    .next(); it is presumed that every Token functions as a non-empty
	    iterator.\n"""

	    src, bit = self.__x[b], 1<<b
	    if src is None: raise StopIteration
	    try: p = src.next()
	    except StopIteration:
		fix(bit, self.__n, self.__d)
		del self.__x[b], self.__p[b]
	    else:
		while isinstance(p, Token):
		    if isinstance(p, Cycle):
			self.__p[b] = len(p.values)
		    elif isinstance(p, Series) and p.step == 0:
			self.__p[b] = 1
		    self.__x[b] = src = iter(p)
		    p = src.next()
		mix(p, bit, self.__n, self.__d)

	del clear, stir

	def worst(cs, wide=span): # tool
	    bit, bad, bent, t, b = len(cs), [], [], 0, 1
	    while bit > 1:
		bit >>= 1
		try: n, d = wide(cs, bit)
		except ValueError: bent.append(bit)
		else:
		    if n * b > t * n: bad, t, b = [ bit ], n, d
		    elif n * b == t * n: bad.append(bit)

	    assert bad # at least some direction has some difference along at least one edge
	    if bent: return tuple(bent)
	    return tuple(bad)

	def __shrink(cs, judge=worst):
	    for bit in judge(cs):
		self.__step(bit)

	from natural import gcd

	def __paracheck(self, hcf=gcd):
	    """Checks whether .__n and .__d are parallel"""
	    n, d = sum(map(abs, self.__n)), sum(map(abs, self.__d))
	    if filter(None, map(lambda i, e, n=n, d=d: n*e -d*i, self.__n, self.__d)):
		return

	    # self just represents the rational n/d.
	    i = hcf(n, d)
	    self.__n[:], self.__d[:] = [ n/i ], [ d/i ]
	    del self.__x[:], self.__p[:]

	del gcd

	def next(self):
	    """Compute next integer in continued fraction for our number.

	    At any moment, self represents
		F = P(.__n, X) / P(.__d, Y)
	    and, if the range of possible values this could represent lies
	    entirely between p-.5 and p+.5 for some integer p, we can return p
	    after changing the coefficients around to make self represent
	    1/(F-p); see .__pop(p).  Otherwise we can work out which X[b], by
	    the uncertainty in its value, contributed most to the width of the
	    range of values for Z; we then step the iterator associated with the
	    culprit, by calling .__step(b).

	    For purposes of working out error-bar estimates, this code assumes
	    that, aside from the first terms (which the constructor steps
	    through), no iterator yields any integers strictly between -2 and
	    2.  This is equivalent to assuming that, for each b, 1/X[b] is
	    always between -1/2 and +1/2.

	    Since F is a ratio P(.__n, X)/P(.__d, X), we can divide both
	    numerator and denominator by product(X) to get each into the form of
	    a function with the same form as P, using the same coefficients, but
	    with X replaced by the variables Y[b] = 1/X[b] for b < len(X) and
	    Y[b] shows up as a factor in the [i] coefficient's term precisely if
	    bit b of i is *not* set.  Since each Y[b] lies between -.5 and +.5,
	    we can fairly readilly compute bounds on the values the result can
	    take.

	    If F's denominator is zero anywhere inside our unit cube (and F's
	    numerator isn't also zero in all the same places - but __paracheck()
	    deals with this possibility), then the range of values F may take is
	    unbounded and, in particular, not restricted to any interval from
	    p-.5 to p+.5 with p natural.\n"""

	    # First, check __n and __d aren't parallel; if so, we can reduce our
	    # complex expression to a simple rational:
	    if len(self.__x): self.__paracheck()
	    if len(self.__x) == 0:
		# This can also happen if .__step() has hit StopIteration enough
		if self.__d[0] == 0:
		    # self is infinite, represented by the empty sequence
		    raise StopIteration

		n, d = self.__n[0], self.__d[0]
		if d < 0: n, d = -n, -d
		q, r = divmod(n, d)
		if 2 * r > d or (2 * r == d and q % 1): q += 1
		self.__pop(q)
		return q

	    cs = self.__corners()
	    try: q = self.__consensus(cs)
	    except ValueError: pass
	    else:
		self.__pop(q)
		return q

	    self.__shrink(cs)

	@staticmethod
	def __consensus(cs):
	    """See if corner values agree on a single integer.
	    """

	    n, d = cs[0]
	    if filter(lambda (m, e): e * d <= 0, cs[1:]):
		raise ValueError, "Denominator's sign varies"

	    if d > 0: j = 1
	    else: j, n, d = -1, -n, -d
	    q, r = divmod(n, d)
	    if 2 * r > d: ok = ( q + 1, )
	    elif 2 * r < d: ok = ( q, )
	    else: ok = (q, q + 1)

	    i = len(cs),
	    while i > 1 and ok:
		i -= 1
		m, e = cs[i]
		m, e = m * j, e * j
		ok = filter(lambda q: 2 * abs(m - q) < e, ok)

	    if len(ok) > 1: # When we have a choice, prefer even
		ok = filter(lambda q: q % 2 == 0, ok)
		assert ok
	    elif len(ok) < 1:
		raise ValueError, "No consensus integer"

	    return ok[0]

	def __corners(self):
	    """Returns F's values at the corners of X's range of values.

	    At each corner, each X[b] is either -2 or 2; each corner can be
	    characterised by the subset of b with either sign; each such subset
	    can be encoded as an integer with as many bits as there are
	    variables in X.  The return is a tuple of twoples, (n, d), giving
	    the values of numerator and denominator; its entry at index i is for
	    the corner at which each X[b] is 2 if bit b of i is set, else -2.\n"""

	    ns, ds, ans, i = self.__n, self.__d, [], 0
	    assert len(ds) == len(ns) == 1 << len(self.__x)
	    while i < len(ns):
		tn = td = 0 # accumulators
		j = len(ns)
		while j > 0:
		    j -= 1
		    # First, compute product({ X[b]: bit b of j is set}):
		    bit, xs = len(ns), 1
		    while bit > 1:
			bit >>= 1
			if bit & j:
			    xs *= 2
			    if bit & i == 0: xs = -xs

		    tn += xs * ns[j]
		    td += xs * ds[j]

		ans.append(tn, td)
		i += 1

	    return tuple(ans)

    def ingest(val):
	try: return iter(val.__ns)
	except AttributeError: pass
	return real_continued(val)

    def __add__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, 1, 1, 0),
			       (1, 0, 0, 0)))
    __radd__ = __add__
    def __sub__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, -1, 1, 0),
			       (1, 0, 0, 0)))

    def __rsub__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, 1, -1, 0),
			       (1, 0, 0, 0)))

    def __mul__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, 0, 0, 1),
			       (1, 0, 0, 0)))
    __rmul__ = __mul__

    def __truediv__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, 0, 1, 0),
			       (0, 1, 0, 0)))

    def __rtruediv__(self, other, get=ingest, grind=Grinder):
	return Continued(grind((iter(self.__ns), get(other)),
			       (0, 1, 0, 0),
			       (0, 0, 1, 0)))

    # any more ?
    del Grinder

    def __cmp__(self, other, get=ingest):
	i, ns = 0, self.__ns
	try:
	    for y in get(other):
		y = cmp(ns[i], y)
		if y:
		    # allegedly ...
		    if i % 2: return -y
		    return y
		i += 1

	    if i == len(ns): return 0

	except IndexError: # no such ns[i]
	    raise NotImplementedError
	else:
	    raise NotImplementedError

    del ingest
