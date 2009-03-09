"""Continued fractions.

Exports:
  real_continued(val) -- iterator over continued fraction approximation to val
  rationalize(x [, tol, depth]) -- try to approximate x = n / d for whole n, d

See also:
http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
expounding the virtues of continued fractions.

$Id: continued.py,v 1.2 2009-03-09 05:20:56 eddy Exp $
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
	int i = 0
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
	"""Iterator to represent combination of continued fractions.

	Represents a combination (e.g. product, sum, difference, ratio) of two
	continued fractions as an iterator that generates the

	See 'Item 101B (Gosper): Continued Fraction Arithmetic' in the hakmem
	document referenced in this module's header.\n"""

	def __iter__(self): return self
	def __init__(self, srcs, numerator, denominator):
	    """Set up the iterator.

            Requires three arguments:

              src -- a tuple of iterators over the sequences representing the
                     numbers to combine as continued fractions;
              numerator -- a tuple of coefficients for the numerator and
              denominator -- a tuple of coefficients for the denominator.

            The length of each tuple of coefficients is 2**len(src).  The
            numbers to be combined are referred to as X[b], described by
            iterators x[b], for b in range(len(src)).  In each tuple of
            coefficients, the one at index i is the multiplier for a product of
            the X[b] with i&(1<<b) set; so, if cs is a tuple of coefficients,
            the actual numerator or denominator it represents is:

            P(X, cs)
              = cs[0] +cs[1]*X[0] +cs[2]*X[1] +cs[3]*X[0]*X[1] +cs[4]*X[2] +...
              = sum(map(lambda c, xs: reduce(lambda x, y: x * y, xs, c),
                        cs,
                        map(lambda i: map(lambda b, X=X: X(b),
                                          filter(lambda b, i=i: i & (1<<b),
                                                 range(len(src)))),
                            range(1<<len(src)))))

	    A typical term in P(X, cs) can be understood as

	    T(X, cs, bs) =
	      reduce(lambda b, p, X=X: X[b] * p, bs, cs[sum(map(lambda b: 1<<b, bs))])

	    for some sorted tuple bs of distinct (so the sum in cs[...]
	    implements | on the bits) naturals < len(src).

	    For details of how these data are used, see next().\n"""

	    assert len(numerator) == 1<<len(src) == len(denominator)
	    self.__x = list(src)
	    self.__n = list(numerator)
	    self.__d = list(denominator)
	    # .__p[i] = Period of Token in which src[i] terminated, if any:
	    self.__p = [ None ] * len(src)

	def __pop(self, p):
	    """Rearrange our expression F to represent 1/(F-p)."""
	    ns = self.__n
	    self.__n = ds = self.__d
	    self.__d = map(lambda n, d, c=p: n -c*d, ns, ds)

	def stir(p, bit, cs):
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

	    i = len(cs)
	    while i > 0:
		i -= 1
		if i & bit: continue
		j = i | bit
		cs[i], cs[j] = cs[j], cs[i] + p * cs[j]

	def clear(bit, cs):
	    """Coefficient adjustment when x[b] runs out, with 1<<b == bit.

	    As for stir, consider i, j with j = i|bit, i = j&~bit; the terms in
	    P(X, cs) with cs[i] and cs[j] as coefficient are, aside from a
	    shared product of X-factors other than X[b], cs[j]*X[b]
	    +cs[i].  Replacing X[b] simply with p, this becomes cs[j]*p +cs[i],
	    which is exactly what we replaced cs[j] with when we supposed we
	    should replace X[b] with p+1/X[b].  So we can correct our error by
	    replacing what we now have as cs[j] and cs[i] with 0 and the value
	    we computed earlier for cs[j].\n"""

	    i = len(cs)
	    while i > 0:
		i -= 1
		if i & bit: continue
		j = i | bit
		cs[i], cs[j] = cs[j], 0

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
		fix(bit, self.__n)
		fix(bit, self.__d)
		self.__x[b] = None
	    else:
		while isinstance(p, Token):
		    if isinstance(p, Cycle):
			self.__p[b] = len(p.values)
		    elif isinstance(p, Series) and p.step == 0:
			self.__p[b] = 1
		    self.__x[b] = src = iter(p)
		    p = src.next()
		mix(p, bit, self.__n)
		mix(p, bit, self.__d)

	del clear, stir

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
	    that only the first term in any sequence might be -1, 0 or 1.\n"""

	    raise NotImplementedError

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
