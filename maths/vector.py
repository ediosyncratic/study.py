"""Vectors (and tensors).

Exports:
  Vector -- manages the coordinates of vectors or tensors
  Tensor -- alias for Vector, for use when rank > 1
  Namely -- for when coordinates are thought of by name mostly

See the documentation of each for more.

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop
from study.snake.sequence import Tuple

class Vector (Tuple):
    """Tuple type supporting entry-by-entry arithmetic.

    Built-in tuple and list types support addition by concatenation,
    producing a longer sequence, and multiplication (only by whole numbers) as
    repeated addition.  In contrast, Vector supports
     * addition and subtraction as operations on corresponding entries
       (i.e. (x+y)[i] = x[i]+y[i], etc.), and
     * multiplication as acting on each entry, i.e. (x*k)[i] = x[i]*k, with
       (only) scalar multiplication defined commutative (so, for scalar k, k*x
       is just x*k); this leads to a (non-commutative) tensorial
       multiplication when k, as well as x, is a Vector.

    In general, division is not well-defined (even where it can be defined, it
    is not always unambiguous).  I may, some day, add support for at least
    some of the cases where it is well-defined; but that hasn't happened yet.

    The product of two Vector objects is technically a Vector whose entries are
    Vector-valued; such a vector is termed a tensor.  The depth of nesting of
    Vector within Vector is known as the 'rank' of the tensor; a plain Vector,
    with numeric entries, has rank 1; multiplying two tensors of ranks n and m
    yields a tensor of rank n+m.  (A raw scalar can be thought of as a tensor of
    rank 0; but this is not attempted here.)

    For a sequence to be a valid input to the constructor, either its entries
    should all be numeric (and behave reasonably like reals) or its entries
    should all be Vector instances, with equal .dimension attributes.  See
    pseudo-constructor fromSeq() for a more liberal approach.

    Pseudo-constructors:
      fromSeq(seq [, dim]) -- recursively traverse into seq's entries
      diagonal(seq) -- specify the diagonal entries of a square

    Lazy properties:
      rank -- depth of nesting of Vector; see above.
      dimension -- tuple of ranges of successive viable indices

    Methods:
      symmetrise([ranks]) -- average over permutations of given ranks (or all)
      antisymmetrise([ranks]) -- as symmetrise, scaling by signature
      transpose([n=1]) -- swap rank 0 with rank n
      tau(pattern) -- generalised permutate-and-trace
      permutrace(shuffle, *pairs) -- alternative permute-and-trace
      dot(other, [n=1, out=True]) -- contract with other on self's right
      rdot(other, [n=1, out=True]) -- contract with other on self's left

    Arithmetic operations allow the other operand to be vectors - for addition
    and subtraction, they should have the same .dimension - or sequences that
    are acceptable to the constructor.  Its indexing accepts a tuple, applying
    each entry successively.\n"""

    @classmethod
    def _vector_(cls, seq):
        """Pseudo-constructor by which derived classes can mimic base.

        By default, various methods of this class create objects of the same
        class as the instance whose method was called; this is achieved simply
        by passing this method the sequence of (or iterator over) values to be
        used as entries in the new object, exactly as for Tuple.  If a derived
        class has a new/constructor with a different signature, it should
        over-ride this method to do something sensible; it is always called as
        vec._vector_(seq) with vec either a class based on Vector or an
        instance of such a class.\n"""
        return cls(seq)

    @classmethod
    def xerox(cls, dims, leaf=0.):
        """A tensor duplicating a given leaf through specified dimensions.

        Required argument, dims, is a sequence of (or iterator over)
        dimensions.  Optional second argument, leaf, is either a number
        (default is zero) or a Tensor.

        A pure number tacitly has an empty tuple as its .dimension; the
        .dimension of the return value shall be dims (as a tuple) plus that of
        leaf.  For every valid index tuple of a tensor with dims as its
        .dimension, the result's value for that tuple shall be leaf.

        Note that each Tensor created by this method uses the same object for
        all of its entries - a Vector of dims[-1] references to leaf is
        created; that Vector is then referenced dim[-2] times by the Tensor of
        next higher rank; and so on.  For any valid index tuples, s and t, of
        equal length no greater than that of dims, into r = cls.xerox(dims,
        leaf), we can assert r[s] is r[t].  Thus only len(dims) Tensor objects
        are created (albeit sum(dims) references are created); this is
        significantly more compact than what cls.xerox(dims, 1) * leaf will
        give you.

        Example: cls.xerox(dims) is the zero tensor whose .dimension is
        dims.\n"""

        dims = iter(dims) # no-op if dims is already an iterator
        try: n = dims.next()
        except StopIteration: return leaf
        return cls._vector_((cls.xerox(dims, leaf),) * n)

    @classmethod
    def delta(cls, dim, *ns):
        """Vector selecting given co-ordinates.

        First argument, dim, is the .dimension of the resulting Vector or
        Tensor; all subsequent arguments are indices into it.  The result has
        value 1 at each of these (ignoring any index with any component >= the
        matching entry in dim) and all other entries zero.  In particular,
        cls.delta(dim) is the zero tensor with the given dimensions.

        If an index argument is a sequence shorter than dim, it matches
        all indices of full rank that begin with the given indexing; so indexing
        the result with such a short index shall get a tensor whose entries are
        all 1.  Thus cls.delta(dim, ()) is the tensor, with the given
        dimensions, whose co-ordinate values are all 1.

        Index arguments longer than dim are effectively trimmed at dim's length.

        As a convenience: passing an integer as dim, with each subsequent
        argument as an integer, is accepted as equivalent to packing each as a
        one-tuple - which involves way too much punctuation !\n"""
        try: n, tail = dim[0], dim[1:]
        except TypeError:
            assert isinstance(dim, (int, long))
            assert all(isinstance(n, (int, long)) for n in ns)
            return cls._vector_([1 if i in ns else 0 for i in range(dim)])
        except IndexError: return 1 if () in ns else 0

        if tail:
            return cls._vector_([cls.delta(tail, *[ts[1:] for ts in ns
                                                     if not ts or ts[0] == i])
                                   for i in range(n)])
        return cls._vector_([1 if i in [ n[0] if n else i for n in ns ] else 0
                               for i in range(n)])

    @classmethod
    def diagonal(cls, seq, rank=2):
        """Returns a tensor whose [i][i] is seq[i].

        All other entries are zero.  Required argument, seq, gives the values on
        the diagonal.  These should normally be numeric, but vectors or tensors
        are accommodated.  For the usual 'identity' matrix of dimension n,
        Vector.diagonal([1]*n) will do fine.

        Optional second argument, rank,
        defaults to 2; when given, a tensor of this rank is returned, whose
        entries are zero except those with indeces (i,) * rank, where seq[i] is
        used.  (The case rank = 1 is thus equivalent to .fromSeq).\n"""

        row, n = [], len(seq)
        f = (lambda n: n) if rank == 2 else (lambda n, r=rank-1: (n,) * r)
        for i, v in enumerate(seq):
            row.append(cls.delta(f(n), f(i)) * v)

        return cls._vector_(row)

    @classmethod
    def fromSeq(cls, seq, form=None):
        """Construct a Vector from a loosely suitable sequence.

        Required first argument, seq, is a sequence whose entries either are
        all numbers or are all suitable as first arguments to this method.  In
        the latter case, each is indeed passed, recursively, to this method;
        the sequence of returns is used in place of seq - save that, if any
        two of them have different .dimension, a ValueError is raised.

        Optional second argument, form, may be None (its default), in which case
        it is ignored.  Otherwise, it should be a Vector instance with the
        expected type of the return; this implies dimension, but also controls
        which classes based on Vector to use for each entry, at each rank, in
        the result.  For each ind for which form[ind] is valid, the matching
        [ind] of the returned tensor shall have the same type and .dimension as
        form[ind] (in particular, from ind = (), the class of form is used for
        the return, whose .dimension is equal to that of form); if this is not
        possible (i.e. the length of some sequence doesn't match the
        .dimension[0] of the vector it's to be turned into), a ValueError is
        raised.

        Returns a Vector object that's entry-by-entry equal to seq, where such
        equality is defined by: two sequences r, s are entry-by-entry equal
        precisely if, for every i for which either r[i] or s[i] doesn't raise
        an IndexError:
         * neither r[i] nor s[i] raises any exception when evaluated
         * if either r[i] or s[i] is a sequence, so is the other and the two
           are entry-by-entry equal;
         * otherwise, both r[i] and s[i] are numbers and r[i] == s[i].

        This method exists so that repr() can avoid repeating this class's name
        a whole lot !  It is, hopefully, also useful generally.\n"""

        assert None not in seq, 'Vector entries should be numeric'
        if form is not None:
            assert isinstance(form, Vector), 'Template should be a Vector'
            if not issubclass(cls, form.__class__):
                cls = form.__class__ # form over-rides which class to use

        if isinstance(seq, Vector):
            if form is None: pass # use seq as is; nothing to check or adapt
            else:
                if not form.__sametype(seq, form):
                    raise ValueError('Mismatched types', form, seq)
                # Recurse so as to ensure correct types, in case seq uses
                # different types to form at some positions:
                seq = [ f.fromSeq(s, f) for s, f in zip(seq, form) ]

        else:
            seq = tuple(seq) # in case it's an iterator
            if form is not None and len(seq) != form.dimension[0]:
                raise ValueError('Mismatched dimension', form.dimension[0], seq)

            isnum = map(cls.__isnumeric, seq)
            if all(isnum):
                if form is not None:
                    if form.rank > 0:
                        raise ValueError('Expected vector or tensor entries',
                                         seq, form.dimension)
            elif any(isnum):
                raise ValueError('Mixed numeric and non-numeric entries', seq)
            else:
                if form is None: seq = [ cls.fromSeq(s) for s in seq ]
                else: seq = [ f.fromSeq(s, f) for s, f in zip(seq, form) ]

        return cls._vector_(seq)

    @staticmethod
    def __isnumeric(val):
        if isinstance(val, Vector): return False
        try: iter(val) # you can't iterate a number
        except (TypeError, AttributeError):
            try: 0 * val + val # but you can add zero times it to it
            # We can't simply try: 0 + val, as val might have units;
            # e.g. if it's an instance of study.value.Quantity.
            except TypeError: pass # well, actually, fail
            else: return True

        return False

    @classmethod
    def __sametype(cls, seq, form):
        assert cls is form.__class__
        if not isinstance(seq, cls): return False
        if len(seq) != len(form): return False

        if isinstance(form[0], Vector):
            assert all(isinstance(val, Vector) for val in form[1:])
            return all(t.__sametype(o, t) for o, t in zip(seq, tail))

        assert not any(isinstance(val, Vector) for val in form[1:])
        return all(cls.__isnumeric(val) for val in seq)

    # Special casees of .dimension == (2, 2):
    from math import pi, sin, cos, sinh, cosh, atanh
    @classmethod
    def circulate(cls, angle, unit=2*pi, s=sin, c=cos):
        """Rotation through the given angle (of a circle).

        Required argument, angle, is the angle through which to rotate; by
        default, the rotation is anti-clockwise and the angle is measured in
        turns.

        Optional second argument, unit, defaults to 2*pi; it is the unit of
        angle, measured in radians; pass unit=1 for angles in radians.  To
        pass an angle in degrees, simply divide it by 360 and pass it as
        angle, using the default unit; this is easier than computing pi/180 as
        unit !

        Returns a tensor of .dimension (2, 2) representing the specified
        rotation.  Use .embed() for higher dimensions.\n"""
        angle *= unit
        s, c = s(angle), c(angle)
        # Avoid stupid 6e-17ish value for one when it should be zero:
        if s in (-1, 1): c = 0
        if c in (-1, 1): s = 0
        return cls.fromSeq(((c, s), (-s, c)))

    @classmethod
    def hyperbolate(cls, speed, mode=atanh, s=sinh, c=cosh):
        """Hyperbolic 'rotation'

        Required argument, speed, controls how far the 'rotation' deviates
        from the identity; by default, its meaning corresponds to the fraction
        of the speed of light at which an observer is moving, whose frame of
        reference is 'rotated' to the degree in question.

        Optional second argument is a callable; it defaults to math.atanh and
        is used to convert speed to an appropriate input to cosh and
        sinh.  Passing math.log in its place will read speed as the Doppler
        shift of the observer's frame, instead of its velocity.  If None is
        passed instead, the identity is implicitly used; this is suitable for
        use with the .Lorentz attribute of a Quantity whose value is a speed.

        Returns a tensor of .dimension (2, 2) representing the Lorentz
        transformation corresponding to the specified relative motion.  Use
        .embed() for higher dimensions.\n"""
        if mode is not None: a = mode(speed)
        s, c = s(a), c(a)
        return cls.fromSeq(((c, s), (s, c)))
    del pi, sin, cos, sinh, cosh, atanh

    @classmethod
    def antisymmetric(cls, dim, scale=None):
        """Totally antisymmetric tensor on a space of dimension dim.

        Required argument, dim, is the dimension of the underlying space and the
        rank of the returned tensor.  Optional argument, scale, defaults to
        None; if given (and not None), it should be a number (although,
        technically, you'll get away with passing any Tensor as it); if None, it
        defaults to 1/dim! (to get the same result as symmetrising the product
        of the standard basis).  If you index the returned tensor with any even
        permutation of dim, you'll get scale; while any odd permutation shall
        give -scale.  The value of the result is equal to the result of (right)
        multiplying, by scale, the result you'd have obtained by passing 1 as
        scale: but the returned tensor is more compact (by re-using entries,
        c.f. what .xerox() does).

        The result is the tensor A for which, when s is an index-sequence of
        length dim, with all(0 <= si < dim for si in s), A[s] is zero unless s
        is a permutation, in which case A[s] is scale times s.sign, its
        signature, in (+1, -1).

        See also symmetrise() and antisymmetrise(), for when you already have a
        tensor and want the relevant part of it.\n"""

        # Implement scale's default:
        if scale is None:
            scale, n = cls.__rat_over(1), dim
            while n > 0: scale, n = scale / n, n - 1

        if dim < 2: return cls._vector_((scale,) * dim) # boring

        # Delegate to private method, mainly to isolate its huge theory doc-string !
        return cls.__antisymmetric(dim,
                                   0 * scale,
                                   { tuple(range(dim)): (scale, -scale) },
                                   cls._vector_)

    def __repr__(self):
        nom = self.__class__.__name__
        if self.rank > 1:
            if nom == 'Vector': nom = 'Tensor'
            nom += '.fromSeq'
        return '%s(%s)' % (nom, str(self))

    def __str__(self):
        if len(self) == 1: return '(%s,)' % str(self[0])
        return '(' + ', '.join(self.map(str)) + ')'

    def __nonzero__(self):
        for it in self:
            if it: return True
        return False

    def __add__(self, other):
        assert len(other) == len(self)
        return self._vector_(x + y for x, y in zip(self, other))

    __radd__ = __add__
    def __neg__(self):
        return self._vector_(-x for x in self)

    def __sub__(self, other):
        assert len(other) == len(self)
        return self._vector_(x - y for x, y in zip(self, other))

    def __rsub__(self, other):
        assert len(other) == len(self)
        return self._vector_(y - x for x, y in zip(self, other))

    def __mul__(self, other):
        if not (self.__isnumeric(other) or
                isinstance(other, Vector)):
            other = self.fromSeq(other)

        return self.__mul(other)

    def __mul(self, other):
        return self._vector_(s * other for s in self)

    def __rmul__(self, other):
        if self.__isnumeric(other): return self.__mul(other)
        if isinstance(other, Vector): return other.__mul(self)
        return self.fromSeq(other).__mul(self)

    def __pow__(self, other, base=None):
        assert base is None # TODO: can we make any sense of it, if not ?
        if other < 0:
            # self, other = self.inverse, -other # not meaningful for all tensors
            raise ValueError('Raising to negative powwer', other)

        other, r = divmod(other, 2)
        result = self if r else 1
        while other > 0:
            self = self * self
            other, r = divmod(other, 2)
            if r: result = self * result

        return result

    __upget = tuple.__getitem__
    def __getitem__(self, key):
        """Extend indexing to accept list of indices at successive ranks.

        Single argument, key, may be a simple integer, in which case it is
        used as index into self, as a tuple, in the usual way.  Otherwise, key
        should be a sequence of indices, of length at most self.rank; the
        result is defined inductively by: self[()] = self, self[(h, *t)] =
        self[h][t] or, more intuitively if marginally less rigorously,
        specified as
          self[(i, j, ..., n)] = self[i][j][...][n].
        Such dereferencing is apt to lead to IndexError unless 0 <= key[i] <
        self.dimension[i] for 0 <= i < len(key).\n"""

        try: ks = key.to_slice() # See study.snake.regular
        except AttributeError: pass
        else: key = ks
        if isinstance(key, slice):
            return self._vector_(self.__upget(key))

        try: key[:]
        except TypeError:
            return self.__upget(key)

        assert not None in key
        for k in key: self = self[k]
        return self

    @lazyprop
    def rank(self):
        """The rank of self, as a tensor.

        If the entries in self are numbers, self's rank is 1; otherwise, each
        entry in self is a Vector and all have the same rank; self's rank is
        one greater than that of its entries.\n"""
        if isinstance(self[0], Vector):
            assert all(isinstance(x, Vector) for x in self)
            r = self[0].rank
            assert all(x.rank == r for x in self)
            return 1 + r
        return 1

    @lazyprop
    def dimension(self):
        """The sequence of dimensions at different ranks.

        For a tensor, len(self[i]) is the same for all 0 <= i < len(self); for
        each such i, len(self[i][j]) is the same for all 0 <= j <
        len(self[i]); and so on.  The tuple self.dimension, of length
        self.rank, collects up these lengths, starting with dimension[0] =
        len(self).

        Given the extended indexing provided by __getitem__, in so far as
        dimension[:i] is determined and i <= self.rank, for any sequence s of
        length i having, for 0 <= j < i, 0 <= s[j] < dimension[j], we define
        dimension[i] to be len(self[s]) and assert that its value is
        independent of choice of s, subject to given constraints.\n"""
        if isinstance(self[0], Vector):
            assert all(isinstance(x, Vector) for x in self)
            tail = self[0]. dimension
            assert all(x.dimension == tail for x in self)
            return (len(self),) + tail

        return (len(self),)

    @lazyprop
    def squaresum(self):
        """The sum of squares of self's components.

        When the co-ordinates in use are orthonormal, with respect to whatever
        metric it makes sense for you to be using, the square roof of this is
        the length of self.  For the sake of the case where self's components
        are complex, the square of the absolute value is used.\n"""

        if self.rank < 2: return sum(abs(x) ** 2 for x in self)
        return sum(x.squaresum for x in self)

    @lazyprop
    def biggest(self):
        """The index of a maximal co-ordinate of self.

        Various algorithms want to know this so that they can work out how to
        refine an answer in one way or another.\n"""

        each = self.iteritems()
        big, was = each.next()
        was = abs(was)

        for ind, val in each:
            val = abs(val)
            if val > was: was, big = val, ind

        return big

    def iteritems(self, depth=None, *others):
        """Enumerates self, optionally in parallel with others.

        All arguments are optional.  The first, depth, should be a natural up to
        self.rank or None, in which case self.rank is used.  Each yield of this
        function shall be a tuple whose first entry is an index-tuple, of length
        depth, into self; when this first entry is ind, the second is self[ind].

        Thus, if no arguments are passed, using .iteritems() as if self were a
        mapping, we iterate over the maximal-length indexing-tuples it accepts,
        as if it were a mapping from these to the co-ordinates they produce.

        All others arguments, if any, should be iterables (they may be
        iterators) with the same structure as self; each tuple yielded by this
        functio shall be, in effect, (ind, self[ind]) + tuple(o[ind] for o in
        others), except that the values of o[ind] are obtained by iterating the
        entries in others, and recursively iterating the values they yield,
        instead of performing lookups using __getitem__().\n"""

        if depth is None: depth = self.rank
        if depth:
            depth -= 1
            others = tuple(iter(o) for o in others)
            if depth:
                for i, t in self.enumerate():
                    for seq in t.iteritems(depth, *[ o.next() for o in others ]):
                        yield ((i,) + seq[0],) + seq[1:]
            else:
                for i, t in self.enumerate():
                    yield ((i,), t,) + tuple(o.next() for o in others)

        else: yield ((), self) + others

    def pointwise(self, func, rank=None, *others):
        """Pointwise combination of many tensors.

        First argument, func, is a function; second argument, rank, is either a
        natural up to self.rank or None, in which case self.rank is used.  All
        subsequent arguments should be Vector (or Tensor) objects whose
        .dimension[:rank] matches self's.  For each valid index ind of the given
        rank, func is called with self[ind] and the [ind] entries of the others
        as its arguments; what it returns is used as the [ind] of the result of
        pointwise().\n"""

        if rank is None: rank = self.rank
        assert all(o.dimension[:rank] == self.dimension[:rank] for o in others)
        if rank < 1: return func(self, *others)
        if rank == 1: return Vector([ func(self[i], *[ o[i] for o in others ])
                                      for i in range(self.dimension[0]) ])
        return Vector([ self[i].pointwise(func, rank-1,
                                          *[ o[i] for o in others ])
                        for i in range(self.dimension[0])])

    def symmetrise(self, ranks=None):
        """Return symmetric part of a tensor.

        Single optional argument, ranks, is either None or an iterable
        yielding some sub-set of range(0, self.rank); if None, the whole
        range is presumed.  All members, i, of the set indicated by
        ranks must have self.dimension[i] equal.  The result is obtained
        by averaging self.tau(s) over all permutations s of range(0,
        self.rank) that preserve the indices not in ranks.\n"""

        return self.__perm_average(self.dimension, ranks, self.tau)

    def antisymmetrise(self, ranks=None):
        """Return antisymmetric part of a tensor.

        Single optional argument, ranks, is either None or an iterable
        yielding some sub-set of range(0, self.rank); if None, the whole
        range is presumed.  All members, i, of the set indicated by
        ranks must have self.dimension[i] equal.  The result is obtained
        by averaging self.tau(s) * s.sign over all permutations s of
        range(0, self.rank) that preserve the indices not in ranks.\n"""

        return self.__perm_average(self.dimension, ranks,
                                   lambda e, t=self.tau: t(e) * e.sign)

    def transpose(self, n=1):
        """Transpose a tensor; only applicable if self.rank > 1.

        Optional argument, n, defaults to 1; it must be a natural number less
        than self.rank (and passing 0 is fatuous; you get self).\n"""

        if n < 0 or n != int(n):
            raise ValueError("Should be a natural number", n)
        elif n >= self.rank:
            raise ValueError("Should be less than rank", n, self.rank)
        elif n == 1:
            return self._vector_(self[0].mapwith(
                    lambda *args: args, *self[1:]).map(self._vector_))
        elif n == 0: return self

        return self._vector_(
            self.transpose().map(lambda v: v.transpose(n-1))).transpose()

    def embed(self, route, other=None):
        """Embeds a tensor into a higher dimension.

        Required argument, route, is a sequence of sequences of indices, with
        len(route) <= self.rank, len(route[i]) <= self.dimension[i] for each i
        and no duplicate entries in any given route[i].  (Note that route and
        each of its entries shall be iterated repeatedly; so must be
        sequences, not iterators.)  The returned tensor shall have each
        .dimension[i] >= that of self and > max(route[i]).

        Any index-tuple s valid for the result with each s[i] in route[i],
        hence s[i] = route[i][t[i]] for some tuple t, has self[t] as the entry
        at [s], at least when len(s) == len(route); otherwise, the entry in
        the result is a suitable .embed() of the entry in self.

        Optional second argument, other, defaults to None, denoting a suitably
        large Tensor whose entries are all zero; otherwise, it is a Tensor from
        which to draw the values not determined by the fore-going rule for
        entries in the result.  As such, it must have the same .rank as self,
        with max(route[i]) < other.dimension[i] >= self.dimension[i] for each
        i.  In particular, self and other must have equal
        .dimension[len(route):] to make the 'leaf' values of the embedding match
        up.  (We could pad with map(range, self.dimension[len(route):]) if this
        condition were not met, but it'd be less efficient and risk hiding
        errors; the caller can do such padding if really intended.)

        Example: for n < k > m,
        Tensor.circulate(a).embed(((n, m), (n, m)), Tensor.diagonal([1] * k))
        is a linear map on a k-dimensional space, acting as a rotation on the
        n-th and m-th directions and the identity on all others.\n"""

        n = len(route)
        if n > self.rank:
            raise ValueError('Too many index sequences', route, self.rank)

        bad = [ r for r in route if min(r) < 0 ]
        if bad: raise ValueError('Negative indices', bad)

        bad = [ r for r in route if len(set(r)) != len(r) ]
        if bad: raise ValueError('Duplicate entries in indexing', bad)

        bad = [ i for i in range(n) if len(route[i]) > self.dimension[i] ]
        if bad: raise ValueError(
            'Indexing outside available range', bad, self.dimension, route)

        if other is None:
            # Compute implicit 'big enough' Tensor of zeros:
            other = self.xerox(tuple(1 + max(d - 1, *r) for d, r in
                                     zip(self.dimension, route)) +
                               self.dimension[n:])

        elif other.rank != self.rank:
            raise ValueError('Mismatched rank', other.rank, self.rank)
        elif other.dimension[n:] != self.dimension[n:]: raise ValueError(
            'Mismatched leaf dimensions', n, other.dimension, self.dimension)
        else:
            bad = [ i for i in range(n) if max(route[i]) >= other.dimension[i] ]
            if bad: raise ValueError('Indexing outside reference tensor',
                                     bad, other.dimension, route)

        return self.fromSeq(self.__embed(route, other))

    def __embed(self, route, other):
        """See embed.  Computes the brute sequence [of sequences ...]."""

        row, here, route = list(other), route[0], route[1:]
        if route:
            assert self.rank > len(route)
            assert len(row) > max(here) and min(here) >= 0
            assert self.dimension[0] >= len(here)
            for i, r in enumerate(here):
                row[r] = self[i].__embed(route, other[r])
        else:
            assert other.dimension[1:] == self.dimension[1:]
            for i, r in enumerate(here):
                row[r] = self[i]

        return row

    # TODO: can this be more efficient ?
    # __mul__ is quite heavy-weight; can we short-cut it ?
    # Is it worth it ?
    def dot(self, other, n=1, out=True):
        """Contracts self with other.

        Required argument, other, should be a Vector.  Optional arguments:
          n -- number of ranks of self to contract out with equally many of
               other; defaults to 1.
          out -- when n > 1, determines how ranks of self are matched up with
                 ranks of other; see below.

        This method first computes self * other, then traces away some ranks
        from the product.  If n is 1, the last rank from self is traced with
        the first of out.  When n > 1, if out is true, ranks of self are
        matched with ranks of other starting 'where they meet', with self's
        last and other's first, and 'working outwards from there', so self's
        last-but-one rank is contracted with others second rank, and so on,
        until self's last-but-(n-1) rank is contracted with other's n-th
        rank.  If out is false, self's last rank is contracted with other's
        n-th rank; self's last-but-i rank is contracted with other's (n-i)-th
        rank and so on, until self's last-but-(n-1) rank is contracted with
        other's first rank.

        Returns what's left of the product after all this tracing has been
        applied.\n"""
        return (self * other).permutrace((), *self.__derange(out, n, self.rank))

    @staticmethod
    def __derange(out, n, r):
        """Returns pairs of indices for contraction.

        Arguments out and n are as for .dot() and .rdot(); r is the index of
        the first rank of the right operand.  We thus pair up the indices from
        r to n+r-1 with those from r-n to r-1, either in same order or reverse
        order according to out.\n"""

        i = n + r
        if out: j, s = r - n, 1
        else: j, s = r - 1, -1

        while i > r:
            i -= 1
            yield j, i
            j += s

    def rdot(self, other, n=1, out=True):
        """Reverse contraction; c.f. .dot()

        Takes the same arguments as .dot(); the effect is exactly as if self
        and other were swapped, save that other need not actually be a Vector
        (or Tensor) for it to work; indeed, the implementation expects that it
        is not (so is mildly less efficient than other.dot(self) when other is
        a Vector or Tensor).\n"""
        prod = other * self
        return prod.permutrace((), *self.__derange(out, n, prod.rank - self.rank))

    def tau(self, pattern):
        """Generalised trace-permutation operator.

        Single operand, pattern, is a sequence of length at most self.rank
        whose entries may be of two kinds: for some natural n, the whole
        numbers 0 through n-1 appear once each in pattern; any other entries
        in pattern must be strings, each of which must appear exactly twice in
        pattern, at indices whose matching entries in self.dimension are
        equal.  (The tensor modelled by self must in fact have mutually dual
        spaces at its relevant tensor rank factors; but this implementation
        only knows about their dimensions.)

        For each pair of indices in pattern that share the same string, we
        contract out (trace) the ranks of self having those indices.  The
        remaining ranks of self we permute according to the integer indices;
        if pattern[a] is an integer i, then the result's i-th rank shall
        correspond to self's a-th rank.

        The result is the tensor that would be obtained by taking the
        following steps, with dim = self.dimension:

          * whenever pattern[a] == pattern[b], b > a, we require dim[a] ==
            dim[b] and replace self with a tensor having two fewer ranks; this
            has dimension = dim[:a] +dim[a+1:b] +dim[b+1:]; for each valid
            index-tuple s into it, with len(s) == b-1, its [s] entry is the
            sum over i in range(dim[b]) of self[s[:a] + (i,) + s[a:] + (i,)].

            After replacing self with the thus-contracted tensor, we use
            pattern[:a] +pattern[a+1:b] +pattern[b+1:] in place of pattern.

          * once all strings are thus eliminated, we are left with a
            permutation as pattern; we re-organise self to produce a result:
            whose dimension[pattern[a]] is dim[a], for each a; and, for each
            valid index-tuple s into it, its [s] entry is self[t] where t[a] =
            s[pattern[a]] for each index a into pattern.

        The final result is not, however, computed as inefficiently as this
        would imply.  Contrast .permutrace().\n"""

        stub, pairs, bok, n = [ None ] * len(pattern), [], {}, []
        for i, a in enumerate(pattern):
            if isinstance(a, basestring):
                try: j = bok[a]
                except KeyError: bok[a] = i
                else:
                    bok[a] = None
                    if j is None: raise ValueError(
                        'Trace marker appears more than twice', a, pattern)
                    if self.dimension[i] != self.dimension[j]:
                        raise ValueError(
                            'Can only trace between ranks of equal dimension',
                            (j, i), self.dimension, a, pattern)
                    pairs.append((j, i))
            else:
                stub[i] = a
                if a+1 > len(n): n += range(len(n), a+1)
                if n[a] is None:
                    raise ValueError('Permutation index repeated', a, pattern)
                assert n[a] == a
                n[a] = None

        # Check everything in n did get used up:
        if [ a for a in n if a is not None ]:
            raise ValueError('Incomplete permutation', tuple(n), pattern)

        return self.__trace_permute(stub, pairs)

    def permutrace(self, shuffle, *pairs):
        """Alternate trace-permute operation.

        First argument, shuffle, is a permutation optionally padded with None
        entries; its length must not exceed self.rank and the non-None entries
        in it should be the integers 0 through n-1, each appearing exactly
        once, for some natural n.

        Each subsequent argument, if any, must be a pair (i, j) of naturals
        less than self.rank, with self.dimension[i] == self.dimension[j].  If
        either is less than len(shuffle) the entry in shuffle at this index
        must be None.  No two pairs may have an entry in common.  Each None
        entry in shuffle must appear in exactly one pair.

        If any pair includes an index greater than or equal to shuffle's
        length, shuffle is implicitly padded with None entries for each such
        index in a pair and an order-preserving continuation of its
        permutation.  Thus permutrace((None, 1, 0), (0, 5), (3, 7)) uses
        (None, 1, 0, None, 2, None, 3, None) as its implicit extended shuffle.

        Has the same effect as tau (q.v.) but with a string, unique for each
        pair (i, j), used in place of shuffle[i] and shuffle[j], to make a
        suitable pattern.  Each pair indicates two ranks to trace out; the
        remaining ranks are permuted according to shuffle.\n"""

        bad = [ p for p in pairs if len(p) != 2 ]
        if bad:
            raise ValueError("Each index pair's length should be two", bad)

        bad = filter(lambda (i, j), d=self.dimension: d[i] != d[j], pairs)
        if bad: raise ValueError(
            "Traced ranks must have equal dimension", bad, self.dimension)

        bok = set()
        for i in reduce(lambda x, y: x+y, pairs, ()):
            if i < 0 or (
                i < len(shuffle) and shuffle[i] is not None
                ) or i in bok:
                bad.append(i)
            bok.add(i)
        if bad: raise ValueError(
            "Bad or repeat index in tracing pairs", bad, tuple(bok), shuffle)

        n = [ x for x in shuffle if x is not None ]
        if n: n = range(1 + max(n))
        for i, a in enumerate(shuffle):
            if a is None:
                if i not in bok: raise ValueError(
                    "Rank neither traced nor permuted", i, shuffle, pairs)
            else:
                if n[a] is None: bad.append[a]
                n[a] = None
        if bad:
            raise ValueError("Not a permutation", bad, shuffle)
        bad = [ i for i in n if i is not None ]
        if bad:
            raise ValueError("Incomplete permutation", bad, shuffle)

        return self.__trace_permute(shuffle, pairs)

    # Implementation of __trace_permute:
    def __listify(self):
        if self.rank < 2: return list(self)
        return [ x.__listify() for x in self ]

    @classmethod
    def __ranger(cls, ms):
        """Iterator over index tuples.

        Single argument, ms, is an iterator over dimensions.  Yields every
        tuple s, of the same length as ms, for which, for every 0 <= i <
        len(ms), 0 <= s[i] < ms[i].\n"""

        try: m = ms.next()
        except StopIteration: yield ()
        else:
            for s in cls.__ranger(ms):
                i = m
                while i > 0:
                    i -= 1
                    yield (i,) + s

    def __summands(self, tmpl, pairs):
        try: i, j = pairs.next()
        except StopIteration: yield tuple(tmpl)
        else:
            assert self.dimension[i] == self.dimension[j]
            assert tmpl[i] is None is tmpl[j]
            if i > j: i, j = j, i
            for s in self.__summands(tmpl, pairs):
                m = self.dimension[i]
                while m > 0:
                    m -= 1
                    yield s[:i] + (m,) + s[i+1:j] + (m,) + s[j+1:]

    def __total(self, tmpl, pairs):
        es = self.__summands(tmpl, iter(pairs))
        tot = self[es.next()]
        for e in es: tot += self[e]
        return tot

    def setcell(grid, key, val): # tool function for __trace_permute
        old = key.next()
        for it in key: grid, old = grid[old], it
        assert grid[old] is None
        grid[old] = val

    def __trace_permute(self, shuffle, pairs, store=setcell):
        """Implementation of tau and permutrace, q.v.

        Takes two arguments, a permutation optionally padded with None
        entries and a sequence of pairs of indices to trace.\n"""

        ns, i = [ x for x in shuffle if x is not None ], len(shuffle)
        if ns: j = max(ns) + 1
        else: j = 0

        if pairs:
            # Pad shuffle so that every pair's indices have None in it:
            ns = reduce(lambda x, y: x+y, pairs)
            n, shuffle = max(ns), list(shuffle)
            while i <= n:
                if i in ns: shuffle.append(None)
                else:
                    shuffle.append(j)
                    j += 1
                i += 1
        shuffle = tuple(shuffle) # we're done modifying it

        # Construct reverse-lookup for shuffle:
        rev = [ None ] * j
        while i > 0:
            i -= 1
            if shuffle[i] is not None:
                assert rev[shuffle[i]] is None
                rev[shuffle[i]] = i
        assert None not in rev, (rev, shuffle)

        # Perform contraction:
        slab, total = (None,) * len(shuffle), self.__total
        if rev:
            dim = tuple( self.dimension[r] for r in rev )
            grid = self.xerox(dim, None).__listify()

            for s in self.__ranger(iter(dim)):
                tmpl = list(slab)
                for i, a in enumerate(rev): tmpl[a] = s[i]
                store(grid, iter(s), total(tmpl, pairs))

            # Don't pass dim; may lack a few late entries:
            return self.fromSeq(grid)

        return total(slab, pairs)
    del setcell

    # Further implementation details
    from study.maths.ratio import Rational
    @staticmethod
    def __rat_over(n, R=Rational): return R(1, n)
    del Rational

    from study.maths.permute import Permutation
    @staticmethod
    def __perm_average(cls, dims, ranks, func,
                       gen=Permutation.fixed):
        """Average over permutations, needed by (anti-)symmetrise()
        """
        if ranks is None: ranks = tuple(range(len(dims)))
        else: ranks = tuple(ranks)
        if len(set(dims[i] for i in ranks)) != 1:
            raise ValueError(
                'Can only average over permutations of ranks of equal dimension',
                ranks, dims)

        if ranks: n = max(ranks) + 1
        else: n = 0
        pat = range(n)
        for i in ranks: pat[i] = None
        es = gen(n, pat)

        ans, n = func(es.next()), 1
        for e in es: ans, n = ans + func(e), n + 1
        return ans * cls.__rat_over(n)

    del Permutation

    def tail_twist(index, key, old, flip): # tool-function; del'd later
        """Performs key-munging needed by __antisymmetric().

        Arguments:
          index -- any in range(dim) but not in key
          key -- a sub-set of range(dim), in increasing order
          old -- maps keys one longer than key to tensors
          flip -- parity toggle; 0 for the positive result, 1 for negative

        Used when computing key's pair of tensors from old, which maps all keys
        longer than key by one to their relevant pairs of tensors; see the
        Theory section of .__antisymmetric()'s doc-string.  Pass 0 as flip for
        the primary value for key and 1 for the negated value.\n"""
        n = len([x for x in key if x < index])
        pre, post = key[:n], key[n:]
        assert all(x > index for x in post) # key is in increasing order
        n = len(post) + flip
        # assert: old has the key looked up here (and maps it to a twople):
        return old[pre + (index,) + post][n % 2]

    @staticmethod
    def __antisymmetric(dim, zero, bok, vec, munge=tail_twist):
        """Gory implementation of antisymmetric().

        Required arguments:
          dim -- dimension of space; also rank of final tensor
          zero -- the zero of the kind matching scale
          bok -- initial mapping from full-length key to (scale, -scale).
          vec -- constructor for vectors

        == Theory ==

        We'll create the returned tensor, its dim entries, each of which has dim
        entries, and so on, for a total of sum(dim**i for i in range(dim)),
        which is (dim**dim - 1) / (dim - 1); if we create this the most obvious
        way (or, as it happens, if we multiply the answer by a scalar), this is
        how many tensor objects we'll create.  However, many of these objects
        duplicate one another - most obviously, most of them are zero, of a
        relevant rank - so we can do better by reusing them.

        For each n in range(1+dim), there are chose(dim, n) distinct sets of n
        entries in range(dim); each ordering of each such set is a key with
        which we can index our answer to get a non-zero entry, of rank
        dim-n.  For n > 1, half of the available orderings give one entry, the
        other half give its negation.  Any valid key not of this form gives zero
        as answer, so we have 1 + 2 * chose(dim, n) distinct entries of this
        rank.  For n = 1, chose(dim, n) = dim; for n = 0, chose(dim, n) = 1; in
        each of these cases, there is only one ordering and we don't use the
        zero tensor (making special handling of these two ranks beneficial, as
        exploited below), so we only have chose(dim, n) dictinct entries.  For n
        = dim, the results of indexing are scale, -scale and 0, which aren't
        tensor objects (well, the can be, but I'm ignoring that
        possibility).  So, for dim > 1, the number of tensor objects we need is:

            1 + dim + sum(1 + 2*chose(dim, n) for n in range(2, dim)]
          = 1 + dim + dim - 2 + 2 * sum(chose(dim, n) for n in range(2, dim))
          = 2 * dim - 1 + 2 * (sum(chose(dim, n) for n in range(dim+1)) - 1 - dim - 1)

        since chose(dim, 0), chose(dim, 1) and chose(dim, dim) are 1, dim and 1,
        respectively.  The given sum of chose(dim, n) is just 2**dim (it ranges
        over all values of n for which chose(dim, n) can be non-zero; so is just
        the number of subsets, of any size, of a set of size dim), so we are
        left with 2**(dim+1) - 5 tensor objects that we need to create.  This
        grows much less rapidly than (dim**dim - 1) / (dim - 1), as dim grows;
        and is already bigger at dim = 3.

        Aside from ranks dim and dim - 1, and a zero of each rank, our distinct
        entries come in pairs, differing only in sign; and there is exactly one
        such pair for each partition of range(dim+1) into two subsets.  One of
        those subsets defines the keys we index our result with to obtain the
        pair of entries; swapping two entries in such a key switches between the
        two possibilities for the entry.  The other subset provides the keys by
        which one can index either entry to get a non-zero scalar result (or, at
        least, result of scale's rank); half the orderings of this subset give
        scale, the other half give -scale.

        We can represent a partition by either of the two subsets of range(dim);
        the other subset is implied, by virtue of being its complement.  We can
        represent a subset by the tuple in which its members appear in
        increasing order.  We can build a mapping from such tuples to the
        entries associated with the partition they represent.  Since the entries
        associated with shorter keys are built out of those with longer keys, we
        can in fact start with a mapping from longer keys and iteratively use it
        to build mappings with shorter keys, discarding each mapping once the
        next is available (and its values are using the previous mapping's
        values as entries).

        One side of each partition is used as index into our result tensor, to
        obtain the two tensor entries associated with this indexing; swapping
        two of the indices in this key switches between the two entries.  So use
        (the increasing-order tuple of) this side of the partition, which shall
        work as a key into our final tensor, as key in our transient mappings;
        and map it to the twople whose first entry is the tensor our final
        tensor shall map it to, with its negation as second entry.

        We thus start with one key, tuple(range(dim)), mapped to the pair
        (scale, -scale); and with 0 * scale as zero (two of the parameters
        passed to this method).  From each such mapping and associated zero, we
        then build the equivalent for keys one index shorter, with values of
        rank one higher; the entries associated with these are vectors of the
        previous mapping's entries.  For each old key, deleting one entry from
        it yields some new key: but each new key can (at least potentially)
        arise from several old keys.  To avoid duplication, traverse each key
        from its bottom upwards until it hits a gap from an earlier iteration's
        deletion: any later deletion than this shall have appened to a peer of
        the key being traversed, that didn't delete the gap we've just hit, so
        *its* traversal shall include deletion of this gap.  (The ability to do
        this is what makes ordered tuples better than feozensets as keys.)

        So, given a key, an index to delete from it and the old mapping, we need
        to compute the values, for the reduced key, in the new mapping.  Let the
        index be m and the portions of key before and after it A, Z, so that
        A+(m,)+Z is the old key, of length n, and A+Z is the new key.  Our
        duplication-avoidance ensures m = len(A), so len(Z) = n - m - 1.  With R
        as the final result, we want to compute S = R[A+Z] and its
        negative.  For any k in A+Z, S[k] is zero, since it has a repeated
        index.  For any other k in range(dim), including k = m, S[k] is
        non-zero; and it's R[A+Z+(k,)], which our old mapping can tell us,
        exploiting the fact that A+Z is in increasing order, so cycling k and
        the entries in A+Z greater than it one step shall give a key in
        increasing order.  This cycle is even or odd precisely as the number of
        entries in A+Z greater than k is even or odd; if even, S[k] is the
        thus-cycled key's primary value; otherwise, its negated value.  This
        lets us select the values from the old mapping, that are needed in order
        to make the values for the new (and likewise their negations, by taking
        the opposite choice in each case).

        We can thus obtain the needed mappings for progressively shorter keys;
        and can trivially compute the zero that each rank needs to fill in its
        gaps.  We could repeat this until we're left with a mapping whose sole
        key is () and return the primary value it maps this to; however, we can
        be more efficient.  Clearly we don't need to compute the negated version
        of the returned value, or the zero of its rank.  Each entry in the final
        value (taken from the previous iteration's mapping) is non-zero, so we
        didn't need the previous iteration's zero, either.  Furthermore, as this
        penultimate iteration's mapping's keys have length one, hence only one
        possible ordering, we don't need their negated values.  Thus it's
        possible to unroll the last two iterations and deliver a result straight
        off the mapping whose keys have length two.\n"""

        assert dim > 1
        assert len(bok.keys()) == 1 and bok.keys()[0] == tuple(range(dim))
        assert len(bok.values()[0]) == 2

        n = dim
        while n > 2:
            kob = {} # new mapping
            for key in bok.iterkeys():
                assert len(key) == n
                for i, m in enumerate(key):
                    if i < m: break # avoid duplication
                    assert i == m
                    yek = key[:i] + key[i+1:] # new mapping's key, skipping m
                    assert not kob.has_key(yek) # we avoided duplication
                    kob[yek] = tuple(vec(zero if k in yek else munge(k, yek, bok, i)
                                         for k in range(dim))
                                     for i in (0, 1))

            n -= 1
            # Check duplication-avoidance isn't over-zealous:
            # len(bok) is chose(dim, n+1) = dim! / (n+1)! / (dim-n-1)!
            # len(kob) is chose(dim, n) = dim! / n! / (dim-n)!
            assert (n + 1) * len(bok) == (dim - n) * len(kob)

            # Replace the old with the new:
            zero, bok = vec((zero,) * dim), kob

        assert all(len(key) == 2 for key in bok.iterkeys())
        assert 2 * len(bok) == (dim - 1) * dim
        # No need for negated values or zeros in last two ranks:
        return vec(vec(zero if k == i else munge(k, (i,), bok, 0)
                       for k in range(dim))
                   for i in range(dim))

    del tail_twist

Tensor = Vector # alias
del lazyprop, Tuple

class Namely (Vector):
    """A vector with more emphasis on the names of its components.

    Derived classes need to define _component_names_ to a sequence (preferably a
    tuple; all manner of nonsense would happen if it changed during the lifetime
    of an object) of names of components.  These serve as attribute names for
    the components of the vector and can be used as keyword names for arguments
    to the constructor.  Components can be passed simply as positional arguments
    to the constructor (not as entries in a list passed as first argument,
    unlike Vector); any not supplied either positionally or as keywords default
    to zero.  The repr() of the vector uses whichever of these forms is tersest
    (so, unless many components are zero, usually the positional form); str() is
    as for Vector.

    Based on Vector, so supports everything it does, although the results may
    prove a bit odd in some cases.\n"""

    # Data to over-ride in derived classes:
    _component_names_ = () # names of components, in order
    _component_aliases_ = () # .items() of { alias: name } mapping

    __upnew = Vector.__new__
    def __new__(cls, *args, **kw):
        """Create the instance.

        See class doc-string for details.\n"""
        if len(args) > len(cls._component_names_):
            raise ValueError('Too many components in vector', args, cls._component_names_)

        for nom, name in cls._component_aliases_:
            try: val = kw[nom]
            except KeyError: pass
            else:
                if kw.has_key(name):
                    raise ValueError('Name duplicated by alias',
                                     (name, kw[name]), (nom, val))
                del kw[nom]
                kw[name] = val

        for nom in cls._component_names_[len(args):]:
            try: val = kw[nom]
            except KeyError: val = None
            else: del kw[nom]
            args += (val,)
        if kw:
            raise ValueError('Extraneous keyword arguments specified', kw)

        good = [ val for val in args if isinstance(val, Vector) ]
        bad = [ val for val in args if val is not None and not isinstance(val, Vector) ]
        if bad:
            pass # TODO: erm ... what's this about ?

        # Need an "is" check, not "None in args" which is an == check, in case
        # some entry in args doesn't like to test == None:
        if any(x is None for x in args):
            given = [ val for val in args if val is not None ]
            if given: # use zero of same rank as given values:
                zero = given[0] * 0
                assert all(val * 0 == zero for val in given[1:])
            else: zero = 0 # ddefault to scalar zero
            # replace each None with zero:
            args = (zero if val is None else val for val in args)

        return cls.__upnew(cls, args)

    @classmethod
    def _vector_(cls, seq): return cls(*tuple(seq))

    def __repr__(self):
        seq, byname, index = [], [], []
        for val, nom in zip(self, self._component_names_):
            index.append(len(byname))
            seq.append(repr(val))
            if val: byname.append(nom + '=' + seq[-1])

        if len(byname) < len(seq):
            # Find where to switch from positional to keyword form, for shortest text:
            i = len(index)
            while i:
                i = len([j for j in index if j < index[i-1]])
                assert i == 0 or self[i-1] # i.e. we have to mention self[i-1]
                if sum(2 + len(x) for x in seq[i:]) > sum(2 + len(x) for x in byname[index[i]:]):
                    seq = seq[:i] +byname[index[i]:]

        return self.__class__.__name__ + '(' + ', '.join(seq) + ')'

    def __getattr__(self, key):
        try: ind = self._component_names_.index(key)
        except ValueError: pass
        else: return self[ind]

        raise AttributeError(key)
