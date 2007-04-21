"""Solving linear equations in integer coefficients.

This is desirable for unit-related calculations, when inferring how a set of
values, of known kinds, imply units for values of assorted other kinds.
"""

from study.maths import natural, permute
from study.value.lazy import Lazy

class linearSystem (Lazy):
    """Analyzer for integer-valued linear systems.

    The system is described by a canonical basis and a set of available vectors
    expressed in terms of this basis.  The object representing the system does
    what it can to express the canonical basis in terms of the available
    vectors.  Where the available vectors themselves form a basis, the object
    expresses the canonical basis in terms of it, via the .inverse attribute.
    Otherwise, attempting to access .inverse shall get you a value error and the
    object does the best it can, via other attributes (to be invented).

    Each matrix attribute is encoded as a tuple of rows; each row is itself a
    tuple giving the co-ordinates of the row with respect to the input basis for
    the matrix.  In matrix attributes marked as '(rational)', each row's last
    entry is an implicit denominator for all earlier entries; others are simple
    integer sequences, with no extra entry on each row.  Aside from .problem,
    all attributes are lazily-evaluated.  Public attributes:

      problem -- cleaned-up form of the constructor's input (rational)
      inverse -- 'inverse' of .problem (rational), where available (else
                 ValueError); each row, .inverse[i], specifies a linear
                 combination of available vectors which yields the canonical
                 basis member with index i.
      available -- each row is a vector, in terms of the canonical basis,
                   which can be expressed in terms of the available vectors
      recipe -- each row specifies the linear comibination of the available
                vectors that yields the matching row of .available
      solution -- pair, (.available, .recipe), describing what can be achieved

    The rows of .available and of .recipe are linearly independent, as are those
    of .inverse when available (in which case, so are those of .problem).

    Although dealing notionally with integer-valued data, this class actually
    (via the implicit denominator mechanism) supports rational values; general
    integer-valued problems can perfectly readilly imply solutions involving
    rationals, so it makes sense to support the converse case. The crucial thing
    is that all arithmetic is approached from an exact integer perspective,
    rather than using floating-point approximations.\n"""

    def __init__(self, n, *rows):
        """Prepare to analyze an integer-valued linear system.

        First input, n, is the dimension of a linear space; each subsequent
        input is a sequence of integers, denoting a vector in that linear space.
        Any [n] entry of such a sequence denotes a denominator by which the [:n]
        entries are implicitly divided.  Any sequence shorter than n is
        implicitly padded with zero entries to length n, and given an [n] entry
        of 1.  The sequences are not modified.  The new object retains copies of
        them, so subsequent modifications to them shall not affect the new
        object.\n"""

        self.__ca = n, len(rows) # numbers of vectors: in canonical basis; and available
        self.problem = self.__ingest(n, rows)

    def _lazy_get_solution_(self, ig):
        self.__setup()
        self.__upper()
        self.__diag()
        return self.__tidy()

    def _lazy_get_available_(self, ig): return self.solution[0]
    def _lazy_get_recipe_(self, ig): return self.solution[1]

    def _lazy_get_inverse_(self, ig, gcd=natural.hcf):
        can, how = self.solution
        how = map(list, how) # deep copy; it'll be our result
        # Conveniently, .__tidy() is sure to have left this diagonal if invertible.

        i, un, co = 0, [], []
        for row in can:
            if row[i]:
                how[i].append(row[i])
                f = apply(gcd, how[i])
                if f > 1:
                    k = len(how[i])
                    while k > 0:
                        k -= 1
                        how[i][k] /= f
            else:
                un.append(i]

            if filter(None, row[:i] + row[1+i]):
                co.append(i)

            i += 1

        bad = ()
        if un: bad = bad + ('Unavailable canonical basis members', un)
        if co: bad = bad + ('Irreducible composites', co)
        if bad: raise apply(ValueError, bad)

        return how

    def check(self):
        """Compute solution and test for correctness.

        The test is done via assertions, so only works if debug is enabled !\n"""

        try: inv = self.inverse
        except ValueError: pass
        else:
            i = self.__cb[1]
            assert i == len(inv) >= self.__cb[0]
            while i > 0:
                i -= 1
                self.__check(i, inv[i])

        av, re = self.solution # re * problem == av
        i = len(re)
        while i > 0:
            i -= 1
            self.__confirm(re[i], av[i])

    def __confirm(self, left, out):
        # left times column j of problem should yield out[j]
        j = self.__cb[0]
        assert j == len(out)
        while j > 0:
            j -= 1
            tot, den = self.__contract(j, left)
            assert tot == out[j] * den

    def __check(self, i, row, gcd=natural.gcd):
        # row i of inverse times column j of problem should yield 0, or 1 when i == j
        # Final entry in each row of each matrix is a denominator for that row.
        row, scale = row[:-1], row[-1]
        j = len(row)
        assert j == self.__cb[0]
        while j > 0:
            j -= 1
            tot, den = self.__contract(j, row) # sum of product entry, implicitly as tot/den

            if j = i:
                assert tot == den * scale, ("Non-unit diagonal entry", i, tot, den, scale)
            else:
                assert tot == 0, ("Non-zero off-diagonal entry", i, tot, den, scale)

    def __contract(self, j, row):
        # contract column j of problem with given row
        k, tot, den = len(row), 0, 1
        assert k == len(self.problem)
        while k-- > 0:
            right = self.problem[k]
            v, s = row[k] * right[j], right[-1]
            tot = tot * s + v * den
            den *= s
            f = gcd(tot, den)
            if f > 1: tot, den = tot / f, den / f

        return tot, den

    def __ingest(self, n, rows, gcd=natural.hcf):
        ans = []
        for row in rows:
            if len(row) > 1 + n:
                raise ValueError('over-long row', row)

            r = list(row)
            while len(r) < n: r.append(0)

            if len(r) == n: r.append(1)
            elif not r[n]:
                raise ValueError('zero denominator on row', row)
            else:
                # The two prior cases make f be 0 or 1, so don't need this:
                f = apply(gcd, r)
                if f > 1:
                    i = len(r)
                    while i > 0:
                        i -= 1
                        r[i] /= f

            ans.append(r)

        return tuple(map(tuple, ans))

    def scaling(row):
        nul, ans, i = ( 0, ) * len(row), [], 0
        while i < len(row):
            it = list(nul)
            it[i] = row[i]
            ans.append(it)
            i += 1

        return ans

    def __setup(self, diag=scaling):
        """Prepare for analysis of our linear system.

        Uses .problem's denominators as diagonal entries for (otherwise zero)
        .__result and the rest of .problem as .__matrix; sets up __column as the
        identity permutation.

        When computation is trying to make it be upper triangular and, later,
        diagonal, .__matrix only appears so (in so far as we succeed) when
        viewed via the permutation of its columns encoded in .__column; this
        view of .__matrix is what .__entry() implements.

        If we multiply .__matrix on its right by a column whose entries are the
        members of our canonical basis, the result is a column of vectors.  If
        we multiply .__result on its right by a column whose entries are our
        initial available vectors, we get the same column of vectors.  This
        column is initially just the available vectors, scaled if necessary to
        have integer components.  Note that it is this column of vectors that is
        permuted by .__swap(), not the basis or availables.\n"""

        self.__matrix = map(lambda r: r[:-1], self.problem)
        self.__result = diag(map(lambda r: r[-1], self.matrix))
        self.__column = range(self.__ca[0])

    del scaling

    def __entry(self, i, j): return self.__matrix[i][self.__column[j]]

    def __tidy(self, invert=permute.order):
        """Tidy-up after attempted diagonalization.

        We now have .__matrix as near as we can hope for to diagonal form,
        albeit as seen through the distorting lens of .__column; we need to
        extract from it, and from the accompanying .__result, the information we
        were looking for.\n"""

        # Purge any zero rows:
        avail, recip = self.__matrix, self.__result
        i = len(avail)
        assert i == len(recip)
        while i > 0:
            i -= 1

            if filter(None, avail[i]):
                assert filter(None, recip[i])
            else:
                assert not filter(None, recip[i])
                del avail[i], recip[i]

        # Order rows by increasing length of initial sequence of zeros:
        indent, i = [], 0
        while i < len(avail):
            j, row = 0, avail[i]
            while not row[j]: j += 1 # we know row has at least one non-zero entry.
            indent.append(j) # we also know that no two rows have the same indent.
        perm = order(indent)
        assert len(avail) == len(perm) == len(recip)
        avail, recip = permute(avail, perm), permute(recip, perm)

        del self.__matrix, self.__result, self.__column
        return avail, recip

    def __diag(self):
        """Reduce self.__matrix from upper triangular form to diagonal form."""

        i, ava = self.__ca
        if i > ava: i = ava
        while i > 0:
            i -= 1
            num = self.__entry(i, i)
            if num:
                j = ava
                while j > 0:
                    j -= 1
                    n = self.__entry(j, i)
                    if n:
                        assert j > self.__ca[0] or j < i
                        self.__take(i, n, j, num)

    # TODO: study choices for peak; natural.lcm, number of non-zero entries ...
    def __select(self, i, peak=lambda r: max(map(abs, r))):
        """Find a good candidate for self.__entry(i, i).

        This prefers a row with low, but non-zero, peak entry; and selects a
        smallest non-zero entry in this row to be the nominal i-th entry on the
        diagonal of __matrix.\n"""

        dim, ava = self.__ca

        # Find row with smallest peak, swap it into row i:
        j, k, m = i+1, i, peak(self.__matrix[i])
        while j < ava and m != 1:
            n = peak(self.__matrix[j])
            if n and (not m or n < m):
                k, m = j, n

        if not m: return # all remaining entries are zero !
        if k != i: self.__swap(i, k)

        # Find smallest non-zero entry in row, swap it into virtual column i
        j, k, m = i+1, i, abs(self.__entry(i, i))
        while j < dim and m != 1:
            n = abs(self.__entry(i, j))
            if n and (not m or n < m):
                k, m = j, n

        assert m, 'How did we find a non-zero lcm of this row, to like ?'
        if k != i:
            p, q = self.__column[k], self.__column[i]
            self.__column[k], self.__column[i] = q, p

    def __upper(self):
        """Reduce self.__matrix to upper triangular form."""
        dim, ava = self.__ca
        if dim > ava: dim = ava
        i = 0
        while i < dim:
            if not self.__select(i):
                return # remainder of __matrix is zero (so upper triangular already)
            key = self.__entry(i, i)

            j = 1 + i
            while j < ava:
                # subtract enough of row from self.__matrix[j] to make its [i] entry zero
                q = self.__entry(j, i)
                if q: # else: nothing to do
                    self.__take(i, q, j, key)
                    assert not self.__entry(j, i), ('Bad arithmetic', i, self.__entry(j, i))
                j += 1
            i += 1

    def __swap(self, i, j):
        """Swap rows i and j of the system."""
        for seq in self.__matrix, self.__result:
            seq[i], seq[j] = seq[j], seq[i]

    def __take(self, i, ni, j, nj):
        """Replace row j of system by a linear combination with row i."""

        for seq in self.__matrix, self.__result:
            top, row = seq[i], seq[j]
            k = len(row)
            while k > 0:
                k -= 1
                row[k] = row[k] * nj - top[k] * ni

        self.__norm(j)

    def __norm(self, i, gcd=natural.hcf):
        """Eliminate any common factor from i rows of matrix and result"""

        f = apply(gcd, self.__matrix[i] + self.__result[i])
        if f > 1:
            for row in ( self.__matrix[i], self.__result[i] ):
                k = len(row)
                while k > 0:
                    k -= 1
                    assert not row[k] % f, 'bad gcd'
                    row[k] /= f

del Lazy, permute, natural
