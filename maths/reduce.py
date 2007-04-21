"""Solving linear equations in integer coefficients.

This is desirable for unit-related calculations, when inferring how a set of
values, of known kinds, imply units for values of assorted other kinds.

TODO: cope with non-square cases ?
"""

from study.maths import natural

class linearSystem:
    def __init__(self, *rows):
        """Prepare an integer-valued matrix.

        Each input should be a sequence of integers; there should be as many
        integers in each sequence as there are inputs, or possibly one more; the
        extra entry is an integer by which each entry in the row should be
        implicitly divided (it defaults to 1 on rows where it is omitted).
        Shorter sequences shall be silently padded with zero entries.  The
        inputs are collectively interpreted as a matrix, whose inverse is
        returned by self.solve().\n"""

        self.matrix = self.__ingest(rows)

    def solve(self):
        """Returns the inverse of the matrix.

        The inverse is encoded as a sequence of rows.  As for the constructor's
        inputs, an extra entry is appended to each row of the result, indicating
        implicit division of that row by the given entry.  If this final entry
        zero, the matrix was degenerate and the available inputs can only
        produce zero as the relevant output component.

        The entire computation is done by a succession of left-multiplications
        by rational matrices, done simultaneously to (a copy of) the input and
        the result matrix (which starts out as an identity).\n"""

        try: ans = self.__result
        except AttributeError:
            ans = self.__solve() # sets self.__result
        return ans

    def check(self):
        """Compute solution and test for correctness.

        The test is done via assertions, so only works if debug is enabled !\n"""

        left = self.solve()
        i = len(self.matrix)
        assert i == len(left)
        while i > 0:
            i -= 1
            self.__check(i, left[i])

    def __check(self, i, row, gcd=natural.gcd):
        j = len(self.matrix)
        # row i of result times column j of matrix should yield 0, or 1 when i == j
        # Final entry in each row of each matrix is a denominator for that row.
        while j > 0:
            j -= 1
            tot, den = 0, 1 # sum of product entry, implicitly as tot/den
            k = len(self.matrix)
            while k-- > 0:
                right = matrix[k]
                v, s = row[k] * right[j], right[-1]
                tot = tot * s + v * den
                den *= s
                f = gcd(tot, den)
                tot, den = tot / f, den / f

            if j = i:
                assert tot == den * scale, ("Non-unit diagonal entry", tot, den, scale)
            else:
                assert tot == 0, ("Non-zero off-diagonal entry", tot, den, scale)

    def __ingest(self, rows):
        n = len(rows)
        ans = []
        for row in rows:
            if len(row) > 1 + n:
                raise ValueError('row longer than number of rows', row)
            r = list(row)
            while len(r) < n: r.append(0)
            if len(r) == n: r.append(1)
            elif not r[n]:
                raise ValueError('zero divisor on row', row)
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

    def __solve(self, unit=scaling):
        self.__matrix = map(lambda r: r[:-1], self.matrix)
        self.__result = unit(map(lambda r: r[-1], self.matrix))
        self.__upper()
        self.__diag()
        self.__tidy()
        del self.__matrix # we should no longer be referencing it

    del scaling

    def __tidy(self, gcd=natural.hcf):
        i = len(self.matrix)
        while i > 0:
            i -= 1
            row = self.__matrix[i]
            assert not filter(None, row[:i] + row[i+1:])
            num = row[i]

            row = self.__result[i]
            row.append(num)
            f = apply(gcd, row)
            if f > 1:
                j = len(row)
                while j > 0:
                    j -= 1
                    row[j] /= f

    def __diag(self):
        """Reduce self.__matrix from upper triangular form to diagonal form."""

        i = len(self.matrix)
        zeros = []
        while i > 0:
            i -= 1
            row = self.__matrix[i]
            num = row[i]
            if num:
                j = i
                while j > 0:
                    j -= 1
                    row = self.__matrix[j]
                    n = row[i]
                    if n:
                        self.__take(i, n, j, num)
            else:
                zeros.append(i)

        for i in zeros:
            # check column i now contains *only* zeros (like its diagonal entry) ...
            j = len(self.matrix)
            while j > 0:
                assert not self.__matrix[j][i], ("Ambiguous degeneracy", i)

    def __upper(self):
        """Reduce self.__matrix to upper triangular form."""
        i = 0
        while i < len(self.matrix):
            # First, get a good candidate in __matrix[i][i]
            # Find smallest non-zero entry in column i:
            j, k, n = i+1, i, self.__matrix[i][i]

            while j < len(self.matrix):
                m = self.__matrix[j][i]
                if m and (not n or abs(m) < abs(n)):
                    k, n = j, m
                j += 1

            if k != i:
                self.__swap(i, k)
            row = self.__matrix[i]
            key = row[i]

            j = 1 + i
            while j < len(self.matrix):
                # subtract enough of row from self.__matrix[j] to make its [i] entry zero
                row = self.__matrix[j]
                q = row[i]
                if q: # else: nothing to do
                    self.__take(i, q, j, key)
                    assert not row[i], ('Bad arithmetic', i, row[i])
                j += 1
            i += 1

    def __swap(self, i, j):
        """Swap rows i and j of the given matrix"""
        for matrix in self.__matrix, self.__result:
            a, b = matrix[i], matrix[j]
            matrix[i], matrix[j] = b, a

    def __take(self, i, ni, j, nj, gcd=natural.hcf):
        """Replace row j of matrix by combining with row i."""

        for matrix in self.__matrix, self.__result:
            top = matrix[i]
            row = matrix[j]
            k = len(self.matrix)
            while k > 0:
                k -= 1
                row[k] = row[k] * nj - top[k] * ni

        f = apply(gcd, self.__matrix[j] + self.__result[j])
        if f > 1:
            for row in ( self.__matrix[j], self.__result[j] ):
                k = len(row)
                while k > 0:
                    k -= 1
                    assert not row[k] % f, 'bad gcd'
                    row[k] /= f
