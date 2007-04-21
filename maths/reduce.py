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
            self.__check(i, left)

    def __check(self, i, left, gcd=natural.gcd):
        j = len(self.matrix)
        row, scale = left[:j], left[j]
        # row i of left times column j of matrix should yield i == j
        while j > 0:
            j -= 1
            tot, den = 0, 1 # sum of product entry, implicitly as tot/den
            k = len(self.matrix)
            while k-- > 0:
                right = matrix[k]
                v, s = row[k] * right[j], right[-1]
                d = gcd(s, den)
                tot = tot * s / d + v * den / d
                den *= s/d

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

        self.__matrix = ans
        return tuple(map(tuple, ans))

    def identity(n):
        nul, ans, i = ( 0, ) * n + ( 1, ), [], 0
        while i < n:
            it = list(nul)
            it[i] = 1
            ans.append(it)
            i += 1

        return ans

    def __solve(self, unit=identity):
        # Should actually use __matrix's denominators as diagonal entries.
        # This would eliminate most of the denominator complications ...
        self.__result = unit(len(self.__matrix))
        self.__upper()
        self.__diag()
        self.__tidy()

    del identity

    def __tidy(self, gcd=natural.gcd):
        i = len(self.matrix)
        while i > 0:
            i -= 1
            row = self.__matrix[i]
            assert not filter(None, row[:i] + row[i+1:-1])
            num, den = row[i], row[-1]
            row = self.__result[i]

            j = len(self.matrix)
            f = row[j] = row[j] * num
            while j > 0:
                j -= 1
                row[j] *= den
                f = gcd(f, row[j])

            if f > 1:
                j = len(row)
                while j > 0:
                    j -= 1
                    row[j] /= f

        del self.__matrix # we should no longer be referencing it

    def __diag(self):
        """Reduce self.__matrix from upper triangular form to diagonal form."""

        i = len(self.matrix)
        zeros = []
        while i > 0:
            i -= 1
            row = self.__matrix[i]
            num, den = row[i], row[-1]
            if num:
                j = i
                while j > 0:
                    j -= 1
                    row = self.__matrix[j]
                    n, d = row[i], row[-1]
                    if n:
                        self.__take(self.__matrix, i, n, d, j, num, den)
                        self.__take(self.__result, i, n, d, j, num, den)
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
            j, k, n, d = i+1, i, self.__matrix[i][i], self.__matrix[i][-1]

            while j < len(self.matrix):
                m, e = self.__matrix[j][i], self.__matrix[j][-1]
                if m and (not n or abs(m*d) < abs(n*e)):
                    k, n, d = j, m, e
                j += 1

            if k != i:
                self.__swap(self.__matrix, i, k)
                self.__swap(self.__result, i, k)
            row = self.__matrix[i]
            key, scale = row[i], row[-1]

            j = 1 + i
            while j < len(self.matrix):
                # subtract enough of row from self.__matrix[j] to make its [i] entry zero
                row = self.__matrix[j]
                q, s = row[i], row[-1]
                if q: # else: nothing to do
                    self.__take(self.__matrix, i, q, s, j, key, scale)
                    self.__take(self.__result, i, q, s, j, key, scale)
                    assert not row[i], ('Bad arithmetic', i, row[i])
                j += 1
            i += 1

    def __swap(self, matrix, i, j):
        """Swap rows i and j of the given matrix"""
        a, b = matrix[i], matrix[j]
        matrix[i], matrix[j] = b, a

    def __take(self, matrix, i, ni, di, j, nj, dj, gcd=natural.gcd):
        """Replace row j of matrix by combining with row i."""

        row = matrix[i]
        top, scale = row[:-1], row[-1]
        row = matrix[j]
        k, s = len(self.matrix), row[-1]
        f = row[-1] = row[-1] * di * dj * scale
        while k > 0:
            k -= 1
            row[k] = row[k] * nj * di * scale - top[k] * ni * dj * s
            f = gcd(f, row[k])

        if f > 1:
            k = len(row)
            while k > 0:
                k -= 1
                assert not row[k] % f, 'bad gcd'
                row[k] /= f
