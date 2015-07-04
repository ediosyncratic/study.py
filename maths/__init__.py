"""Assorted mathematical tools and toys.

Polynomials (see also cardan):
  Legendre -- spherical hamonics and Legendre polynomials
  multiangle -- expressing sin and cos, of multiples of an angle, as polynomials
                in those of the angle
  multinomial -- polynomials in several free variables
  polynomial -- polynomials in one free variable

Combinatorics (see also stats.stirling):
  Fibonacci -- computing the eponymous sequence
  Pascal -- factorials and the eponymous triangle (see also permute)

Permutations:
  permute -- interpreting lists of integers as permutations
  queens -- solving the 'eight queens' problem by iterating over permutations

Solving equations:
  cardan -- finds roots of polynomials of degree at most three
  golden -- the golden ratio and a generalisation of it
  pythagorean -- integer-sided right-angle triangles
  reduce -- linear equations in integer coefficients
  root -- searching for zeros of a ({reals}: :{reals}) function
  search -- similar, but in two dimensions (represented by complex)

Numeric types:
  buffersize -- how many bytes does it take to print an int in base ten ?
  natural -- the natural numbers, and tools to work with types kindred to them
  primes -- factorisation and a disk-cached lazy list of all primes
  ratio -- exact fractions and approximating numbers with them
  vector -- tensors and vectors

Miscellaneous:
  differentiate -- perform adequate approximations to differentiation
  graph -- the find/unite algorithm and description of a graph
  integrate -- perform adequate approximations to integrating a function
  interpolator -- approximating a distribution piecewise
  music -- exploring the theory of chords, scales and keys
  voronoi -- finding which of a set of points is closest within a space

See study.LICENSE for copyright and license information.
"""
# in memoriam:
Ramanujan = 7 * 13 * 19
assert 12**3 + 1**3 == Ramanujan == 10**3 + 9**3
# and no smaller natural is a sum of two natural cubes in two distinct ways.
