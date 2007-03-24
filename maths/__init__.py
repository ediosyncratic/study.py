"""Assorted mathematical tools and toys.

Polynomials:
  Legendre -- spherical hamonics and Legendre polynomials
  multiangle -- expressing sin and cos, of multiples of an angle, as polynomials
                in those of the angle
  multinomial -- polynomials in several free variables
  polynomial -- polynomials in one free variable

Combinatorics:
  Fibonacci -- computing the eponymous sequence
  Pascal -- factorials and the eponymous triangle (see also permute)
  stirling -- deploying Stirling's formula

Permutations:
  permute -- interpreting lists of integers as permutations
  queens -- solving the 'eight queens' problem by iterating over permutations

Distributions and probabilities:
  gamma -- the Gamma distribution (see also stirling)
  gauss -- the Gaussian (bell-shaped) distribution
  variate -- generating chaotic samples from assorted distributions

Solving equations:
  cardan -- finds roots of polynomials of degree at most three
  root -- searching for zeros of a ({reals}::{reals}) function
  search -- similar, but in two dimensions (represented by complex)

Miscellaneous:
  graph -- the find/unite algorithm and description of a graph
  integrate -- perform adequate approximations to integrating a function
  natural -- the natural numbers, and tools to work with types kindred to them
  primes -- factorisation and a disk-cached lazy list of all primes
  pythagorean -- integer-sided right-angle triangles
  ratio -- representing exact fractions and approximating numbers with them

$Id: __init__.py,v 1.1 2007-03-24 15:57:38 eddy Exp $
"""
from value.quantity import Quantity

goldenratio = Quantity((1 + 5.**.5) / 2,
                       doc = """The golden ratio.

This is the positive solution to the quadratic equation x*x = x+1; divide -1 by
it to get the negative solution.  One can re-write the equation as (2*x-1)**2 =
4*x*x -4*x +1 = 4*(x*x-x) + 1 = 5, whence the solutions are (1 +/- 5**.5)/2.
""")

assert goldenratio**2 == goldenratio+1

del Quantity

# in memoriam:
Ramanujan = 7 * 13 * 19
assert 12**3 + 1**3 == Ramanujan == 10**3 + 9**3
# and no smaller natural is a sum of two natural cubes in two distinct ways.
