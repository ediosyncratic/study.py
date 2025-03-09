"""Calculating pi

Mathologer: https://www.youtube.com/watch?v=ypxKzWi-Bwg

The basic pi/4 = sum(: (-1)^i/(2.i +1) &larr;i |{naturals}) - derived from the
formula for arctan(1), obtained by integrating 1/(1 +power(2)) from 0 to 1 -
converges rather slowly, as the individual steps only shrink as O(1/n) after n
terms.  However, analysis of how early terms differ from the eventual limit
reveals an error estimate term that can be used to significantly speed the
convergence.

Specifically, for each positive natural n, pi is approximated by:
 4/1 -4/3 +4/5 -4/7 +... +4.(-1)^(n-1)/(2.n -1)
 +(-1)^n/(n +1^2/(4.n +2^2/(n +3^2/(4.n +4^2/(n ...
	+(n-1)^2/((1 +3.(n-1 mod 2)).n +n/(1 +3.(n mod 2)))...)))))

The first few layers of the final continued fraction were discovered
by Madhava, in Kerala, India, c.1340 to c.1425.
https://en.wikipedia.org/wiki/Madhava_of_Sangamagrama

That Wikipedia page also mentions pi/sqrt(12) being equal to
sum(: power(i, -1/3)/(2.i +1) &larr;i |{naturals}),
apparently obtained by a rearrangement of the arctan formula; this
doesn't converge as fast as the above, but does beat the truncated
versions of it Madhava was working with.
(There are faster-converging algorithms for computing digits of pi.)

Below, contail(n) computes the correction term and continued() iterates the
successive approximations, getting as accurate as 53-bit-mantissa arithmetic
allows within 10 iterations - reducing the error, on each iteration, by factors
that start around 46 and diminish to about 33 by the time the arithmetic hits
its limits.  More accurate results can be obtained by representing the result
as a rational; see continued()'s parameter.

If we just end in the (-1)^n/n correction,
s(n) = 4.sum(: power(i, -1)/(2.i +1) &larr;i |n) +power(n, -1)/n,
then s(n+1) -s(n)
  = 4.power(n, -1)/(2.n +1) -power(n, -1)/n +power(1+n, -1)/(n+1)
  = power(n, -1).(4.n.n +4.n -(2.n.n +3.n +1) -2.n.n -n)/(2.n +1)/n/(n +1)
  = power(n+1, -1)/n/(2.n +1)/(n +1)
  = -4.power(n, -1)/(power(3, 2.n +1) -(2.n +1))
so s(n) = s(1) +sum(: s(i +2) -s(i+1) &larr;i |n-1)
  = 3 +4.sum(: power(i, -1)/(i +1)/(2.i +3)/(i +2) &larr;i |n-1)
where each term in the sum has denominator odd^3 -odd,
giving pi as: 3 +4/(3^3 -3) -4/(5^3 -5) +4/(7^3 -7) -4/(9^3 -9) +...

If instead we go to the 1/(n +1/4/n) = 4.n/(4.n.n +1) correction, s(n) =
4.sum(: power(i, -1)/(2.i +1) &larr;i |n) +4.power(n, -1).n/(4.n.n +1),
we get s(n +1) -s(n)
  = 4.power(n, -1)/(2.n +1) -4.power(n, -1).n/(4.n.n +1)
	+4.power(n+1, -1).(n +1)/(4.(n +1).(n +1) +1)
  = 4.power(n, -1).(1/(2.n +1) -n/(4.n.n +1) -(n+1)/(4.n.n +8.n +5))
  = 4.power(n, -1).((4.n.n +1).(4.n.n +8.n +5) -n.(2.n +1).(4.n.n +8.n +5)
	-(n+1).(2.n +1).(4.n.n +1))/(2.n +1)/(4.n.n +1)/(4.n.n +8.n +5)
  = 16.power(n, -1)/(2.n +1)/(4.n.n +1)/(4.(n +1).(n +1) +1)
in which (4.n.n +1).(4.n.n +8.n +5) is (1 +(k -1)^2).(1 +(k +1)^2)
for k = 2.n +1; this, in turn, is (2 +k.k -2.k).(2 +k.k +2.k)
  = (2 +k.k)^2 -4.k.k = 4 +k^4 or 4 +(2.n +1)^2
so Mahdava (or at least his followers whose writings survive) knew to compute:
pi/16 = sum(: (-1)^i/(2.i +1)/(4 +(2.i +1)^4) &larr;i |{naturals})

Given that they also knew about the third layer of correction,
1/(n +1/(4.n +4/n)) = 4.(n.n +1)/n/(4.n.n +5), the Kerala school could
presumably also use s(n) = 4.sum(: power(i, -1)/(2.i +1) &larr;i |n)
+4.power(n, -1).(n.n +1)/n/(4.n.n +5), giving rise to

s(n +1) -s(n)
  = 4.power(n, -1)/(2.n +1) -4.power(n, -1).(n.n +1)/n/(4.n.n +5)
	+4.power(n+1, -1).(n.n +2.n +2)/(n +1)/(4.n.n +8.n +9)
  = 4.power(n, -1).(n.(4.n.n +5).(n +1).(4.n.n +8.n +9)
	-(n.n +1).(2.n +1).(n +1).(4.n.n +8.n +9)
	-(n.n +2.n +2).(2.n +1).n.(4.n.n +5)
	)/(2.n +1)/n/(4.n.n +5)/(n +1)/(4.n.n +8.n +9)
  = 4.power(n, -1).(
	16.n.n.n.n.n.n +48.n.n.n.n.n +88.n.n.n.n +96.n.n.n +85.n.n +45.n
	-(8.n.n.n.n.n.n +28.n.n.n.n.n +54.n.n.n.n +63.n.n.n +55.n.n +35.n +9)
	-(8.n.n.n.n.n.n +20.n.n.n.n.n +34.n.n.n.n +33.n.n.n +30.n.n +10.n)
	)/n/(n +1)/(2.n +1)/(4.n.n +5)/(4.n.n +8.n +9)
  = -36.power(n, -1)/n/(n +1)/(2.n +1)/(4.n.n +5)/(4.(n +1)^2 +5)
in which, with k = 2.n +1 as before, four times the denominator is
(k -1).k.(k +1).((k -1)^2 +5).((k +1)^2 +5)
  = k.(k.k -1).(k^4 +8.k.k +36)
  = k.(k.k -1).((k.k +4)^2 +20)
  = k.(k^6 +7.k^4 +28.k^2 -36)
but I can't see a neat way to rearrange that.

Let's look at the rationals implied by the first few values of our
full formula.
n = 1: 4 -1/(1 +1/4) = 4 -4/5 = 16/5 = 3.2
n = 2: 4 -4/3 +1/(2 +1/(8 +4/2)) = 3 +1/7 = 22/7 = 3.142857
n = 3: 4 -4/3 +4/5 -1/(3 +1/(12 +4/(3 +3/4)))
	= 3 +427/3015 = 9472/3015 = 3.141625
n = 4: 4 -4/3 +4/5 -4/7 +1/(4 +1/(16 +4/(4 +9/(16 +4))))
	= 3 -11/3/35 +1504/6105 = 44752 / 14245 = 3.1415935
n = 5: 4 -4/3 +4/5 -4/7 +4/9 -1/(5 +1/(20 +4/(5 +9/(20 +16/(5 +5/4)))))
	= 3 +107/315 -63156/318825 = 7011328 / 2231775 = 3.141592678
The last is accurate to one part in 40 million.

See study.LICENSE for copyright and license information.
"""

def contail(n, div = lambda n, s: n * 1. / d):
    assert n > 0
    k, i = 1 + 3 * (n & 1), n
    tail = div(n, k)
    while i > 1:
        i -= 1
        k = 5 -k
        tail = div(i * i, k * n +tail)
    return div(1, n +tail)

def continued(div = lambda n, s: n * 1. / d):
    """Iterate a sequence that converges on pi.

    This is the finitely-continued fraction representation, that gets
    one step deeper with each iteration.  Optional argument is a
    function taking numerator and denominator, that should return some
    value representing their ratio.  Its default is just to use
    python's built-in float division, but see study.maths.ratio's
    Rational for an alternative.\n"""
    sign, val, n = 1, 4, 1
    while True:
        sign = -sign
        yield val +sign * contail(n, div)
        n += 1
        val += sign * div(4, 2 * n - 1)

def rootle():
    thirdy, val, odd = 12**.5, 0, 1
    while True:
        val += thirdy / odd
        yield val
        odd += 2
        thirdy /= -3.
