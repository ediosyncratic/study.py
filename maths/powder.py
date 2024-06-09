"""One-sided power distributions.

The integral of power(q) over an interval is the change of power(1+q)/(1+q)
over that same interval, provided q is not -1 (where we get log, instead; and
this goes to infinities at zero and infinity).  For this to be finite with one
end of the interval zero, we need 1+q > 0; for it to be finite when one end of
the interval is infinite, we need q < -1.  So a density 1/(power(-(z+1))
+power(t+1)) is normalisable for z and t positive.

See study.LICENSE for copyright and license information.
"""

def total(z, t, tiny = .01):
    """Normalization of a one-sided power distribution.

    Density functions of form R(z, t) = 1/(power(-(1 +z)) + power(1 +t)) on
    {positives}, with z and t also both positive, should be normalizable.  This
    computes their normalisation factors.  The n-th moments of R(z, t) are just
    the ratios of the normalisation constants of R(n+z, t-n) and R(z, t).
    Notably, for n >= t, they are infinite.

    To compute the normalisation we must, in general, do brute force arithmetic
    integration.  We can take a low-interval (from zero) well approximated by
    power(1 +z), with integral power(2 +z)/(2 +z) evaluated at the high end of
    the interval and a high-interval (to infinity) well approximated by
    power(-(1 +t)), with integral power(-t)/t evaluated at the low end of the
    interval.  In between we must use brute force on intervals, but we can at
    least chose the widths of the intervals so as to use narrower intervals
    where the density is large enough to matter and varying fast, while using
    wider intervals where it is low or stable."""
    assert z >= -1
    assert t > 0
    if tiny > .1:
        tiny = .1
    e = 1. / (2. + z + t)

    def func(x):
        return 1/(x**-(1 +z) +x**(1 +t))
    def deriv(x):
        return ((1 +t) * x**t -(1 +z) * x**-(2 +z))/(x**-(1 +z) +x**(1 +t))**2

    # Allow fractional errors of 1% in the integrand at the bounds of the two
    # tails:
    low = tiny**e
    top = 1 / low
    assert low < top
    cuts = [low, top]

    # Cut at the peak:
    peak = ((1 +z) / (1 +t)) ** e # where derivative is zero
    if low < peak < top:
        cuts.insert(1, peak)

    # Compute points of inflection:
    a = 2 +3 * t +t * t # (2 +t) * (1 +t) > 0
    b = 6 +5 * t +7 * z +t * t +4 * z * t +z * z
    c = (1 +z +z * z) # (z +.5)**2 +.75 > 0
    u = b * .5 / a
    d = u**2 - c / a
    if d > 0:
        d = d**.5
        assert d <= u, (a, c, b, u, d) # See comments above; c/a > 0, so d**2 < u**2
        inflect = (u -d)**e, (u +d)**e
        if cuts[0] < inflect[0] < cuts[1]:
            cuts.insert(1, inflect[0])
        if cuts[-2] < inflect[1] < cuts[-1]:
            cuts.insert(-1, inflect[1])
    elif d == 0:
        if cut[0] < u < cuts[1]:
            cuts.insert(1, u)
        elif cut[-2] < u < cut[-1]:
            cuts.insert(-1, u)

    changed = True
    while changed:
        changed, n = False, len(cuts)
        while n > 1:
            n -= 1
            x, y = cuts[n - 1 : n + 1]
            mid = (x +y) * .5
            if mid in (x, y):
                continue # gap already as narrow as it can get
            xd, yd = deriv(x), deriv(y)
            r = abs(yd - xd) * 2. / (abs(xd) + abs(yd))
            if r > tiny:
                changed = True
                cuts.insert(n, mid)

    ws = tuple(y - x for x, y in zip(cuts[:-1], cuts[1:]))
    fs = tuple(func(x) for x in cuts)
    hs = tuple(y + x for x, y in zip(fs[:-1], fs[1:]))
    return low ** (z +2) / (z +2) + top ** -t / t +sum(
        w * h * .5 for w, h in zip(ws, hs))
