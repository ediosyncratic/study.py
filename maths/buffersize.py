"""How big a buffer do you need for printing decimals ?

The answer is 53 * n // 22 plus an offset.  The 53 // 22 is a reasonably good
(obtained using maths.continued.real_continued(), naturally) approximation to
log(256)/log(10), erring slightly on the high side (so that we can be confident
of safety at large size).  The offset includes one for the '\0' that a C string
needs; skip that if you're in a better language that doesn't need it.  Aside
from that, unsigned needs +1 (for random bumpiness of base 256 vs base 10
lengths) and signed needs another +1 for the minus sign.

See study.LICENSE for copyright and license information.
"""
def bigenough(n): return 53 * n // 22

def bufsizs(bytes): return len(str(-(1 << (bytes * 8 - (bytes > 0))))) + 1
def bufsizu(bytes): return len(str((1 << (bytes * 8)) - 1)) + 1
HUGE = 4096 # an integral type with this many bytes has 16k bits
SANE = 128 # one this big has 1k bits
counts = [2 + bigenough(n) - bufsizu(n) for n in range(HUGE)]
signed = [3 + bigenough(n) - bufsizs(n) for n in range(HUGE)]
assert(min(counts) >= 0 and min(signed) >= 0)
assert(max(counts[:SANE]) < 3 and max(signed[:SANE]) < 3)
