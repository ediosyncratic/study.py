"""Converting between binary-coded decimal and pure binary.

In Gottschalk v. Benson (409 U.S. 63 (1972) the US supreme court ruled on a
claimed patent on an algorithm for converting BCD to binary.  Having never seen
the claimed algorithm, or any algorithm to perform this task, I here embark on
an attempt at devising an efficient algorithm to perform the task.  As a
practitioner of the craft (computer programming) of ordinary skill (albeit
perhaps more experience than most), I contend that any algorithm I come up with
without reference to the claimed patent (which was, in any case, rejected)
should be deemed obvious in the technical sense relevant to patent law.

Each of these functions reads its input as BCD and converts to binary.  BCD
needs four bits per decimal digit, so each byte of data encodes two decimal
digits.  I assume that the data comes in big-endian form; the corresponding
algorithm for little-endian may be obtained by a trivial and well-known
transformation.
"""

def dumb(data, ten=10):
    """The most obvious solution possible.

    This is so obvious that it amounts to a formal a specification of what I
    have understood 'binary-coded decimal' to mean.

    The algorithm, as coded here, is instantly applicable to binary coded n-ary
    for any base n strictly between 8 and 16, here given as the optional second
    argument 'ten', whose default is 10 (the abuse of language and notation is
    deliberate).\n"""

    num = 0
    while data:
        (hi, lo), data = divmod(data[0], 16), data[1:]
        num = lo + ten * (hi + ten * num)

    return num
