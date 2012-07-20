"""Tools for use with SVG.
"""

class Transform (object):
    """Represents a linear map followed by translation.

    This is the class of transformations supported by SVG.
    """
    from study.maths.vector import Tensor as __tensor

    def __init__(self, offset=(0, 0), morph=((1,0),(0,1))):
        """Package an offset vector and a square matrix as a transformation.

        Takes two optional arguments:
           offset -- defaults to (0, 0)
           morph -- defaults to the identity matrix on two dimensions

        Represents a mapping that takes an input vector v, applies the linear
        map represented by the matrix, then adds the offset.\n"""
        self.__off, self.__morph =  self.__tensor(offset), self.__tensor.fromSeq(morph)

    def __repr__(self):
        return 'Transform.parse("matrix(%s, %s, %s, %s, %s, %s)")' % (
            self.__morph[0][0], self.__morph[1][0],
            self.__morph[0][1], self.__morph[1][1],
            self.__off[0], self.__off[1])

    def unwrap(other, what): # tool function used by arithmetic operators
        try: v, m = other.__off, other.__morph
        except AttributeError: pass
        else: return v, m

        # TODO: what other unpacking do we want to support ?
        raise ValueError("Transform can only %s another" % what, other)

    @classmethod
    def __mul(cls, (v1, m1), (v0, m0)):
        """Compose two transforms.

        Specified by:
          (: v1 +m1*x &larr;x :)&on;(: v0 +m0*x &larr;x :)
        = (: v1 +m1*(v0 +m0*x) &larr;x :)
        = (: (v1 +m1*v0) +m1*m0*x &larr;x :)
        """
        return cls(v1 + m1.dot(v0), m1.dot(m0))

    def __mul__(self, other, unpack=unwrap):
        return self.__mul((self.__off, self.__morph), unpack(other, "multiply by"))
    def __rmul__(self, other, unpack=unwrap):
        return self.__mul(unpack(other, "multiply"), (self.__off, self.__morph))

    # Division isn't even necessarily well-defined.

    @classmethod
    def __add(cls, (v1, m1), (v0, m0)): return cls(v1 +v0, m1 +m0)
    def __add__(self, other, unpack=unwrap):
        return self.__add((self.__off, self.__morph), unpack(other, "add"))
    def __radd__(self, other, unpack=unwrap):
        return self.__add(unpack(other, "add to"), (self.__off, self.__morph))

    @classmethod
    def __sub(cls, (v1, m1), (v0, m0)): return cls(v1 -v0, m1 -m0)
    def __sub__(self, other, unpack=unwrap):
        return self.__sub((self.__off, self.__morph), unpack(other, "subtract"))
    def __rsub__(self, other, unpack=unwrap):
        return self.__sub(unpack(other, "subtract from"), (self.__off, self.__morph))

    del unwrap
    def __call__(self, vec): return self.__off + self.__morph.dot(vec)

    @classmethod
    def parse(cls, text):
        """Parses transform attribute values.
        """
        raise NotImplementedError
