"""Describe triangles.

Provides:
  Polygon -- a minimal description of a polygon
  Triangle -- a description of a triangle

See study.LICENSE for copyright and license information.
"""
from study.cache.property import lazyprop

class Polygon (object):
    def __init__(self, *edges):
        """Takes the lengths of the edges.

        Stores them as a tuple attribute named edges, in the given
        order.  Note that derived classes that arrange to set this
        attribute some other way (e.g. as a property) need not call
        this base constructor.  Edges may be Quantity lengths or any
        other kind of number, as long as they can be added and
        dividing one by another gives a scalar.\n"""
        self.edges = edges

class Triangle (Polygon):
    __upinit = Polygon.__init__
    def __init__(self, a, b, c):
        self.__upinit(a, b, c)

    @lazyprop
    def cosines(self):
        """The triple of cosines of angles.

        These are scalar Quantity objects.  Each entry in the triple
        is the cosine of the angle opposite the corresponding entry in
        self.edges.\n"""
        return tuple(self.__cosines(*self.edges))

    @lazyprop
    def angles(self):
        """The triple of angles in the triangle.

        These are Quantity objects, with units of angle.  Each entry
        in the triple is the angle opposite the matching entry in
        self.edges.\n"""
        return tuple(x.arcCos for x in self.cosines)

    @lazyprop
    def sines(self):
        """The triple of cosines of angles.

        These are scalar Quantity objects.  Each entry in the triple
        is the sine of the angle opposite the matching entry in edges;
        the ratio between matching entries in this triple and in edges
        should be constant.\n"""
        return tuple(x.Sin for x in self.angles)

    from study.value.quantity import Quantity
    @staticmethod
    def __cosines(a, b, c, Q = Quantity):
        loop = 3
        while loop > 0:
            loop -= 1
            yield Q((b * b + c * c - a * a) * 0.5 / b / c)
            a, b, c = b, c, a
    del Quantity

del lazyprop
