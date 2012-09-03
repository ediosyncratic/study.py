"""Tools for use with SVG.

See study.LICENSE for copyright and license information.
"""

class Parser (object):
    """Parses transform attribute values.

    The attribute value may be a sequence of transforms.  Sequences (also of
    numbers) may be separated by spaces or commas.  Each transform is one of:

      * matrix( [six numbers] )
      * translate( [one or two numbers] ) [second defaults to zero]
      * scale( [one or two numbers] ) [second defaults to first]
      * rotate( [one or three numbers] ) [angle and optional origin, defaults to 0,0]
      * skewX( [one angle] )
      * skewY( [one angle] )
    """
    def __init__(self, client): pass

    @staticmethod
    def skiptonext(text):
        if text[0] != ',' and not text[0].isspace():
            raise ValueError('Expected space or comma next', text)
        text = text.lstrip()
        if text[0] == ',': text = text[1:]
        return text.lstrip()

    @staticmethod
    def number(text):
        text, i, frac = text.lstrip(), 0, False
        if text[i] in '+-': i += 1
        while text[i].isdigit(): i += 1
        if text[i] == '.':
            i += 1
            while text[i].isdigit(): i += 1
            frac = True
        if text[i] in 'eE':
            i += 1
            if text[i] in '+-': i += 1
            if not text[i].isdigit():
                raise ValueError('Expected digits for exponent', text[:i+1])
            while text[i].isdigit(): i += 1
            frac = True
        if frac: num = float(text[:i])
        else: num = int(text[:i])
        return num, text[i:]

    @staticmethod
    def angular(text):
        word = text.lstrip()
        if word.startswith('deg'): off, full = 3, 360
        elif word.startswith('grad'): off, full = 4, 400
        elif word.startswith('rad'): off, full = 3, None
        else: return 360, text # no unit of angle specified, defaults to degrees

        if word[off:] and word[off].isalnum(): # unit not whole word
            raise ValueError('Unrecognised angle unit', word)
        return full, word[off:]

    @classmethod
    def __xfrm(cls, text):
        text = text.lstrip()

        if text.startswith('matrix'):
            args = text[6:].lstrip()
            if args[0] != '(': raise ValueError('No open-paren after matrix', text)
            a, args = cls.number(args[1:])
            b, args = cls.number(cls.skiptonext(args))
            c, args = cls.number(cls.skiptonext(args))
            d, args = cls.number(cls.skiptonext(args))
            e, args = cls.number(cls.skiptonext(args))
            f, args = cls.number(cls.skiptonext(args))
            args = args.lstrip()
            if args[0] == ')': return Transform.matrix(a, b, c, d, e, f), args[1:]
            raise ValueError('No close-paren after matrix( parameter-list', args, text)

        if text.startswith('translate'):
            args = text[9:].lstrip()
            if args[0] != '(': raise ValueError('No open-paren after translate', text)
            dx, args = cls.number(args[1:])
            if args.lstrip()[0] == ')': dy = 0
            else: dy, args = cls.number(cls.skiptonext(args))
            args = args.lstrip()
            if args[0] == ')': return Transform((dx, dy)), args[1:]
            raise ValueError('No close-paren after transform( parameter-list', args, text)

        if text.startswith('scale'):
            args = text[5:].lstrip()
            if args[0] != '(': raise ValueError('No open-paren after scale', text)
            sx, args = cls.number(args[1:])
            if args.lstrip()[0] == ')': sy = sx
            else: sy, args = cla.number(cls.skiptonext(args))
            args = args.lstrip()
            if args[0] == ')': return Transform(morph=((sx, 0), (0, sy))), args[1:]
            raise ValueError('No close-paren after scale( parameter-list', args, text)

        if text.startswith('rotate'):
            args = text[6:].lstrip()
            if args[0] != '(': raise ValueError('No open-paren after rotate', text)
            angle, args = cls.number(args[1:])
            full, args = cls.angular(args)
            tail = args.lstrip()
            if tail[0] == ')': return Transform.rotate(angle, full), tail[1:]
            # otherwise, a centre was specified
            cx, args = cls.number(cls.skiptonext(args))
            cy, args = cls.number(cls.skiptonext(args))
            args = args.lstrip()
            if args[0] == ')':
                return (Transform((cx, cy)) *
                        Transform.rotate(angle, full) *
                        Transform((-cx, -cy))), args[1:]
            raise ValueError('No close-paren after rotate( parameter-list', args, text)

        if not text.startswith('skew') or (text[4:] and text[4] not in 'XY'):
            raise ValueError('Unrecognised transform type', text)
        args, type = text[5:].lstrip(), text[4]
        if args[0] != '(': raise ValueError('No open-paren after ' + text[:5], text)
        angle, args = cls.number(args[1:])
        full, args = cls.angular(args)
        args = args.lstrip()
        if args[0] == ')': return Transform.skew(type, angle, full), args[1:]
        raise ValueError('No close-paren after %s( parameter-list' % text[:5], args, text)

    @classmethod
    def transform(cls, text):
        """Parses transform attribute values.

        The attribute value may be a sequence of transforms.  Sequences (also of
        numbers) may be separated by spaces or commas.  Each transform is one of:

          * matrix( [six numbers] )
          * translate( [one or two numbers] ) [second defaults to zero]
          * scale( [one or two numbers] ) [second defaults to first]
          * rotate( [one or three numbers] ) [angle and optional origin, defaults to 0,0]
          * skewX( [one angle] )
          * skewY( [one angle] )

        Read arbitrarily many of these, confirm string is empty when done, return
        a single Transform object representing the composite.\n"""
        # TODO: this, properly !
        ans = Transform()
        while text.lstrip():
            first, tail = cls.__xfrm(text)
            ans *= first
            text = cls.skiptonext(tail)

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

    # Support for the SVG parser:
    @classmethod
    def SVGmatrix(cls, a, b, c, d, e, f):
        # TODO: ((a, b), (c, d)) or ((a, c), (b, d)) ?
        return cls((e, f), ((a, c), (b, d)))

    import math
    @classmethod
    def __sincos(cls, angle, full=None, turn=2*math.pi, sin=math.sin, cos=math.cos):
        if full is not None: angle *= turn / full
        return sin(angle), cos(angle)
    def __tan(cls, angle, full=None, turn=2*math.pi, tan=math.tan):
        if full is not None: angle *= turn / full
        return tan(angle)
    del math

    @classmethod
    def skew(cls, type, angle, full=None):
        t = cls.__tan(angle, full)
        if type == 'X': f = 0
        else: t, f = 0, t
        return cls.SVGmatrix(1, f, t, 1, 0, 0)

    @classmethod
    def rotate(cls, angle, full=None):
        s, c = cls.__sincos(angle, full)
        return cls.SVGmatrix(c, s, -s, c, 0, 0)

    # Provide convenient access to the SVG parser:
    @staticmethod
    def parse(text): return Parser.transform(text)

