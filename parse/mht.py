"""Digesting .mht files.

Some web browsers (notably including Opera), when asked to save a web
page to disk, save it as a pseudo-mail-message *.mht file; it is
desirable to be able to unpack these.  Some uses for that would be well
served by providing for unpacking as a directory-full of files.  This
module provides that functionality.
"""

from study.cache.property import Cached, lazyprop
class MHT (Cached):
    def __init__(self, filename):
        self.__msg = self.__ingest(filename)

    from email import message_from_file
    @staticmethod
    def __ingest(filename, parse=message_from_file):
        fp = open(filename)
        try: return parse(fp)
        finally: fp.close()
    del message_from_file

    def __leaves(self):
        """Iterate over the non-multipart parts within self.

        Uses Message.walk() to find all the parts of the message that
        aren't multipart, yielding each in turn.\n"""

        # if self.__msg isn't multipart, it's its sole .walk() entry.
        for part in self.__msg.walk():
            if not part.is_multipart(): yield part

    def __locations(self):
        """Iterate over Content-Location header values.

        Returns an iterator over all non-multipart parts of the message,
        yielding the value of any Content-Location header from each.\n"""
        for part in self.__leaves():
            loc = part['Content-Location']
            if loc: yield loc

    @lazyprop
    def stem(self, ig=None, cut='/\\'):
        """Maximal common prefix of Content-Location headers.

        This is the longest common path-like prefix of all
        Content-Location headers in the message.  For a simple message,
        that will be the single Content-Location header.  If no part of
        the message supplies a Content-Location, an AttributeError is
        raised if you try to evaluate this lazy property.\n"""

        seq = self.__locations()
        try: ans = seq.next()
        except StopIteration:
            raise AttributeError('Message has no Content-Location headers')

        head = len(ans)
        for it in seq:
            while it[:head] != ans:
                while head > 0 and ans[head-1] not in cut: head -= 1
                ans = ans[:head]

        return ans

    def unpack(self, into, prefix=None):
        """Unpacks all parts as files under a directory.

        Required argument, into, is a prefix (typically the name of a
        directory, with trailing directory separator) to be added to the
        names of files into which to unpack the parts of the
        message.

        Optional argument, prefix, is None or a prefix to be checked on
        Content-Location headers, such as might have been obtained as
        self.stem (q.v.).

        If prefix is specified and is an initial portion of the
        Content-Location header of a part, the remainder of that header
        is used as the part's name relative to into.  Otherwise, if the
        part has a Content-Disposition that specifies a filename, this
        is used, with Content-Location serving as an alternate source
        for such a filename. but any directory-like part of this name is
        discarded.  Otherwise, the part is discarded.

        Note that an empty string prefix (which .stem may be) would mean
        to use whole Content-Location headers, as opposed to ignoring
        such headers when prefix is None; so passing self.stem or None
        is probably more suitable than passing self.stem verbatim; and
        it is generally prudent to sanity-check self.stem before using
        it.\n"""

        for part in self.__leaves():
            nom = self.__name(part, prefix)
            if nom:
                self.__mkdirs(into, nom)
                fd = open(into + nom, 'w')
                try: fd.write(part.get_payload(decode=True))
                finally: fd.close()

    import os
    @classmethod
    def __mkdirs(cls, into, nom, mkdir=os.mkdir,
                 split=os.path.split, isdir=os.path.isdir):
        head = split(nom)[0]
        if not head: return
        if isdir(into + head): return
        cls.__mkdirs(into, head)
        mkdir(into + head)
    del os

    @staticmethod
    def __name(part, prefix):
        nom = part['Content-Location']
        if nom and prefix is not None:
            if nom.startswith(prefix):
                return nom[len(prefix):]
            print nom, "doesn't start with", prefix

        nom = part.get_filename(nom)
        if not nom: return None
        cut = max(nom.rfind('/'), nom.rfind('\\'))
        if cut >= 0: nom = nom[cut:]
        return nom

if __name__ == '__main__':
    # usage: python study/money/mht.py blah.mht outdir/
    # Be sure to not forget the trailing / !
    import sys
    box = MHT(sys.argv[1])
    box.unpack(sys.argv[2], box.stem or None)
