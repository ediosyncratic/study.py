"""Digesting .mht files.

Some web browsers (notably including Opera), when asked to save a web
page to disk, save it as a pseudo-mail-message *.mht file; it is
desirable to be able to unpack these.  Some uses for that would be well
served by providing for unpacking as a directory-full of files.  This
module provides that functionality.

See study.LICENSE for copyright and license information.
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

        Returns an iterator over all non-multipart parts of the
        message, yielding the path of any Content-Location header from
        each.  If the header value appears to be an URL, the path of
        the URL is returned.  HTML content's name gets a .html suffix
        rather than any .asp, .php or similar that might have
        generated it.\n"""
        for part in self.__leaves():
            loc = self.__cleanName(part)
            if loc: yield loc

    @lazyprop
    def stem(self):
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
                head = self.__lastSlash(ans)
                if head <= 0:
                    return ''
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

        If prefix is specified and is an initial portion of the path
        extracted from the Content-Location header of a part, the
        remainder of that header is used as the part's name relative to
        into.  Otherwise, if the part has a Content-Disposition that
        specifies a filename, this is used, with Content-Location
        serving as an alternate source for such a filename; but any
        directory-like part of this name is discarded.  Failing even
        this, the part is discarded.

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
        if not head or head == nom or isdir(into + head):
            return
        cls.__mkdirs(into, head)
        mkdir(into + head)
    del os

    @classmethod
    def __name(cls, part, prefix):
        nom = cls.__cleanName(part)
        if nom and prefix is not None:
            if nom.startswith(prefix):
                return nom[len(prefix):]
            print nom, "doesn't start with", prefix

        nom = part.get_filename(nom)
        if not nom: return None
        cut = cls.__lastSlash(nom)
        if cut >= 0: nom = nom[cut:]
        return nom

    @staticmethod
    def __lastSlash(path, cuts = '/\\'):
        return max(path.rfind(ch) for ch in cuts)

    @staticmethod
    def __cleanName(part, scheme = '://', tails = '#?'):
        loc = part['Content-Location']

        # Prune scheme and host, if present:
        cut = loc.find(scheme)
        if cut >= 0:
            loc = loc[cut + len(scheme):]
            cut = loc.find('/')
            if cut < 0: return ''
            loc = loc[cut:]

        # Remove fragment identifier and query string:
        for tail in tails:
            cut = loc.find(tail)
            if cut >= 0: loc = loc[:cut]

        # Turn .php or .asp suffixes into .html
        if loc and part.get('Content-Type') == 'text/html':
            cut = loc.rfind('.')
            if cut < 0:
                loc = loc + '.html'
            elif loc[cut + 1:] not in ('html', 'xhtml'):
                loc = loc[:cut + 1] + 'html'

        return loc

if __name__ == '__main__':
    # usage: python study/money/mht.py blah.mht outdir/
    # Be sure to not forget the trailing / !
    import sys
    box = MHT(sys.argv[1])
    box.unpack(sys.argv[2], box.stem or None)
