"""Lazily-evaluated and cache-aware mappings.

Contents:
  LazyDict -- mapping populated with data only when needed

$Id: mapping.py,v 1.2 2009-01-28 08:35:46 eddy Exp $
"""

class LazyDict (dict):
    def __init__(self, each=None, fill=None):
	"""Initialize a lazilly-filled dictionary.

	Accepts two optional arguments:
	  each -- callable to look up a single entry, or (default) None.
	  fill -- callable to initially populate self, or (default) None.

	If fill is not None, the first look-up in self shall begin by calling
	self.update(fill()), so fill can be used to populate self (e.g. by
	parsing a file on disk).  This can be convenient if a dictionary object,
	that would be expensive to populate, must be created before it is
	practical to know whether it shall actually be needed.

	If each is not None and an attempt to access self[key] fails, for some
	key, then each(key) shall be called: whatever it returns is saved as
	self[key] and returned.  If each cannot provide a value for key, it
	should raise KeyError (and it shall be called again on all future
	attempts to access self[key]).\n"""

	if fill is not None: self.__fill = fill
	if each is not None: self.__each = each

    __upget = dict.__getitem__
    def __getitem__(self, key):
	try: f = self.__fill
	except AttributeError: pass
	else:
	    del self.__fill
	    self.update(f())

	try: return self.__upget(key)
	except KeyError: pass

	try: e = self.__each
	except AttributeError:
	    raise KeyError(key)

	self[key] = ans = e(key)
	return ans
