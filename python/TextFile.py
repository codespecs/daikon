# Text files with automatic (un)compression
#
# Written by: Konrad Hinsen <hinsenk@ere.umontreal.ca>
# Last revision: 1996-2-29
# 

"""This module defines a class TextFile whose instances can be
accessed like normal file objects (i.e. by calling readline(),
readlines(), and write()), but can also be accessed as sequence
objects via indexing. Note that in that case the whole file will be
stored in memory. Note also that the two access modes should not be
mixed.

The class TextFile also handles compression transparently, i.e. it is
possible to read lines from a compressed text file as if it were not
compressed.  Compression is deduced from the file name suffixes '.Z'
(compress/uncompress) and '.gz' (gzip/gunzip).

Finally, TextFile objects accept file names that start with '~' or
'~user' to indicate a home directory.
"""

import os

class TextFile:

    def __init__(self, filename, mode = 'r'):
	filename = os.path.expanduser(filename)
	if mode == 'r':
	    if not os.path.exists(filename):
		raise IOError, (2, 'No such file or directory')
	    if filename[-2:] == '.Z':
		self.file = os.popen("uncompress -c " + filename, mode)
	    elif filename[-3:] == '.gz':
		self.file = os.popen("gunzip -c " + filename, mode)
	    else:
		self.file = open(filename, mode)
	elif mode == 'w':
	    if filename[-2:] == '.Z':
		self.file = os.popen("compress > " + filename, mode)
	    elif filename[-3:] == '.gz':
		self.file = os.popen("gzip > " + filename, mode)
	    else:
		self.file = open(filename, mode)
	else:
	    raise IOError, (0, 'Illegal mode')
	self.write_flag = mode == 'w'
	self.line = 0
	self.cache = []
	self.open = 1

    def __del__(self):
	self.close()

    def _checkRead(self):
	if self.write_flag:
	    raise IOError, (9, 'Write access to read-only file')

    def _checkWrite(self):
	if not self.write_flag:
	    raise IOError, (9, 'Read access to write-only file')

    def __getitem__(self, item):
	self._checkRead()
	self._readall()
	return self.cache[item]

    def __setitem__(self, item, string):
	self._checkWrite()
	if item >= len(self.cache):
	    self.cache = self.cache + (item-len(self.cache)+1)*['']
	self.cache[item] = string

    def __len__(self):
	if not self.write_flag:
	    self._readall()
	return len(self.cache)

    def _readall(self):
	if self.open:
	    while 1:
		line = self.file.readline()
		if not line: break
		self.cache.append(line)
	    self.file.close()
	    self.open = 0

    def readline(self):
	self._checkRead()
	if self.cache:
	    l = self.cache[self.line]
	elif self.open:
	    l = self.file.readline()
	else:
	    l = ""
	self.line = self.line + 1
	return l

    def readlines(self):
	self._checkRead()
	self.cache = []
	self._readall()
	return self.cache

    def write(self, data):
	self._checkWrite()
	self.file.write(data)

    def writelines(self, list):
	self._checkWrite()
	for line in list:
	    self.file.write(line)

    def close(self):
	if self.open:
	    if self.write_flag and self.cache:
		for line in self.cache:
		    self.file.write(line)
	    self.file.close()
	    self.open = 0
	self.cache = None
