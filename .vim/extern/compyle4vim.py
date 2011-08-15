#!/usr/bin/env python

# Original author: Oleg Broytmann
# http://mail.python.org/pipermail/python-list/2002-June/148699.html

import sys

if len(sys.argv) == 1:
	print "Usage: %s <file.py>" % sys.argv[0] 
	sys.exit(1)

filename = sys.argv[1]
infile = open(filename, 'r')
codestring = infile.read()
infile.close()

try:
	compile(codestring, filename, "exec")
except SyntaxError, detail:
	pass
else:
	sys.exit()

msg, (_fname, lineno, offset, line) = detail

sys.stderr.write("""  File "%s", line %d, column %d
SyntaxError: %s
""" % (filename, lineno, offset, msg))

