# Grab the tips from Options.xml

from xml.sax import *
from xml.sax.handler import ContentHandler
import string, os

print "Extracting translatable bits from Options.xml..."

class Handler(ContentHandler):
	data = ""

	def startElement(self, tag, attrs):
		for x in ['title', 'label', 'end', 'unit']:
			if attrs.has_key(x):
				self.trans(attrs[x])
		self.data = ""
	
	def characters(self, data):
		self.data = self.data + data
	
	def endElement(self, tag):
		data = string.strip(self.data)
		if data:
			self.trans(data)
		self.data = ""
	
	def trans(self, data):
		data = string.join(string.split(data, '\n'), '\\n')
		if data:
			out.write('_("%s")\n' % data.replace('"', '\\"'))

try:
	os.chdir("po")
except OSError:
	pass
	
out = open('../tips', 'wb')
parse('../../Options.xml', Handler())
out.close()
