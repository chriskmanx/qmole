#!/usr/bin/python

import string, sys


def lReadEfile(sFileName):
	try:
		sLines = open(sFileName).read()
	except:
		print ('Error opening %s' %sFileName)
	lLines = []	
	lLines = string.splitfields(sLines, '\n')	
	return lLines
		

def dElines2Dict(lElines):
	dAliases = {}
	for sEntry in lElines:
		if '"' in sEntry:
			lChunks = string.splitfields(sEntry, '"')
		else:
			lChunks = string.splitfields(sEntry, ' ')
		if lChunks[0] <> 'alias':
			print ('ignoring invalid line: %s' %sEntry)
		else:
			sAdresses = string.joinfields(lChunks[2:], ',')
			print ('Entry added: %s %s' %(lChunks[1],sEntry))
			dAliases[lChunks[1]]=sAdresses
	return dAliases


def vWriteGfile(dAliases, sFileName):
	try:
		oFile = open(sFileName, 'w')
	except:
		print ('Error opening %s' %sFileName)
		return 0
	for sKey in dAliases.keys():
		#print ('BEGIN:VCARD')
		#print ('N:;%s' %sKey)
		#print ('BDAY:')
		#print ('ADR;HOME:;;;;;;')
		#print ('TEL:;')
		#print ('EMAIL;INTERNET:%s' %dAliases[sKey])	
		#print ('END:VCARD')
		oFile.write ('BEGIN:VCARD\n')
		oFile.write ('FN:%s\n' %sKey)
		oFile.write ('N:;%s\n' %sKey)
		oFile.write ('BDAY:\n')
		oFile.write ('ADR;HOME:;;;;;;;\n')
		oFile.write ('TEL:;\n')
		oFile.write ('EMAIL;INTERNET:%s\n' %dAliases[sKey])
		oFile.write ('END:VCARD\n')
	oFile.close()
	return 1 


if __name__ == '__main__':
	if len(sys.argv) >= 3:
		sEfileName = sys.argv[1]
		sGfileName = sys.argv[2]
		lAliases = lReadEfile(sEfileName)
		dAliases = dElines2Dict(lAliases)
		if vWriteGfile(dAliases, sGfileName) == 1:
			print ('Done!')
		else:
			print ('Error saving output-file')
	else:
		print ('Usage:\n %s <Eudora addressbook> <Gnomecard file>' %sys.argv[0])
