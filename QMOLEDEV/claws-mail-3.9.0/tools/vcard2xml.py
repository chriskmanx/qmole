#!/usr/bin/python2.2
"""

Copyright © 2003 Bogdan Sumanariu <zarrok@yahoo.com>

  This file is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3 of the License, or
  (at your option) any later version.
   
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  script name : evolutionvcard2claws.py

 script purpose : convert an evolution addressbook VCARD file 
 into a Claws Mail addressbook

 tested with evolution 1.2.x, and 1.4.x

"""

import string
import sys
import time
import os

keywds = ('x-evolution-file-as','fn', 'n','email;internet','nickname', 'url', 'org')


################################################################################
##  reads a vcard and stores as hash pairs key/value where value is a list    ##
################################################################################

def readVCARD (file) :

	"""

	skips fom <file> until a 'begin' tag from VCARD is encountered.
	from this point starts constructing a map (key, [values] ) 
	VCARD entry format -> tag:value 
	
		key <- tag
		[values] <- list with the values of <tag> if there are more tags with the same name

	"""
	r=' '
	bgn,end = -1, -1;
	d = dict()
	while r and bgn < 0 :
		r = file.readline()
		if len (r)  == 0 : return dict()
		if string.find('begin',string.lower(string.strip(r))) :
			bgn = 1
	while r and end < 0 :
		r = file.readline()
		s = string.split(string.lower(string.strip(r)),':')
		if s[0] <> '' :
			if d.has_key(s[0]) :
				d[s[0]].append(s[1])
			elif len(s) > 1:
				d[s[0]] = [s[1]]	
			else :
				d[s[0]] = ['']
			if s[0] == 'end' : end = 1	
	return d

##################################################################################
				 

###############################################################################################
## writes on a given file an xml representation for claws-mail addressbook received as a hash  ##
###############################################################################################

def writeXMLREPR (vcard,file,uid) :

	"""
	based on <vcard> and <uid> writes only recognized tags (the ones defined in <keywds> list)
	NOTE: <url> and <org> tag will be written as attributes (there are such tags in claws-mail's
	      XML schema)
	"""
	if len (vcard.keys()) == 0 : return
	name = string.split(vcard.get(keywds[2])[0],';')

	fn, ln, nick, cn, a = '', '', '', '', ''

	if len(name) == 2 :
		fn = name[0]
		ln = name[1]
	elif len(name) ==1 :
		fn = name[0]
	
	if vcard.has_key(keywds[4]) :
		nick = vcard.get(keywds[4])[0]
	if len(vcard.get(keywds[1])[0]) :
		cn = vcard.get(keywds[1])[0]
	else :
		cn = vcard.get(keywds[0])[0];

	a += str('\n<person uid=\"' + str(uid[0]) + '\" first-name=\"' + fn + '\" last-name=\"' + ln
		+ '\" nick-name=\"' + nick + '\" cn=\"' + cn + '\" >\n')
	a += '\t<address-list>\n'
	if vcard.get(keywds[3]) :
		for c in vcard.get(keywds[3]) :
			uid[0] = uid[0] + 1
			a += '\t\t<address uid=\"' + str(uid[0]) + '\" alias=\"' + nick  + '\" email=\"' + c + '\" remarks=\"\" />\n'
	else :
		uid[0] = uid[0]+1
		a += '\t\t<address uid=\"' + str(uid[0]) + '\" alias=\"' +  nick + '\" email=\"\" remarks=\"\" />\n'
	a += '\t</address-list>\n'
	a += '\t<attribute-list>\n'
	for key in keywds[5:] :
		if vcard.get(key) :
        	        for c in vcard.get(key) :
        	       	        uid[0] = uid[0] + 1
	                       	a += '\t\t<attribute uid=\"' + str(uid[0]) + '\" name=\"' + key +'\">'+c+'</attribute>\n'
	a += '\t</attribute-list>\n'
	a += '</person>\n'
	file.write(a)
	file.flush()
		
###################################################################################################

def convert (in_f, o_f, name='INBOX') :
	d = {'d':1}
        uid = [int(time.time())]
	try : 
	        print 'proccessing...\n'
        	o_f.write('<?xml version="1.0" encoding="ISO-8859-1" ?>\n<address-book name="'+name+'" >\n');

	        while len(d.keys()) > 0 :
        	        d = readVCARD(in_f)
                	writeXMLREPR (d, o_f, uid)
	                uid[0] = uid [0]+1

        	o_f.write('\n</address-book>')
		print 'finished processing...\n'
	except IOError, err :
		print 'Caught an IOError : ',err,'\t ABORTING!!!'
		raise err

#################################################################################################

def execute () :
	if len(sys.argv) <> 3 and len(sys.argv) <> 2 :
		print str("\nUsage: vcard2xml.py  source_file [destination_file]\n\n" +
		'\tWhen only <source_file> is specified will overwrite the existing addressbook.\n'+
		'\tWhen both arguments are suplied will create a new additional addressbook named \n\tas the destination file.'+'\n\tNOTE: in both cases the Claws Mail must be closed and ran at least once.\n\n')
		sys.exit(1)

	in_file = None
	out_file = None
	path_to_out = os.environ['HOME']+'/.claws-mail/'
	adr_idx = 'addrbook--index.xml'
	adr_idx_file = None
	tmp_adr_idx_file= None
	got_ex = 0

	try :
		in_file = open(sys.argv[1])
	except IOError, e:
		print 'Could not open input file <',sys.argv[1],'>  ABORTING'
		sys.exit(1)

	if len(sys.argv) == 2 :
		try :
			dlist = os.listdir(path_to_out);
			flist=[]
			for l in dlist :
				if l.find('addrbook') == 0 and l.find("addrbook--index.xml") < 0 and l.find('bak') < 0 :
					flist.append(l)
			flist.sort()
			out_file = flist.pop()
			os.rename(path_to_out+out_file, path_to_out+out_file+'.tmp')
			out_file = open(path_to_out+out_file,'w')
			convert(in_file, out_file)
		except Exception, e:
			got_ex = 1
			print 'got exception: ', e
	else :
		try :
			os.rename(path_to_out+adr_idx, path_to_out+adr_idx+'.tmp')
			tmp_adr_idx_file = open(path_to_out+adr_idx+'.tmp')
			adr_idx_file = open(path_to_out+adr_idx,'w')
		except Exception, e :
			print 'Could not open <', path_to_out+adr_idx,'> file. Make sure you started Claws Mail at least once.'
			sys.exit(1)
		try :
			out_file = open(path_to_out+sys.argv[2],'w')
			convert(in_file, out_file, sys.argv[2].split('.xml')[0])
			l = tmp_adr_idx_file.readline()
			while l :
				if l.strip() == '</book_list>' :
					adr_idx_file.write('\t<book name="'+sys.argv[2].split('.xml')[0] +'" file="'+sys.argv[2]+'" />\n')
					adr_idx_file.write(l)
				else :
					adr_idx_file.write(l)
				l = tmp_adr_idx_file.readline()
		except Exception, e:
			got_ex = 1
			print 'got exception: ', e
	

	if got_ex :
		#clean up the mess
		print 'got exception, cleaning up the mess... changed files will be restored...\n'
		if adr_idx_file :
			adr_idx_file.close()
		if out_file :
			out_file.close()
		if len(sys.argv) == 2 :
			os.rename(out_file.name+'.tmp', out_file.name)
		else :
			os.remove(out_file.name)
			os.rename(path_to_out+adr_idx+'.tmp', path_to_out+adr_idx)
		if tmp_adr_idx_file :
			tmp_adr_idx_file.close()
				
	else :
		#closing all and moving temporary data into place
		print 'closing open files...\n'
		in_file.close()
		out_file.close()	
		if len(sys.argv) == 3 :
			os.rename(path_to_out+adr_idx+'.tmp',path_to_out+adr_idx+'.bak' )
		if len(sys.argv) == 2 :
			os.rename(out_file.name+'.tmp', out_file.name+'.bak')
		if adr_idx_file :
			adr_idx_file.close()
		if tmp_adr_idx_file :
			tmp_adr_idx_file.close()
		print 'done!'
		

if __name__ == '__main__':
    execute ()


