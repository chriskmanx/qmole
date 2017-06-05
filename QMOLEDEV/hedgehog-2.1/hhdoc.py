import string
import sys
import re

def printline(text, indent):
    print "%*s%s" % (indent * 1, "", text)

def protect(str):
    str = string.join(str.split("&"), "&amp;")
    str = string.join(str.split("<"), "&lt;")
    str = string.join(str.split(">"), "&gt;")
    return str

def stripcmt(str):
    while str and str[0] == ";":
    	str = str[1:]
    return str

def title2id(str):
    return string.join(str.lower().split(), "")

def paras(text, indent):
    lines = map(lambda s: stripcmt(s).strip(), text.split("\n"))
    while lines:
	i = lines.index("")
	if i == -1:
	    i = len(lines)
	para = map(lambda s: protect(s), lines[:i])
	if len(para) == 1:
	    printline("<para>%s</para>" % para[0], indent)
	elif len(para) > 1:
	    printline("<para>%s" % para[0], indent)
	    for line in para[1:-1]:
		printline("%s" % line, indent)
	    printline("%s</para>" % para[-1], indent)
	lines = lines[i+1:]

def funcdef(text):
    text = text.strip()
    assert len(text) > 0
    assert text[0] == "("
    assert text[-1] == ")"
    words = text[1:-1].split()
    assert len(words) > 0
    if len(words) == 1:
    	return "(%s)" % protect(words[0])
    else:
	return "(%s %s)" % (protect(words[0]),
	    	    	    string.join(map(lambda s: 
			    	    	    "<replaceable>%s</replaceable>" % 
					    protect(s),
					    words[1:]),
					" "))

def printfunc(m):
    printline("<glossentry>", 2)
    printline("<glossterm>%s</glossterm>" % funcdef(m.group("decl")), 3)
    printline("<glossdef>", 3)
    paras(m.group("text"), 4)
    printline("</glossdef>", 3)
    printline("</glossentry>", 2)

def printconst(s, m):
    text = m.group("text")

    printline("<glossentry>", 2)

    printline("<glossterm>%s</glossterm>" % m.group("decl"), 3)
    s = s[m.end():]
    m = moreconst.match(s)
    while m:
	printline("<glossterm>%s</glossterm>" % m.group("decl"), 3)
    	s = s[m.end():]
	m = moreconst.match(s)

    printline("<glossdef>", 3)
    paras(text, 4)
    printline("</glossdef>", 3)
    printline("</glossentry>", 2)

    return s

section = re.compile("(.*\n)*?(?P<section>;; Section:(.*\n)*?)(;; Section:|$)")

intro = re.compile("^;; Section:\s+(?P<title>.*)\n(?P<text>(;;.*\n)*)")

func = re.compile("(.*\n)*?(?P<text>(;;.*\n)+)" +
    	          "\\s+\\(def(-syntax)?\\s+(?P<decl>\\(([^)]|\n)*\))")

firstconst = re.compile("(.*\n)*?(?P<text>(;;.*\n)+)" +
			"\\s+\\((set|def-syntax)\\s+(?P<decl>\\S*)\\s.*\\n")
moreconst = re.compile("\\((set|def-syntax)\\s+(?P<decl>\\S*)\\s.*\\n")



for filename in sys.argv[1:]:
    f = open(filename, "r")
    data = f.read()
    f.close()

    m = section.match(data)
    while m:
	s = m.group("section")
	data = data[m.end("section"):]
	m = intro.match(s)
	s = s[m.end():]
	printline("<sect2 id='%s'>" % title2id(m.group("title")), 0)
	printline("<title>%s</title>" % m.group("title"), 1)
	printline("", 1)
	
    	paras(m.group("text"), 1)
    	printline("<glosslist>", 1)

    	mf = func.match(s)
	mc = firstconst.match(s)
	while mf or mc:
    	    if mf and not mc:
		printfunc(mf)
		s = s[mf.end():]
	    elif mc and not mf:
	    	s = printconst(s, mc)
	    elif mf.end() < mc.end():
	    	printfunc(mf)
		s = s[mf.end():]
	    else:
	    	s = printconst(s, mc)
	    mf = func.match(s)
	    mc = firstconst.match(s)

    	printline("</glosslist>", 1)
    	printline("", 1)
	printline("</sect2>", 0)
    	printline("", 0)
	m = section.match(data)
