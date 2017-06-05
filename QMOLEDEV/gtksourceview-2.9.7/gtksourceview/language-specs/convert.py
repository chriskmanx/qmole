#!/usr/bin/env python

import xml.dom.minidom as dom
import cgi

default_styles = {
    'Comment' : 'def:comment',
    'String' : 'def:string',
    'Preprocessor' : 'def:preprocessor',
    'Keyword' : 'def:keyword',
    'Data Type' : 'def:type',
    'Decimal' : 'def:decimal',
    'Specials' : 'def:specials',
    'Function' : 'def:function',
    'Base-N Integer' : 'def:base-n-integer',
    'Floating Point' : 'def:floating-point',
    'Floating point' : 'def:floating-point',
    'Others' : None,
    'Other' : None,
    'Others 2' : None,
    'Others 3' : None,
}

def escape_escape_char(ch):
    if ch == '\\':
        return '\\\\'
    elif ch in ['@']:
        return ch
    raise RuntimeError("don't know how to escape '%s'" % (ch,))

def escape_regex(s):
    return cgi.escape(s)

def normalize_id(id):
    if id == "C#":
        return "c-sharp"
    elif id == ".desktop":
        return "desktop"
    elif id == ".ini":
        return "ini"
    elif id == "C++ Line Comment":
        return "cpp-line-comment"
    elif id == "Markup (inline)":
        return "markup-inline"
    elif id == "Markup (block)":
        return "markup-block"
    else:
        return id.replace(', ', '-').replace('.', '-').replace('*', '-').replace(',', '-').replace(' ', '-').replace('/', '-').replace('#', '-').lower()

class LangFile(object):
    def __init__(self, id, name, _name, section, _section, mimetypes, globs, filename):
        object.__init__(self)

        assert name or _name
        assert section or _section

        self.id = normalize_id(id or name or _name)
        self.name = name
        self._name = _name
        self.section = section
        self._section = _section
        self.mimetypes = mimetypes
        self.globs = globs
        self.filename = filename
        self.contexts = []
        self.escape_char = None

    def set_esc_char(self, char):
        self.escape_char = char

    def add_context(self, ctx):
        self.contexts.append(ctx)

    def format_header(self, indent):
        string = '<?xml version="1.0" encoding="UTF-8"?>\n<language id="%s"' % (self.id,)

        if self.name:
            string += ' name="%s"' % (self.name,)
        else:
            string += ' _name="%s"' % (self._name,)

        string += ' version="2.0"'

        if self.section:
            string += ' section="%s"' % (self.section,)
        else:
            string += ' _section="%s"' % (self._section,)

        string += '>\n'

        if self.mimetypes or self.globs:
            string += indent + '<metadata>\n'
            if self.mimetypes:
                string += 2*indent + '<property name="mimetypes">%s</property>\n' % (cgi.escape(self.mimetypes),)
            if self.globs:
                string += 2*indent + '<property name="globs">%s</property>\n' % (cgi.escape(self.globs),)
            string += indent + '</metadata>\n\n'

        return string

    def format_footer(self, indent):
        return '</language>\n'

    def format_styles(self, indent):
        string = indent + "<styles>\n"
        styles = {}
        for ctx in self.contexts:
            map_to = default_styles[ctx.style_name]
            styles[ctx.style] = [ctx.style_name, map_to]
        for s in styles:
            id = s
            name, map_to = styles[s]
            if map_to:
                string += indent*2 + '<style id="%s" _name="%s" map-to="%s"/>\n' % (id, name, map_to)
            else:
                string += indent*2 + '<style id="%s" _name="%s"/>\n' % (id, name)
        string += indent + "</styles>\n\n"
        return string

    def format_contexts(self, indent):
        string = indent + '<definitions>\n'

        if self.escape_char and self.escape_char != '\\':
            char = escape_escape_char(self.escape_char)

            string += indent*2 + '<context id="generated-escape">\n'
            string += indent*3 + '<match>%s.</match>\n' % (char,)
            string += indent*2 + '</context>\n'

            string += indent*2 + '<context id="generated-line-escape">\n'
            string += indent*3 + '<start>%s$</start>\n' % (char,)
            string += indent*3 + '<end>^</end>\n'
            string += indent*2 + '</context>\n'

        for ctx in self.contexts:
            if self.escape_char:
                if self.escape_char != '\\':
                    esc_ctx = 'generated-escape'
                    line_esc_ctx = 'generated-line-escape'
                else:
                    esc_ctx = 'def:escape'
                    line_esc_ctx = 'def:line-continue'
            else:
                esc_ctx = None
                line_esc_ctx = None

            string += ctx.format(indent, esc_ctx, line_esc_ctx)

        string += indent*2 + '<context id="%s">\n' % (self.id,)
        string += indent*3 + '<include>\n'
        for ctx in self.contexts:
            string += indent*4 + '<context ref="%s"/>\n' % (ctx.id,)
        string += indent*3 + '</include>\n'
        string += indent*2 + '</context>\n'

        string += indent + '</definitions>\n'
        return string

    def format(self, indent='  '):
        string = self.format_header(indent)
        string += self.format_styles(indent)
        string += self.format_contexts(indent)
        string += self.format_footer(indent)
        return string

class Context(object):
    def __init__(self, name, _name, style):
        object.__init__(self)
        assert (name or _name) and style
        self.name = name
        self._name = _name
        self.style_name = style
        self.style = style.replace(' ', '-').lower()
        self.id = normalize_id(name or _name)
        self.is_container = False

    def format(self, indent, esc_ctx, line_esc_ctx):
        print "Implement me: %s.format()" % (type(self).__name__,)
        return indent*2 + '<context id="%s"/>\n' % (self.id)

    def format_escape(self, indent, esc_ctx, line_esc_ctx):
        string = ""
        if self.is_container and esc_ctx is not None:
            string += indent*3 + '<include>\n'
            string += indent*4 + '<context ref="%s"/>\n' % (esc_ctx,)
            string += indent*4 + '<context ref="%s"/>\n' % (line_esc_ctx,)
            string += indent*3 + '</include>\n'
        return string

class KeywordList(Context):
    def __init__(self, name, _name, style, keywords, case_sensitive,
                 match_empty_string_at_beginning,
                 match_empty_string_at_end,
                 beginning_regex, end_regex):
        Context.__init__(self, name, _name, style)
        self.keywords = keywords
        self.case_sensitive = case_sensitive # ???
        self.match_empty_string_at_beginning = match_empty_string_at_beginning
        self.match_empty_string_at_end = match_empty_string_at_end
        self.beginning_regex = beginning_regex
        self.end_regex = end_regex

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s">\n' % (self.id, self.style)

        if self.beginning_regex:
            string += indent*3 + '<prefix>%s</prefix>\n' % (escape_regex(self.beginning_regex),)
        elif not self.match_empty_string_at_beginning:
            string += indent*3 + '<prefix></prefix>\n'

        if self.end_regex:
            string += indent*3 + '<suffix>%s</suffix>\n' % (escape_regex(self.end_regex),)
        elif not self.match_empty_string_at_end:
            string += indent*3 + '<suffix></suffix>\n'

        for kw in self.keywords:
            string += indent*3 + '<keyword>%s</keyword>\n' % (escape_regex(kw),)

        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

class PatternItem(Context):
    def __init__(self, name, _name, style, pattern):
        Context.__init__(self, name, _name, style)
        assert pattern
        self.pattern = pattern

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s">\n' % (self.id, self.style)
        string += indent*3 + '<match>%s</match>\n' % (escape_regex(self.pattern),)
        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

class LineComment(Context):
    def __init__(self, name, _name, style, start):
        Context.__init__(self, name, _name, style)
        assert start
        self.start = start
        self.is_container = True

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s" end-at-line-end="true">\n' % (self.id, self.style)
        string += indent*3 + '<start>%s</start>\n' % (escape_regex(self.start),)
        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

class BlockComment(Context):
    def __init__(self, name, _name, style, start, end):
        Context.__init__(self, name, _name, style)
        assert start and end
        self.start = start
        self.end = end
        self.is_container = True

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s">\n' % (self.id, self.style)
        string += indent*3 + '<start>%s</start>\n' % (escape_regex(self.start),)
        string += indent*3 + '<end>%s</end>\n' % (escape_regex(self.end),)
        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

class String(Context):
    def __init__(self, name, _name, style, start, end, end_at_line_end):
        Context.__init__(self, name, _name, style)
        assert start and end
        self.start = start
        if end and end.endswith("\\n"):
            end = end[:-2]
            end_at_line_end = True
        self.end = end
        self.end_at_line_end = end_at_line_end
        self.is_container = True

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s"' % (self.id, self.style)
        if self.end_at_line_end:
            string += ' end-at-line-end="true"'
        string += '>\n'

        if self.start:
            string += indent*3 + '<start>%s</start>\n' % (escape_regex(self.start),)
        if self.end:
            string += indent*3 + '<end>%s</end>\n' % (escape_regex(self.end),)

        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

class SyntaxItem(Context):
    def __init__(self, name, _name, style, start, end):
        Context.__init__(self, name, _name, style)
        assert start and end
        self.start = start
        self.end = end
        self.end_at_line_end = False
        if end and end.endswith("\\n"):
            self.end = end[:-2]
            self.end_at_line_end = True
        self.is_container = True

    def format(self, indent, esc_ctx, line_esc_ctx):
        string = indent*2 + '<context id="%s" style-ref="%s"' % (self.id, self.style)
        if self.end_at_line_end:
            string += ' end-at-line-end="true"'
        string += '>\n'

        if self.start:
            string += indent*3 + '<start>%s</start>\n' % (escape_regex(self.start),)
        if self.end:
            string += indent*3 + '<end>%s</end>\n' % (escape_regex(self.end),)

        string += self.format_escape(indent, esc_ctx, line_esc_ctx)
        string += indent*2 + '</context>\n'
        return string

def first_child(node):
    child = node.firstChild
    while child is not None and child.nodeType != dom.Node.ELEMENT_NODE:
        child = child.nextSibling
    return child
def next_sibling(node):
    next = node.nextSibling
    while next is not None and next.nodeType != dom.Node.ELEMENT_NODE:
        next = next.nextSibling
    return next

def parseLineComment(cur, name, _name, style):
    child = first_child(cur)
    assert child is not None and child.tagName == "start-regex"
    return LineComment(name, _name, style, child.firstChild.nodeValue)

def parseBlockComment(cur, name, _name, style):
    start_regex = None
    end_regex = None
    child = first_child(cur)

    while child is not None:
        if child.tagName == "start-regex":
            start_regex = child.firstChild.nodeValue
        elif child.tagName == "end-regex":
            end_regex = child.firstChild.nodeValue
        child = next_sibling(child)

    assert start_regex is not None
    assert end_regex is not None

    return BlockComment(name, _name, style, start_regex, end_regex)

def parseString(cur, name, _name, style):
    start_regex = None
    end_regex = None
    end_at_line_end = True

    prop = cur.getAttribute("end-at-line-end")
    if prop:
        if prop in ["TRUE", "1"]:
            end_at_line_end = True
        else:
            end_at_line_end = False

    child = first_child(cur)

    while child is not None:
        if child.tagName == "start-regex":
            start_regex = child.firstChild.nodeValue
        elif child.tagName == "end-regex":
            end_regex = child.firstChild.nodeValue
        child = next_sibling(child)

    assert start_regex is not None
    assert end_regex is not None

    return String(name, _name, style, start_regex, end_regex, end_at_line_end)

def parseKeywordList(cur, name, _name, style):
    case_sensitive = True
    match_empty_string_at_beginning = True
    match_empty_string_at_end = True
    beginning_regex = None
    end_regex = None
    keywords = []

    prop = cur.getAttribute("case-sensitive")
    if prop:
        if prop in ["TRUE", "1"]:
            case_sensitive = True
        else:
            case_sensitive = False

    prop = cur.getAttribute("match-empty-string-at-beginning")
    if prop:
        if prop in ["TRUE", "1"]:
            match_empty_string_at_beginning = True
        else:
            match_empty_string_at_beginning = False

    prop = cur.getAttribute("match-empty-string-at-end")
    if prop:
        if prop in ["TRUE", "1"]:
            match_empty_string_at_end = True
        else:
            match_empty_string_at_end = False

    prop = cur.getAttribute("beginning-regex")
    if prop:
        beginning_regex = prop

    prop = cur.getAttribute("end-regex")
    if prop:
        end_regex = prop

    child = first_child(cur)

    while child is not None:
        if child.tagName == "keyword":
            keywords.append(child.firstChild.nodeValue)
        child = next_sibling(child)

    assert keywords

    return KeywordList(name, _name, style, keywords, case_sensitive,
                       match_empty_string_at_beginning,
                       match_empty_string_at_end,
                       beginning_regex, end_regex)

def parsePatternItem(cur, name, _name, style):
    child = first_child(cur)
    assert child is not None and child.tagName == "regex"
    return PatternItem(name, _name, style, child.firstChild.nodeValue)

def parseSyntaxItem(cur, name, _name, style):
    start_regex = None
    end_regex = None

    child = first_child(cur)

    while child is not None:
        if child.tagName == "start-regex":
            start_regex = child.firstChild.nodeValue
        elif child.tagName == "end-regex":
            end_regex = child.firstChild.nodeValue
        child = next_sibling(child)

    assert start_regex is not None
    assert end_regex is not None

    return SyntaxItem(name, _name, style, start_regex, end_regex)

def parseTag(cur):
    _name = None
    name = None

    _name = cur.getAttribute("_name")
    name = cur.getAttribute("name")
    assert name or _name
    style = cur.getAttribute("style") or "Normal"

    if cur.tagName == "line-comment":
        ctx = parseLineComment(cur, name, _name, style)
    elif cur.tagName == "block-comment":
        ctx = parseBlockComment(cur, name, _name, style)
    elif cur.tagName == "string":
        ctx = parseString(cur, name, _name, style)
    elif cur.tagName == "keyword-list":
        ctx = parseKeywordList(cur, name, _name, style)
    elif cur.tagName == "pattern-item":
        ctx = parsePatternItem(cur, name, _name, style)
    elif cur.tagName == "syntax-item":
        ctx = parseSyntaxItem(cur, name, _name, style)
    else:
        print "Unknown tag: %s" % (cur.tagName,)
        ctx = None

    return ctx

def parse_file(filename):
    doc = dom.parse(filename)
    node = doc.documentElement
    contexts = []
    esc_char = None

    assert node.tagName == "language"

    lang_file = LangFile(node.getAttribute("id"),
                         node.getAttribute("name"),
                         node.getAttribute("_name"),
                         node.getAttribute("section"),
                         node.getAttribute("_section"),
                         node.getAttribute("mimetypes"),
                         node.getAttribute("globs"),
                         filename)

    node = first_child(node)
    assert node is not None

    while node is not None:
        if node.tagName == "escape-char":
            lang_file.set_esc_char(node.firstChild.nodeValue)
        else:
            lang_file.add_context(parseTag(node))
        node = next_sibling(node)

    return lang_file

if __name__ == '__main__':
    import sys

    if not sys.argv[1:]:
        print "usage: %s LANG_FILE" % (sys.argv[0])
        sys.exit(1)

    lang_file = parse_file(sys.argv[1])
    sys.stdout.write(lang_file.format())
