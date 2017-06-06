# vim: set fileencoding=utf-8 :

# Copyright (C) 2007 Insecure.Com LLC.
#
# Author: João Paulo de Souza Medeiros <ignotus21@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

import xml.sax
import xml.sax.saxutils
from xml.sax.xmlreader import AttributesImpl as Attributes



def convert_to_utf8(text):
    """
    """
    return text.encode('utf8', 'replace')



class XMLNode:
    """
    """
    def __init__(self, name):
        """
        """
        self.__name = name
        self.__text = ""
        self.__attrs = dict()
        self.__children = []


    def set_text(self, text):
        """
        """
        self.__text = text


    def get_text(self):
        """
        """
        return self.__text


    def set_name(self, name):
        """
        """
        self.__name = name


    def get_name(self):
        """
        """
        return self.__name


    def add_attr(self, key, value):
        """
        """
        self.__attrs[key] = value


    def add_child(self, child):
        """
        """
        self.__children.append(child)


    def get_keys(self):
        """
        """
        return self.__attrs.keys()


    def get_attr(self, attr):
        """
        """
        if self.__attrs.has_key(attr):
            return self.__attrs[attr]

        return None


    def get_attrs(self):
        """
        """
        return self.__attrs


    def get_children(self):
        """
        """
        return self.__children


    def query_children(self, name, attr, value, first=False, deep=False):
        """
        """
        result = []

        for child in self.__children:

            if child.get_name() == name:

                if child.get_attrs().has_key(attr):

                    c_value = child.get_attr(attr)

                    if c_value == value or c_value == str(value):
                        result.append(child)

            if deep:

                c_result = child.query_children(name, attr, value, first, deep)

                if c_result != None:

                    if first:
                        return c_result

                    else:
                        result.extend(c_result)

        if first and len(result) > 0:
            return result[0]

        if first:
            return None

        return result


    def search_children(self, name, first=False, deep=False):
        """
        """
        result = []

        for child in self.__children:

            if child.get_name() == name:

                result.append(child)

                if first:
                    return result[0]

            if deep:

                c_result = child.search_children(name, first, deep)

                if c_result != None and c_result != []:

                    if first:
                        return c_result

                    else:
                        result.extend(c_result)

        if first:
            return None

        return result



class XMLWriter(xml.sax.saxutils.XMLGenerator):
    """
    """
    def __init__(self, file, root=None, encoding="utf-8"):
        """
        """
        xml.sax.saxutils.XMLGenerator.__init__(self, file, encoding)

        self.__root = root


    def set_root(self, root):
        """
        """
        self.__root = root


    def write(self):
        """
        """
        self.startDocument()
        self.write_xml_node([self.__root])
        self.endDocument()


    def write_xml_node(self, root):
        """
        """
        for child in root:

            self.startElement(child.get_name(), Attributes(child.get_attrs()))

            if child.get_text() != "":
                self.characters(child.get_text())

            self.write_xml_node(child.get_children())

            self.endElement(child.get_name())



class XMLReader(xml.sax.ContentHandler):
    """
    """
    def __init__(self, file=None):
        """
        """
        self.__text = ""
        self.__status = []

        self.__file = file
        self.__root = None

        self.__parser = xml.sax.make_parser();
        self.__parser.setContentHandler(self);


    def set_file(self, file, root):
        """
        """
        self.__file = file


    def get_file(self):
        """
        """
        return self.__file


    def get_root(self):
        """
        """
        return self.__root


    def parse(self):
        """
        """
        if self.__file != None:
            self.__parser.parse(self.__file)


    def startDocument(self):
        """
        """
        pass


    def startElement(self, name, attrs):
        """
        """
        # create new node
        node = XMLNode(name)

        # putting attributes and values in node
        for attr in attrs.getNames():
            node.add_attr(attr, convert_to_utf8(attrs.get(attr).strip()))

        # who is my father?
        if len(self.__status) > 0:
            self.__status[-1].add_child(node)

        if self.__root == None:
            self.__root = node

        self.__status.append(node)


    def endElement(self, name):
        """
        """
        self.__status[-1].set_text(convert_to_utf8(self.__text.strip()))

        self.__text = ""
        self.__status.pop()


    def endDocument(self):
        """
        """
        pass


    def characters(self, text):
        """
        """
        self.__text += text



if __name__ == "__main__":

    import sys

    reader = XMLReader(sys.argv[1])
    reader.parse()

    root = reader.get_root()

    writer = XMLWriter(open("test.xml", 'w'), root)
    writer.write()
