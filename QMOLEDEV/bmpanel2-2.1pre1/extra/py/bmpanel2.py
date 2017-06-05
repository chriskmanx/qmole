#!/usr/bin/env python
# encoding: utf-8

"""
Bmpanel2 classes for python.

 1. Theme/config parser/deparser (read/write).
 2. Remote control, currently only one signal (reload config).
 3. Themes operations (list, install, create bundle).
 4. Config operations (all variables as a python interface, get/set,
    etc).
"""

import sys, os

#----------------------------------------------------------------------
# Config parser.
#
# This version may be slow, but it should do some extra stuff, like
# keeping comments and formatting. It should be really friendly in that
# kind of area.
#----------------------------------------------------------------------

def _parse_indent(line):
	"""
	This function is used for parsing indents, it returns the
	contents of an indent area and the count of indent symbols.
	"""
	offset = 0
	contents = ""
	for c in line:
		if c.isspace():
			offset += 1
			contents = contents + c
		else:
			break

	return (contents, offset)

# ConfigNode types
CONFIG_NODE_NAME_VALUE	= 1
CONFIG_NODE_COMMENT	= 2
CONFIG_NODE_EMPTY	= 3

class ConfigNode:
	def __init__(self, **kw):
		"""
		Init a node with initial values. Possible are:
		 - 'name': The name of the node. Or it also serves as
		   contents of the comment line, you should include '#'
		   symbol in that case too.
		 - 'value': The value of the node.
		 - 'type': The type - CONFIG_NODE_*, the default is
		   CONFIG_NODE_NAME_VALUE.
		 - 'parent': The parent of this node.
		"""
		self.name = None
		if 'name' in kw:
			self.name = kw['name']
		self.value = None
		if 'value' in kw:
			self.value = kw['value']
		self.type = CONFIG_NODE_NAME_VALUE
		if 'type' in kw:
			self.type = kw['type']
		self.parent = None
		if 'parent' in kw:
			self.parent = kw['parent']
		self.children = []
		self.indent_contents = ""
		self.indent_offset = 0
		self.children_offset = -1

	def is_child(self, indent_offset):
		"""
		Using 'indent_offset' figures out is the node with that
		offset is a child of this node. Also updates
		'children_offset' implicitly.
		"""
		if self.children_offset != -1:
			return self.children_offset == indent_offset
		elif self.indent_offset < indent_offset:
			self.children_offset = indent_offset
			return True
		return False

	def parse(self, line):
		"""
		Parse a line of text to a node.
		"""
		# check empty line
		sline = line.strip()
		if sline == "":
			self.type = CONFIG_NODE_EMPTY
			return

		# non-empties have indent, parse it
		(indent_contents, indent_offset) = _parse_indent(line)
		self.indent_contents = indent_contents
		self.indent_offset = indent_offset

		# check comment (first non-indent symbol: #)
		if line[indent_offset] == "#":
			self.type = CONFIG_NODE_COMMENT
			self.name = line[indent_offset:]
			return

		# and finally try to parse name/value
		self.type = CONFIG_NODE_NAME_VALUE

		cur = indent_offset

		# name
		name_beg = cur
		while cur < len(line) and not line[cur].isspace():
			cur += 1
		name_end = cur
		self.name = line[name_beg:name_end]

		# value
		value = line[name_end:].lstrip()
		if value != "":
			self.value = value

	def make_child_of(self, parent):
		"""
		Make this node a child of the 'parent'. This method
		doesn't add 'self' to the children list of the 'parent'.
		You should do it manually. Updates 'children_offset'
		implicitly.
		"""
		if parent.children_offset == -1:
			parent.children_offset = parent.indent_offset + 1
		co = parent.children_offset
		self.indent_contents = co * "\t"
		self.indent_offset = co
		self.parent = parent

	def __getitem__(self, item):
		for c in self.children:
			if c.name == item:
				return c
		raise KeyError, item

#----------------------------------------------------------------------
# ConfigFormat
#----------------------------------------------------------------------
class ConfigFormat(ConfigNode):
	def __init__(self, filename):
		"""
		Parse a config format file from the 'filename'.
		"""
		ConfigNode.__init__(self)
		self.indent_offset = -1
		self.filename = filename

		data = ""
		try:
			with file(filename, "r") as f:
				data = f.read()
		except IOError:
			pass
		lines = data.splitlines()
		nodes = []
		parent = self
		for line in lines:
			node = ConfigNode()
			node.parse(line)
			nodes.append(node)

			# add tree info
			if node.type != CONFIG_NODE_NAME_VALUE:
				continue

			if parent.indent_offset < node.indent_offset:
				if parent.is_child(node.indent_offset):
					node.parent = parent
					parent.children.append(node)
					parent = node
			else:
				while parent.indent_offset >= node.indent_offset:
					parent = parent.parent

				if parent.is_child(node.indent_offset):
					node.parent = parent
					parent.children.append(node)
					parent = node

		self.nodes = nodes

	def save(self, filename):
		"""
		Save config format file in the 'filename'. Function makes sure
		that the dir for 'filename' exists, if not - tries to create it.
		"""
		d = os.path.dirname(filename)
		if not os.path.exists(d):
			os.makedirs(d)
		with file(filename, "w") as f:
			for node in self.nodes:
				f.write(node.indent_contents)
				if node.name:
					f.write(node.name)
				if node.value:
					f.write(" ")
					f.write(node.value)
				f.write("\n")

	def _find_last_node(self, root):
		"""
		Find an index in the nodes list of the last node in the
		tree chain::

		 ------------------------------------------
		 one <---- ('root')
 		 	two
		 	three
		 	four
		 		five
		 	six
		 		seven <----(this one)
		 ------------------------------------------
		"""
		if not len(root.children):
			try:
				return self.nodes.index(root)
			except ValueError:
				return 0

		return self._find_last_node(root.children[-1])

	def append_node_after(self, node, after):
		"""
		Append 'node' after another node. Handles both tree and
		nodes list.
		"""
		parent = after.parent
		nodei = parent.children.index(after)

		node.make_child_of(parent)

		i = self._find_last_node(after)
		parent.children.insert(nodei+1, node)
		self.nodes.insert(i+1, node)

	def append_node_as_child(self, node, parent):
		"""
		Append 'node' as a child of the 'parent'. Handles both
		tree and nodes list.
		"""
		node.make_child_of(parent)

		i = self._find_last_node(parent)
		parent.children.append(node)
		self.nodes.insert(i+1, node)

	def remove_node(self, node):
		"""
		Remove 'node' and all its children. Handles both tree
		and nodes list.
		"""
		for c in node.children:
			self.remove_node(c)

		parent = node.parent
		if not parent:
			return
		parent.children.remove(node)
		self.nodes.remove(node)

#----------------------------------------------------------------------
# XDG functions
#----------------------------------------------------------------------
def XDG_get_config_home():
	"""
	Return XDG_CONFIG_HOME directory according to XDG spec.
	"""
	xdghome = os.getenv("XDG_CONFIG_HOME")
	if not xdghome:
		xdghome = os.path.join(os.getenv("HOME"), ".config")
	return xdghome

def XDG_get_data_dirs():
	"""
	Return XDG_DATA_HOME + XDG_DATA_DIRS array according to XDG spec.
	"""
	ret = []
	xdgdata = os.getenv("XDG_DATA_HOME")
	if not xdgdata:
		xdgdata = os.path.join(os.getenv("HOME"), ".local/share")
	ret.append(xdgdata)

	xdgdirs = os.getenv("XDG_DATA_DIRS")
	if xdgdirs:
		ret += xdgdirs.split(":")
	return ret

#----------------------------------------------------------------------
# Bmpanel2Config
#----------------------------------------------------------------------
class Bmpanel2Config:
	def _get_int_value(self, name, default):
		try:
			ret = int(self.tree[name].value)
		except:
			ret = default
		return ret

	def _set_int_value(self, name, value):
		s = "{0}".format(value)
		try:
			node = self.tree[name]
			node.value = s
		except:
			node = ConfigNode(name=name, value=s)
			self.tree.append_node_as_child(node, self.tree)
		self.fire_unsaved_notifiers(True)

	def _get_str_value(self, name, default):
		try:
			ret = self.tree[name].value
		except:
			ret = default
		return ret

	def _set_str_value(self, name, value):
		try:
			node = self.tree[name]
			node.value = value
		except:
			node = ConfigNode(name=name, value=value)
			self.tree.append_node_as_child(node, self.tree)
		self.fire_unsaved_notifiers(True)

	def _get_bool_value(self, name):
		try:
			node = self.tree[name]
			return True
		except:
			return False

	def _set_bool_value(self, name, value):
		try:
			node = self.tree[name]
			if not value:
				self.tree.remove_node(node)
		except:
			if value:
				node = ConfigNode(name=name)
				self.tree.append_node_as_child(node, self.tree)
		self.fire_unsaved_notifiers(True)
	#--------------------------------------------------------------
	def __init__(self):
		self.path = os.path.join(XDG_get_config_home(), "bmpanel2/bmpanel2rc")
		self.tree = ConfigFormat(self.path)

		# an array of function pointers
		# function(state)
		# where 'state' is a boolean
		# functions are being called when there are:
		# True - unsaved changes are here
		# False - config was just saved, no unsaved changes
		self.unsaved_notifiers = []

	def add_unsaved_notifier(self, notifier):
		self.unsaved_notifiers.append(notifier)

	def fire_unsaved_notifiers(self, status):
		for n in self.unsaved_notifiers:
			n(status)

	def save(self):
		self.tree.save(self.path)
		self.fire_unsaved_notifiers(False)
		#self.tree.save("testrc")

	def get_theme(self):
		return self._get_str_value('theme', 'native')

	def set_theme(self, value):
		self._set_str_value('theme', value)

	def get_task_death_threshold(self):
		return self._get_int_value('task_death_threshold', 50)

	def set_task_death_threshold(self, value):
		self._set_int_value('task_death_threshold', value)

	def get_drag_threshold(self):
		return self._get_int_value('drag_threshold', 30)

	def set_drag_threshold(self, value):
		self._set_int_value('drag_threshold', value)

	def get_task_urgency_hint(self):
		return self._get_bool_value('task_urgency_hint')

	def set_task_urgency_hint(self, value):
		self._set_bool_value('task_urgency_hint', value)

	def get_clock_prog(self):
		return self._get_str_value('clock_prog', None)

	def set_clock_prog(self, value):
		self._set_str_value('clock_prog', value)

	# TODO: launchbar

#----------------------------------------------------------------------
# Bmpanel2Remote
#----------------------------------------------------------------------
class Bmpanel2Remote:
	def __init__(self):
		self.started_with_theme = False
		self.pid = None
		self.update_pid()

	def update_pid(self):
		# find pid
		try:
			self.pid = int(os.popen("pidof bmpanel2").read().splitlines()[0])
		except:
			return
		# check if bmpanel2 was started with "--theme" parameter
		try:
			args = os.popen("ps --no-heading o %a -p {0}".format(self.pid)).read().splitlines()[0]
			self.started_with_theme = args.find("--theme") != -1
		except:
			pass

	def reconfigure(self):
		if self.pid:
			os.kill(self.pid, 10)

#----------------------------------------------------------------------
# Bmpanel2Themes
#----------------------------------------------------------------------
class Theme:
	def __init__(self, dirname, name=None, author=None, path=None):
		self.dirname = dirname
		self.name = name
		self.author = author
		self.path = path

class Bmpanel2Themes:
	def _try_load_theme(self, dirname, themefile):
		c = ConfigFormat(themefile)
		path = os.path.dirname(themefile)
		name = None
		author = None
		try:
			t = c['theme']
			name = t['name'].value
			author = t['author'].value
		except:
			pass

		if not dirname in self.themes:
			self.themes[dirname] = Theme(dirname, name, author, path)

	def _lookup_for_themes(self, d):
		try:
			files = os.listdir(d)
		except OSError:
			return

		for f in files:
			path = os.path.join(d, f)
			path = os.path.join(path, "theme")
			if os.path.exists(path):
				self._try_load_theme(f, path)

	def __init__(self):
		self.themes = {}
		dirs = XDG_get_data_dirs()
		for d in dirs:
			path = os.path.join(d, "bmpanel2/themes")
			self._lookup_for_themes(path)

		def get_dirname(theme):
			if theme.name:
				return theme.name
			else:
				return theme.dirname
		tmp = self.themes.values()
		tmp.sort(key=get_dirname)
		self.themes = tmp
#----------------------------------------------------------------------
# Bmpanel2Launchbar
#----------------------------------------------------------------------

class LaunchbarItem:
	def __init__(self, prog=None, icon=None):
		self.prog = prog
		self.icon = icon

class Bmpanel2Launchbar:
	def __init__(self, config):
		try:
			launchbar = config.tree['launchbar']
		except:
			launchbar = ConfigNode(name="launchbar")
			config.tree.append_node_as_child(launchbar, config.tree)

		self.launchbar = launchbar

	def __iter__(self):
		for c in self.launchbar.children:
			yield LaunchbarItem(c.value, c['icon'].value)

	def __getitem__(self, n):
		c = self.launchbar.children[n]
		return LaunchbarItem(c.value, c['icon'].value)

