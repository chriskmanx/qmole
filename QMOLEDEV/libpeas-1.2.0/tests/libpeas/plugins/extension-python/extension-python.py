# -*- coding: utf-8 -*-
# ex:set ts=4 et sw=4 ai:

from gi.repository import GObject, Introspection, Peas

class CallablePythonPlugin(GObject.Object, Introspection.Callable):
    def do_call_with_return(self):
        return "Hello, World!";

    def do_call_no_args(self):
        pass

    def do_call_single_arg(self):
        return True

    def do_call_multi_args(self, in_, inout):
        return (inout, in_)

class PropertiesPythonPlugin(GObject.Object, Introspection.Properties):
    construct_only = GObject.property(type=str)

    read_only = GObject.property(type=str, default="read-only")
                                      
    write_only = GObject.property(type=str)

    readwrite = GObject.property(type=str, default="readwrite")

class ActivatablePythonExtension(GObject.Object, Peas.Activatable):
    object = GObject.property(type=GObject.Object)

    def do_activate(self):
        pass

    def do_deactivate(self):
        pass

    def update_state(self):
        pass
