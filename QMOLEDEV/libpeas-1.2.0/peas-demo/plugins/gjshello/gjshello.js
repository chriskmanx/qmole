const Gtk = imports.gi.Gtk;

var LABEL_STRING = "GJS Also Says Hello!";

print("LABEL_STRING=" +  LABEL_STRING);

function activatable_extension() {
}

activatable_extension.prototype = {
  activate: function() {
    print("GJSHelloPlugin.activate");
    this.object._gjshello_label = new Gtk.Label({ label: LABEL_STRING });
    this.object._gjshello_label.show();
    this.object.get_child().add(this.object._gjshello_label);
  },
  deactivate: function() {
    print("GJSHelloPlugin.deactivate");
    this.object.get_child().remove(this.object._gjshello_label);
    this.object._gjshello_label.destroy();
  },
  update_state: function() {
    print("GJSHelloPlugin.update_state");
  }
};

function configurable_extension() {
  this.create_configure_widget = function () {
    return new Gtk.Label({ label: "Example of configuration dialog for a GJS plugin" });
  };
};

var extensions = {
  'PeasActivatable': activatable_extension,
  'PeasGtkConfigurable': configurable_extension,
};
