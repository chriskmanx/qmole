namespace PeasDemo {
  public class ValaHelloPlugin : Peas.ExtensionBase, Peas.Activatable {
    private Gtk.Widget label;
    public Object object { get; construct; }

    public ValaHelloPlugin () {
      Object ();
    }

    public void activate () {
      var window = object as Gtk.Window;

      label = new Gtk.Label ("Hello World from Vala!");
      var box = window.get_child () as Gtk.Box;
      box.pack_start (label);
      label.show ();
    }

    public void deactivate () {
      var window = object as Gtk.Window;

      var box = window.get_child () as Gtk.Box;
      box.remove (label);
    }

    public void update_state () {
    }
  }

  public class ValaPluginConfig : Peas.ExtensionBase, PeasGtk.Configurable {
    public ValaPluginConfig () {
      Object ();
    }

    public Gtk.Widget create_configure_widget () {
      string text = "This is a configuration dialog for the ValaHello plugin.";
      return new Gtk.Label (text);
    }
  }
}

[ModuleInit]
public void peas_register_types (GLib.TypeModule module) {
  var objmodule = module as Peas.ObjectModule;
  objmodule.register_extension_type (typeof (Peas.Activatable),
                                     typeof (PeasDemo.ValaHelloPlugin));
  objmodule.register_extension_type (typeof (PeasGtk.Configurable),
                                     typeof (PeasDemo.ValaPluginConfig));
}
