/*
 * seedhello.js
 * This file is part of libpeas
 *
 * Copyright (C) 2009-2010 Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

var Gtk = imports.gi.Gtk;

var LABEL_STRING = "Seed Says Hello Too!";

print("LABEL_STRING=" +  LABEL_STRING);

function activatable_extension() {
}

activatable_extension.prototype = {
  activate: function() {
    print("SeedHelloPlugin.activate");
    this.object._seedhello_label = new Gtk.Label({ label: LABEL_STRING });
    this.object._seedhello_label.show();
    this.object.get_child().pack_start(this.object._seedhello_label);
  },
  deactivate: function() {
    print("SeedHelloPlugin.deactivate");
    this.object.get_child().remove(this.object._seedhello_label);
    this.object._seedhello_label.destroy();
  },
  update_state: function() {
    print("SeedHelloPlugin.update_state");
  }
};

function configurable_extension() {
  this.create_configure_widget = function () {
    return new Gtk.Label({ label: "Example of configuration dialog for a Seed plugin" });
  };
};

extensions = {
  'PeasActivatable': activatable_extension,
  'PeasGtkConfigurable': configurable_extension,
};
