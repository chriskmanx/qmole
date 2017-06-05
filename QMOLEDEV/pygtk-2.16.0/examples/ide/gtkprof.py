#!/usr/bin/env python

import profile, pstats, fpformat

import pygtk
pygtk.require('2.0')
import gtk

class PStatWindow(gtk.Window):
    def __init__(self, stats):
        gtk.Window.__init__(self)
        self.connect("destroy", self.quit)
        self.connect("delete_event", self.quit)
        self.set_title("Profile Statistics")

        self.stats = stats

        box1 = gtk.VBox()
        self.add(box1)
        box1.show()

        text = `stats.total_calls` + " function calls "
        if stats.total_calls != stats.prim_calls:
            text = text + "( " + `stats.prim_calls` + " primitive calls) "
        text = text + "in " + fpformat.fix(stats.total_tt, 3) + " CPU seconds"
        label = gtk.Label(text)
        label.set_padding(2, 2)
        box1.pack_start(label, expand=False)
        label.show()

        swin = gtk.ScrolledWindow()
        swin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        box1.pack_start(swin)
        swin.show()

        titles = [('ncalls', 40), ('tottime', 50), ('percall', 50),
                  ('cumtime', 50), ('percall', 50),
                  ('filename:lineno(function)', 10)]
        ls = gtk.ListStore(*((str,)*len(titles)))
        list = gtk.TreeView(ls)
        for n in range(len(titles)):
            cell = gtk.CellRendererText()
            cell.set_property('xalign', 1.0)
            tvc = gtk.TreeViewColumn(titles[n][0], cell, text=n)
            tvc.set_min_width(titles[n][1])
            list.append_column(tvc)
        list.set_size_request(500, 200)
        self.list = list
        list.set_border_width(10)
        swin.add(list)
        list.show()

        self.insert_stats()

        separator = gtk.HSeparator()
        box1.pack_start(separator, expand=False)
        separator.show()

        box2 = gtk.VBox(spacing=10)
        box2.set_border_width(10)
        box1.pack_start(box2, expand=False)
        box2.show()

        button = gtk.Button("close")
        button.connect("clicked", self.quit)
        self.close_button = button
        box2.pack_start(button)
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()
        button.show()

    def quit(self, *args):
        self.hide()
        self.destroy()
        gtk.main_quit()

    def get_stats_list(self):
        if self.stats.fcn_list:
            return self.stats.fcn_list[:]
        else:
            return self.stats.stats.keys()

    def insert_stats(self):
        list = self.get_stats_list()
        if list:
            row = [None] * 6
            model = self.list.get_model()
            for func in list:
                cc,nc,tt,ct,callers = self.stats.stats[func]
                row[0] = `nc`
                if nc != cc:
                    row[0] = row[0] + '/' + `cc`
                row[1] = fpformat.fix(tt, 3)
                if nc == 0:
                    row[2] = ''
                else:
                    row[2] = fpformat.fix(tt/nc, 3)
                row[3] = fpformat.fix(ct, 3)
                if cc == 0:
                    row[4] = ''
                else:
                    row[4] = fpformat.fix(ct/cc, 3)
                file,line,name = func
                row[5] = file + ":" + `line` + "(" + name + \
                         ")"
                self.list.get_model().append(row)
        return

def run(cmd):
    prof = profile.Profile()
    try:
        stats = pstats.Stats(prof.run(cmd))
    except SystemExit:
        pass
    stats.strip_dirs().sort_stats("time", "module", "name")
    win = PStatWindow(stats)
    win.show()
    gtk.main()

def run_file(file):
    return run('execfile("' + file + '")')


if __name__ == '__main__':
    import sys, os
    if not sys.argv[1:]:
        print "usage: gtkprof.py scriptfile [args ...]"
        sys.exit(2)
    filename = sys.argv[1]
    del sys.argv[0]
    sys.path.insert(0, os.path.dirname(filename))

    run_file(filename)
