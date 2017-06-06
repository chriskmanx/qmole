# vim: set fileencoding=utf-8 :

# Copyright (C) 2007, 2008 Insecure.Com LLC.
#
# Author: JoÃ£o Paulo de Souza Medeiros <ignotus21@gmail.com>
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

import gtk
import math
import time
import copy
import gobject

import radialnet.util.drawing as drawing
import radialnet.util.geometry as geometry
import radialnet.util.misc as misc

from radialnet.core.Coordinate import PolarCoordinate, CartesianCoordinate
from radialnet.core.Interpolation import Linear2DInterpolator
from radialnet.core.Graph import Graph, Node
from radialnet.gui.NodeWindow import NodeWindow
from radialnet.gui.Image import Icons


REGION_COLORS = [(1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
REGION_RED    = 0
REGION_YELLOW = 1
REGION_GREEN  = 2

SQUARE_TYPES = ['router', 'switch', 'wap']

ICON_DICT = {'router':      'router',
             'switch':      'switch',
             'wap':         'wireless',
             'firewall':    'firewall'}

POINTER_JUMP_TO = 0
POINTER_INFO    = 1
POINTER_GROUP   = 2
POINTER_FILL    = 3

LAYOUT_SYMMETRIC = 0
LAYOUT_WEIGHTED  = 1

INTERPOLATION_CARTESIAN = 0
INTERPOLATION_POLAR     = 1


class RadialNet(gtk.DrawingArea):
    """
    Radial network visualization widget
    """
    def __init__(self, layout=LAYOUT_SYMMETRIC):
        """
        Constructor method of RadialNet widget class
        @type  number_of_rings: number
        @param number_of_rings: Number of rings in radial layout
        """
        self.__center_of_widget = (0, 0)
        self.__graph = None

        self.__number_of_rings = 0
        self.__ring_gap = 30
        self.__min_ring_gap = 10

        self.__layout = layout
        self.__interpolation = INTERPOLATION_POLAR
        self.__interpolation_slow_in_out = True

        self.__animating = False
        self.__animation_rate = 1000 / 60 # 60Hz (human perception factor)
        self.__number_of_frames = 60

        self.__scale = 1.0
        self.__rotate = 225 # rotated so that single-host traceroute doesn't have overlapping hosts
        self.__translation = (0, 0)

        self.__button1_press = False
        self.__button2_press = False
        self.__button3_press = False

        self.__last_motion_point = None

        self.__fisheye = False
        self.__fisheye_ring = 0
        self.__fisheye_spread = 0.5
        self.__fisheye_interest = 2

        self.__show_address = True
        self.__show_hostname = True
        self.__show_icon = True
        self.__show_latency = False
        self.__show_ring = True
        self.__show_region = True
        self.__region_color = REGION_RED

        self.__node_views = dict()
        self.__last_group_node = None

        self.__pointer_status = POINTER_JUMP_TO

        self.__sorted_nodes = list()
        self.__reverse_sorted_nodes = list()

        self.__icon = Icons()

        super(RadialNet, self).__init__()

        self.connect('expose_event', self.expose)
        self.connect('button_press_event', self.button_press)
        self.connect('button_release_event', self.button_release)
        self.connect('motion_notify_event', self.motion_notify)
        self.connect('enter_notify_event', self.enter_notify)
        self.connect('leave_notify_event', self.leave_notify)
        self.connect('key_press_event', self.key_press)
        self.connect('key_release_event', self.key_release)
        self.connect('scroll_event', self.scroll_event)

        self.add_events(gtk.gdk.BUTTON_PRESS_MASK |
                        gtk.gdk.BUTTON_RELEASE_MASK |
                        gtk.gdk.ENTER_NOTIFY |
                        gtk.gdk.LEAVE_NOTIFY |
                        gtk.gdk.MOTION_NOTIFY |
                        gtk.gdk.NOTHING |
                        gtk.gdk.KEY_PRESS_MASK |
                        gtk.gdk.KEY_RELEASE_MASK |
                        gtk.gdk.POINTER_MOTION_HINT_MASK |
                        gtk.gdk.POINTER_MOTION_MASK |
                        gtk.gdk.SCROLL_MASK)

        self.set_flags(gtk.CAN_FOCUS)
        self.grab_focus()


    def graph_is_not_empty(function):
        """
        Decorator function to prevent the execution when graph not is set
        @type  function: function
        @param function: Protected function
        """
        def check_graph_status(*args):
            if args[0].__graph == None:
                return False
            return function(*args)

        return check_graph_status


    def not_is_in_animation(function):
        """
        Decorator function to prevent the execution when graph is animating
        @type  function: function
        @param function: Protected function
        """
        def check_animation_status(*args):
            if args[0].__animating == True:
                return False
            return function(*args)

        return check_animation_status


    def get_slow_inout(self):
        """
        """
        return self.__interpolation_slow_in_out


    def set_slow_inout(self, value):
        """
        """
        self.__interpolation_slow_in_out = value


    def get_region_color(self):
        """
        """
        return self.__region_color


    def set_region_color(self, value):
        """
        """
        self.__region_color = value


    def get_show_region(self):
        """
        """
        return self.__show_region


    def set_show_region(self, value):
        """
        """
        self.__show_region = value
        self.queue_draw()


    def get_pointer_status(self):
        """
        """
        return self.__pointer_status


    def set_pointer_status(self, pointer_status):
        """
        """
        self.__pointer_status = pointer_status


    def get_show_address(self):
        """
        """
        return self.__show_address


    def get_show_hostname(self):
        """
        """
        return self.__show_hostname


    def get_show_ring(self):
        """
        """
        return self.__show_ring


    def set_show_address(self, value):
        """
        """
        self.__show_address = value
        self.queue_draw()


    def set_show_hostname(self, value):
        """
        """
        self.__show_hostname = value
        self.queue_draw()


    def set_show_ring(self, value):
        """
        """
        self.__show_ring = value
        self.queue_draw()


    def get_min_ring_gap(self):
        """
        """
        return self.__min_ring_gap


    @graph_is_not_empty
    @not_is_in_animation
    def set_min_ring_gap(self, value):
        """
        """
        self.__min_ring_gap = int(value)

        if self.__ring_gap < self.__min_ring_gap:
            self.__ring_gap = self.__min_ring_gap

        self.__update_nodes_positions()
        self.queue_draw()

        return True


    def get_number_of_frames(self):
        """
        """
        return self.__number_of_frames


    @not_is_in_animation
    def set_number_of_frames(self, number_of_frames):
        """
        """
        if number_of_frames > 2:

            self.__number_of_frames = int(number_of_frames)
            return True

        self.__number_of_frames = 3
        return False


    @not_is_in_animation
    def update_layout(self):
        """
        """
        if self.__graph is None:
            return
        self.__animating = True
        self.__calc_interpolation(self.__graph.get_main_node())
        self.__livens_up()


    @not_is_in_animation
    def set_layout(self, layout):
        """
        """
        if self.__layout != layout:

            self.__layout = layout

            if self.__graph != None:

                self.__animating = True
                self.__calc_interpolation(self.__graph.get_main_node())
                self.__livens_up()

            return True

        return False


    def get_layout(self):
        """
        """
        return self.__layout


    @not_is_in_animation
    def set_interpolation(self, interpolation):
        """
        """
        self.__interpolation = interpolation

        return True


    def get_interpolation(self):
        """
        """
        return self.__interpolation


    def get_number_of_rings(self):
        """
        """
        return self.__number_of_rings


    def get_fisheye_ring(self):
        """
        """
        return self.__fisheye_ring


    def get_fisheye_interest(self):
        """
        """
        return self.__fisheye_interest


    def get_fisheye_spread(self):
        """
        """
        return self.__fisheye_spread


    def get_fisheye(self):
        """
        """
        return self.__fisheye


    def set_fisheye(self, enable):
        """
        """
        self.__fisheye = enable

        self.__update_nodes_positions()
        self.queue_draw()


    def set_fisheye_ring(self, value):
        """
        """
        self.__fisheye_ring = value
        self.__check_fisheye_ring()

        self.__update_nodes_positions()
        self.queue_draw()


    def set_fisheye_interest(self, value):
        """
        """
        self.__fisheye_interest = value

        self.__update_nodes_positions()
        self.queue_draw()


    def set_fisheye_spread(self, value):
        """
        """
        self.__fisheye_spread = value

        self.__update_nodes_positions()
        self.queue_draw()


    def get_show_icon(self):
        """
        """
        return self.__show_icon


    def set_show_icon(self, value):
        """
        """
        self.__show_icon = value
        self.queue_draw()


    def get_show_latency(self):
        """
        """
        return self.__show_latency


    def set_show_latency(self, value):
        """
        """
        self.__show_latency = value
        self.queue_draw()


    def get_scale(self):
        """
        """
        return self.__scale


    def get_zoom(self):
        """
        """
        return int(round(self.__scale * 100))


    def set_scale(self, scale):
        """
        """
        if scale >= 0.01:

            self.__scale = scale
            self.queue_draw()


    def set_zoom(self, zoom):
        """
        """
        if float(zoom) >= 1:

            self.set_scale( float(zoom) / 100.0 )
            self.queue_draw()


    def get_ring_gap(self):
        """
        """
        return self.__ring_gap


    @not_is_in_animation
    def set_ring_gap(self, ring_gap):
        """
        """
        if ring_gap >= self.__min_ring_gap:

            self.__ring_gap = ring_gap
            self.__update_nodes_positions()
            self.queue_draw()


    def scroll_event(self, widget, event):
        """
        """
        if event.direction == gtk.gdk.SCROLL_UP:
            self.set_scale(self.__scale + 0.01)

        if event.direction == gtk.gdk.SCROLL_DOWN:
            self.set_scale(self.__scale - 0.01)

        self.queue_draw()


    @graph_is_not_empty
    @not_is_in_animation
    def key_press(self, widget, event):
        """
        """
        key = gtk.gdk.keyval_name(event.keyval)

        if key == 'KP_Add':
            self.set_ring_gap(self.__ring_gap + 1)

        elif key == 'KP_Subtract':
            self.set_ring_gap(self.__ring_gap - 1)

        elif key == 'Page_Up':
            self.set_scale(self.__scale + 0.01)

        elif key == 'Page_Down':
            self.set_scale(self.__scale - 0.01)

        self.queue_draw()

        return True


    @graph_is_not_empty
    def key_release(self, widget, event):
        """
        """
        key = gtk.gdk.keyval_name(event.keyval)

        if key == 'c':
            self.__translation = (0, 0)

        elif key == 'r':
            self.__show_ring = not self.__show_ring

        elif key == 'a':
            self.__show_address = not self.__show_address

        elif key == 'h':
            self.__show_hostname = not self.__show_hostname

        elif key == 'i':
            self.__show_icon = not self.__show_icon

        elif key == 'l':
            self.__show_latency = not self.__show_latency

        self.queue_draw()

        return True


    @graph_is_not_empty
    @not_is_in_animation
    def enter_notify(self, widget, event):
        """
        """
        self.grab_focus()
        return False


    @graph_is_not_empty
    @not_is_in_animation
    def leave_notify(self, widget, event):
        """
        """
        for node in self.__graph.get_nodes():
            node.set_draw_info({'over':False})

        self.queue_draw()

        return False


    @graph_is_not_empty
    def button_press(self, widget, event):
        """
        Drawing callback
        @type  widget: GtkWidget
        @param widget: Gtk widget superclass
        @type  event: GtkEvent
        @param event: Gtk event of widget
        @rtype: boolean
        @return: Indicator of the event propagation
        """
        result = self.__get_node_by_coordinate(self.get_pointer())

        if event.button == 1: self.__button1_press = True

        # animate if node is pressed
        if self.__pointer_status == POINTER_JUMP_TO and event.button == 1:

            # prevent double animation
            if self.__animating == True: return False

            if result != None:

                node, point = result
                main_node = self.__graph.get_main_node()

                if node != main_node:

                    if node.get_draw_info('group') == True:

                        node.set_draw_info({'group':False})
                        node.set_subtree_info({'grouped':False,
                                               'group_node':None})

                    self.__animating = True
                    self.__calc_interpolation(node)
                    self.__livens_up()

        # group node if it's pressed
        elif self.__pointer_status == POINTER_GROUP and event.button == 1:

            # prevent group on animation
            if self.__animating == True: return False

            if result != None:

                node, point = result
                main_node = self.__graph.get_main_node()

                if node != main_node:

                    if node.get_draw_info('group') == True:

                        node.set_draw_info({'group':False})
                        node.set_subtree_info({'grouped':False,
                                               'group_node':None})

                    else:

                        self.__last_group_node = node

                        node.set_draw_info({'group':True})
                        node.set_subtree_info({'grouped':True,
                                               'group_node':node})

                self.__animating = True
                self.__calc_interpolation(self.__graph.get_main_node())
                self.__livens_up()

        # setting to show node's region
        elif self.__pointer_status == POINTER_FILL and event.button == 1:

            if result != None:

                node, point = result

                if node.get_draw_info('region') == self.__region_color:
                    node.set_draw_info({'region': None})

                else:
                    node.set_draw_info({'region': self.__region_color})

                self.queue_draw()

        # show node details
        elif event.button == 3 or self.__pointer_status == POINTER_INFO:

            if event.button == 3:
                self.__button3_press = True

            if result != None:

                xw, yw = self.window.get_origin()
                node, point = result
                x, y = point

                if node in self.__node_views.keys():

                    self.__node_views[node].restore(int(xw + x), int(yw + y))

                elif node.get_info('scanned'):

                    view = NodeWindow(node, (int(xw + x), int(yw + y)), self)
                    view.show_all()
                    self.__node_views[node] = view

        return False


    @graph_is_not_empty
    def button_release(self, widget, event):
        """
        Drawing callback
        @type  widget: GtkWidget
        @param widget: Gtk widget superclass
        @type  event: GtkEvent
        @param event: Gtk event of widget
        @rtype: boolean
        @return: Indicator of the event propagation
        """
        if event.button == 1:
            self.__button1_press = False

        if event.button == 2:
            self.__button2_press = False

        if event.button == 3:
            self.__button3_press = False

        self.grab_focus()

        return False


    @graph_is_not_empty
    def motion_notify(self, widget, event):
        """
        Drawing callback
        @type  widget: GtkWidget
        @param widget: Gtk widget superclass
        @type  event: GtkEvent
        @param event: Gtk event of widget
        @rtype: boolean
        @return: Indicator of the event propagation
        """
        xc, yc = self.__center_of_widget
        pointer = self.get_pointer()

        for node in self.__graph.get_nodes():
            node.set_draw_info({'over':False})

        result = self.__get_node_by_coordinate(self.get_pointer())

        if result != None:
            result[0].set_draw_info({'over':True})

        elif self.__button1_press == True and self.__last_motion_point != None:

            ax, ay = pointer
            ox, oy = self.__last_motion_point
            tx, ty = self.__translation

            self.__translation = (tx + ax - ox, ty - ay + oy)

        self.__last_motion_point = pointer

        self.grab_focus()
        self.queue_draw()
        
        return False


    def expose(self, widget, event):
        """
        Drawing callback
        @type  widget: GtkWidget
        @param widget: Gtk widget superclass
        @type  event: GtkEvent
        @param event: Gtk event of widget
        @rtype: boolean
        @return: Indicator of the event propagation
        """
        self.context = widget.window.cairo_create()

        self.context.rectangle(*event.area)
        self.context.set_source_rgb(1.0, 1.0, 1.0)
        self.context.fill()

        self.__draw()

        return False


    @graph_is_not_empty
    def __draw(self):
        """
        Drawing method
        """
        # getting allocation reference
        allocation = self.get_allocation()

        self.__center_of_widget = (allocation.width / 2,
                                   allocation.height / 2)

        aw, ah = allocation.width, allocation.height
        xc, yc = self.__center_of_widget

        ax, ay = self.__translation

        # xc = 320 yc = 240

        # -1.5 | -0.5 ( 480,  360)
        # -1.0 |  0.0 ( 320,  240)
        # -0.5 |  0.5 ( 160,  120)
        #  0.0 |  1.0 (   0,    0)
        #  0.5 |  1.5 (-160, -120)
        #  1.0 |  2.0 (-320, -240)
        #  1.5 |  2.5 (-480, -360)

        # scaling and translate
        factor = -(self.__scale - 1)

        self.context.translate(xc * factor + ax, yc * factor - ay)

        if self.__scale != 1.0:
            self.context.scale(self.__scale, self.__scale)

        # drawing over node's region
        if self.__show_region and not self.__animating:

            for node in self.__sorted_nodes:

                not_grouped = not node.get_draw_info('grouped')

                if node.get_draw_info('region') != None and not_grouped:

                    x, y = node.get_cartesian_coordinate()
                    xc, yc = self.__center_of_widget
                    r, g, b = REGION_COLORS[node.get_draw_info('region')]

                    start, final = node.get_draw_info('range')

                    i_radius = node.get_coordinate_radius()
                    f_radius = self.__calc_radius(self.__number_of_rings - 1)

                    is_fill_all = abs(final - start) == 360

                    final = math.radians(final + self.__rotate)
                    start = math.radians(start + self.__rotate)

                    self.context.move_to(xc, yc)
                    self.context.set_source_rgba(r, g, b, 0.1)
                    self.context.new_path()
                    self.context.arc(xc, yc, i_radius, -final, -start)
                    self.context.arc_negative(xc, yc, f_radius, -start, -final)
                    self.context.close_path()
                    self.context.fill()
                    self.context.stroke()

                    if not is_fill_all:

                        self.context.set_source_rgb(r, g, b)
                        self.context.set_line_width(1)

                        xa, ya = PolarCoordinate(i_radius, final).to_cartesian()
                        xb, yb = PolarCoordinate(f_radius, final).to_cartesian()

                        self.context.move_to(xc + xa, yc - ya)
                        self.context.line_to(xc + xb, yc - yb)
                        self.context.stroke()

                        xa, ya = PolarCoordinate(i_radius, start).to_cartesian()
                        xb, yb = PolarCoordinate(f_radius, start).to_cartesian()

                        self.context.move_to(xc + xa, yc - ya)
                        self.context.line_to(xc + xb, yc - yb)
                        self.context.stroke()

        # drawing network rings
        if self.__show_ring == True and self.__animating != True:

            for i in range(1, self.__number_of_rings):

                radius = self.__calc_radius(i)

                self.context.arc(xc, yc, radius, 0, 2 * math.pi)
                self.context.set_source_rgb(0.8, 0.8, 0.8)
                self.context.set_line_width(1)
                self.context.stroke()

        # drawing nodes and your connections
        for edge in self.__graph.get_edges():

            # check group constraints for edges
            a, b = edge.get_nodes()

            a_is_grouped = a.get_draw_info('grouped')
            b_is_grouped = b.get_draw_info('grouped')

            a_is_group = a.get_draw_info('group')
            b_is_group = b.get_draw_info('group')

            a_group = a.get_draw_info('group_node')
            b_group = b.get_draw_info('group_node')

            a_is_child = a in b.get_draw_info('children')
            b_is_child = b in a.get_draw_info('children')

            last_group = self.__last_group_node
            groups = [a_group, b_group]

            if last_group in groups and last_group != None:
                self.__draw_edge(edge)

            elif not a_is_grouped or not b_is_grouped:
            
                if not (a_is_group and b_is_child or b_is_group and a_is_child):
                    self.__draw_edge(edge)

            elif a_group != b_group:
                self.__draw_edge(edge)

        for node in self.__reverse_sorted_nodes:

            # check group constraints for nodes
            group = node.get_draw_info('group_node')
            grouped = node.get_draw_info('grouped')

            if group == self.__last_group_node or not grouped:
                self.__draw_node(node)


    def __draw_edge(self, edge):
        """
        Draw the connection between two nodes
        @type  : Edge
        @param : The second node that will be connected
        """
        a, b = edge.get_nodes()

        xa, ya = a.get_cartesian_coordinate()
        xb, yb = b.get_cartesian_coordinate()
        xc, yc = self.__center_of_widget

        a_children = a.get_draw_info('children')
        b_children = b.get_draw_info('children')

        latency = edge.get_weigths_mean()

        # check if isn't an hierarchy connection
        if a not in b_children and b not in a_children:
            self.context.set_source_rgba(1.0, 0.6, 0.1, 0.8)

        elif a.get_draw_info('no_route') or b.get_draw_info('no_route'):
            self.context.set_source_rgba(0.0, 0.0, 0.0, 0.8)

        else:
            self.context.set_source_rgba(0.1, 0.5, 1.0, 0.8)

        # calculating line thickness by latency
        if latency != None:

            min = self.__graph.get_min_edge_mean_weight()
            max = self.__graph.get_max_edge_mean_weight()

            if max != min:
                thickness = (latency - min) * 4 / (max - min) + 1

            else:
                thickness = 1

            self.context.set_line_width(thickness)

        else:

            self.context.set_dash([2, 2])
            self.context.set_line_width(1)

        self.context.move_to(xc + xa, yc - ya)
        self.context.line_to(xc + xb, yc - yb)
        self.context.stroke()

        self.context.set_dash([1, 0])

        if not self.__animating and self.__show_latency:

            if latency != None:

                self.context.set_font_size(8)
                self.context.set_line_width(1)
                self.context.move_to(xc + (xa + xb) / 2 + 1,
                                     yc - (ya + yb) / 2 + 4)
                self.context.show_text(str(round(latency, 2)))
                self.context.stroke()


    def __draw_node(self, node):
        """
        Draw nodes and your informations
        @type  : NetNode
        @param : The node will be draw
        """
        x, y = node.get_cartesian_coordinate()
        xc, yc = self.__center_of_widget
        r, g, b = node.get_draw_info('color')
        radius = node.get_draw_info('radius')

        type = node.get_info('device_type')

        x_gap = radius + 2
        y_gap = 0

        # draw group indication
        if node.get_draw_info('group') == True:

            x_gap += 5

            if type in SQUARE_TYPES:
                self.context.rectangle(xc + x - radius - 5,
                                       yc - y - radius - 5,
                                       2 * radius + 10,
                                       2 * radius + 10)

            else:
                self.context.arc(xc + x, yc - y, radius + 5, 0, 2 * math.pi)

            self.context.set_source_rgb(1.0, 1.0, 1.0)
            self.context.fill_preserve()

            if node.deep_search_child(self.__graph.get_node_by_id(0)):
                self.context.set_source_rgb(0.0, 0.0, 0.0)

            else:
                self.context.set_source_rgb(0.1, 0.5, 1.0)

            self.context.set_line_width(2)
            self.context.stroke()

        # draw over node
        if node.get_draw_info('over') == True:

            self.context.set_line_width(0)

            if type in SQUARE_TYPES:
                self.context.rectangle(xc + x - radius - 5,
                                       yc - y - radius - 5,
                                       2 * radius + 10,
                                       2 * radius + 10)

            else:
                self.context.arc(xc + x, yc - y, radius + 5, 0, 2 * math.pi)

            self.context.set_source_rgb(0.1, 0.5, 1.0)
            self.context.fill_preserve()
            self.context.stroke()

        # draw node
        if type in SQUARE_TYPES:
            self.context.rectangle(xc + x - radius,
                                   yc - y - radius,
                                   2 * radius,
                                   2 * radius)

        else:
            self.context.arc(xc + x, yc - y, radius, 0, 2 * math.pi)

        # draw icons
        if not self.__animating and self.__show_icon:

            icons = list()

            if type in ICON_DICT.keys():
                icons.append(self.__icon.get_pixbuf(ICON_DICT[type]))

            if node.get_info('filtered'):
                icons.append(self.__icon.get_pixbuf('padlock'))

            for icon in icons:

                self.context.set_source_pixbuf(icon,
                                               round(xc + x + x_gap),
                                               round(yc - y + y_gap - 6))
                self.context.paint()

                x_gap += 13

        # draw node text
        self.context.set_source_rgb(r, g, b)
        self.context.fill_preserve()

        if node.get_draw_info('valid') or node.get_id() == 0:
            self.context.set_source_rgb(0.0, 0.0, 0.0)

        else:
            self.context.set_source_rgb(0.1, 0.5, 1.0)

        if not self.__animating and self.__show_address:

            self.context.set_font_size(8)
            self.context.move_to(round(xc + x + x_gap),
                                 round(yc - y + y_gap + 4))

            hostname = node.get_info('hostname')

            if hostname != None and self.__show_hostname:
                self.context.show_text(hostname)

            elif node.get_info('ip') != None:
                self.context.show_text(node.get_info('ip'))

        self.context.set_line_width(1)
        self.context.stroke()


    def __check_fisheye_ring(self):
        """
        """
        if self.__fisheye_ring >= self.__number_of_rings:
            self.__fisheye_ring = self.__number_of_rings - 1


    def __set_number_of_rings(self, value):
        """
        """
        self.__number_of_rings = value
        self.__check_fisheye_ring()


    def __fisheye_function(self, ring):
        """
        """
        distance = abs(self.__fisheye_ring - ring)
        level_of_detail = self.__ring_gap * self.__fisheye_interest
        spreaded_distance = distance - distance * self.__fisheye_spread

        value = level_of_detail / (spreaded_distance + 1)

        if value < self.__min_ring_gap:
            value = self.__min_ring_gap

        return value


    @graph_is_not_empty
    @not_is_in_animation
    def __update_nodes_positions(self):
        """
        """
        for node in self.__sorted_nodes:

            if node.get_draw_info('grouped') == True:

                # deep group check
                group = node.get_draw_info('group_node')

                while group.get_draw_info('group_node') != None:
                    group = group.get_draw_info('group_node')

                ring = group.get_draw_info('ring')
                node.set_coordinate_radius(self.__calc_radius(ring))

            else:
                ring = node.get_draw_info('ring')
                node.set_coordinate_radius(self.__calc_radius(ring))


    @graph_is_not_empty
    def __get_node_by_coordinate(self, point):
        """
        """
        xc, yc = self.__center_of_widget

        for node in self.__graph.get_nodes():

            if node.get_draw_info('grouped') == True:
                continue

            ax, ay = self.__translation
        
            xn, yn = node.get_cartesian_coordinate()
            center = (xc + xn * self.__scale + ax, yc - yn * self.__scale - ay)
            radius = node.get_draw_info('radius') * self.__scale

            type = node.get_info('device_type')

            if type in SQUARE_TYPES:
                if geometry.is_in_square(point, radius, center) == True:
                    return node, center

            else:
                if geometry.is_in_circle(point, radius, center) == True:
                    return node, center

        return None


    def __calc_radius(self, ring):
        """
        """
        if self.__fisheye:

            radius = 0

            while ring > 0:

                radius += self.__fisheye_function(ring)
                ring -= 1

        else:
            radius = ring * self.__ring_gap

        return radius

    
    @graph_is_not_empty
    def __arrange_nodes(self):
        """
        """
        new_nodes = [self.__graph.get_main_node()]
        old_nodes = list()

        number_of_needed_rings = 1
        ring = 0

        # while new nodes were found
        while len(new_nodes) > 0:

            tmp_nodes = list()

            # for each new nodes
            for node in new_nodes:

                old_nodes.append(node)

                # set ring location
                node.set_draw_info({'ring':ring})

                # check group constraints
                if node.get_draw_info('group') or node.get_draw_info('grouped'):
                    children = node.get_draw_info('children')

                else:

                    # getting connections and fixing multiple fathers
                    children = self.__graph.get_node_connections(node)

                    misc.list_difference_update(children, old_nodes)
                    misc.list_difference_update(children, tmp_nodes)
                    misc.list_difference_update(children, new_nodes)

                    # dropping foreign children
                    foreign_children = list()

                    for child in children:

                        if child.get_draw_info('grouped'):
                            foreign_children.append(child)

                    misc.list_difference_update(children, foreign_children)

                    children = misc.sort_children(children, node)

                # setting father foreign
                for child in children:
                    child.set_draw_info({'father':node})

                node.set_draw_info({'children':children})
                misc.list_update(tmp_nodes, children)

            # check group influence in number of rings
            for node in tmp_nodes:

                if node.get_draw_info('grouped') != True:

                    number_of_needed_rings += 1
                    break

            # update new nodes set
            misc.list_update(new_nodes, tmp_nodes)
            misc.list_difference_update(new_nodes, old_nodes)

            ring += 1

        self.__set_number_of_rings(number_of_needed_rings)


    def __weighted_layout(self):
        """
        """
        # calculating the space needed by each node
        self.__graph.get_main_node().set_draw_info({'range':(0, 360)})
        new_nodes = [self.__graph.get_main_node()]

        self.__graph.get_main_node().calc_needed_space()

        while len(new_nodes) > 0:

            tmp_nodes = list()

            for node in new_nodes:

                # add only no grouped nodes
                children = list()

                for child in node.get_draw_info('children'):

                    if child.get_draw_info('grouped') != True:
                        children.append(child)

                if len(children) > 0:

                    min, max = node.get_draw_info('range')

                    node_total = max - min
                    children_need = node.get_draw_info('children_need')

                    for child in children:

                        child_need = child.get_draw_info('space_need')
                        child_total = node_total * child_need / children_need

                        theta = child_total / 2 + min + self.__rotate

                        child.set_coordinate_theta(theta)
                        child.set_draw_info({'range':(min, min + child_total)})

                        min += child_total

                misc.list_update(tmp_nodes, children)

            new_nodes = list()
            misc.list_update(new_nodes, tmp_nodes)


    def __symmetric_layout(self):
        """
        """
        self.__graph.get_main_node().set_draw_info({'range':(0, 360)})
        new_nodes = [self.__graph.get_main_node()]

        while len(new_nodes) > 0:

            tmp_nodes = list()

            for node in new_nodes:

                # add only no grouped nodes
                children = list()

                for child in node.get_draw_info('children'):

                    if child.get_draw_info('grouped') != True:
                        children.append(child)

                if len(children) > 0:

                    min, max = node.get_draw_info('range')
                    factor = float(max - min) / len(children)

                    for child in children:

                        theta = factor / 2 + min + self.__rotate

                        child.set_coordinate_theta(theta)
                        child.set_draw_info({'range':(min, min + factor)})

                        min += factor

                misc.list_update(tmp_nodes, children)

            new_nodes = list()
            misc.list_update(new_nodes, tmp_nodes)


    @graph_is_not_empty
    def __calc_layout(self, reference):
        """
        """
        # selecting layout algorithm
        if self.__layout == LAYOUT_SYMMETRIC:
            self.__symmetric_layout()

        elif self.__layout == LAYOUT_WEIGHTED:
            self.__weighted_layout()

        # rotating focus' children to keep orientation
        if reference != None:

            father, angle = reference
            theta = father.get_coordinate_theta()
            factor = theta - angle

            for node in self.__graph.get_nodes():

                theta = node.get_coordinate_theta()
                node.set_coordinate_theta(theta - factor)

                a, b = node.get_draw_info('range')
                node.set_draw_info({'range':(a - factor, b - factor)})


    @graph_is_not_empty
    def __calc_node_positions(self, reference=None):
        """
        """
        # set nodes' hierarchy
        self.__arrange_nodes()
        self.calc_sorted_nodes()

        # set nodes' coordinate radius
        for node in self.__graph.get_nodes():

            ring = node.get_draw_info('ring')
            node.set_coordinate_radius(self.__calc_radius(ring))

        # set nodes' coordinate theta
        self.__calc_layout(reference)


    def __calc_interpolation(self, focus):
        """
        """
        old_main_node = self.__graph.get_main_node()
        self.__graph.set_main_node(focus)

        # getting initial coordinates
        for node in self.__graph.get_nodes():

            if self.__interpolation == INTERPOLATION_POLAR:
                coordinate = node.get_polar_coordinate()

            elif self.__interpolation == INTERPOLATION_CARTESIAN:
                coordinate = node.get_cartesian_coordinate()

            node.set_draw_info({'start_coordinate':coordinate})

        father = focus.get_draw_info('father')

        # calculate nodes positions (and father orientation)?
        if father != None:

            xa, ya = father.get_cartesian_coordinate()
            xb, yb = focus.get_cartesian_coordinate()

            angle = math.atan2(yb - ya, xb - xa)
            angle = math.degrees(angle)

            self.__calc_node_positions((father, 180 + angle))

        else:
            self.__calc_node_positions()

        # steps for slow-in/slow-out animation
        steps = range(self.__number_of_frames)

        for i in range(len(steps) / 2):
            steps[self.__number_of_frames - 1 - i] = steps[i]

        # normalize angles and calculate interpolated points
        for node in self.__sorted_nodes:

            l2di = Linear2DInterpolator()

            # change grouped nodes coordinate
            if node.get_draw_info('grouped') == True:

                group_node = node.get_draw_info('group_node')
                a, b = group_node.get_draw_info('final_coordinate')

                if self.__interpolation == INTERPOLATION_POLAR:
                    node.set_polar_coordinate(a, b)

                elif self.__interpolation == INTERPOLATION_CARTESIAN:
                    node.set_cartesian_coordinate(a, b)

            # change interpolation method
            if self.__interpolation == INTERPOLATION_POLAR:

                coordinate = node.get_polar_coordinate()
                node.set_draw_info({'final_coordinate':coordinate})

                # adjusting polar coordinates
                ri, ti = node.get_draw_info('start_coordinate')
                rf, tf = node.get_draw_info('final_coordinate')

                # normalization [0, 360]
                ti = geometry.normalize_angle(ti)
                tf = geometry.normalize_angle(tf)

                # against longest path
                ti, tf = geometry.calculate_short_path(ti, tf)

                # main node goes direct to center (no arc)
                if node == self.__graph.get_main_node(): tf = ti

                # old main node goes direct to new position (no arc)
                if node == old_main_node: ti = tf

                node.set_draw_info({'start_coordinate':(ri, ti)})
                node.set_draw_info({'final_coordinate':(rf, tf)})

            elif self.__interpolation == INTERPOLATION_CARTESIAN:

                coordinate = node.get_cartesian_coordinate()
                node.set_draw_info({'final_coordinate':coordinate})

            # calculate interpolated points
            ai, bi = node.get_draw_info('start_coordinate')
            af, bf = node.get_draw_info('final_coordinate')

            l2di.set_start_point(ai, bi)
            l2di.set_final_point(af, bf)

            if self.__interpolation_slow_in_out:
                points = l2di.get_weighed_points(self.__number_of_frames, steps)

            else:
                points = l2di.get_points(self.__number_of_frames)

            node.set_draw_info({'interpolated_coordinate':points})

        return True


    def __livens_up(self, index=0):
        """
        """
        # prepare interpolated points
        if index == 0:

            # prevent unnecessary animation
            no_need_to_move = True

            for node in self.__graph.get_nodes():

                ai, bi = node.get_draw_info('start_coordinate')
                af, bf = node.get_draw_info('final_coordinate')

                start_c = round(ai), round(bi)
                final_c = round(af), round(bf)

                if start_c != final_c:
                    no_need_to_move = False

            if no_need_to_move:

                self.__animating = False
                return False

        # move all nodes for pass 'index'
        for node in self.__graph.get_nodes():

            a, b = node.get_draw_info('interpolated_coordinate')[index]
            
            if self.__interpolation == INTERPOLATION_POLAR:
                node.set_polar_coordinate(a, b)

            elif self.__interpolation == INTERPOLATION_CARTESIAN:
                node.set_cartesian_coordinate(a, b)

        self.queue_draw()

        # animation continue condition
        if index < self.__number_of_frames - 1:
            gobject.timeout_add(self.__animation_rate, # time to recall
                                self.__livens_up,      # recursive call
                                index + 1)             # next iteration
        else:
            self.__last_group_node = None
            self.__animating = False

        return False


    @not_is_in_animation
    def set_graph(self, graph):
        """
        Set graph to be displayed in layout
        @type  : Graph
        @param : Set the graph used in visualization
        """
        if graph.get_number_of_nodes() > 0:

            self.__graph = graph

            self.__calc_node_positions()
            self.queue_draw()

        else:
            self.__graph = None


    def get_scanned_nodes(self):
        """
        """
        nodes = list()
        if self.__graph is None:
            return nodes

        for node in self.__graph.get_nodes():

            if node.get_info('scanned'):
                nodes.append(node)

        return nodes


    def get_graph(self):
        """
        """
        return self.__graph


    def set_empty(self):
        """
        """
        del(self.__graph)
        self.__graph = None

        self.queue_draw()


    def get_rotation(self):
        """
        """
        return self.__rotate


    @graph_is_not_empty
    def set_rotation(self, angle):
        """
        """
        delta = angle - self.__rotate
        self.__rotate = angle

        for node in self.__graph.get_nodes():

            theta = node.get_coordinate_theta()
            node.set_coordinate_theta(theta + delta)

        self.queue_draw()


    def get_translation(self):
        """
        """
        return self.__translation


    @graph_is_not_empty
    def set_translation(self, translation):
        """
        """
        self.__translation = translation
        self.queue_draw()


    def is_empty(self):
        """
        """
        if self.__graph == None:
            return True

        return False


    def is_in_animation(self):
        """
        """
        return self.__animating


    def calc_sorted_nodes(self):
        """
        """
        nodes = list()

        for node in self.__graph.get_nodes():

            ring = node.get_draw_info('ring')
            count = 0

            for s_node in nodes:
            
                if ring < s_node.get_draw_info('ring'): break
                count +=1

            nodes.insert(count, node)

        self.__sorted_nodes = nodes
        self.__reverse_sorted_nodes = copy.copy(nodes)
        self.__reverse_sorted_nodes.reverse()



class NetNode(Node):
    """
    Node class for radial network widget
    """
    def __init__(self, id=Node):
        """
        """
        self.__draw_info = dict()
        """Hash with draw information"""
        self.__coordinate = PolarCoordinate()

        super(NetNode, self).__init__(id)


    def get_coordinate_theta(self):
        """
        """
        return self.__coordinate.get_theta()


    def get_coordinate_radius(self):
        """
        """
        return self.__coordinate.get_radius()


    def set_coordinate_theta(self, value):
        """
        """
        self.__coordinate.set_theta(value)


    def set_coordinate_radius(self, value):
        """
        """
        self.__coordinate.set_radius(value)


    def set_polar_coordinate(self, r, t):
        """
        Set polar coordinate
        @type  r: number
        @param r: The radius of coordinate
        @type  t: number
        @param t: The angle (theta) of coordinate in radians
        """
        self.__coordinate.set_coordinate(r, t)


    def get_polar_coordinate(self):
        """
        Get cartesian coordinate
        @rtype: tuple
        @return: Cartesian coordinates (x, y)
        """
        return self.__coordinate.get_coordinate()


    def set_cartesian_coordinate(self, x, y):
        """
        Set cartesian coordinate
        """
        cartesian = CartesianCoordinate(x, y)
        r, t = cartesian.to_polar()

        self.set_polar_coordinate(r, math.degrees(t))


    def get_cartesian_coordinate(self):
        """
        Get cartesian coordinate
        @rtype: tuple
        @return: Cartesian coordinates (x, y)
        """
        return self.__coordinate.to_cartesian()


    def get_draw_info(self, info=None):
        """
        Get draw information about node
        @type  : string
        @param : Information name
        @rtype: mixed
        @return: The requested information
        """
        if info == None:
            return self.__draw_info

        if self.__draw_info.has_key(info):
            return self.__draw_info[info]
            
        return None


    def set_draw_info(self, info):
        """
        Set draw information
        @type  : dict
        @param : Draw information dictionary
        """
        for key in info:
            self.__draw_info[key] = info[key]


    def deep_search_child(self, node):
        """
        """
        for child in self.get_draw_info('children'):

            if child == node:
                return True

            elif child.deep_search_child(node):
                return True

        return False


    def set_subtree_info(self, info):
        """
        """
        for child in self.get_draw_info('children'):

            child.set_draw_info(info)

            if child.get_draw_info('group') != True:
                child.set_subtree_info(info)


    def calc_needed_space(self):
        """
        """
        number_of_children = len(self.get_draw_info('children'))

        sum_angle = 0
        own_angle = 0

        if number_of_children > 0 and self.get_draw_info('group') != True:

            for child in self.get_draw_info('children'):

                child.calc_needed_space()
                sum_angle += child.get_draw_info('space_need')
        
        distance = self.get_coordinate_radius()
        size = self.get_draw_info('radius') * 2
        own_angle = geometry.angle_from_object(distance, size)

        self.set_draw_info({'children_need':sum_angle})
        self.set_draw_info({'space_need':max(sum_angle, own_angle)})

