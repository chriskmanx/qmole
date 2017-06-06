# vim: set fileencoding=utf-8 :

# Copyright (C) 2007, 2008 Insecure.Com LLC.
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

from radialnet.core.Coordinate import CartesianCoordinate
from radialnet.gui.RadialNet import *
from radialnet.util.geometry import *
import math



def ipv4_compare(ip1, ip2):
    """
    """
    ip1 = [int(i) for i in ip1.split('.')]
    ip2 = [int(i) for i in ip2.split('.')]

    for i in range(4):

        if ip1[i] != ip2[i]:

            if ip1[i] < ip2[i]:
                return -1

            else:
                return 1

    return 0


def list_difference_update(list, difference):
    """
    """
    for item in difference:

        if item in list:
            list.remove(item)


def list_update(list, append):
    """
    """
    for item in append:

        if item not in list:
            list.append(item)


def swap(list, a, b):
    """
    """
    list[a], list[b] = list[b], list[a]


def sort_children(children, father):
    """
    """
    if len(children) < 2:
        return children

    # create angle reference
    f_x, f_y = father.get_cartesian_coordinate()

    for child in children:

        c_x, c_y = child.get_cartesian_coordinate()
        _, angle = CartesianCoordinate(c_x - f_x, c_y - f_y).to_polar()

        child.set_draw_info({'angle_from_father': math.degrees(angle)})

    return sort_children_by_angle(children)


def sort_children_by_angle(children):
    """
    """
    if len(children) < 2:
        return children

    vector = list()
    vector.append(children.pop())

    for a in children:

        theta_a = normalize_angle(a.get_draw_info('angle_from_father'))

        for i in range(len(vector) -1, -1, -1):

            b = vector[i]

            theta_b = normalize_angle(b.get_draw_info('angle_from_father'))

            if theta_b <= theta_a <= theta_b + 180:

                vector.insert(i + 1, a)
                break

        else:
            vector.insert(0, a)

    return vector
