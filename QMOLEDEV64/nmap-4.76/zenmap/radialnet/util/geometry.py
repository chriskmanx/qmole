# vim: set fileencoding=utf-8 :

# Copyright (C) 2007 Insecure.Com LLC.
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

import math


def is_in_square(point, half_side, center=(0, 0)):
    """
    """
    x, y = point
    a, b = center

    if a + half_side >= x >= a - half_side:
        if b + half_side >= y >= b - half_side:
            return True

    return False


def is_in_circle(point, radius=1, center=(0, 0)):
    """
    """
    x, y = point
    a, b = center

    if ((x - a)**2 + (y - b)**2) <= (radius**2):
        return True

    return False


def atan_scale(point, scale_ceil):
    """
    """
    new_point = float(10.0 * point / scale_ceil) - 5
    return math.atan(abs(new_point))


def normalize_angle(angle):
    """
    """
    new_angle = 360.0 * (float(angle / 360) - int(angle / 360))

    if new_angle < 0:
        return 360 + new_angle

    return new_angle


def is_between_angles(a, b, c):
    """
    """
    a = normalize_angle(a)
    b = normalize_angle(b)
    c = normalize_angle(c)

    if a > b:

        if c >= a and c <= 360 or c <= b:
            return True

        return False

    else:

        if c >= a and c <= b:
            return True

        return False


def angle_distance(a, b):
    """
    """
    distance = abs(normalize_angle(a) - normalize_angle(b))

    if distance > 180:
        return 360 - distance

    return distance


def calculate_short_path(iangle, fangle):
    """
    """
    if iangle - fangle > 180:
        fangle += 360

    if iangle - fangle < -180:
        fangle -= 360

    return iangle, fangle


def angle_from_object(distance, size):
    """
    """
    return math.degrees(math.atan2(size / 2.0, distance))
