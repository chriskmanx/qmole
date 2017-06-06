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

import math


class PolarCoordinate:
    """
    Class to implement a polar coordinate object
    """

    def __init__(self, r=0, t=0):
        """
        Constructor method of PolarCoordinate class
        @type  r: number
        @param r: The radius of coordinate
        @type  t: number
        @param t: The angle (theta) of coordinate in radians
        """

        self.__r = r
        """Radius of polar coordinate"""
        self.__t = t
        """Angle (theta) of polar coordinate in radians"""


    def get_theta(self):
        """
        """
        return math.degrees(self.__t)


    def get_radius(self):
        """
        """
        return self.__r


    def set_theta(self, t):
        """
        """
        self.__t = math.radians(t)


    def set_radius(self, r):
        """
        """
        self.__r = r


    def get_coordinate(self):
        """
        Set polar coordinate
        @rtype: tuple
        @return: Polar coordinates (r, t)
        """
        return (self.__r, math.degrees(self.__t))


    def set_coordinate(self, r, t):
        """
        Set polar coordinate
        @type  r: number
        @param r: The radius of coordinate
        @type  t: number
        @param t: The angle (theta) of coordinate
        """
        self.__r = r
        self.__t = math.radians(t)


    def to_cartesian(self):
        """
        Convert polar in cartesian coordinate
        @rtype: tuple
        @return: cartesian coordinates (x, y)
        """
        x = self.__r * math.cos(self.__t)
        y = self.__r * math.sin(self.__t)

        return (x, y)



class CartesianCoordinate:
    """
    Class to implement a cartesian coordinate object
    """

    def __init__(self, x=0, y=0):
        """
        Constructor method of CartesianCoordinate class
        @type  x: number
        @param x: The x component of coordinate
        @type  y: number
        @param y: The y component of coordinate
        """
        self.__x = x
        """X component of cartesian coordinate"""
        self.__y = y
        """Y component of cartesian coordinate"""


    def get_coordinate(self):
        """
        Get cartesian coordinate
        @rtype: tuple
        @return: Cartesian coordinates (x, y)
        """
        return (self.__x, self.__y)


    def set_coordinate(self, x, y):
        """
        Set cartesian coordinate
        @type  x: number
        @param x: The x component of coordinate
        @type  y: number
        @param y: The y component of coordinate
        """
        self.__x = x
        self.__y = y


    def to_polar(self):
        """
        Convert cartesian in polar coordinate
        @rtype: tuple
        @return: polar coordinates (r, t)
        """
        r = math.sqrt(self.__x**2 + self.__y**2)

        if self.__x > 0:

            if self.__y >= 0:
                t = math.atan( self.__y / self.__x )

            else:
                t = math.atan( self.__y / self.__x ) + 2 * math.pi

        elif self.__x < 0:
            t = math.atan( self.__y / self.__x ) + math.pi

        elif self.__x == 0:

            if self.__y == 0:
                t = 0

            if self.__y > 0:
                t = math.pi / 2

            else:
                t = -math.pi / 2

        return (r, t)



if __name__ == "__main__":

    # Testing application

    polar     = PolarCoordinate(1, math.pi)
    cartesian = CartesianCoordinate(-1,  0)

    print polar.to_cartesian()
    print cartesian.to_polar()
