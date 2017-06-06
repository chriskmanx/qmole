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


class Linear2DInterpolator:
    """
    Implements a bidimesional linear interpolator.
    """

    def __init__(self):
        """
        Constructor method of Linear2DInterpolator class
        """
        self.__start_point = (0, 0)
        """Initial point of interpolation"""
        self.__final_point = (0, 0)
        """Final point of interpolation"""
        self.__interpolated_points = []
        """Interpolated points vector"""


    def set_start_point(self, a, b):
        """
        Set initial coordinate
        Set final coordinate
        @type  a: number
        @param a: The first component of final point
        @type  b: number
        @param b: The second component of final point
        """
        self.__start_point = (a, b)


    def set_final_point(self, a, b):
        """
        Set final coordinate
        @type  a: number
        @param a: The first component of final point
        @type  b: number
        @param b: The second component of final point
        """
        self.__final_point = (a, b)


    def get_weighed_points(self, number_of_pass, pass_vector):
        """
        Return the vector of coordinates between the initial and final
        coordinates with the specified size
        @type  number_of_pass: number
        @param number_of_pass: The number of pass of interpolation
        @rtype: list
        @return: A list of tuples with interpolated points
        """
        (ai, bi) = self.__start_point
        (af, bf) = self.__final_point

        a_conversion_factor = float(af - ai) / sum(pass_vector)
        b_conversion_factor = float(bf - bi) / sum(pass_vector)

        a_pass = 0
        b_pass = 0

        self.__interpolated_points = range(number_of_pass)

        for i in range(0, number_of_pass):

            a_pass += pass_vector[i] * a_conversion_factor
            b_pass += pass_vector[i] * b_conversion_factor
            self.__interpolated_points[i] = (ai + a_pass, bi + b_pass)

        return self.__interpolated_points


    def get_points(self, number_of_pass):
        """
        Return the vector of coordinates between the initial and final
        coordinates with the specified size
        @type  number_of_pass: number
        @param number_of_pass: The number of pass of interpolation
        @rtype: list
        @return: A list of tuples with interpolated points
        """
        (ai, bi) = self.__start_point
        (af, bf) = self.__final_point

        a_pass = float(af - ai) / number_of_pass
        b_pass = float(bf - bi) / number_of_pass

        self.__interpolated_points = range(number_of_pass)

        for i in range(1, number_of_pass + 1):
            self.__interpolated_points[i - 1] = (ai + a_pass * i,
                                                 bi + b_pass * i)

        return self.__interpolated_points



if __name__ == "__main__":

    # Testing application

    i = Linear2DInterpolator()

    i.set_start_point(0, 0)
    i.set_final_point(1, 1)

    print len(i.get_points(10)), i.get_points(10)

