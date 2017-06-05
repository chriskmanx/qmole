/*
 * Copyright (C) 2009-2010, Pino Toscano <pino@kde.org>
 * Copyright (C) 2010, Hib Eris <hib@hiberis.nl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "poppler-global.h"

#include "DateInfo.h"

#include <cerrno>
#include <ctime>
#include <cstring>
#include <iostream>

#include <iconv.h>

#include "config.h"

namespace
{

struct MiniIconv
{
    MiniIconv(const char *to_code, const char *from_code)
        : i_(iconv_open(to_code, from_code))
    {}
    ~MiniIconv()
    { if (is_valid()) iconv_close(i_); }
    bool is_valid() const
    { return i_ != (iconv_t)-1; }
    operator iconv_t() const
    { return i_; }
    iconv_t i_;
};

}

using namespace poppler;

/**
 \namespace poppler

 Single namespace containing all the classes and functions of poppler-cpp.
 */

/**
 \class poppler::noncopyable

 A class that cannot be copied.
 */


noncopyable::noncopyable()
{
}

noncopyable::~noncopyable()
{
}


ustring::ustring()
{
}

ustring::ustring(size_type len, value_type ch)
    : std::basic_string<value_type>(len, ch)
{
}

ustring::~ustring()
{
}

byte_array ustring::to_utf8() const
{
    if (!size()) {
        return byte_array();
    }

    MiniIconv ic("UTF-8", "UTF-16");
    if (!ic.is_valid()) {
        return byte_array();
    }
    const value_type *me_data = data();
    byte_array str(size());
    char *str_data = &str[0];
    size_t me_len_char = size();
    size_t str_len_left = str.size();
    size_t ir = iconv(ic, (ICONV_CONST char **)&me_data, &me_len_char, &str_data, &str_len_left);
    if ((ir == (size_t)-1) && (errno == E2BIG)) {
        const size_t delta = str_data - &str[0];
        str_len_left += str.size();
        str.resize(str.size() * 2);
        str_data = &str[delta];
        ir = iconv(ic, (ICONV_CONST char **)&me_data, &me_len_char, &str_data, &str_len_left);
        if (ir == (size_t)-1) {
            return byte_array();
        }
    }
    if (str_len_left >= 0) {
        str.resize(str.size() - str_len_left);
    }
    return str;
}

std::string ustring::to_latin1() const
{
    if (!size()) {
        return std::string();
    }

    const size_type mylength = size();
    std::string ret(mylength, '\0');
    const value_type *me = data();
    for (size_type i = 0; i < mylength; ++i) {
        ret[i] = (char)*me++;
    }
    return ret;
}

ustring ustring::from_utf8(const char *str, int len)
{
    if (len <= 0) {
        len = std::strlen(str);
        if (len <= 0) {
            return ustring();
        }
    }

    MiniIconv ic("UTF-16", "UTF-8");
    if (!ic.is_valid()) {
        return ustring();
    }

    ustring ret(len * 2, 0);
    char *ret_data = reinterpret_cast<char *>(&ret[0]);
    char *str_data = const_cast<char *>(str);
    size_t str_len_char = len;
    size_t ret_len_left = ret.size();
    size_t ir = iconv(ic, (ICONV_CONST char **)&str_data, &str_len_char, &ret_data, &ret_len_left);
    if ((ir == (size_t)-1) && (errno == E2BIG)) {
        const size_t delta = ret_data - reinterpret_cast<char *>(&ret[0]);
        ret_len_left += ret.size();
        ret.resize(ret.size() * 2);
        ret_data = reinterpret_cast<char *>(&ret[delta]);
        ir = iconv(ic, (ICONV_CONST char **)&str_data, &str_len_char, &ret_data, &ret_len_left);
        if (ir == (size_t)-1) {
            return ustring();
        }
    }
    if (ret_len_left >= 0) {
        ret.resize(ret.size() - ret_len_left);
    }

    return ret;
}

ustring ustring::from_latin1(const std::string &str)
{
    const size_type l = str.size();
    if (!l) {
        return ustring();
    }
    const char *c = str.data();
    ustring ret(l, 0);
    for (size_type i = 0; i < l; ++i) {
        ret[i] = *c++;
    }
    return ret;
}


/**
 Converts a string representing a PDF date to a value compatible with time_t.
 */
unsigned int poppler::convert_date(const std::string &date)
{
    int year, mon, day, hour, min, sec, tzHours, tzMins;
    char tz;

    if (!parseDateString(date.c_str(), &year, &mon, &day, &hour, &min, &sec,
                                       &tz, &tzHours, &tzMins)) {
        return (unsigned int)(-1);
    }

    struct tm time;
    time.tm_sec = sec;
    time.tm_min = min;
    time.tm_hour = hour;
    time.tm_mday = day;
    time.tm_mon = mon - 1;
    time.tm_year = year - 1900;
    time.tm_wday = -1;
    time.tm_yday = -1;
    time.tm_isdst = -1;
    return mktime(&time);
}

std::ostream& poppler::operator<<(std::ostream& stream, const byte_array &array)
{
    stream << "[";
    const std::ios_base::fmtflags f = stream.flags();
    std::hex(stream);
    const char *data = &array[0];
    const byte_array::size_type out_len = std::min<byte_array::size_type>(array.size(), 50);
    for (byte_array::size_type i = 0; i < out_len; ++i)
    {
        if (i != 0) {
            stream << " ";
        }
        stream << ((data[i] & 0xf0) >> 4) << (data[i] & 0xf);
    }
    stream.flags(f);
    if (out_len < array.size()) {
        stream << " ...";
    }
    stream << "]";
    return stream;
}
