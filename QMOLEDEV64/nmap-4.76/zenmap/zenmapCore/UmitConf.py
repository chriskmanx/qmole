#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
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

import re

from types import StringTypes
from ConfigParser import DuplicateSectionError, NoSectionError, NoOptionError

from zenmapCore.Paths import Path
from zenmapCore.UmitLogging import log
from zenmapCore.UmitConfigParser import UmitConfigParser
from zenmapCore.I18N import _

# This is the global configuration parser object that represents the contents of
# zenmap.conf. It should be initialized once by the application. Most
# interaction with the global parser is done by other classes in this file, like
# SearchConfig, that wrap specific configuration sections.
config_parser = UmitConfigParser()

# Check if running on Maemo
MAEMO = False
try:
    import hildon
    MAEMO = True
except ImportError:
    pass

def is_maemo():
    return MAEMO

class UmitConf(object):
    def save_changes(self):
        config_parser.save_changes()

    def get_colored_diff(self):
        try:
            cd = config_parser.get('diff', 'colored_diff')
            if cd == "False" or \
                cd == "false" or \
                cd == "0" or \
                cd == "" or \
                cd == False:
                return False
            return True
        except:
            return True

    def set_colored_diff(self, enable):
        if not config_parser.has_section('diff'):
            config_parser.add_section('diff')

        config_parser.set('diff', 'colored_diff', str(enable))

    def get_diff_mode(self):
        try: return config_parser.get('diff', 'diff_mode')
        except: return "compare"

    def set_diff_mode(self, diff_mode):
        if not config_parser.has_section('diff'):
            config_parser.add_section('diff')
        
        config_parser.set('diff', 'diff_mode', diff_mode)

    colored_diff = property(get_colored_diff, set_colored_diff)
    diff_mode = property(get_diff_mode, set_diff_mode)


class SearchConfig(UmitConfigParser, object):
    section_name = "search"

    def __init__(self):
        if not config_parser.has_section(self.section_name):
            self.create_section()

    def save_changes(self):
        config_parser.save_changes()

    def create_section(self):
        config_parser.add_section(self.section_name)
        self.directory = ""
        self.file_extension = "usr"
        self.save_time = "60;days"
        self.store_results = True
        self.search_db = True

    def _get_it(self, p_name, default):
        return config_parser.get(self.section_name, p_name, default)

    def _set_it(self, p_name, value):
        config_parser.set(self.section_name, p_name, value)
        
    def boolean_sanity(self, attr):
        if attr == True or \
           attr == "True" or \
           attr == "true" or \
           attr == "1":

            return 1

        return 0

    def get_directory(self):
        return self._get_it("directory", "")

    def set_directory(self, directory):
        self._set_it("directory", directory)

    def get_file_extension(self):
        return self._get_it("file_extension", "usr").split(";")

    def set_file_extension(self, file_extension):
        if type(file_extension) == type([]):
            self._set_it("file_extension", ";".join(file_extension))
        elif type(file_extension) in StringTypes:
            self._set_it("file_extension", file_extension)

    def get_save_time(self):
        return self._get_it("save_time", "60;days").split(";")

    def set_save_time(self, save_time):
        if type(save_time) == type([]):
            self._set_it("save_time", ";".join(save_time))
        elif type(save_time) in StringTypes:
            self._set_it("save_time", save_time)

    def get_store_results(self):
        return self.boolean_sanity(self._get_it("store_results", True))

    def set_store_results(self, store_results):
        self._set_it("store_results", self.boolean_sanity(store_results))

    def get_search_db(self):
        return self.boolean_sanity(self._get_it("search_db", True))

    def set_search_db(self, search_db):
        self._set_it("search_db", self.boolean_sanity(search_db))

    def get_converted_save_time(self):
        try:
            return int(self.save_time[0]) * self.time_list[self.save_time[1]]
        except:
            # If something goes wrong, return a save time of 60 days
            return 60 * 60 * 24 * 60

    def get_time_list(self):
        # Time as key, seconds a value
        return {_("Hours"): 60 * 60,
                _("Days"): 60 * 60 * 24,
                _("Weeks"): 60 * 60 * 24 * 7,
                _("Months"): 60 * 60 * 24 * 7 * 30,
                _("Years"): 60 * 60 * 24 * 7 * 30 * 12,
                _("Minutes"): 60,
                _("Seconds"): 1}
    
    directory = property(get_directory, set_directory)
    file_extension = property(get_file_extension, set_file_extension)
    save_time = property(get_save_time, set_save_time)
    store_results = property(get_store_results, set_store_results)
    search_db = property(get_search_db, set_search_db)
    converted_save_time = property(get_converted_save_time)
    time_list = property(get_time_list)


class Profile(UmitConfigParser, object):
    """This class represents not just one profile, but a whole collection of
    them found in a config file such as scan_profiles.usp. The methods therefore
    all take an argument that is the name of the profile to work on."""

    def __init__(self, user_profile = None, *args):
        UmitConfigParser.__init__(self, *args)

        if not user_profile:
            user_profile = Path.scan_profile

        fconf = open(user_profile, 'r')
        self.readfp(fconf, user_profile)

        fconf.close()
        del(fconf)

        self.attributes = {}

    def _get_it(self, profile, attribute):
        if self._verify_profile(profile):
            return self.get(profile, attribute)
        return ""

    def _set_it(self, profile, attribute, value=''):
        if self._verify_profile(profile):
            return self.set(profile, attribute, value)

    def add_profile(self, profile_name, **attributes):
        """Add a profile with the given name and attributes to the collection of
        profiles. If a profile with the same name exists, it is not overwritten,
        and the method returns immediately. The backing file for the profiles is
        automatically updated."""

        log.debug(">>> Add Profile '%s': %s" % (profile_name, attributes))

        try:
            self.add_section(profile_name)
        except DuplicateSectionError:
            return None

        # Set each of the attributes ("command", "options", "description") in
        # the ConfigParser.
        for attr in attributes:
            if attr == "options":
                # We handle the "options" attribute specially because it is a
                # dict, not a plain string.
                options = attributes["options"]
                assert type(options) == dict

                # The "options" attribute looks like, for example,
                #   options = Ports to scan,Verbose,Aggressive
                self._set_it(profile_name, "options", ",".join(options.keys()))

                # Some of the options take an argument, which is also stored in
                # the scan profiles file. Arguments are the values associated
                # with keys in the dict, if not None. For example,
                #   ports to scan = 1-1024
                # might be stored in the file, meaning "-p1-1024". (Notice that
                # case is not significant.)
                for opt in options:
                    if options[opt]:
                        self._set_it(profile_name, opt, options[opt])
            else:
                # The other attributes are just strings so simply set them.
                self._set_it(profile_name, attr, attributes[attr])

        self.save_changes()

    def remove_profile(self, profile_name):
        try: self.remove_section(profile_name)
        except: pass
        self.save_changes()

    def _verify_profile(self, profile_name):
        if profile_name not in self.sections():
            return False
        return True

class CommandProfile (Profile, object):
    """This class is a wrapper around Profile that provides accessors for the
    attributes of a profile: command, options, and description"""
    def __init__(self, user_profile = None):
        Profile.__init__(self, user_profile)
        
    def get_command(self, profile):
        return self._get_it(profile, 'command')

    def get_description(self, profile):
        return self._get_it(profile, 'description')
    
    def get_options(self, profile):
        """Return a dict of options and their corresponding arguments. Use None
        as the value for options that don't have an argument."""
        dic = {}
        options_str = self._get_it(profile, 'options')
        # If we get back an empty string, there are no options.
        if options_str.strip() == '':
            return dic
        for opt in options_str.split(','):
            opt = opt.strip()
            # Arguments for options are stored in the same way as the list of
            # options itself, as name-value pairs, with the option as the name
            # and the argument as the value. So for example you might have
            #   options = Ports to scan,Verbose,Aggressive
            #   ports to scan = 1-1024
            # and case is not significant in the option name.
            try:
                dic[unicode(opt)] = self._get_it(profile, opt)
            except NoOptionError:
                dic[unicode(opt)] = None
        return dic

    def set_command(self, profile, command=''):
        self._set_it(profile, 'command', command)

    def set_description(self, profile, description=''):
        self._set_it(profile, 'description', description)
    
    def set_options(self, profile, options={}):
        for opt in options:
            if options[opt]:
                self._set_it(profile, opt, options[opt])
        self._set_it(profile, 'options', ",".join(options.keys()))

    def get_profile(self, profile_name):
        return {'profile':profile_name, \
                'command':self.get_command(profile_name), \
                'description':self.get_description(profile_name), \
                'options':self.get_options(profile_name)}


class NmapOutputHighlight(object):
    setts = ["bold", "italic", "underline", "text", "highlight", "regex"]
    
    def save_changes(self):
        config_parser.save_changes()

    def __get_it(self, p_name):
        property_name = "%s_highlight" % p_name

        try:
            return self.sanity_settings([config_parser.get(property_name,
                                                         prop,
                                                         True) \
                                         for prop in self.setts])
        except:
            settings = []
            prop_settings = self.default_highlights[p_name]
            settings.append(prop_settings["bold"])
            settings.append(prop_settings["italic"])
            settings.append(prop_settings["underline"])
            settings.append(prop_settings["text"])
            settings.append(prop_settings["highlight"])
            settings.append(prop_settings["regex"])

            self.__set_it(p_name, settings)

            return settings

    def __set_it(self, property_name, settings):
        property_name = "%s_highlight" % property_name
        settings = self.sanity_settings(list(settings))

        [config_parser.set(property_name, self.setts[pos], settings[pos]) \
         for pos in xrange(len(settings))]

    def sanity_settings(self, settings):
        """This method tries to convert insane settings to sanity ones ;-)
        If user send a True, "True" or "true" value, for example, it tries to
        convert then to the integer 1.
        Same to False, "False", etc.

        Sequence: [bold, italic, underline, text, highlight, regex]
        """
        #log.debug(">>> Sanitize %s" % str(settings))
        
        settings[0] = self.boolean_sanity(settings[0])
        settings[1] = self.boolean_sanity(settings[1])
        settings[2] = self.boolean_sanity(settings[2])

        tuple_regex = "[\(\[]\s?(\d+)\s?,\s?(\d+)\s?,\s?(\d+)\s?[\)\]]"
        if isinstance(settings[3], basestring):
            settings[3] = [int(t) for t in re.findall(tuple_regex, settings[3])[0]]

        if isinstance(settings[4], basestring):
            settings[4]= [int(h) for h in re.findall(tuple_regex, settings[4])[0]]

        return settings

    def boolean_sanity(self, attr):
        if attr == True or attr == "True" or attr == "true" or attr == "1":
            return 1
        return 0

    def get_date(self):
        return self.__get_it("date")

    def set_date(self, settings):
        self.__set_it("date", settings)

    def get_hostname(self):
        return self.__get_it("hostname")

    def set_hostname(self, settings):
        self.__set_it("hostname", settings)

    def get_ip(self):
        return self.__get_it("ip")

    def set_ip(self, settings):
        self.__set_it("ip", settings)

    def get_port_list(self):
        return self.__get_it("port_list")

    def set_port_list(self, settings):
        self.__set_it("port_list", settings)

    def get_open_port(self):
        return self.__get_it("open_port")

    def set_open_port(self, settings):
        self.__set_it("open_port", settings)

    def get_closed_port(self):
        return self.__get_it("closed_port")

    def set_closed_port(self, settings):
        self.__set_it("closed_port", settings)

    def get_filtered_port(self):
        return self.__get_it("filtered_port")

    def set_filtered_port(self, settings):
        self.__set_it("filtered_port", settings)

    def get_details(self):
        return self.__get_it("details")

    def set_details(self, settings):
        self.__set_it("details", settings)

    def get_enable(self):
        enable = True
        try:
            enable = config_parser.get("output_highlight", "enable_highlight")
        except NoSectionError:
            config_parser.set("output_highlight", "enable_highlight", str(True))
        
        if enable == "False" or enable == "0" or enable == "":
            return False
        return True

    def set_enable(self, enable):
        if enable == False or enable == "0" or enable == None or enable == "":
            config_parser.set("output_highlight", "enable_highlight", str(False))
        else:
            config_parser.set("output_highlight", "enable_highlight", str(True))

    date = property(get_date, set_date)
    hostname = property(get_hostname, set_hostname)
    ip = property(get_ip, set_ip)
    port_list = property(get_port_list, set_port_list)
    open_port = property(get_open_port, set_open_port)
    closed_port = property(get_closed_port, set_closed_port)
    filtered_port = property(get_filtered_port, set_filtered_port)
    details = property(get_details, set_details)
    enable = property(get_enable, set_enable)

    # These settings are made when there is nothing set yet. They set the "factory" \
    # default to highlight colors
    default_highlights = {"date":{"bold":str(True),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[0, 0, 0],
                            "highlight":[65535, 65535, 65535],
                            "regex":"\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}\s.{1,4}"},
                          "hostname":{"bold":str(True),
                            "italic":str(True),
                            "underline":str(True),
                            "text":[0, 111, 65535],
                            "highlight":[65535, 65535, 65535],
                            "regex":"(\w{2,}://)*[\w-]{2,}\.[\w-]{2,}(\.[\w-]{2,})*(/[[\w-]{2,}]*)*"},
                          "ip":{"bold":str(True),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[0, 0, 0],
                            "highlight":[65535, 65535, 65535],
                            "regex":"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}"},
                          "port_list":{"bold":str(True),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[0, 1272, 28362],
                            "highlight":[65535, 65535, 65535],
                            "regex":"PORT\s+STATE\s+SERVICE(\s+VERSION)?[^\n]*"},
                          "open_port":{"bold":str(True),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[0, 41036, 2396],
                            "highlight":[65535, 65535, 65535],
                            "regex":"\d{1,5}/.{1,5}\s+open\s+.*"},
                          "closed_port":{"bold":str(False),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[65535, 0, 0],
                            "highlight":[65535, 65535, 65535],
                            "regex":"\d{1,5}/.{1,5}\s+closed\s+.*"},
                          "filtered_port":{"bold":str(False),
                            "italic":str(False),
                            "underline":str(False),
                            "text":[38502, 39119, 0],
                            "highlight":[65535, 65535, 65535],
                            "regex":"\d{1,5}/.{1,5}\s+filtered\s+.*"},
                          "details":{"bold":str(True),
                            "italic":str(False),
                            "underline":str(True),
                            "text":[0, 0, 0],
                            "highlight":[65535, 65535, 65535],
                            "regex":"^(\w{2,}[\s]{,3}){,4}:"}}

class DiffColors(object):
    section_name = "diff_colors"

    def save_changes(self):
        config_parser.save_changes()

    def __get_it(self, p_name):
        return self.sanity_settings(config_parser.get(self.section_name, p_name))

    def __set_it(self, property_name, settings):
        settings = self.sanity_settings(settings)
        config_parser.set(self.section_name, property_name, settings)

    def sanity_settings(self, settings):
        log.debug(">>> Sanitize %s" % str(settings))
        
        tuple_regex = "[\(\[]\s?(\d+)\s?,\s?(\d+)\s?,\s?(\d+)\s?[\)\]]"
        if isinstance(settings, basestring):
            settings = [int(t) for t in re.findall(tuple_regex, settings)[0]]

        return settings

    def get_unchanged(self):
        return self.__get_it("unchanged")

    def set_unchanged(self, settings):
        self.__set_it("unchanged", settings)

    def get_added(self):
        return self.__get_it("added")

    def set_added(self, settings):
        self.__set_it("added", settings)

    def get_modified(self):
        return self.__get_it("modified")

    def set_modified(self, settings):
        self.__set_it("modified", settings)

    def get_not_present(self):
        return self.__get_it("not_present")

    def set_not_present(self, settings):
        self.__set_it("not_present", settings)

    unchanged = property(get_unchanged, set_unchanged)
    added = property(get_added, set_added)
    modified = property(get_modified, set_modified)
    not_present = property(get_not_present, set_not_present)


# Retrieve details from zenmap.conf regarding paths subsection
# (e.g. nmap_command_path) - jurand
class PathsConfig(object):
    section_name = "paths"

    # This accounts for missing entries conf file.
    # Defaults to "nmap" if these errors occur.
    # NoOptionError, NoSectionError
    def __get_it(self, p_name):
	try:
        	return config_parser.get(self.section_name, p_name)
	except (NoOptionError,NoSectionError):
		log.debug(">>> nmap command path not configured. Using default 'nmap'.")
		return "nmap"

    def __set_it(self, property_name, settings):
        config_parser.set(self.section_name, property_name, settings)

    def get_nmap_command_path(self):
        return self.__get_it("nmap_command_path")

    def set_nmap_command_path(self, settings):
        self.__set_it("nmap_command_path", settings)

    nmap_command_path = property(get_nmap_command_path, set_nmap_command_path)

# Exceptions
class ProfileNotFound:
    def __init__ (self, profile):
        self.profile = profile
    def __str__ (self):
        return "No profile named '"+self.profile+"' found!"

class ProfileCouldNotBeSaved:
    def __init__ (self, profile):
        self.profile = profile
    def __str__ (self):
        return "Profile named '"+self.profile+"' could not be saved!"


if __name__ == "__main__":
    pass
