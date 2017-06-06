#!/usr/bin/env python

# This is a test class for the non-trivial escaping done by zenmap_wrapper.py.

import unittest

import zenmap_wrapper

class test_key_file(unittest.TestCase):
    def test_escape(self):
        TESTS = (
            ("", ""),
            ("a", "a"),
            ("a\nb\tc\rd\\e", "a\\nb\\tc\\rd\\\\e"),
            ("a\"b", "a\"b")
        )
        for test_line, expected in TESTS:
            actual = zenmap_wrapper.escape_key_file_value(test_line)
            self.assertEqual(expected, actual)

    def test_escape_first_space(self):
        # Check first-character space escaping.
        self.assert_(zenmap_wrapper.escape_key_file_value("  abc").startswith("\\s"))

    def test_substitute(self):
        original = "abc"
        replacements = {"b": "\"\\\t\r\ndef"}
        expected = "a\"\\\\\\t\\r\\ndefc"
        actual = zenmap_wrapper.substitute_key_file_line(original, replacements)
        self.assertEqual(expected, actual)

class test_modules_file(unittest.TestCase):
    def test_escape(self):
        TESTS = (
            (r'', []),
            (r' ', []),
            (r'a ', ["a"]),
            (r'a b c', ["a", "b", "c"]),
            (r' a  "b""c"', ["a", "b", "c"]),
            (r' a  "b""c"', ["a", "b", "c"]),
            (r' "a\"b"  "b\\c" "c\nd" " d"', ["a\"b", "b\\c", "c\nd", " d"])
        )
        for test_line, expected in TESTS:
            # Parse the test line and make sure we get the list we expect.
            actual = zenmap_wrapper.split_modules_file_line(test_line)
            self.assertEqual(expected, actual)
            # Escape the line ourselves and parse it again.
            escaped = zenmap_wrapper.substitute_modules_file_line(test_line, {})
            actual = zenmap_wrapper.split_modules_file_line(escaped)
            self.assertEqual(expected, actual)

    def test_escape_space(self):
        TESTS = (r' ', r' a', r'a ', r' a ', r'a b')
        for test in TESTS:
            result = zenmap_wrapper.escape_modules_file_value(test)
            self.assert_(result.startswith("\""), "escape(%s) = %s" % (repr(test), repr(result)))
            self.assert_(result.endswith("\""), "escape(%s) = %s" % (repr(test), repr(result)))

    def test_substitute(self):
        original = "abc"
        replacements = {"b": "\"\\\n def"}
        expected = "\"a\\\"\\\\\\n defc\"\n"
        actual = zenmap_wrapper.substitute_modules_file_line(original, replacements)
        self.assertEqual(expected, actual)

unittest.main()
