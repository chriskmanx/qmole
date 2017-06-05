# -*- Mode: Python -*-

import unittest

from common import gobject


class TestGObjectAPI(unittest.TestCase):
    def testGObjectModule(self):
        obj = gobject.GObject()
        self.assertEquals(obj.__module__,
                          'gobject._gobject')
