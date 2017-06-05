#!/usr/bin/env python

import exceptions
import os
import sys
import select
import unittest

from common import glib

class TestMainLoop(unittest.TestCase):
    def testExceptionHandling(self):
        pipe_r, pipe_w = os.pipe()

        pid = os.fork()
        if pid == 0:
            os.close(pipe_w)
            select.select([pipe_r], [], [])
            os.close(pipe_r)
            os._exit(1)

        def child_died(pid, status, loop):
            loop.quit()
            raise Exception("deadbabe")

        loop = glib.MainLoop()
        glib.child_watch_add(pid, child_died, loop)

        os.close(pipe_r)
        os.write(pipe_w, "Y")
        os.close(pipe_w)

        def excepthook(type, value, traceback):
            assert type is exceptions.Exception
            assert value.args[0] == "deadbabe"
        sys.excepthook = excepthook

        got_exception = False
        try:
            loop.run()
        except:
            got_exception = True

        #
        # The exception should be handled (by printing it)
        # immediately on return from child_died() rather
        # than here. See bug #303573
        #
        sys.excepthook = sys.__excepthook__
        assert not got_exception

if __name__ == '__main__':
    unittest.main()
