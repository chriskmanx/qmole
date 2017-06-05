SET(BMPANEL2CFG_DESTDIR /home/chris/bmpanel2-2.1pre1)
IF(IS_ABSOLUTE $ENV{DESTDIR})
	SET(BMPANEL2CFG_DESTDIR $ENV{DESTDIR})
ELSE(IS_ABSOLUTE $ENV{DESTDIR})
	SET(BMPANEL2CFG_DESTDIR "/home/chris/bmpanel2-2.1pre1/$ENV{DESTDIR}")
ENDIF(IS_ABSOLUTE $ENV{DESTDIR})

MESSAGE(STATUS "Installing: bmpanel2cfg and python libraries")

EXECUTE_PROCESS(COMMAND /usr/bin/python setup.py -q
			build -b /home/chris/bmpanel2-2.1pre1/extra/py
			install --root=${BMPANEL2CFG_DESTDIR} --prefix=/usr/local
		WORKING_DIRECTORY /home/chris/bmpanel2-2.1pre1/extra/py)
