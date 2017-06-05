major=@major_shared@
minor=@minor_shared@
( cd usr/X11/lib ; ln -sf libXm.so.${major}.${minor} libXm.so.${major} )
( cd usr/X11/lib ; ln -sf libXm.so.${major} libXm.so )
( cd usr/X11/lib ; ln -sf libMrm.so.${major}.${minor} libMrm.so.${major} )
( cd usr/X11/lib ; ln -sf libMrm.so.${major} libMrm.so )
( cd usr/X11/lib ; ln -sf libUil.so.${major}.${minor} libUil.so.${major} )
( cd usr/X11/lib ; ln -sf libUil.so.${major} libUil.so )
if fgrep "/usr/X11/lib" etc/ld.so.conf ; then
	BOGUS_FLAG="bogus_value"
else
	echo "/usr/X11/lib" >> etc/ld.so.conf
fi
ldconfig
