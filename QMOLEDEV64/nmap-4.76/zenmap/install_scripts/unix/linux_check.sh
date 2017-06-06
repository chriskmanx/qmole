#!/bin/sh

echo "##################################################"
echo "Dependencies installed in your system:"
echo ""

nmap > /dev/null 2> /dev/null
if [ $? -eq 127 ]; then
   echo "Nmap Failed!";
   exit;
else
   nmap -V | tail -1;
fi


python -V > /dev/null 2> /dev/null
if [ $? -eq 127 ]; then
   echo "Python Failed!";
   exit;
else
   python -V;
fi

python -c "import gtk" > /dev/null 2> /dev/null
if [ $? -eq 127 ]; then
   echo "GTK Failed!";
   exit;
else
   python -c "import gtk; print 'GTK version: %d.%d.%d' % gtk.gtk_version";
fi 

python -c "import pygtk" > /dev/null 2> /dev/null
if [ $? -eq 127 ]; then
   echo "PyGTK Failed!";
   exit;
else
   python -c "import gtk; print 'PyGTK version: %d.%d.%d' % gtk.pygtk_version";
fi 

python -c "import pysqlite2" > /dev/null 2> /dev/null
if [ $? -eq 127 ]; then
   echo "PySQLite 2 Failed!";
   exit;
else
   python -c "from pysqlite2 import dbapi2; print 'PySQLite version: %s.%s.%s' % \
dbapi2.version_info";
fi

echo "##################################################"
echo ""
echo "##################################################"
echo "Everything seens to work!"
echo "Check the following needed versions:"
echo "    * Nmap 3.95 or greater"
echo "    * Python 2.4 or greater"
echo "    * GTK 2.6 or greater"
echo "    * PyGTK 2.6 or greater"
echo "    * PySQLite 2 or greater"
echo ""
echo "If all the versions above are in agreement, you're ok to run now."
echo "##################################################"
echo ""
