#!/bin/bash
rm ~/ldid.log
FILES=`ls /usr/local/_bin/`
cd /usr/local/_bin/
for f in $FILES
do
 MYTYPE=`file -b $f`
 if [ "$MYTYPE" = "Mach-O executable acorn" ]; then
   echo "Processing $f"
   echo "Signing $f " > ~/ldid.log
   ldid -S $f 2>&1 > ~/ldid.log
 else
   echo "Skipping $f, file type is: " $MYTYPE
   echo "Skipping $f, file type is: " $MYTYPE > ~/ldid.log
 fi
done
