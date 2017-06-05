#!/bin/bash
cd /usr/lib

if [ -d /usr/lib/classpath.new ]
then
    sudo mv classpath classpath.old
    sudo mv classpath.new classpath
fi 

cd /usr/share

if [ -d /usr/share/classpath.new ]
then
    sudo mv classpath classpath.old
    sudo mv classpath.new classpath
fi
