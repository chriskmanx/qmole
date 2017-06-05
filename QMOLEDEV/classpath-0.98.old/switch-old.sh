#!/bin/bash
cd /usr/lib

if [ -d /usr/lib/classpath.old ]
then
    sudo mv classpath classpath.new
    sudo mv classpath.old classpath
fi

cd /usr/share

if [ -d /usr/share/classpath.old ]
then
    sudo mv classpath classpath.new
    sudo mv classpath.old classpath
fi
