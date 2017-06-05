#!/bin/bash

if [ $# -ne 1 ]; then
	echo "usage: bumpversion.sh VERSION"
	echo "script will update src/version.h file and add commit to git"
	exit 1
fi

VERSION=$1

echo "bumping version up to $VERSION"

echo "writing $VERSION to src/version.h..."
echo "#define BMPANEL_VERSION \"$VERSION\"" > src/version.h

echo "commiting changes..."
git commit -a -m "Version bump up to $VERSION"

echo "tagging commit..."
git tag $VERSION
