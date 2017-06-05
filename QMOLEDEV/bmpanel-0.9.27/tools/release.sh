#!/bin/bash
# script for releasing bmpanel

exit_with_error() {
	echo $1
	exit $2
}

if [[ $# != 1 && $# != 2 ]]; then 
	echo 'usage: release.sh GITNAME [suffix]'
	exit 1
fi

gitname=$1
suffix=$gitname
releasedir="release"
tarball=bmpanel-$suffix.tar

if [ $# == 2 ]; then
	suffix=$2
fi

echo "releasing for: $gitname"
echo "creating tarball: $releasedir/$tarball"
[ -d $releasedir ] || mkdir $releasedir
git-archive --format=tar --prefix=bmpanel-$suffix/ $gitname > $releasedir/$tarball
[ -s $releasedir/$tarball ] || exit_with_error "failed to create tarball (wrong git name?)" 1

echo "gzipping tarball to: $releasedir/$tarball.gz"
gzip -c $releasedir/$tarball > $releasedir/$tarball.gz
rm $releasedir/$tarball

echo "calculating md5sum for: $releasedir/$tarball.gz"
md5=$(md5sum $releasedir/$tarball.gz | awk '{ print $1 }')
echo "md5: $md5"

echo "creating PKGBUILD for release"
[ -d $releasedir/bmpanel ] || mkdir $releasedir/bmpanel
sed -e "s/%VERSION%/$suffix/g" -e "s/%MD5%/$md5/g" tools/PKGBUILD-pattern > $releasedir/bmpanel/PKGBUILD

echo "packing PKGBUILD to bmpanel.tar.gz"
cd release
tar -zcvf bmpanel.tar.gz bmpanel/

echo "done"
