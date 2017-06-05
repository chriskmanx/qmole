#!/bin/sh
# "./check.sh files..." will validate files given on command line.
# "./check.sh" without arguments will validate all lang and styles files
# in the source directory

check_file() {
  case $1 in
  testv1.lang) ;; # skip test file for old format
  *.xml)
    xmllint --relaxng styles.rng --noout $file || exit 1
    ;;
  *)
    xmllint --relaxng language2.rng --noout $file || exit 1
    ;;
  esac
}

if [ $1 ]; then
  for file in $@; do
    check_file $file
  done
  exit 0
fi

if [ "$srcdir" ]; then
  cd $srcdir
fi

langs=""
for l in *.lang; do
  case $l in
    msil.lang) ;;
    *)
      langs="$langs $l"
      ;;
  esac
done

for file in $langs *.xml; do
  check_file $file
done
