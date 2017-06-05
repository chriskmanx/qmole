#!/usr/bin/env bash

PERL_SCRIPT="claws-mail-kdeservicemenu.pl"
DESKTOP="claws-mail-attach-files.desktop"

function check_environ {
echo "Checking for kde4-config..."
if [ ! -z "$(type 'kde4-config' 2> /dev/null)" ]; then
  echo "Found kde4-config..."
  SERVICEMENU_DIR="share/kde4/services/ServiceMenus"
  DESKTOP_TEMPLATE="claws-mail-attach-files.desktop.kde4template"
  KDECONFIG="kde4-config"
else
  echo "kde4-config not found..."
  echo "Checking for kde-config..."
  if [ ! -z "$(type 'kde-config' 2> /dev/null)" ]; then
      echo "Found kde-config..."
      SERVICEMENU_DIR="share/apps/konqueror/servicemenus"
      DESKTOP_TEMPLATE="claws-mail-attach-files.desktop.template"
      KDECONFIG="kde-config"
  else
    echo "kde-config not found..."
    echo "asking user to find kde4-config or kde-config..."
    KDECONFIG=$(kdialog --title "Locate kde-config or kde4-config" --getopenfilename / )
    test -z $KDECONFIG && exit 1
    if [[ $KDECONFIG == *4-config ]]; then
      SERVICEMENU_DIR="share/kde4/services/ServiceMenus"
      DESKTOP_TEMPLATE="claws-mail-attach-files.desktop.kde4template"
    else
      SERVICEMENU_DIR="share/apps/konqueror/servicemenus"
      DESKTOP_TEMPLATE="claws-mail-attach-files.desktop.template"
    fi 
  fi
fi
}

function install_all {
echo "Generating $DESKTOP ..."
SED_PREFIX=${PREFIX//\//\\\/}
sed "s/SCRIPT_PATH/$SED_PREFIX\\/bin\\/$PERL_SCRIPT/" $DESKTOP_TEMPLATE > $DESKTOP
echo "Installing $PREFIX/$SERVICEMENU_DIR/$DESKTOP"
mv -f $DESKTOP $PREFIX/$SERVICEMENU_DIR/$DESKTOP
if [[ $? -ne 0 ]]
then
  kdialog --error "Could not complete installation."
  exit
fi
echo "Installing $PREFIX/bin/$PERL_SCRIPT"
cp -f $PERL_SCRIPT $PREFIX/bin/
echo "Setting permissions ..."
chmod 0644 $PREFIX/$SERVICEMENU_DIR/$DESKTOP
chmod 0755 $PREFIX/bin/$PERL_SCRIPT
echo "Finished installation."
kdialog --msgbox "Finished installation."
}

function uninstall_all {
echo "Removing $PREFIX/$SERVICEMENU_DIR/$DESKTOP"
rm $PREFIX/$SERVICEMENU_DIR/$DESKTOP
if [[ $? -ne 0 ]]
then
  kdialog --error "Could not complete uninstall."
  exit
fi
echo "Removing $PREFIX/bin/$PERL_SCRIPT"
rm $PREFIX/bin/$PERL_SCRIPT
echo "Finished uninstall."
kdialog --msgbox "Finished uninstall."
}

function show_help {
    echo "Usage: $0 [--global|--local|--uninstall-global|--uninstall-local]"
    echo
    echo "    --global            attempts a system-wide installation."
    echo "    --local             attempts to install in your home directory."
    echo "    --uninstall-global  attempts a system-wide uninstallation."
    echo "    --uninstall-local   attempts to uninstall in your home directory."
    echo
    exit 0
}

if [ -z $1 ]
    then option="--$(kdialog --menu "Please select installation type" \
				local "install for you only" \
				global "install for all users" \
				uninstall-local "uninstall for you only" \
				uninstall-global "uninstall for all users"  2> /dev/null)"
    else option=$1
fi

case $option in
  "--global" )
    check_environ
    PREFIX=$($KDECONFIG --prefix)
    echo "Installing in $PREFIX/$SERVICEMENU_DIR ..."
    if [ "$(id -u)" != "0" ]; then
	exec kdesu "$0 --global"
    fi
    install_all
    ;;
  "--local" )
    check_environ
    PREFIX=$($KDECONFIG --localprefix)
    echo "Installing in $PREFIX$SERVICEMENU_DIR ..."
    if [ ! -d $PREFIX/bin ]; then
      mkdir $PREFIX/bin
    fi
    if [ ! -d $PREFIX/$SERVICEMENU_DIR ]; then
      mkdir $PREFIX/$SERVICEMENU_DIR
    fi
    install_all
    ;;
  "--uninstall-global" )
    check_environ
    PREFIX=$($KDECONFIG --prefix)
    echo "Uninstalling from $PREFIX/$SERVICEMENU_DIR ..."
    if [ "$(id -u)" != "0" ]; then
	exec kdesu "$0 --uninstall-global"
    fi
    uninstall_all
    ;;
  "--uninstall-local" )
    check_environ
    PREFIX=$($KDECONFIG --localprefix)
    echo "Uninstalling from $PREFIX$SERVICEMENU_DIR ..."
    uninstall_all
    ;;
  "-h" )
    show_help
    ;;
  "--help" )
    show_help
    ;;
  * )
    show_help
esac

echo "Done."
