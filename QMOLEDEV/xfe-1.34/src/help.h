#ifndef HELP_H
#define HELP_H
  
#include "config.h"
#include "i18n.h"

#define HELP_TEXT _("\n \
\n \
\n \
                                  XFE, X File Explorer File Manager\n \
\n \
                                    <http://roland65.free.fr/xfe>\n \
                                <http://sourceforge.net/projects/xfe>\n \
\n \
\n \
\n \
 [This help file is best viewed using a fixed text font. You can set it by using the font tab of the Preferences dialog.]\n \
\n \
\n \
\n \
 This program is free software; you can redistribute it and/or modify it under the terms of the GNU\n \
 General Public License as published by the Free Software Foundation; either version 2, or (at your option)\n \
 any later version.\n \
\n \
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; \n \
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. \n \
 See the GNU General Public License for more details.\n \
\n \
\n \
\n \
  Description\n \
  =-=-=-=-=-=\n \
\n \
 X File Explorer (xfe) is a lightweight file manager for X11, written using the FOX toolkit.\n \
 It is desktop independent and can easily be customized.\n \
 It has Commander or Explorer styles and it is very fast and small.\n \
 Xfe is based on the popular, but discontinued X Win Commander, originally written by Maxim Baranov.\n \
\n \
\n \
\n \
  Features\n \
  =-=-=-=-=\n \
\n \
    - Very fast graphic user interface\n \
    - UTF-8 support\n \
    - Commander/Explorer interface with four file manager modes : a) one panel, b) a directory tree\n \
      and one panel, c) two panels and d) a directory tree and two panels\n \
    - Panels synchronization and switching\n \
    - Integrated text editor (X File Write, xfw)\n \
    - Integrated text viewer (X File View, xfv)\n \
    - Integrated image viewer (X File Image, xfi)\n \
    - Integrated package (rpm or deb) viewer / installer / uninstaller (X File Package, xfp)\n \
    - Copy/Cut/Paste files from and to your favorite desktop (GNOME/KDE/XFCE/ROX)\n \
    - Drag and Drop files from and to your favorite desktop (GNOME/KDE/XFCE/ROX)\n \
    - Root mode with authentication by su or sudo\n \
    - Status line\n \
    - File associations\n \
    - Optional trash can for file delete operations (compliant with freedesktop.org standards)\n \
    - Auto save registry\n \
    - Double click or single click files and directories navigation\n \
    - Mouse right click pop-up menu in tree list and file list\n \
    - Change file attributes\n \
    - Mount/Unmount devices (Linux only)\n \
    - Warn when mount point are not responding (Linux only)\n \
    - Toolbars\n \
    - Bookmarks\n \
    - Back and forward history lists for directory navigation\n \
    - Color themes (GNOME, KDE, Windows...)\n \
    - Icon themes (Xfe, GNOME, KDE, XFCE, Tango, Windows...)\n \
    - Control themes (Standard or Clearlooks like)\n \
    - Create archives (tar, compress, zip, gzip, bzip2, xz and 7zip formats are supported)\n \
    - Extract archives (tar, compress, zip, gzip, bzip2, xz, lzh, rar, ace, arj and 7zip formats are supported)\n \
    - Tooltips with file properties\n \
    - Progress bars or dialogs for lengthy file operations\n \
    - Thumbnails image previews\n \
    - Configurable key bindings\n \
    - Startup notification (optional)\n \
    - and much more...\n \
\n \
\n \
\n \
  Default Key bindings\n \
  =-=-=-=-=-=-=-=-=-=-=\n \
\n \
 Below are the global default key bindings. These key bindings are common to all X File applications.\n \
\n \
    * Select all                         - Ctrl-A\n \
    * Copy to clipboard                  - Ctrl-C\n \
    * Search                             - Ctrl-F\n \
    * Search previous                    - Shift-Ctrl-G\n \
    * Search next                        - Ctrl-G\n \
    * Go to home directory               - Ctrl-H\n \
    * Invert selection                   - Ctrl-I\n \
    * Open file                          - Ctrl-O\n \
    * Print file                         - Ctrl-P\n \
    * Quit application                   - Ctrl-Q\n \
    * Paste from clipboard               - Ctrl-V\n \
    * Close window                       - Ctrl-W\n \
    * Cut to clipboard                   - Ctrl-X\n \
    * Deselect all                       - Ctrl-Z\n \
    * Display help                       - F1\n \
    * Create new file                    - F2\n \
    * Create new folder                  - F7\n \
    * Big icon list                      - F10\n \
    * Small icon list                    - F11\n \
    * Detailed file list                 - F12\n \
    * Toggle display hidden files        - Ctrl-F6\n \
    * Toggle display thumbnails          - Ctrl-F7\n \
    * Go to working directory            - Shift-F2\n \
    * Go to parent directory             - Backspace\n \
    * Go to previous directory           - Ctrl-Backspace\n \
    * Go to next directory               - Shift-Backspace\n \
\n \
\n \
 Below are the default X File Explorer key bindings. These key bindings are specific to the xfe application.\n \
\n \
    * Add bookmark                      - Ctrl-B\n \
    * Filter files                      - ctrl-D\n \
    * Execute command                   - Ctrl-E\n \
    * Create new symbolic link          - Ctrl-J\n \
    * Switch panels                     - Ctrl-K\n \
    * Clear location bar                - Ctrl-L\n \
    * Mount file system (Linux only)    - Ctrl-M\n \
    * Rename file                       - Ctrl-N\n \
    * Refresh panels                    - Ctrl-R\n \
    * Symlink files to location         - Ctrl-S\n \
    * Launch terminal                   - Ctrl-T\n \
    * Unmount file system (Linux only)  - Ctrl-U\n \
    * Synchronize panels                - Ctrl-Y\n \
    * Create new window                 - F3\n \
    * Edit                              - F4\n \
    * Copy files to location            - F5\n \
    * Move files to location            - F6\n \
    * File properties                   - F9\n \
    * One panel mode                    - Ctrl-F1\n \
    * Tree and panel mode               - Ctrl-F2\n \
    * Two panels mode                   - Ctrl-F3\n \
    * Tree and two panels mode          - Ctrl-F4\n \
    * Toggle display hidden directories - Ctrl-F5\n \
    * Go to trash can                   - Ctrl-F8\n \
    * Create new root window            - Shift-F3\n \
    * View                              - Shift-F4\n \
    * Move files to trash can           - Del\n \
    * Restore files from trash can      - Alt-Del\n \
    * Delete files                      - Shift-Del\n \
    * Empty trash can                   - Ctrl-Del\n \
\n \
\n \
 Below are the default X File Image key bindings. These key bindings are specific to the xfi application.\n \
\n \
    * Zoom to fit window                - Ctrl-F\n \
    * Mirror image horizontally         - Ctrl-H\n \
    * Zoom image to 100%                - Ctrl-I\n \
    * Rotate image to left              - Ctrl-L\n \
    * Rotate image to right             - Ctrl-R\n \
    * Mirror image vertically           - Ctrl-V\n \
\n \
\n \
 Below are the default X File Write key bindings. These key bindings are specific to the xfw application.\n \
\n \
    * Toggle word wrap mode             - Ctrl-K\n \
    * Goto line                         - Ctrl-L\n \
    * Create new document               - Ctrl-N\n \
    * Replace string                    - Ctrl-R\n \
    * Save changes to file              - Ctrl-S\n \
    * Toggle line numbers mode          - Ctrl-T\n \
    * Toggle upper case mode            - Shift-Ctrl-U\n \
    * Toggle lower case mode            - Ctrl-U\n \
    * Redo last change                  - Ctrl-Y\n \
    * Undo last change                  - Ctrl-Z\n \
\n \
\n \
 X File View (xfv) and X File Package (xfp) only use some of the global key bindings.\n \
\n \
 Note that all the default key bindings listed above can be customized in the xfe Preferences dialog. However,\n \
 some key actions are hardcoded an cannot be changed. These include:\n \
\n \
    * Ctrl-+ and Ctrl--                 - zoom in and zoom out image in xfi\n \
    * Shift-F10                         - display context menus in xfe\n \
    * Return                            - enter directories in file lists, open files, select button actions, etc.\n \
    * Space                             - enter directories in file lists\n \
    * Esc                               - close current dialog, unselect files, etc.\n \
\n \
\n \
\n \
  Drag and Drop operations\n \
  =-=-=-=-=-=-=-=-=-=-=-=-=\n \
\n \
 Dragging a file or group or files (by moving the mouse while maintaining the left button pressed)\n \
 to a directory or a file panel optionally opens a dialog that allows one to select the file operation: copy,\n \
 move, link or cancel.\n \
\n \
\n \
\n \
  Trash system\n \
  =-=-=-=-=-=-=\n \
\n \
 Starting with version 1.32, xfe implements a trash system that is fully compliant with the freedesktop.org standards.\n \
 This allows the user to move files to the trash can and to restore files from within xfe or your favorite\n \
 desktop.\n \
 Note that the trash files location is now: ~/.local/share/Trash/files\n \
\n \
\n \
\n \
  Configuration\n \
  =-=-=-=-=-=-=\n \
\n \
 You can perform any xfe customization (layout, file associations, key bindings, etc.) without editing any file\n \
 by hand. However, you may want to understand the configuration principles, because some customizations can also\n \
 easily be done by manually editing the configurations files.\n \
 Be careful to quit xfe before manually editing any configuration file, otherwise changes could not be taken\n \
 into account.\n \
\n \
 The system-wide configuration file xferc is located in /usr/share/xfe, /usr/local/share/xfe\n \
 or /opt/local/share/xfe, in the given order of precedence.\n \
\n \
 Starting with version 1.32, the location of the local configuration files has changed. This is to be compliant\n \
 with the freedesktop.org standards.\n \
 \n \
 The local configuration files for xfe, xfw, xfv, xfi, xfp are now located in the ~/.config/xfe directory.\n \
 They are named xferc, xfwrc, xfvrc, xfirc and xfprc.\n \
 \n \
 At the very first xfe run, the system-wide configuration file is copied into the local configuration file\n \
 ~/.config/xfe/xferc which does not exists yet. If the system-wide configuration file is not found\n \
 (in case of an unusal install place), a dialog asks the user to select the right place. It is thus easier to\n \
 customize xfe (this is particularly true for the file associations) by hand editing because all the local options are\n \
 located in the same file.\n \
\n \
 Default PNG icons are located in /usr/share/xfe/icons/xfe-theme or /usr/local/share/xfe/icons/xfe-theme, depending\n \
 on your installation. You can easily change the icon theme path in Preferences dialog.\n \
\n \
\n \
\n \
  Non Latin based languages\n \
  =-=-=-=-=-=-=-=-=-=-=-=-=\n \
\n \
 Xfe can display its user interface and also the file names in non latin character based languages, provided that you\n \
 have selected a Unicode font that supports your character set. To select a suitable font, use the\n \
 Edit / Preferences / Font menu item.\n \
\n \
 Multilingual Unicode TrueType fonts can be found at this address: http://www.slovo.info/unifonts.htm\n \
\n \
\n \
\n \
  Tips\n \
  =-=-=\n \
\n \
 File list\n \
    - Select files and right click to open a context menu on the selected files\n \
    - Press Ctrl + right click to open a context menu on the file panel\n \
    - When dragging a file/folder to a folder, hold on the mouse on the folder to open it\n \
\n \
 Tree list\n \
    - Select a folder and right click to open a context menu on the selected folder\n \
    - Press Ctrl + right click to open a context menu on the tree panel\n \
    - When dragging a file/folder to a folder, hold on the mouse on the folder to expand it\n \
\n \
 Copy/paste file names\n \
    - Select a file and press Ctrl-C to copy its name into the clipboard. Then in a dialog,press Ctrl-V to paste\n \
      the file name.\n \
    - In a file operation dialog, select a filename in the line containing the source name and paste it directly\n \
      to the destination using the middle button of your mouse. Then modify it to suit your needs.\n \
\n \
 Startup notification\n \
    - Startup notification is the process that displays a feedback (a sandbox cursor or whatever) to the user when\n \
      he has started an action (file copying, application launching, etc.). Depending on the system, there can be\n \
      some issues with startup notification. If Xfe was compiled with startup notification support, the user can\n \
      disable it for all applications at the global Preferences level. He can also disable it for individual\n \
      applications, by using the dedicated option in the first tab of the Properties dialog. This latter way is\n \
      only available when the file is an executable. Disabling startup notification can be useful when starting\n \
      an old application that doesn't support the startup notification protocol (e.g. Xterm).\n \
\n \
\n \
\n \
  Bugs\n \
  =-=-=\n \
\n \
 Please report any found bug to Roland Baudin <roland65@free.fr>. Don't forget to mention the xfe version you use,\n \
 the FOX library version and your system name and version.\n \
\n \
\n \
\n \
  Translations\n \
  =-=-=-=-=-=-=\n \
 \n \
 xfe is now available in 19 languages but some translations are only partial. To translate xfe to your language,\n \
 open the xfe.pot file located in the po directory of the source tree with a software like poedit, kbabel\n \
 or gtranslator and fill it with your translated strings (be careful to the hotkeys and c-format characters),\n \
 and then send it back to me. I'll be pleased to integrate your work in the next xfe release.\n \
\n \
\n \
\n \
  Patches\n \
  =-=-=-=\n \
\n \
 If you have coded some interesting patch, please send it to me, I will try to include it in the next release...\n \
\n \
\n \
 Many thanks to Maxim Baranov for his excellent X Win Commander and to all people that have provided useful\n \
 patches, translations, tests and advices.\n \
\n \
 [Last revision: 6/04/2012]\n \
\n \
 ")
;


#endif
