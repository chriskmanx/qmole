
"""pygobject is now installed on your machine.

Local configuration files were successfully updated."""

import os, re, sys

prefix_pattern=re.compile("^prefix=.*")


def replace_prefix(s):
    if prefix_pattern.match(s):
        s='prefix='+sys.prefix.replace("\\","/")+'\n'
    s=s.replace("@DATADIR@",
                os.path.join(sys.prefix,'share').replace("\\","/"))
    
    return s

# TODO : Check that shortcuts are created system-wide when the user
# has admin rights (hint: see pywin32 postinstall)
def create_shortcuts():
    progs_folder= get_special_folder_path("CSIDL_COMMON_PROGRAMS")
    site_packages_dir = os.path.join(sys.prefix , 'lib','site-packages')
   
    pygtk_shortcuts = os.path.join(progs_folder, 'PyGTK')
    if not os.path.isdir(pygtk_shortcuts):
        os.mkdir(pygtk_shortcuts)
       
    pygobject_doc_link=os.path.join(pygtk_shortcuts,
                                    'PyGObject Documentation.lnk')
    if os.path.isfile(pygobject_doc_link):   
        os.remove(pygobject_doc_link)
    
    create_shortcut(os.path.join(sys.prefix,'share','gtk-doc','html',
                                 'pygobject','index.html'),
                    'PyGObject Documentation', pygobject_doc_link)

def remove_shortcuts():
    pygtk_shortcuts = os.path.join(
        get_special_folder_path('CSIDL_COMMON_PROGRAMS'), 'PyGTK')
    os.remove(os.path.join(pygtk_shortcuts,'PyGObject Documentation.lnk'))
    try:
        os.rmdir(pygtk_shortcuts)
    except OSError, e:
        # Directory is not empty, so leave it like that !
        pass

if len(sys.argv) == 2:
    if sys.argv[1] == "-install":
        filenames=['lib/pkgconfig/pygobject-2.0.pc',
                   'share/pygobject/xsl/fixxref.py']
        for filename in filenames: 
            pkgconfig_file = os.path.normpath(
                os.path.join(sys.prefix,filename))

            lines=open(pkgconfig_file).readlines()
            open(pkgconfig_file, 'w').writelines(map(replace_prefix,lines))
        # TODO: Add an installer option for shortcut creation 
        # create_shortcuts()
        print __doc__
    elif sys.argv[1] == "-remove":
        pass
        # remove_shortcuts()
