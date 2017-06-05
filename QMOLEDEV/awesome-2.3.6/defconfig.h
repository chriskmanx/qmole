static const char *AWESOME_DEFAULT_CONFIG = 
"screen 0\n\
{\n\
    styles\n\
    {\n\
        normal\n\
        {\n\
            font = \"sans 8\"\n\
            fg = \"#dddddd\"\n\
            bg = \"#444444\"\n\
            border = \"#555555\"\n\
        }\n\
        focus\n\
        {\n\
            fg = \"#000000\"\n\
            bg = \"#535d6c\"\n\
            border = \"#535d6c\"\n\
        }\n\
        urgent\n\
        {\n\
            fg = \"#111111\"\n\
            bg = \"#ff4500\"\n\
        }\n\
    }\n\
    tags\n\
    {\n\
        tag one { }\n\
        tag two { }\n\
        tag three { }\n\
        tag four { }\n\
        tag five { }\n\
        tag six { }\n\
        tag seven { }\n\
        tag eight { }\n\
        tag nine { }\n\
    }\n\
    layouts\n\
    {\n\
        layout tile { image = \"/usr/local/share/awesome/icons/layouts/tilew.png\" }\n\
        layout tileleft { image = \"/usr/local/share/awesome/icons/layouts/tileleftw.png\" }\n\
        layout tilebottom { image = \"/usr/local/share/awesome/icons/layouts/tilebottomw.png\" }\n\
        layout tiletop { image = \"/usr/local/share/awesome/icons/layouts/tiletopw.png\" }\n\
        layout max { image = \"/usr/local/share/awesome/icons/layouts/maxw.png\" }\n\
        layout spiral { image = \"/usr/local/share/awesome/icons/layouts/spiralw.png\" }\n\
        layout dwindle { image = \"/usr/local/share/awesome/icons/layouts/dwindlew.png\" }\n\
        layout floating { image = \"/usr/local/share/awesome/icons/layouts/floatingw.png\" }\n\
    }\n\
    statusbar mystatusbar\n\
    {\n\
        position = \"top\"\n\
\n\
        taglist mytaglist\n\
        {\n\
            mouse\n\
            {\n\
                button = \"1\"\n\
                command = \"tag_view\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"1\"\n\
                modkey = {\"Mod4\"}\n\
                command = \"client_tag\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"3\"\n\
                command = \"tag_toggleview\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"3\"\n\
                modkey = {\"Mod4\"}\n\
                command = \"client_toggletag\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"4\"\n\
                command = \"tag_viewnext\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"5\"\n\
                command = \"tag_viewprev\"\n\
            }\n\
        }\n\
        layoutinfo mylayoutinfo\n\
        {\n\
            mouse\n\
            {\n\
                button = \"1\"\n\
                command = \"tag_setlayout\"\n\
                arg = \"+1\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"4\"\n\
                command = \"tag_setlayout\"\n\
                arg = \"+1\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"3\"\n\
                command = \"tag_setlayout\"\n\
                arg = \"-1\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"5\"\n\
                command = \"tag_setlayout\"\n\
                arg = \"-1\"\n\
            }\n\
        }\n\
        tasklist mytasklist\n\
        {\n\
            mouse\n\
            {\n\
                button = \"4\"\n\
                command = \"client_focusnext\"\n\
            }\n\
            mouse\n\
            {\n\
                button = \"5\"\n\
                command = \"client_focusprev\"\n\
            }\n\
            mouse\n\
            {\n\
                modkey = {\"Mod4\"}\n\
                button = \"4\"\n\
                command = \"client_swapnext\"\n\
            }\n\
            mouse\n\
            {\n\
                modkey = {\"Mod4\"}\n\
                button = \"5\"\n\
                command = \"client_swapprev\"\n\
            }\n\
        }\n\
        iconbox logo\n\
        {\n\
            image = \"/usr/local/share/awesome/icons/awesome16.png\"\n\
            mouse\n\
            {\n\
                button = \"1\"\n\
                command = \"spawn\"\n\
                arg = \"exec xterm -e man awesome\"\n\
            }\n\
        }\n\
    }\n\
}\n\
\n\
rules\n\
{\n\
    rule { name = \"Gimp\" float = true }\n\
    rule { name = \"MPlayer\" float = true }\n\
    rule { name = \"Acroread\" float = true }\n\
    rule { name = \"pinentry\" float = true }\n\
}\n\
\n\
mouse\n\
{\n\
    root\n\
    {\n\
        button = \"3\"\n\
        command = \"spawn\"\n\
        arg = \"exec xterm\"\n\
    }\n\
    root\n\
    {\n\
        button = \"4\"\n\
        command = \"tag_viewnext\"\n\
    }\n\
    root\n\
    {\n\
        button = \"5\"\n\
        command = \"tag_viewprev\"\n\
    }\n\
    client\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        button = \"1\"\n\
        command = \"client_movemouse\"\n\
    }\n\
    client\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        button = \"2\"\n\
        command = \"client_zoom\"\n\
    }\n\
    client\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        button = \"3\"\n\
        command = \"client_resizemouse\"\n\
    }\n\
    titlebar\n\
    {\n\
        button = \"1\"\n\
        command = \"client_movemouse\"\n\
    }\n\
    titlebar\n\
    {\n\
        button = \"3\"\n\
        command = \"client_resizemouse\"\n\
    }\n\
}\n\
\n\
keys\n\
{\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"F1\"\n\
        command = \"spawn\"\n\
        arg = \"exec xterm -e man `for i in /usr/share/man/man?;do ls $i; done | cut -d. -f1 | awesome-menu 'See manual page for:'`\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"F2\"\n\
        command = \"spawn\"\n\
        arg = \"exec find /usr/bin -type f -executable ! -empty | sed 's,.*/,,' | awesome-menu -e 'exec ' Execute:\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"F3\"\n\
        command = \"spawn\"\n\
        arg = \"exec xterm -e ssh `cut -d' ' -f1 ~/.ssh/known_hosts | cut -d, -f1 | awesome-menu 'ssh to:'`\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"Return\"\n\
        command = \"spawn\"\n\
        arg = \"exec xterm\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"space\"\n\
        command = \"tag_setlayout\"\n\
        arg = \"+1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"space\"\n\
        command = \"tag_setlayout\"\n\
        arg = \"-1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"b\"\n\
        command = \"statusbar_toggle\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"j\"\n\
        command = \"client_focusnext\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"k\"\n\
        command = \"client_focusprev\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"Tab\"\n\
        command = \"focus_history\"\n\
        arg = \"-1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"j\"\n\
        command = \"client_swapnext\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"k\"\n\
        command = \"client_swapprev\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"j\"\n\
        command = \"screen_focus\"\n\
        arg = \"+1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"k\"\n\
        command = \"screen_focus\"\n\
        arg = \"-1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"h\"\n\
        command = \"tag_setmwfact\"\n\
        arg = \"-0.05\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"l\"\n\
        command = \"tag_setmwfact\"\n\
        arg = \"+0.05\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"h\"\n\
        command = \"tag_setnmaster\"\n\
        arg = \"+1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"l\"\n\
        command = \"tag_setnmaster\"\n\
        arg = \"-1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"h\"\n\
        command = \"tag_setncol\"\n\
        arg = \"+1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"l\"\n\
        command = \"tag_setncol\"\n\
        arg = \"-1\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"Escape\"\n\
        command = \"tag_prev_selected\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"Left\"\n\
        command = \"tag_viewprev\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"Right\"\n\
        command = \"tag_viewnext\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"m\"\n\
        command = \"client_togglemax\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"Return\"\n\
        command = \"client_zoom\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"space\"\n\
        command = \"client_togglefloating\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        key = \"s\"\n\
        command = \"client_togglescratch\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"s\"\n\
        command = \"client_setscratch\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"c\"\n\
        command = \"client_kill\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"q\"\n\
        command = \"quit\"\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"r\"\n\
        command = \"restart\"\n\
    }\n\
    key\n\
    {\n\
       modkey = {\"Mod4\"}\n\
       key = \"0\"\n\
       command = \"tag_view\"\n\
    }\n\
    keylist\n\
    {\n\
        modkey = {\"Mod4\"}\n\
        command = \"tag_view\"\n\
        keylist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
        arglist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
    }\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        key = \"0\"\n\
        command = \"tag_toggleview\"\n\
    }\n\
    keylist\n\
    {\n\
        modkey = {\"Mod4\", \"Control\"}\n\
        command = \"tag_toggleview\"\n\
        keylist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
        arglist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
    }\n\
\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        key = \"0\"\n\
        command = \"client_tag\"\n\
    }\n\
    keylist\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\"}\n\
        command = \"client_tag\"\n\
        keylist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
        arglist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
    }\n\
\n\
    key\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\", \"Control\"}\n\
        key = \"0\"\n\
        command = \"client_toggletag\"\n\
    }\n\
    keylist\n\
    {\n\
        modkey = {\"Mod4\", \"Shift\", \"Control\"}\n\
        command = \"client_toggletag\"\n\
        keylist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
        arglist = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }\n\
    }\n\
}\n\
# vim: filetype=conf\n\
";
