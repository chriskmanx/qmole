" Vim syntax file
" Language: bmpanel2 themes
" Maintainer: nsf <no.smile.face@gmail.com>
"
" I use "au BufRead,BufNewFile theme setf bmpanel2" in a .vimrc
"-------------------------------------------------------------------------------------

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn region bmpanel2_root_key		matchgroup=Normal start="^" end="\s\|\n"
syn region bmpanel2_nonroot_key		matchgroup=Normal start="^\s\+" end="\s\|\n"
syn match bmpanel2_comment		"^\s*#.*$"

hi def link bmpanel2_comment		Comment
hi def link bmpanel2_root_key		Special
hi def link bmpanel2_nonroot_key	Constant
