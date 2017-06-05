##########################################################################
#                                                                        #
#                        TypeRex OCaml Studio                            #
#                                                                        #
#                 Thomas Gazagnaire, Fabrice Le Fessant                  #
#                                                                        #
#  Copyright 2011-2012 OCamlPro                                          #
#  All rights reserved.  This file is distributed under the terms of     #
#  the GNU Public License version 3.0.                                   #
#                                                                        #
#  TypeRex is distributed in the hope that it will be useful,            #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU General Public License for more details.                          #
#                                                                        #
##########################################################################

-include ../../Makefile.config
OCPIDE_PLUGIN_MAKER = ../../_obuild/ocp-ide/ocp-ide.asm
OCP_WIZARD_PLUGIN_MAKER=../../_obuild/ocp-wizard/ocp-wizard.asm
LIB_IDE=../../libs/ocamlpro/ide
LIB_RPC=../../libs/ocamlpro/rpc
OCP_WIZARD=../ocp-wizard

all: emacs/typerex.el emacs/typerex.elc

emacs/typerex.el: \
	$(LIB_IDE)/emacs/faces.el \
	$(LIB_IDE)/emacs/util.el \
	$(LIB_RPC)/emacs-lisp/ocp-rpc.el \
	emacs/typerex-config.el \
	emacs/typerex-tuareg.el \
	emacs/ocp-wizard-plugin.el \
	$(OCP_WIZARD)/emacs/ocp-flymake.el \
	$(OCP_WIZARD)/emacs/ocp-tokenize.el \
	$(OCP_WIZARD)/emacs/ocp-connection.el \
	$(OCP_WIZARD)/emacs/ocp-completion.el \
	$(OCP_WIZARD)/emacs/ocp-wizard.el
	cat $^ > $@

#	emacs/ocp-ide.el \

#	@for tool in ../ocp-*; do \
#	  basename=`basename $$tool`; \
#	  el="$$tool/emacs/$$basename.el"; \
#	  if [ -e $$el ]; then \
#	    echo "cat $$el >> $@"; \
#	    cat $$el >> $@; \
#	  fi; \
#	done

$(OCPIDE):
	 make -C ../..

emacs/ocp-ide.el: $(OCPIDE_PLUGIN_MAKER)
	$< -o $@

emacs/ocp-wizard-plugin.el: $(OCP_WIZARD_PLUGIN_MAKER)
	$< emacs-plugin $@

clean:
	cd emacs ; rm -f ocp-ide.el ocp-wizard-plugin.el typerex.el

%.elc: %.el
	$(EMACS) -Q -L . -L $(EMACSDIR) -L ../../emacs-plugins \
	-batch -f batch-byte-compile $<
