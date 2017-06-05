-include Makefile.config

all: compile

# Workaround ocp-build bug #92
FIX=typerex-config ocplib-lang ocplib-system ocaml-config
compile: ./_obuild/unixrun ocp-typerex.ocp
	$(OCPBUILD) -init -scan -sanitize $(FIX) $(TOOLS)
	make -C tools/ocp-typerex-ide
	@echo "TypeRex compilation successful"

ocp-typerex.ocp: ocp-typerex.ocp.template Makefile.config
	@echo genetaring $@
	@echo "(* Generated from $< *)" >$@
	@cat  $< | \
	sed -e 's/OCP_BUILD_OCAMLC/$(subst /,\/,$(OCP_BUILD_OCAMLC))/g' | \
	sed -e 's/OCP_BUILD_OCAMLOPT/$(subst /,\/,$(OCP_BUILD_OCAMLOPT))/g' | \
	sed -e 's/OCP_BUILD_OCAMLDEP/$(subst /,\/,$(OCP_BUILD_OCAMLDEP))/g' | \
	sed -e 's/OCP_BUILD_OCAMLLEX/$(subst /,\/,$(OCP_BUILD_OCAMLLEX))/g' | \
	sed -e 's/OCP_BUILD_OCAMLYACC/$(subst /,\/,$(OCP_BUILD_OCAMLYACC))/g' | \
	sed -e 's/OCP_BUILD_CCOPT/$(subst /,\/,$(OCP_BUILD_CCOPT))/g' | \
	cat >>$@

./_obuild/unixrun:
	mkdir -p ./_obuild
	ocamlc -o ./_obuild/unixrun -make-runtime unix.cma

scan: ./_obuild/unixrun
	$(OCPBUILD) -scan
sanitize: ./_obuild/unixrun
	$(OCPBUILD) -sanitize
byte: ./_obuild/unixrun
	$(OCPBUILD) -byte
opt: ./_obuild/unixrun
	$(OCPBUILD) -asm

ocp-build:
	$(OCPBUILD) ocp-build

tests: all
	$(UNIT_TESTS)

#	ocp-build
TOOLS=\
	ocp-type \
	ocp-wrapper \
	ocp-wizard

install: install-binaries install-emacs
	@echo "\nInstallation complete."
	@echo \
	  "Important notice: TypeRex is incompatible with tuareg-mode. Do not enable both!"
	@echo "Remember to configure Emacs for TypeRex. You may do it by running:"
	@echo "    cat emacs.append >>~/.emacs"

install-binaries:
	mkdir -p $(BINDIR)
	@for f in $(TOOLS); do \
	  echo "install _obuild/$$f/$$f.asm $(BINDIR)/$$f"; \
	  install _obuild/$$f/$$f.asm $(BINDIR)/$$f; \
	done
	@for compiler in ocamlc ocamlopt ocamlc.opt ocamlopt.opt; do \
	  echo "install _obuild/ocp-wrapper/ocp-wrapper.asm" \
          "$(BINDIR)/ocp-$$compiler"; \
	  install _obuild/ocp-wrapper/ocp-wrapper.asm \
          $(BINDIR)/ocp-$$compiler; \
	done

uninstall:
	rm -f $(EMACSDIR)/typerex.el $(EMACSDIR)/typerex.elc
	@for f in $(TOOLS); do \
	  echo "rm -f $(BINDIR)/$$f"; \
	  rm -f $(BINDIR)/$$f; \
	done
	@for compiler in ocamlc ocamlopt ocamlc.opt ocamlopt.opt; do \
	  echo "rm $(BINDIR)/ocp-wrapper-$$compiler"; \
	  rm $(BINDIR)/ocp-$$compiler; \
	done
	@echo "Remember to remove the TypeRex section in your .emacs"
ifdef INSTALL_AUTO_COMPLETE
	@echo "Emacs Auto Complete Mode was not deleted. To remove it, run:"
	@echo "    cd $(EMACSDIR)"
	@echo "    rm -r auto-complete* popup.* fuzzy.* ac-dict"
endif

clean: clean-temps
	-$(OCPBUILD) -clean
	rm -f tools/ocp-wizard/completion/syntactic/owz_parser.mli
	make -C tools/ocp-typerex-ide clean
	make -C emacs-plugins clean

clean-temps:
	find . -name '*~' -exec rm -f {} \;
	find . -name '#*#' -exec rm -f {} \;
	find . -name '*.annot' -exec rm -f {} \;
	find . -name '*.cmt' -exec rm -f {} \;
	find . -name '*.cmti' -exec rm -f {} \;
	find . -name '*.last_compiled' -and \
		-not -path "./tools/ocp-wizard/test/errors.ml.last_compiled" \
		-exec rm -f {} \;
	find docs -name '*.aux' -exec rm -f {} \;
	find docs -name '*.log' -exec rm -f {} \;
	find docs -name '*.toc' -exec rm -f {} \;

distclean: clean
	-$(OCPBUILD) -distclean
	rm -rf _obuild ocp-build.root ocp-build.root.old
	Makefile.config ocp-typerex.ocp emacs.append

# update boot/ with and check it works
bootstrap: byte
	mv boot Saved
	mkdir boot
	mv Saved boot/Saved
	_obuild/ocp-bytehack/ocp-bytehack.byte -static _obuild/ocp-build/ocp-build.byte -o boot/ocp-build.boot
	$(MAKE) clean
	$(MAKE) byte

# restore saved version of boot/
restore:
	mv boot/Saved Current
	mv boot boot-to-remove
	mv Current boot
	rm -rf boot-to-remove

# clean all old versions of boot/./
bootclean:
	rm -rf boot/Saved

runner-from-unix:
	ocamlc -custom -make-runtime -o boot/ocp-build-runner -linkall unix.a unix.cma

make-boot:
	ocamlc -o boot/ocp-build.boot -use-runtime  boot/ocp-build-runner \
	   -use-prims boot/prims_needed.txt \
	   unix.cma \
           ./_obuild/ocplib-lang/ocplib-lang.cma \
           ./_obuild/ocplib-system/ocplib-system.cma \
           ./_obuild/ocp-build-engine/ocp-build-engine.cma \
           ./_obuild/ocp-build-lib/ocp-build-lib.cma \
           ./_obuild/ocp-build/buildMain.cmo

user-manual:
	find libs -name 'user-manual.tex' -exec cat {} \; > docs/user-manual/auto-libs.tex
	find tools -name 'user-manual.tex' -exec cat {} \; > docs/user-manual/auto-tools.tex
	cd docs/user-manual; $(MAKE)

# Emacs
install-emacs:
	mkdir -p $(EMACSDIR)
	install tools/ocp-typerex-ide/emacs/typerex.el $(EMACSDIR)/typerex.el
	@if [ -z "$(NOCOMPILE)" ]; then \
	echo \
	  "install tools/ocp-ide/emacs/typerex.elc $(EMACSDIR)/typerex.elc"; \
	install tools/ocp-typerex-ide/emacs/typerex.elc $(EMACSDIR)/typerex.elc; \
	fi
ifdef INSTALL_AUTO_COMPLETE
	@mkdir -p $(EMACSDIR)/auto-complete-mode
	@echo `make -s -C emacs-plugins install DIR=$(EMACSDIR)/auto-complete-mode &>/dev/null` > /dev/null
endif
