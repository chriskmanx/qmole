# Set OCamlMakefile to use
export OCAMLMAKEFILE = ../OCamlMakefile

# Export some common variable settings
export THREADS = yes

# Define project "ex1"
define PROJ_ex1
  SOURCES = ex1.ml
  RESULT = ex1
endef
export PROJ_ex1

# Define project "ex2"
define PROJ_ex2
  SOURCES = ex2.ml
  RESULT = ex2
endef
export PROJ_ex2

# If the environment does not define subprojects to handle,
# then use "ex1 ex2" as default
ifndef SUBPROJS
  export SUBPROJS = ex1 ex2
endif

# Default target to use
all:	bc

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
