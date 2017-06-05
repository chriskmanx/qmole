#########################################################################
##                                                                     ##
##                         Applied Type System                         ##
##                                                                     ##
##                              Hongwei Xi                             ##
##                                                                     ##
#########################################################################

##
## ATS/Anairiats - Unleashing the Potential of Types!
##
## Copyright (C) 2002-2008 Hongwei Xi, Boston University
##
## All rights reserved
##
## ATS is  free software;  you can redistribute it and/or modify it under
## the  terms of the  GNU General Public License as published by the Free
## Software Foundation; either version 2.1, or (at your option) any later
## version.
## 
## ATS is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
## for more details.
## 
## You  should  have  received  a  copy of the GNU General Public License
## along  with  ATS;  see the  file COPYING.  If not, please write to the
## Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.
##

## ###### ###### ##

## Original Version
## Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
## Time: December 2007

## Major Revision
## Author: Yuri D'Elia (wavexx AT thregr DOT org)
## Time: June 2010

## ###### ###### ##

#
# The bootstrapping procedure:
#
# 1. Use [gcc] to compile the C code in [bootstrap0-anairiats] to build
#    the ATS compiler [atsopt]
#
#    Alternatively, use [ocamlopt] to compile the Objective Caml code in
#    [bootstrap0-geizella] to build the ATS compiler [atsopt]
#
# 2. Use [atsopt] to compile the ATS code in [src] into C code and store
#    it in [bootstrap1]
#
# 3. Use [gcc] to compile the C code in [bootstrap1] to build the ATS
#    compiler [atsopt]
#
# 4. Use [atsopt] to compile the ATS code in [utils/scripts] to build the
#    batch compilation command [atscc]
#
# 5. Use [atscc] to build libats, GC and atslex.
#

######

.SUFFIXES:
all::

######

include ../config.mk

######

#ATSHOMEQ = "$(ATSHOME)"
ATSRUNTIME := $(ATSHOMEQ)/ccomp/runtime

BOOTSTRAP1=1
ifdef BOOTSTRAP1
ATSCCFLAGS=-I$(ATSHOMEQ) -I$(ATSRUNTIME) -O2 -g
endif

LIBGMP=
ifdef HAVE_LIBGMP
LIBGMP=-lgmp
endif

LIBOBJS := $(LIBGMP)

######

SOURCES := \
  ats_array.sats ats_array.dats \
  ats_charlst.sats ats_charlst.dats \
  ats_comarg.sats ats_comarg.dats \
  ats_counter.sats ats_counter.dats \
  ats_debug.sats ats_debug.dats \
  ats_effect.sats ats_effect.dats \
  ats_error.sats ats_error.dats \
  ats_filename.sats ats_filename.dats \
  ats_fixity.sats ats_fixity_prec.dats ats_fixity_fxty.dats \
  ats_global.sats ats_global.dats \
  ats_hashtbl.sats ats_hashtbl.dats \
  ats_intinf.sats ats_intinf.dats \
  ats_keyword.sats ats_keyword.dats \
  ats_label.sats ats_label.dats \
  ats_location.sats ats_location.dats \
  ats_namespace.sats ats_namespace.dats \
  ats_posmark.sats ats_posmark.dats \
  ats_stamp.sats ats_stamp.dats \
  ats_symbol.sats ats_symbol.dats \
  ats_symenv.sats ats_symenv.dats \
  ats_symtbl.sats ats_symtbl.dats \
  ats_list.sats ats_list.dats \
  ats_map_lin.sats ats_map_lin.dats \
  ats_reference.sats ats_reference.dats \
  ats_set_fun.sats ats_set_fun.dats \
  ats_parser.sats ats_parser.dats \
  ats_syntax.sats ats_syntax.dats \
  ats_syntax_depgen.dats \
  ats_syntax_posmark.dats \
  ats_staexp1.sats ats_staexp1.dats \
  ats_staexp1_print.dats \
  ats_dynexp1.sats ats_dynexp1.dats \
  ats_dynexp1_print.dats \
  ats_dynexp1_syndef.dats \
  ats_dynexp1_syndef.sats \
  ats_trans1_env.sats ats_trans1_env.dats \
  ats_e1xp_eval.sats ats_e1xp_eval.dats \
  ats_trans1.sats ats_trans1_sta.dats ats_trans1_dyn.dats \
  ats_staexp2.sats ats_staexp2.dats \
  ats_staexp2_print.dats \
  ats_staexp2_scst.dats \
  ats_staexp2_svVar.dats \
  ats_staexp2_dcon.dats \
  ats_staexp2_util1.dats ats_staexp2_util2.dats \
  ats_staexp2_pprint.sats ats_staexp2_pprint.dats \
  ats_dynexp2.sats ats_dynexp2.dats \
  ats_dynexp2_dcst.dats \
  ats_dynexp2_dmac.dats \
  ats_dynexp2_dvar.dats \
  ats_dynexp2_print.dats \
  ats_dynexp2_util.dats \
  ats_trans2_env.sats ats_trans2_env.dats \
  ats_stadyncst2.sats ats_stadyncst2.dats \
  ats_trans2.sats \
  ats_trans2_sta.dats \
  ats_trans2_dyn1.dats ats_trans2_dyn2.dats \
  ats_macro2.sats ats_macro2.dats \
  ats_patcst2.sats ats_patcst2.dats \
  ats_string_parse.sats ats_string_parse.dats \
  ats_printf_c.lats \
  ats_dynexp3.sats ats_dynexp3.dats \
  ats_dynexp3_print.dats \
  ats_trans3_env.sats \
  ats_trans3_env.dats \
  ats_trans3_env_eff.dats \
  ats_trans3_env_loop.dats \
  ats_trans3_env_met.dats \
  ats_trans3_env_scst.dats \
  ats_trans3_env_state.dats \
  ats_trans3_env_print.dats \
  ats_staexp2_solve.sats ats_staexp2_solve.dats \
  ats_trans3.sats ats_trans3_pat.dats \
  ats_trans3_assgn.dats \
  ats_trans3_deref.dats \
  ats_trans3_view.dats \
  ats_trans3_util.dats \
  ats_trans3_exp_up.dats \
  ats_trans3_exp_dn.dats \
  ats_trans3_loop.dats ats_trans3_dec.dats \
  ats_constraint.sats ats_constraint.dats \
  ats_constraint_print.dats \
  ats_solver_fm.sats ats_solver_fm.dats \
  ats_hiexp.sats ats_hiexp.dats \
  ats_hiexp_print.dats ats_hiexp_util.dats \
  ats_trans4.sats ats_trans4.dats \
  ats_ccomp.sats ats_ccomp.dats \
  ats_ccomp_env.sats ats_ccomp_env.dats \
  ats_ccomp_print.dats \
  ats_ccomp_util.dats \
  ats_ccomp_trans.dats \
  ats_ccomp_trans_clau.dats \
  ats_ccomp_trans_tailcal.dats \
  ats_ccomp_trans_temp.dats \
  ats_ccomp_emit.dats \
  ats_ccomp_main.dats \

OBJECTS := $(SOURCES)
OBJECTS := $(patsubst %.sats, %_sats.o, $(OBJECTS))
OBJECTS := $(patsubst %.lats, %_lats.dats, $(OBJECTS))
OBJECTS := $(patsubst %.dats, %_dats.o, $(OBJECTS))

ATSOPT_GC_OBJECTS := \
  ../ccomp/runtime/GCATS/gc.o

PRELUDE_DATS_OBJECTS := \
  prelude_dats_array_dats.o \
  prelude_dats_basics_dats.o \
  prelude_dats_bool_dats.o \
  prelude_dats_file_dats.o \
  prelude_dats_integer_dats.o \
  prelude_dats_printf_dats.o \
  prelude_dats_string_dats.o

LIBC_SATS_OBJECTS := \
  libc_sats_gmp_sats.o libc_sats_time_sats.o

LIBC_DATS_OBJECTS := \
  libc_dats_unistd_dats.o

LIBATS_LEX_OBJECTS := \
  libats_lex_lexing_sats.o \
  libats_lex_lexing_dats.o \
  libats_lex_tables_dats.o

OBJECTS += \
  ats_lexer_lats_dats.o \
  ats_lexer_sats.o \
  ats_grammar_yats.o \
  $(PRELUDE_DATS_OBJECTS) \
  $(LIBC_SATS_OBJECTS) \
  $(LIBC_DATS_OBJECTS) \
  $(LIBATS_LEX_OBJECTS) \
  ats_main_dats.c \
  ../ccomp/runtime/ats_prelude.c \
  ../ccomp/runtime/ats_bootstrap.c \

######

COMPILE = $(CC) $(ATSCCFLAGS) -o $@
#COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) $(ATSCCFLAGS) -o $@
CLINK = $(CC) $(ATSCCFLAGS) $(LDFLAGS) -o $@
#CLINK = $(CC) $(CPPFLAGS) $(CFLAGS) $(ATSCCFLAGS) $(LDFLAGS) -o $@

######

#
# HX-2011-01-26: please select the proper one
#
# if [yacc] or [byacc] is used to generate [ats_grammar_yats.c]
# ATSYACCFLAGS=$(ATSCCFLAGS)
#
# select it if [bison] is used to generate [ats_grammar_yats.c]
ATSYACCFLAGS=-D_ATS_YYVALISLOCAL $(ATSCCFLAGS)
#

ats_grammar_yats.o: ats_grammar_yats.c; $(CC) $(ATSYACCFLAGS) -o $@ -c $<

######

%.o: %.c
	$(COMPILE) -c $<

######

all:: atsopt

atsopt: $(OBJECTS)
	$(CLINK) $(OBJECTS) $(LIBOBJS)

atsopt_gc: $(OBJECTS) $(ATSOPT_GC_OBJECTS)
	$(CLINK) -D_ATS_GCATS $(OBJECTS) $(ATSOPT_GC_OBJECTS) $(LIBOBJS)

######

clean::
	rm -f *.o

cleanall:: clean
	rm -f atsopt

######

## end of [Makefile] ##

######
