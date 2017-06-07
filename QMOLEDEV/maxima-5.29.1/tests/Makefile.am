include $(top_srcdir)/common.mk
MAXIMA = @POSIX_SHELL@ $(top_srcdir)/maxima-local
if CLISP
check: check-clisp
check-clisp:
	echo "Running test suite with clisp..."; \
	$(MAXIMA) --lisp=clisp --batch-string="run_testsuite();" < /dev/null
endif

if CMUCL
check: check-cmucl
check-cmucl:
	echo "Running test suite with cmucl..."; \
	$(MAXIMA) --lisp=cmucl --batch-string="run_testsuite();" < /dev/null
endif

if SCL
check: check-scl
check-scl:
	echo "Running test suite with scl..."; \
	$(MAXIMA) --lisp=scl --batch-string="run_testsuite();" < /dev/null
endif

if SBCL
check: check-sbcl
check-sbcl:
	echo "Running test suite with sbcl..."; \
	$(MAXIMA) --lisp=sbcl --batch-string="run_testsuite();" < /dev/null
endif

if GCL
check: check-gcl
check-gcl:
	echo "Running test suite with gcl..."; \
	$(MAXIMA) --lisp=gcl --batch-string="run_testsuite();" < /dev/null
endif

if ACL
check: check-acl
check-acl:
	echo "Running test suite with acl..."; \
	$(MAXIMA) --lisp=acl --batch-string="run_testsuite();" < /dev/null
endif

if OPENMCL
check: check-openmcl
check-openmcl:
	echo "Running test suite with openmcl..."; \
	$(MAXIMA) --lisp=openmcl --batch-string="run_testsuite();" < /dev/null
endif

if ECL
check: check-ecl
check-ecl:
	echo "Running test suite with ecl..."; \
	$(MAXIMA) --lisp=ecl --batch-string="run_testsuite();" < /dev/null
endif

genericdir = $(insttestsdir)
genericdirDATA = rexamples.mac \
rtest10.mac \
rtest11.mac \
rtest12.mac \
rtest13.mac \
rtest13s.mac \
rtest14.mac \
rtest15.mac \
rtest16.mac \
rtest1a.mac \
rtest1.mac \
rtest2.mac \
rtest3.mac \
rtest4.mac \
rtest5.mac \
rtest6a.mac \
rtest6b.mac \
rtest6.mac \
rtest7.mac \
rtest8.mac \
rtest9a.mac \
rtest9.mac \
rtestode.mac \
rtestode_zp.mac \
rtestnset.mac \
rtesthyp.mac \
rtestmt19937.mac \
rtest_numth.mac \
mcond_display_elseif.mac \
mcond_display_elseif.out \
rtestmcond_display.mac \
rtest_allnummod.mac \
rtestconjugate.mac \
rtestsum.mac \
rtest_trig.mac \
rtest_zeta.mac \
rtest_diff_invtrig.mac \
rtest_scalarp.mac \
rtest_everysome.mac \
rtestint.mac \
rtestifactor.mac \
rtest_equal.mac \
rtest_abs.mac \
rtest_taylor.mac \
rtest_dot.mac \
rtest_mset.mac \
rtest_boolean.mac \
rtest_round.mac \
rtest_map.mac \
rtest_sign.mac \
rtest_algebraic.mac \
rtest_hypgeo.mac \
rtest_gamma.mac \
rtest_expintegral.mac \
rtest_signum.mac \
rtest_lambert_w.mac \
rtest_laplace.mac \
rtest_elliptic.mac \
rtest_integrate.mac \
rtest_integrate_special.mac \
rtest_plot.mac \
rtest_translator.mac \
rtest_ask.mac \
rtest_sqrt.mac \
rtest_carg.mac \
rtest_log.mac \
rtest_power.mac \
rtestdefstruct.mac \
rtest_rules.mac \
README.how-to \
testsuite.lisp \
wester_problems/wester-gpl-permission-message.txt \
wester_problems/test_algebra.mac \
wester_problems/test_boolean_logic.mac \
wester_problems/test_calculus.mac \
wester_problems/test_combinatorics.mac \
wester_problems/test_complex_domain.mac \
wester_problems/test_definite_integrals.mac \
wester_problems/test_equations.mac \
wester_problems/test_indefinite_integrals.mac \
wester_problems/test_inequalities.mac \
wester_problems/test_limits.mac \
wester_problems/test_math_vs_cs_local.mac \
wester_problems/test_math_vs_cs_match.mac \
wester_problems/test_matrix_theory.mac \
wester_problems/test_numbers.mac \
wester_problems/test_number_theory.mac \
wester_problems/test_numerical_analysis.mac \
wester_problems/test_odes.mac \
wester_problems/test_operators.mac \
wester_problems/test_pdes.mac \
wester_problems/test_pdes_heat.mac \
wester_problems/test_products.mac \
wester_problems/test_programming.mac \
wester_problems/test_programming_dif.mac \
wester_problems/test_series.mac \
wester_problems/test_set_theory.mac \
wester_problems/test_special_functions.mac \
wester_problems/test_statistics.mac \
wester_problems/test_sums.mac \
wester_problems/test_tensor_analysis.mac \
wester_problems/test_transforms.mac \
wester_problems/test_trigonometry.mac \
wester_problems/test_vector_analysis.mac \
wester_problems/test_zero_equivalence.mac

EXTRA_DIST = $(genericdirDATA)
