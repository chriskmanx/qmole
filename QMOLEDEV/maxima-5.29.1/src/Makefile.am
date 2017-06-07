include $(top_srcdir)/common.mk

if WIN32
bin_SCRIPTS = maxima maxima-command.ico maxima.bat set_lang.vbs
else
bin_SCRIPTS = maxima rmaxima
endif

LOADDEFSYSTEM = (load "$(top_srcdir)/lisp-utils/defsystem.lisp")
LOADMAKEDEPENDS = (load "$(top_srcdir)/lisp-utils/make-depends.lisp")
DEFSYSTEMCOMPILE = (funcall (intern (symbol-name :operate-on-system) :mk) "maxima" :compile :verbose t)
DEFSYSTEMLOAD = (funcall (intern (symbol-name :operate-on-system) :mk) "maxima" :load :verbose t)
DEFSYSTEMTESTLOAD = (funcall (intern (symbol-name :operate-on-system) :mk) "maxima" :load :verbose t :test t)


if CLISP_EXEC
CLISP_MAXIMA = binary-clisp/maxima$(EXEEXT)
CLISP_INST_BIN = true
CLISP_INST_MAXIMA = $(INSTALL_PROGRAM) $(CLISP_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CLISP_MAXIMA)"
else
CLISP_MAXIMA = binary-clisp/maxima.mem
CLISP_INST_BIN = $(INSTALL_PROGRAM) @CLISP_RUNTIME_PATH@ "$(DESTDIR)$(verpkglibdir)/binary-clisp/@CLISP_RUNTIME@"
CLISP_INST_MAXIMA = $(INSTALL_DATA) $(CLISP_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CLISP_MAXIMA)"
endif

if CLISP
EXECUTECLISP = $(CLISP_NAME) -norc -q -x
all-local: sharefiles.mk  $(CLISP_MAXIMA)
install-exec-local: install-clisp
uninstall-hook: uninstall-clisp
clean: clean-clisp
distclean: clean-clisp

clisp: $(CLISP_MAXIMA)

binary-clisp/maxima.mem:
	test -d binary-clisp || mkdir binary-clisp
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:saveinitmem "$@" :init-function (function cl-user::run)))'

binary-clisp/maxima$(EXEEXT):
	test -d binary-clisp || mkdir binary-clisp
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:saveinitmem "$@" :init-function (function cl-user::run) :EXECUTABLE t))'

install-clisp:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-clisp"
	$(CLISP_INST_MAXIMA)
	$(CLISP_INST_BIN)

uninstall-clisp:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-clisp/@CLISP_RUNTIME@"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-clisp/maxima$(EXEEXT)"

clean-clisp:
	rm -rf binary-clisp

clisp-depends.mk: maxima.system
	test -d binary-clisp || mkdir binary-clisp
	$(EXECUTECLISP) '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "$$(CLISP_MAXIMA)" "clisp-depends.mk"))'
include clisp-depends.mk
endif

if CMUCL_EXEC
CMU_MAXIMA = binary-cmucl/maxima
CMU_BUILD = (echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-cmucl/maxima" :executable t :init-function '\''cl-user::run))' | $(EXECUTECMUCL))
CMU_INST_BIN = true
CMU_INST_MAXIMA = $(INSTALL_PROGRAM) $(CMU_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
else
CMU_MAXIMA = binary-cmucl/maxima.core
CMU_BUILD = (echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-cmucl/maxima.core"))' | $(EXECUTECMUCL))
CMU_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/binary-cmucl/lisp"
CMU_INST_MAXIMA = $(INSTALL_DATA) $(CMU_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
endif

if CMUCL
EXECUTECMUCL = $(CMUCL_NAME) -noinit -batch
# Newer versions of CMUCL have an INTL package that is compatible with
# maxima's.  We just bind intl::*default-domain* here so that when we
# compile the files, we will get appropriate translations.  (Otherwise
# we have to put (intl:textdomain "maxima") in each Lisp file.
INITINTL = (when (find-package "INTL") (set (find-symbol "*DEFAULT-DOMAIN*" "INTL") "maxima"))
all-local: sharefiles.mk  $(CMU_MAXIMA)
install-exec-local: install-cmucl
uninstall-hook: uninstall-cmucl
clean: clean-cmucl
distclean: clean-cmucl

cmucl: $(CMU_MAXIMA)

$(CMU_MAXIMA):
	test -d binary-cmucl || mkdir binary-cmucl
	test -d binary-cmucl/numerical || mkdir binary-cmucl/numerical
	test -d binary-cmucl/numerical/slatec || mkdir binary-cmucl/numerical/slatec
	(echo '(progn $(INITINTL) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTECMUCL)) && \
	$(CMU_BUILD)

install-cmucl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-cmucl"
	$(CMU_INST_MAXIMA)
	$(CMU_INST_BIN)

uninstall-cmucl:
	rm -f "$(DESTDIR)$(verpkglibdir)/$(CMU_MAXIMA)"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-cmucl/@CMUCL_RUNTIME@"

clean-cmucl:
	rm -rf binary-cmucl
cmucl-depends.mk: maxima.system
	test -d binary-cmucl || mkdir binary-cmucl
	echo '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "$(CMU_MAXIMA)" "cmucl-depends.mk"))' | $(EXECUTECMUCL)

include cmucl-depends.mk
endif


if SCL
EXECUTESCL = $(SCL_NAME) -noinit -batch
all-local: sharefiles.mk  binary-scl/maxima.core
install-exec-local: install-scl
uninstall-hook: uninstall-scl
clean: clean-scl
distclean: clean-scl

scl: binary-scl/maxima.core

binary-scl/maxima.core:
	test -d binary-scl || mkdir binary-scl
	test -d binary-scl/numerical || mkdir binary-scl/numerical
	test -d binary-scl/numerical/slatec || mkdir binary-scl/numerical/slatec
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTESCL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-scl/maxima.core"))' | $(EXECUTESCL))

install-scl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-scl"
	$(INSTALL_DATA) binary-scl/maxima.core "$(DESTDIR)$(verpkglibdir)/binary-scl/maxima.core"
	$(INSTALL_PROGRAM) @SCL_RUNTIME_PATH@ "$(DESTDIR)$(verpkglibdir)/binary-scl/@SCL_RUNTIME@"

uninstall-scl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-scl/maxima.core"
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-scl/@SCL_RUNTIME@"

clean-scl:
	rm -rf binary-scl
scl-depends.mk: maxima.system
	test -d binary-scl || mkdir binary-scl
	echo '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern (symbol-name :create-dependency-file) :mk) "binary-scl/maxima.core" "scl-depends.mk"))' | $(EXECUTESCL)

include scl-depends.mk
endif


if ACL
EXECUTEACL = $(ACL_NAME) -batch
all-local: sharefiles.mk  binary-acl/maxima.dxl
install-exec-local: install-acl
uninstall-hook: uninstall-acl
clean: clean-acl
distclean: clean-acl

acl:binary-acl/maxima.dxl

binary-acl/maxima.dxl:
	test -d binary-acl || mkdir binary-acl
	test -d binary-acl/numerical || mkdir binary-acl/numerical
	test -d binary-acl/numerical/slatec || mkdir binary-acl/numerical/slatec
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' | $(EXECUTEACL)) && \
	(echo '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (excl:dumplisp :name "binary-acl/maxima.dxl"))' | $(EXECUTEACL))

install-acl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-acl"
	$(INSTALL_DATA) binary-acl/maxima.dxl "$(DESTDIR)$(verpkglibdir)/binary-acl/maxima.dxl"

uninstall-acl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-acl/maxima.dxl"

clean-acl:
	rm -rf binary-acl
acl-depends.mk: maxima.system
	test -d binary-acl || mkdir binary-acl
	echo '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-acl/maxima.dxl" "acl-depends.mk"))' | $(EXECUTEACL)
include acl-depends.mk
endif

if SBCL_EXEC
SBCL_MAXIMA = binary-sbcl/maxima$(EXEEXT)
SBCL_INST_BIN = true
SBCL_INST_MAXIMA = $(INSTALL_PROGRAM) $(SBCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)"
else
SBCL_MAXIMA = binary-sbcl/maxima.core
## FIXME:  This rule can't work.  Need to replace "lisp" with the 
## sbcl_runtime, after defining that in configure.in
## See install-clisp or install-scl.  DB 2012-06-07
SBCL_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/sbcl/lisp"
SBCL_INST_MAXIMA = $(INSTALL_DATA) $(SBCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)"
endif

if SBCL
EXECUTESBCL = "$(SBCL_NAME)" --noinform --noprint --eval
all-local: sharefiles.mk $(SBCL_MAXIMA)
install-exec-local: install-sbcl
uninstall-hook: uninstall-sbcl
clean: clean-sbcl
distclean: clean-sbcl

sbcl:binary-sbcl/maxima.core

binary-sbcl/maxima.core:
	test -d binary-sbcl || mkdir binary-sbcl
	test -d binary-sbcl/numerical || mkdir binary-sbcl/numerical
	test -d binary-sbcl/numerical/slatec || mkdir binary-sbcl/numerical/slatec
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit))' && \
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (sb-ext:save-lisp-and-die "$@") (sb-ext:quit))'

binary-sbcl/maxima$(EXEEXT):
	test -d binary-sbcl || mkdir binary-sbcl
	test -d binary-sbcl/numerical || mkdir binary-sbcl/numerical
	test -d binary-sbcl/numerical/slatec || mkdir binary-sbcl/numerical/slatec
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit))' && \
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (sb-ext:save-lisp-and-die "$@" :executable t) (sb-ext:quit))'

install-sbcl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-sbcl
	$(SBCL_INST_MAXIMA)

uninstall-sbcl:
	rm -f $(DESTDIR)$(verpkglibdir)/$(SBCL_MAXIMA)

clean-sbcl:
	rm -rf binary-sbcl
	rm -f numerical/binary-sbcl numerical/slatec/binary-sbcl
sbcl-depends.mk: maxima.system
	test -d binary-sbcl || mkdir binary-sbcl
	$(EXECUTESBCL) '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "$$(SBCL_MAXIMA)" "sbcl-depends.mk") (sb-ext:quit))'
include sbcl-depends.mk
endif

if GCL
EXECUTEGCL = $(GCL_NAME) -batch -eval
all-local: sharefiles.mk  binary-gcl/maxima
install-exec-local: install-gcl
uninstall-hook: uninstall-gcl
clean: clean-gcl
distclean: clean-gcl

sys-proclaim:
	rm -rf binary-gcl
	test -d binary-gcl || mkdir binary-gcl
	test -d binary-gcl/numerical || mkdir binary-gcl/numerical
	test -d binary-gcl/numerical/slatec || mkdir binary-gcl/numerical/slatec
	$(EXECUTEGCL) '(progn (load "generate-sys-proclaim.lisp"))'

gcl:binary-gcl/maxima

binary-gcl/maxima:
if GCL_ALT_LINK
	test -d binary-gcl || mkdir binary-gcl
	test -d binary-gcl/numerical || mkdir binary-gcl/numerical
	test -d binary-gcl/numerical/slatec || mkdir binary-gcl/numerical/slatec
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE))' && \
	$(EXECUTEGCL) '(let ((com (quote (progn (defvar compiler::*gazonk-prefix* "gazonk") (defun compiler::gazonk-name (&aux tem)(dotimes (i 1000) (unless (probe-file (setq tem (merge-pathnames (format nil "~d~d.lsp" compiler::*gazonk-prefix* i))))(return-from compiler::gazonk-name (pathname tem))))(error "1000 gazonk names used already!"))(let ((compiler::*gazonk-prefix* "maxima_gazonk")(compiler::*keep-gaz* t))$(LOADDEFSYSTEM)$(DEFSYSTEMLOAD))))))(let ((si::*collect-binary-modules* t))(eval com)(let ((compiler::*default-system-p* t))(dolist (l (directory "maxima_gazonk*.lsp")) (compile-file l) (delete-file l)))(compiler::link si::*binary-modules* "binary-gcl/maxima" (format nil "~S" com) "" nil)(dolist (l (directory "maxima_gazonk*.lsp")) (delete-file l))))'
else
	test -d binary-gcl || mkdir binary-gcl
	test -d binary-gcl/numerical || mkdir binary-gcl/numerical
	test -d binary-gcl/numerical/slatec || mkdir binary-gcl/numerical/slatec
if WIN32
	$(EXECUTEGCL) '(progn (compiler::emit-fn t))' -eval '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) $(LOADMAKEPROCLAIM) (compiler::make-all-proclaims "*/*.fn" "*/*/*/*.fn"))' && \
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (when (fboundp (quote si::sgc-on))(si::sgc-on t)) (si:save-system "binary-gcl/maxima"))'
else
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) (compiler::emit-fn t) $(DEFSYSTEMCOMPILE) $(LOADMAKEPROCLAIM) (compiler::make-all-proclaims "*/*.fn" "*/*/*/*.fn"))' && \
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (when (fboundp (quote si::sgc-on))(si::sgc-on t)) (si:save-system "binary-gcl/maxima"))'
endif	
endif

install-gcl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-gcl"
	$(INSTALL_PROGRAM) binary-gcl/maxima "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"

uninstall-gcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"
clean-gcl:
	rm -rf binary-gcl

gcl-depends.mk: maxima.system
	test -d binary-gcl || mkdir binary-gcl
	$(EXECUTEGCL) '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-gcl/maxima" "gcl-depends.mk"))'
include gcl-depends.mk
endif

if OPENMCL_EXEC
OPENMCL_MAXIMA = binary-openmcl/maxima$(EXEEXT)
OPENMCL_INST_BIN = true
OPENMCL_INST_MAXIMA = $(INSTALL_PROGRAM) $(OPENMCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(OPENMCL_MAXIMA)"
else
OPENMCL_MAXIMA = binary-openmcl/maxima.image
## FIXME:  This rule can't work.  Need to replace "lisp" with the 
## openmcl_runtime, after defining that in configure.in
## See install-clisp or install-scl.  DB 2012-06-07
OPENMCL_INST_BIN = $(INSTALL_PROGRAM) lisp "$(DESTDIR)$(verpkglibdir)/binary-opencl/lisp"
OPENMCL_INST_MAXIMA = $(INSTALL_DATA) $(OPENMCL_MAXIMA) "$(DESTDIR)$(verpkglibdir)/$(OPENMCL_MAXIMA)"
endif

if OPENMCL
EXECUTEOPENMCL = $(OPENMCL_NAME) -e
all-local: sharefiles.mk  $(OPENMCL_MAXIMA)
install-exec-local: install-openmcl
uninstall-hook: uninstall-openmcl
clean: clean-openmcl
distclean: clean-openmcl

openmcl:$(OPENMCL_MAXIMA)

binary-openmcl/maxima.image:
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@") (ccl::quit))'

binary-openmcl/maxima$(EXEEXT):
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit))' && \
	$(EXECUTEOPENMCL) \
	  '(progn (require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@" :prepend-kernel t) (ccl::quit))'

install-openmcl: $(OPENMCL_MAXIMA)
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-openmcl"
	$(OPENMCL_INST_MAXIMA)

uninstall-openmcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-openmcl/$(OPENMCL_MAXIMA)"

clean-openmcl:
	rm -rf binary-openmcl

openmcl-depends.mk: maxima.system
	test -d binary-openmcl || mkdir binary-openmcl
	$(EXECUTEOPENMCL) '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "$$(OPENMCL_MAXIMA)" "openmcl-depends.mk") (ccl:quit))'
include openmcl-depends.mk
endif

if ECL
EXECUTEECL = $(ECL_NAME) -norc
all-local: sharefiles.mk  binary-ecl/maxima
install-exec-local: install-ecl
uninstall-hook: uninstall-ecl
clean: clean-ecl
distclean: clean-ecl

ecl:binary-ecl/maxima

binary-ecl/maxima:
	test -d binary-ecl || mkdir binary-ecl
	$(EXECUTEECL) -eval '(progn $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (build-maxima-lib))' -eval '(ext:quit)'

install-ecl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-ecl"
	$(INSTALL_PROGRAM) binary-ecl/maxima "$(DESTDIR)$(verpkglibdir)/binary-ecl/maxima"

uninstall-ecl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-ecl/maxima"

clean-ecl:
	rm -rf binary-ecl libmaxima.a

ecl-depends.mk: maxima.system
	test -d binary-ecl || mkdir binary-ecl
	$(EXECUTEECL) -eval '(progn $(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-ecl/maxima" "ecl-depends.mk") (quit))'
include ecl-depends.mk
endif

echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"

# Create the list of files that will be installed.  If there is no
# difference between what make_share_list computes and the original
# file, do nothing.  Otherwise, update the original with the new
# output of make_share_list.  The update will cause share/Makefile to
# be rebuilt, and thus have the correct list of files to be installed.

sharefiles.mk : FORCE
	../admin/make_share_list

# Should we use GNU make .PHONY instead?
FORCE :

genericdir = $(instsrcdir)
real_lisp_sources = acall.lisp \
airy.lisp \
algfac.lisp \
algsys.lisp \
ar.lisp \
askp.lisp \
asum.lisp \
autol.lisp \
bessel.lisp \
buildq.lisp \
cl-info.lisp \
clmacs.lisp \
combin.lisp \
comm.lisp \
comm2.lisp \
command-line.lisp \
commac.lisp \
compar.lisp \
compat.lisp \
conjugate.lisp \
cpoly.lisp \
csimp.lisp \
csimp2.lisp \
db.lisp \
defcal.lisp \
defint.lisp \
desoln.lisp \
displa.lisp \
displm.lisp \
dskfn.lisp \
ecl-port.lisp \
elim.lisp \
ellipt.lisp \
evalw.lisp \
expintegral.lisp \
ezgcd.lisp \
factor.lisp \
fcall.lisp \
float.lisp \
fortra.lisp \
gamma.lisp \
generate-sys-proclaim.lisp \
generr.lisp \
getopt.lisp \
gnuplot_def.lisp \
grind.lisp \
hayat.lisp \
homog.lisp \
hyp.lisp \
hypgeo.lisp \
ifactor.lisp \
init-cl.lisp \
inmis.lisp \
intl.lisp \
intpol.lisp \
invert.lisp \
irinte.lisp \
laplac.lisp \
lesfac.lisp \
letmac.lisp \
limit.lisp \
linnew.lisp \
lmdcls.lisp \
logarc.lisp \
macdes.lisp \
macsys.lisp \
mactex.lisp \
marray.lisp \
mat.lisp \
matcom.lisp \
matrix.lisp \
matrun.lisp \
max_ext.lisp \
maxima-package.lisp \
maxmac.lisp \
maxmin.lisp \
mdebug.lisp \
mdefun.lisp \
mdot.lisp \
merror.lisp \
mforma.lisp \
mformt.lisp \
mhayat.lisp \
mlisp.lisp \
mload.lisp \
mmacro.lisp \
mopers.lisp \
mormac.lisp \
mrgmac.lisp \
mstuff.lisp \
mtrace.lisp \
mutils.lisp \
nalgfa.lisp \
newdet.lisp \
newinv.lisp \
nforma.lisp \
nisimp.lisp \
nparse.lisp \
nrat4.lisp \
nregex.lisp \
nset.lisp \
nummod.lisp \
numeric.lisp \
numth.lisp \
opers.lisp \
optim.lisp \
optimize.lisp \
option.lisp \
outmis.lisp \
pade.lisp \
plot.lisp \
plasma.lisp \
pois2.lisp \
pois3.lisp \
polyrz.lisp \
psolve.lisp \
rand-mt19937.lisp \
rat3a.lisp \
rat3b.lisp \
rat3c.lisp \
rat3d.lisp \
rat3e.lisp \
ratmac.lisp \
ratout.lisp \
residu.lisp \
result.lisp \
risch.lisp \
rpart.lisp \
rzmac.lisp \
schatc.lisp \
scs.lisp \
series.lisp \
server.lisp \
simp.lisp \
sin.lisp \
sinint.lisp \
sloop.lisp \
solve.lisp \
specfn.lisp \
spgcd.lisp \
sprdet.lisp \
strmac.lisp \
sublis.lisp \
sumcon.lisp \
suprv1.lisp \
sys-proclaim.lisp \
tlimit.lisp \
todd-coxeter.lisp \
trans1.lisp \
trans2.lisp \
trans3.lisp \
trans4.lisp \
trans5.lisp \
transf.lisp \
transl.lisp \
transm.lisp \
transq.lisp \
transs.lisp \
trdata.lisp \
trgred.lisp \
trigi.lisp \
trigo.lisp \
trmode.lisp \
troper.lisp \
trpred.lisp \
trprop.lisp \
trutil.lisp \
ufact.lisp \
utils.lisp \
xmaxima_def.lisp \
zero.lisp \
autoconf-variables.lisp \
numerical/f2cl-lib.lisp \
numerical/f2cl-package.lisp \
numerical/slatec/d9aimp.lisp \
numerical/slatec/d9b0mp.lisp \
numerical/slatec/d9b1mp.lisp \
numerical/slatec/d9lgmc.lisp \
numerical/slatec/d9upak.lisp \
numerical/slatec/daie.lisp \
numerical/slatec/dai.lisp \
numerical/slatec/dasyik.lisp \
numerical/slatec/dasyjy.lisp \
numerical/slatec/dbesi0.lisp \
numerical/slatec/dbesi1.lisp \
numerical/slatec/dbesi.lisp \
numerical/slatec/dbesj0.lisp \
numerical/slatec/dbesj1.lisp \
numerical/slatec/dbesj.lisp \
numerical/slatec/dbesk0.lisp \
numerical/slatec/dbesk1.lisp \
numerical/slatec/dbesk.lisp \
numerical/slatec/dbesy0.lisp \
numerical/slatec/dbesy1.lisp \
numerical/slatec/dbesy.lisp \
numerical/slatec/dbi.lisp \
numerical/slatec/dbie.lisp \
numerical/slatec/dbsi0e.lisp \
numerical/slatec/dbsi1e.lisp \
numerical/slatec/dbsk0e.lisp \
numerical/slatec/dbsk1e.lisp \
numerical/slatec/dbsknu.lisp \
numerical/slatec/dbsynu.lisp \
numerical/slatec/dcsevl.lisp \
numerical/slatec/de1.lisp \
numerical/slatec/dei.lisp \
numerical/slatec/derfc.lisp \
numerical/slatec/derf.lisp \
numerical/slatec/dgamlm.lisp \
numerical/slatec/dgamln.lisp \
numerical/slatec/dgamma.lisp \
numerical/slatec/dgtsl.lisp \
numerical/slatec/djairy.lisp \
numerical/slatec/dlngam.lisp \
numerical/slatec/dqage.lisp \
numerical/slatec/dqagie.lisp \
numerical/slatec/dqagi.lisp \
numerical/slatec/dqag.lisp \
numerical/slatec/dqagp.lisp \
numerical/slatec/dqagpe.lisp \
numerical/slatec/dqagse.lisp \
numerical/slatec/dqags.lisp \
numerical/slatec/dqawce.lisp \
numerical/slatec/dqawc.lisp \
numerical/slatec/dqawfe.lisp \
numerical/slatec/dqawf.lisp \
numerical/slatec/dqawoe.lisp \
numerical/slatec/dqawo.lisp \
numerical/slatec/dqawse.lisp \
numerical/slatec/dqaws.lisp \
numerical/slatec/dqc25c.lisp \
numerical/slatec/dqc25f.lisp \
numerical/slatec/dqc25s.lisp \
numerical/slatec/dqcheb.lisp \
numerical/slatec/dqelg.lisp \
numerical/slatec/dqk15i.lisp \
numerical/slatec/dqk15.lisp \
numerical/slatec/dqk15w.lisp \
numerical/slatec/dqk21.lisp \
numerical/slatec/dqk31.lisp \
numerical/slatec/dqk41.lisp \
numerical/slatec/dqk51.lisp \
numerical/slatec/dqk61.lisp \
numerical/slatec/dqmomo.lisp \
numerical/slatec/dqng.lisp \
numerical/slatec/dqpsrt.lisp \
numerical/slatec/dqwgtc.lisp \
numerical/slatec/dqwgtf.lisp \
numerical/slatec/dqwgts.lisp \
numerical/slatec/dyairy.lisp \
numerical/slatec/dspenc.lisp \
numerical/slatec/fdump.lisp \
numerical/slatec/initds.lisp \
numerical/slatec/j4save.lisp \
numerical/slatec.lisp \
numerical/slatec/quadpack.lisp \
numerical/slatec/quadpack.system \
numerical/slatec/README \
numerical/slatec/slatec.system \
numerical/slatec/xercnt.lisp \
numerical/slatec/xerhlt.lisp \
numerical/slatec/xermsg.lisp \
numerical/slatec/xerprn.lisp \
numerical/slatec/xersve.lisp \
numerical/slatec/xgetua.lisp \
numerical/slatec/zabs.lisp \
numerical/slatec/zacai.lisp \
numerical/slatec/zacon.lisp \
numerical/slatec/zairy.lisp \
numerical/slatec/zasyi.lisp \
numerical/slatec/zbesh.lisp \
numerical/slatec/zbesi.lisp \
numerical/slatec/zbesj.lisp \
numerical/slatec/zbesk.lisp \
numerical/slatec/zbesy.lisp \
numerical/slatec/zbinu.lisp \
numerical/slatec/zbiry.lisp \
numerical/slatec/zbknu.lisp \
numerical/slatec/zbuni.lisp \
numerical/slatec/zbunk.lisp \
numerical/slatec/zdiv.lisp \
numerical/slatec/zexp.lisp \
numerical/slatec/zkscl.lisp \
numerical/slatec/zlog.lisp \
numerical/slatec/zmlri.lisp \
numerical/slatec/zmlt.lisp \
numerical/slatec/zrati.lisp \
numerical/slatec/zs1s2.lisp \
numerical/slatec/zseri.lisp \
numerical/slatec/zshch.lisp \
numerical/slatec/zsqrt.lisp \
numerical/slatec/zuchk.lisp \
numerical/slatec/zunhj.lisp \
numerical/slatec/zuni1.lisp \
numerical/slatec/zuni2.lisp \
numerical/slatec/zunik.lisp \
numerical/slatec/zunk1.lisp \
numerical/slatec/zunk2.lisp \
numerical/slatec/zuoik.lisp \
numerical/slatec/zwrsk.lisp


genericdirDATA = $(real_lisp_sources) autoconf-variables.lisp

EXTRA_DIST = $(real_lisp_sources) maxima.asd maxima.system clisp-depends.mk  cmucl-depends.mk  scl-depends.mk  gcl-depends.mk acl-depends.mk openmcl-depends.mk ecl-depends.mk sbcl-depends.mk numerical/slatec/fortran maxima-build.lisp maxima-command.ico set_lang.vbs lisp sharefiles.mk
