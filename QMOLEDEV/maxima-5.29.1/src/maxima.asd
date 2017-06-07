; -*- Lisp -*-

; Copyright 2007 by Luigi Panzeri.
; This file is released under the terms of
; the GNU General Public License, version 2.

(in-package :asdf)

;; Don't try to optimize so much in ECL.
;; Therefore functions can be redefined (essential for share libraries).
#+ecl (declaim (optimize (debug 2)))

(defvar *binary-output-dir* "binary-ecl")

(defmethod output-files :around ((operation compile-op) (c source-file))
  (let* ((source (component-pathname c))
        (source-dir (pathname-directory source))
        (paths (call-next-method))
        (this-dir (pathname-directory (first (directory ""))))
        (binary-dir (append this-dir (list *binary-output-dir*))))
    (mapcar #'(lambda (path)
               (merge-pathnames 
                    (make-pathname 
                        :directory
                            (append binary-dir 
                                (last source-dir 
                                        (- (length source-dir) 
                                            (length this-dir)))))
                    path))
            paths)))

(in-package :cl-user)

(defvar *maxima-build-time* (multiple-value-list (get-decoded-time)))
(export '*maxima-build-time*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :maxima.system)
    (defpackage :maxima.system
      (:use :common-lisp :asdf))))

(in-package :maxima.system)

(defsystem :maxima
  :description "Maxima is a symbolic computation program." 
  :licence "GPL" 
  :serial t
  :components (
	       (:module package :pathname ""
			:components (#-gcl(:file "maxima-package")
					  #+ecl (:file "ecl-port")
					  (:file "autoconf-variables" :depends-on ("maxima-package"))))
	       (:module info :pathname ""
			:components ((:file "nregex")
                     (:file "intl")
				     (:file "cl-info")))
	       (:module sloop :pathname ""
			:components ((:file "sloop")))
               (:module declarations :pathname ""
                        :components ((:file "lmdcls"))) 
               (:module destructuring-let :pathname ""
                        :components ((:file "letmac")))
               (:module compatibility-macros1 :pathname ""
                        :components ((:file "generr")
				     (:file "clmacs")))
               (:module compatibility-macros :pathname ""
                        :components ((:file "commac"))) 
               (:module prerequisites :pathname ""
                        :components ((:file "mormac") 
                                     (:file "compat")))
	       (:module maxima-language-compiler-macros :pathname ""
			:components ((:file "transm")))
	       (:module getopt :pathname ""
			:components ((:file "getopt")))
	       (:module command-line :pathname ""
			:depends-on (getopt)
			:components ((:file "command-line")))
               (:module fundamental-macros :pathname ""
                        :components ((:file "defcal") 
                                     (:file "maxmac")))
               (:module utility-macros :pathname ""
                        :components ((:file "mopers") 
                                     (:file "mforma")))
               (:module other-macros :pathname ""
                        :components ((:file "mrgmac") 
                                     (:file "rzmac")    
                                     (:file "strmac") 
                                     (:file "displm")))
               (:module rat-macros :pathname ""
                        :components ((:file "ratmac") 
                                     (:file "mhayat")))
	       #+gcl (:file "optimize")		; jfa check this

	       (:module utilities :pathname ""
			:depends-on (utility-macros)
                        :components ((:file "opers")
                                     (:file "utils") 
                                     (:file "sumcon") 
                                     (:file "sublis") 
                                     (:file "merror") 
                                     (:file "mformt") 
                                     (:file "mutils") 
                                     (:file "outmis") 
                                     (:file "ar")))

               (:module commands :pathname ""
                        :components ((:file "comm")
                                     (:file "comm2")))
               (:module evaluator :pathname ""
                        :components ((:file "mlisp") 
                                     (:file "mmacro") 
                                     (:file "buildq")))

	       (:module numerical
			:components
			(
			 (:module packages :pathname ""
				  :components
				  ((:file "f2cl-package")
				   (:file "slatec")))
			 (:module f2cl-lib :pathname ""
                  :depends-on (packages)
				  :components ((:file "f2cl-lib")))
			 (:module slatec 
				  :depends-on (f2cl-lib packages)
				  :components
				  (
				   (:file "fdump")
				   (:file "j4save")
				   (:file "initds"
					  :depends-on ("xermsg"))
				   (:file "xgetua")
				   (:file "xermsg"
					  :depends-on ("fdump" "j4save" "xercnt" "xerhlt" "xerprn" "xersve"))
				   (:file "xercnt")
				   (:file "xerhlt")
				   (:file "xerprn"
					  :depends-on ("xgetua"))
				   (:file "xersve"
					  :depends-on ("j4save"))
				   (:file "dcsevl"
					  :depends-on ("xermsg"))
	     
				   ;; Gamma function
				   (:file "d9lgmc"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "dgamlm"
					  :depends-on ("xermsg"))
				   (:file "dgamma"
					  :depends-on ("d9lgmc" "dcsevl" "dgamlm" "initds" "xermsg"))
				   (:file "dgamln")
				   (:file "dlngam"
					  :depends-on ("d9lgmc" "dgamma" "xermsg"))
	     
				   ;; Bessel J functions
				   (:file "d9b0mp"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "d9b1mp"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "dbesj0"
					  :depends-on ("d9b0mp" "dcsevl" "initds"))
				   (:file "dbesj1"
					  :depends-on ("d9b1mp" "dcsevl" "initds" "xermsg"))
				   (:file "djairy")
				   (:file "dasyjy")
				   (:file "dbesj"
					  :depends-on ("dasyjy" "djairy" "dlngam" "xermsg"))
				   ;; Bessel I functions
				   (:file "dbsi0e"
					  :depends-on ("dcsevl" "initds"))
				   (:file "dbsi1e"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "dbesi0"
					  :depends-on ("dbsi0e" "dcsevl" "initds" "xermsg"))
				   (:file "dbesi1"
					  :depends-on ("dbsi1e" "dcsevl" "initds" "xermsg"))
				   (:file "dasyik")
				   (:file "dbesi"
					  :depends-on ("dasyik" "dlngam" "xermsg"))
				   (:file "zbesi"
					  :depends-on ("zabs" "zbinu"))
	     
				   ;; Bessel J function for complex
				   ;; arg and real order.

				   (:file "zabs")
				   (:file "zacai"
					  :depends-on ("zabs" "zasyi" "zbknu" "zmlri" "zs1s2" "zseri"))
				   (:file "zairy"
					  :depends-on ("zabs" "zacai" "zbknu" "zexp" "zsqrt"))
				   (:file "zasyi"
					  :depends-on ("zabs" "zdiv" "zexp" "zmlt" "zsqrt"))
				   (:file "zbesj"
					  :depends-on ("zabs" "zbinu"))
				   (:file "zbinu"
					  :depends-on ("zabs" "zasyi" "zbuni" "zmlri" "zseri" "zuoik" "zwrsk"))
				   (:file "zbknu"
					  :depends-on ("dgamln" "zabs" "zdiv" "zexp" "zkscl"
								"zlog" "zmlt" "zshch" "zsqrt" "zuchk"))
				   (:file "zbuni"
					  :depends-on ("zabs" "zuni1" "zuni2"))
				   (:file "zdiv")
				   (:file "zexp")
				   (:file "zkscl"
					  :depends-on ("zabs" "zlog" "zuchk"))
				   (:file "zlog"
					  :depends-on ("zabs"))
				   (:file "zmlri"
					  :depends-on ("dgamln" "zabs" "zexp" "zlog" "zmlt"))
				   (:file "zmlt")
				   (:file "zrati"
					  :depends-on ("zabs" "zdiv"))
				   (:file "zs1s2"
					  :depends-on ("zabs" "zexp" "zlog"))
				   (:file "zseri"
					  :depends-on ("dgamln" "zabs" "zdiv" "zlog" "zmlt" "zuchk"))
				   (:file "zshch")
				   (:file "zsqrt" :depends-on ("zabs"))
				   (:file "zuchk")
				   (:file "zunhj"
					  :depends-on ("zabs" "zdiv" "zlog" "zsqrt"))
				   (:file "zuni1"
					  :depends-on ("zabs" "zuchk" "zunik" "zuoik"))
				   (:file "zuni2"
					  :depends-on ("zabs" "zairy" "zuchk" "zunhj" "zuoik"))
				   (:file "zunik"
					  :depends-on ("zdiv" "zlog" "zsqrt"))
				   (:file "zuoik"
					  :depends-on ("zabs" "zlog" "zuchk" "zunhj" "zunik"))
				   (:file "zwrsk"
					  :depends-on ("zabs" "zbknu" "zrati"))
	     
				   ;; Bessel Y functions
				   (:file "dbesy0"
					  :depends-on ("d9b0mp" "dbesj0" "dcsevl" "initds" "xermsg"))
				   (:file "dbesy1"
					  :depends-on ("d9b1mp" "dbesj1" "dcsevl" "initds" "xermsg"))
				   (:file "dbesy"
					  :depends-on ("dasyjy" "dbesy0" "dbesy1" "dbsynu" "dyairy" "xermsg"))
				   (:file "dbsynu"
					  :depends-on ("dgamma" "xermsg"))
				   (:file "dyairy")
	     
				   (:file "zbesy"
					  :depends-on ("zbesh"))
				   (:file "zbesh"
					  :depends-on ("zabs" "zacon" "zbknu" "zbunk" "zuoik"))
				   (:file "zacon"
					  :depends-on ("zabs" "zbinu" "zbknu" "zmlt" "zs1s2"))
				   (:file "zbunk"
					  :depends-on ("zunk1" "zunk2"))
				   (:file "zunk1"
					  :depends-on ("zabs" "zs1s2" "zuchk" "zunik"))
				   (:file "zunk2"
					  :depends-on ("zabs" "zairy" "zs1s2" "zuchk" "zunhj"))

				   ;; Bessel K functions
				   (:file "dbesk0"
					  :depends-on ("dbesi0" "dbsk0e" "dcsevl" "initds" "xermsg"))
				   (:file "dbsk0e"
					  :depends-on ("dbesi0" "dcsevl" "initds" "xermsg"))
				   (:file "dbesk1"
					  :depends-on ("dbesi1" "dbsk1e" "dcsevl" "initds" "xermsg"))
				   (:file "dbsk1e"
					  :depends-on ("dbesi1" "dcsevl" "initds" "xermsg"))
				   (:file "dbesk"
					  :depends-on ("dasyik" "dbesk0" "dbesk1" "dbsk0e" "dbsk1e" "dbsknu" "xermsg"))
				   (:file "dbsknu"
					  :depends-on ("dgamma" "xermsg"))
				   (:file "zbesk"
					  :depends-on ("zabs" "zacon" "zbknu" "zbunk" "zuoik"))
				   
				   ;; Airy functions
				   (:file "d9aimp"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "daie"
					  :depends-on ("d9aimp" "dcsevl" "initds"))
				   (:file "dai"
					  :depends-on ("d9aimp" "daie" "dcsevl" "initds" "xermsg"))
                                  (:file "dbie"
                                         :depends-on ("d9aimp" "dcsevl" "initds"))
                                  (:file "dbi"
                                         :depends-on ("d9aimp" "dbie" "dcsevl" "initds" "xermsg"))
                                  (:file "zbiry"
                                         :depends-on ("zabs" "zbinu" "zdiv" "zsqrt"))
				   ;; Error functions
				   (:file "derf"
					  :depends-on ("dcsevl" "derfc" "initds"))
				   (:file "derfc"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   ;; Exponential integrals
				   (:file "de1"
					  :depends-on ("dcsevl" "initds" "xermsg"))
				   (:file "dei"
					  :depends-on ("de1"))
				   (:file "dspenc"
					  :depends-on ("d9upak"))
				   (:file "d9upak")))
			 (:module quadpack
				  :pathname "slatec/"
				  :components
				  (
				   ;; Support
				   (:file "dqwgtf")
				   (:file "dqcheb")
				   (:file "dqk15w")
				   (:file "dqwgts")
				   (:file "dqwgtc")
				   (:file "dgtsl")
				   ;; Core integration routines
				   (:file "dqk15")
				   (:file "dqk31")
				   (:file "dqk41")
				   (:file "dqk51")
				   (:file "dqk61")
				   (:file "dqk21")
				   (:file "dqk15i")
				   (:file "dqelg")
				   (:file "dqpsrt")
				   (:file "dqc25s"
					  :depends-on ("dqcheb" "dqk15w"))
				   (:file "dqmomo")
				   (:file "dqc25c"
					  :depends-on ("dqcheb"
						       "dqk15w"))
				   (:file "dqc25f"
					  :depends-on ("dgtsl"
						       "dqcheb"
						       "dqk15w"
						       "dqwgtf"))
				   ;; Basic integrators
				   (:file "dqage"
					  :depends-on ("dqk15"
						       "dqk31"
						       "dqk41"
						       "dqk51"
						       "dqk61"
						       "dqk21"
						       "dqpsrt"))
				   (:file "dqagie"
					  :depends-on ("dqelg"
						       "dqk15i"
						       "dqpsrt"))
				   (:file "dqagpe"
					  :depends-on ("dqelg"
						       "dqpsrt"
						       "dqk21"
						       ))
				   (:file "dqagse"
					  :depends-on ("dqk21"
						       "dqelg"
						       "dqpsrt"))
				   (:file "dqawfe"
					  :depends-on ("dqagie"
						       "dqawoe"
						       "dqelg"))
				   (:file "dqawoe"
					  :depends-on ("dqc25f"
						       "dqpsrt"
						       "dqelg"))
				   (:file "dqawse"
					  :depends-on ("dqc25s"
						       "dqmomo"
						       "dqpsrt"))
				   (:file "dqawce"
					  :depends-on ("dqc25c"
						       "dqpsrt"))
				   ;; Simplified interface routines
				   (:file "dqng")
				   (:file "dqag"
					  :depends-on ("dqage"))
				   (:file "dqags"
					  :depends-on ("dqagse"))
				   (:file "dqagi"
					  :depends-on ("dqagie"))
				   (:file "dqawf"
					  :depends-on ("dqawfe"))
				   (:file "dqawo"
					  :depends-on ("dqawoe"))
				   (:file "dqaws"
					  :depends-on ("dqawse"))
				   (:file "dqawc"
					  :depends-on ("dqawce"))
				   ;; Maxima interface
				   (:file "quadpack")
				   )
			 )))
               (:module simplification :pathname ""
                        :components ((:file "simp") 
                                     (:file "float") 
                                     (:file "csimp") 
                                     (:file "csimp2") 
                                     (:file "zero")
                                     (:file "logarc") 
                                     (:file "rpart")))
	       (:module numeric-bigfloat :pathname ""
			:components ((:file "numeric")))
	       (:module server :pathname ""
			:components ((:file "server")))
               (:module i-o :pathname ""
			:depends-on (compatibility-macros)
                        :components ((:file "macsys") 
                                     (:file "mload") 
                                     (:file "suprv1")
                                     (:file "dskfn")))
               (:module factoring :pathname ""
                        :components ((:file "lesfac") 
                                     (:file "factor") 
                                     (:file "algfac") 
                                     (:file "nalgfa") 
                                     (:file "ufact") 
                                     (:file "result")))
	       (:module ifactor :pathname ""
			:components ((:file "ifactor")))

	       (:module rational-functions :pathname ""
			:components ((:file "rat3a") 
				     (:file "rat3b") 
				     (:file "rat3d") 
				     (:file "numth") 
				     (:file "rat3c") 
				     (:file "rat3e") 
				     (:file "nrat4") 
				     (:file "ratout")))

	       (:module maxima-language-compiler :pathname ""
			:components ((:file "transl") 
				     (:file "transs") 
				     (:file "trans1") 
				     (:file "trans2") 
				     (:file "trans3") 
				     (:file "trans4") 
				     (:file "trans5") 
				     (:file "transf") 
				     (:file "troper") 
				     (:file "trutil") 
				     (:file "trmode") 
				     (:file "trdata") 
				     (:file "trpred") 
				     (:file "transq") 
				     (:file "acall")
				     (:file "fcall") 
				     (:file "evalw") 
				     (:file "trprop") 
				     (:file "mdefun"))
			:depends-on (maxima-language-compiler-macros))

	       (:module trigonometry :pathname ""
			:components ((:file "trigi") 
				     (:file "trigo") 
				     (:file "trgred")))

	       (:module numerical-functions :pathname ""
			:depends-on (trigonometry)
			:components ((:file "bessel")
				     (:file "ellipt")
				     (:file "airy"
					    :depends-on ("ellipt"))
				     (:file "plasma")
				     (:file "intpol")))

	       (:module reader :pathname ""
			:depends-on (compatibility-macros)
			:components ((:file "nparse")))

	       (:module display :pathname ""
			:components ((:file "displa") 
				     (:file "nforma") 
				     (:file "grind")))

	       (:module gcd :pathname ""
			:components ((:file "spgcd")
				     (:file "ezgcd")))
	       (:module documentation :pathname ""
			:components ((:file "option")
				     (:file "macdes")))
	       (:module algebraic-database :pathname ""
			:components ((:file "inmis") 
				     (:file "db") 
				     (:file "compar") 
				     (:file "askp"))) ;does this belong here?
	       (:module integration :pathname ""
			:components ((:file "sinint") 
				     (:file "sin") 
				     (:file "risch")))
	       (:module taylor-series :pathname ""
			:depends-on (rat-macros)
			:components ((:file "hayat")))
	       (:module definite-integration :pathname ""
			:components ((:file "defint") 
				     (:file "residu")))
	       (:module special-functions :pathname ""
			:components ((:file "specfn")))
	       (:module matrix-algebra :pathname ""
			:components ((:file "mat") 
                     (:file "linnew")
				     (:file "matrix")))
	       (:module determinants :pathname ""
			:components ((:file "sprdet") 
				     (:file "newinv") 
				     (:file "newdet")))
	       (:module pattern-matching :pathname ""
			:components ((:file "schatc") 
				     (:file "matcom") 
				     (:file "matrun") 
				     (:file "nisimp")))
	       (:module limits :pathname ""
			:components ((:file "tlimit") 
				     (:file "limit")))
	       (:module solve :pathname ""
			:components ((:file "solve") 
				     (:file "psolve") 
				     (:file "algsys") 
				     (:file "polyrz") 
				     (:file "cpoly")))
	       (:module debugging :pathname ""
			:components ((:file "mtrace")
				     (:file "mdebug")))
	       (:module miscellaneous :pathname ""
			:components ((:file "scs") 
				     (:file "asum") 
				     (:file "fortra") 
				     (:file "optim") 
				     (:file "marray") 
				     (:file "mdot") 
				     (:file "irinte") 
				     (:file "series") 
				     (:file "laplac") 
				     (:file "pade") 
				     (:file "homog") 
				     (:file "combin") 
				     (:file "nset")
				     (:file "rand-mt19937")
				     (:file "maxmin")
				     (:file "nummod")
				     (:file "conjugate")
                                     (:file "expintegral")
                                     (:file "gamma")
				     (:file "mstuff")))
	       (:module poisson-series :pathname ""
			:components ((:file "pois2") 
				     (:file "pois3")))
	       (:module translated-packages :pathname ""
			:depends-on ("maxima-language-compiler-macros")
			:components
			((:file "desoln")
			 (:file "elim")
			 (:file "invert")
			 (:file "hypgeo")
			 (:file "hyp")
			 (:file "todd-coxeter")
			 (:file "mactex")
			 (:file "plot")))
	       (:module graphics-drivers :pathname ""
			:components ((:file "gnuplot_def")
				     (:file "xmaxima_def")))

	       (:module final :pathname ""
			;; These are not compiled, for whatever reason
			:components ((:file "autol")
				     (:file "max_ext")
				     (:file "share-subdirs")
				     (:file "init-cl"))))
  :serial t
  :depends-on ())
  
  
