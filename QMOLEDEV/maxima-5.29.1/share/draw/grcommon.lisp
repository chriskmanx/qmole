;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2007-2012 Mario Rodriguez Riotorto
;;;  
;;;  This program is free software; you can redistribute
;;;  it and/or modify it under the terms of the
;;;  GNU General Public License as published by
;;;  the Free Software Foundation; either version 2 
;;;  of the License, or (at your option) any later version. 
;;;  
;;;  This program is distributed in the hope that it
;;;  will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY
;;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;;  GNU General Public License for more details at
;;;  http://www.gnu.org/copyleft/gpl.html

;;; This file is the options handler of package draw

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es


($put '$grcommon 1 '$version)

;; Possible draw renderers:
;;      gnuplot_pipes (default)
;;      gnuplot
;;      vtk
(defvar $draw_renderer '$gnuplot_pipes)

(defvar $draw_use_pngcairo nil "If true, use pngcairo terminal when png is requested.")

;; Stores actual graphics options
(defvar *gr-options* (make-hash-table))

;; Stores user defaults
(defvar *user-gr-default-options* nil)

;; Stores user defined allocations of scenes
(defvar *allocations* nil)

(defun $set_draw_defaults (&rest opts)
   (setf *user-gr-default-options* opts)
   (cons '(mlist) opts))

;; Sets user default values of graphics options
(defun user-defaults ()
   (dolist (x *user-gr-default-options*)
      (if (equal ($op x) "=")
         (update-gr-option ($lhs x) ($rhs x))
         (merror "draw: item ~M is not recognized as an option assignment" x))))


;; Sets default values of graphics options
(defun ini-gr-options ()
  (setf
      ; global options to control general aspects of graphics
      (gethash '$allocation *gr-options*)       nil      ; or user-defined allocation
      (gethash '$proportional_axes *gr-options*) '$none  ; three possible options: none, xy, xyz
      (gethash '$xrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$xrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$yrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$yrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$zrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$cbrange *gr-options*)          nil      ; nil => automatic computation
      (gethash '$logx *gr-options*)             nil
      (gethash '$logy *gr-options*)             nil
      (gethash '$logz *gr-options*)             nil
      (gethash '$logcb *gr-options*)            nil
      (gethash '$title *gr-options*)            ""
      (gethash '$view *gr-options*)             '(60 30) ; in [0,180] x [0,360]
      (gethash '$xy_file *gr-options*)          ""
      (gethash '$user_preamble *gr-options*)    ""
      (gethash '$xyplane *gr-options*)          nil
      (gethash '$font *gr-options*)             "";
      (gethash '$font_size *gr-options*)        10;

      ; colors are specified by name
      (gethash '$background_color *gr-options*) "#ffffff"
      (gethash '$color *gr-options*)            "#0000ff" ; for lines, points, borders and labels
      (gethash '$fill_color *gr-options*)       "#ff0000" ; for filled regions
      (gethash '$fill_density *gr-options*)     0         ; in [0,1], only for object 'bars


      ; implicit plot options
      (gethash '$ip_grid *gr-options*)    '((mlist simp) 50 50)
      (gethash '$ip_grid_in *gr-options*) '((mlist simp) 5 5)
      (gethash '$x_voxel *gr-options*)       10
      (gethash '$y_voxel *gr-options*)       10
      (gethash '$z_voxel *gr-options*)       10

      ; tics
      (gethash '$grid *gr-options*)            nil
      (gethash '$xtics *gr-options*)           "autofreq"
      (gethash '$xtics_secondary *gr-options*) nil   ; no tics in top x-axis
      (gethash '$ytics *gr-options*)           "autofreq"
      (gethash '$ytics_secondary *gr-options*) nil   ; no tics in right y-axis
      (gethash '$ztics *gr-options*)           "autofreq"
      (gethash '$cbtics *gr-options*)          "autofreq"
      (gethash '$xtics_rotate *gr-options*)    nil
      (gethash '$xtics_secondary_rotate *gr-options*) nil
      (gethash '$ytics_rotate *gr-options*)    nil
      (gethash '$ytics_secondary_rotate *gr-options*) nil
      (gethash '$ztics_rotate *gr-options*)    nil
      (gethash '$xtics_axis *gr-options*)      nil
      (gethash '$xtics_secondary_axis *gr-options*)   nil
      (gethash '$ytics_axis *gr-options*)      nil
      (gethash '$ytics_secondary_axis *gr-options*)   nil
      (gethash '$ztics_axis *gr-options*)      nil

      ; axis
      (gethash '$axis_bottom *gr-options*) t
      (gethash '$axis_left *gr-options*)   t
      (gethash '$axis_top *gr-options*)    t
      (gethash '$axis_right *gr-options*)  t
      (gethash '$axis_3d *gr-options*)     t
      (gethash '$xaxis *gr-options*)       nil   ; no xaxis by default
      (gethash '$xaxis_width *gr-options*) 1
      (gethash '$xaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$xaxis_color *gr-options*) "#000000"
      (gethash '$yaxis *gr-options*)       nil  ; no yaxis by default
      (gethash '$yaxis_width *gr-options*) 1
      (gethash '$yaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$yaxis_color *gr-options*) "#000000"
      (gethash '$zaxis *gr-options*)       nil  ; no zaxis by default
      (gethash '$zaxis_width *gr-options*) 1
      (gethash '$zaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$zaxis_color *gr-options*) "#000000"
      (gethash '$xlabel *gr-options*) ""
      (gethash '$ylabel *gr-options*) ""
      (gethash '$zlabel *gr-options*) ""

      ; point options
      (gethash '$point_size *gr-options*)    1
      (gethash '$point_type *gr-options*)    1
      (gethash '$points_joined *gr-options*) nil ; other options are: true and $impulses

      ; error bars option
      (gethash '$error_type *gr-options*)   '$y

      ; polygon  options
      (gethash '$transparent *gr-options*) nil
      (gethash '$opacity *gr-options*)     1
      (gethash '$border *gr-options*)      t

      ; vector  options
      (gethash '$head_both *gr-options*)   nil
      (gethash '$head_length *gr-options*) 2        ; in x-axis units
      (gethash '$head_angle *gr-options*)  45       ; with respect to the segment
      (gethash '$head_type *gr-options*)   '$filled ; other options are: $empty and $nofilled
      (gethash '$unit_vectors *gr-options*) nil

      ; label options
      (gethash '$label_alignment *gr-options*)   '$center     ; other options are: $left and $right
      (gethash '$label_orientation *gr-options*) '$horizontal ; the other option is $vertical

      ; line options
      (gethash '$line_width *gr-options*) 1
      (gethash '$line_type *gr-options*)  1    ; two options: 1 (solid) and 0 (dots)

      ; function options
      (gethash '$nticks *gr-options*)          29
      (gethash '$adapt_depth *gr-options*)     10
      (gethash '$key *gr-options*)             ""          ; by default, no keys
      (gethash '$filled_func *gr-options*)     nil         ; false, true (y axis) or an expression
      (gethash '$xaxis_secondary *gr-options*) nil
      (gethash '$yaxis_secondary *gr-options*) nil
      (gethash '$draw_realpart *gr-options*)   t

      ; transformation option
      (gethash '$transform *gr-options*) '$none

      ; 3d options
      (gethash '$xu_grid *gr-options*)        30
      (gethash '$yv_grid *gr-options*)        30
      (gethash '$surface_hide *gr-options*)   nil
      (gethash '$enhanced3d *gr-options*)     '$none
      (gethash '$wired_surface *gr-options*)  nil
      (gethash '$contour *gr-options*)        '$none  ; other options are: $base, $surface, $both and $map
      (gethash '$contour_levels *gr-options*) 5       ; 1-50, [lowest_level,step,highest_level] or {z1,z2,...}
      (gethash '$colorbox *gr-options*)       t       ; in pm3d mode, always show colorbox
      (gethash '$palette  *gr-options*)       '$color ; '$color is a short cut for [7,5,15]
                                                      ; and '$gray is a short cut for [3,3,3].
                                                      ; See command 'show palette rgbformulae' in gnuplot.
      (gethash '$tube_extremes *gr-options*) '((mlist simp) '$open '$open) ; or '$closed
  ) )




;; Returns option value
(defun get-option (opt) (gethash opt *gr-options*))





;; update options color, fill_color, xaxis_color, yaxis_color,
;; $zaxis_color, and $background_color
;; -----------------------------------------------------------
;; defined as a color name or hexadecimal #rrggbb
(defun atom-to-downcased-string (val)
   (substitute
      #\- #\_ 
      (string-downcase
        (string-trim 
           "\""
           (coerce (mstring val) 'string)))))

(defvar *color-table* (make-hash-table :test 'equal))
(setf (gethash "white" *color-table*) "#ffffff"
      (gethash "black" *color-table*) "#000000"
      (gethash "gray0" *color-table*) "#000000"
      (gethash "grey0" *color-table*) "#000000"
      (gethash "gray10" *color-table*) "#1a1a1a"
      (gethash "grey10" *color-table*) "#1a1a1a"
      (gethash "gray20" *color-table*) "#333333"
      (gethash "grey20" *color-table*) "#333333"
      (gethash "gray30" *color-table*) "#4d4d4d"
      (gethash "grey30" *color-table*) "#4d4d4d"
      (gethash "gray40" *color-table*) "#666666"
      (gethash "grey40" *color-table*) "#666666"
      (gethash "gray50" *color-table*) "#7f7f7f"
      (gethash "grey50" *color-table*) "#7f7f7f"
      (gethash "gray60" *color-table*) "#999999"
      (gethash "grey60" *color-table*) "#999999"
      (gethash "gray70" *color-table*) "#b3b3b3"
      (gethash "grey70" *color-table*) "#b3b3b3"
      (gethash "gray80" *color-table*) "#cccccc"
      (gethash "grey80" *color-table*) "#cccccc"
      (gethash "gray90" *color-table*) "#e5e5e5"
      (gethash "grey90" *color-table*) "#e5e5e5"
      (gethash "gray100" *color-table*) "#ffffff"
      (gethash "grey100" *color-table*) "#ffffff"
      (gethash "gray" *color-table*) "#bebebe"
      (gethash "grey" *color-table*) "#c0c0c0"
      (gethash "light-gray" *color-table*) "#d3d3d3"
      (gethash "light-grey" *color-table*) "#d3d3d3"
      (gethash "dark-gray" *color-table*) "#a0a0a0"
      (gethash "dark-grey" *color-table*) "#a0a0a0"
      (gethash "red" *color-table*) "#ff0000"
      (gethash "light-red" *color-table*) "#f03232"
      (gethash "dark-red" *color-table*) "#8b0000"
      (gethash "yellow" *color-table*) "#ffff00"
      (gethash "dark-yellow" *color-table*) "#c8c800"
      (gethash "green" *color-table*) "#00ff00"
      (gethash "light-green" *color-table*) "#90ee90"
      (gethash "dark-green" *color-table*) "#006400"
      (gethash "spring-green" *color-table*) "#00ff7f"
      (gethash "forest-green" *color-table*) "#228b22"
      (gethash "sea-green" *color-table*) "#2e8b57"
      (gethash "blue" *color-table*) "#0000ff"
      (gethash "light-blue" *color-table*) "#add8e6"
      (gethash "dark-blue" *color-table*) "#00008b"
      (gethash "midnight-blue" *color-table*) "#191970"
      (gethash "navy" *color-table*) "#000080"
      (gethash "medium-blue" *color-table*) "#0000cd"
      (gethash "royalblue" *color-table*) "#4169e1"
      (gethash "skyblue" *color-table*) "#87ceeb"
      (gethash "cyan" *color-table*) "#00ffff"
      (gethash "light-cyan" *color-table*) "#e0ffff"
      (gethash "dark-cyan" *color-table*) "#00eeee"
      (gethash "magenta" *color-table*) "#ff00ff"
      (gethash "light-magenta" *color-table*) "#f055f0"
      (gethash "dark-magenta" *color-table*) "#c000ff"
      (gethash "turquoise" *color-table*) "#40e0d0"
      (gethash "light-turquoise" *color-table*) "#afeeee"
      (gethash "dark-turquoise" *color-table*) "#00ced1"
      (gethash "pink" *color-table*) "#ffc0c0"
      (gethash "light-pink" *color-table*) "#ffb6c1"
      (gethash "dark-pink" *color-table*) "#ff1493"
      (gethash "coral" *color-table*) "#ff7f50"
      (gethash "light-coral" *color-table*) "#f08080"
      (gethash "orange-red" *color-table*) "#ff4500"
      (gethash "salmon" *color-table*) "#fa8072"
      (gethash "light-salmon" *color-table*) "#ffa070"
      (gethash "dark-salmon" *color-table*) "#e9967a"
      (gethash "aquamarine" *color-table*) "#7fffd4"
      (gethash "khaki" *color-table*) "#f0e68c"
      (gethash "dark-khaki" *color-table*) "#bdb76b"
      (gethash "goldenrod" *color-table*) "#ffc020"
      (gethash "light-goldenrod" *color-table*) "#eedd82"
      (gethash "dark-goldenrod" *color-table*) "#b8860b"
      (gethash "gold" *color-table*) "#ffd700"
      (gethash "beige" *color-table*) "#f5f5dc"
      (gethash "brown" *color-table*) "#a52a2a"
      (gethash "orange" *color-table*) "#ffa500"
      (gethash "dark-orange" *color-table*) "#c04000"
      (gethash "violet" *color-table*) "#ee82ee"
      (gethash "dark-violet" *color-table*) "#9400d3"
      (gethash "plum" *color-table*) "#dda0dd"
      (gethash "purple" *color-table*) "#c080ff")

(defun correct-color-name (str)
  (some #'(lambda (z) (string= z str))
        (loop for k being the hash-keys of *color-table* collect k)))

(defun correct-color-hex (str)
  (and (= (length str) 7)
       (char= (schar str 0) #\#)
       (every #'(lambda (z) (position z "0123456789abcdef"))
              (subseq str 1))))

(defun correct-color (str)
  (or (correct-color-name str)
      (correct-color-hex str)
      (string= str "auto")))

(defun hex-to-numeric-list (str)
  (let ((hex1 (subseq str 1 3))
        (hex2 (subseq str 3 5))
        (hex3 (subseq str 5)))
    (list
      (/ (parse-integer hex1 :radix 16) 255.0)
      (/ (parse-integer hex2 :radix 16) 255.0)
      (/ (parse-integer hex3 :radix 16) 255.0))))

(defun hex-to-rgb (str)
  (format nil "rgb '~a'" str))

(defun update-color (opt val)
  (let ((str (atom-to-downcased-string val)))
    (unless (correct-color str)
      (merror "model3d: unknown color ~M" val))
    (when (correct-color-name str)
      (setf str (gethash str *color-table*)))
    (setf (gethash opt *gr-options*) str)))



;; update option terminal
;; ----------------------
;; *draw-terminal-number* is used when working with
;; multiple windows. Empty string means
;; we are working with only one window
(defvar *draw-terminal-number* "")

(defun update-terminal (val)
  (let ((terms '($screen $png $pngcairo $jpg $gif $eps $eps_color 
                 $epslatex $epslatex_standalone $svg $x11
                 $dumb $dumb_file $pdf $pdfcairo $wxt $animated_gif
                 $multipage_pdfcairo $multipage_pdf $multipage_eps 
                 $multipage_eps_color $aquaterm $tiff $vrml $obj $pnm)))
     (cond
       ((member val terms)
          (when (and (eq val '$png) $draw_use_pngcairo)
            (setq val '$pngcairo))
          (setf (gethash '$terminal *gr-options*) val
                *draw-terminal-number* ""))
       ((and ($listp val)
             (= ($length val) 2)
             (member (cadr val) '($screen $wxt $aquaterm))
             (integerp (caddr val))
             (>= (caddr val) 0))
          (setf (gethash '$terminal *gr-options*) (cadr val)
                *draw-terminal-number* (caddr val)))
       (t
          (merror "draw: illegal terminal specification: ~M" val)))))



;; update option transform
;; -----------------------
(defvar *draw-transform-dimensions* 0)
(defvar *draw-transform-f1* nil)
(defvar *draw-transform-f2* nil)
(defvar *draw-transform-f3* nil)

(defmacro transform-point (n)
  (if (= n 2)
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold xx)
              (yold yy))
          (setf xx (funcall *draw-transform-f1* xold yold)
                yy (funcall *draw-transform-f2* xold yold))))
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold xx)
              (yold yy)
              (zold zz))
          (setf xx (funcall *draw-transform-f1* xold yold zold)
                yy (funcall *draw-transform-f2* xold yold zold)
                zz (funcall *draw-transform-f3* xold yold zold)  )))))

(defmacro transform-lists (n)
  (if (= n 2)
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold x)
              (yold y))
          (setf x (mapcar #'(lambda (u1 u2) (funcall *draw-transform-f1* u1 u2))
                          xold yold)
                y (mapcar #'(lambda (u1 u2) (funcall *draw-transform-f2* u1 u2))
                          xold yold)) ))
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold x)
              (yold y)
              (zold z))
          (setf x (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f1* u1 u2 u3))
                          xold yold zold)
                y (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f2* u1 u2 u3))
                          xold yold zold)
                z (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f3* u1 u2 u3))
                          xold yold zold))) )))

(defun update-transform (val)
  (let (vars)
    (cond
      ((equal val '$none)
        (setf (gethash '$transform *gr-options*) '$none
              *draw-transform-dimensions* 0
              *draw-transform-f1* nil
              *draw-transform-f2* nil
              *draw-transform-f3* nil))
      ((and ($listp val)
            (= ($length val) 4)
            ($subsetp ($union ($setify ($listofvars ($first val)))
                              ($setify ($listofvars ($second val))))
                      ($setify (list '(mlist) ($third val) ($fourth val)))) )
         ; transformation in 2d
         (setf vars (list '(mlist) ($third val) ($fourth val)))
         (setf (gethash '$transform *gr-options*) val
               *draw-transform-dimensions* 2
               *draw-transform-f1* (coerce-float-fun ($first val) vars)
               *draw-transform-f2* (coerce-float-fun ($second val) vars) ))
      ((and ($listp val)
            (= ($length val) 6)
            ($subsetp ($union ($setify ($listofvars ($first val)))
                              ($setify ($listofvars ($second val)))
                              ($setify ($listofvars ($third val))))
                      ($setify (list '(mlist) ($fourth val) ($fifth val) ($sixth val)))) )
         ; transformation in 3d
         (setf vars (list '(mlist) ($fourth val) ($fifth val) ($sixth val)))
         (setf (gethash '$transform *gr-options*) val
               *draw-transform-dimensions* 3
               *draw-transform-f1* (coerce-float-fun ($first val) vars)
               *draw-transform-f2* (coerce-float-fun ($second val) vars)
               *draw-transform-f3* (coerce-float-fun ($third val) vars)))
      (t
         (merror "draw: illegal transform definition")) )))



;; update option enhanced3d
;; ------------------------
(defvar *draw-enhanced3d-type* 0)
(defvar *draw-enhanced3d-fun* nil)

(defun check-enhanced3d-model (grobj lis)
  (when (null (position *draw-enhanced3d-type* lis))
    (merror (format nil "draw (~a): unacceptable enhanced3d model" grobj))))

(defun update-enhanced3d-expression (vars)
  (let ((texture (gethash '$enhanced3d *gr-options*)))
    (when (not ($subsetp ($setify ($listofvars texture))
                         ($setify vars)))
      (merror "draw: incompatible variables in enhanced3d expression"))
    (setf *draw-enhanced3d-fun* (coerce-float-fun texture vars))))

(defun update-enhanced3d (val)
  (cond
    ((or (null val)
         (equal val '$none))
      (setf (gethash '$enhanced3d *gr-options*) '$none
            *draw-enhanced3d-type* 0
            *draw-enhanced3d-fun* nil))
    ((equal val t)
      (let ((model '((mlist) $z $x $y $z)))
        (setf (gethash '$enhanced3d *gr-options*) model
              *draw-enhanced3d-type* 3
              *draw-enhanced3d-fun* (coerce-float-fun
                              ($first model)
                              (list '(mlist) ($second model) ($third model) ($fourth model))))))
    ((and ($listp val)
          ($subsetp ($setify ($listofvars ($first val)))
                    ($setify ($rest val))))
       (case ($length val)
         (2 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 1
                  *draw-enhanced3d-fun* (coerce-float-fun
                              ($first val)
                              (list '(mlist) ($second val)))))
         (3 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 2
                  *draw-enhanced3d-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val)))))
         (4 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 3
                  *draw-enhanced3d-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val) ($fourth val)))))
         (otherwise (merror "draw: illegal length of enhanced3d"))))
    ((not ($listp val))
       ; enhanced3d is given an expression without 
       ; explicit declaration of its variables.
       ; Each graphic object must check for them.
       (setf (gethash '$enhanced3d *gr-options*) val
             *draw-enhanced3d-type* 99
             *draw-enhanced3d-fun* nil))
    (t
        (merror "draw: illegal enhanced3d definition")) ) )



;; update boolean type options
;; ---------------------------
(defun update-boolean-option (opt val)
  (if (or (equal val t)
          (null val))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: non boolean value: ~M " val)))



;; update positive integer type options
;; ------------------------------------
(defun update-positive-integer (opt val)
  (if (and (integerp val)
           (> val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: non positive integer: ~M " val)))



;; update positive float type options
;; ----------------------------------
(defun update-positive-float (opt val)
  (setf val ($float val))
  (if (and (numberp val)
           (> val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: Non positive number: ~M " val)))



;; update non negative float type options
;; --------------------------------------
(defun update-nonnegative-float (opt val)
  (setf val ($float val))
  (if (and (numberp val)
           (>= val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: Not a non-negative number: ~M " val)))



;; update option: view
;; -------------------
(defun update-view (val)
  (cond
    ((and ($listp val)
          (= ($length val) 2))
       (let ((rv ($float (cadr val)))
             (rh ($float (caddr val))) )
         (unless
           (and (numberp rv) (>= rv 0) (<= rv 360) )
           (merror "draw: vertical rotation angle must be in [0, 360]"))
         (unless
           (and (numberp rh) (>= rh 0) (<= rh 360) )
           (merror "draw: horizontal rotation angle must be in [0, 360]"))
         (setf (gethash '$view *gr-options*)
               (list rv rh))))
    (t
      (merror "draw: illegal view specification ~M" val)) ))



;; update option allocation
;; ------------------------
(defun update-allocation (val)
  (let (fval cls)
    (cond
      ((and ($listp val)
            (= ($length val) 2)
            (every #'(lambda (z) (and ($listp z)
                                      (= ($length z) 2)))
                   (rest val))
         (setf fval (rest ($float val)))
         (setf cls (list (cadar fval) (caddar fval) (cadadr fval) (caddr (cadr fval))))
         (if (every #'(lambda (z) (and (float z) (>= z 0.0) (<= z 1.0))) cls)
           (setf (gethash '$allocation *gr-options*) cls)
           (merror "draw: allocations must me given in relative values")) ))
      (t
         (merror "draw: illegal allocation format ~M" val)))))



;; update option dimensions
;; ------------------------
(defun update-dimensions (val)
  (let (cls)
    (cond
      ((and ($listp val)
            (= ($length val) 2))
         (setf cls (rest val))
         (if (every #'(lambda (z) (and (numberp z) (> z 0))) cls)
             (setf (gethash '$dimensions *gr-options*) cls)
             (merror "draw: illegal dimensions")))
      (t
         (merror "draw: illegal dimensions format ~M" val)))))



;; update option point_type
;; ------------------------
(defun update-pointtype (val)
  (cond
    ((and (integerp val) (>= val -1 ))
       (setf (gethash '$point_type *gr-options*) val))
    (t (let ((shapes '($none $dot $plus $multiply $asterisk
                       $square $filled_square $circle $filled_circle
                       $up_triangle $filled_up_triangle $down_triangle
                       $filled_down_triangle $diamant $filled_diamant
                       $sphere $cube $cylinder $cone)))
          (if (member val shapes)
              (setf (gethash '$point_type *gr-options*) (- (position val shapes) 1))
              (merror "draw: illegal point type: ~M " val)))))  )



;; update string type options
;; --------------------------
(defun update-string (opt val)
  (setf (gethash opt *gr-options*) val))



;; update opacity option
;; ---------------------
(defun update-opacity (val)
  (let ((value ($float val)))
    (when (or (not (floatp value))
              (> value 1)
              (< value 0))
      (merror "draw: illegal opacity value: ~M " val))
    (setf (gethash '$opacity *gr-options*) value)))



;; update palette option
;; ---------------------

(defun update-palette (val)
  ; defined as $color, $gray, [f1,f2,f3], with -36<=fi<=36,
  ; or a list of triplets defining a user palette.
  (cond ((member val '($color $gray))
          (setf (gethash '$palette *gr-options*) val))

        ((and ($listp val)
              (= ($length val) 3)
              (every #'(lambda (x) (and (integerp x) (<= (abs x) 36)))
                     (cdr val)) )
          (setf (gethash '$palette *gr-options*) (rest val)))

        ((and ($listp val)  ; user defined palette without transparency
              (not ($listp (cadr val)) ))
           (let* ((palette (cdr val))
                  (n (length palette))
                  str color)
             (setf (gethash '$palette *gr-options*)
                   (loop for k below n
                      do (setf str (atom-to-downcased-string (nth k palette)))
                         (cond ((correct-color-hex str)
                                  (setf color (hex-to-numeric-list str)))
                               ((correct-color-name str)
                                  (setf color (hex-to-numeric-list (gethash str *color-table*))))
                               (t
                                  (merror "draw: illegal color in palette description")))
                      collect color))))

        ((and ($listp val)  ; user defined palette with transparency
              (every #'(lambda (x) (and ($listp x) (= (length x) 3)))
                     (cdr val)))
           (let* ((palette (cdr val))
                  (n (length palette))
                  str color transparency)
             (setf (gethash '$palette *gr-options*)
                   (loop for k below n
                      do (setf str (atom-to-downcased-string ($first (nth k palette))))
                         (cond ((correct-color-hex str)
                                  (setf color (hex-to-numeric-list str)))
                               ((correct-color-name str)
                                  (setf color (hex-to-numeric-list (gethash str *color-table*))))
                               (t
                                  (merror "draw: illegal color in palette description")))
                         (setf transparency ($float ($second (nth k palette))))
                         (when (or (< transparency 0)
                                   (> transparency 1)
                                   (not (floatp transparency)))
                           (merror "draw: illegal transparency in palette description"))
                      collect (append color (list transparency))))))

        (t
          (merror "draw: illegal palette description: ~M" val))) )



;; update line_type and line-axes
;; Negative indices indicate the number of faces to be drawn in case of tubes.
;; Default number of faces is 8.
;; --------------------------------------------------------------------------
(defun update-linestyle (opt val)
  (cond
    ((atom val)
       (case val
         ($dots     (setf (gethash opt *gr-options*) 0))
         ($solid    (setf (gethash opt *gr-options*) 1))
         ($dashes   (setf (gethash opt *gr-options*) 2))
         ($dot_dash (setf (gethash opt *gr-options*) 6))
         ($tube     (setf (gethash opt *gr-options*) -8))
         (otherwise  (merror "draw: illegal line type: ~M" val) )))
    ((and ($listp val)
          (= ($length val) 2)
          (equal ($first val) '$tube)
          (integerp ($second val))
          (> ($second val) 2))
       (setf (gethash opt *gr-options*) (- ($second val)) ) )
    (t
       (merror "draw: unknown line type: ~M" val))))



;; update points_joined option
;; ---------------------------
(defun update-pointsjoined (val)
  (if (member val '(t nil $impulses))
    (setf (gethash '$points_joined *gr-options*) val)
    (merror "draw: illegal points_joined option: ~M " val)) )






(defun ini-local-option-variables ()
  (setf ; global variables
       *draw-transform-dimensions* 0
       *draw-transform-f1* nil
       *draw-transform-f2* nil
       *draw-transform-f3* nil
       *draw-enhanced3d-type* 0
       *draw-enhanced3d-fun* nil)  )




;; Sets default values to global options
(defun ini-global-options ()
  (setf ; global options
      (gethash '$columns *gr-options*)      1
      (gethash '$terminal *gr-options*)     '$screen  ; defined as screen, png, jpg, gif, svg,
                                                      ; eps, eps_color, pdf, pdfcairo, wxt or
                                                      ; aquaterm. A list of type [term, number]
                                                      ; is also admited if term is screen, wxt
                                                      ; or aquaterm
      (gethash '$dimensions *gr-options*)   '(600 500)
      (gethash '$file_name *gr-options*)         "maxima_out"
      (gethash '$gnuplot_file_name *gr-options*) "maxout.gnuplot"
      (gethash '$data_file_name *gr-options*)    "data.gnuplot"
      (gethash '$delay *gr-options*)        5      ; delay for animated gif's, default 5*(1/100) sec
   ))


;; Sets new values to graphic options
(defun update-gr-option (opt val)
   (case opt
      ($allocation
        (update-allocation val))
      ($dimensions
        (update-dimensions val))
      ($fill_density ; in range [0, 1]
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 1 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: fill_density must be a number in [0, 1]")))
      (($line_width $head_length $head_angle $xaxis_width $yaxis_width $zaxis_width $font_size)
            (update-positive-float opt val))
      ($xyplane ; defined as real number or false
            (setf val ($float val))
            (if (or (numberp val)
                    (null val ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal xyplane allocation: ~M " val)))
      ($point_size ; defined as non negative numbers
        (update-nonnegative-float opt val))
      ($points_joined ; defined as true, false or $impulses
        (update-pointsjoined val))
      ($colorbox ; defined as true, false or string
            (if (or (member val '(t nil))
                    (stringp val) )
              (setf (gethash opt *gr-options*) val)
              (merror "draw: illegal colorbox option: ~M " val)) )
      (($line_type $xaxis_type $yaxis_type $zaxis_type) ; defined as $solid or $dots
         (update-linestyle opt val) )
      (($tube_extremes) ; defined as a list of two elements, $open and/or $closed
            (if (and ($listp val)
                     (= ($length val) 2)
                     (member ($first val) '($open $closed))
                     (member ($second val) '($open $closed)))
               (setf (gethash opt *gr-options*) val)
               (merror "draw: illegal tube extreme types: ~M" val)))
      ($point_type
         (update-pointtype val))
      (($columns $nticks $adapt_depth $xu_grid $yv_grid $delay $x_voxel $y_voxel $z_voxel)
            (update-positive-integer opt val))
      ($contour_levels    ; positive integer, increment or set of points
            (cond ((and (integerp val) (> val 0 ))
                    (setf (gethash opt *gr-options*) val))
                  ((and ($listp val) (= ($length val) 3) )
                     (let ((ini  ($float (nth 1 val)))
                           (step ($float (nth 2 val)))
                           (end  ($float (nth 3 val))))
                        (cond ((and (< ini end)
                                    (< step (- end ini)))
                                (setf (gethash opt *gr-options*) (format nil "incremental ~a,~a,~a" ini step end)))
                              (t
                                (merror "draw: illegal contour level incremental description: ~M " val))) ))
                  ((and ($setp val) (not ($emptyp val)))
                     (let ((pts (map 'list #'$float (rest val)))
                           (str "discrete ") )
                       (dolist (num pts 'done)
                         (setf str (concatenate 'string str " " (format nil "~a," num))))
                       (setf (gethash opt *gr-options*) (string-trim '(#\,) str) ) ))
                  (t
                    (merror "draw: unknown contour level description: ~M " val))))
      ($opacity
         (update-opacity val))
      (($transparent $border $logx $logy $logz $logcb $head_both $grid
        $xaxis_secondary $yaxis_secondary $axis_bottom $axis_left $axis_top
        $axis_right $axis_3d $surface_hide $xaxis $yaxis $zaxis $unit_vectors
        $xtics_rotate $ytics_rotate $xtics_secondary_rotate $ytics_secondary_rotate
        $ztics_rotate $xtics_axis $ytics_axis $xtics_secondary_axis
        $ytics_secondary_axis $ztics_axis $draw_realpart $wired_surface) ; true or false
          (update-boolean-option opt val))
      ($filled_func  ; true, false or an expression
         (setf (gethash opt *gr-options*) val))
      ($transform
         (update-transform val))
      ($enhanced3d
         (update-enhanced3d val))
      (($xtics $ytics $xtics_secondary $ytics_secondary $ztics $cbtics)
        ; $auto or t, $none or nil, number, increment, set, set of pairs
            (cond ((member val '($none nil))   ; nil is maintained for back-portability
                     (setf (gethash opt *gr-options*) nil))
                  ((member val '($auto t))     ; t is maintained for back-portability
                     (setf (gethash opt *gr-options*) "autofreq"))
                  ((and (numberp (setf val ($float val)))  ; increment
                        (> val 0 ))
                     (setf (gethash opt *gr-options*) val))
                  ((and ($listp val)                          ; [ini,incr,end]
                        (= ($length val) 3)
                        (< (cadr val) (cadddr val))
                        (> (caddr val) 0)
                        (< (caddr val) (- (cadddr val) (cadr val))) )
                     (setf (gethash opt *gr-options*) 
                           (format nil "~a,~a,~a" (cadr val) (caddr val) (cadddr val))))
                  ((and ($setp val)
                        (every #'(lambda (z) (numberp z))
                               (cdr val)) )                   ; {n1,n2,n3,...}
                     (setf 
                        (gethash opt *gr-options*)
                        (do ((k (cdr val) (cdr k))
                             (str "" (concatenate 'string str (format nil "~a," (car k)))) )
                            ((null k) (concatenate 
                                         'string
                                         "("
                                         (string-right-trim "," str)
                                         ")")))))
                  ((and ($setp val)
                        (every #'(lambda (z) (and ($listp z)
                                                  (= ($length z) 2)
                                                  (numberp (caddr z)) ))
                               (cdr val)) )                     ; {[lab1,n1],[lab2,n2],...}
                     (setf 
                        (gethash opt *gr-options*)
                        (do ((k (cdr val) (cdr k))
                             (str "" (concatenate
                                        'string
                                         str
                                        (format nil "\"~a\" ~a," (cadar k) (caddar k)))))
                            ((null k) (concatenate
                                         'string
                                         "("
                                         (string-right-trim "," str)
                                         ")")))))
                  (t
                     (merror "draw: illegal tics allocation: ~M" val)) ))
      ($terminal
        (update-terminal val))
      ($head_type ; defined as $filled, $empty and $nofilled
            (if (member val '($filled $empty $nofilled))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal head type for vectors: ~M" val)))
      ($contour ; defined as $none, $base, $surface, $both and $map
            (if (member val '($base $surface $both $map))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal contour allocation: ~M" val)))
      ($proportional_axes ; defined as $none, $xy and $xyz
            (if (member val '($none $xy $xyz))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal proportional_axes specification")))
      ($error_type ; defined as $x, $y and $xy
            (if (member val '($x $y $xy $boxes))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal error_type specification")))
      ($label_alignment ; defined as $center, $left and $right
            (if (member val '($center $left $right))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label alignment: ~M" val)))
      ($label_orientation ; defined as $horizontal and $vertical
            (if (member val '($horizontal $vertical))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label orientation: ~M" val)))
      (($key $file_name $xy_file $title $xlabel $ylabel $zlabel $font
        $gnuplot_file_name $data_file_name)
            (update-string opt val))
      ($user_preamble ; defined as a string or a Maxima list of strings
            (let ((str ""))
              (cond
                (($atom val)
                  (setf str val))
                (($listp val)
                  (dolist (st (rest val))
                    (if (not ($atom st))
                        (merror "draw: user preamble ~M should be a string" st))
                    (setf str (concatenate 'string
                                            str
                                            (format nil (if (string= str "") "~a" "~%~a") st)))))
                (t (merror "draw: illegal user preamble especification")))
              (setf (gethash opt *gr-options*) str))  )
      (($xrange $yrange $xrange_secondary $yrange_secondary
        $zrange $cbrange) ; defined as a Maxima list with two numbers in increasing order
            (cond ((member val '($auto nil))     ; nil is maintained for back-portability
                     (setf (gethash opt *gr-options*) nil))
                  ((or (not ($listp val))
                       (/=  ($length val) 2))
                     (merror "draw: illegal range: ~M " val))
                  (t
                     (let ((fval1 ($float (cadr val)))
                           (fval2 ($float (caddr val))))
                       (cond
                         ((or (not (floatp fval1))
                              (not (floatp fval2))
                              (< fval2 fval1))
                            (merror "draw: illegal values in range specification"))
                         ((= ($length val) 2)  ; it's a trick: length 2 => user change
                            (setf (gethash opt *gr-options*) (list fval1 fval2)))
                         (t  ; should be length 3 or nil option => automatic computation of ranks
                            (setf (gethash opt *gr-options*) (list fval1 fval2 0)) ))  ))) )
      (($ip_grid $ip_grid_in)
       (if (not ($listp val))
           (merror "draw: illegal value for grid")
           (if (not (and (integerp ($first val))
                         (integerp ($second val))))
               (merror "draw: illegal value for grid")
               (setf (gethash opt *gr-options*) val))))
      ($palette
        (update-palette val))
      (($color $fill_color $xaxis_color $yaxis_color
        $zaxis_color $background_color)
        (update-color opt val))
      ($view
        (update-view val))


      ; DEPRECATED OPTIONS
      ($file_bgcolor
        ($print "WARNING: 'file_bgcolor' is deprecated, using 'background_color' instead...")
        (update-color '$background_color val))
      ($rot_vertical
        ($print "WARNING: 'rot_vertical' is deprecated, using 'view' instead...")
        (update-view (list '(mlist) val (second (gethash '$view *gr-options*)))))
      ($rot_horizontal
        ($print "WARNING: 'rot_horizontal' is deprecated, using 'view' instead...")
        (update-view (list '(mlist) (first (gethash '$view *gr-options*)) val)))
      ($pic_width
        ($print "WARNING: 'pic_width' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) val (second (gethash '$dimensions *gr-options*)))))
      ($pic_height
        ($print "WARNING: 'pic_height' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (first (gethash '$dimensions *gr-options*)) val)))
      (($eps_width $pdf_width)
        ($print "WARNING: 'eps_width' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (* 100 val) (second (gethash '$dimensions *gr-options*)))))
      (($eps_height $pdf_height)
        ($print "WARNING: 'eps_height' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (first (gethash '$dimensions *gr-options*)) (* 100 val))))

      (otherwise (merror "draw: unknown option ~M " opt))  ) )










;;; COMMON GNUPLOT - VTK AUXILIARY FUNCTIONS

(defun near-equal (a b)
  (let ((eps 10.0d-14))
    (< (abs (- a b)) eps)))

;; Transforms arguments to make-scene-2d, make-scene-3d,
;; draw, and vtk3d to a unique list. With this piece of code,
;; gr2d, gr3d, draw, and model3d admit as arguments nested lists
;; of options and graphic objects
(defmacro listify-arguments ()
   '(rest ($flatten
             ($tree_reduce 
               '$append
               (cons '(mlist)
                     (map 
                       'list #'(lambda (z) (if ($listp z) z (list '(mlist) z)))
                       args))))))

;; The following functions implement the marching cubes algorithm
;; for implicit functions in 3d.
(simplify ($load "implicit3d.lisp"))

; Copies multidimensional arrays.
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
      (make-array
         dims
         :element-type (array-element-type array)
         :displaced-to array)
      dims)))

; Calculates surface-edge intersection by interpolation
(defun edge-interpolation (x1 y1 z1 x2 y2 z2 v1 v2)
  (cond ((or (< (abs v1) 0.00001)
             (< (abs (- v1 v2)) 0.00001))
          (list x1 y1 z1))
        ((< (abs v2) 0.00001)
          (list x2 y2 z2))
        (t
          (let ((m (/ (- v1) (- v2 v1))))
            (list
              (+ x1 (* m (- x2 x1)))
              (+ y1 (* m (- y2 y1)))
              (+ z1 (* m (- z2 z1))))))))

(defmacro make-triangle-vertices (i1 j1 k1 i2 j2 k2 e1 e2)
  `(edge-interpolation
       (aref px ,i1) (aref py ,j1) (aref pz ,k1)
       (aref px ,i2) (aref py ,j2) (aref pz ,k2)
       (aref val ,e1) (aref val ,e2)))

(defun flatten (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
          (append (flatten (car lis)) (flatten (cdr lis))))
        (t
          (append (list (car lis)) (flatten (cdr lis))))))

(defun find-triangles (expr par1 xmin xmax par2 ymin ymax par3 zmin zmax)
  (let* ((nx (get-option '$x_voxel))
         (ny (get-option '$y_voxel))
         (nz (get-option '$z_voxel))
         (dx (/ (- xmax xmin) nx))
         (dy (/ (- ymax ymin) ny))
         (dz (/ (- zmax zmin) nz))
         (fcn (coerce-float-fun (m- ($lhs expr) ($rhs expr)) `((mlist) ,par1 ,par2 ,par3)))
         (vert '())
         (px (make-array (+ nx 1) :element-type 'flonum))
         (py (make-array (+ ny 1) :element-type 'flonum))
         (pz (make-array (+ nz 1) :element-type 'flonum))
         (oldval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum))
         (newval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum))    )
    ; initialize coordinate arrays
    (loop for i to nx do (setf (aref px i) (+ xmin (* i dx))))
    (loop for j to ny do (setf (aref py j) (+ ymin (* j dy))))
    (loop for k to nz do (setf (aref pz k) (+ zmin (* k dz))))

    ; initialize first layer
    (loop for i to nx do
      (loop for j to ny do
        (let ((fxy (funcall fcn (aref px i) (aref py j) (aref pz 0))))
          (if (floatp fxy)
            (setf (aref oldval i j) fxy)
            (merror "draw3d (implicit): non real value")))))

    ; begin triangularization process
    (loop for k from 1 to nz do

      ; calculate node values in new layer
      (loop for i to nx do
        (loop for j to ny do
          (let ((fxy (funcall fcn (aref px i) (aref py j) (aref pz k))))
            (if (floatp fxy)
              (setf (aref newval i j) fxy)
              (merror "draw3d (implicit): check surface definition; non real value")))))

      ; analyze voxels in this slide
      (loop for i below nx do
        (loop for j below ny do
          (let* (triangles
                 (cubidx 0)
                 (k-1 (- k 1))
                 (i+1 (+ i 1))
                 (j+1 (+ j 1))
                 (val (make-array 8 :element-type 'flonum
                                    :initial-contents
                                       `(,(aref oldval i j+1) ,(aref oldval i+1 j+1)
                                         ,(aref oldval i+1 j) ,(aref oldval i j)
                                         ,(aref newval i j+1) ,(aref newval i+1 j+1)
                                         ,(aref newval i+1 j) ,(aref newval i j)))))
            (when (<= (aref val 0) 0.0) (setf cubidx (logior cubidx 1)))
            (when (<= (aref val 1) 0.0) (setf cubidx (logior cubidx 2)))
            (when (<= (aref val 2) 0.0) (setf cubidx (logior cubidx 4)))
            (when (<= (aref val 3) 0.0) (setf cubidx (logior cubidx 8)))
            (when (<= (aref val 4) 0.0) (setf cubidx (logior cubidx 16)))
            (when (<= (aref val 5) 0.0) (setf cubidx (logior cubidx 32)))
            (when (<= (aref val 6) 0.0) (setf cubidx (logior cubidx 64)))
            (when (<= (aref val 7) 0.0) (setf cubidx (logior cubidx 128)))
            (setf triangles (aref *i3d_triangles* cubidx))   ; edges intersecting the surface
            (do ((e 0 (1+ e)))
                ((= (aref triangles e) -1) 'done)
              (push 
                (case (aref triangles e)
                  (0  (make-triangle-vertices i j+1 k-1 i+1 j+1 k-1 0 1))
                  (1  (make-triangle-vertices i+1 j+1 k-1 i+1 j k-1 1 2))
                  (2  (make-triangle-vertices i+1 j k-1 i j k-1 2 3))
                  (3  (make-triangle-vertices i j k-1 i j+1 k-1 3 0))
                  (4  (make-triangle-vertices i j+1 k i+1 j+1 k 4 5))
                  (5  (make-triangle-vertices i+1 j+1 k i+1 j k 5 6))
                  (6  (make-triangle-vertices i+1 j k i j k 6 7))
                  (7  (make-triangle-vertices i j k i j+1 k 7 4))
                  (8  (make-triangle-vertices i j+1 k-1 i j+1 k 0 4))
                  (9  (make-triangle-vertices i+1 j+1 k-1 i+1 j+1 k 1 5))
                  (10 (make-triangle-vertices i+1 j k-1 i+1 j k 2 6))
                  (11 (make-triangle-vertices i j k-1 i j k 3 7)) )
                vert)))))
      ; make oldval a copy of newval
      (setf oldval (copy-array newval)))
    vert))

