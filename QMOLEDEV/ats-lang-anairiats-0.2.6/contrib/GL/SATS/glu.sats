(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009
//
(* ****** ****** *)

%{#
#include "contrib/GL/CATS/glu.cats"
%}

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"

(* ****** ****** *)

abst@ype GLUquadricObj = $extype"ats_GLUquadricObj_type"

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluCylinder (
  GLUquadric* quad, GLdouble base, GLdouble top, GLdouble height
, GLint slices, GLint stacks
);
*)
fun gluCylinder {n,sk:nat} (
  qobj: &GLUquadricObj
, base: GLdouble, top: GLdouble, height: GLdouble
, slices: int n, stacks: int sk
) : void
  = "mac#atsctrb_gluCylinder"
// end of [gluCylinder]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluDeleteQuadric (GLUquadric* quad);
*)
fun gluDeleteQuadric
  {l:addr} (
  pf: GLUquadricObj @ l | p: ptr l
) : void
  = "mac#atsctrb_gluDeleteQuadric"
// end of [gluDeleteQuadric]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluDisk (
  GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops
) ;
*)
fun gluDisk {n,lp:nat} (
  qobj: &GLUquadricObj
, inner: GLdouble, outer: GLdouble, slices: int n, loops: int lp
) : void = "mac#atsctrb_gluDisk"
// end of [gluDisk]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluLookAt (
  GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ
, GLdouble centerX, GLdouble centerY, GLdouble centerZ
, GLdouble upX, GLdouble upY, GLdouble upZ
) ;
*)
typedef gluLookAt_type (a:t@ype) = (
  a(*eyeX*), a(*eyeY*), a(*eyeZ*)
, a(*centerX*), a(*centerY*), a(*centerZ*)
, a(*upX*), a(*upY*), a(*upZ*)
) -<fun1> void // end of [gluLookAt_type]

symintr gluLookAt

fun gluLookAt_double
  : gluLookAt_type (double) = "mac#atsctrb_gluLookAt"
overload gluLookAt with gluLookAt_double

fun gluLookAt_GLdouble
  : gluLookAt_type (GLdouble) = "mac#atsctrb_gluLookAt"
overload gluLookAt with gluLookAt_GLdouble

(* ****** ****** *)

fun gluNewQuadric (
// there is no argument
) : [l:addr] (
  option_v (GLUquadricObj @ l, l <> null) | ptr l
) = "mac#atsctrb_gluNewQuadric"
// end of [gluNewQuadric]

fun gluNewQuadric_exn (
// there is no argument
) : [l:addr] (GLUquadricObj @ l | ptr l)
  = "atsctrb_gluNewQuadric_exn" // this is a function!
// end of [gluNewQuadric_exn]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluOrtho2D (
  GLdouble left, GLdouble right, GLdouble bottom, GLdouble top
) ;
*)
typedef gluOrtho2D_type (a:t@ype) =
  (a(*lft*), a(*rgh*), a(*bot*), a(*top*)) -<fun1> void

symintr gluOrtho2D

fun gluOrtho2D_double
  : gluOrtho2D_type (double) = "mac#atsctrb_gluOrtho2D"
overload gluOrtho2D with gluOrtho2D_double

fun gluOrtho2D_GLdouble
  : gluOrtho2D_type (GLdouble) = "mac#atsctrb_gluOrtho2D"
overload gluOrtho2D with gluOrtho2D_GLdouble

(* ****** ****** *)
  
(*
GLAPI void GLAPIENTRY gluPartialDisk (
  GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops
, GLdouble start, GLdouble sweep
) ;
*)
fun gluPartialDisk
  {n,lp:nat} (
  qobj: &GLUquadricObj, inner: GLdouble, outer: GLdouble, slices: int n, loops: int lp
, start: GLdouble, sweep: GLdouble
) : void
  = "mac#atsctrb_gluPartialDisk"
// end of [fun]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluPerspective (GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar);
*)
typedef gluPerspective_type (a:t@ype) =
  (a(*fovy*), a(*aspect*), a(*zNear*), a(*zFar*)) -<fun1> void
// end of [gluPerspective_type]

symintr gluPerspective

fun gluPerspective_double
  : gluPerspective_type (double)
  = "mac#atsctrb_gluPerspective"
overload gluPerspective with gluPerspective_double

fun gluPerspective_GLdouble
  : gluPerspective_type (GLdouble)
  = "mac#atsctrb_gluPerspective"
overload gluPerspective with gluPerspective_GLdouble

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluQuadricDrawStyle (GLUquadric* quad, GLenum draw);
*)
fun gluQuadricDrawStyle (
  qobj: &GLUquadricObj, draw: GLenum
) : void
  = "mac#atsctrb_gluQuadricDrawStyle"
// end of [fun]

(*
GLAPI void GLAPIENTRY gluQuadricNormals (GLUquadric* quad, GLenum normal);
*)
fun gluQuadricNormals (
  qobj: &GLUquadricObj, normal: GLenum
) : void
  = "mac#atsctrb_gluQuadricNormals"
// end of [fun]

(*
GLAPI void GLAPIENTRY gluQuadricOrientation (GLUquadric* quad, GLenum orientation);
*)
fun gluQuadricTexture (
  qobj: &GLUquadricObj, orientation: GLenum
) : void
  = "mac#atsctrb_gluQuadricOrientation"
// end of [fun]

(*
GLAPI void GLAPIENTRY gluQuadricTexture (GLUquadric* quad, GLboolean texture);
*)
fun gluQuadricTexture (
  qobj: &GLUquadricObj, texture: GLboolean
) : void
  = "mac#atsctrb_gluQuadricTexture"
// end of [fun]

(* ****** ****** *)

(*
GLAPI void GLAPIENTRY gluSphere (GLUquadric* quad, GLdouble radius, GLint slices, GLint stacks);
*)
fun gluSphere
  {n,lp:nat} (
  qobj: &GLUquadricObj
, radius: GLdouble, slices: int n, loops: int lp
) : void
  = "mac#atsctrb_gluSphere"

(* ****** ****** *)

(*
GLAPI GLint GLAPIENTRY gluUnProject (
  GLdouble winX, GLdouble winY, GLdouble winZ
, const GLdouble *model, const GLdouble *proj
, const GLint *view
, GLdouble* objX, GLdouble* objY, GLdouble* objZ
) ;
*)
fun gluUnProject (
  winX: GLdouble
, winY: GLdouble
, winZ: GLdouble
, model: &(@[GLdouble][16])
, project: &(@[GLdouble][16])
, viewport: &(@[GLint][4])
, objX: &GLdouble? >> GLdouble
, objY: &GLdouble? >> GLdouble
, objZ: &GLdouble? >> GLdouble
) : GLint
  = "mac#atsctrb_gluUnProject"
// end of [fun]

(* ****** ****** *)

(* end of [glu.sats] *)
