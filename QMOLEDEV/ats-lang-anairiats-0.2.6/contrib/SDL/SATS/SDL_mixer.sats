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
// Starting time: March, 2010
//
(* ****** ****** *)

%{#
#include "contrib/SDL/CATS/SDL_mixer.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for static loading at run-time

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

macdef MIX_DEFAULT_FORMAT = $extval (Uint16, "MIX_DEFAULT_FORMAT")
macdef MIX_DEFAULT_FREQUENCY = $extval (int, "MIX_DEFAULT_FREQUENCY")
macdef MIX_DEFAULT_CHANNELS = $extval (int, "MIX_DEFAULT_CHANNELS")
macdef MIX_MAX_VOLUME = $extval (int, "MIX_MAX_VOLUME")

(* ****** ****** *)

abst@ype Mix_Fading = $extype"Mix_Fading"
macdef MIX_NO_FADING = $extval (Mix_Fading, "MIX_NO_FADING")
macdef MIX_FADING_OUT = $extval (Mix_Fading, "MIX_FADING_OUT")
macdef MIX_FADING_IN = $extval (Mix_Fading, "MIX_FADING_IN")

abst@ype Mix_MusicType = $extype"Mix_MusicType"
macdef MUS_NONE = $extval (Mix_MusicType, "MUS_NONE")
macdef MUS_CMD = $extval (Mix_MusicType, "MUS_CMD")
macdef MUS_WAV = $extval (Mix_MusicType, "MUS_WAV")
macdef MUS_MOD = $extval (Mix_MusicType, "MUS_MOD")
macdef MUS_MID = $extval (Mix_MusicType, "MUS_MID")
macdef MUS_OGG = $extval (Mix_MusicType, "MUS_OGG")
macdef MUS_MP3 = $extval (Mix_MusicType, "MUS_MP3")

(* ****** ****** *)

(*
viewtypedef Mix_Chunk =
  $extype_struct "Mix_Chunk" of {
  allocated= int
, abuf= ptr // Uint8* // this needs to be taken care of at some point
, alen= Uint32
, volume= Uint8 // Per-sample volume, 0-128
} // end of [Mix_Chunk]
*)
//
// this type is not refcounted:
//
absviewtype Mix_Chunk_ref (l:addr) // = Mix_Chunk*
viewtypedef Mix_Chunk_ref0 = [l:agez] Mix_Chunk_ref (l)
viewtypedef Mix_Chunk_ref1 = [l:addr | l > null] Mix_Chunk_ref (l)

fun Mix_Chunk_ref_is_null
  {l:addr} (x: !Mix_Chunk_ref l):<> bool (l == null)
  = "atsctrb_SDL_ref_is_null"
// overload ref_is_null with Mix_Chunk_ref_is_null

fun Mix_Chunk_ref_isnot_null
  {l:addr} (x: !Mix_Chunk_ref l):<> bool (l <> null)
  = "atsctrb_SDL_ref_isnot_null"
// overload ref_isnot_null with Mix_Chunk_ref_isnot_null

(* ****** ****** *)
//
// HX: is this type refcounted?
//
absviewtype Mix_Music_ref (l:addr) // = Mix_Music*
viewtypedef Mix_Music_ref0 = [l:agez] Mix_Music_ref (l)
viewtypedef Mix_Music_ref1 = [l:addr | l > null] Mix_Music_ref (l)

fun Mix_Music_ref_is_null
  {l:addr} (x: !Mix_Music_ref l):<> bool (l == null)
  = "atsctrb_SDL_ref_is_null"
// overload ref_is_null with Mix_Music_ref_is_null

fun Mix_Music_ref_isnot_null
  {l:addr} (x: !Mix_Music_ref l):<> bool (l <> null)
  = "atsctrb_SDL_ref_isnot_null"
// overload ref_isnot_null with Mix_Music_ref_isnot_null

(* ****** ****** *)

fun Mix_OpenAudio (
  frequency: int, format: Uint16, channels: int, chunksize: int
) : int(*err*)
  = "mac#atsctrb_Mix_OpenAudio"
// end of [Mix_OpenAudio]

(* ****** ****** *)

fun Mix_AllocateChannels
  (numchans: int): int = "mac#atsctrb_Mix_AllocateChannels"
// end of [Mix_AllocateChannels]

(* ****** ****** *)

fun Mix_QuerySpec (
  frequency: &int? >> opt (int, i > 0)
, format: &Uint16? >> opt (Uint16, i > 0)
, channels: &int? >> opt (int, i > 0)
) : #[i:two] int i
  = "mac#atsctrb_Mix_QuerySpec"

(* ****** ****** *)

(*
/* Load a wave file or a music (.mod .s3m .it .xm) file */
extern DECLSPEC Mix_Chunk * SDLCALL Mix_LoadWAV_RW(SDL_RWops *src, int freesrc);
#define Mix_LoadWAV(file)	Mix_LoadWAV_RW(SDL_RWFromFile(file, "rb"), 1)
extern DECLSPEC Mix_Music * SDLCALL Mix_LoadMUS(const char *file);
*)
fun Mix_LoadWAV (file: string): Mix_Chunk_ref0 = "mac#atsctrb_Mix_LoadWAV"
fun Mix_LoadMUS (file: string): Mix_Music_ref0 = "mac#atsctrb_Mix_LoadMUS"

(* ****** ****** *)

fun Mix_FreeChunk
  {l:agz} (chunk: Mix_Chunk_ref l): void = "mac#atsctrb_Mix_FreeChunk"
// end of [Mix_FreeChunk]

fun Mix_FreeMusic
  {l:agz} (music: Mix_Music_ref l): void = "mac#atsctrb_Mix_FreeMusic"
// end of [Mix_FreeMusic]

(* ****** ****** *)

(*
#define Mix_PlayChannel(channel,chunk,loops) Mix_PlayChannelTimed(channel,chunk,loops,-1)
extern
DECLSPEC int SDLCALL Mix_PlayChannelTimed(int channel, Mix_Chunk *chunk, int loops, int ticks);
extern
DECLSPEC int SDLCALL Mix_PlayMusic(Mix_Music *music, int loops);
*)
fun Mix_PlayChannel {l:agz}
  (channel: int, chunk: !Mix_Chunk_ref l, loops: int): int
  = "mac#atsctrb_Mix_PlayChannel"
// end of [Mix_PlayChannel]

fun Mix_PlayChannelTimed
  {l:agz} (
  channel: int, chunk: !Mix_Chunk_ref l, loops: int, ticks: int
) : int
  = "mac#atsctrb_Mix_PlayChannelTimed"
// end of [Mix_PlayChannelTimed]

fun Mix_PlayMusic
  {l:agz} (
  music: !Mix_Music_ref l, loops: int
) : int
  = "mac#atsctrb_Mix_PlayMusic"
// end of [Mix_PlayMusic]

fun Mix_HaltChannel
  (channel: int): int = "mac#atsctrb_Mix_HaltChannel"
fun Mix_HaltGroup (tag: int): int = "mac#atsctrb_Mix_HaltGroup"
fun Mix_HaltMusic (): int = "mac#atsctrb_Mix_HaltMusic"

(*
/* Pause/Resume the music stream */
extern DECLSPEC void SDLCALL Mix_PauseMusic(void);
extern DECLSPEC void SDLCALL Mix_ResumeMusic(void);
extern DECLSPEC void SDLCALL Mix_RewindMusic(void);
extern DECLSPEC int SDLCALL Mix_PausedMusic(void);
*)
fun Mix_PauseMusic (): void = "mac#atsctrb_Mix_PauseMusic"
fun Mix_ResumeMusic (): void = "mac#atsctrb_Mix_ResumeMusic"
fun Mix_RewindMusic (): void = "mac#atsctrb_Mix_RewindMusic"
fun Mix_PausedMusic (): int = "mac#atsctrb_Mix_PausedMusic"

(*
extern DECLSPEC int SDLCALL Mix_Playing(int channel);
extern DECLSPEC int SDLCALL Mix_PlayingMusic(void);
*)
fun Mix_Playing
  (channel: int): int = "mac#atsctrb_Mix_Playing"
// end of [Mix_Playing]

fun Mix_PlayingMusic (): int = "mac#atsctrb_Mix_PlayingMusic"

(* ****** ****** *)

(* end of [SDL_mixer.sats] *)
