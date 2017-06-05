/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
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
*/

/* ****** ****** */

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: March, 2010

/* ****** ****** */

#ifndef ATSCTRB_SDL_MIXER_CATS
#define ATSCTRB_SDL_MIXER_CATS

/* ****** ****** */

#include "SDL/SDL.h"
#include "SDL/SDL_mixer.h"

/* ****** ****** */

#define atsctrb_Mix_OpenAudio Mix_OpenAudio
#define atsctrb_Mix_AllocateChannels Mix_AllocateChannels

#define atsctrb_Mix_QuerySpec Mix_QuerySpec

#define atsctrb_Mix_LoadWAV Mix_LoadWAV
#define atsctrb_Mix_LoadMUS Mix_LoadMUS

#define atsctrb_Mix_FreeChunk Mix_FreeChunk
#define atsctrb_Mix_FreeMusic Mix_FreeMusic

#define atsctrb_Mix_PlayChannel Mix_PlayChannel
#define atsctrb_Mix_PlayChannelTimed Mix_PlayChannelTimed
#define atsctrb_Mix_PlayMusic Mix_PlayMusic

#define atsctrb_Mix_HaltChannel Mix_HaltChannel
#define atsctrb_Mix_HaltGroup Mix_HaltGroup
#define atsctrb_Mix_HaltMusic Mix_HaltMusic

#define atsctrb_Mix_PauseMusic Mix_PauseMusic
#define atsctrb_Mix_ResumeMusic Mix_ResumeMusic
#define atsctrb_Mix_RewindMusic Mix_RewindMusic
#define atsctrb_Mix_PausedMusic Mix_PausedMusic

#define atsctrb_Mix_Playing Mix_Playing
#define atsctrb_Mix_PlayingMusic Mix_PlayingMusic

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_MIXER_CATS]

/* end of [SDL_mixer.cats] */
