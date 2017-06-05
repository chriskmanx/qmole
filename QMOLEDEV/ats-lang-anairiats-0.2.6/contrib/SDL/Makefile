#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: January, 2010
#

######

ATSHOMEQ="$(ATSHOME)"
ATSCC=$(ATSHOMEQ)/bin/atscc -Wall
SDLCFLAGS=`sdl-config --cflags`

######

all:: atsctrb_SDL.o
all:: clean

######

OBJECTS := \
  SDL_sats.o \
  SDL_dats.o \
  SDL_image_sats.o \
  SDL_ttf_sats.o \

######

atsctrb_SDL.o: $(OBJECTS) ; ld -r -o $@ $(OBJECTS)

######

SDL_sats.o: \
   SATS/SDL.sats \
   SATS/SDL_thread.sats \
   SATS/SDL_timer.sats \
   SATS/SDL_video.sats
	$(ATSCC) $(SDLCFLAGS) -o $@ -c SATS/SDL.sats

SDL_dats.o: DATS/SDL.dats
	$(ATSCC) $(SDLCFLAGS) -o $@ -c DATS/SDL.dats

######

SDL_image_sats.o: SATS/SDL_image.sats
	$(ATSCC) $(SDLCFLAGS) -o $@ -c SATS/SDL_image.sats

######

SDL_ttf_sats.o: SATS/SDL_ttf.sats
	$(ATSCC) $(SDLCFLAGS) -o $@ -c SATS/SDL_ttf.sats

######

RMF=rm -f

######

clean:: ; $(RMF) SDL_sats.c SDL_sats.o
clean:: ; $(RMF) SDL_dats.c SDL_dats.o
clean:: ; $(RMF) SDL_image_sats.c SDL_image_sats.o
clean:: ; $(RMF) SDL_ttf_sats.c SDL_ttf_sats.o

cleanall: clean ; $(RMF) atsctrb_SDL.o

###### end of [Makefile] ######
