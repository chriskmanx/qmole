#!/bin/bash
#
# This script generates a gource visualisation with some parameters.
#
# you need a recent gource and ffmpeg install for this to work

# Settings

TITLE="Netsurf"

# length and quality
TYPE="sml" # sml, std, lrg

#Camera mode
CMODE=overview # overview, track

# standard monitor (suitable for video projector playback)
OUTPUT_SIZE="1024x768"

# HD widescreen 720p
#OUTPUT_SIZE="1280x720"

# HD widescreen 1080p
#OUTPUT_SIZE="1280x1080"


######################################################################

#quality parameters
case ${TYPE} in
    "std")
    # standard overview
    QPARAM="-s 0.25 -i 600 -a 1"
    ;;
  
    "lrg")
    # large overview
    QPARAM="-s 0.5 -i 200 -a 5"
    ;;

    "sml")
    # rapid overview (3mins)
    QPARAM="-s 0.04 -i 30 -a 1"
    ;;

    *)
    # bad type
    echo "bad type"
    exit 1
    ;;

esac

# filename
FILENAME=${TITLE}-gource-${TYPE}-${OUTPUT_SIZE}-${CMODE}.mp4

# filter some directories which are not interesting
FILEFILTER="\!NetSurf/|riscos/distribution/|gtk/res/|framebuffer/res/|amiga/resources/|beos/res/|cocoa/res/|windows/res/|atari/res"

#gource -1280x720 -s 0.04 -i 30 -a 1 --highlight-all-users --output-framerate 25 --hide filenames --disable-progress --stop-at-end --date-format "%d %B %Y" --bloom-intensity 0.2 --file-filter '/art/' --file-filter '/changemailer/' --file-filter '/libnspng/' --file-filter '/libnsbmp/examples/' --file-filter '/netsurfweb/' --file-filter '/netsurfbuild/' --file-filter '/netsurftest/' --file-filter '/hubbub/test/' --file-filter '/dom/test/'  --file-filter '/iconv/' --file-filter '/libharu/' --camera-mode track --output-ppm-stream - > temp.ppm

#gource -1280x720 -s 0.04 -i 30 -a 1 --bloom-multiplier 0.10 --bloom-intensity 0.5 --title "Netsurf" --highlight-all-users --output-framerate 25 --hide filenames --stop-at-end --date-format "%d %B %Y" --bloom-intensity 0.2 --file-filter '/art/' --file-filter '/changemailer/' --file-filter '/libnspng/' --file-filter '/libnsbmp/examples/' --file-filter '/netsurfweb/' --file-filter '/netsurfbuild/' --file-filter '/netsurftest/' --file-filter '/hubbub/test/' --file-filter '/dom/test/'  --file-filter '/iconv/' --file-filter '/libharu/' --camera-mode track --output-ppm-stream - > temp.ppm

#gource -stop-at-end --key 5C--title "NetSurf Development" --highlight-users --max-file-lag -1 -f -1280x720 --seconds-per-day 0.06 --hide mouse,progress,filenames --file-idle-time 20 --disable-bloom --date-format "%e %b %Y" -o - | ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset veryslow -crf 1 -threads 0 -bf 0 gource.mp4

# generate
gource --title "NetSurf Development" -${OUTPUT_SIZE} ${QPARAM} --max-files 10000 --bloom-multiplier 0.10 --bloom-intensity 0.5 --title ${TITLE} --highlight-all-users --output-framerate 25 --hide filenames --stop-at-end --date-format "%d %B %Y" --bloom-intensity 0.2 --file-filter "${FILEFILTER}" --key --camera-mode ${CMODE} --output-ppm-stream - > temp.ppm

#convert the ppm to movie
cat temp.ppm | ffmpeg -y -b 2000K -r 25 -f image2pipe -vcodec ppm -i - -vcodec libx264 ${FILENAME}


