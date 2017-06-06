/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                  CCCC   OOO   L       OOO   RRRR    SSSSS                   %
%                 C      O   O  L      O   O  R   R   SS                      %
%                 C      O   O  L      O   O  RRRR     SSS                    %
%                 C      O   O  L      O   O  R R        SS                   %
%                  CCCC   OOO   LLLLL   OOO   R  R    SSSSS                   %
%                                                                             %
%                                                                             %
%                  Methods to Count the Colors in an Image                    %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"
#if defined(HasX11)
#include "xwindows.h"
#endif

/*
  Structures.
*/
typedef struct _NodeInfo
{
  unsigned char
    level;

  unsigned long
    number_unique;

  ColorPacket
    *list;

  struct _NodeInfo
    *child[8];
} NodeInfo;

typedef struct _Nodes
{
  NodeInfo
    nodes[NodesInAList];

  struct _Nodes
    *next;
} Nodes;

typedef struct _CubeInfo
{
  NodeInfo
    *root;

  unsigned int
    progress;

  unsigned long
    colors;

  unsigned int
    free_nodes;

  NodeInfo
    *node_info;

  Nodes
    *node_list;
} CubeInfo;

/*
  Color list.
*/
const ColorlistInfo
  XColorlist[757] =
  {
    { "AliceBlue", 240, 248, 255 },
    { "AntiqueWhite", 250, 235, 215 },
    { "AntiqueWhite1", 255, 239, 219 },
    { "AntiqueWhite2", 238, 223, 204 },
    { "AntiqueWhite3", 205, 192, 176 },
    { "AntiqueWhite4", 139, 131, 120 },
    { "BlanchedAlmond", 255, 235, 205 },
    { "BlueViolet", 138, 43, 226 },
    { "CadetBlue", 95, 158, 160 },
    { "CadetBlue1", 152, 245, 255 },
    { "CadetBlue2", 142, 229, 238 },
    { "CadetBlue3", 122, 197, 205 },
    { "CadetBlue4", 83, 134, 139 },
    { "CornflowerBlue", 100, 149, 237 },
    { "Crimson", 220, 20, 60 },
    { "DarkBlue", 0, 0, 139 },
    { "DarkCyan", 0, 139, 139 },
    { "DarkGoldenrod", 184, 134, 11 },
    { "DarkGoldenrod1", 255, 185, 15 },
    { "DarkGoldenrod2", 238, 173, 14 },
    { "DarkGoldenrod3", 205, 149, 12 },
    { "DarkGoldenrod4", 139, 101, 8 },
    { "DarkGray", 169, 169, 169 },
    { "DarkGreen", 0, 100, 0 },
    { "DarkGrey", 169, 169, 169 },
    { "DarkKhaki", 189, 183, 107 },
    { "DarkMagenta", 139, 0, 139 },
    { "DarkOliveGreen", 85, 107, 47 },
    { "DarkOliveGreen1", 202, 255, 112 },
    { "DarkOliveGreen2", 188, 238, 104 },
    { "DarkOliveGreen3", 162, 205, 90 },
    { "DarkOliveGreen4", 110, 139, 61 },
    { "DarkOrange", 255, 140, 0 },
    { "DarkOrange1", 255, 127, 0 },
    { "DarkOrange2", 238, 118, 0 },
    { "DarkOrange3", 205, 102, 0 },
    { "DarkOrange4", 139, 69, 0 },
    { "DarkOrchid", 153, 50, 204 },
    { "DarkOrchid1", 191, 62, 255 },
    { "DarkOrchid2", 178, 58, 238 },
    { "DarkOrchid3", 154, 50, 205 },
    { "DarkOrchid4", 104, 34, 139 },
    { "DarkRed", 139, 0, 0 },
    { "DarkSalmon", 233, 150, 122 },
    { "DarkSeaGreen", 143, 188, 143 },
    { "DarkSeaGreen1", 193, 255, 193 },
    { "DarkSeaGreen2", 180, 238, 180 },
    { "DarkSeaGreen3", 155, 205, 155 },
    { "DarkSeaGreen4", 105, 139, 105 },
    { "DarkSlateBlue", 72, 61, 139 },
    { "DarkSlateGray", 47, 79, 79 },
    { "DarkSlateGray1", 151, 255, 255 },
    { "DarkSlateGray2", 141, 238, 238 },
    { "DarkSlateGray3", 121, 205, 205 },
    { "DarkSlateGray4", 82, 139, 139 },
    { "DarkSlateGrey", 47, 79, 79 },
    { "DarkTurquoise", 0, 206, 209 },
    { "DarkViolet", 148, 0, 211 },
    { "DeepPink", 255, 20, 147 },
    { "DeepPink1", 255, 20, 147 },
    { "DeepPink2", 238, 18, 137 },
    { "DeepPink3", 205, 16, 118 },
    { "DeepPink4", 139, 10, 80 },
    { "DeepSkyBlue", 0, 191, 255 },
    { "DeepSkyBlue1", 0, 191, 255 },
    { "DeepSkyBlue2", 0, 178, 238 },
    { "DeepSkyBlue3", 0, 154, 205 },
    { "DeepSkyBlue4", 0, 104, 139 },
    { "DimGray", 105, 105, 105 },
    { "DimGrey", 105, 105, 105 },
    { "DodgerBlue", 30, 144, 255 },
    { "DodgerBlue1", 30, 144, 255 },
    { "DodgerBlue2", 28, 134, 238 },
    { "DodgerBlue3", 24, 116, 205 },
    { "DodgerBlue4", 16, 78, 139 },
    { "FloralWhite", 255, 250, 240 },
    { "ForestGreen", 34, 139, 34 },
    { "GhostWhite", 248, 248, 255 },
    { "GreenYellow", 173, 255, 47 },
    { "HotPink", 255, 105, 180 },
    { "HotPink1", 255, 110, 180 },
    { "HotPink2", 238, 106, 167 },
    { "HotPink3", 205, 96, 144 },
    { "HotPink4", 139, 58, 98 },
    { "IndianRed", 205, 92, 92 },
    { "IndianRed1", 255, 106, 106 },
    { "IndianRed2", 238, 99, 99 },
    { "IndianRed3", 205, 85, 85 },
    { "IndianRed4", 139, 58, 58 },
    { "Indigo", 75, 0, 130 },
    { "Indigo2", 33, 136, 104 },
    { "LavenderBlush", 255, 240, 245 },
    { "LavenderBlush1", 255, 240, 245 },
    { "LavenderBlush2", 238, 224, 229 },
    { "LavenderBlush3", 205, 193, 197 },
    { "LavenderBlush4", 139, 131, 134 },
    { "LawnGreen", 124, 252, 0 },
    { "LemonChiffon", 255, 250, 205 },
    { "LemonChiffon1", 255, 250, 205 },
    { "LemonChiffon2", 238, 233, 191 },
    { "LemonChiffon3", 205, 201, 165 },
    { "LemonChiffon4", 139, 137, 112 },
    { "LightBlue", 173, 216, 230 },
    { "LightBlue1", 191, 239, 255 },
    { "LightBlue2", 178, 223, 238 },
    { "LightBlue3", 154, 192, 205 },
    { "LightBlue4", 104, 131, 139 },
    { "LightCoral", 240, 128, 128 },
    { "LightCyan", 224, 255, 255 },
    { "LightCyan1", 224, 255, 255 },
    { "LightCyan2", 209, 238, 238 },
    { "LightCyan3", 180, 205, 205 },
    { "LightCyan4", 122, 139, 139 },
    { "LightGoldenrod", 238, 221, 130 },
    { "LightGoldenrod1", 255, 236, 139 },
    { "LightGoldenrod2", 238, 220, 130 },
    { "LightGoldenrod3", 205, 190, 112 },
    { "LightGoldenrod4", 139, 129, 76 },
    { "LightGoldenrodYellow", 250, 250, 210 },
    { "LightGray", 211, 211, 211 },
    { "LightGreen", 144, 238, 144 },
    { "LightGrey", 211, 211, 211 },
    { "LightPink", 255, 182, 193 },
    { "LightPink1", 255, 174, 185 },
    { "LightPink2", 238, 162, 173 },
    { "LightPink3", 205, 140, 149 },
    { "LightPink4", 139, 95, 101 },
    { "LightSalmon", 255, 160, 122 },
    { "LightSalmon1", 255, 160, 122 },
    { "LightSalmon2", 238, 149, 114 },
    { "LightSalmon3", 205, 129, 98 },
    { "LightSalmon4", 139, 87, 66 },
    { "LightSeaGreen", 32, 178, 170 },
    { "LightSkyBlue", 135, 206, 250 },
    { "LightSkyBlue1", 176, 226, 255 },
    { "LightSkyBlue2", 164, 211, 238 },
    { "LightSkyBlue3", 141, 182, 205 },
    { "LightSkyBlue4", 96, 123, 139 },
    { "LightSlateBlue", 132, 112, 255 },
    { "LightSlateGray", 119, 136, 153 },
    { "LightSlateGrey", 119, 136, 153 },
    { "LightSteelBlue", 176, 196, 222 },
    { "LightSteelBlue1", 202, 225, 255 },
    { "LightSteelBlue2", 188, 210, 238 },
    { "LightSteelBlue3", 162, 181, 205 },
    { "LightSteelBlue4", 110, 123, 139 },
    { "LightYellow", 255, 255, 224 },
    { "LightYellow1", 255, 255, 224 },
    { "LightYellow2", 238, 238, 209 },
    { "LightYellow3", 205, 205, 180 },
    { "LightYellow4", 139, 139, 122 },
    { "LimeGreen", 50, 205, 50 },
    { "MediumAquamarine", 102, 205, 170 },
    { "MediumBlue", 0, 0, 205 },
    { "MediumOrchid", 186, 85, 211 },
    { "MediumOrchid1", 224, 102, 255 },
    { "MediumOrchid2", 209, 95, 238 },
    { "MediumOrchid3", 180, 82, 205 },
    { "MediumOrchid4", 122, 55, 139 },
    { "MediumPurple", 147, 112, 219 },
    { "MediumPurple1", 171, 130, 255 },
    { "MediumPurple2", 159, 121, 238 },
    { "MediumPurple3", 137, 104, 205 },
    { "MediumPurple4", 93, 71, 139 },
    { "MediumSeaGreen", 60, 179, 113 },
    { "MediumSlateBlue", 123, 104, 238 },
    { "MediumSpringGreen", 0, 250, 154 },
    { "MediumTurquoise", 72, 209, 204 },
    { "MediumVioletRed", 199, 21, 133 },
    { "MidnightBlue", 25, 25, 112 },
    { "MintCream", 245, 255, 250 },
    { "MistyRose", 255, 228, 225 },
    { "MistyRose1", 255, 228, 225 },
    { "MistyRose2", 238, 213, 210 },
    { "MistyRose3", 205, 183, 181 },
    { "MistyRose4", 139, 125, 123 },
    { "NavajoWhite", 255, 222, 173 },
    { "NavajoWhite1", 255, 222, 173 },
    { "NavajoWhite2", 238, 207, 161 },
    { "NavajoWhite3", 205, 179, 139 },
    { "NavajoWhite4", 139, 121, 94 },
    { "NavyBlue", 0, 0, 128 },
    { "OldLace", 253, 245, 230 },
    { "OliveDrab", 107, 142, 35 },
    { "OliveDrab1", 192, 255, 62 },
    { "OliveDrab2", 179, 238, 58 },
    { "OliveDrab3", 154, 205, 50 },
    { "OliveDrab4", 105, 139, 34 },
    { "OrangeRed", 255, 69, 0 },
    { "OrangeRed1", 255, 69, 0 },
    { "OrangeRed2", 238, 64, 0 },
    { "OrangeRed3", 205, 55, 0 },
    { "OrangeRed4", 139, 37, 0 },
    { "PaleGoldenrod", 238, 232, 170 },
    { "PaleGreen", 152, 251, 152 },
    { "PaleGreen1", 154, 255, 154 },
    { "PaleGreen2", 144, 238, 144 },
    { "PaleGreen3", 124, 205, 124 },
    { "PaleGreen4", 84, 139, 84 },
    { "PaleTurquoise", 175, 238, 238 },
    { "PaleTurquoise1", 187, 255, 255 },
    { "PaleTurquoise2", 174, 238, 238 },
    { "PaleTurquoise3", 150, 205, 205 },
    { "PaleTurquoise4", 102, 139, 139 },
    { "PaleVioletRed", 219, 112, 147 },
    { "PaleVioletRed1", 255, 130, 171 },
    { "PaleVioletRed2", 238, 121, 159 },
    { "PaleVioletRed3", 205, 104, 137 },
    { "PaleVioletRed4", 139, 71, 93 },
    { "PapayaWhip", 255, 239, 213 },
    { "PeachPuff", 255, 218, 185 },
    { "PeachPuff1", 255, 218, 185 },
    { "PeachPuff2", 238, 203, 173 },
    { "PeachPuff3", 205, 175, 149 },
    { "PeachPuff4", 139, 119, 101 },
    { "PowderBlue", 176, 224, 230 },
    { "RosyBrown", 188, 143, 143 },
    { "RosyBrown1", 255, 193, 193 },
    { "RosyBrown2", 238, 180, 180 },
    { "RosyBrown3", 205, 155, 155 },
    { "RosyBrown4", 139, 105, 105 },
    { "RoyalBlue", 65, 105, 225 },
    { "RoyalBlue1", 72, 118, 255 },
    { "RoyalBlue2", 67, 110, 238 },
    { "RoyalBlue3", 58, 95, 205 },
    { "RoyalBlue4", 39, 64, 139 },
    { "SaddleBrown", 139, 69, 19 },
    { "SandyBrown", 244, 164, 96 },
    { "SeaGreen", 46, 139, 87 },
    { "SeaGreen1", 84, 255, 159 },
    { "SeaGreen2", 78, 238, 148 },
    { "SeaGreen3", 67, 205, 128 },
    { "SeaGreen4", 46, 139, 87 },
    { "SkyBlue", 135, 206, 235 },
    { "SkyBlue1", 135, 206, 255 },
    { "SkyBlue2", 126, 192, 238 },
    { "SkyBlue3", 108, 166, 205 },
    { "SkyBlue4", 74, 112, 139 },
    { "SlateBlue", 106, 90, 205 },
    { "SlateBlue1", 131, 111, 255 },
    { "SlateBlue2", 122, 103, 238 },
    { "SlateBlue3", 105, 89, 205 },
    { "SlateBlue4", 71, 60, 139 },
    { "SlateGray", 112, 128, 144 },
    { "SlateGray1", 198, 226, 255 },
    { "SlateGray2", 185, 211, 238 },
    { "SlateGray3", 159, 182, 205 },
    { "SlateGray4", 108, 123, 139 },
    { "SlateGrey", 112, 128, 144 },
    { "SpringGreen", 0, 255, 127 },
    { "SpringGreen1", 0, 255, 127 },
    { "SpringGreen2", 0, 238, 118 },
    { "SpringGreen3", 0, 205, 102 },
    { "SpringGreen4", 0, 139, 69 },
    { "SteelBlue", 70, 130, 180 },
    { "SteelBlue1", 99, 184, 255 },
    { "SteelBlue2", 92, 172, 238 },
    { "SteelBlue3", 79, 148, 205 },
    { "SteelBlue4", 54, 100, 139 },
    { "VioletRed", 208, 32, 144 },
    { "VioletRed1", 255, 62, 150 },
    { "VioletRed2", 238, 58, 140 },
    { "VioletRed3", 205, 50, 120 },
    { "VioletRed4", 139, 34, 82 },
    { "WhiteSmoke", 245, 245, 245 },
    { "YellowGreen", 154, 205, 50 },
    { "alice blue", 240, 248, 255 },
    { "antique white", 250, 235, 215 },
    { "aquamarine", 127, 255, 212 },
    { "aquamarine1", 127, 255, 212 },
    { "aquamarine2", 118, 238, 198 },
    { "aquamarine3", 102, 205, 170 },
    { "aquamarine4", 69, 139, 116 },
    { "azure", 240, 255, 255 },
    { "azure1", 240, 255, 255 },
    { "azure2", 224, 238, 238 },
    { "azure3", 193, 205, 205 },
    { "azure4", 131, 139, 139 },
    { "beige", 245, 245, 220 },
    { "bisque", 255, 228, 196 },
    { "bisque1", 255, 228, 196 },
    { "bisque2", 238, 213, 183 },
    { "bisque3", 205, 183, 158 },
    { "bisque4", 139, 125, 107 },
    { "black", 0, 0, 0 },
    { "blanched almond", 255, 235, 205 },
    { "blue", 0, 0, 255 },
    { "blue violet", 138, 43, 226 },
    { "blue1", 0, 0, 255 },
    { "blue2", 0, 0, 238 },
    { "blue3", 0, 0, 205 },
    { "blue4", 0, 0, 139 },
    { "brown", 165, 42, 42 },
    { "brown1", 255, 64, 64 },
    { "brown2", 238, 59, 59 },
    { "brown3", 205, 51, 51 },
    { "brown4", 139, 35, 35 },
    { "burlywood", 222, 184, 135 },
    { "burlywood1", 255, 211, 155 },
    { "burlywood2", 238, 197, 145 },
    { "burlywood3", 205, 170, 125 },
    { "burlywood4", 139, 115, 85 },
    { "cadet blue", 95, 158, 160 },
    { "chartreuse", 127, 255, 0 },
    { "chartreuse1", 127, 255, 0 },
    { "chartreuse2", 118, 238, 0 },
    { "chartreuse3", 102, 205, 0 },
    { "chartreuse4", 69, 139, 0 },
    { "chocolate", 210, 105, 30 },
    { "chocolate1", 255, 127, 36 },
    { "chocolate2", 238, 118, 33 },
    { "chocolate3", 205, 102, 29 },
    { "chocolate4", 139, 69, 19 },
    { "coral", 255, 127, 80 },
    { "coral1", 255, 114, 86 },
    { "coral2", 238, 106, 80 },
    { "coral3", 205, 91, 69 },
    { "coral4", 139, 62, 47 },
    { "cornflower blue", 100, 149, 237 },
    { "cornsilk", 255, 248, 220 },
    { "cornsilk1", 255, 248, 220 },
    { "cornsilk2", 238, 232, 205 },
    { "cornsilk3", 205, 200, 177 },
    { "cornsilk4", 139, 136, 120 },
    { "cyan", 0, 255, 255 },
    { "cyan1", 0, 255, 255 },
    { "cyan2", 0, 238, 238 },
    { "cyan3", 0, 205, 205 },
    { "cyan4", 0, 139, 139 },
    { "dark blue", 0, 0, 139 },
    { "dark cyan", 0, 139, 139 },
    { "dark goldenrod", 184, 134, 11 },
    { "dark gray", 169, 169, 169 },
    { "dark green", 0, 100, 0 },
    { "dark grey", 169, 169, 169 },
    { "dark khaki", 189, 183, 107 },
    { "dark magenta", 139, 0, 139 },
    { "dark olive green", 85, 107, 47 },
    { "dark orange", 255, 140, 0 },
    { "dark orchid", 153, 50, 204 },
    { "dark red", 139, 0, 0 },
    { "dark salmon", 233, 150, 122 },
    { "dark sea green", 143, 188, 143 },
    { "dark slate blue", 72, 61, 139 },
    { "dark slate gray", 47, 79, 79 },
    { "dark slate grey", 47, 79, 79 },
    { "dark turquoise", 0, 206, 209 },
    { "dark violet", 148, 0, 211 },
    { "deep pink", 255, 20, 147 },
    { "deep sky blue", 0, 191, 255 },
    { "dim gray", 105, 105, 105 },
    { "dim grey", 105, 105, 105 },
    { "dodger blue", 30, 144, 255 },
    { "firebrick", 178, 34, 34 },
    { "firebrick1", 255, 48, 48 },
    { "firebrick2", 238, 44, 44 },
    { "firebrick3", 205, 38, 38 },
    { "firebrick4", 139, 26, 26 },
    { "floral white", 255, 250, 240 },
    { "forest green", 34, 139, 34 },
    { "fractal", 128, 128, 128 },
    { "gainsboro", 220, 220, 220 },
    { "ghost white", 248, 248, 255 },
    { "gold", 255, 215, 0 },
    { "gold1", 255, 215, 0 },
    { "gold2", 238, 201, 0 },
    { "gold3", 205, 173, 0 },
    { "gold4", 139, 117, 0 },
    { "goldenrod", 218, 165, 32 },
    { "goldenrod1", 255, 193, 37 },
    { "goldenrod2", 238, 180, 34 },
    { "goldenrod3", 205, 155, 29 },
    { "goldenrod4", 139, 105, 20 },
    { "gray", 190, 190, 190 },
    { "green", 0, 255, 0 },
    { "green yellow", 173, 255, 47 },
    { "green1", 0, 255, 0 },
    { "green2", 0, 238, 0 },
    { "green3", 0, 205, 0 },
    { "green4", 0, 139, 0 },
    { "grey", 190, 190, 190 },
    { "honeydew", 240, 255, 240 },
    { "honeydew1", 240, 255, 240 },
    { "honeydew2", 224, 238, 224 },
    { "honeydew3", 193, 205, 193 },
    { "honeydew4", 131, 139, 131 },
    { "hot pink", 255, 105, 180 },
    { "indian red", 205, 92, 92 },
    { "ivory", 255, 255, 240 },
    { "ivory1", 255, 255, 240 },
    { "ivory2", 238, 238, 224 },
    { "ivory3", 205, 205, 193 },
    { "ivory4", 139, 139, 131 },
    { "khaki", 240, 230, 140 },
    { "khaki1", 255, 246, 143 },
    { "khaki2", 238, 230, 133 },
    { "khaki3", 205, 198, 115 },
    { "khaki4", 139, 134, 78 },
    { "lavender", 230, 230, 250 },
    { "lavender blush", 255, 240, 245 },
    { "lawn green", 124, 252, 0 },
    { "lemon chiffon", 255, 250, 205 },
    { "light blue", 173, 216, 230 },
    { "light coral", 240, 128, 128 },
    { "light cyan", 224, 255, 255 },
    { "light goldenrod", 238, 221, 130 },
    { "light goldenrod yellow", 250, 250, 210 },
    { "light gray", 211, 211, 211 },
    { "light green", 144, 238, 144 },
    { "light grey", 211, 211, 211 },
    { "light pink", 255, 182, 193 },
    { "light salmon", 255, 160, 122 },
    { "light sea green", 32, 178, 170 },
    { "light sky blue", 135, 206, 250 },
    { "light slate blue", 132, 112, 255 },
    { "light slate gray", 119, 136, 153 },
    { "light slate grey", 119, 136, 153 },
    { "light steel blue", 176, 196, 222 },
    { "light yellow", 255, 255, 224 },
    { "lime green", 50, 205, 50 },
    { "linen", 250, 240, 230 },
    { "magenta", 255, 0, 255 },
    { "magenta1", 255, 0, 255 },
    { "magenta2", 238, 0, 238 },
    { "magenta3", 205, 0, 205 },
    { "magenta4", 139, 0, 139 },
    { "maroon", 176, 48, 96 },
    { "maroon1", 255, 52, 179 },
    { "maroon2", 238, 48, 167 },
    { "maroon3", 205, 41, 144 },
    { "maroon4", 139, 28, 98 },
    { "medium aquamarine", 102, 205, 170 },
    { "medium blue", 0, 0, 205 },
    { "medium orchid", 186, 85, 211 },
    { "medium purple", 147, 112, 219 },
    { "medium sea green", 60, 179, 113 },
    { "medium slate blue", 123, 104, 238 },
    { "medium spring green", 0, 250, 154 },
    { "medium turquoise", 72, 209, 204 },
    { "medium violet red", 199, 21, 133 },
    { "midnight blue", 25, 25, 112 },
    { "mint cream", 245, 255, 250 },
    { "misty rose", 255, 228, 225 },
    { "moccasin", 255, 228, 181 },
    { "navajo white", 255, 222, 173 },
    { "navy", 0, 0, 128 },
    { "navy blue", 0, 0, 128 },
    { "old lace", 253, 245, 230 },
    { "olive drab", 107, 142, 35 },
    { "orange", 255, 165, 0 },
    { "orange red", 255, 69, 0 },
    { "orange1", 255, 165, 0 },
    { "orange2", 238, 154, 0 },
    { "orange3", 205, 133, 0 },
    { "orange4", 139, 90, 0 },
    { "orchid", 218, 112, 214 },
    { "orchid1", 255, 131, 250 },
    { "orchid2", 238, 122, 233 },
    { "orchid3", 205, 105, 201 },
    { "orchid4", 139, 71, 137 },
    { "pale goldenrod", 238, 232, 170 },
    { "pale green", 152, 251, 152 },
    { "pale turquoise", 175, 238, 238 },
    { "pale violet red", 219, 112, 147 },
    { "papaya whip", 255, 239, 213 },
    { "peach puff", 255, 218, 185 },
    { "peru", 205, 133, 63 },
    { "pink", 255, 192, 203 },
    { "pink1", 255, 181, 197 },
    { "pink2", 238, 169, 184 },
    { "pink3", 205, 145, 158 },
    { "pink4", 139, 99, 108 },
    { "plum", 221, 160, 221 },
    { "plum1", 255, 187, 255 },
    { "plum2", 238, 174, 238 },
    { "plum3", 205, 150, 205 },
    { "plum4", 139, 102, 139 },
    { "powder blue", 176, 224, 230 },
    { "purple", 160, 32, 240 },
    { "purple1", 155, 48, 255 },
    { "purple2", 145, 44, 238 },
    { "purple3", 125, 38, 205 },
    { "purple4", 85, 26, 139 },
    { "red", 255, 0, 0 },
    { "red1", 255, 0, 0 },
    { "red2", 238, 0, 0 },
    { "red3", 205, 0, 0 },
    { "red4", 139, 0, 0 },
    { "rosy brown", 188, 143, 143 },
    { "royal blue", 65, 105, 225 },
    { "saddle brown", 139, 69, 19 },
    { "salmon", 250, 128, 114 },
    { "salmon1", 255, 140, 105 },
    { "salmon2", 238, 130, 98 },
    { "salmon3", 205, 112, 84 },
    { "salmon4", 139, 76, 57 },
    { "sandy brown", 244, 164, 96 },
    { "sea green", 46, 139, 87 },
    { "seashell", 255, 245, 238 },
    { "seashell1", 255, 245, 238 },
    { "seashell2", 238, 229, 222 },
    { "seashell3", 205, 197, 191 },
    { "seashell4", 139, 134, 130 },
    { "sienna", 160, 82, 45 },
    { "sienna1", 255, 130, 71 },
    { "sienna2", 238, 121, 66 },
    { "sienna3", 205, 104, 57 },
    { "sienna4", 139, 71, 38 },
    { "sky blue", 135, 206, 235 },
    { "slate blue", 106, 90, 205 },
    { "slate gray", 112, 128, 144 },
    { "slate grey", 112, 128, 144 },
    { "snow", 255, 250, 250 },
    { "snow1", 255, 250, 250 },
    { "snow2", 238, 233, 233 },
    { "snow3", 205, 201, 201 },
    { "snow4", 139, 137, 137 },
    { "spring green", 0, 255, 127 },
    { "steel blue", 70, 130, 180 },
    { "tan", 210, 180, 140 },
    { "tan1", 255, 165, 79 },
    { "tan2", 238, 154, 73 },
    { "tan3", 205, 133, 63 },
    { "tan4", 139, 90, 43 },
    { "thistle", 216, 191, 216 },
    { "thistle1", 255, 225, 255 },
    { "thistle2", 238, 210, 238 },
    { "thistle3", 205, 181, 205 },
    { "thistle4", 139, 123, 139 },
    { "tomato", 255, 99, 71 },
    { "tomato1", 255, 99, 71 },
    { "tomato2", 238, 92, 66 },
    { "tomato3", 205, 79, 57 },
    { "tomato4", 139, 54, 38 },
    { "turquoise", 64, 224, 208 },
    { "turquoise1", 0, 245, 255 },
    { "turquoise2", 0, 229, 238 },
    { "turquoise3", 0, 197, 205 },
    { "turquoise4", 0, 134, 139 },
    { "violet", 238, 130, 238 },
    { "violet red", 208, 32, 144 },
    { "wheat", 245, 222, 179 },
    { "wheat1", 255, 231, 186 },
    { "wheat2", 238, 216, 174 },
    { "wheat3", 205, 186, 150 },
    { "wheat4", 139, 126, 102 },
    { "white", 255, 255, 255 },
    { "white smoke", 245, 245, 245 },
    { "yellow", 255, 255, 0 },
    { "yellow green", 154, 205, 50 },
    { "yellow1", 255, 255, 0 },
    { "yellow2", 238, 238, 0 },
    { "yellow3", 205, 205, 0 },
    { "yellow4", 139, 139, 0 },
    { "gray0", 0, 0, 0 },
    { "gray1", 3, 3, 3 },
    { "gray10", 26, 26, 26 },
    { "gray100", 255, 255, 255 },
    { "gray11", 28, 28, 28 },
    { "gray12", 31, 31, 31 },
    { "gray13", 33, 33, 33 },
    { "gray14", 36, 36, 36 },
    { "gray15", 38, 38, 38 },
    { "gray16", 41, 41, 41 },
    { "gray17", 43, 43, 43 },
    { "gray18", 46, 46, 46 },
    { "gray19", 48, 48, 48 },
    { "gray2", 5, 5, 5 },
    { "gray20", 51, 51, 51 },
    { "gray21", 54, 54, 54 },
    { "gray22", 56, 56, 56 },
    { "gray23", 59, 59, 59 },
    { "gray24", 61, 61, 61 },
    { "gray25", 64, 64, 64 },
    { "gray26", 66, 66, 66 },
    { "gray27", 69, 69, 69 },
    { "gray28", 71, 71, 71 },
    { "gray29", 74, 74, 74 },
    { "gray3", 8, 8, 8 },
    { "gray30", 77, 77, 77 },
    { "gray31", 79, 79, 79 },
    { "gray32", 82, 82, 82 },
    { "gray33", 84, 84, 84 },
    { "gray34", 87, 87, 87 },
    { "gray35", 89, 89, 89 },
    { "gray36", 92, 92, 92 },
    { "gray37", 94, 94, 94 },
    { "gray38", 97, 97, 97 },
    { "gray39", 99, 99, 99 },
    { "gray4", 10, 10, 10 },
    { "gray40", 102, 102, 102 },
    { "gray41", 105, 105, 105 },
    { "gray42", 107, 107, 107 },
    { "gray43", 110, 110, 110 },
    { "gray44", 112, 112, 112 },
    { "gray45", 115, 115, 115 },
    { "gray46", 117, 117, 117 },
    { "gray47", 120, 120, 120 },
    { "gray48", 122, 122, 122 },
    { "gray49", 125, 125, 125 },
    { "gray5", 13, 13, 13 },
    { "gray50", 127, 127, 127 },
    { "gray51", 130, 130, 130 },
    { "gray52", 133, 133, 133 },
    { "gray53", 135, 135, 135 },
    { "gray54", 138, 138, 138 },
    { "gray55", 140, 140, 140 },
    { "gray56", 143, 143, 143 },
    { "gray57", 145, 145, 145 },
    { "gray58", 148, 148, 148 },
    { "gray59", 150, 150, 150 },
    { "gray6", 15, 15, 15 },
    { "gray60", 153, 153, 153 },
    { "gray61", 156, 156, 156 },
    { "gray62", 158, 158, 158 },
    { "gray63", 161, 161, 161 },
    { "gray64", 163, 163, 163 },
    { "gray65", 166, 166, 166 },
    { "gray66", 168, 168, 168 },
    { "gray67", 171, 171, 171 },
    { "gray68", 173, 173, 173 },
    { "gray69", 176, 176, 176 },
    { "gray7", 18, 18, 18 },
    { "gray70", 179, 179, 179 },
    { "gray71", 181, 181, 181 },
    { "gray72", 184, 184, 184 },
    { "gray73", 186, 186, 186 },
    { "gray74", 189, 189, 189 },
    { "gray75", 191, 191, 191 },
    { "gray76", 194, 194, 194 },
    { "gray77", 196, 196, 196 },
    { "gray78", 199, 199, 199 },
    { "gray79", 201, 201, 201 },
    { "gray8", 20, 20, 20 },
    { "gray80", 204, 204, 204 },
    { "gray81", 207, 207, 207 },
    { "gray82", 209, 209, 209 },
    { "gray83", 212, 212, 212 },
    { "gray84", 214, 214, 214 },
    { "gray85", 217, 217, 217 },
    { "gray86", 219, 219, 219 },
    { "gray87", 222, 222, 222 },
    { "gray88", 224, 224, 224 },
    { "gray89", 227, 227, 227 },
    { "gray9", 23, 23, 23 },
    { "gray90", 229, 229, 229 },
    { "gray91", 232, 232, 232 },
    { "gray92", 235, 235, 235 },
    { "gray93", 237, 237, 237 },
    { "gray94", 240, 240, 240 },
    { "gray95", 242, 242, 242 },
    { "gray96", 245, 245, 245 },
    { "gray97", 247, 247, 247 },
    { "gray98", 250, 250, 250 },
    { "gray99", 252, 252, 252 },
    { "grey0", 0, 0, 0 },
    { "grey1", 3, 3, 3 },
    { "grey10", 26, 26, 26 },
    { "grey100", 255, 255, 255 },
    { "grey11", 28, 28, 28 },
    { "grey12", 31, 31, 31 },
    { "grey13", 33, 33, 33 },
    { "grey14", 36, 36, 36 },
    { "grey15", 38, 38, 38 },
    { "grey16", 41, 41, 41 },
    { "grey17", 43, 43, 43 },
    { "grey18", 46, 46, 46 },
    { "grey19", 48, 48, 48 },
    { "grey2", 5, 5, 5 },
    { "grey20", 51, 51, 51 },
    { "grey21", 54, 54, 54 },
    { "grey22", 56, 56, 56 },
    { "grey23", 59, 59, 59 },
    { "grey24", 61, 61, 61 },
    { "grey25", 64, 64, 64 },
    { "grey26", 66, 66, 66 },
    { "grey27", 69, 69, 69 },
    { "grey28", 71, 71, 71 },
    { "grey29", 74, 74, 74 },
    { "grey3", 8, 8, 8 },
    { "grey30", 77, 77, 77 },
    { "grey31", 79, 79, 79 },
    { "grey32", 82, 82, 82 },
    { "grey33", 84, 84, 84 },
    { "grey34", 87, 87, 87 },
    { "grey35", 89, 89, 89 },
    { "grey36", 92, 92, 92 },
    { "grey37", 94, 94, 94 },
    { "grey38", 97, 97, 97 },
    { "grey39", 99, 99, 99 },
    { "grey4", 10, 10, 10 },
    { "grey40", 102, 102, 102 },
    { "grey41", 105, 105, 105 },
    { "grey42", 107, 107, 107 },
    { "grey43", 110, 110, 110 },
    { "grey44", 112, 112, 112 },
    { "grey45", 115, 115, 115 },
    { "grey46", 117, 117, 117 },
    { "grey47", 120, 120, 120 },
    { "grey48", 122, 122, 122 },
    { "grey49", 125, 125, 125 },
    { "grey5", 13, 13, 13 },
    { "grey50", 127, 127, 127 },
    { "grey51", 130, 130, 130 },
    { "grey52", 133, 133, 133 },
    { "grey53", 135, 135, 135 },
    { "grey54", 138, 138, 138 },
    { "grey55", 140, 140, 140 },
    { "grey56", 143, 143, 143 },
    { "grey57", 145, 145, 145 },
    { "grey58", 148, 148, 148 },
    { "grey59", 150, 150, 150 },
    { "grey6", 15, 15, 15 },
    { "grey60", 153, 153, 153 },
    { "grey61", 156, 156, 156 },
    { "grey62", 158, 158, 158 },
    { "grey63", 161, 161, 161 },
    { "grey64", 163, 163, 163 },
    { "grey65", 166, 166, 166 },
    { "grey66", 168, 168, 168 },
    { "grey67", 171, 171, 171 },
    { "grey68", 173, 173, 173 },
    { "grey69", 176, 176, 176 },
    { "grey7", 18, 18, 18 },
    { "grey70", 179, 179, 179 },
    { "grey71", 181, 181, 181 },
    { "grey72", 184, 184, 184 },
    { "grey73", 186, 186, 186 },
    { "grey74", 189, 189, 189 },
    { "grey75", 191, 191, 191 },
    { "grey76", 194, 194, 194 },
    { "grey77", 196, 196, 196 },
    { "grey78", 199, 199, 199 },
    { "grey79", 201, 201, 201 },
    { "grey8", 20, 20, 20 },
    { "grey80", 204, 204, 204 },
    { "grey81", 207, 207, 207 },
    { "grey82", 209, 209, 209 },
    { "grey83", 212, 212, 212 },
    { "grey84", 214, 214, 214 },
    { "grey85", 217, 217, 217 },
    { "grey86", 219, 219, 219 },
    { "grey87", 222, 222, 222 },
    { "grey88", 224, 224, 224 },
    { "grey89", 227, 227, 227 },
    { "grey9", 23, 23, 23 },
    { "grey90", 229, 229, 229 },
    { "grey91", 232, 232, 232 },
    { "grey92", 235, 235, 235 },
    { "grey93", 237, 237, 237 },
    { "grey94", 240, 240, 240 },
    { "grey95", 242, 242, 242 },
    { "grey96", 245, 245, 245 },
    { "grey97", 247, 247, 247 },
    { "grey98", 250, 250, 250 },
    { "grey99", 252, 252, 252 },
    { (char *) NULL, 0, 0, 0 }
  };

/*
  Forward declarations.
*/
static NodeInfo
  *InitializeNode(CubeInfo *,const unsigned int);

static void
  Histogram(CubeInfo *,const NodeInfo *,FILE *);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p r e s s C o l o r m a p                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CompressColormap compresses an image colormap removing any
%  duplicate and unused color entries.
%
%  The format of the CompressColormap method is:
%
%      void CompressColormap(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void CompressColormap(Image *image)
{
  ColorPacket
    *colormap;

  int
    number_colors;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  /*
    Determine if colormap can be compressed.
  */
  assert(image != (Image *) NULL);
  if (image->class != PseudoClass)
    return;
  number_colors=image->colors;
  for (i=0; i < (int) image->colors; i++)
    image->colormap[i].flags=False;
  image->colors=0;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    if (!image->colormap[p->index].flags)
      {
        /*
          Eliminate duplicate colors.
        */
        for (j=0; j < number_colors; j++)
          if ((j != p->index) && image->colormap[j].flags)
            if (ColorMatch(image->colormap[p->index],image->colormap[j],0))
              break;
        if (j != number_colors)
          image->colormap[p->index].index=image->colormap[j].index;
        else
          image->colormap[p->index].index=image->colors++;
        image->colormap[p->index].flags=True;
      }
    p++;
  }
  if ((int) image->colors == number_colors)
    return;  /* no duplicate or unused entries */
  /*
    Compress colormap.
  */
  colormap=(ColorPacket *) AllocateMemory(image->colors*sizeof(ColorPacket));
  if (colormap == (ColorPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to compress colormap",
        "Memory allocation failed");
      image->colors=number_colors;
      return;
    }
  /*
    Eliminate unused colormap entries.
  */
  for (i=0; i < number_colors; i++)
    if (image->colormap[i].flags)
      {
        index=image->colormap[i].index;
        colormap[index].red=image->colormap[i].red;
        colormap[index].green=image->colormap[i].green;
        colormap[index].blue=image->colormap[i].blue;
      }
  /*
    Remap pixels.
  */
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    p->index=image->colormap[p->index].index;
    p++;
  }
  FreeMemory((char *) image->colormap);
  image->colormap=colormap;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  D e s t r o y L i s t                                                      %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyList traverses the color cube tree and free the list of
%  unique colors.
%
%  The format of the DestroyList method is:
%
%      void DestroyList(const NodeInfo *node_info)
%
%  A description of each parameter follows.
%
%    o node_info: The address of a structure of type NodeInfo which points to a
%      node in the color cube tree that is to be pruned.
%
%
*/
static void DestroyList(const NodeInfo *node_info)
{
  register unsigned int
    id;

  /*
    Traverse any children.
  */
  for (id=0; id < 8; id++)
    if (node_info->child[id] != (NodeInfo *) NULL)
      DestroyList(node_info->child[id]);
  if (node_info->level == MaxTreeDepth)
    if (node_info->list != (ColorPacket *) NULL)
      FreeMemory((char *) node_info->list);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  G e t N u m b e r C o l o r s                                              %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetNumberColors returns the number of unique colors in an image.
%
%  The format of the GetNumberColors method is:
%
%      number_colors=GetNumberColors(image,file)
%
%  A description of each parameter follows.
%
%    o number_colors: Method GetNumberColors returns the number of unique
%      colors in the specified image.
%
%    o image: The address of a byte (8 bits) array of run-length
%      encoded pixel data of your source image.  The sum of the
%      run-length counts in the source image must be equal to or exceed
%      the number of pixels.
%
%    o file:  An pointer to a FILE.  If it is non-null a list of unique pixel
%      field values and the number of times each occurs in the image is
%      written to the file.
%
%
%
*/
Export unsigned long GetNumberColors(const Image *image,FILE *file)
{
#define NumberColorsImageText  "  Computing image colors...  "

  CubeInfo
    color_cube;

  NodeInfo
    *node_info;

  Nodes
    *nodes;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned int
    id,
    index,
    level;

  /*
    Initialize color description tree.
  */
  assert(image != (Image *) NULL);
  color_cube.node_list=(Nodes *) NULL;
  color_cube.progress=0;
  color_cube.colors=0;
  color_cube.free_nodes=0;
  color_cube.root=InitializeNode(&color_cube,0);
  if (color_cube.root == (NodeInfo *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to determine image class",
        "Memory allocation failed");
      return(0);
    }
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    /*
      Start at the root and proceed level by level.
    */
    node_info=color_cube.root;
    index=MaxTreeDepth-1;
    for (level=1; level <= MaxTreeDepth; level++)
    {
      id=(((Quantum) DownScale(p->red) >> index) & 0x01) << 2 |
         (((Quantum) DownScale(p->green) >> index) & 0x01) << 1 |
         (((Quantum) DownScale(p->blue) >> index) & 0x01);
      if (node_info->child[id] == (NodeInfo *) NULL)
        {
          node_info->child[id]=InitializeNode(&color_cube,level);
          if (node_info->child[id] == (NodeInfo *) NULL)
            {
              MagickWarning(ResourceLimitWarning,"Unable to determine image class",
                "Memory allocation failed");
              return(0);
            }
        }
      node_info=node_info->child[id];
      index--;
      if (level != MaxTreeDepth)
        continue;
      for (j=0; j < (int) node_info->number_unique; j++)
         if (ColorMatch(*p,node_info->list[j],0))
           break;
      if (j < (int) node_info->number_unique)
        {
          node_info->list[j].count+=p->length+1;
          continue;
        }
      if (node_info->number_unique == 0)
        node_info->list=(ColorPacket *) AllocateMemory(sizeof(ColorPacket));
      else
        node_info->list=(ColorPacket *)
          ReallocateMemory(node_info->list,(j+1)*sizeof(ColorPacket));
      if (node_info->list == (ColorPacket *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to determine image class",
            "Memory allocation failed");
          return(0);
        }
      node_info->list[j].red=p->red;
      node_info->list[j].green=p->green;
      node_info->list[j].blue=p->blue;
      node_info->list[j].count=p->length+1;
      node_info->number_unique++;
      color_cube.colors++;
    }
    p++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(NumberColorsImageText,i,image->packets);
  }
  if (file != (FILE *) NULL)
    {
      Histogram(&color_cube,color_cube.root,file);
      (void) fflush(file);
    }
  /*
    Release color cube tree storage.
  */
  DestroyList(color_cube.root);
  do
  {
    nodes=color_cube.node_list->next;
    FreeMemory((char *) color_cube.node_list);
    color_cube.node_list=nodes;
  }
  while (color_cube.node_list != (Nodes *) NULL);
  return(color_cube.colors);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  H i s t o g r a m                                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method Histogram traverses the color cube tree and produces a list of
%  unique pixel field values and the number of times each occurs in the image.
%
%  The format of the Histogram method is:
%
%      void Histogram(CubeInfo *color_cube,const NodeInfo *node_info,
%        FILE *file)
%
%  A description of each parameter follows.
%
%    o color_cube: A pointer to the CubeInfo structure.
%
%    o node_info: The address of a structure of type NodeInfo which points to a
%      node in the color cube tree that is to be pruned.
%
%
*/
static void Histogram(CubeInfo *color_cube,const NodeInfo *node_info,FILE *file)
{
#define HistogramImageText  "  Computing image histogram...  "

  register unsigned int
    id;

  /*
    Traverse any children.
  */
  for (id=0; id < 8; id++)
    if (node_info->child[id] != (NodeInfo *) NULL)
      Histogram(color_cube,node_info->child[id],file);
  if (node_info->level == MaxTreeDepth)
    {
      char
        name[MaxTextExtent];

      register ColorPacket
        *p;

      register int
        i;

      p=node_info->list;
      for (i=0; i < (int) node_info->number_unique; i++)
      {
#if (QuantumDepth == 8)
        (void) fprintf(file,"%10lu: (%3d,%3d,%3d)  #%02x%02x%02x",
#else
        (void) fprintf(file,"%10lu: (%5d,%5d,%5d)  #%04x%04x%04x",
#endif
          p->count,p->red,p->green,p->blue,(unsigned int) p->red,
          (unsigned int) p->green,(unsigned int) p->blue);
        (void) fprintf(file,"  ");
        (void) QueryColorName(p,name);
        (void) fprintf(file,"%.1024s",name);
        (void) fprintf(file,"\n");
        p++;
      }
      if (QuantumTick(color_cube->progress,color_cube->colors))
        ProgressMonitor(HistogramImageText,color_cube->progress,
          color_cube->colors);
      color_cube->progress++;
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  I n i t i a l i z e N o d e                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method InitializeNode allocates memory for a new node in the color cube
%  tree and presets all fields to zero.
%
%  The format of the InitializeNode method is:
%
+      node_info=InitializeNode(color_cube,level)
%
%  A description of each parameter follows.
%
%    o color_cube: A pointer to the CubeInfo structure.
%
%    o level: Specifies the level in the classification the node resides.
%
%
*/
static NodeInfo *InitializeNode(CubeInfo *color_cube,const unsigned int level)
{
  register int
    i;

  NodeInfo
    *node_info;

  if (color_cube->free_nodes == 0)
    {
      Nodes
        *nodes;

      /*
        Allocate a new nodes of nodes.
      */
      nodes=(Nodes *) AllocateMemory(sizeof(Nodes));
      if (nodes == (Nodes *) NULL)
        return((NodeInfo *) NULL);
      nodes->next=color_cube->node_list;
      color_cube->node_list=nodes;
      color_cube->node_info=nodes->nodes;
      color_cube->free_nodes=NodesInAList;
    }
  color_cube->free_nodes--;
  node_info=color_cube->node_info++;
  for (i=0; i < 8; i++)
    node_info->child[i]=(NodeInfo *) NULL;
  node_info->level=level;
  node_info->number_unique=0;
  node_info->list=(ColorPacket *) NULL;
  return(node_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s G r a y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsGrayImage returns True if the image is grayscale otherwise
%  False is returned.  If the image is DirectClass and grayscale, it is demoted
%  to PseudoClass.
%
%  The format of the IsGrayImage method is:
%
%      unsigned int IsGrayImage(Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method IsGrayImage returns True if the image is grayscale
%      otherwise False is returned.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export unsigned int IsGrayImage(Image *image)
{
  register int
    i;

  /*
    Determine if image is grayscale.
  */
  assert(image != (Image *) NULL);
  if (!IsPseudoClass(image))
    return(False);
  for (i=0; i < (int) image->colors; i++)
    if (!IsGray(image->colormap[i]))
      return(False);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%   I s M o n o c h r o m e I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsMonochromeImage returns True if the image is monochrome otherwise
%  False is returned.  If the image is DirectClass and monochrome, it is
%  demoted to PseudoClass.
%
%  The format of the IsMonochromeImage method is:
%
%      status=IsMonochromeImage(image)
%
%  A description of each parameter follows:
%
%    o status: Method IsMonochromeImage returns True if the image is
%      monochrome otherwise False is returned.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export unsigned int IsMonochromeImage(Image *image)
{
  /*
    Determine if image is monochrome.
  */
  assert(image != (Image *) NULL);
  if (image->pixels == (RunlengthPacket *) NULL)
    return(False);
  if (!IsGrayImage(image))
    return(False);
  if (image->colors > 2)
    return(False);
  if ((Intensity(image->colormap[0]) != 0) &&
      (Intensity(image->colormap[0]) != MaxRGB))
    return(False);
  if (image->colors == 2)
    if ((Intensity(image->colormap[1]) != 0) &&
        (Intensity(image->colormap[1]) != MaxRGB))
      return(False);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  I s P s e u d o C l a s s                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsPseudoClass returns True if the image is PseudoClass and has 256
%  unique colors or less.  If the image is DirectClass and has 256 colors
%  or less, the image is demoted to PseudoClass.
%
%  The format of the IsPseudoClass method is:
%
%      unsigned int IsPseudoClass(Image *image)
%
%  A description of each parameter follows.
%
%    o status:  Method IsPseudoClass returns True is the image is
%      PseudoClass or has 256 color or less.
%
%    o image: The address of a byte (8 bits) array of run-length
%      encoded pixel data of your source image.  The sum of the
%      run-length counts in the source image must be equal to or exceed
%      the number of pixels.
%
%
*/
Export unsigned int IsPseudoClass(Image *image)
{
  CubeInfo
    color_cube;

  Nodes
    *nodes;

  register int
    i,
    j;

  register NodeInfo
    *node_info;

  register RunlengthPacket
    *p;

  register unsigned int
    index,
    level;

  unsigned int
    id;

  assert(image != (Image *) NULL);
  if ((image->class == PseudoClass) && (image->colors <= 256))
    return(True);
  if (image->matte)
    return(False);
  if (image->colorspace == CMYKColorspace)
    return(False);
  /*
    Initialize color description tree.
  */
  color_cube.node_list=(Nodes *) NULL;
  color_cube.colors=0;
  color_cube.free_nodes=0;
  color_cube.root=InitializeNode(&color_cube,0);
  if (color_cube.root == (NodeInfo *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to determine image class",
        "Memory allocation failed");
      return(False);
    }
  p=image->pixels;
  for (i=0; (i < (int) image->packets) && (color_cube.colors <= 256); i++)
  {
    /*
      Start at the root and proceed level by level.
    */
    node_info=color_cube.root;
    index=MaxTreeDepth-1;
    for (level=1; level < MaxTreeDepth; level++)
    {
      id=((DownScale(p->red) >> index) & 0x01) << 2 |
         ((DownScale(p->green) >> index) & 0x01) << 1 |
         ((DownScale(p->blue) >> index) & 0x01);
      if (node_info->child[id] == (NodeInfo *) NULL)
        {
          node_info->child[id]=InitializeNode(&color_cube,level);
          if (node_info->child[id] == (NodeInfo *) NULL)
            {
              MagickWarning(ResourceLimitWarning,
                "Unable to determine image class","Memory allocation failed");
              return(False);
            }
        }
      node_info=node_info->child[id];
      index--;
    }
    for (j=0; j < (int) node_info->number_unique; j++)
      if (ColorMatch(*p,node_info->list[j],0))
        break;
    if (j == (int) node_info->number_unique)
      {
        /*
          Add this unique color to the color list.
        */
        if (node_info->number_unique == 0)
          node_info->list=(ColorPacket *) AllocateMemory(sizeof(ColorPacket));
        else
          node_info->list=(ColorPacket *)
            ReallocateMemory(node_info->list,(j+1)*sizeof(ColorPacket));
        if (node_info->list == (ColorPacket *) NULL)
          {
            MagickWarning(ResourceLimitWarning,
              "Unable to determine image class","Memory allocation failed");
            return(False);
          }
        node_info->list[j].red=p->red;
        node_info->list[j].green=p->green;
        node_info->list[j].blue=p->blue;
        node_info->list[j].index=color_cube.colors++;
        node_info->number_unique++;
      }
    p++;
  }
  if (color_cube.colors <= 256)
    {
      /*
        Create colormap.
      */
      image->class=PseudoClass;
      image->colors=color_cube.colors;
      if (image->colormap == (ColorPacket *) NULL)
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
      else
        image->colormap=(ColorPacket *) ReallocateMemory((char *)
          image->colormap,image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to determine image class",
            "Memory allocation failed");
          return(False);
        }
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        /*
          Start at the root and proceed level by level.
        */
        node_info=color_cube.root;
        index=MaxTreeDepth-1;
        for (level=1; level < MaxTreeDepth; level++)
        {
          id=((DownScale(p->red) >> index) & 0x01) << 2 |
             ((DownScale(p->green) >> index) & 0x01) << 1 |
             ((DownScale(p->blue) >> index) & 0x01);
          node_info=node_info->child[id];
          index--;
        }
        for (j=0; j < (int) node_info->number_unique; j++)
          if (ColorMatch(*p,node_info->list[j],0))
            break;
        p->index=node_info->list[j].index;
        image->colormap[p->index]=node_info->list[j];
        p++;
      }
    }
  /*
    Release color cube tree storage.
  */
  DestroyList(color_cube.root);
  do
  {
    nodes=color_cube.node_list->next;
    FreeMemory((char *) color_cube.node_list);
    color_cube.node_list=nodes;
  } while (color_cube.node_list != (Nodes *) NULL);
  return((image->class == PseudoClass) && (image->colors <= 256));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   Q u e r y C o l o r D a t a b a s e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method QueryColorDatabase looks up a RGB values for a color given in the
%  target string.
%
%  The format of the QueryColorDatabase method is:
%
%      unsigned int QueryColorDatabase(const char *target,ColorPacket *color)
%
%  A description of each parameter follows:
%
%    o status:  Method QueryColorDatabase returns True if the RGB values
%      of the target color is defined, otherwise False is returned.
%
%    o target: Specifies the color to lookup in the X color database.
%
%    o color: A pointer to an ColorPacket structure.  The RGB value of the target
%      color is returned as this value.
%
%
*/
Export unsigned int QueryColorDatabase(const char *target,ColorPacket *color)
{
  char
    colorname[MaxTextExtent],
    text[MaxTextExtent];

  int
    blue,
    count,
    green,
    index,
    red;

  register const ColorlistInfo
    *p;

  static FILE
    *database = (FILE *) NULL;

  /*
    Initialize color return value.
  */
  assert(color != (ColorPacket *) NULL);
  color->red=0;
  color->green=0;
  color->blue=0;
  color->index=0;
  color->flags=DoRed | DoGreen | DoBlue;
  if ((target == (char *) NULL) || (*target == '\0'))
    target=BackgroundColor;
  while (isspace((int) (*target)))
    target++;
  if (*target == '#')
    {
      char
        c;

      register int
        i;

      unsigned long
        n;

      green=0;
      blue=0;
      index=0;
      target++;
      n=Extent(target);
      if ((n == 3) || (n == 6) || (n == 9) || (n == 12))
        {
          /*
            Parse RGB specification.
          */
          n/=3;
          do
          {
            red=green;
            green=blue;
            blue=0;
            for (i=(int) n-1; i >= 0; i--)
            {
              c=(*target++);
              blue<<=4;
              if ((c >= '0') && (c <= '9'))
                blue|=c-'0';
              else
                if ((c >= 'A') && (c <= 'F'))
                  blue|=c-('A'-10);
                else
                  if ((c >= 'a') && (c <= 'f'))
                    blue|=c-('a'-10);
                  else
                    return(False);
            }
          } while (*target != '\0');
        }
      else
        if ((n != 4) && (n != 8) && (n != 16))
          return(False);
        else
          {
            /*
              Parse RGBA specification.
            */
            color->flags|=DoMatte;
            n/=4;
            do
            {
              red=green;
              green=blue;
              blue=index;
              index=0;
              for (i=(int) n-1; i >= 0; i--)
              {
                c=(*target++);
                index<<=4;
                if ((c >= '0') && (c <= '9'))
                  index|=c-'0';
                else
                  if ((c >= 'A') && (c <= 'F'))
                    index|=c-('A'-10);
                  else
                    if ((c >= 'a') && (c <= 'f'))
                      index|=c-('a'-10);
                    else
                      return(False);
              }
            } while (*target != '\0');
          }
      n<<=2;
      color->red=((unsigned long) (65535L*red)/((1 << n)-1));
      color->green=((unsigned long) (65535L*green)/((1 << n)-1));
      color->blue=((unsigned long) (65535L*blue)/((1 << n)-1));
      color->index=((unsigned long) (65535L*index)/((1 << n)-1));
      return(True);
    }
  if (database == (FILE *) NULL)
    database=fopen(RGBColorDatabase,"r");
  if (database != (FILE *) NULL)
    {
      /*
        Match color against the X color database.
      */
      (void) fseek(database,0L,SEEK_SET);
      while (fgets(text,MaxTextExtent,database) != (char *) NULL)
      {
        count=sscanf(text,"%d %d %d %[^\n]\n",&red,&green,&blue,colorname);
        if (count != 4)
          continue;
        if (Latin1Compare(colorname,target) == 0)
          {
            color->red=257*red;
            color->green=257*green;
            color->blue=257*blue;
            return(True);
          }
      }
    }
  /*
    Search our internal color database.
  */
  for (p=XColorlist; p->name != (char *) NULL; p++)
    if (Latin1Compare(p->name,target) == 0)
      {
        color->red=257*p->red;
        color->green=257*p->green;
        color->blue=257*p->blue;
        return(True);
      }
  /*
    Let the X server define the color for us.
  */
#if defined(HasX11)
  return(XQueryColorDatabase(target,color));
#else
  return(False);
#endif
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  Q u e r y C o l o r N a m e                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method QueryColorName returns the name of the color that is closest to the
%  supplied color in RGB space.
%
%  The format of the QueryColorName method is:
%
%      unsigned int QueryColorName(const ColorPacket *color,char *name)
%
%  A description of each parameter follows.
%
%    o distance: Method QueryColorName returns the distance-squared in RGB
%      space as well as the color name that is at a minimum distance.
%
%    o color: This is a pointer to a ColorPacket structure that contains the
%      color we are searching for.
%
%    o name: The name of the color that is closest to the supplied color is
%      returned in this character buffer.
%
%
*/
Export unsigned int QueryColorName(const ColorPacket *color,char *name)
{
  double
    min_distance;

  register const ColorlistInfo
    *p;

  register double
    distance_squared;

  register int
    distance;

  *name='\0';
  min_distance=0;
  for (p=XColorlist; p->name != (char *) NULL; p++)
  {
    distance=DownScale(color->red)-(int) p->red;
    distance_squared=distance*distance;
    distance=DownScale(color->green)-(int) p->green;
    distance_squared+=distance*distance;
    distance=DownScale(color->blue)-(int) p->blue;
    distance_squared+=distance*distance;
    if ((p == XColorlist) || (distance_squared < min_distance))
      {
        min_distance=distance_squared;
        (void) strcpy(name,p->name);
      }
  }
  if (min_distance != 0.0)
    FormatString(name,HexColorFormat,(unsigned int) color->red,
      (unsigned int) color->green,(unsigned int) color->blue);
  return((unsigned int) min_distance);
}
