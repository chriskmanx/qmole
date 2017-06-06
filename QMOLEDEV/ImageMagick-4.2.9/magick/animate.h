/*
  Methods to Interactively Animate an Image Sequence.
*/
#ifndef _ANIMATE_H
#define _ANIMATE_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  Animate state declarations.
*/
#define AutoReverseAnimationState 0x0004
#define ForwardAnimationState 0x0008
#define HighlightState  0x0010
#define PlayAnimationState 0x0020
#define RepeatAnimationState 0x0040
#define StepAnimationState 0x0080

/*
  Static declarations.
*/
const char
  *AnimateHelp[]=
  {
    "BUTTONS",
    "",
    "  Press any button to map or unmap the Command widget.",
    "",
    "COMMAND WIDGET",
    "  The Command widget lists a number of sub-menus and commands.",
    "  They are",
    "",
    "    Animate",
    "      Open",
    "      Play",
    "      Step",
    "      Repeat",
    "      Auto Reverse",
    "    Speed",
    "      Slower",
    "      Faster",
    "    Direction",
    "      Forward",
    "      Reverse",
    "      Help",
    "        Overview",
    "        Browse Documentation",
    "        About Animate",
    "    Image Info",
    "    Quit",
    "",
    "  Menu items with a indented triangle have a sub-menu.  They",
    "  are represented above as the indented items.  To access a",
    "  sub-menu item, move the pointer to the appropriate menu and",
    "  press a button and drag.  When you find the desired sub-menu",
    "  item, release the button and the command is executed.  Move",
    "  the pointer away from the sub-menu if you decide not to",
    "  execute a particular command.",
    "",
    "KEYBOARD ACCELERATORS",
    "  Accelerators are one or two key presses that effect a",
    "  particular command.  The keyboard accelerators that",
    "  animate(1) understands is:",
    "",
    "  Ctl+O  Press to open an image from a file.",
    "",
    "  space  Press to display the next image in the sequence.",
    "",
    "  <      Press to speed-up the display of the images.  Refer to",
    "         -delay for more information.",
    "",
    "  >      Press to slow the display of the images.  Refer to",
    "         -delay for more information.",
    "",
    "  F1     Press to display helpful information about animate(1).",
    "",
    "  Find   Press to browse documentation about ImageMagick.",
    "",
    "  ?      Press to display information about the image.  Press",
    "         any key or button to erase the information.",
    "",
    "         This information is printed: image name;  image size;",
    "         and the total number of unique colors in the image.",
    "",
    "  Ctl-q  Press to discard all images and exit program.",
    (char *) NULL
  };

/*
  Constant declarations.
*/
static const unsigned char
  HighlightBitmap[8] =
  {
    0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55
  },
  ShadowBitmap[8] =
  {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  };

/*
  Enumeration declarations.
*/
typedef enum
{
  OpenCommand,
  PlayCommand,
  StepCommand,
  RepeatCommand,
  AutoReverseCommand,
  SlowerCommand,
  FasterCommand,
  ForwardCommand,
  ReverseCommand,
  HelpCommand,
  BrowseDocumentationCommand,
  VersionCommand,
  InfoCommand,
  QuitCommand,
  StepBackwardCommand,
  StepForwardCommand,
  NullCommand
} CommandType;

/*
  Stipples.
*/
#define HighlightWidth  8
#define HighlightHeight  8
#define ShadowWidth  8
#define ShadowHeight  8

/*
  Function prototypes.
*/
static Image
  *XMagickCommand(Display *,XResourceInfo *,XWindows *,const CommandType,
    Image **,unsigned int *);

static int
  SceneCompare(const void *,const void *);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
