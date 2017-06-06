/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            X   X  TTTTT PPPP                                %
%                             X X     T   P   P                               %
%                              X      T   PPPP                                %
%                             X X     T   P                                   %
%                            X   X    T   P                                   %
%                                                                             %
%                                                                             %
%                         File transfer program.                              %
%                                                                             %
%                                                                             %
%                                                                             %
%                            Software Design                                  %
%                              John Cristy                                    %
%                             October 1992                                    %
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
%  Xtp is a utility for retrieving, listing, or printing files from a
%  remote network site.  Xtp performs most of the same functions as the
%  FTP program, but does not require any interactive commands.  You simply
%  specify the file transfer task on the command line and xtp performs the
%  transfer automatically.
%
%  This program was adapted from a similiar program written by Steve Singles,
%  University of Delaware.
%
%  Command syntax:
%
%  Usage: xtp [-options ...] < uniform resource locator >
%
%  Where options include:
%    -account password    supplemental password
%    -binary              retrieve files as binary
%    -exclude expression  exclude files that match the expression
%    -directory           list file names that match the expression
%    -file name           store the file with this name
%    -get                 get files that match the expression
%    -port number         port number of FTP server
%    -print               print files that match the expression
%    -proxy hostname      access remote host via this proxy host
%    -prune               do not recursively search for files
%    -put                 put files that match the expression
%    -retrieve            retrieve files that match the expression
%    -timeout seconds     specifies maximum seconds of XTP session
%
%
*/

/*
  Include declarations.
*/
#define __EXTENSIONS__  1
#include "xtp.h"
#include "regular.h"
#include <termios.h>
#include <fcntl.h>
#if defined(HAVE_PTMX)
#include <stropts.h>
#include <sys/socket.h>
#endif
#include <sys/stat.h>
#include <sys/wait.h>
/*
  Variable declarations.
*/
static char
  *client_name,
  slave_tty[16];

static int
  child,
  master,
  status;

static RegularExpression
  *directory_expression,
  *exclude_expression,
  *print_expression,
  *retrieve_expression;

/*
  External declarations.
*/
extern char
  *GetHostInfo(char *);

/*
  Forward declarations.
*/
static char
  *Wait(void);

static int
  MakeDirectory(char *);

static void
  DirectoryRequest(char *, char *),
  Error(char *,char *),
  ExecuteFtp(char *,char *),
  GetPseudoTerminal(void),
  PrintRequest(char *,unsigned int),
  ProcessRequest(char *,unsigned int,unsigned int),
  RetrieveRequest(char *,unsigned int),
  Usage(void);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D i r e c t o r y R e q u e s t                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DirectoryRequest lists a file name and its attributes.
%
%  The format of the DirectoryRequest routine is:
%
%    DirectoryRequest(fileinfo,filename)
%
%  A description of each parameter follows:
%
%    o fileinfo:  Specifies a pointer to a character array that contains
%      information about the file.
%
%    o filename:  Specifies a pointer to a character array that contains
%      the name of the file.
%
*/
static void DirectoryRequest(char *fileinfo,char *filename)
{
  register char
    *p;

  status=0;
  for (p=filename; *p != '\0'; p++)
    if (!isprint((int) *p))
      *p=' ';
  if (*fileinfo == '\0')
    (void) fprintf(stdout,"%s\n",filename);
  else
    (void) fprintf(stdout,"%s %s\n",fileinfo,filename);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E r r o r                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Error displays an error message and then terminates the program.
%
%  The format of the Error routine is:
%
%      Error(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
static void Error(char *message,char *qualifier)
{
  (void) fprintf(stderr,"%s: %s",client_name,message);
  if (qualifier != (char *) NULL)
    (void) fprintf(stderr," (%s)",qualifier);
  (void) fprintf(stderr,".\n");
  if (child > 0)
    if (master != -1)
      {
        (void) fcntl(master,F_SETFL,fcntl(master,F_GETFL) | O_NONBLOCK);
        write(master,"quit\n",5);
        while (Wait());
      }
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E x p a n d F i l e n a m e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ExpandFilename expands '~' in a filename.
%
%  The format of the ExpandFilename function is:
%
%      ExpandFilename(filename)
%
%  A description of each parameter follows:
%
%    o filename: Specifies a pointer to an character array that contains the
%      filename.
%
%
*/
void ExpandFilename(char *filename)
{
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
  char
    expanded_filename[MaxTextLength];

  register char
    *p;

  if (filename == (char *) NULL)
    return;
  if (*filename != '~')
    return;
  if (*(filename+1) == '/')
    {
      /*
        Substitute ~ with $HOME.
      */
      p=(char *) getenv("HOME");
      if (p == (char *) NULL)
        p=".";
      (void) strcpy(expanded_filename,p);
      (void) strcat(expanded_filename,filename+1);
    }
  else
    {
      char
        username[MaxTextLength];

      struct passwd
        *entry;

      /*
        Substitute ~ with home directory from password file.
      */
      (void) strcpy(username,filename+1);
      p=strchr(username,'/');
      if (p != (char *) NULL)
        *p='\0';
      entry=getpwnam(username);
      if (entry == (struct passwd *) NULL)
        return;
      (void) strcpy(expanded_filename,entry->pw_dir);
      if (p != (char *) NULL)
        {
          (void) strcat(expanded_filename,"/");
          (void) strcat(expanded_filename,p+1);
        }
    }
  (void) strcpy(filename,expanded_filename);
#endif
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E x e c u t e F t p                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ExecuteFtp executes the FTP program as a child process.
%
%  The format of the ExecuteFtp routine is:
%
%    ExecuteFtp(hostname,port)
%
%  A description of each parameter follows:
%
%    o hostname:  Specifies a pointer to a character array that contains the
%      name of the host to establish a connection to a FTP server.
%
%    o port:  Specifies a port number.  If the port number is NULL, xtp
%      attempts to contact a FTP server at the default port.
%
%
%
*/
static void ExecuteFtp(char *hostname,char *port)
{
  int
    slave;

  struct sigaction
    action;

  struct termios
    attributes;

  /*
    Get slave tty line.
  */
  action.sa_handler=SIG_IGN;
  (void) sigemptyset(&action.sa_mask);
  action.sa_flags=0;
  (void) sigaction(SIGTSTP,&action,(struct sigaction *) NULL);
  slave=open(slave_tty,O_RDWR | O_NOCTTY);
  if (slave < 0)
    Error("Unable to open slave pseudo-terminal",slave_tty);
  /*
    Condition slave tty line.
  */
#if defined(HAVE_PTMX)
  (void) ioctl(slave,I_PUSH,"ptem");
  (void) ioctl(slave,I_PUSH,"ldterm");
  (void) ioctl(slave,I_PUSH,"ttcompat");
#endif
#if defined(HAVE_PTMX_BSD)
  {
    char
      buffer[10240];

    if ((void) ioctl(slave,I_LOOK,buffer) != 0)
      (void) ioctl(slave,I_PUSH,"ldterm");
  }
#endif
  (void) tcgetattr(slave,&attributes);
  attributes.c_iflag&=(~(BRKINT | IGNPAR | PARMRK | INPCK | ISTRIP | INLCR |
    IGNCR | ICRNL | IXON));
  attributes.c_iflag|=IGNBRK | IXOFF;
  attributes.c_oflag&=(~OPOST);
  attributes.c_lflag&=
    (~(ECHO | ECHOE | ECHOK | ECHONL | ICANON | ISIG | NOFLSH | TOSTOP));
  attributes.c_cflag&=(~(CSIZE | CSTOPB | HUPCL | PARENB));
  attributes.c_cflag|=CLOCAL | CREAD | CS8;
  (void) tcflush(slave,TCIFLUSH);
  (void) tcsetattr(slave,TCSANOW,&attributes);
  /*
    Execute FTP program as a child process.
  */
  (void) close(master);
  (void) dup2(slave,STDIN_FILENO);
  (void) dup2(slave,STDOUT_FILENO);
  (void) dup2(slave,STDERR_FILENO);
  (void) close(slave);
  (void) execlp(XTP_FTP,"ftp","-n","-i","-g","-v",hostname,port,(char *) 0);
  perror("ftp");
  (void) kill(0,SIGTERM);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t L o g i n I n f o                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GetLoginInfo searches the .netrc file for a machine token that
%  matches the remote machine specified with parameter hostname.  Once a
%  match is made, the subsequent .netrc tokens are processed, stopping when
%  the EOF is reached or another machine token is encountered.
%
%  The format of the GetLoginInfo routine is:
%
%      GetLoginInfo(hostname,user,ident,account)
%
%
*/
static void GetLoginInfo(const char *hostname,char *user,char *ident,
  char *account)
{
  char
    filename[MaxTextLength],
    keyword[MaxTextLength],
    value[MaxTextLength];

  FILE
    *file;

  int
    match;

  register int
    c;

  /*
    Open netrc file.
  */
  (void) strcpy(filename,"~/.netrc");
  ExpandFilename(filename);
  file=fopen(filename,"r");
  if (file == (FILE *) NULL)
    return;
  /*
    Search netrc file for a machine name match.
  */
  match=False;
  c=fgetc(file);
  if (c == EOF)
    return;
  while (isgraph(c) && (c != EOF))
  {
    register char
      *p;

    if (!isalnum(c))
      c=fgetc(file);
    else
      {
        /*
          Determine a keyword and its value.
        */
        p=keyword;
        do
        {
          if ((p-keyword) < (MaxTextLength-1))
            *p++=(char) c;
          c=fgetc(file);
        } while (isalnum(c));
        *p='\0';
        while (isspace(c) || (c == '='))
          c=fgetc(file);
        p=value;
        if (c != '"')
          while (!isspace(c) && (c != EOF))
          {
            if ((p-value) < (MaxTextLength-1))
              *p++=(char) c;
            c=fgetc(file);
          }
        else
          {
            c=fgetc(file);
            while ((c != '"') && (c != EOF))
            {
              if ((p-value) < (MaxTextLength-1))
                *p++=(char) c;
              c=fgetc(file);
            }
          }
        *p='\0';
        /*
          Assign a value to the specified keyword.
        */
        if (strcmp(keyword,"machine") == 0)
          {
            if (match)
              break;
            match=strcmp(hostname,value) == 0;
            *user='\0';
            *ident='\0';
            *account='\0';
          }
        if (match)
          {
            if (strcmp(keyword,"login") == 0)
              (void) strcpy(user,value);
            if (strcmp(keyword,"password") == 0)
              (void) strcpy(ident,value);
            if (strcmp(keyword,"account") == 0)
              (void) strcpy(account,value);
          }
      }
    while (isspace(c))
      c=fgetc(file);
  }
  (void) fclose(file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t P s e u d o T e r m i n a l                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GetPseudoTerminal returns a master/slave pair of pseudo-terminals.
%
%  The format of the GetPseudoTerminal routine is:
%
%    GetPseudoTerminal()
%
%
*/
static void GetPseudoTerminal(void)
{
  register char
    *cp;

  struct termios
    attributes;

#if defined(HAVE_PTMX)
  *slave_tty='\0';
  master=open("/dev/ptmx",O_RDWR);
  if (master > 0)
    if ((grantpt(master) != -1) && (unlockpt(master) != -1))
      {
        cp=(char *) ptsname(master);
        if (cp != (char *) NULL)
          (void) strcpy(slave_tty,cp);
      }
  if (*slave_tty == '\0')
    {
      close(master);
      master=(-1);
    }
#else
  char
    master_tty[16];

  register char
    *bank;

  struct stat
    info;

  master=(-1);
  for (bank="pqrs"; *bank; bank++)
  {
    (void) sprintf(master_tty,"/dev/pty%c0",*bank);
    (void) sprintf(slave_tty,"/dev/tty%c0",*bank);
    if (stat(master_tty,&info) < 0)
      break;
    for (cp="0123456789abcdef"; *cp; cp++)
    {
      (void) sprintf((char *) master_tty,"/dev/pty%c%c",*bank,*cp);
      master=open(master_tty,O_RDWR);
      if (master < 0)
        continue;
      /*
        Verify slave side is usable.
      */
      (void) sprintf(slave_tty,"/dev/tty%c%c",*bank,*cp);
      if (access(slave_tty,R_OK | W_OK) == 0)
        break;
      (void) close(master);
    }
    if (access(slave_tty,R_OK | W_OK) == 0)
      break;
  }
#endif
  if (master < 0)
    Error("All network ports in use",(char *) NULL);
  /*
    Condition master tty line.
  */
  (void) tcgetattr(master,&attributes);
  attributes.c_lflag&=(~(ICANON | ECHO));
  (void) tcflush(master,TCIFLUSH);
  (void) tcsetattr(master,TCSANOW,&attributes);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a k e D i r e c t o r y                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MakeDirectory checks each component of a directory path and if it
%  does not exist, creates it.
%
%  The format of the MakeDirectory routine is:
%
%    MakeDirectory(directory)
%
%  A description of each parameter follows:
%
%    o directory:  Specifies a pointer to a character array that contains
%      the name of the directory to create.
%
%
*/
static int MakeDirectory(char *directory)
{
  register char
    *p;

  struct stat
    info;

  /*
    Determine first component of the directory.
  */
  p=strrchr(directory,'/');
  if ((p == (char *) NULL) || (p == directory))
    return(False);
  *p='\0';
#if !defined(__OPENNT)
  if (lstat(directory,&info) < 0)
#else
  if (stat(directory,&info) < 0)
#endif
    {
      /*
        Path component does not exist;  create it.
      */
      if (MakeDirectory(directory) == 0)
        if (mkdir(directory,(mode_t) 0777) >= 0)
          {
            *p='/';
            return(False);
          }
    }
  else
    if (S_ISDIR(info.st_mode))
      {
        /*
          Path component already exists.
        */
        *p='/';
        return(False);
      }
  /*
    Path component is a file not a directory.
  */
  *p='/';
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P r i n t R e q u e s t                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function PrintRequest prints a file on the remote FTP server.
%
%  The format of the PrintRequest routine is:
%
%    PrintRequest(filename,verbose)
%
%  A description of each parameter follows:
%
%    o filename:  Specifies a pointer to a character array that contains
%      the name of the file to print.
%
%    o verbose: An unsigned integer.  A value other than zero dhows all
%      responses from the remote server.
%
%
*/
static void PrintRequest(char *filename,unsigned int verbose)
{
  char
    command[MaxTextLength],
    *response;

  /*
    get remote-file [ - | < |zcat > ].
  */
  (void) sprintf(command,"get %s",filename);
  if (strcmp(filename+strlen(filename)-2,".Z") == 0)
    (void) strcat(command," |zcat\n");
  else
    if (strcmp(filename+strlen(filename)-3,".gz") == 0)
      (void) strcat(command," |gunzip -c\n");
    else
      (void) strcat(command," -\n");
  (void) write(master,command,strlen(command));
  (void) fprintf(stdout,"%s:\n",filename);
  while ((response=Wait()))
  {
    if (status == 0)
      (void) fprintf(stdout,"%s\n",response);
    else
      if ((status == 5) || verbose)
        (void) fprintf(stderr,"%s\n",response);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P r o c e s s R e q u e s t                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ProcessRequest first gets a recursive listing of the remote
%  directory.  Next each filename in the list is either accepted or rejected
%  based on a user specified regular expresssion.  If any files match the
%  regular expression, its filename is listed, printed, or retrieved as
%  specified by the command line arguments.
%
%  The format of the ProcessRequest routine is:
%
%    ProcessRequest(system_type,prune,verbose)
%
%  A description of each parameter follows:
%
%    o system_type:  Specifies what type of system the remote host is:
%      UNIX, VMS, or other.
%
%    o prune:  Specifies whether to recusively search for files.
%
%    o verbose: An unsigned integer.  A value other than zero dhows all
%      responses from the remote server.
%
%
*/
static void ProcessRequest(char *system_type,unsigned int prune,
  unsigned int verbose)
{
#define AccessExpression  "[Pp]ermission denied|not found|cannot access"
#define DateExpression  " (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) "
#define ListExpression  "ls-l([Rt])+([Rt])*([^ ])*"

  char
    command[MaxTextLength],
    directory[MaxTextLength],
    **filelist,
    filename[MaxTextLength << 1],
    *info,
    *response;

  register char
    *p;

  register int
    i;

  RegularExpression
    *access_expression,
    *date_expression;

  unsigned int
    match,
    maximum_files,
    number_files;

  /*
    Ask remote server for a file listing.
  */
  (void) strcpy(command,"dir\n");
  if (!prune)
    {
      if (strncmp("VMS",system_type,3) == 0)
        (void) strcpy(command,"ls [...]\n");
      else
        if ((strncmp("UNIX",system_type,4) == 0) ||
            (strncmp("Windows_NT",system_type,10) == 0))
          {
            RegularExpression
              *list_expression;

            /*
              Get a recursive file listing.
            */
            (void) write(master,command,strlen(command));
            (void) strcpy(command,"ls -ltR\n");
            list_expression=CompileRegularExpression(ListExpression);
            while ((response=Wait()))
            {
              if ((status == 0) && (*response != '\0'))
                if (ExecuteRegularExpression(list_expression,response))
                  {
                    /*
                      Remote site has a directory listing file.
                    */
                    (void) strncpy(filename,list_expression->subpattern[0],
                      list_expression->pattern_length);
                    (void) fprintf(stderr,"Using remote file listing %s...\n",
                      filename);
                    (void) sprintf(command,"get %s",filename);
                    if (strcmp(filename+strlen(filename)-2,".Z") == 0)
                      (void) strcat(command," |zcat\n");
                    else
                      if (strcmp(filename+strlen(filename)-3,".gz") == 0)
                        (void) strcat(command," |gunzip -c\n");
                      else
                        (void) strcat(command," -\n");
                    while (Wait());
                    break;
                  }
            }
            free((char *) list_expression);
          }
    }
  (void) write(master,command,strlen(command));
  while ((response=Wait()))
  {
    if ((status == 0) || (status == 5))
      break;
  }
  if (status == 5)
    {
      /*
        Directory command has limited functionality.
      */
      while (Wait());
      (void) strcpy(command,"dir\n");
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if ((status == 0) || (status == 5))
          break;
      }
    }
  status=(-1);
  if (response == (char *) NULL)
    return;
  /*
    Search the recursive directory listing and act on expression matches.
  */
  access_expression=CompileRegularExpression(AccessExpression);
  date_expression=CompileRegularExpression(DateExpression);
  *directory='\0';
  if (print_expression || retrieve_expression)
    {
      maximum_files=2048;
      filelist=(char **) malloc(maximum_files*sizeof(char *));
      if (filelist == (char **) NULL)
        Error("Unable to allocate memory",(char *) NULL);
      number_files=0;
    }
  do
  {
    if ((status > 0) || (*response == '\0'))
      continue;
    /*
      Construct file name and info.
    */
    while (*response == ' ')
      response++;
    info=response;
    p=response+strlen(response)-1;
    if (*p == '\r')
      p--;
    if ((strncmp("UNIX",system_type,4) != 0) &&
        (strncmp("Windows_NT",system_type,10) != 0))
      {
        if (strncmp("VMS",system_type,3) == 0)
          if (*p == ']')
            {
              /*
                File is a directory.
              */
              do { p--; } while (*p == ' ');
              *(++p)='\0';
              (void) strcpy(directory,response);
              (void) strcat(directory,"]");
              continue;
            }
        while ((*info != ' ') && *info)
          info++;
        *info='\0';
        p=response;
      }
    else
      {
        if ((*response != '-') && (*response != 'F'))
          {
            if (*p == ':')
              {
                /*
                  File is a directory.
                */
                do { p--; } while (*p == ' ');
                *(++p)='\0';
                (void) strcpy(directory,response);
                (void) strcat(directory,"/");
              }
            continue;
          }
        if (ExecuteRegularExpression(access_expression,response))
          continue;
        if (ExecuteRegularExpression(date_expression,response))
          p=date_expression->subpattern[0]+13;
        *p++='\0';
        while (*p == ' ')
          p++;
      }
    (void) strcpy(filename,directory);
    (void) strcat(filename,p);
    if (exclude_expression)
      if (ExecuteRegularExpression(exclude_expression,filename))
        continue;
    if (directory_expression)
      if (ExecuteRegularExpression(directory_expression,filename))
        DirectoryRequest(info,filename);
    match=False;
    if (print_expression)
      match|=ExecuteRegularExpression(print_expression,filename);
    if (retrieve_expression)
      match|=ExecuteRegularExpression(retrieve_expression,filename);
    if (!match)
      continue;
    if (number_files >= maximum_files)
      {
        maximum_files<<=1;
        filelist=(char **) realloc(filelist,maximum_files*sizeof(char *));
        if (filelist == (char **) NULL)
          Error("Unable to allocate memory",(char *) NULL);
      }
    filelist[number_files]=(char *) malloc((strlen(filename)+1)*sizeof(char));
    if (filelist[number_files] == (char *) NULL)
      Error("Unable to allocate memory",(char *) NULL);
    (void) strcpy(filelist[number_files],filename);
    number_files++;
  } while ((response=Wait()));
  free((char *) date_expression);
  free((char *) access_expression);
  if (!print_expression && !retrieve_expression)
    return;
  if (number_files == 0)
    {
      Warning("no files matched your expression",(char *) NULL);
      return;
    }
  /*
    Print, or retrieve a file.
  */
  for (i=0; i < (int) number_files; i++)
  {
    if (print_expression)
      if (ExecuteRegularExpression(print_expression,filelist[i]))
        PrintRequest(filelist[i],verbose);
    if (retrieve_expression)
      if (ExecuteRegularExpression(retrieve_expression,filelist[i]))
        RetrieveRequest(filelist[i],verbose);
    free((char *) filelist[i]);
  }
  free((char *) filelist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e t r i e v e R e q u e s t                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function RetrieveRequest retrieves a file from the remote FTP server.
%
%  The format of the RetrieveRequest routine is:
%
%    RetrieveRequest(filename,verbose)
%
%  A description of each parameter follows:
%
%    o filename:  Specifies a pointer to a character array that contains
%      the name of the file to retrieve.
%
%    o verbose: An unsigned integer.  A value other than zero dhows all
%      responses from the remote server.
%
%
*/
static void RetrieveRequest(char *filename,unsigned int verbose)
{
  char
    command[MaxTextLength],
    *response;

  /*
    get remote-file
  */
  (void) MakeDirectory(filename);
  (void) sprintf(command,"get %s\n",filename);
  (void) write(master,command,strlen(command));
  while ((response=Wait()))
  {
    if (verbose)
      (void) fprintf(stderr,"%s\n",response);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S i g n a l A l a r m                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SignalAlarm is called if the timer expires.
%
%  The format of the SignalAlarm routine is:
%
%    SignalAlarm(status)
%
%
*/
static void SignalAlarm(int status)
{
  char
    message[MaxTextLength];

  int
    process_status;

  while (waitpid((pid_t) NULL,&process_status,WNOHANG) > 0);
  (void) sprintf(message,"timeout expired, status %x",process_status);
  Error(message,(char *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S i g n a l C h i l d                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SignalChild is called if the status of the child process changes.
%
%  The format of the SignalChild routine is:
%
%    SignalChild(status)
%
%
*/
static void SignalChild(int status)
{
  char
    message[MaxTextLength];

  int
    process_status;

  while (waitpid((pid_t) NULL,&process_status,WNOHANG) > 0);
  (void) sprintf(message,"child died, status %x",process_status);
  Error(message,(char *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U s a g e                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure Usage displays the program usage;
%
%  The format of the Usage routine is:
%
%      Usage()
%
%
*/
static void Usage(void)
{
  char
    **p;

  static char
    *options[]=
    {
      "  -account password    supplemental password",
      "  -binary              retrieve files as binary",
      "  -exclude expression  exclude files that match the expression",
      "  -directory           list file names that match the expression",
      "  -file name           store the file with this name",
      "  -get                 get files that match the expression",
      "  -port number         port number of FTP server",
      "  -print               print files that match the expression",
      "  -prune               do not recursively search for files",
      "  -proxy hostname      access remote host via this proxy host",
      "  -put                 put files that match the expression",
      "  -retrieve            retrieve files that match the expression",
      "  -timeout seconds     specifies maximum seconds of XTP session",
      "  -verbose             show all responses from the remote server",
      "",
      "<uniform resource locator> has the format:",
      "",
      "    protocol://host/[directory/[filename]]",
      "",
      "where protocol is `ftp' and host is `[user[:password]]@hostname'.",
      "User defaults to `anonymous' and password defaults to `host.domain'.",
      "Note that `directory/[filename]' is interpreted relative to the home",
      "directory for `user', thus an absolute pathname must be specified",
      "with the leading '/':",
      "",
      "    ftp://host//tmp/anyfile",
      "",
      "As an extension, the filename part of the locator is expanded",
      "by the shell for options -get or -put, otherwise it is processed",
      "as a regular expression.  For convenience, the protocol component",
      "of the uniform resource locator (ftp://) may be omitted.",
      NULL
    };
  (void) fprintf(stderr,
    "Usage: %s [-options ...] <uniform resource locator>\n",client_name);
  (void) fprintf(stderr,"\nWhere options include:\n");
  for (p=options; *p; p++)
    (void) fprintf(stderr,"%s\n",*p);
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W a i t                                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Wait reads a line of output from the remote FTP server.
%
%  The format of the Wait() routine is:
%
%    response=Wait()
%
%  A description of each parameter follows:
%
%    o response:  Function Wait returns this pointer to the output obtained
%      from the remote FTP server.
%
%
*/
static char *Wait(void)
{
  register char
    *p;

  static char
    buffer[1024],
    *q;

  static char
    line[1024];

  static int
    count=0;

  status=0;
  p=line;
  do
  {
    if (count <= 0)
      {
        /*
          The buffer is empty;  read output from the remote FTP server.
        */
        count=read(master,buffer,sizeof(buffer));
        q=buffer;
        if (count <= 0)
          {
            if (p == line)
              return((char *) NULL);
            break;
          }
      }
    count--;
    *p=(*q++);
    if (*p == '\n')
      break;
    p++;
    if ((p-line) >= 5)
      if (!strncmp(p-5,"ftp> ",5))
        if (count == 0)
          return((char *) NULL);
  } while (p < (line+sizeof(line)));
  *p='\0';
  if (isdigit((int) *line))
    status=atoi(line)/100;
  return(line);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   m a i n                                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char **argv)
{
#define AccountExpression "[Aa]ccount"
#define ConnectExpression  "[Nn]ot connected"
#define IdentExpression  "[Pp]assword"
#define TypeExpression  "system type is "

  char
    account[MaxTextLength],
    command[MaxTextLength],
    directory[MaxTextLength],
    filename[MaxTextLength],
    *get_expression,
    *host_info,
    hostname[MaxTextLength],
    ident[MaxTextLength],
    *localname,
    *port,
    protocol[MaxTextLength],
    *proxy,
    *put_expression,
    *remotename,
    system_type[MaxTextLength],
    *url,
    user[MaxTextLength];

  int
    binary,
    field,
    process_status;

  register char
    *p,
    *q,
    *response;

  RegularExpression
    *account_expression,
    *connect_expression,
    *ident_expression,
    *type_expression;

  struct sigaction
    action;

  unsigned int
    count,
    prune,
    timeout,
    verbose;

  client_name=argv[0];
  if (argc < 2)
    Usage();
#if defined(SOCKS)
  SOCKSinit(argv[0]);
#endif
  /*
    Initialize program variables.
  */
  *account='\0';
  binary=True;
  child=(-1);
  *directory='\0';
  directory_expression=(RegularExpression *) NULL;
  exclude_expression=(RegularExpression *) NULL;
  *filename='\0';
  *ident='\0';
  get_expression=(char *) NULL;
  *hostname='\0';
  localname=(char *) NULL;
  master=(-1);
  port=(char *) NULL;
  print_expression=(RegularExpression *) NULL;
  *protocol='\0';
  proxy=(char *) getenv("xtp_proxy");
  prune=False;
  put_expression=(char *) NULL;
  remotename=(char *) NULL;
  retrieve_expression=(RegularExpression *) NULL;
  *system_type='\0';
  timeout=0;
  *user='\0';
  verbose=False;
  /*
    Parse uniform resource locator.
  */
  field=0;
  url=argv[argc-1];
  p=url;
  for (q=url; *q != '\0'; q++)
  {
    if (*q != '/')
      continue;
    switch (field)
    {
      case 0:
      {
        if ((q > url) && (*(q-1) == ':'))
          (void) strncat(protocol,p,q-p);
        else
          {
            (void) strncat(hostname,p,q-p);
            field+=2;
          }
        break;
      }
      case 1:
        break;
      case 2:
      {
        (void) strncat(hostname,p,q-p);
        break;
      }
      default:
      {
        (void) strncat(directory,p,q-p+1);
        break;
      }
    }
    field++;
    p=q+1;
  }
  if (*directory != '\0')
    directory[strlen(directory)-1]='\0';
  (void) strncat(filename,p,q-p);
  if (*hostname == '\0')
    (void) gethostname(hostname,64);
  GetLoginInfo(hostname,user,ident,account);
  p=strrchr(hostname,'@');
  if (p != (char *) NULL)
    {
      /*
        Parse login-name:password@site.
      */
      GetLoginInfo(p+1,user,ident,account);
      strncpy(user,hostname,p-hostname);
      user[p-hostname]='\0';
      strcpy(hostname,p+1);
      p=strchr(user,':');
      if (p != (char *) NULL)
        {
          strcpy(ident,p+1);
          user[p-user]='\0';
        }
    }
  /*
    Parse command line arguments.
  */
  for (argv++; *argv && ((**argv == '-') || (**argv == '+')); argv++)
    switch (argv[0][1])
    {
      case 'a':
      {
        (void) strcpy(account,*++argv);
        break;
      }
      case 'b':
      {
        binary=(**argv == '-');
        break;
      }
      case 'd':
      {
        directory_expression=CompileRegularExpression(filename);
        if (!directory_expression)
          exit(1);
        break;
      }
      case 'e':
      {
        exclude_expression=CompileRegularExpression(*++argv);
        if (!exclude_expression)
          exit(1);
        break;
      }
      case 'f':
      {
        localname=(*++argv);
        remotename=(*++argv);
        break;
      }
      case '?':
      case 'h':
      {
        Usage();
        break;
      }
      case 'g':
      {
        get_expression=filename;
        if (localname == (char *) NULL)
          localname=filename;
        break;
      }
      case 'p':
      {
        if (strncmp("port",*argv+1,2) == 0)
          {
            port=(*++argv);
            break;
          }
        if (strncmp("proxy",*argv+1,3) == 0)
          {
            proxy=(char *) NULL;
            if (**argv == '-')
              {
                argv++;
                if (*argv == (char *) NULL)
                  Error("Missing host on -proxy",(char *) NULL);
                proxy=(*argv);
              }
            break;
          }
        if (strncmp("prune",*argv+1,3) == 0)
          {
            prune=(**argv == '-');
            break;
          }
        if (strncmp("print",*argv+1,3) == 0)
          {
            print_expression=CompileRegularExpression(filename);
            if (!print_expression)
              exit(1);
            break;
          }
        if (strncmp("put",*argv+1,2) == 0)
          {
            put_expression=filename;
            if (remotename == (char *) NULL)
              remotename=filename;
            break;
          }
        Error("Unrecognized option",*argv);
        break;
      }
      case 'r':
      {
        retrieve_expression=CompileRegularExpression(filename);
        if (!retrieve_expression)
          exit(1);
        break;
      }
      case 't':
      {
        timeout=atoi(*++argv);
        break;
      }
      case 'v':
      {
        verbose=True;
        break;
      }
      default:
      {
        Error("Unrecognized option",(char *) NULL);
        break;
      }
    }
  if ((directory_expression == (RegularExpression *) NULL) &&
      (print_expression == (RegularExpression *) NULL) &&
      (retrieve_expression == (RegularExpression *) NULL) &&
      (put_expression == (char *) NULL))
    get_expression=filename;
  if (*protocol != '\0')
    {
      if (strcmp(protocol,"ftp:") != 0)
        {
          if ((get_expression == (char *) NULL) ||
              (strlen(get_expression) == 0))
            Error("Unsupported protocol",protocol);
          else
            {
              /*
                Use GET to handle non-ftp protocol but only for *get*.
              */
              (void) sprintf(command,"GET %s > %s",url,filename);
              status=system(command);
              return(status < 0);
            }
        }
    }
  if ((*ident == '\0') && (*user == '\0'))
    {
      struct passwd
        *user_info;

      /*
        Identify user as user@host.domain.
      */
      (void) strcpy(user,"anonymous");
      (void) strcpy(ident,"anonymous");
      user_info=getpwuid(geteuid());
      if (user_info != (struct passwd *) NULL)
        (void) strcpy(ident,user_info->pw_name);
      if (proxy != (char *) NULL)
        {
          (void) strcat(ident,"@");
          (void) strcat(ident,proxy);
        }
      else
        {
          p=ident+strlen(ident);
          *p++='@';
          (void) gethostname(p,64);
#if !defined(__OPENNT)
          p=ident+strlen(ident);
          *p++='.';
          (void) getdomainname(p,64);
#endif
        }
    }
  if (proxy != (char *) NULL)
    {
      /*
        Access the remote host via a proxy ftpd client.
      */
      (void) strcat(user,"@");
      (void) strcat(user,hostname);
      (void) strcpy(hostname,proxy);
    }
  host_info=GetHostInfo(hostname);
  if (host_info == (char *) NULL)
    Error("Unknown host",hostname);
  if (verbose)
    {
      if (*directory == '\0')
        (void) fprintf(stderr,"%s\n",host_info);
      else
        (void) fprintf(stderr,"%s %s\n",host_info,directory);
    }
  GetPseudoTerminal();
  /*
    Set signal handlers.
  */
  action.sa_handler=SignalAlarm;
  (void) sigemptyset(&action.sa_mask);
  action.sa_flags=0;
  (void) sigaction(SIGALRM,&action,(struct sigaction *) NULL);
  if (timeout != 0)
    (void) alarm(Max(timeout >> 4,120));  /* enable login timer. */
  action.sa_handler=SignalChild;
  (void) sigaction(SIGCHLD,&action,(struct sigaction *) NULL);
  /*
    Connect and logon to host.
  */
  child=fork();
  if (child < 0)
    Error("Unable to fork",(char *) NULL);
  if (child == 0)
    ExecuteFtp(hostname,port);
  type_expression=CompileRegularExpression(TypeExpression);
  while ((response=Wait()))
  {
    if (verbose)
      (void) fprintf(stderr,"%s\n",response);
    if (ExecuteRegularExpression(type_expression,response))
      (void) strcpy(system_type,type_expression->subpattern_end[0]);
  }
  free((char *) type_expression);
  if (*user == '\0')
    (void) strcpy(user,"anonymous");
  (void) sprintf(command,"user %s\n",user);
  if (*ident != '\0')
    (void) sprintf(command,"user %s %s\n",user,ident);
  (void) write(master,command,strlen(command));
  /*
    Logon dialog may require a password or account information.
  */
  account_expression=CompileRegularExpression(AccountExpression);
  connect_expression=CompileRegularExpression(ConnectExpression);
  ident_expression=CompileRegularExpression(IdentExpression);
  while ((response=Wait()))
  {
    if (verbose)
      (void) fprintf(stderr,"%s\n",response);
    if (status == 3)
      if (*ident == '\0')
        if (ExecuteRegularExpression(ident_expression,response))
          {
            if (!verbose)
              (void) fprintf(stderr,"%s\n",response);
            count=read(master,command,sizeof(command));
            command[count]='\0';
            (void) fprintf(stderr,"%s",command);
          }
    if (status == 3)
      if (*account == '\0')
        if (ExecuteRegularExpression(account_expression,response))
          {
            if (!verbose)
              (void) fprintf(stderr,"%s\n",response);
            count=read(master,command,sizeof(command));
            command[count]='\0';
            (void) fprintf(stderr,"%s",command);
          }
    if (status == 5)
      Error(response,user);
    if (ExecuteRegularExpression(connect_expression,response))
      Error("Unable to connect to remote host",hostname);
  }
  free((char *) ident_expression);
  free((char *) connect_expression);
  free((char *) account_expression);
  if (timeout != 0)
    (void) alarm(timeout);  /* enable session timer. */
  if (*system_type == '\0')
    {
      /*
        Determine system type.
      */
      (void) strcpy(system_type,"UNIX");
      (void) strcpy(command,"quote syst\n");
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (status == 2)
          (void) strcpy(system_type,response+4);
      }
    }
  if (*directory != '\0')
    {
      /*
        Change remote working directory.
      */
      (void) sprintf(command,"cd %s\n",directory);
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (verbose)
          (void) fprintf(stderr,"%s\n",response);
        if (status == 5)
          Error("No such directory",directory);
      }
      (void) strcpy(command,"pwd\n");
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (verbose)
          (void) fprintf(stderr,"%s\n",response);
      }
    }
  if (binary)
    {
      /*
        Set file transfer type.
      */
      (void) strcpy(command,"binary\n");
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (verbose)
          (void) fprintf(stderr,"%s\n",response);
      }
      (void) strcpy(command,"type\n");
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (verbose)
          (void) fprintf(stderr,"%s\n",response);
      }
    }
  if (get_expression != (char *) NULL)
    {
      /*
        Process get request.
      */
      (void) sprintf(command,"mget %s\n",get_expression);
      if (!IsGlob(get_expression) && (localname != (char *) NULL))
        {
          if ((strncmp("UNIX",system_type,4) != 0) &&
              (strncmp("Windows_NT",system_type,10) != 0))
            (void) sprintf(command,"get %s %s\n",get_expression,localname);
          else
            (void) sprintf(command,"get %s %s~\n",get_expression,localname);
        }
      (void) write(master,command,strlen(command));
      while ((response=Wait()))
      {
        if (verbose)
          (void) fprintf(stderr,"%s\n",response);
        if (status == 5)
          Error(response,user);
      }
      if (!IsGlob(get_expression) && (localname != (char *) NULL))
        if ((strncmp("UNIX",system_type,4) == 0) ||
            (strncmp("Windows_NT",system_type,10) == 0))
          {
            (void) sprintf(command,"rename %s~ %s\n",localname,localname);
            (void) write(master,command,strlen(command));
            while ((response=Wait()))
            {
              if (verbose)
                (void) fprintf(stderr,"%s\n",response);
              if (status == 5)
                Error(response,user);
            }
          }
    }
  else
    if (put_expression != (char *) NULL)
      {
        /*
          Process put request.
        */
        (void) strcpy(command,"glob on\n");
        (void) write(master,command,strlen(command));
        while ((response=Wait()))
        {
          if (verbose)
            (void) fprintf(stderr,"%s\n",response);
        }
        (void) sprintf(command,"mput %s\n",put_expression);
        if (!IsGlob(put_expression) && (remotename != (char *) NULL))
          {
            if ((strncmp("UNIX",system_type,4) != 0) &&
                (strncmp("Windows_NT",system_type,10) != 0))
              (void) sprintf(command,"put %s %s\n",put_expression,remotename);
            else
              (void) sprintf(command,"put %s %s~\n",put_expression,remotename);
          }
        (void) write(master,command,strlen(command));
        while ((response=Wait()))
        {
          if (verbose)
            (void) fprintf(stderr,"%s\n",response);
          if (status == 5)
            Error(response,user);
        }
        if (!IsGlob(put_expression) && (remotename != (char *) NULL))
          {
            if ((strncmp("UNIX",system_type,4) == 0) ||
                (strncmp("Windows_NT",system_type,10) == 0))
              {
                (void) sprintf(command,"rename %s~ %s\n",remotename,
                  remotename);
                (void) write(master,command,strlen(command));
                while ((response=Wait()))
                {
                  if (verbose)
                    (void) fprintf(stderr,"%s\n",response);
                  if (status == 5)
                    Error(response,user);
                }
              }
          }
      }
    else
      ProcessRequest(system_type,prune,verbose);
  (void) strcpy(command,"quit\n");
  (void) write(master,command,strlen(command));
  /*
    Wait for child to finish.
  */
  action.sa_handler=SIG_DFL;
  (void) sigemptyset(&action.sa_mask);
  action.sa_flags=0;
  (void) sigaction(SIGCHLD,&action,(struct sigaction *) NULL);
  (void) fcntl(master,F_SETFL,fcntl(master,F_GETFL) | O_NONBLOCK);
  while (Wait());
  (void) waitpid(child,&process_status,WNOHANG);
  (void) close(master);
  if (directory_expression != (RegularExpression *) NULL)
    free((char *) directory_expression);
  if (exclude_expression != (RegularExpression *) NULL)
    free((char *) exclude_expression);
  if (print_expression != (RegularExpression *) NULL)
    free((char *) print_expression);
  if (retrieve_expression != (RegularExpression *) NULL)
    free((char *) retrieve_expression);
  return(status < 0);
}
