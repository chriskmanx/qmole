////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "FtpListParser.h"
#include <time.h>
#include <ctype.h>
#ifndef _WIN32
 #include <strings.h> //Linux strncasecmp
#endif
static int symperms (const char *s);
static void parse_date(const char *str, struct tm &timestamp);
static void parse_time(const char *str, struct tm &timestamp);
static void parse_time2(const char *str, struct tm &timestamp);


CFtpListParser::CFtpListParser()
{
	m_szSystem[0] = '\0';
}

CFtpListParser::~CFtpListParser()
{
}

void CFtpListParser::SetSystemType(const char *szName)
{
	strncpy(m_szSystem, szName, sizeof(m_szSystem));
	m_szSystem[sizeof(m_szSystem)-1] = '\0';
}

bool CFtpListParser::ParseList(CStrList &list, VfsListing &out)
{
	//ASSERT(strlen(m_szSystem) > 0);

	//TOFIX better algorithm
	//TOFIX autodetect
	if(0 == strncmp(m_szSystem, "UNIX Type", strlen("UNIX Type")))
		return ParseUnix(list, out);
	else if (0 == strncmp(m_szSystem, "Windows_NT", strlen("Windows_NT")))
		return ParseWinNT(list, out);
	else if (0 == strncmp(m_szSystem, "VMS", strlen("VMS")))
		return ParseVMS(list, out);
	else
		return ParseUnix(list, out);

	return false;
}

//	Convert the Unix-ish style directory listing stored in FILE to a
//  linked list of fileinfo (system-independent) entries.  The contents
//  of FILE are considered to be produced by the standard Unix `ls -la'
//  output (whatever that might be). BSD (no group) and SYSV (with group) 
//  listings are handled.
//
//  The time stamps are stored in a separate variable, time_t
//  compatible (I hope).  The timezones are ignored.

// >>>>>>>> used from wget program
bool CFtpListParser::ParseUnix(CStrList &list, VfsListing &out)
{
	static const char *months[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};

	int next, len, j, error, ignore;
	int year, month, day;		/* for time analysis */
	int hour, min, sec;
	struct tm timestruct, *tnow;
	time_t timenow;
	
	char *tok;		/* tokenizer */

	char buffer[1024] = "";
	VfsItem info;

	// for each text line
	for(unsigned int i=0; i<list.size(); i++)
	{
		len = list[i].size();

		strncpy(buffer, list[i].c_str(), 1023);
		buffer[len] = '\0';

		//TRACE("Line buffer: %s\n", buffer);	
		
		// Destroy <CR> if there is one.
		char *szEnd = strchr(buffer, '\r');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}

		szEnd = strchr(buffer, '\n');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}

		// Skip if line "total...", "Total ...", ...
	#ifdef _WIN32
		if (!_strnicmp (buffer, "total", 5))
	#else
		if (!strncasecmp (buffer, "total", 5))
	#endif
			continue;

		// Get the first token (permissions).
		tok = strtok (buffer, " ");
		if (!tok)
			continue;

		// Decide whether we deal with a file or a directory
		switch (*tok){
			case '-':
				info.m_nAttrib = 0;
				//TRACE("PLAINFILE;\n");
				break;
			case 'd':
				info.m_nAttrib = ATTR_DIR;
				//TRACE("DIRECTORY;");
				break;
			case 'l':
				info.m_nAttrib = ATTR_LINK;
				//TRACE("SYMLINK;");
				break;
			default:
				info.m_nAttrib = 0;
				//TRACE("UNKOWN;");
				break;
		}

		//TOFIX code into my portable flags!
		//info.m_dwAttributes = symperms (tok + 1);
		//TRACE("Permissions %d ", info.m_dwAttributes);

		// Errnoeous and ignoring entries are treated equally for now.
		error = ignore = 0;
		year = hour = min = sec = 0; // Silence the compiler.
		month = day = 0;
		next = -1;

		/* While there are tokens on the line, parse them.  Next is the
		number of tokens left until the filename. Use the month-name token
		as the "anchor" (the place where the position wrt the file name is "known").
		When a month name is encountered, `next' is set to 5.  Also, the preceding
		characters are parsed to get the file size. This tactic is quite dubious when
		it comes to	internationalization issues (non-English month names), but it works for now.*/
		while ((tok = strtok (NULL, " ")))
		{
			--next;
			if (next < 0)		/* a month name was not encountered */
			{
				for (j = 0; j < 12; j++)
					if (!strcmp (tok, months[j]))
						break;
				/* If we got a month, it means the token before it is the
				size, and the filename is three tokens away.  */
				if (j != 12)
				{
					char *t = tok - 2;
					long mul = 1;

					for (info.m_nSize = 0; t > buffer && isdigit(*t); mul *= 10, t--)
						info.m_nSize += mul * (*t - '0');
					if (t == buffer)
					{
						//TRACE("ERROR: t == buffer\n");
						error = 1; // Something is seriously wrong.
						break;
					}
					
					month = j;
					next = 5;
					//TRACE("month: %s; ", months[month]);
				}
			}
			else if (next == 4)	/* days */
			{
				if (tok[1])	/* two-digit... */
					day = 10 * (*tok - '0') + tok[1] - '0';
				else		/* ...or one-digit */
					day = *tok - '0';
				//TRACE("day: %d; ", day);
			}
			else if (next == 3)
			{
				/* This ought to be either the time, or the year.  Let's
				be flexible!
				If we have a number x, it's a year.  If we have x:y,
				it's hours and minutes.  If we have x:y:z, z are
				seconds.  */
				year = 0;
				min = hour = sec = 0;
				/* We must deal with digits.  */
				if (isdigit(*tok))
				{
					// Suppose it's year.
					for ( ; isdigit(*tok); tok++)
					year = (*tok - '0') + 10 * year;
					if (*tok == ':')
					{
						/* This means these were hours!  */
						hour = year;
						year = 0;
						++tok;
						/* Get the minutes...  */
						for (; isdigit(*tok); tok++)
							min = (*tok - '0') + 10 * min;
						if (*tok == ':')
						{
							/* ...and the seconds.  */
							++tok;
							for (; isdigit (*tok); tok++)
								sec = (*tok - '0') + 10 * sec;
						}
					}
				}
				/*
				if (year)
					TRACE("year: %d (no tm); \n", year);
				else
					TRACE("time: %02d:%02d:%02d (no yr);\n", hour, min, sec);
				*/
			}
			else if (next == 2)    /* The file name */
			{
				int fnlen;
				char *p;

				/* Since the file name may contain a SPC, it is possible
				for strtok to handle it wrong.  */
				fnlen = strlen (tok);
				if (fnlen < len - (tok - buffer))
				{
					// So we have a SPC in the file name.  Restore the original.
					tok[fnlen] = ' ';
					// If the file is a symbolic link, it should have a	` -> ' somewhere.
					if (info.m_nAttrib == ATTR_LINK)
					{
						p = strstr (tok, " -> ");
						if (!p)
						{
							//TRACE("ERROR: !p\n");
							error = 1;
							break;
						}
						
						//cur.linkto = xstrdup (p + 4); //TOFIX da li ovo podrati
						//TRACE("link to: %s\n", cur.linkto);
						*p = '\0';	// And separate it from the file name.
					}
				}
				// If we have the filename, add it to the list of files or directories.
				// "." is an exception!  */
				if (!strcmp (tok, "."))
				{
					//TRACE("\nIgnoring `.';\n");
					ignore = 1;
					break;
				}
				/* Some FTP sites choose to have ls -F as their default
				LIST output, which marks the symlinks with a trailing
				`@', directory names with a trailing `/' and
				executables with a trailing `*'.  This is no problem
				unless encountering a symbolic link ending with `@',
				or an executable ending with `*' on a server without
				default -F output.  I believe these cases are very
				rare.  */
				fnlen = strlen (tok); /* re-calculate `fnlen' */
				char buff[256];
				memcpy (buff, tok, fnlen + 1);
				buff[fnlen + 1] = '\0';
				
				info.m_strName = buff;

				if (fnlen)
				{
					if (info.m_nAttrib == ATTR_DIR && info.m_strName.at(fnlen - 1) == '/')
					{
						info.m_strName.at(fnlen - 1) = '\0';
						//TRACE("trailing `/' on dir.\n");
					}
					else if (info.m_nAttrib == ATTR_LINK && info.m_strName.at(fnlen - 1) == '@')
					{
						info.m_strName.at(fnlen - 1) = '\0';
						//TRACE("trailing `@' on link.\n");
					}
					else if (info.m_nAttrib == 0 /*&& (info.m_dwAttributes & 0111)*/ && info.m_strName.at(fnlen - 1) == '*')
					{
						info.m_strName.at(fnlen - 1) = '\0';
						//TRACE("trailing `*' on exec.\n");
					}
				} // if (fnlen)
				else{
					//TRACE("ERROR !fnlen\n");
					error = 1;
				}
				break;
			}
			else
				abort ();	//TOFIX
		} // while

		if (info.m_strName.IsEmpty() /*|| (cur.type == VfsItem::FT_SYMLINK && !cur.linkto)*/)
		{
			//TRACE("ERROR filename empty.\n");
			error = 1;
		}
		
		if (error || ignore)
		{
			//TRACE("Skipping.\n");
			continue;
		}

		/* Get the current time.  */
		timenow = time (NULL);
		tnow = localtime (&timenow);
		// Build the time-stamp (the idea by zaga@fly.cc.fer.hr)
		timestruct.tm_sec   = sec;
		timestruct.tm_min   = min;
		timestruct.tm_hour  = hour;
		timestruct.tm_mday  = day;
		timestruct.tm_mon   = month;

		if (year == 0)
		{
			// Some listings will not specify the year if it is "obvious"
			// that the file was from the previous year.  E.g. if today
			// is 97-01-12, and you see a file of Dec 15th, its year is
			// 1996, not 1997.  Thanks to Vladimir Volovich for mentioning this!
			if (month > tnow->tm_mon)
				timestruct.tm_year = tnow->tm_year - 1;
			else
				timestruct.tm_year = tnow->tm_year;
		}
		else
			timestruct.tm_year = year;

		if (timestruct.tm_year >= 1900)
			timestruct.tm_year -= 1900;

		timestruct.tm_wday  = 0;
		timestruct.tm_yday  = 0;
		timestruct.tm_isdst = -1;

		info.m_nLastModDate = mktime (&timestruct); /* store the time-stamp */
		info.CalcExt();

		out.Insert(info);
		info.Clear();
	}

	//TOFIX do this automatically
//	out.FilterList();	//important

	//insert ".." dir (if it is not listed)
	if(-1 == out.FindItem(".."))
	{
		info.m_nAttrib		= ATTR_DIR;
		info.m_strName		= "..";
		info.m_nSize		= -1;
		info.m_nLastModDate = 0;
		info.CalcExt();

		out.Insert(info);
	}

	return true;
}

bool CFtpListParser::ParseWinNT(CStrList &list, VfsListing &out)
{
	char *tok;	// tokenizer
	char buffer[1024] = "";

	VfsItem info;

	// Line loop to end of file:
	for(unsigned int i=0; i<list.size(); i++)
	{
		int len = list[i].size();

		strncpy(buffer, list[i].c_str(), 1023);
		buffer[len] = '\0';

		//TRACE("Line buffer: %s\n", buffer);
		char *szEnd = strchr(buffer, '\r');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}

		szEnd = strchr(buffer, '\n');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}

		// Get the first token (date)
		tok = strtok (buffer, " ");
		if (!tok)
			continue;

		// Build the time-stamp
		struct tm timestamp;
		memset(&timestamp, 0, sizeof(timestamp));

		parse_date(tok, timestamp);

		// Get the next token (time)
		tok = strtok (NULL, " ");
		if (!tok)
			continue;

		parse_time(tok, timestamp);

		// Get the next token ("<DIR>" or file size)
		tok = strtok (NULL, " ");
		if (!tok)
			continue;

		if(0 == strcmp(tok, "<DIR>"))
		{
			info.m_nSize = -1;
			info.m_nAttrib = ATTR_DIR;
		}
		else
		{
			info.m_nSize = atoi(tok);
			info.m_nAttrib = 0;
		}

		//The rest of the string is filename (CAN have spaces inside) 
		// Get the next token (file name)
		tok = strtok (NULL, "\n");
		if (!tok)
			continue;
		
		while(tok && isspace(*tok))
			tok ++;

		info.m_strName = tok;

		if (info.m_strName.IsEmpty())
		{
			//TRACE("ERROR filename empty.\n");
		}

		info.m_nLastModDate = mktime(&timestamp);
		info.CalcExt();

		out.Insert(info);
		info.Clear();
	}

	//insert ".." dir (if it is not listed)
	if(-1 == out.FindItem(".."))
	{
		info.m_nAttrib		= ATTR_DIR;
		info.m_strName		= "..";
		info.m_nSize		= -1;
		info.m_nLastModDate = 0;
		info.CalcExt();

		out.Insert(info);
	}

	return true;
}

// VMS listing sample:
//-------------------------------------------------
// 
//ANON:[PUBLIC]
//
//GUNZIP.EXE;51             221  29-JUL-1998 19:03 [ANONYMOUS] (RWED,RE,RE,RE)
//HTMLENV.DIR;1               1   8-JUN-1997 01:18 [ANONYMOUS] (RWED,RE,RE,RE)
//LIST-ARCHIVES.DIR;1         2  16-FEB-2000 07:05 [SYSTEM] (RWE,RWE,RE,RE)
//TECHPORT.TXT;1             57  23-APR-2003 17:24 [ANONYMOUS] (RWED,RWED,RE,)
//UNZIP.EXE;1               324  27-FEB-2001 21:17 [ANONYMOUS] (RWED,RE,RE,RE)
//UNZIP_VAX.EXE;1           195  27-FEB-2001 21:17 [ANONYMOUS] (RWED,RE,RE,RE)
//
//
//Total of 800 blocks in 6 files.
//
//-------------------------------------------------
bool CFtpListParser::ParseVMS(CStrList &list, VfsListing &out)
{
	char *tok;	// tokenizer
	char buffer[1024] = "";

	VfsItem info;

	// Line loop to end of file:
	for(unsigned int i=0; i<list.size(); i++)
	{
		int len = list[i].size();

		strncpy(buffer, list[i].c_str(), 1023);
		buffer[len] = '\0';

		//TRACE("Line buffer: %s\n", buffer);
		char *szEnd = strchr(buffer, '\r');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}
		
		szEnd = strchr(buffer, '\n');
		if(NULL != szEnd){
			*szEnd = '\0';
			len = strlen(buffer);
		}

		// Get the first token (name)
		szEnd = strchr(buffer, ';');
		if(!szEnd)
			continue;

		*szEnd = '\0';
		szEnd ++;

		info.m_strName = buffer;

		//skip? next token (file version)
		const char *szPos = strtok(szEnd, " \t");
		if(!szPos)
			continue;

		//next token is file size
		szPos = strtok(NULL, " \t");
		if(!szPos)
			continue;

		info.m_nSize = atoi(szPos);
		
		if(info.m_nSize > 0L)	//TOFIX better guessing
			info.m_nAttrib	= 0;
		else
			info.m_nAttrib	= ATTR_DIR;

		//next token is file date
		szPos = strtok(NULL, " \t");
		if(!szPos)
			continue;

		// Build the time-stamp
		struct tm timestamp;
		memset(&timestamp, 0, sizeof(timestamp));
		parse_date(szPos, timestamp);

		// Get the next token (time)
		tok = strtok (NULL, " ");
		if (!tok)
			continue;

		parse_time2(tok, timestamp);

		if (info.m_strName.IsEmpty())
		{
			//TRACE("ERROR filename empty.\n");
		}
		info.m_nLastModDate = mktime(&timestamp);
		info.CalcExt();

		out.Insert(info);
		info.Clear();
	}

	//insert ".." dir (if it is not listed)
	if(-1 == out.FindItem(".."))
	{
		info.m_nAttrib		= ATTR_DIR;
		info.m_strName		= "..";
		info.m_nSize		= -1;
		info.m_nLastModDate	= 0;
		info.CalcExt();

		out.Insert(info);
	}

	return true;
}

void parse_date(const char *str, struct tm &timestamp)
{
	//ASSERT(strlen(str)>=8);

	char temp[3];
	strncpy(temp, str, sizeof(temp));
	temp[2] = '\0';

	int nMonth = atoi(temp);

	strncpy(temp, str+3, sizeof(temp));
	temp[2] = '\0';

	int nDay = atoi(temp);

	strncpy(temp, str+6, sizeof(temp));
	temp[2] = '\0';

	int nYear = 1900 + atoi(temp);
	if(nYear < 1980)
		nYear += 100;

	timestamp.tm_mon   = nMonth;
	timestamp.tm_mday  = nDay;
	timestamp.tm_year  = nYear;
	timestamp.tm_wday  = 0;
	timestamp.tm_yday  = 0;
	timestamp.tm_isdst = -1;
}

void parse_time(const char *str, struct tm &timestamp)
{
	//ASSERT(strlen(str)>=7);

	char temp[3];
	strncpy(temp, str, sizeof(temp));
	temp[2] = '\0';

	int nHour = atoi(temp);

	strncpy(temp, str+3, sizeof(temp));
	temp[2] = '\0';

	int nMinute = atoi(temp);

	strncpy(temp, str+5, sizeof(temp));
	temp[2] = '\0';
	if(0 == strcmp(temp, "PM"))
	{
		nHour += 12;
		if(nHour >= 24)
			nHour = 0;
	}

	timestamp.tm_sec   = 0;
	timestamp.tm_hour  = nHour;
	timestamp.tm_min   = nMinute;
}

// Converts symbolic permissions to number-style ones, e.g. string rwxr-xr-x to 755.
// For now, it knows nothing of setuid/setgid/sticky. ACLs are ignored.
int symperms (const char *s)
{
	int perms = 0, i;

	if (strlen (s) < 9)
		return 0;
	for (i = 0; i < 3; i++, s += 3)
	{
		perms <<= 3;
		perms += (((s[0] == 'r') << 2) + ((s[1] == 'w') << 1) +
		(s[2] == 'x' || s[2] == 's'));
	}
	return perms;
}

//VMS time "hh:mm"
void parse_time2(const char *str, struct tm &timestamp)
{
	//ASSERT(strlen(str)==5);

	char temp[3];
	strncpy(temp, str, sizeof(temp));
	temp[2] = '\0';

	int nHour = atoi(temp);

	strncpy(temp, str+3, sizeof(temp));
	temp[2] = '\0';

	int nMinute = atoi(temp);

	timestamp.tm_sec   = 0;
	timestamp.tm_hour  = nHour;
	timestamp.tm_min   = nMinute;
}

