////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

/*
 * PuTTY version numbering
 */

#define STR1(x) #x
#define STR(x) STR1(x)

#if defined SNAPSHOT

char ver[] = "Development snapshot " STR(SNAPSHOT);
char sshver[] = "Atol-Snapshot-" STR(SNAPSHOT);

#elif defined RELEASE

char ver[] = "Release " STR(RELEASE);
char sshver[] = "Atol-Release-" STR(RELEASE);

#else

char ver[] = "Unidentified build, " __DATE__ " " __TIME__;
//char sshver[] = "PuTTY-Local: " __DATE__ " " __TIME__;
char sshver[] = "Atol-0.5.5";	//TOFIX automatisation of the version change!

#endif

/*
 * SSH local version string MUST be under 40 characters. Here's a
 * compile time assertion to verify this.
 */
enum { vorpal_sword = 1 / (sizeof(sshver) <= 40) };
