////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////
/*
 * console.c: various interactive-prompt routines shared between
 * the console PuTTY tools
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "putty.h"
#include "storage.h"
#include "ssh.h"

int console_batch_mode = FALSE;

/*
 * Clean up and exit.
 */
void cleanup_exit(int code)
{
    /*
     * Clean up.
     */
	//TOFIX
	//sk_cleanup();
	//WSACleanup();

    if (cfg.protocol == PROT_SSH) {
		random_save_seed();
	#ifdef MSCRYPTOAPI
		crypto_wrapup();
	#endif
    }

    //exit(code);
}

void verify_ssh_host_key(CSshSession &session, char *host, int port, char *keytype, char *keystr, char *fingerprint)
{
    int ret, choice = 0;
    //HANDLE hin;
    //DWORD savemode, i;

    static const char absentmsg_batch[] =
	"The server's host key is not cached in the registry. You\n"
	"have no guarantee that the server is the computer you\n"
	"think it is.\n"
	"The server's key fingerprint is:\n"
	"%s\n"
	"Connection abandoned.\n";
    static const char absentmsg[] =
	"The server's host key is not cached in the registry. You\n"
	"have no guarantee that the server is the computer you\n"
	"think it is.\n"
	"The server's key fingerprint is:\n"
	"%s\n"
	"If you trust this host, press \"Yes\" to add the key to\n"
	"PuTTY's cache and carry on connecting.\n"
	"If you want to carry on connecting just once, without\n"
	"adding the key to the cache, press \"No\".\n"
	"If you do not trust this host, press \"Cancel\" to abandon the\n"
	"connection.\n"
	"Store key in cache? (y/n) ";

    static const char wrongmsg_batch[] =
	"WARNING - POTENTIAL SECURITY BREACH!\n"
	"The server's host key does not match the one PuTTY has\n"
	"cached in the registry. This means that either the\n"
	"server administrator has changed the host key, or you\n"
	"have actually connected to another computer pretending\n"
	"to be the server.\n"
	"The new key fingerprint is:\n"
	"%s\n"
	"Connection abandoned.\n";
    static const char wrongmsg[] =
	"WARNING - POTENTIAL SECURITY BREACH!\n"
	"The server's host key does not match the one PuTTY has\n"
	"cached in the registry. This means that either the\n"
	"server administrator has changed the host key, or you\n"
	"have actually connected to another computer pretending\n"
	"to be the server.\n"
	"The new key fingerprint is:\n"
	"%s\n"
	"If you were expecting this change and trust the new key,\n"
	"press \"Yes\" to update Atol cache and continue connecting.\n"
	"If you want to carry on connecting but without updating\n"
	"the cache, press \"No\".\n"
	"If you want to abandon the connection completely, press\n"
	"\"Cancel\". Pressing \"Cancel\" is the ONLY guaranteed\n"
	"safe choice.\n"
	"Update cached key? (y/n, \"Cancel\" cancels connection) ";

    static const char abandoned[] = "Connection abandoned.\n";

//    char line[32];

    /*
     * Verify the key against the registry.
     */
    ret = verify_host_key(host, port, keytype, keystr);

    if (ret == 0)		       /* success - key matched OK */
		return;

	char szBuffer[512];

    if (ret == 2) {		       /* key was different */
		if (console_batch_mode) {
			//TOFIX fprintf(stderr, wrongmsg_batch, fingerprint);
			sprintf(szBuffer, wrongmsg_batch, fingerprint);
			if(session.m_fnKeyWarning)
				session.m_fnKeyWarning(szBuffer, 0, session.m_fnKeyData);

			cleanup_exit(1);
		}
		//TOFIX fprintf(stderr, wrongmsg, fingerprint);
		//TOFIX fflush(stderr);
		sprintf(szBuffer, wrongmsg, fingerprint);
		if(session.m_fnKeyWarning)
			choice = session.m_fnKeyWarning(szBuffer, 1, session.m_fnKeyData);
    }
    if (ret == 1) {		       /* key was absent */
		if (console_batch_mode) {
		    //TOFIX fprintf(stderr, absentmsg_batch, fingerprint);
			sprintf(szBuffer, absentmsg_batch, fingerprint);
			if(session.m_fnKeyWarning)
				session.m_fnKeyWarning(szBuffer, 0, session.m_fnKeyData);

			cleanup_exit(1);
		}

		//TOFIX fprintf(stderr, absentmsg, fingerprint);
		//TOFIX fflush(stderr);
		sprintf(szBuffer, absentmsg, fingerprint);
		if(session.m_fnKeyWarning)
			choice = session.m_fnKeyWarning(szBuffer, 1, session.m_fnKeyData);
    }

    //hin = GetStdHandle(STD_INPUT_HANDLE);
	//if(hin)
	{
		//GetConsoleMode(hin, &savemode);
		//SetConsoleMode(hin, (savemode | ENABLE_ECHO_INPUT |
		//		 ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT));
		//ReadFile(hin, line, sizeof(line) - 1, &i, NULL);
		//SetConsoleMode(hin, savemode);

	    //if (line[0] != '\0' && line[0] != '\r' && line[0] != '\n') {
		if(choice > 0){
			//if (line[0] == 'y' || line[0] == 'Y')
			if(1 == choice)
				store_host_key(host, port, keytype, keystr);
		} else {
			//TOFIX fprintf(stderr, abandoned);
			sprintf(szBuffer, abandoned, fingerprint);
			if(session.m_fnKeyWarning)
				session.m_fnKeyWarning(szBuffer, 0, session.m_fnKeyData);
			cleanup_exit(0);
		}
	}
}

/*
 * Ask whether the selected cipher is acceptable (since it was
 * below the configured 'warn' threshold).
 * cs: 0 = both ways, 1 = client->server, 2 = server->client
 */
void askcipher(char *ciphername, int cs)
{
    HANDLE hin;
    DWORD savemode, i;

    static const char msg[] =
	"The first %scipher supported by the server is\n"
	"%s, which is below the configured warning threshold.\n"
	"Continue with connection? (y/n) ";
    static const char msg_batch[] =
	"The first %scipher supported by the server is\n"
	"%s, which is below the configured warning threshold.\n"
	"Connection abandoned.\n";
    static const char abandoned[] = "Connection abandoned.\n";

    char line[32];

    if (console_batch_mode) {
		//TOFIX fprintf(stderr, msg_batch,
		//TOFIX 	(cs == 0) ? "" :
		//TOFIX 	(cs == 1) ? "client-to-server " : "server-to-client ",
		//TOFIX 	ciphername);
	cleanup_exit(1);
    }

    //TOFIX fprintf(stderr, msg,
	//TOFIX     (cs == 0) ? "" :
	//TOFIX	    (cs == 1) ? "client-to-server " : "server-to-client ",
	//TOFIX     ciphername);
    //fflush(stderr);

    hin = GetStdHandle(STD_INPUT_HANDLE);
    GetConsoleMode(hin, &savemode);
    SetConsoleMode(hin, (savemode | ENABLE_ECHO_INPUT |
			 ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT));
    ReadFile(hin, line, sizeof(line) - 1, &i, NULL);
    SetConsoleMode(hin, savemode);

    if (line[0] == 'y' || line[0] == 'Y') {
	return;
    } else {
	//TOFIX fprintf(stderr, abandoned);
	cleanup_exit(0);
    }
}

/*
 * Ask whether to wipe a session log file before writing to it.
 * Returns 2 for wipe, 1 for append, 0 for cancel (don't log).
 */
int askappend(char *filename)
{
    HANDLE hin;
    DWORD savemode, i;

    static const char msgtemplate[] =
	"The session log file \"%.*s\" already exists.\n"
	"You can overwrite it with a new session log,\n"
	"append your session log to the end of it,\n"
	"or disable session logging for this session.\n"
	"Enter \"y\" to wipe the file, \"n\" to append to it,\n"
	"or just press Return to disable logging.\n"
	"Wipe the log file? (y/n, Return cancels logging) ";

    static const char msgtemplate_batch[] =
	"The session log file \"%.*s\" already exists.\n"
	"Logging will not be enabled.\n";

    char line[32];

    if (cfg.logxfovr != LGXF_ASK) {
		return ((cfg.logxfovr == LGXF_OVR) ? 2 : 1);
    }
    if (console_batch_mode) {
		//TOFIX fprintf(stderr, msgtemplate_batch, FILENAME_MAX, filename);
		//TOFIX fflush(stderr);
		return 0;
    }
    //TOFIX fprintf(stderr, msgtemplate, FILENAME_MAX, filename);
    //fflush(stderr);

    hin = GetStdHandle(STD_INPUT_HANDLE);
    GetConsoleMode(hin, &savemode);
    SetConsoleMode(hin, (savemode | ENABLE_ECHO_INPUT |
			 ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT));
    ReadFile(hin, line, sizeof(line) - 1, &i, NULL);
    SetConsoleMode(hin, savemode);

    if (line[0] == 'y' || line[0] == 'Y')
	return 2;
    else if (line[0] == 'n' || line[0] == 'N')
	return 1;
    else
	return 0;
}

/*
 * Warn about the obsolescent key file format.
 */
void old_keyfile_warning(void)
{
    static const char message[] =
	"You are loading an SSH 2 private key which has an\n"
	"old version of the file format. This means your key\n"
	"file is not fully tamperproof. Future versions of\n"
	"PuTTY may stop supporting this private key format,\n"
	"so we recommend you convert your key to the new\n"
	"format.\n"
	"\n"
	"Once the key is loaded into PuTTYgen, you can perform\n"
	"this conversion simply by saving it again.\n";

    fputs(message, stderr);
}

//TOFIX move to CSshSession
void logevent(CSshSession &session, char *string)
{
	if(session.m_dbgFn)
		session.m_dbgFn(string, session.m_dwDbgData);
}

char *console_password = NULL;

int console_get_line(const char *prompt, char *str,
			    int maxlen, int is_pw)
{
    HANDLE hin, hout;
    DWORD savemode, newmode, i;

    if (is_pw && console_password) {
	static int tried_once = 0;

	if (tried_once) {
	    return 0;
	} else {
	    strncpy(str, console_password, maxlen);
	    str[maxlen - 1] = '\0';
	    tried_once = 1;
	    return 1;
	}
    }

    if (console_batch_mode) {
	if (maxlen > 0)
	    str[0] = '\0';
    } else {
	hin = GetStdHandle(STD_INPUT_HANDLE);
	hout = GetStdHandle(STD_OUTPUT_HANDLE);
	if (hin == INVALID_HANDLE_VALUE || hout == INVALID_HANDLE_VALUE) {
	    //TOFIX fprintf(stderr, "Cannot get standard input/output handles\n");
	    cleanup_exit(1);
	}

	if (hin == NULL || hout == NULL)
		return 1;

	GetConsoleMode(hin, &savemode);
	newmode = savemode | ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT;
	if (is_pw)
	    newmode &= ~ENABLE_ECHO_INPUT;
	else
	    newmode |= ENABLE_ECHO_INPUT;
	SetConsoleMode(hin, newmode);

	WriteFile(hout, prompt, strlen(prompt), &i, NULL);
	ReadFile(hin, str, maxlen - 1, &i, NULL);

	SetConsoleMode(hin, savemode);

	if ((int) i > maxlen)
	    i = maxlen - 1;
	else
	    i = i - 2;
	str[i] = '\0';

	if (is_pw)
	    WriteFile(hout, "\r\n", 2, &i, NULL);

    }
    return 1;
}
