%option nounput never-interactive

%{
/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (c) 1999-2007 by Hiroyuki Yamamoto & The Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include "quote_fmt_lex.h"
#include "quote_fmt_parse.h"

%}

%option prefix="quote_fmt"
%option outfile="lex.yy.c"

%s S_NORMAL S_DATE

%{
/*
 * see notes below.
 */
int quote_fmt_firsttime = 1;
int line = -1;
int escaped_string = 0;
%}

%%

%{
/*
 * NOTES:
 * this lex script used to use characters also in use
 * by strftime() (which we want to use for custom
 * time formats in replies and templates). to circumvent
 * this we have to play a little bit with states.
 *
 * these are the characters we also want to use in the
 * %D time customizer:
 *
 * %a %A %b %B %c %C %d %H %I %j %m %M %p %S %w %x %y %Y %Z
 *
 * you can use these characters too, but don't forget to
 * prepend them with the <S_NORMAL> state.
 *
 * also there is also work around for resetting the state
 * (firsttime variable). this assumes that yylex() will
 * always return to S_NORMAL after quote fmt parsing is
 * done.
 */
%} 

%{
	if (quote_fmt_firsttime) {
		BEGIN S_NORMAL;
		quote_fmt_firsttime = 0;
	}	
%}

<S_NORMAL>("%X"|"%cursor") /* cursor pos */ return SET_CURSOR_POS;
<S_NORMAL>("%A"|"%email") /* email address */ return SHOW_MAIL_ADDRESS;
<S_NORMAL>("%c"|"%cc") /* cc */ return SHOW_CC;
<S_NORMAL>("%d"|"%date") /* date */ return SHOW_DATE;
<S_NORMAL>("%D"|"%date_fmt") /* date */ { BEGIN S_DATE; return SHOW_DATE_EXPR; }
<S_NORMAL>("%f"|"%from") /* from */ return SHOW_FROM;
<S_NORMAL>("%F"|"%firstname") /* first name */ return SHOW_FIRST_NAME;
<S_NORMAL>("%i"|"%messageid") /* message-id */ return SHOW_MESSAGEID;
<S_NORMAL>("%I"|"%initials") /* initial of sender */ return SHOW_SENDER_INITIAL;
<S_NORMAL>("%m"|"%msg_no_sig") /* message with no signature */ return SHOW_MESSAGE_NO_SIGNATURE;
<S_NORMAL>("%M"|"%msg") /* message */ return SHOW_MESSAGE;
<S_NORMAL>("%n"|"%newsgroups") /* newsgroups */ return SHOW_NEWSGROUPS;
<S_NORMAL>("%N"|"%fullname") /* full name */ return SHOW_FULLNAME;
<S_NORMAL>("%L"|"%lastname") /* last name */ return SHOW_LAST_NAME;
<S_NORMAL>("%r"|"%references") /* references */ return SHOW_REFERENCES;
<S_NORMAL>("%s"|"%subject") /* subject */ return SHOW_SUBJECT;
<S_NORMAL>("%t"|"%to") /* to */ return SHOW_TO;
<S_NORMAL>("%T"|"%dict") /* current dictionary */ return SHOW_DICT;
<S_NORMAL>("%tags") /* tags */ return SHOW_TAGS;
<S_NORMAL>("%Q"|"%quoted_msg") /* quoted message */ return SHOW_QUOTED_MESSAGE;
<S_NORMAL>("%q"|"%quoted_msg_no_sig") /* quoted message with no signature */ return SHOW_QUOTED_MESSAGE_NO_SIGNATURE;
<S_NORMAL>("%af"|"%account_fullname") /* full name in compose account */ return SHOW_ACCOUNT_FULL_NAME;
<S_NORMAL>("%am"|"%account_email") /* mail address in compose account */ return SHOW_ACCOUNT_MAIL_ADDRESS;
<S_NORMAL>("%an"|"%account_name") /* compose account name itself */ return SHOW_ACCOUNT_NAME;
<S_NORMAL>("%ao"|"%account_org") /* organization in compose account */ return SHOW_ACCOUNT_ORGANIZATION;
<S_NORMAL>("%as"|"%account_sig") /* signature in compose account */ return SHOW_ACCOUNT_SIG;
<S_NORMAL>("%asp"|"%account_sigpath") /* signature path in compose account */ return SHOW_ACCOUNT_SIGPATH;
<S_NORMAL>("%aT"|"%account_dict") /* main dict (if enabled) in compose account */ return SHOW_ACCOUNT_DICT;
<S_NORMAL>("%ABc"|"%addrbook_cc") /* completion of 'Cc' from the address book */ return SHOW_ADDRESSBOOK_COMPLETION_FOR_CC;
<S_NORMAL>("%ABf"|"%addrbook_from") /* completion of 'From' from the address book */ return SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM;
<S_NORMAL>("%ABt"|"%addrbook_to") /* completion of 'To' from the address book */ return SHOW_ADDRESSBOOK_COMPLETION_FOR_TO;
"\\\%" /* % */ return SHOW_PERCENT;
"\\\\" /* \ */ return SHOW_BACKSLASH;
"\\t"|"\t" /* tab */ return SHOW_TAB;
"\n" /* return */ { line++; return SHOW_EOL; }
"\\n" /* escaped return */ { if (escaped_string) line++; return SHOW_EOL; }
"\\?" /* ? */ return SHOW_QUESTION_MARK;
"\\!" return SHOW_EXCLAMATION_MARK;
"\\|" return SHOW_PIPE;
"\\{" return SHOW_OPARENT;
"\\}" return SHOW_CPARENT;
("?d"|"?date") /* query date */ return QUERY_DATE;
("?f"|"?from") /* query from */ return QUERY_FROM;
("?N"|"?F"|"?L"|"?I"|"?fullname"|"?firstname"|"?lastname"|"?initials") /* query from name */ return QUERY_FULLNAME;
("?s"|"?subject") /* query subject */ return QUERY_SUBJECT;
("?t"|"?to") /* query to */ return QUERY_TO;
("?T"|"?dict") /* query current dictionary set and enabled */ return QUERY_DICT;
("?c"|"?cc") /* query cc */ return QUERY_CC;
("?n"|"?newsgroups") /* query newsgroups */ return QUERY_NEWSGROUPS;
("?i"|"?messageid") /* query message-id */ return QUERY_MESSAGEID;
("?r"|"?references") /* query references */ return QUERY_REFERENCES;
("?af"|"?account_fullname") /* query full name in compose account */ return QUERY_ACCOUNT_FULL_NAME;
("?ao"|"?account_org") /* query organization in compose account */ return QUERY_ACCOUNT_ORGANIZATION;
("?as"|"?account_sig") /* query signature */ return QUERY_ACCOUNT_SIG;
("?asp"|"?account_sigpath") /* query signature path */ return QUERY_ACCOUNT_SIGPATH;
("?aT"|"?account_dict") /* query account main dict enabled */ return QUERY_ACCOUNT_DICT;
("?ABc"|"?addrbook_cc") /* query completion for 'Cc' in address book */ return QUERY_CC_FOUND_IN_ADDRESSBOOK;
("?ABf"|"?addrbook_from") /* query completion for 'From' in address book */ return QUERY_FROM_FOUND_IN_ADDRESSBOOK;
("?ABt"|"?addrbook_to") /* query completion for 'To' in address book */ return QUERY_TO_FOUND_IN_ADDRESSBOOK;
("!d"|"!date") /* query not(date) */ return QUERY_NOT_DATE;
("!f"|"!from") /* query not(from) */ return QUERY_NOT_FROM;
("!N"|"!F"|"!L"|"!I"|"!fullname"|"!firstname"|"!lastname"|"!initials") /* query not(from name) */ return QUERY_NOT_FULLNAME;
("!s"|"!subject") /* query not(subject) */ return QUERY_NOT_SUBJECT;
("!t"|"!to") /* query not(to) */ return QUERY_NOT_TO;
("!T"|"!dict") /* query not(current dictionary set and enabled) */ return QUERY_NOT_DICT;
("!c"|"!cc") /* query not(cc) */ return QUERY_NOT_CC;
("!n"|"!newsgroups") /* query not(newsgroups) */ return QUERY_NOT_NEWSGROUPS;
("!i"|"!messageid") /* query not(message-id) */ return QUERY_NOT_MESSAGEID;
("!r"|"!references") /* query not(references) */ return QUERY_NOT_REFERENCES;
("!af"|"!account_fullname") /* query not(full name in compose account) */ return QUERY_NOT_ACCOUNT_FULL_NAME;
("!ao"|"!account_org") /* query not(organization in compose account) */ return QUERY_NOT_ACCOUNT_ORGANIZATION;
("!as"|"!account_sig") /* query not(signature) */ return QUERY_NOT_ACCOUNT_SIG;
("!asp"|"!account_sigpath") /* query not(signature path) */ return QUERY_NOT_ACCOUNT_SIGPATH;
("!aT"|"!account_dict") /* query not(account main dict enabled and set) */ return QUERY_NOT_ACCOUNT_DICT;
("!ABc"|"!addrbook_cc") /* query not(completion for 'Cc' in address book) */ return QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK;
("!ABf"|"!addrbook_from") /* query not(completion for 'From' in address book) */ return QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK;
("!ABt"|"!addrbook_to") /* query not(completion for 'To' in address book) */ return QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK;
("|f"|"|file") /* insert file */ return INSERT_FILE;
("|p"|"|program") /* insert program output */ return INSERT_PROGRAMOUTPUT;
("|i"|"|input") /* insert user input */ return INSERT_USERINPUT;
("|a"|"|attach") /* attach file */ return ATTACH_FILE;
<S_DATE>"{" return OPARENT;
<S_DATE>"}" { BEGIN S_NORMAL; return CPARENT; }
<S_NORMAL>"{" return OPARENT;
<S_NORMAL>"}" return CPARENT;
. { yylval.chr = yytext[0]; return CHARACTER; }

%%
