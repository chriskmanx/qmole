/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  
 * 02111-1307, USA.
 */

#ifndef IE_IMPEXP_DOCBOOK_H
#define IE_IMPEXP_DOCBOOK_H

#define TT_OTHER			0	// anything else
#define TT_DOCUMENT			1	// a document <book>
#define TT_SECTION			2	// section <section>
#define TT_BLOCK			3	// a paragraph <para>
#define TT_PHRASE			4	// formatted text
#define TT_EMPHASIS			5	// emphasized (italic) text
#define TT_SUPERSCRIPT		6	// superscript
#define TT_SUBSCRIPT		7	// subscript
#define TT_BLOCKQUOTE		8	// block quote
#define TT_BRIDGEHEAD		9	// heading  <bridgehead ...>
#define TT_CHAPTER			10	// legacy abiword documents
#define TT_TITLE			11	// title
#define	TT_PAGEBREAK		12	// <beginpage>
#define	TT_PLAINTEXT		13
#define	TT_LINK				14
#define	TT_ULINK			15
#define	TT_BOOKMARK			16
#define	TT_FIGURE			17
#define	TT_MEDIAOBJECT		18
#define	TT_IMAGEOBJECT		19
#define	TT_IMAGEDATA		20
#define TT_TABLE			21	// table without a title <informaltable>
#define TT_COLSPEC			22	// <colspec>
#define TT_TBODY			23	// <tbody>
#define TT_TGROUP			24	// <tgroup>
#define TT_ROW				25	// table row <row>
#define TT_ENTRY			26	// table cell <entry>
#define TT_FOOTNOTE			27	// footnote <footnote>
#define TT_BOOKINFO			28	// <bookinfo>
#define TT_AUTHOR			29	// <author>
#define TT_HONORIFIC		30	// <honorific>
#define TT_PERSONNAME		31	// <personname>
#define TT_FIRSTNAME		32	// <firstname>
#define TT_SURNAME			33	// <surname>
#define TT_OTHERNAME		34	// <othername>
#define TT_KEYWORDSET		35	// <keywordset>
#define TT_KEYWORD			36	// <keyword>
#define TT_PUBLISHER		37	// <publisher>
#define TT_PUBLISHERNAME	38	// <publishername>
#define TT_ABSTRACT			39	// <abstract>
#define TT_DATE				40	// <date>
#define TT_LEGALNOTICE		41	// <legalnotice>
#define TT_SUBJECTSET		42	// <subjectset>
#define TT_SUBJECT			43	// <subject>
#define TT_SUBJECTTERM		44	// <subjectterm>
#define TT_COLLAB			45	// a collaborator <collab>
#define TT_COLLABNAME		46	// <collabname>
#define TT_REVHISTORY		47	// <revhistory>
#define TT_REVISION			48	// <revision>
#define TT_REVNUMBER		49	// <revnumber>
#define TT_REVREMARK		50	// <revremark>
#define TT_BIBLIOMISC		51	// <bibliomisc>
#define TT_APPLICATION		52	// <application>
#define TT_ENTRYTBL			53	// nested table <entrytbl>
#define TT_TEXTOBJECT		54	// <textobject>
#define TT_INLINEEQUATION	55	// <inlineequation>
#define TT_MATHPHRASE		56	// <mathphrase>
#define TT_INFORMALFIGURE	57	// a figure without a title <informalfigure>
#define TT_FOOTNOTEREF		58	// <footnoteref>
#define TT_ITEMIZEDLIST		59	// <itemizedlist>
#define TT_LISTITEM			60	// <listitem>
#define TT_TOC				61	// table of contents <toc>
#define TT_TOCBACK			62	// <tocback>
#define TT_TOCCHAP			63	// <tocchap>
#define TT_TOCFRONT			64	// <tocfront>
#define TT_TOCPART			65	// <tocpart>
#define TT_TOCLEVEL1		66	// <toclevel1>
#define TT_TOCLEVEL2		67	// <toclevel2>
#define TT_TOCLEVEL3		68	// <toclevel3>
#define TT_TOCLEVEL4		69	// <toclevel4>
#define TT_TOCLEVEL5		70	// <toclevel5>
#define TT_COL				71	// <col>
#define TT_THEAD			72	// table header <thead>
#define TT_TFOOT			73	// table footer <tfoot>
#define TT_QUOTE			74	// <quote>
#define TT_EMAIL			75	// <email>
#define TT_BIBLIOCOVERAGE	76	// <bibliocoverage>
#define TT_BIBLIORELATION	77	// <bibliorelation>
#define TT_BIBLIOSOURCE		78	// <bibliosource>
#define TT_REVDESCRIPTION	79	// <revdescription>
#define TT_ABBREVIATION		80	// <abbreviation>
#define TT_ACRONYM			81	// <acronym>
#define TT_SECTIONINFO		82	// <sectioninfo> and <sect1info> - <sect5info>
#define TT_PART				83	// <part>
#define TT_ARTICLE			84	// <article>
#define TT_PUBDATE			85	// <pubdate>
#define TT_AUTHORGROUP		86	// <authorgroup>
#define TT_INDEX			87	// <index>
#define TT_INDEXDIV			88	// <indexdiv>
#define TT_INDEXENTRY		89	// <indexentry>
#define TT_INDEXINFO		90	// <indexinfo>
#define TT_INDEXTERM		91	// <indexterm>
#define TT_PRIMARY			92	// <primary>
#define TT_SECONDARY		93	// <secondary>
#define TT_TERTIARY			94	// <tertiary>
#define TT_PRIMARYIE		95	// <primaryie>
#define TT_SECONDARYIE		96	// <secondaryie>
#define TT_TERTIARYIE		97	// <tertiaryie>
#define TT_SEE				98	// <see>
#define TT_SEEALSO			99	// <seealso>
#define TT_SEEIE			100	// <seeie>
#define TT_ARTICLEINFO		101	// <articleinfo>
#define TT_PREFACE			102	// <preface>
#define TT_PREFACEINFO		103	// <prefaceinfo>
#define TT_EQUATION			104 // <equation>, <inlineequation>, <informalequation>
#define TT_GRAPHIC			105	// <graphic>
#define TT_CITETITLE		106	// <citetitle>
#define TT_PRODUCTNAME		107	// <productname>
#define TT_PRODUCTNUMBER	108	// <productnumber>
#define TT_INVPARTNUMBER	109	// <invpartnumber>
#define TT_VARNAME			110	// <varname>
#define TT_COMMAND			111	// <command>
#define TT_ORDEREDLIST		112	// <orderedlist>
#define TT_FORMALPARA		113	// <formalpara>
#define TT_TIP				114	// <tip>
#define TT_APPENDIX			115	// <appendix>
#define TT_APPENDIXINFO		116	// <appendixinfo>
#define TT_GLOSSARY			117	// <glossary>
#define TT_ACKNO			118	// <ackno>
#define TT_ALT				119	// <alt>
#define TT_VIDEOOBJECT		120	// <videoobject>
#define TT_VIDEODATA		121	// <videodata>
#define TT_YEAR				122	// <year>
#define TT_COPYRIGHT		123	// <copyright>
#define TT_EDITION			124	// <edition>
#define TT_SET				125	// <set>
#define TT_SCREENSHOT		126	// <screenshot>
#define TT_SCREENINFO		127	// <screeninfo>
#define TT_EDITOR			128	// <editor>
#define TT_CHAPTERINFO		129	// <chapterinfo>
#define TT_PRINTHISTORY		130	// <printhistory>
#define TT_COLOPHON			131	// <colophon>
#define TT_DEDICATION		132	// <dedication>
#define TT_PARTINFO			133	// <partinfo>
#define TT_PARTINTRO		134	// <partintro>
#define TT_AUTHORBLURB		135	// <authorblurb>
#define TT_BIBLIOGRAPHY		136	// <bibliography>
#define TT_BIBLIOENTRY		137	// <biblioentry>
#define TT_BIBLIOMIXED		138	// <bibliomixed>
#define TT_BIBLIOMSET		139	// <bibliomset>
#define TT_BIBLIOSET		140	// <biblioset>
#define TT_AUTHORINITIALS	141	// <authorinitials>
#define TT_ARTPAGENUMS		142	// <artpagenums>
#define TT_VOLUMENUM		143	// <volumenum>
#define TT_ISSUENUM			144	// <issuenum>
#define TT_HOLDER			145	// <holder>
#define TT_REFENTRY			146	// <refentry>
#define TT_REFSYNOPSISDIV	147	// <refsynopsisdiv>
#define TT_SGMLTAG			148	// <sgmltag>
#define TT_ATTRIBUTION		149	// <attribution>
#define TT_LINEAGE			150	// <lineage>
#define TT_BIBLIODIV		151	// <bibliodiv>
#define TT_EPIGRAPH			152	// <epigraph>
#define TT_ITERMSET			153	// <itermset>
#define TT_VARIABLELIST		154	// <variablelist>
#define TT_CMDSYNOPSIS		155	// <cmdsynopsis>
#define TT_SEGMENTEDLIST	156	// <segmentedlist>
#define TT_SEGLISTITEM		157	// <seglistitem>
#define TT_SEG				158	// <seg>
#define TT_AREA				159	// <area>
#define TT_AREASET			160	// <areaset>
#define TT_AREASPEC			161	// <areaspec>
#define TT_GRAPHICCO		162	// <graphicco>
#define TT_IMAGEOBJECTCO	163	// <imageobjectco>
#define TT_MEDIAOBJECTCO	164	// <mediaobjectco>
#define TT_FUNCSYNOPSIS		165	// <funcsynopsis>
#define TT_SCREEN			166	// <screen>
#define TT_SYNOPSIS			167	// <synopsis>
#define TT_SIMPARA			168	// <simpara>

#endif
