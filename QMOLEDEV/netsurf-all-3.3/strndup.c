<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<title>strndup.c</title>
<style type="text/css">
.enscript-comment { font-style: italic; color: rgb(178,34,34); }
.enscript-function-name { font-weight: bold; color: rgb(0,0,255); }
.enscript-variable-name { font-weight: bold; color: rgb(184,134,11); }
.enscript-keyword { font-weight: bold; color: rgb(160,32,240); }
.enscript-reference { font-weight: bold; color: rgb(95,158,160); }
.enscript-string { font-weight: bold; color: rgb(188,143,143); }
.enscript-builtin { font-weight: bold; color: rgb(218,112,214); }
.enscript-type { font-weight: bold; color: rgb(34,139,34); }
.enscript-highlight { text-decoration: underline; color: 0; }
</style>
</head>
<body id="top">
<h1 style="margin:8px;" id="f1">strndup.c&nbsp;&nbsp;&nbsp;<span style="font-weight: normal; font-size: 0.5em;">[<a href="?txt">plain text</a>]</span></h1>
<hr/>
<div></div>
<pre>
<span class="enscript-comment">/* Implement the strndup function.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Written by Kaveh R. Ghazi &lt;<a href="mailto:ghazi@caip.rutgers.edu">ghazi@caip.rutgers.edu</a>&gt;.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */</span>

<span class="enscript-comment">/*

@deftypefn Extension char* strndup (const char *@var{s}, size_t @var{n})

Returns a pointer to a copy of @var{s} with at most @var{n} characters
in memory obtained from @code{malloc}, or @code{NULL} if insufficient
memory was available.  The result is always NUL terminated.

@end deftypefn

*/</span>

#<span class="enscript-reference">include</span> <span class="enscript-string">&quot;ansidecl.h&quot;</span>
#<span class="enscript-reference">include</span> <span class="enscript-string">&lt;stddef.h&gt;</span>

<span class="enscript-type">extern</span> size_t	strlen (<span class="enscript-type">const</span> <span class="enscript-type">char</span>*);
<span class="enscript-type">extern</span> PTR	malloc (size_t);
<span class="enscript-type">extern</span> PTR	memcpy (PTR, <span class="enscript-type">const</span> PTR, size_t);

<span class="enscript-type">char</span> *
<span class="enscript-function-name">strndup</span> (<span class="enscript-type">const</span> <span class="enscript-type">char</span> *s, size_t n)
{
  <span class="enscript-type">char</span> *result;
  size_t len = strlen (s);

  <span class="enscript-keyword">if</span> (n &lt; len)
    len = n;

  result = (<span class="enscript-type">char</span> *) malloc (len + 1);
  <span class="enscript-keyword">if</span> (!result)
    <span class="enscript-keyword">return</span> 0;

  result[len] = <span class="enscript-string">'\0'</span>;
  <span class="enscript-keyword">return</span> (<span class="enscript-type">char</span> *) memcpy (result, s, len);
}
</pre>
<hr />
</body></html>