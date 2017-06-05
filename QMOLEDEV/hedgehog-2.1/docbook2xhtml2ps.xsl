<?xml version="1.0" encoding="iso-8859-1"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

    <xsl:param name="version" value="unknown"/>

    <xsl:output method="html" encoding="utf-8"
		doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"/>

    <xsl:template match="book">
    	<html>
	    <xsl:call-template name="make-book-head"/>
	    <xsl:call-template name="make-body"/>
	</html>
    </xsl:template>

    <xsl:template name="make-book-head">
    	<head>
	    <title>
	    	<xsl:value-of select="bookinfo/title"/>
		<xsl:text> </xsl:text>
		<xsl:value-of select="$version"/>
	    </title>
	    <link rel="stylesheet" href="oliodoc.css" type="text/css"/>
	</head>
    </xsl:template>

    <xsl:template match="article">
    	<html>
	    <xsl:call-template name="make-article-head"/>
	    <xsl:call-template name="make-body"/>
	</html>
    </xsl:template>

    <xsl:template name="make-article-head">
    	<head>
	    <title>
	    	<xsl:value-of select="artheader/title"/>
		<xsl:text> </xsl:text>
		<xsl:value-of select="$version"/>
	    </title>
	    <link rel="stylesheet" href="oliodoc.css" type="text/css"/>
	</head>
    </xsl:template>

    <xsl:template name="make-body">
    	<body>
	    <xsl:apply-templates/>
	</body>
    </xsl:template>
    
    <xsl:template name="make-authors">
	<xsl:apply-templates select="bookinfo/author | artheader/author"/>
    </xsl:template>

    <xsl:template name="make-toc">
    	<h1>Contents</h1>
    	<ul>
	    <xsl:apply-templates select="sect1" mode="toc"/>
	</ul>
    </xsl:template>
    
    <xsl:template match="sect1 | sect2" mode="toc">
    	<li>
	    <a href="#{@id}">
		<xsl:apply-templates select="title" mode="toc"/>
	    </a>
	    <xsl:if test="sect2">
	    	<ul>
		    <xsl:apply-templates select="sect2" mode="toc"/>
		</ul>
	    </xsl:if>
	</li>
    </xsl:template>
    
    <xsl:template match="title" mode="toc">
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="author">
    	<p>
    	<xsl:value-of select="firstname"/>
	<xsl:text> </xsl:text>
    	<xsl:value-of select="surname"/>
	<xsl:text> (</xsl:text>
	<xsl:value-of select="affiliation/address/email"/>
	<xsl:text>)</xsl:text>
	</p>
    </xsl:template>

    <xsl:template match="chapter | sect1 | preface | appendix">
    	<h1>
	    <a name="{@id}">
		<xsl:value-of select="title"/>
	    </a>
	</h1>
	
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="sect2 | simplesect">
    	<h2>
	    <a name="{@id}">
		<xsl:value-of select="title"/>
	    </a>
	</h2>
	
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="para">
    	<p><xsl:apply-templates/></p>
    </xsl:template>
    
    <xsl:template match="emphasis | citetitle | replaceable">
    	<em><xsl:apply-templates/></em>
    </xsl:template>
    
    <xsl:template match="function | literal | command | option | filename | 
    	    	    	 markup | email | application | envar">
    	<code><xsl:apply-templates/></code>
    </xsl:template>
    
    <xsl:template match="prompt">
    	<strong><code><xsl:apply-templates/></code></strong>
    </xsl:template>
    
    <xsl:template match="userinput">
    	<em><code><xsl:apply-templates/></code></em>
    </xsl:template>
    
    <xsl:template match="ulink">
    	<a href="{@url}">
	    <xsl:apply-templates/>
	</a>
    </xsl:template>

    <xsl:template match="blockquote">
    	<blockquote>
	    <xsl:apply-templates/>
	</blockquote>
    </xsl:template>

    <xsl:template match="example">
    	<div class="example">
	    <xsl:if test="title">
		<p>
		    <strong>
		    	Example:
			<xsl:value-of select="title"/>
		    </strong>
		</p>
	    </xsl:if>
	    <xsl:apply-templates/>
	</div>
    </xsl:template>

    <xsl:template match="computeroutput | literallayout">
    	<pre><xsl:apply-templates/></pre>
    </xsl:template>

    <xsl:template match="programlisting | screen">
    	<blockquote><pre><xsl:apply-templates/></pre></blockquote>
    </xsl:template>

    <xsl:template match="orderedlist">
    	<ol>
	    <xsl:apply-templates/>
	</ol>
    </xsl:template>

    <xsl:template match="simplelist | itemizedlist">
    	<ul>
	    <xsl:apply-templates/>
	</ul>
    </xsl:template>

    <xsl:template match="member | listitem">
    	<li>
	    <xsl:apply-templates/>
	</li>
    </xsl:template>

    <xsl:template match="informaltable">
    	<table>
	    <xsl:apply-templates/>
	</table>
    </xsl:template>

    <xsl:template match="row">
    	<tr>
	    <xsl:apply-templates/>
	</tr>
    </xsl:template>

    <xsl:template match="entry">
    	<td>
	    <xsl:apply-templates/>
	</td>
    </xsl:template>
    
    <xsl:template match="figure">
    	<div class="figure">
	    <table class="figure">
		<tr>
		    <td>
			<img alt="{title}" src="{graphic/@fileref}.png"/>
		    </td>
		</tr>
		<tr>
		    <td>Figure: <xsl:value-of select="title"/></td>
		</tr>
	    </table>
    	</div>
    </xsl:template>
    
    <xsl:template match="table">
    	<div class="table">
	    <p class="tabletitle">
	    	Table:
	    	<xsl:value-of select="title"/>
	    </p>
	    <table>
		<xsl:apply-templates select="tgroup/row"/>
	    </table>
    	</div>
    </xsl:template>

    <xsl:template match="row">
    	<tr>
	    <xsl:apply-templates/>
	</tr>
    </xsl:template>
    
    <xsl:template match="entry[1]">
    	<td class="func">
	    <xsl:apply-templates/>
	</td>
    </xsl:template>

    <xsl:template match="entry[2]">
    	<td class="desc">
	    <xsl:apply-templates/>
	</td>
    </xsl:template>
    
    <xsl:template match="glosslist">
    	<dl>
	    <xsl:apply-templates/>
	</dl>
    </xsl:template>

    <xsl:template match="glossentry">
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="glossterm">
    	<dt>
	    <xsl:apply-templates/>
	</dt>
    </xsl:template>

    <xsl:template match="glossdef">
    	<dd>
	    <xsl:apply-templates/>
	</dd>
    </xsl:template>

    <xsl:template match="foreignphrase | tgroup | tbody | date">
    	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="title | toc | bookinfo | artheader | xref">
    </xsl:template>
    
    <xsl:template match="*">
    	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    </xsl:template>
    
</xsl:stylesheet>
