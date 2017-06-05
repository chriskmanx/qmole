<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text"
            encoding="ISO-8859-1"
	    indent="no"/>

    <xsl:template match='/'>
	    <xsl:apply-templates select='/book/appendix/refentry/*'/>
    </xsl:template>

    <xsl:template match='refmeta'>
	    <xsl:text>.TH </xsl:text>
	    <xsl:value-of select="refentrytitle"/>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="manvolnum"/>
	    <xsl:text> "</xsl:text>
	    <xsl:value-of select="/book/bookinfo/copyright/year[1]"/>
	    <xsl:text>" "</xsl:text>
	    <xsl:value-of select="/book/bookinfo/author/firstname"/>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="/book/bookinfo/author/surname"/>
	    <xsl:text>" ""
</xsl:text>
    </xsl:template>

    <xsl:template match='refnamediv'>
	<xsl:text>.SH NAME
</xsl:text>
	<xsl:value-of select="refname"/> \- <xsl:value-of select="refpurpose"/>
	<xsl:text>
</xsl:text>
    </xsl:template>
	
    <xsl:template match='refsynopsisdiv'>
	<xsl:text>.SH SYNOPSIS
</xsl:text>
<xsl:apply-templates select='cmdsynopsis/*'/><xsl:text>
</xsl:text>
    </xsl:template>

    <xsl:template match='command'>
	    <xsl:text>
.B </xsl:text><xsl:value-of select='.'/><xsl:text>
</xsl:text>
    </xsl:template>

    <xsl:template match='arg'>
	    <xsl:if test='@choice="opt"'>[</xsl:if>
	    <xsl:apply-templates/>
	    <xsl:if test='@choice="opt"'>]</xsl:if>
	    <xsl:if test='@rep="repeat"'>...</xsl:if><xsl:text> </xsl:text>
    </xsl:template>

    <xsl:template match='refsect1'>
	    <xsl:text>.SH </xsl:text>
	    <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match='para'>
	    <xsl:text>
.PP
</xsl:text>
	    <xsl:apply-templates/><xsl:text>
</xsl:text>
    </xsl:template>

    <xsl:template match='varlistentry'>
	    <xsl:text>
.TP
</xsl:text><xsl:apply-templates select='term'/><xsl:text>
</xsl:text>
<xsl:apply-templates select='listitem/*' mode='nopara'/>
    </xsl:template>

    <xsl:template match='term'>
	    <xsl:apply-templates/>
	    <xsl:if test='position()&lt;last()'><xsl:text>, </xsl:text></xsl:if>
    </xsl:template>

    <xsl:template match='option'>
	    <xsl:text>\fB</xsl:text><xsl:value-of select='.'/><xsl:text>\fP</xsl:text>
    </xsl:template>

    <xsl:template match='title'><xsl:apply-templates/></xsl:template>
	
    <xsl:template match='*'>
	    <xsl:text> </xsl:text><xsl:apply-templates/><xsl:text> </xsl:text>
    </xsl:template>

    <xsl:template match='text()'>
	    <xsl:value-of select='normalize-space(.)'/>
    </xsl:template>

    <xsl:template match='ulink'>
	    <xsl:text>
</xsl:text><xsl:value-of select='@url'/>
    </xsl:template>
	
    <xsl:template match='email'>
	    <xsl:text> &lt;</xsl:text><xsl:apply-templates/><xsl:text>&gt;</xsl:text>
    </xsl:template>

    <xsl:template match='member'>
	    <xsl:apply-templates/>
	    <xsl:if test='position()&lt;last()'><xsl:text>,
</xsl:text></xsl:if>
    </xsl:template>

    <xsl:template match='xref' mode='nopara'>
	    <xsl:text>the manual</xsl:text>
    </xsl:template>

    <xsl:template match='simplelist'>
	<xsl:apply-templates select='member'/>
    </xsl:template>
	
</xsl:stylesheet>