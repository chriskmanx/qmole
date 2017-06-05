<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

  <xsl:import href="/usr/share/xml/docbook/stylesheet/nwalsh/current/xhtml/docbook.xsl"/>
<!--
  <xsl:param name="generate.component.toc">0</xsl:param>
-->
  <xsl:template name="head.content">
    <link rel="stylesheet" href="../style.css" type="text/css" media="all"/>
    <title><xsl:value-of select='/book/bookinfo/title'/></title>
  </xsl:template>

  <xsl:template match="guimenuitem">
    <span class="guimenuitem">
      <xsl:call-template name="inline.charseq"/>
    </span>
  </xsl:template>

  <xsl:template match="function">
    <span class="function"><xsl:apply-templates/></span>
  </xsl:template>

  <xsl:template match="parameter">
    <span class="parameter"><xsl:apply-templates/></span>
  </xsl:template>

  <xsl:template match="filename">`<tt class="filename"><xsl:apply-templates/></tt>'</xsl:template>

  <xsl:template match="keycap">
    <span class="keycap"><xsl:apply-templates/></span>
  </xsl:template>

  <xsl:template match="book">
    <div class='chapter'>
      <xsl:call-template name="book.titlepage"/>
      <xsl:call-template name="division.toc"/>
    </div>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="othercredit" mode="titlepage.mode">
    <h3 class="{name(.)}"><xsl:call-template name="person.name"/></h3>
    <xsl:apply-templates mode="titlepage.mode" select="./contrib"/>
    <xsl:apply-templates mode="titlepage.mode" select="./affiliation"/>
  </xsl:template>

  <xsl:template match="emphasis">
    <xsl:choose>
      <xsl:when test="@role='underline'">
        <u><xsl:apply-templates/></u>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-imports/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="citation">
    <xsl:text>[</xsl:text>
    <xsl:variable name="cited"><xsl:value-of select="."/></xsl:variable>
    <a href="#{generate-id(/book/bibliography/bibliomixed[string(abbrev/.) = $cited])}">
      <xsl:call-template name="inline.charseq"/>
    </a>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="bibliomixed">
    <div xmlns="http://www.w3.org/1999/xhtml" class="{name(.)}" id="{generate-id(.)}">
      <xsl:call-template name="anchor"/>
      <p class="{name(.)}">
        <xsl:call-template name="biblioentry.label"/>
        <xsl:apply-templates mode="bibliomixed.mode"/>
      </p>
    </div>
  </xsl:template>

  <xsl:template match="email">
    <xsl:call-template name="inline.monoseq">
      <xsl:with-param name="content">
        <xsl:text>&lt;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&gt;</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>
