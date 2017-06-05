/* AbiSource
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
 * Copyright 2009 AbiSource Corporation B.V.
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
 
// Class definition include
#include "ODe_Table_Listener.h"

// Internal includes
#include "ODe_AuxiliaryData.h"
#include "ODe_Common.h"
#include "ODe_ListenerAction.h"
#include "ODe_AutomaticStyles.h"
#include "ODe_Style_Style.h"
#include "ODe_Text_Listener.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <gsf/gsf-output-memory.h>


/**
 * Constructor
 */
ODe_Table_Listener::ODe_Table_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                                       GsfOutput* pTextOutput,
                                       ODe_AuxiliaryData& rAuxiliaryData,
                                       UT_uint8 zIndex,
                                       UT_uint8 spacesOffset)
                                       :
                                       ODe_AbiDocListenerImpl(spacesOffset),
                                       m_pColumns(NULL),
                                       m_numColumns(0),
                                       m_pRows(NULL),
                                       m_numRows(0),
                                       m_pTextOutput(pTextOutput),
                                       m_rAutomatiStyles(rAutomatiStyles),
                                       m_rAuxiliaryData(rAuxiliaryData),
                                       m_zIndex(zIndex)
{
}


/**
 * Destructor
 */
ODe_Table_Listener::~ODe_Table_Listener() {
    DELETEPV(m_pColumns);
    DELETEPV(m_pRows);
    UT_VECTOR_PURGEALL(ODe_Table_Cell*, m_cells);
    UT_VECTOR_PURGEALL(UT_UTF8String*, columnStyleNames);
    UT_VECTOR_PURGEALL(UT_UTF8String*, rowStyleNames);
}


/**
 * 
 */
void ODe_Table_Listener::openTable(const PP_AttrProp* pAP,
                                   ODe_ListenerAction& /*rAction*/) {
    const gchar* pValue;
    bool ok;
    const gchar* pVar;
    ODe_Style_Style* pStyle;
    std::string buffer;
    UT_UTF8String styleName;

    m_rAuxiliaryData.m_tableCount++;
    UT_UTF8String_sprintf(m_tableName, "Table%u", m_rAuxiliaryData.m_tableCount);

    if (ODe_Style_Style::hasTableStyleProps(pAP)) {
        m_tableStyleName = m_tableName; // Plain simple
        
        pStyle = m_rAutomatiStyles.addTableStyle(m_tableStyleName);
        pStyle->fetchAttributesFromAbiTable(pAP);
        pStyle = NULL; // We're done with it.
                       // OBS: There's no need to delete it as it will be done
                       //      later by ODe_AutomaticStyles destructor.
    }
  
    
    // We don't have to check if there are any properties to export at all,
    // because AbiWord has different default cell style properties than OpenDocument,
    // which means we'll always have to export the styles. This will only result in
    // writing out redundant properties when the user-selected style properties 
    // exactly match the default OpenDocument default properties; we will just ignore
    // this case.
    m_tableWideCellStyle.fetchAttributesFromAbiCell(pAP);

    // NOTE: Don't use the number of table-column-props values to determine
    // the number of columns in the table. There can be more values than there
    // are actual columns; see bug 11863 for an example.
    m_numColumns = 0;
    UT_uint32 curColProp = 0;
    ok = pAP->getProperty("table-column-props", pValue);
    if (ok && pValue != NULL) {
        pVar = pValue;
        while (*pVar != 0) {
            if (*pVar == '/') {
                if (!buffer.empty()) {
                    curColProp++;
                    UT_UTF8String_sprintf(styleName, "%s.col%u",
                                          m_tableName.utf8_str(), curColProp);
                                          
                    pStyle = m_rAutomatiStyles.addTableColumnStyle(styleName);
                    pStyle->setColumnWidth(buffer.c_str());

                    columnStyleNames.addItem(new UT_UTF8String(styleName));
                    
                    buffer.clear();
                } else {
                    columnStyleNames.addItem(new UT_UTF8String(""));
                }
            } else {
                buffer += *pVar;
            }
            pVar++;
        }
    }

    buffer.clear();

    // NOTE: Don't use the number of table-row-height values to determine
    // the number of rows in the table. There can be more values than there
    // are actual rows; see bug 11863 for an example.
    m_numRows = 0;
    UT_uint32 curRowProp = 0;
    ok = pAP->getProperty("table-row-heights", pValue);
    if (ok && pValue != NULL) {
        pVar = pValue;
        while (*pVar != 0) {
            if (*pVar == '/') {
                if (!buffer.empty()) {
                    curRowProp++;
                    UT_UTF8String_sprintf(styleName, "%s.row%u",
                                          m_tableName.utf8_str(), curRowProp);

                    pStyle = m_rAutomatiStyles.addTableRowStyle(styleName);
					// Row heights values in AbiWord really are *minimum* row
					//  heights; the property name is unfortunate.
                    pStyle->setMinRowHeight(buffer.c_str());
                    
                    rowStyleNames.addItem(new UT_UTF8String(styleName));

                    buffer.clear(); // Clear the buffer.
                } else {
                    rowStyleNames.addItem(new UT_UTF8String(""));
                }
            } else {
                buffer += *pVar;
            }
            pVar++;
        }
    }
}


/**
 * 
 */
void ODe_Table_Listener::closeTable(ODe_ListenerAction& rAction) {
    UT_sint32 i;
    UT_UTF8String output;
    
    _buildTable();
    
    _printSpacesOffset(output);
    output += "<table:table table:name=\"";
    output += m_tableName;
    output += "\"";

    ODe_writeAttribute(output, "table:style-name", m_tableStyleName);
    output += ">\n";
    
    ODe_writeToFile(m_pTextOutput, output);
    m_spacesOffset++;

    
    output.clear();
    _printSpacesOffset(output);
    
    for (i=0; i<m_numColumns; i++) {
        m_pColumns[i].write(m_pTextOutput, output);
    }
    
    for (i=0; i<m_numRows; i++) {
        m_pRows[i].write(m_pTextOutput, output);
    }


    output.clear();
    m_spacesOffset--;
    _printSpacesOffset(output);
    output += "</table:table>\n";
    ODe_writeToFile(m_pTextOutput, output);

    rAction.popListenerImpl();
}


/**
 * 
 */
void ODe_Table_Listener::openCell(const PP_AttrProp* pAP,
                                  ODe_ListenerAction& rAction) {
                                    
    ODe_Table_Cell* pCell;
    ODe_Text_Listener* pTextListener;
    ODe_Style_Style* pCellStyle;
             
    // Create the table cell.
    pCell = new ODe_Table_Cell();
    m_cells.addItem(pCell);
                                      
    pCell->loadAbiProps(pAP);

    ////    
    // Try to figure out the table dimensions.
    
    // If this cell is in the rightmost column than we have found the number
    // of columns. If it's not, it will do no harm.
    if (m_numColumns < pCell->m_rightAttach) {
        m_numColumns = pCell->m_rightAttach;
    }
    
    // If this cell is in the last row than we have found the number
    // of rows. If it's not, it will do no harm.
    if (m_numRows < pCell->m_bottomAttach) {
        m_numRows = pCell->m_bottomAttach;
    }
    
    
    ////
    // Define its style
    
    UT_UTF8String_sprintf(pCell->m_styleName, "%s_col%u_row%u",
                          m_tableName.utf8_str(),
                          (pCell->m_leftAttach)+1,
                          (pCell->m_topAttach)+1);
                              
    pCellStyle = m_rAutomatiStyles.addTableCellStyle(pCell->m_styleName);

    // First inherit various the cell style properties from the table style.
    pCellStyle->inheritTableCellProperties(m_tableWideCellStyle);

    // Then load the style properties that are specific for this cell.
    // We don't have to check if there are any properties to export at all,
    // because AbiWord has different default cell style properties than OpenDocument,
    // which means we'll always have to export the styles. This will only result in
    // writing out redundant properties when the user-selected style properties 
    // exactly match the default OpenDocument default properties; we will just ignore
    // this case.
    pCellStyle->fetchAttributesFromAbiCell(pAP);
    

    ////
    // Prepare to read its contents
    
    pCell->m_pTextContent = gsf_output_memory_new ();
    UT_ASSERT(pCell->m_pTextContent != NULL);
    
    pTextListener = new ODe_Text_Listener(m_rAutomatiStyles,
        pCell->m_pTextContent,
        m_rAuxiliaryData,
        m_zIndex,
        // currentOffset + <table:table> + <table:table-row> + <table:table-cell>
        m_spacesOffset+3);
    rAction.pushListenerImpl(pTextListener, true);
}


/**
 * 
 */
void ODe_Table_Listener::_buildTable() {
    
    UT_sint32 i, j;
    ODe_Table_Cell* pCell;
    
    UT_return_if_fail(m_numRows > 0);
    UT_return_if_fail(m_numColumns > 0);
    
    // Create the columns
    m_pColumns = new ODe_Table_Column[m_numColumns];

    for (i=0; (i<m_numColumns) && (i<columnStyleNames.getItemCount()); i++) {
        if (columnStyleNames[i]) {
            m_pColumns[i].m_styleName = *(columnStyleNames[i]);
        }
    }


    // Create the rows
    m_pRows = new ODe_Table_Row[m_numRows];


    for (i=0; (i<m_numRows) && (i<rowStyleNames.getItemCount()); i++) {
        if (rowStyleNames[i]) {
            m_pRows[i].m_styleName = *(rowStyleNames[i]);
        }
    }

    
    // Create the vectors that will hold the cells into its corresponding rows.
    for (i=0; i<m_numRows; i++) {
        m_pRows[i].m_ppCells = new ODe_Table_Cell*[m_numColumns];
        m_pRows[i].m_columnCount = m_numColumns;
        
        for (j=0; j<m_numColumns; j++) {
            m_pRows[i].m_ppCells[j] = NULL;
        }
    }
    
    // Position cells in table
    for (i=0; i<m_cells.getItemCount(); i++) {
        pCell = m_cells.getNthItem(i);

        UT_continue_if_fail(pCell)
        UT_continue_if_fail(pCell->m_topAttach < m_numRows);
        UT_continue_if_fail(pCell->m_leftAttach < m_numColumns);
        
        m_pRows[pCell->m_topAttach].m_ppCells[pCell->m_leftAttach] = pCell;
    }
}


/**
 * 
 */
void ODe_Table_Cell::loadAbiProps(const PP_AttrProp* pAP) {
    const gchar* pValue = NULL;
    bool ok = false;
    
    ok = pAP->getProperty("left-attach", pValue);
    if (!ok || pValue == NULL) {
        UT_DEBUGMSG(("ODe_Table_Cell::loadAbiProps(): missing left-attach property\n"));
        return;
    }
    m_leftAttach = atoi(pValue);
    
    ok = pAP->getProperty("right-attach", pValue);
    if (!ok || pValue == NULL) {
        UT_DEBUGMSG(("ODe_Table_Cell::loadAbiProps(): missing right-attach property\n"));
        return;
    }
    m_rightAttach = atoi(pValue);

    ok = pAP->getProperty("top-attach", pValue);
    if (!ok || pValue == NULL) {
        UT_DEBUGMSG(("ODe_Table_Cell::loadAbiProps(): missing top-attach property\n"));
        return;
    }
    m_topAttach = atoi(pValue);

    ok = pAP->getProperty("bot-attach", pValue);
    if (!ok || pValue == NULL) {
        UT_DEBUGMSG(("ODe_Table_Cell::loadAbiProps(): missing bot-attach property\n"));
        return;
    }
    m_bottomAttach = atoi(pValue);

    // A few sanity checks
    UT_ASSERT(m_leftAttach   <  m_rightAttach);
    UT_ASSERT(m_topAttach    <  m_bottomAttach);
    
    
    ////
    // Define some cell properties

    if (m_rightAttach - m_leftAttach > 1) {
        UT_UTF8String_sprintf(m_numberColumnsSpanned, "%d",
                              m_rightAttach-m_leftAttach);
    }
    
    if (m_bottomAttach - m_topAttach > 1) {
        UT_UTF8String_sprintf(m_numberRowsSpanned, "%d",
                              m_bottomAttach - m_topAttach);
    }
}


/**
 * 
 */
void ODe_Table_Cell::write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset) {
    UT_UTF8String output;
    
    output = rSpacesOffset;
    output += "<table:table-cell";
    ODe_writeAttribute(output, "table:style-name", m_styleName);
    if(m_numberColumnsSpanned.size() > 0)
      ODe_writeAttribute(output, "table:number-columns-spanned", m_numberColumnsSpanned);
    if(m_numberRowsSpanned.size() > 0)
      ODe_writeAttribute(output, "table:number-rows-spanned", m_numberRowsSpanned);

    output += ">\n";
    ODe_writeToFile(pTableOutput, output);
    gsf_output_write (pTableOutput, gsf_output_size (m_pTextContent),
		      gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (m_pTextContent)));

    output = rSpacesOffset;
    output += "</table:table-cell>\n";
    ODe_writeToFile(pTableOutput, output);
}


/**
 * 
 */
void ODe_Table_Column::write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset) {
    UT_UTF8String output;
    
    output = rSpacesOffset;
    output += "<table:table-column";
    ODe_writeAttribute(output, "table:style-name", m_styleName);
    output += "/>\n";
                          
    ODe_writeToFile(pTableOutput, output);
}


/**
 * 
 */
void ODe_Table_Row::write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset) {
    UT_UTF8String output;
    UT_UTF8String cellsOffset;
    UT_uint32 i;
    
    output = rSpacesOffset;
    output += "<table:table-row";
    ODe_writeAttribute(output, "table:style-name", m_styleName);
    output += ">\n";
    
    ODe_writeToFile(pTableOutput, output);
    
    cellsOffset = rSpacesOffset;
    cellsOffset += " ";
    
    for (i=0; i<m_columnCount; i++) {
        if (m_ppCells[i] != NULL) {
            m_ppCells[i]->write(pTableOutput, cellsOffset);
        } else {
            // It's a covered cell.
            output = cellsOffset;
            output += "<table:covered-table-cell/>\n";
            ODe_writeToFile(pTableOutput, output);
        }
    }

    output = rSpacesOffset;
    output += "</table:table-row>\n";
    ODe_writeToFile(pTableOutput, output);
}


/**
 * 
 */
ODe_Table_Row::ODe_Table_Row() 
	: m_ppCells(NULL),
	m_columnCount(0)
{
}


/**
 * 
 */
ODe_Table_Row::~ODe_Table_Row() {
    DELETEPV(m_ppCells);
}
