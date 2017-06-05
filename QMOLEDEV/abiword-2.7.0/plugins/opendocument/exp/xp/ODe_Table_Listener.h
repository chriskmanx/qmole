/* AbiSource
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
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

#ifndef ODE_TABLE_LISTENER_H_
#define ODE_TABLE_LISTENER_H_

// Internal includes
#include "ODe_AbiDocListenerImpl.h"
#include "ODe_Common.h"
#include "ODe_Style_Style.h"

// AbiWord includes
#include <ut_string_class.h>
#include <ut_vector.h>

// External includes
#include <stdio.h>

// Internal classes
class ODe_AutomaticStyles;
class ODe_AuxiliaryData;
class ODe_Styles;


/**
 * A <table:table-cell> element
 */
class ODe_Table_Cell {
public:

    ODe_Table_Cell() : m_pTextContent(NULL) {}
    
    ~ODe_Table_Cell() {
        if (m_pTextContent != NULL) {
            ODe_gsf_output_close(m_pTextContent);
        }
    }
    
    void loadAbiProps(const PP_AttrProp* pAP);
    
    void write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset);

    // <table:table-cell>
    UT_UTF8String m_numberColumnsSpanned; // table:number-columns-spanned
    UT_UTF8String m_numberRowsSpanned;    // table:number-columns-spanned
    UT_UTF8String m_styleName;            // table:style-name
    
    // A temporary holder for its text content.
    GsfOutput* m_pTextContent;
    
    // From AbiWord <cell> element
    UT_sint32 m_leftAttach, m_rightAttach, m_topAttach, m_bottomAttach;
};


/**
 * A table column. It just contains column formating info, not holding any cells.
 * The cells are stored on the table rows.
 */
class ODe_Table_Column {
public:
    void write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset);

    // <table:table-column>
    UT_UTF8String m_styleName; // table:style-name
};


/**
 * A table row. Contains row formating info and also holds its cells.
 */
class ODe_Table_Row {
public:

    ODe_Table_Row();
    ~ODe_Table_Row();
    
    void write(GsfOutput* pTableOutput, const UT_UTF8String& rSpacesOffset);

    // A NULL value means that this cell is covered, that it's a
    // <table:covered-table-cell/> element
    ODe_Table_Cell** m_ppCells;
    
    // <table:table-row>
    UT_UTF8String m_styleName; // table:style-name
    
    UT_uint32 m_columnCount;
};


/**
 * Handles an abi <table> element, writing, as output, it's OpenDocument
 * <table:table> counterpart.
 */
class ODe_Table_Listener : public ODe_AbiDocListenerImpl {
public:

    ODe_Table_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                       GsfOutput* pTextOutput,
                       ODe_AuxiliaryData& rAuxiliaryData,
                       UT_uint8 zIndex,
                       UT_uint8 spacesOffset);
                       
    virtual ~ODe_Table_Listener();

    virtual void openTable(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeTable(ODe_ListenerAction& rAction);
    
    virtual void openCell(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    
private:
    void _buildTable();
    
    ODe_Table_Column* m_pColumns;
    UT_sint32 m_numColumns;
    
    ODe_Table_Row* m_pRows;
    UT_sint32 m_numRows;
    
    UT_GenericVector<ODe_Table_Cell*> m_cells;
    
    GsfOutput* m_pTextOutput;
    ODe_AutomaticStyles& m_rAutomatiStyles;
    ODe_AuxiliaryData& m_rAuxiliaryData;
    UT_uint8 m_zIndex;
    
    UT_UTF8String m_tableName;
    UT_UTF8String m_tableStyleName;
    
    // Abiword has table-wide cell properties (i.e.: cell properties inside
    // <table> element), but OpenDocument doesn't.
    // So I have to propagate this properties into every cell of this table.
    ODe_Style_Style m_tableWideCellStyle;
    UT_GenericVector<UT_UTF8String*> columnStyleNames;
    UT_GenericVector<UT_UTF8String*> rowStyleNames;
};

#endif /*ODE_TABLE_LISTENER_H_*/
