#ifndef TEXTLABEL_H
#define TEXTLABEL_H


class FXAPI TextLabel : public FXFrame
{
    FXDECLARE(TextLabel)
protected:
    FXString      contents;       // Edited text
    const char *delimiters;     // Set of delimiters
    FXFont       *font;           // Text font
    FXColor       textColor;      // Text color
    FXColor       selbackColor;   // Selected background color
    FXColor       seltextColor;   // Selected text color
    FXColor       cursorColor;    // Color of the Cursor
    int         cursor;         // Cursor position
    int         anchor;         // Anchor position
    int         columns;        // Number of columns visible
    int         shift;          // Shift amount
    FXString      clipped;        // Clipped text
protected:
    TextLabel()
    {}
    int index(int x) const;
    int coord(int i) const;
    void drawTextRange(FXDCWindow& dc,int fm,int to);
    void drawTextFragment(FXDCWindow& dc,int x,int y,int fm,int to);
    int rightWord(int pos) const;
    int leftWord(int pos) const;
    int wordStart(int pos) const;
    int wordEnd(int pos) const;
private:
    TextLabel(const TextLabel&);
    TextLabel& operator=(const TextLabel&);
public:
    long onPaint(FXObject*,FXSelector,void*);
    long onUpdate(FXObject*,FXSelector,void*);
    long onKeyPress(FXObject*,FXSelector,void*);
    long onKeyRelease(FXObject*,FXSelector,void*);
    long onLeftBtnPress(FXObject*,FXSelector,void*);
    long onLeftBtnRelease(FXObject*,FXSelector,void*);
    long onMotion(FXObject*,FXSelector,void*);
    long onSelectionLost(FXObject*,FXSelector,void*);
    long onSelectionGained(FXObject*,FXSelector,void*);
    long onSelectionRequest(FXObject*,FXSelector,void* ptr);
    long onClipboardLost(FXObject*,FXSelector,void*);
    long onClipboardGained(FXObject*,FXSelector,void*);
    long onClipboardRequest(FXObject*,FXSelector,void*);
    long onFocusSelf(FXObject*,FXSelector,void*);
    long onFocusIn(FXObject*,FXSelector,void*);
    long onFocusOut(FXObject*,FXSelector,void*);
    long onAutoScroll(FXObject*,FXSelector,void*);
    long onCmdCursorHome(FXObject*,FXSelector,void*);
    long onCmdCursorEnd(FXObject*,FXSelector,void*);
    long onCmdCursorRight(FXObject*,FXSelector,void*);
    long onCmdCursorLeft(FXObject*,FXSelector,void*);
    long onCmdCursorWordLeft(FXObject*,FXSelector,void*);
    long onCmdCursorWordRight(FXObject*,FXSelector,void*);
    long onCmdCursorWordStart(FXObject*,FXSelector,void*);
    long onCmdCursorWordEnd(FXObject*,FXSelector,void*);
    long onCmdMark(FXObject*,FXSelector,void*);
    long onCmdExtend(FXObject*,FXSelector,void*);
    long onCmdSelectAll(FXObject*,FXSelector,void*);
    long onCmdDeselectAll(FXObject*,FXSelector,void*);
    long onCmdCopySel(FXObject*,FXSelector,void*);
    long onUpdHaveSelection(FXObject*,FXSelector,void*);
    long onUpdSelectAll(FXObject*,FXSelector,void*);
public:

    // Default text delimiters
    static const char textDelimiters[];

public:

    enum
    {
        ID_CURSOR_HOME=FXFrame::ID_LAST,
        ID_CURSOR_END,
        ID_CURSOR_RIGHT,
        ID_CURSOR_LEFT,
        ID_CURSOR_WORD_LEFT,
        ID_CURSOR_WORD_RIGHT,
        ID_CURSOR_WORD_START,
        ID_CURSOR_WORD_END,
        ID_MARK,
        ID_EXTEND,
        ID_SELECT_ALL,
        ID_DESELECT_ALL,
        ID_COPY_SEL,
        ID_LAST
    };

public:

    // Construct text field wide enough to display ncols columns
    TextLabel(FXComposite* p,int ncols,FXObject* tgt=NULL,FXSelector sel=0,unsigned int opts=TEXTFIELD_NORMAL,int x=0,int y=0,int w=0,int h=0,int pl=DEFAULT_PAD,int pr=DEFAULT_PAD,int pt=DEFAULT_PAD,int pb=DEFAULT_PAD);

    // Create server-side resources
    virtual void create();

    // Perform layout
    virtual void layout();

    // Enable text field
    virtual void enable();

    // Disable text field
    virtual void disable();

    // Return default width
    virtual int getDefaultWidth();

    // Return default height
    virtual int getDefaultHeight();

    // Yes, text field may receive focus
    virtual bool canFocus() const;

    // Move the focus to this window
    virtual void setFocus();

    // Remove the focus from this window
    virtual void killFocus();

    // Set cursor position
    void setCursorPos(int pos);

    // Return cursor position
    int getCursorPos() const
    {
        return cursor;
    }

    // Change anchor position
    void setAnchorPos(int pos);

    // Return anchor position
    int getAnchorPos() const
    {
        return anchor;
    }

    // Change the text and move cursor to end
    void setText(const FXString& text,FXbool notify=FALSE);

    // Get the text for this label
    FXString getText() const
    {
        return contents;
    }

    // Set the text font
    void setFont(FXFont* fnt);

    // Get the text font
    FXFont* getFont() const
    {
        return font;
    }

    // Change text color
    void setTextColor(FXColor clr);

    // Return text color
    FXColor getTextColor() const
    {
        return textColor;
    }

    // Change selected background color
    void setSelBackColor(FXColor clr);

    // Return selected background color
    FXColor getSelBackColor() const
    {
        return selbackColor;
    }

    // Change selected text color
    void setSelTextColor(FXColor clr);

    // Return selected text color
    FXColor getSelTextColor() const
    {
        return seltextColor;
    }

    // Changes the cursor color
    void setCursorColor(FXColor clr);

    // Return the cursor color
    FXColor getCursorColor() const
    {
        return cursorColor;
    }

    /*
    * Change the default width of the text field in terms of a number
    * of columns times the width of the numeral '8'.
    */
    void setNumColumns(int cols);

    // Return number of columns
    int getNumColumns() const
    {
        return columns;
    }

    /*
    * Change text justification mode. The justify mode is a combination of
    * horizontal justification (JUSTIFY_LEFT, JUSTIFY_RIGHT, or JUSTIFY_CENTER_X),
    * and vertical justification (JUSTIFY_TOP, JUSTIFY_BOTTOM, JUSTIFY_CENTER_Y).
    * Note that JUSTIFY_CENTER_X can not be set from the constructor since by
    * default text fields are left-justified.
    */
    void setJustify(unsigned int mode);

    // Return text justification mode
    unsigned int getJustify() const;

    // Change word delimiters
    void setDelimiters(const char* delims=textDelimiters)
    {
        delimiters=delims;
    }

    // Return word delimiters
    const char* getDelimiters() const
    {
        return delimiters;
    }

    // Select all text
    FXbool selectAll();

    // Select len characters starting at given position pos
    FXbool setSelection(int pos,int len);

    // Extend the selection from the anchor to the given position
    FXbool extendSelection(int pos);

    // Unselect the text
    FXbool killSelection();

    // Return TRUE if position pos is selected
    FXbool isPosSelected(int pos) const;

    // Return TRUE if position is fully visible
    FXbool isPosVisible(int pos) const;

    // Scroll text to make the given position visible
    void makePositionVisible(int pos);

    // Destructor
    virtual ~TextLabel();
};

#endif
