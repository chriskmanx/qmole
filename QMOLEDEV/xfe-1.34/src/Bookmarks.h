#ifndef BOOKMARKS_H
#define BOOKMARKS_H

class FXAPI Bookmarks : public FXObject
{
    FXDECLARE(Bookmarks)
protected:
    FXString    group;          // MRU File group
    FXObject   *target;         // Target object to send message
    FXSelector  message;        // Message to send
    int       maxbookmarks;       // Maximum number of bookmarks to track
private:
    Bookmarks(const Bookmarks&);
    Bookmarks &operator=(const Bookmarks&);
public:
    long onCmdClear(FXObject*,FXSelector,void*);
    long onCmdBookmark(FXObject*,FXSelector,void*);
    long onUpdBookmark(FXObject*,FXSelector,void*);
    long onUpdAnyBookmarks(FXObject*,FXSelector,void*);
public:
    enum
    {
        ID_CLEAR,
        ID_ANYBOOKMARKS,
        ID_BOOKMARK_1,
        ID_BOOKMARK_2,
        ID_BOOKMARK_3,
        ID_BOOKMARK_4,
        ID_BOOKMARK_5,
        ID_BOOKMARK_6,
        ID_BOOKMARK_7,
        ID_BOOKMARK_8,
        ID_BOOKMARK_9,
        ID_BOOKMARK_10,
        ID_BOOKMARK_11,
        ID_BOOKMARK_12,
        ID_BOOKMARK_13,
        ID_BOOKMARK_14,
        ID_BOOKMARK_15,
        ID_BOOKMARK_16,
        ID_BOOKMARK_17,
        ID_BOOKMARK_18,
        ID_BOOKMARK_19,
        ID_BOOKMARK_20
    };
public:

    // Make new Bookmarks Group with default groupname
    Bookmarks();

    // Make new Bookmarks Group with groupname gp
    Bookmarks(const FXString& gp,FXObject *tgt=NULL,FXSelector sel=0);

    // Change number of bookmarks we're tracking
    void setMaxBookmarks(int mx)
    {
        maxbookmarks=mx;
    }

    // Return the maximum number of bookmarks being tracked
    int getMaxBookmarks() const
    {
        return maxbookmarks;
    }

    // Set group name
    void setGroupName(const FXString& name)
    {
        group=name;
    }

    // Return group name
    FXString getGroupName() const
    {
        return group;
    }

    // Change the target
    void setTarget(FXObject *t)
    {
        target=t;
    }

    // Get the target
    FXObject *getTarget() const
    {
        return target;
    }

    // Change the message
    void setSelector(FXSelector sel)
    {
        message=sel;
    }

    // Return the message id
    FXSelector getSelector() const
    {
        return message;
    }

    // Obtain the bookmark name at index
    FXString getBookmark(int index) const;

    // Change the bookmark name at index
    void setBookmark(int index,const FXString& filename);

    // Append a bookmark
    void appendBookmark(const FXString& filename);

    // Remove a bookmark
    void removeBookmark(const FXString& filename);

    // Clear the list of bookmarks
    void clear();

    // Destructor
    virtual ~Bookmarks();
};


#endif
