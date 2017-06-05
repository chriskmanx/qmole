////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VFS class implementation
////////////////////////////////////////////////////////////////////////////

#include "Vfs.h"
#include "PathName.h"

Vfs::Vfs()
{
	m_pProgress = NULL;
	m_nType = UNKNOWN;
}

Vfs::~Vfs()
{
}

bool Vfs::CachedListDir(VfsListing &list, bool &bAbort, bool bForceRefresh)
{
	ASSERT(m_nType == LOCAL || mutex.locked);
	return ListDir(list, bAbort);	// default version falls back to non-cached call
}

void Vfs::ExpandSelection(VfsSelection &sel, bool &bAbort)
{
	ExpandTree(sel.m_lstRootItems, bAbort);
}

void Vfs::ExpandTree(std::vector<VfsSelectionItem> &list, bool &bAbort)
{
    int nSize = list.size();

    for(int i=0; i<nSize; i++)
    {
        if(list[i].IsDir() && !list[i].IsDots())
        {
            String strPath = GetDir();

            String strNewPath  = strPath;
			PathName::EnsureTerminated(strNewPath, '/');  //it is very important to use '//' for SFTP sites!!!
            strNewPath += list[i].GetName();

            if(SetDir(strNewPath))
            {
                VfsListing listing;
                CachedListDir(listing, bAbort);    //TOFIX listing without ".." mode

                //add all listed/filtered files to the subtree
                for(int j=0; j<listing.GetCount(); j++)
                {
                    if(!listing.GetAt(j).IsDots())
                        list[i].m_lstSubItems.push_back(listing.GetAt(j));
                }

                //recursively expand this new list
                ExpandTree(list[i].m_lstSubItems, bAbort);
                
                //restore path
                SetDir(strPath);
            }
            else
            {
                //WXTRACE("WARNING: failed to set directory\n");
                //WXASSERT(FALSE);
            }
        }
    }
}

//are two Vfs pointing to the same location
bool Vfs::IsEqualLocation(Vfs *pOther)
{
	//if Vfs have the same type and point to the same dir, it is the same location
	if(GetType() == pOther->GetType())
	{
		String strDir1 = GetDir();
		PathName::EnsureTerminated(strDir1);

		String strDir2 = pOther->GetDir();
		PathName::EnsureTerminated(strDir2);

		return strDir1 == strDir2;
	}

	return false;
}

bool Vfs::Lock() 
{
	return mutex.Lock();
}

bool Vfs::Unlock() 
{
	return mutex.Unlock();
}
