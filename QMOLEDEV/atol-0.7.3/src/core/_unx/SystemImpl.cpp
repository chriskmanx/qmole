////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: System utility class - Unix/Gnome implementation.
////////////////////////////////////////////////////////////////////////////

#include <vector>
#include <string>
#include <unistd.h>
#include <dirent.h>
#include <limits.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <pwd.h>
#include "../util.h"	// HAVE_GNOME_VFS
#include "../../config.h"	// HAVE_GNOME_VFS

#if defined(HAVE_GNOME_VFS)
 #include <libgnomevfs/gnome-vfs.h>
 #include <libgnomevfs/gnome-vfs-mime-handlers.h>
#endif

#include "../System.h"
#include "../String.h"
#include "../PathName.h"
#include "../debug.h"
#include "../util.h"

#if defined(HAVE_GNOME_VFS)

// Namespace for Gnome VFS wrappers.
namespace GnomeVFS
{

// Initializes/deinitializes Gnome VFS library.
class Initializer
{
	Initializer()
	{
		gnome_vfs_init();
	}
	~Initializer()
	{
		gnome_vfs_shutdown();
	}

	// Suppress GCC warning that Initializer has private construction and
	// no friends.
	friend void f() {}

	static Initializer s_instance;
};

Initializer Initializer::s_instance;

// Wrapper for Gnome VFS URI.
class URI
{
public:
	typedef GnomeVFSURI* (URI::*HandleMemPtr);

	URI() : m_handle(0) {}

	// Transfers ownership.
	explicit URI(GnomeVFSURI* handle) : m_handle(handle) {}

	explicit URI(const std::string& textURI) : m_handle(gnome_vfs_uri_new(textURI.c_str())) {}

	URI(const URI& other) : m_handle(other.m_handle)
	{
		if (m_handle != 0)
			gnome_vfs_uri_ref(m_handle);
	}

	virtual ~URI()
	{
		if (m_handle != 0)
			gnome_vfs_uri_unref(m_handle);
	}

	const URI& operator=(const URI& other)
	{
		URI temp(other);
		Swap(temp);
		return *this;
	}

	void Swap(URI& other)
	{
		std::swap(m_handle, other.m_handle);
	}

	// Provider this operator instead of operator bool to avoid undesired
	// conversions.
	operator HandleMemPtr() const
	{
		return m_handle != 0 ? &URI::m_handle : 0;
	}

	std::string ToString() const;

	void AppendPath(const std::string& path)
	{
		if (m_handle != 0)
		{
			URI newURI(gnome_vfs_uri_append_path(m_handle, path.c_str()));
			Swap(newURI);
		}
	}

	URI GetParent() const
	{
		return URI(m_handle != 0 ? gnome_vfs_uri_get_parent(m_handle) : 0);
	}

	GnomeVFSURI* GetHandle() const
	{
		return m_handle;
	}
 
private:
	GnomeVFSURI* m_handle;
};

std::string URI::ToString() const
{
	std::string str;
	if (m_handle != 0)
	{
		Glib::ScopedCharPtr buffer(
			gnome_vfs_uri_to_string(m_handle, GNOME_VFS_URI_HIDE_NONE));
		ASSERT(buffer);
		str = buffer.Get();
	}
	return str;
}

GnomeVFSResult FindDirectory(
	const URI& nearURI,
	GnomeVFSFindDirectoryKind kind,
	URI& resultURI,
	bool createIfNeeded,
	bool findIfNeeded,
	int permissions)
{
	GnomeVFSURI* rawURI = 0;
	GnomeVFSResult result = gnome_vfs_find_directory(
		nearURI.GetHandle(), kind, &rawURI, createIfNeeded, findIfNeeded, permissions);
	resultURI = URI(rawURI);
	return result;
}

GnomeVFSResult Xfer(
	const URI& sourceURI,
	const URI& targetURI,
	GnomeVFSXferOptions xferOptions,
	GnomeVFSXferErrorMode errorMode,
	GnomeVFSXferOverwriteMode overwriteMode,
	GnomeVFSXferProgressCallback progressCallback,
	gpointer data)
{
	return gnome_vfs_xfer_uri(
		sourceURI.GetHandle(),
		targetURI.GetHandle(),
		xferOptions,
		errorMode,
		overwriteMode,
		progressCallback,
		data);
}

// Wrapper for Gnome VFS mime application.
class MimeApplication
{
public:
	typedef GnomeVFSMimeApplication* (MimeApplication::*HandleMemPtr);

	MimeApplication() : m_handle(0) {}

	// Transfers ownership
	explicit MimeApplication(GnomeVFSMimeApplication* handle) : m_handle(handle) {}

	MimeApplication(const MimeApplication& other) : m_handle(other.m_handle)
	{
		if (m_handle != 0)
			gnome_vfs_mime_application_copy(m_handle);
	}

	virtual ~MimeApplication()
	{
		if (m_handle != 0)
			gnome_vfs_mime_application_free(m_handle);
	}

	const MimeApplication& operator=(const MimeApplication& other)
	{
		MimeApplication temp(other);
		Swap(temp);
		return *this;
	}

	void Swap(MimeApplication& other)
	{
		std::swap(m_handle, other.m_handle);
	}

	// Provider this operator instead of operator bool to avoid undesired
	// conversions.
	operator HandleMemPtr() const
	{
		return m_handle != 0 ? &MimeApplication::m_handle : 0;
	}

	GnomeVFSResult Launch(const Glib::List<std::string>& uris) const
	{
		return gnome_vfs_mime_application_launch(m_handle, uris.GetHandle());
	}

private:
	GnomeVFSMimeApplication* m_handle;
};

// Determines the mime type of uri.
// Returns mime type or empty string if it can't be determined.
std::string GetMimeType(const URI& uri)
{
	std::string mimeType;
	// We don't need to free buffer.
	char* const buffer = gnome_vfs_get_mime_type(uri.ToString().c_str());
	if (buffer != 0)
		mimeType = buffer;
	return mimeType;
}

MimeApplication MimeGetDefaultApplication(const std::string& mimeType)
{
	return MimeApplication(
		gnome_vfs_mime_get_default_application(mimeType.c_str()));
}

} // namespace GnomeVFS

#endif // defined(HAVE_GNOME_VFS)

struct DirCloseStrategy
{
	static void Free(DIR* dir)
	{
		if (dir != 0)
			closedir(dir);
	}
};

EnumResult System::EnumDirectory(
	const std::string& nativePath,
	FN_ENUMDIR callback, void* callbackData,
	int flags)
{
	// open given directory
	typedef ScopedPtr<DIR, DirCloseStrategy> DirPtr;
	DirPtr dir(opendir(nativePath.c_str()));
	if (!dir)
		return ENUM_ERR_DIR; // error: failed to open directory

	char buffer[sizeof(dirent) + NAME_MAX + 1]; // dirent struct buffer
	dirent* entry = reinterpret_cast<dirent*>(buffer);
	dirent* result = 0;

	// read contents for given subdirectory (jedan po jedan unos)
	// use recursive version for readdir function
	while (readdir_r(dir.Get(), entry, &result) == 0 && result != 0) 
	{
		// ignore entries "." and ".." 
		if (strcmp(entry->d_name, ".") == 0 || 
		    strcmp(entry->d_name, "..") == 0)
			continue;

		// create full path
		std::string nativeChildPath(nativePath);
		//ensure teminated
		int nLen = nativeChildPath.size();
		if(0 == nLen || (nLen>0 && nativeChildPath.at(nLen-1) != '/' && nativeChildPath.at(nLen-1) != '\\'))
			nativeChildPath	+= "/";
		nativeChildPath	+= entry->d_name;

		// get info on current entry (is it directory or file)
		// must be "l" stat version, or else link files will not be properly listed
		struct stat64 statbuf;
		if (lstat64(nativeChildPath.c_str(), &statbuf) == -1)
		{
			TRACE("ERROR listing file: %s\n", nativeChildPath.c_str());
			return ENUM_ERR_FILE; // error: failed to stat child
		}

		if (S_ISDIR(statbuf.st_mode)) // if directory
		{
			if (flags & ENUM_LST_DIRS)
			{
				// pass the data to the callback method
				if (!callback(nativeChildPath, callbackData))
				{
					return ENUM_ERR_OK; // abort request by user
				}
			}	

			if (flags & ENUM_RECURSIVE)
			{
				// recurse into subdirectory
				EnumResult result =
					EnumDirectory(nativeChildPath, callback, callbackData, flags);
				if (result < 0)
					return result; // error
			}
		} 
		else // if file
		{
			if (flags & ENUM_LST_FILES)
			{
				// pass the data to the callback method
				if (!callback(nativeChildPath, callbackData))
				{
					return ENUM_ERR_OK;	// abort request by user
				}
			}
		}
	}

	return ENUM_ERR_OK;
}

bool System::Execute(const std::string& path, const std::string& args, const std::string& dir, bool bWaitForTermination /* = false */)
{
	const char* const pathStr = path.c_str();
	std::vector<char> pathBuffer(pathStr, pathStr + path.size() + 1);

	const char* const argsStr = args.c_str();
	std::vector<char> argsBuffer(argsStr, argsStr + args.size() + 1);

	// TODO: support more than one argument
	std::vector<char*> argv;
	argv.push_back(&pathBuffer[0]);
	if (!args.empty())
		argv.push_back(&argsBuffer[0]);
	argv.push_back(0);

	if(bWaitForTermination)
	{
		// it seems to be necessary to request a filedescriptor
		// to make g_spawn_sync wait for termination of the child process
		gint fdOut;
		g_spawn_sync(dir.c_str(), &argv[0], 0, G_SPAWN_SEARCH_PATH, 0, 0, (gchar**)&fdOut, 0, 0, 0);
		close(fdOut);
		return true;
	}

	return g_spawn_async(dir.c_str(), &argv[0], 0, G_SPAWN_SEARCH_PATH, 0, 0, 0, 0);
}

bool System::Open(const std::string& path, bool bWaitForTermination /* = false */)
{
	// TODO: Implement bWaitForTermination
#if defined(HAVE_GNOME_VFS)

	GnomeVFS::URI uri("file:///");
	uri.AppendPath(path);
	std::string mimeType(GnomeVFS::GetMimeType(uri));
	if (!mimeType.empty())
	{
		GnomeVFS::MimeApplication app(GnomeVFS::MimeGetDefaultApplication(mimeType));
		if (app)
		{
			Glib::List<std::string> uris;
			uris.Append(uri.ToString());
			return app.Launch(uris) == GNOME_VFS_OK;
		}
	}

#endif

	return g_spawn_command_line_async(FileNameFromUTF8(path).c_str(), 0);
}

bool System::Remove(const std::string& path)
{
	std::string nativePath(path);

	//check if file exists
	if(0 != laccess(nativePath.c_str(), 00))
	{
		TRACE("System::Remove file %s not found\n", nativePath.c_str());
		return false;
	}

	//if we have no write access, try to set it
	if(0 != laccess(nativePath.c_str(), W_OK))
	{
		TRACE("System::Remove file %s not writeable\n", nativePath.c_str());
		uid_t uid = getuid();
		struct passwd *pwd = getpwuid(uid);
		if(NULL != pwd && 0 == chown(nativePath.c_str(), pwd->pw_uid, pwd->pw_gid)){
			struct stat st;
			if(0 != lstat(nativePath.c_str(), &st))
				return false;
			chmod(nativePath.c_str(), st.st_mode|S_IWUSR);	// allow file delete
		}
	}
	
	if (remove(nativePath.c_str()) != 0)
	{
		TRACE("Failed to remove \"%s\". Error: %d.\n", nativePath.c_str(), errno);
		return false;
	}
	return true;
}

bool System::MoveToTrash(const std::string& path)
{
#if defined(HAVE_GNOME_VFS)

	// get parent directory URI because FindDirectory requires directory URI
	GnomeVFS::URI uri("file:///");
	uri.AppendPath(path);
	GnomeVFS::URI parentURI(uri.GetParent());

	// first try to find trash directory
	GnomeVFS::URI trashURI;
	if (FindDirectory(parentURI, GNOME_VFS_DIRECTORY_KIND_TRASH, trashURI, false, true, 0) != GNOME_VFS_OK || !trashURI)
	{
		// if it was not found, try to create it
		if (FindDirectory(parentURI, GNOME_VFS_DIRECTORY_KIND_TRASH, trashURI, true, false, 0) != GNOME_VFS_OK)
		{
			trashURI = GnomeVFS::URI();
		}
	}

	// if trash directory was found move item there
	if (trashURI)
	{
		// get name
		std::string name;
		const std::string::size_type lastSlashPos = path.rfind('/');
		name = lastSlashPos != std::string::npos ? path.substr(lastSlashPos + 1) : path;

		trashURI.AppendPath(name);

		// move to trash
		return Xfer(
			uri, trashURI,
			static_cast<GnomeVFSXferOptions>(
				GNOME_VFS_XFER_RECURSIVE |
				GNOME_VFS_XFER_DELETE_ITEMS |
				GNOME_VFS_XFER_NEW_UNIQUE_DIRECTORY
			),
			GNOME_VFS_XFER_ERROR_MODE_ABORT,
			GNOME_VFS_XFER_OVERWRITE_MODE_ABORT,
			0, 0);

		// TODO: if item with the same name exist in trash try to rename ours
	}

#endif // defined(HAVE_GNOME_VFS)

	return false;
}

INT64 System::GetAvailBytesOnFS(const std::string& path, INT64 &nTotal)
{
	std::string nativePath(path);

	INT64 avail = INT64_MAX;
	struct statfs st;
	if (statfs(nativePath.c_str(), &st) == 0){
		avail  = INT64(st.f_bavail) * st.f_bsize;
		nTotal = INT64(st.f_blocks) * st.f_bsize;
	}
	return avail;
}

std::string System::GetLinkTarget(const std::string& linkPath)
{
	std::string nativePath(FileNameFromUTF8(linkPath));
	std::string nativeTarget;

	char buffer[PATH_MAX];
	int result = readlink(nativePath.c_str(), buffer, PATH_MAX - 1);
	if (result >= 0)
	{
		buffer[result] = '\0'; // readlink doesn't append a NULL character
		nativeTarget = buffer;
	}

	return FileNameToUTF8(nativeTarget);
}

bool System::RunCommand(const std::string& command, std::string* output)
{
	if(output)
	{	
		gchar *standard_output = NULL;
		if(g_spawn_command_line_sync(command.c_str(), &standard_output, NULL, NULL, NULL))
		{
			*output = (const char *)standard_output;
			g_free(standard_output);
			return true;
		}
	}
	else if(g_spawn_command_line_async(command.c_str(), NULL))
		return true;
		
	return false;
} 
