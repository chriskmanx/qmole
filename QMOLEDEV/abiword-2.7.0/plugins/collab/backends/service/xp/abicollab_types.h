/* Copyright (C) 2008 AbiSource Corporation B.V.
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

#ifndef __ABICOLLAB_TYPES__
#define __ABICOLLAB_TYPES__

#include <boost/shared_ptr.hpp>
#include "soa.h"

namespace abicollab {

class File {
public:
	static boost::shared_ptr<File> construct(soa::GenericPtr value) {
		if (soa::CollectionPtr coll = value->as<soa::Collection>()) {
			boost::shared_ptr<File> file(new File());
			if (soa::StringPtr doc_id_ = coll->get<soa::String>("doc_id"))
				file->doc_id = doc_id_->value();
			if (soa::StringPtr filename_ = coll->get<soa::String>("filename"))
				file->filename = filename_->value();
			if (soa::StringPtr tags_ = coll->get<soa::String>("tags"))
				file->tags = tags_->value();
			if (soa::StringPtr filesize_ = coll->get<soa::String>("filesize"))
				file->filesize = filesize_->value();
			if (soa::StringPtr lastchanged_ = coll->get<soa::String>("lastchanged"))
				file->lastchanged = lastchanged_->value();
			if (soa::IntPtr lastrevision_ = coll->get<soa::Int>("lastrevision"))
				file->lastrevision = lastrevision_->value();
			if (soa::StringPtr access_ = coll->get<soa::String>("access"))
				file->access = access_->value();
			return file;
		}
		return boost::shared_ptr<File>();
	}
	
	std::string doc_id;
	std::string filename;
	std::string tags;
	std::string filesize;
	std::string lastchanged;
	int64_t lastrevision;
	std::string access;
};
typedef boost::shared_ptr<abicollab::File> FilePtr;
typedef boost::shared_ptr< soa::Array< abicollab::FilePtr > > FileArrayPtr;

class Friend : public soa::Collection {
public:
	Friend(const std::string& name)
		: soa::Collection(name)
	{}

	static boost::shared_ptr<Friend> construct(soa::GenericPtr value) {
		if (soa::CollectionPtr coll = value->as<soa::Collection>()) {
			boost::shared_ptr<Friend> friend_(new Friend(coll->name()));
			if (soa::StringPtr name_ = coll->get<soa::String>("name"))
				friend_->name = name_->value();
			if (soa::StringPtr email_ = coll->get<soa::String>("email"))
				friend_->email = email_->value();
			friend_->files = coll->get< soa::Array<soa::GenericPtr> >("files");
			return friend_;
		}
		return boost::shared_ptr<Friend>();
	}

	std::string name;
	std::string email;
	soa::ArrayPtr files;
};
typedef boost::shared_ptr<Friend> FriendPtr;
typedef boost::shared_ptr< soa::Array< FriendPtr > > FriendArrayPtr;

class Group : public soa::Collection {
public:
	Group(const std::string& name)
		: soa::Collection(name)
	{}

	static boost::shared_ptr<Group> construct(soa::GenericPtr value) {
		if (soa::CollectionPtr coll = value->as<soa::Collection>()) {
			boost::shared_ptr<Group> group_(new Group(coll->name()));
			if (soa::StringPtr name_ = coll->get<soa::String>("name"))
				group_->name = name_->value();
			group_->files = coll->get< soa::Array<soa::GenericPtr> >("files");
			return group_;
		}
		return boost::shared_ptr<Group>();
	}

	std::string name;
	soa::ArrayPtr files;
};
typedef boost::shared_ptr<Group> GroupPtr;
typedef boost::shared_ptr< soa::Array< GroupPtr > > GroupArrayPtr;

}

#endif /* __ABICOLLAB_TYPES__ */
