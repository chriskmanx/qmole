/* Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#ifndef __DOCTREEITEM_H__
#define __DOCTREEITEM_H__

class DocHandle;

enum DocTreeItemType
{
	DOCTREEITEM_TYPE_DOCUMENT = 0,
	DOCTREEITEM_TYPE_TAG,
	DOCTREEITEM_TYPE_NILL
};

class DocTreeItem
{
public:
	DocTreeItemType		m_type;
	DocHandle*			m_docHandle;

	DocTreeItem*		m_child;
	DocTreeItem*		m_next;
};

#endif /* DOCTREEITEM_H */
