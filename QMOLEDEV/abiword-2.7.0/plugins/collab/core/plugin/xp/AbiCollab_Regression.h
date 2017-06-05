/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by One Laptop Per Child
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

#ifndef ABI_COLLAB_REGRESSION_H
#define ABI_COLLAB_REGRESSION_H

#include <vector>

#include "ut_types.h"

class AbiCollab_Regression
{
public:
	bool			execute();

private:
	void			_findRegressionFiles(std::vector<std::string>& files);
};

#endif /* ABI_COLLAB_REGRESSION_H */
