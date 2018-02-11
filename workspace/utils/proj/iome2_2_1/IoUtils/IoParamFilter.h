#ifndef IO_PARAM_FILTER_H
#define IO_PARAM_FILTER_H


//#pragma once
/*
IOME LICENSE
IOME Version 1.1.1

IOME Development  Tools
Copyright (C) 2001-2004, Michael Kenneth Griffiths, All Rights Reserved.

--------------------------------------------------------------------------------
IOME public license.

The contents of this file are subject to the IOME Public License Version 1.3
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://81.174.178.112/iome/licensing/iomelicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Michael Kenneth Griffiths.
Copyright (C) 2000-2004 Michael Kenneth Griffiths. All Rights Reserved.
--------------------------------------------------------------------------------
GPL license.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place, Suite 330, Boston, MA 02111-1307 USA

Author contact information:
mikeg@photon0.freeserve.co.uk
--------------------------------------------------------------------------------
*/
	



#include "IoParams.h"

enum IoFilterParType {IO_FILTER_UPPER_PARAM, IO_FILTER_LOWER_PARAM};


class CIoParamFilter :
	public CIoParam
{
public:
	CIoParamFilter(void);
	CIoParamFilter(CIoParam *pLowerBound, CIoParam *pUpperBound, unsigned short int iEnabled=0);

	~CIoParamFilter(void);

	char *GetName(){return m_sName;}
	void SetName(char *sName);
	void Toggle(){m_iEnabled=(m_iEnabled?0:1);}
	int Test(CIoParam *pTestParam);
	int Test(float fTest);
	int Test(vec *vTest);
	unsigned short int Enabled(){return m_iEnabled;}
	void SetUpperBound(CIoParam *pUpperBoundParam);
	void SetLowerBound(CIoParam *pLowerBoundParam);
	CIoParam *GetUpperBound(){return GetParam(IO_FILTER_UPPER_PARAM);}
	CIoParam *GetLowerBound(){return GetParam(IO_FILTER_LOWER_PARAM);}
	char m_sName[120];
	unsigned short int  m_iEnabled;
};

#endif

