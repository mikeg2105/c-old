
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
	



#include "IoParamFilter.h"

CIoParamFilter::CIoParamFilter(void)
{
	float fupper=100.0;
	float flower=-100.0;
	SetName("Default");
	m_iEnabled=1;  // True it is disabled
	AddParam(&fupper);
	AddParam(&flower);
}

CIoParamFilter::CIoParamFilter(CIoParam *pLowerBound, CIoParam *pUpperBound, unsigned short int iEnabled)
{
	SetName("Default");
	m_iEnabled=iEnabled;  // True it is disabled
	AddParam(pUpperBound);
	AddParam(pLowerBound);
}

CIoParamFilter::~CIoParamFilter(void)
{
}

void CIoParamFilter::SetName(char *sName)
{
	if(sName)
	{
		//delete [] m_sName;
		//m_sName = new char[strlen(sName)]; 
		strcpy(m_sName, sName);	
	}

}

int CIoParamFilter::Test(CIoParam *pTestParam)
{
	int result=0;  //default is pass result

	return result;
}

int CIoParamFilter::Test(float fTest)
{
	int result=0;  //default is pass result

	return result;

}

int CIoParamFilter::Test(vec *vTest)
{
	int result=0;  //default is pass result

	return result;
}

void CIoParamFilter::SetUpperBound(CIoParam *pUpperBoundParam)
{
	//Only set the parameter if that passed in is not NULL
	//Check that of the same type
	CIoParam *pParam=NULL;
	pParam=GetParam(IO_FILTER_UPPER_PARAM);
	if(pParam && pParam->CheckParam(pUpperBoundParam))
		SetParam(IO_FILTER_UPPER_PARAM, pUpperBoundParam);
}

void CIoParamFilter::SetLowerBound(CIoParam *pLowerBoundParam)
{
	CIoParam *pParam=NULL;
	pParam=GetParam(IO_FILTER_LOWER_PARAM);
	if(pParam && pParam->CheckParam(pLowerBoundParam))
		SetParam(IO_FILTER_LOWER_PARAM, pLowerBoundParam);
}


