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
	

// IoPropertyManager.cpp: implementation of the CIoPropertyManager class.
//
//////////////////////////////////////////////////////////////////////

#include "IoPropertyManager.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////


//CIoParam *CIoPropertyManager::m_pPropNames=NULL;
//CIoParam *CIoPropertyManager::m_pPropArrayNames=NULL;
//CIoParam *CIoPropertyManager::m_pPropFlags=NULL;
//CIoParam *CIoPropertyManager::m_pPropArrayFlags=NULL;

CIoPropertyManager::CIoPropertyManager()
{
	m_pProperties = new CIoParam;
	DeleteNames();
	DeleteFlags();

}

CIoPropertyManager::~CIoPropertyManager()
{
	if(m_pProperties)
		delete m_pProperties;

	m_pProperties=NULL;
	m_pPropNames.clear();
	m_pArrayNames.clear();
	m_pPropFlags.clear();
	m_pArrayFlags.clear();
	//DeleteNames();
	//DeleteFlags();
}

void CIoPropertyManager::CopyProps(CIoPropertyManager *pPropMan)
{
	if(pPropMan)
		this->UpdateProperties(pPropMan->m_pProperties);
}

void CIoPropertyManager::CopyNames(CIoPropertyManager *pPropMan)
{
	int i;
	if(pPropMan)
	{
		DeleteNames();
		for(i=0; i<pPropMan->GetNumPropNames(); i++)
			AddPropName(pPropMan->GetPropertyName(i));
		for(i=0; i<pPropMan->GetNumArrayNames(); i++)
			AddArrayName(pPropMan->GetArrayName(i));

	}
}

void CIoPropertyManager::CopyFlags(CIoPropertyManager *pPropMan)
{
	int i=0;
	if(pPropMan)
	{
		DeleteFlags();
		for(i=0; i<pPropMan->GetNumPropFlags(); i++)
			AddPropFlag(pPropMan->GetPropertyFlag(i));
		for(i=0; i<pPropMan->GetNumArrayFlags(); i++)
			AddArrayFlag(pPropMan->GetArrayFlag(i));

	}
}




void CIoPropertyManager::DeleteNames()
{
	m_pPropNames.clear();
	m_pArrayNames.clear();

}

void CIoPropertyManager::DeleteFlags()
{
	m_pPropFlags.clear();
	m_pArrayFlags.clear();


}

void CIoPropertyManager::AddPropName(char *name)
{
	string snam;
	snam=name;
	m_pPropNames.push_back(snam);
}

void CIoPropertyManager::AddArrayName(char *name)
{
		string snam;
		snam=name;
		m_pArrayNames.push_back(snam);
}
void CIoPropertyManager::AddPropName(string name)
{
	string snam;
	snam=name;
	m_pPropNames.push_back(snam);
}

void CIoPropertyManager::AddArrayName(string name)
{
		string snam;
		snam=name;
		m_pArrayNames.push_back(snam);
}



void CIoPropertyManager::AddPropFlag(int flag)
{
	m_pPropFlags.push_back(flag);
}

void CIoPropertyManager::AddArrayFlag(int flag)
{
	m_pArrayFlags.push_back(flag);
}


void CIoPropertyManager::DeleteProperties()
{

	if(m_pProperties)
	{
		//m_pProperties->DeleteParams();
		delete m_pProperties;
		m_pProperties = NULL;
	}
}

void CIoPropertyManager::CreateProperties(CIoParam *pParam)
{
	DeleteProperties();
	if(pParam)
	{
		if(m_pProperties)
			DeleteProperties();

		m_pProperties = new CIoParam(pParam);
	}
	else
		m_pProperties = new CIoParam();
}



void CIoPropertyManager::SetProperty(int i, CIoParam *pParam, int iUpdate)
{
	int iCreated=0;

	if(pParam == NULL)
	{
		pParam = new CIoParam;
		iCreated = 1;
	}
	if(m_pProperties)
		m_pProperties->SetParam(i, pParam, iUpdate);

	if(iCreated)
		delete pParam;


}

void CIoPropertyManager::SetArray(int i, CIoParam *pParam,int iUpdate)
{
	int iParamCreated=0;


	if(pParam == NULL)
	{
       pParam = new CIoParam;
	   pParam->AddParam();
	   iParamCreated = 1;
	}

	SetProperty(i, pParam, iUpdate);

	if(iParamCreated)
		delete pParam;

}

void CIoPropertyManager::SetFloat(int i, float f,int iUpdate)
{

  CIoParam *pParam = GetProperty(i);
 
  if(pParam)
	pParam->SetParam(&f, iUpdate);

}

void CIoPropertyManager::SetInt(int i, int ii,int iUpdate)
{
  CIoParam *pParam = GetProperty(i);
 
  if(pParam)
		pParam->SetParam(&ii, iUpdate);
}

void CIoPropertyManager::SetIVec(int i, ivec *v,int iUpdate)
{
  int iCreated = 0;
  CIoParam *pParam = GetProperty(i);

  if(v==NULL)
  {
	  v = new ivec(3,0);
	  iCreated = 1;
  }

  if(pParam)
		pParam->SetParam(v, iUpdate);

  if(iCreated)
	  delete v;

}


void CIoPropertyManager::SetVec(int i, vec *v,int iUpdate)
{
  int iCreated = 0;
  CIoParam *pParam = GetProperty(i);

  if(v==NULL)
  {
	  v = new vec(3,0);
	  iCreated = 1;
  }

  if(pParam)
		pParam->SetParam(v, iUpdate);

  if(iCreated)
	  delete v;

}

void CIoPropertyManager::SetMat(int i, matrix *m,int iUpdate)
{
  int iCreated = 0;
  CIoParam *pParam = GetProperty(i);

  if(m==NULL)
  {
	  m = new matrix(3,0);
	  iCreated = 1;
  }

  if(pParam)
    pParam->SetParam(m, iUpdate);

  if(iCreated)
	  delete m;

}

void CIoPropertyManager::SetString(int i, char *s,int iUpdate)
{
  int iCreated = 0;

  CIoParam *pParam = GetProperty(i);

  if(s==NULL)
  {
	  s = new char [strlen("string")];
	  for(i=0; i<strlen("string"); i++) 
								s [i] = 's';
	  iCreated = 1;
  }

	if(pParam)
		pParam->SetParam(s, iUpdate);

  if(iCreated)
	  delete [] s;

}

void CIoPropertyManager::SetString(int i, string s,int iUpdate)
{
  int iCreated = 0;

  CIoParam *pParam = GetProperty(i);


	if(pParam)
		pParam->SetParam(s, iUpdate);

 }



void CIoPropertyManager::AddProperty(CIoParam *pParam)
{
    if(!m_pProperties)
		m_pProperties = new CIoParam;
	
	m_pProperties->AddParam(pParam);

}

void CIoPropertyManager::AddArray(CIoParam *pParam)
{

    if(!m_pProperties)
		m_pProperties = new CIoParam;
	
	if(pParam && pParam->GetType() == IO_PARAM_ARRAY)
						m_pProperties->AddParam(pParam);

}

void CIoPropertyManager::AddFloat(float f)
{
    if(!m_pProperties)
		m_pProperties = new CIoParam;

	m_pProperties->AddParam(&f);
}

void CIoPropertyManager::AddInt(int ii)
{

    if(!m_pProperties)
		m_pProperties = new CIoParam;



	m_pProperties->AddParam(&ii);

}

void CIoPropertyManager::AddIVec(ivec *v)
{

    if(!m_pProperties)
		m_pProperties = new CIoParam;



	m_pProperties->AddParam(v);

}


void CIoPropertyManager::AddVec(vec *v)
{

    if(!m_pProperties)
		m_pProperties = new CIoParam;



	m_pProperties->AddParam(v);

}

void CIoPropertyManager::AddMat(matrix *m)
{

    if(!m_pProperties)
		m_pProperties = new CIoParam;

	m_pProperties->AddParam(m);

}

void CIoPropertyManager::AddString(char *s)
{
    if(!m_pProperties)
		m_pProperties = new CIoParam;

	m_pProperties->AddParam(s);
}

void CIoPropertyManager::AddString(string s)
{
    if(!m_pProperties)
		m_pProperties = new CIoParam;

	m_pProperties->AddParam(s);
}


void CIoPropertyManager::UpdateProperties(CIoParam *pPropertyData)
{

    //Temporary store for original material data
	//Store the original material data
	//and set the new material data
	CIoParam *pTempData;
	pTempData = new CIoParam(m_pProperties);
	
	//Actually set the shape parameters
	if(pPropertyData  && CheckParamFormat(pPropertyData))
	{
			CreateProperties(pPropertyData);

	}
	pTempData->DeleteParams();
	delete pTempData;

}



char *CIoPropertyManager::GetName()
{
	char *s=NULL;
	string sget;

	if(m_pArrayNames.size()>0)
	{
		sget=(m_pArrayNames[0]);
		s=(char *)sget.c_str();
	}

	return s;
}

IoVarType CIoPropertyManager::GetType(int i, char *carraymap)
{
	CIoParam *pParam = NULL;
	IoVarType eType = IO_PARAM_NULL;
	if(pParam = GetChildParam(i, carraymap))
					eType = pParam->GetType();

	return eType;
}



//Gets name of m_pProperties array
string CIoPropertyManager::GetPropertyName(int i)
{
	char *s=NULL;
	string sget;

	if(m_pPropNames.size()>0)
	{
		return(m_pPropNames[i]);
		//s=(char *)sget.c_str();
	}
	else
		return sget;
}

string CIoPropertyManager::GetArrayName(int i)
{
	char *s=NULL;
	string sget;

	if(m_pArrayNames.size()>0)
	{
		return(m_pArrayNames[i]);
		//s=(char *)sget.c_str();
	}

	return sget;
}

string CIoPropertyManager::GetChildPropertyName(int i, char *carraymap)
{
	string s;

	return s;
}

string CIoPropertyManager::GetChildArrayName(int i, char *carraymap)
{
	string s;



	return s;
}

int CIoPropertyManager::GetPropertyArrayNameIndex(int index, CIoParam *pPropertyNames)
{
	int i=0, j=0;
	
	return 2*index;
}

int CIoPropertyManager::GetPropertyFlag(int i)
{
	int iflag=7;

	if(m_pPropFlags.size()>i)
		iflag=(m_pPropFlags[i]);


	return iflag;


}

void CIoPropertyManager::SetPropertyFlag(int i, int flag)
{
	if(m_pPropFlags.size()>i)
				m_pPropFlags[i]=flag;
}

void CIoPropertyManager::SetArrayFlag(int i, int flag)
{
	if(m_pArrayFlags.size()>i)
				m_pArrayFlags[i]=flag;
}


void CIoPropertyManager::SetPropertyName(int i, char *sname)
{
	if(sname && m_pPropNames.size()>i)
				m_pPropNames[i]=sname;


}

void CIoPropertyManager::SetArrayName(int i, char *sname)
{
	if(sname && m_pArrayNames.size()>i)
				m_pArrayNames[i]=sname;


}

int CIoPropertyManager::GetArrayFlag(int i)
{
	int iflag=7;

	if(m_pArrayFlags.size()>i)
		iflag=(m_pArrayFlags[i]);


	return iflag;

}

int CIoPropertyManager::GetChildPropertyFlag(int i, char *carraymap)
{
	int iflag=0;
	
	return iflag;
}

int CIoPropertyManager::GetChildArrayFlag(int i, char *carraymap)
{
	int iflag=0;


	return iflag;



}


int CIoPropertyManager::GetPropertyArrayFlagIndex(int index, CIoParam *pPropertyFlags)
{
	int i=0, j=0;
	
	return 2*index;



}

int CIoPropertyManager::CheckParamFormat(CIoParam *pParamData)
{
	int status = 0;

	if(pParamData && m_pProperties)
		status = (m_pProperties->CheckParam(pParamData));
		
	return status;
}

int CIoPropertyManager::skipcmt(ifstream &s)
{
	int c;
	s.unsetf(s.skipws);
	if(s.peek()=='#'){
		do{
			c=s.get();
			if(c<0)
				return 0;
		} while((c!=0xd) && (c!=0xa));
		s.setf(s.skipws);
		return 1;
	}
	else{
		s.setf(s.skipws);
		return 0;
	}
}

istream &CIoPropertyManager::ReadProps(istream &s, int noparams, int index)
{
	int num;

	if(m_pProperties)
	{
		num=m_pProperties->GetNumParams();
		m_pProperties->ReadParam(s, num);

	}

	return s;
}

ostream &CIoPropertyManager::WriteProps(ostream &s, int noparams, int index)
{
	int num;

	if(m_pProperties)
	{
		num=m_pProperties->GetNumParams();
		m_pProperties->WriteParam(s, num);
	}

	return s;
}

istream &CIoPropertyManager::ReadFlaggedProps(istream &s, int flag, dqint pArrayFlags, int noparams, int index)
{
	int num;

	if(m_pProperties)
	{
		num=m_pProperties->GetNumParams();
		m_pProperties->ReadFlaggedParamArray(s, flag, pArrayFlags, num);

	}

	return s;
}

ostream &CIoPropertyManager::WriteFlaggedProps(ostream &s, int flag, dqint pArrayFlags, int noparams, int index)
{
	int num;

	if(m_pProperties)
	{
		num=m_pProperties->GetNumParams();
		m_pProperties->WriteFlaggedParamArray(s, flag, pArrayFlags, num);
	}

	return s;
}

istream &CIoPropertyManager::ReadFlaggedPropNames(istream &s, int flag, dqint pArrayFlags, int noparams, int index)
{
	int num;
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropNames(); i++)
			pTempParam->AddParam((char *)(GetPropertyName(i).c_str()));

	num=pTempParam->GetNumParams();
	pTempParam->ReadFlaggedParamArray(s, flag, m_pPropFlags, num);

	delete pTempParam;

	return s;
}

ostream &CIoPropertyManager::WriteFlaggedPropNames(ostream &s, int flag, dqint pArrayFlags, int noparams, int index)
{
	int num;
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropNames(); i++)
			pTempParam->AddParam((char *)(GetPropertyName(i).c_str()));

	num=pTempParam->GetNumParams();
	pTempParam->WriteFlaggedParamArray(s, flag, m_pPropFlags, num);

	delete pTempParam;
	return s;
}



void CIoPropertyManager::ReadPropNames(istream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropNames(); i++)
			pTempParam->AddParam("TempName");

		pTempParam->ReadParam(s,pTempParam->GetNumParams() );
		for(i=0; i<GetNumPropNames(); i++)
			SetPropertyName(i, pTempParam->GetStringParam(i));

		delete pTempParam;
}

void CIoPropertyManager::ReadArrayNames(istream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumArrayNames(); i++)
			pTempParam->AddParam("TempName");

		pTempParam->ReadParam(s,pTempParam->GetNumParams() );
		for(i=0; i<GetNumArrayNames(); i++)
			SetArrayName(i, pTempParam->GetStringParam(i));

		delete pTempParam;


}

void CIoPropertyManager::ReadPropFlags(istream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropFlags(); i++)
			pTempParam->AddParam(&i);

		pTempParam->ReadParam(s,pTempParam->GetNumParams() );
		for(i=0; i<GetNumArrayNames(); i++)
			SetPropertyFlag(i, pTempParam->GetIntParam(i));

		delete pTempParam;



}

void CIoPropertyManager::ReadArrayFlags(istream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropFlags(); i++)
			pTempParam->AddParam(&i);

		pTempParam->ReadParam(s,pTempParam->GetNumParams() );
		for(i=0; i<GetNumArrayFlags(); i++)
			SetArrayFlag(i, pTempParam->GetIntParam(i));

		delete pTempParam;
}

void CIoPropertyManager::WritePropNames(ostream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropNames(); i++)
			pTempParam->AddParam((char *)(GetPropertyName(i).c_str()));

		pTempParam->WriteParam(s,pTempParam->GetNumParams() );
		delete pTempParam;

}

void CIoPropertyManager::WriteArrayNames(ostream &s)
{
	int i;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumArrayNames(); i++)
			pTempParam->AddParam(GetArrayName(i));

		pTempParam->WriteParam(s,pTempParam->GetNumParams() );
		delete pTempParam;
}

void CIoPropertyManager::WritePropFlags(ostream &s)
{
	int i,flag;	
	CIoParam *pTempParam=new CIoParam();
		for(i=0; i<GetNumPropFlags(); i++)
		{
			flag=GetPropertyFlag(i);
			pTempParam->AddParam(&flag);
		}

		pTempParam->WriteParam(s,pTempParam->GetNumParams() );
		delete pTempParam;



}

void CIoPropertyManager::WriteArrayFlags(ostream &s)
{
	int i, flag;	
	CIoParam *pTempParam=new CIoParam();
	for(i=0; i<GetNumArrayFlags(); i++)
			{
			flag=GetArrayFlag(i);
			pTempParam->AddParam(&flag);
		}
		pTempParam->WriteParam(s,pTempParam->GetNumParams() );
		delete pTempParam;
}
