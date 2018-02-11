// IoParams.cpp: implementation of the CIoParam class.
//
//////////////////////////////////////////////////////////////////////
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



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CIoParam::CIoParam()
{
	m_pParam = NULL;
	m_pNextParam = NULL;
	m_sDelimeter+="\n";

	//Default is 0 value integer
	m_type = IO_PARAM_INTEGER;
	m_val.pi = 0;
}



CIoParam::~CIoParam()
{

	//Destructor calls the relevant destructor
	//for each memeber
	// if(m_pParam)
//	  delete m_pParam;

 // if(m_pNextParam)
//	  delete m_pNextParam;
	DeleteParams();

}

CIoParam::CIoParam(CIoParam *pParam)
{

	CIoParam *pTempParam;
	CIoParam *pCurrentParam;
	m_pParam = NULL;
	m_pNextParam = NULL;
	if(pParam==NULL)
	{
				//CIoParam();
				m_type=IO_PARAM_INTEGER;
				m_val.pi=0;
	}
	else
		{

		//Now copy the data for this parameter
		m_type = pParam->GetType();
		switch(m_type)
		{
			case IO_PARAM_INTEGER :
				m_val.pi = pParam->GetIntParam();
			break;

			case IO_PARAM_FLOAT :
				m_val.pf = pParam->GetFloatParam();
			break;

			case IO_PARAM_VEC :
				m_val.pv = (vec *)new vec(*(pParam->GetVecParam()));
			break;

			case IO_PARAM_IVEC :
				m_val.piv = (ivec *)new ivec(*(pParam->GetIVecParam()));
			break;

			case IO_PARAM_MAT :
				m_val.pm = (matrix *)new matrix(*(pParam->GetMatParam()));
			break;

			case IO_PARAM_STRING :
				if(pParam->GetStringParam())
				{
					//m_val.pc = (char *)new char [strlen(pParam->GetStringParam())];
					//strcpy(m_val.pc, pParam->GetStringParam());
					m_val.pc = (IoString *) new IoString(pParam->GetStringParam());
				}
				else
				{
					m_val.pc = (IoString *) new IoString("String");
					//m_val.pc = (char *)new char [100];
					//m_val.pc [0] = '\0';
				}
			break;

			case IO_PARAM_ARRAY :
				m_val.pi = pParam->GetIntParam();

				//If the parameter has a list of parameters
				//create a new list of parameters
				//and call the copy constructor to create this list
				if((pTempParam = pParam->GetParamArray()))
				{
							m_pParam = (CIoParam *)new CIoParam(pTempParam);
							pTempParam = pTempParam->GetNextParam();
							pCurrentParam = m_pParam;
							//Create a copy of the nex element in the parameter list
							while(pTempParam)
							{
								
								pCurrentParam->SetNextParam((CIoParam *)new CIoParam(pTempParam));
								pCurrentParam= pCurrentParam->GetNextParam();
								pTempParam = pTempParam->GetNextParam();
							}
				}


			break;
		}
	}
}

/*!
	Default input parameter for this function is -1 if
	user supplies a value greater zero then the input
	value will be compared with the array size
*/
int CIoParam::IsArrayOfArrays(int isize)
{
	int iIsArrayArray=1;
	int i=0;
	CIoParam *pA;
	if(GetType()==IO_PARAM_ARRAY)
	{
		for(i=0; i<GetNumParams(); i++)
		{
			if((pA=GetParam(i)) && (pA->GetType()==IO_PARAM_ARRAY))
				iIsArrayArray=1;
			else
			{
				iIsArrayArray=0;
				break;
			}

		}

		if((isize>0) && iIsArrayArray)
		{
			if(isize==GetNumParams())
				iIsArrayArray=1;
			else
				iIsArrayArray=0;
		}


	}
	else
		iIsArrayArray=0;



	return iIsArrayArray;
}

void CIoParam::AddParam(CIoParam *pParam)
{
	
	//Can only add a parameter if this is of type array
	//type can only be set when first element set
	CIoParam *pTempParam, *pCurrentParam;
	CIoParam *pNewParam;
	if(pParam)
		pNewParam = (CIoParam *)new CIoParam(pParam);
	else
		pNewParam = (CIoParam *)new CIoParam();
	if(m_pParam && (GetType()==IO_PARAM_ARRAY))
	{
		//The list exists
		//add the parameter to the end of the list
		pCurrentParam = m_pParam;
		while(pTempParam= pCurrentParam->GetNextParam())
			pCurrentParam = pTempParam;

		if(pNewParam != pCurrentParam)
			pCurrentParam->SetNextParam(pNewParam);
		m_val.pi += 1;
	}
	else
	{
		m_pParam = pNewParam;
		m_val.pi = 1;
		m_type = IO_PARAM_ARRAY;
	}

}

void CIoParam::AddParam(ivec *pv)
{

	CIoParam *pParam;
	short int i=0;
	pParam = (CIoParam *)new CIoParam;

	if(pv == NULL)
	{
		pv = (ivec *)new ivec(3,0);
		i=1;
	}

	pParam->SetParam(pv);

	AddParam(pParam);
	delete pParam;
	if(i==1) delete pv;
}

void CIoParam::AddParam(vec *pv)
{

	CIoParam *pParam;
	short int i=0;
	pParam = (CIoParam *)new CIoParam;

	if(pv == NULL)
	{
		pv = (vec *)new vec(3,0);
		i=1;
	}

	pParam->SetParam(pv);

	AddParam(pParam);
	delete pParam;
	if(i==1) delete pv;
}


void CIoParam::AddParam(matrix *pm)
{

	CIoParam *pParam;
	pParam = (CIoParam *)new CIoParam;
	short int i = 0;

	if(pm == NULL)
	{
		pm = (matrix *)new matrix(2,2,0,0);
		i=1;
	}

	pParam->SetParam(pm);

	AddParam(pParam);
	if(i==1) delete pm;
	delete pParam;
}

void CIoParam::AddParam(char *pc)
{

	CIoParam *pParam;
	short int i=0;
	pParam = (CIoParam *)new CIoParam();

	if(pc == NULL)
	{
		pc = (char *)new char [1+strlen("string")];
		strcpy(pc, "string");
		//pc = (IoString *)new IoString("String");
	}

	pParam->SetParam(pc);

	AddParam(pParam);
	delete pParam;
	//if(i==1) delete pc;
}

void CIoParam::AddParam(string pc)
{

	CIoParam *pParam;
	short int i=0;
	pParam = (CIoParam *)new CIoParam();

	pParam->SetParam(pc);

	AddParam(pParam);
	delete pParam;
	//if(i==1) delete pc;
}


void CIoParam::AddParam(float *pf)
{

	CIoParam *pParam;
	float f = 999999;

	pParam = (CIoParam *)new CIoParam;

	if(pf == NULL)
		pf = &f;

	pParam->SetParam(pf);

	AddParam(pParam);

	delete pParam;
}

void CIoParam::AddParam(int *pi)
{

	CIoParam *pParam;
	int i = 99999;

	pParam = (CIoParam *)new CIoParam;

	if(pi == NULL)
		pi = &i;

	pParam->SetParam(pi);

	AddParam(pParam);

	//delete pParam;
}







void CIoParam::DeleteParam(int i)
{
    int count=0;
	CIoParam *pCurrentParam, *pTempParam;
	//Find the ith parameter in the list and delete it
	//can only perform this operation if we really have a list!
   if(m_pParam && (GetType()==IO_PARAM_ARRAY) )
   {
	    //will point to previous parameter
	    pTempParam = NULL;
		pCurrentParam = m_pParam;
		//The current count must be 1 less than the required index i
		//Must always check that the next parameter on exists
		while(count < i && pCurrentParam->GetNextParam())
		{
            pTempParam = pCurrentParam;
			pCurrentParam = pCurrentParam->GetNextParam();
			count ++;
		}
		if(count == i && pCurrentParam)
		{
			
			if(pTempParam)
				pTempParam->SetNextParam(pCurrentParam->GetNextParam());

			//The first element deleted
			if(i==0 && pCurrentParam == m_pParam)
							m_pParam = pCurrentParam->GetNextParam();
			//delete this element
            delete pCurrentParam;

			m_val.pi -= 1;
	
		}

   }

}

void CIoParam::DeleteParam(CIoParam *pParam)
{
    int count=0;
	CIoParam *pCurrentParam, *pTempParam;
	//Find the ith parameter in the list and delete it
	//can only perform this operation if we really have a list!
   if(m_pParam  && (GetType()==IO_PARAM_ARRAY))
   {
	    //will point to previous parameter
	    pTempParam = NULL;
		pCurrentParam = m_pParam;
		//The current count must be 1 less than the required index i
		//Must always check that the next parameter on exists
		while(pParam != pCurrentParam && pCurrentParam->GetNextParam())
		{
            pTempParam = pCurrentParam;
			pCurrentParam = pCurrentParam->GetNextParam();
		}
		if(pCurrentParam == pParam && pCurrentParam)
		{
			
			if(pTempParam)
				pTempParam->SetNextParam(pCurrentParam->GetNextParam());

			//The first element deleted
			if(pCurrentParam == m_pParam)
							m_pParam = pCurrentParam->GetNextParam();
			//delete this element
            delete pCurrentParam;

			m_val.pi -= 1;
	
		}

   }
   else if(m_pParam  && (GetType()==IO_PARAM_VEC))
   {
		if(m_val.pv)
			delete m_val.pv;
		m_val.pv = NULL;
   }
   else if(m_pParam  && (GetType()==IO_PARAM_IVEC))
   {
		if(m_val.piv)
			delete m_val.piv;
		m_val.piv = NULL;
   }
   else if(m_pParam  && (GetType()==IO_PARAM_MAT))
   {
		if(m_val.pm)
			delete m_val.pm;
		m_val.pm = NULL;
   }
   else if(m_type == IO_PARAM_STRING)
   {
		if(m_val.pc)
			delete [] m_val.pc;
		m_val.pc = NULL;
   }

   pParam->m_pNextParam=NULL;

}

int CIoParam::GetNumParams()
{

   int count = 0;
   CIoParam *pCurrentParam;

   pCurrentParam = m_pParam;
   while(pCurrentParam)
   {
		count ++;
		pCurrentParam = pCurrentParam->GetNextParam();
   }
   if(count != m_val.pi)
	   m_val.pi = count;
   return count;
}

CIoParam *CIoParam::GetParam(int i)
{
   CIoParam *pCurrentParam, *pParam=NULL;

   int count = 0;

   if(i==0)
	   pParam = m_pParam;

   pCurrentParam = m_pParam;
   while(count<i && pCurrentParam)
   {
		count ++;
		pCurrentParam = pCurrentParam->GetNextParam();
		pParam = pCurrentParam;
   }

   return pParam;
}

void CIoParam::SetParam(CIoParam *pParam, int iUpdate)
{
	int i;
	float f;
    IoVarType itype;
	if(pParam && (!iUpdate || (iUpdate && CheckParam(pParam))))
	{

		itype = pParam->GetType();

		switch(itype)
		{
		case IO_PARAM_ARRAY:
			{
				if(m_pParam && GetType()== (IO_PARAM_ARRAY))
				{
					delete m_pParam;
					m_pParam = NULL;
				}

 				if(m_type == IO_PARAM_MAT)
					if(m_val.pm)
						delete m_val.pm;

				if(m_type == IO_PARAM_VEC)
					if(m_val.pv)
						delete m_val.pv;

				if(m_type == IO_PARAM_IVEC)
					if(m_val.piv)
						delete m_val.piv;

				if(m_type == IO_PARAM_STRING)
					if(m_val.pc)
						delete [] m_val.pc;

				m_pParam = (CIoParam *)new CIoParam(pParam->GetParamArray());
				m_type = IO_PARAM_ARRAY;
				m_val.pi = pParam->GetIntParam();
			}

			break;
		case IO_PARAM_INTEGER:
			i=pParam->GetIntParam();
			SetParam(&i, iUpdate);
			break;
		case IO_PARAM_FLOAT:
			f = pParam->GetFloatParam();
			SetParam(&f, iUpdate);
			break;
		case IO_PARAM_VEC:
			SetParam(pParam->GetVecParam(), iUpdate);
			break;
		case IO_PARAM_IVEC:
			SetParam(pParam->GetIVecParam(), iUpdate);
			break;
		case IO_PARAM_MAT:
			SetParam(pParam->GetMatParam(), iUpdate);
			break;
		case IO_PARAM_STRING:
			SetParam(pParam->GetStringParam(), iUpdate);
			break;
		}//end of type switch
	}//end of iUpdate check
}

void CIoParam::SetParam(vec *pv, int iUpdate)
{
    vec *tempvec = (vec *)new vec( *pv);
	int vlen=0, i;
	float vf;
	CIoParam *pParam = (CIoParam *)new CIoParam();
	pParam->m_val.pv = tempvec;
	pParam->m_type=IO_PARAM_VEC;
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{	
		if(m_pParam && (GetType()== IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;


		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete [] m_val.pc;

		m_type = IO_PARAM_VEC;
		if(pv)
		{
			vlen = pv->length();
			m_val.pv = (vec *)new vec(vlen, 0);
			for(i=0; i<vlen; i++)
				m_val.pv->set(i, pv->get(i));

			//m_val.pv=new vec(*pv);

			//m_val.pv = new vec( *pv);
		}
		else
			m_val.pv =(vec *) new vec(3,0);
	}
	delete pParam;

}

void CIoParam::SetParam(ivec *pv, int iUpdate)
{
    ivec *tempvec = (ivec *)new ivec( *pv);
	int vlen=0, i;
	float vf;
	CIoParam *pParam = (CIoParam *)new CIoParam();
	pParam->m_val.piv = tempvec;
	pParam->m_type=IO_PARAM_IVEC;
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{	
		if(m_pParam && (GetType()== IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;

		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete [] m_val.pc;

		m_type = IO_PARAM_IVEC;
		if(pv)
		{
			vlen = pv->length();
			m_val.piv = (ivec *)new ivec(vlen, 0);
			for(i=0; i<vlen; i++)
				m_val.piv->set(i, pv->get(i));

			//m_val.pv = new vec( *pv);
		}
		else
			m_val.piv =(ivec *) new ivec(3,0);
	}
	delete pParam;

}



void CIoParam::SetParam(matrix *pm, int iUpdate)
{
	matrix *tempmat;
	
    if(pm)
		tempmat = (matrix *)new matrix( *pm);
	else
		tempmat = (matrix *)new matrix(3,3,0,0);
	CIoParam *pParam = new CIoParam();
	pParam->m_val.pm = tempmat;
	pParam->m_type=IO_PARAM_MAT;
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{	
		if(m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

 		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;

		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete [] m_val.pc;

		m_type = IO_PARAM_MAT;
		if(pm)
			m_val.pm = (matrix *)new matrix ( *pm);
		else
			m_val.pm = (matrix *)new matrix(3, 3, 0, 0);
	}
	delete pParam;
}

void CIoParam::SetParam(char *pc,int iUpdate)
{
	//char *tempstr= (char *)new char [strlen(pc)];
	//strcpy(tempstr, pc);

	IoString *tempstr = (IoString *) new IoString(pc);
    CIoParam *pParam = (CIoParam *)new CIoParam();

	pParam->m_val.pc = tempstr;
	pParam->m_type=IO_PARAM_STRING;
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{
		if(m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

 		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;


		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete m_val.pc;


		m_type = IO_PARAM_STRING;
		if(pc)
		{
			//m_val.pc = (char *)new char [ strlen(pc)];
			//strcpy(m_val.pc, pc);
			m_val.pc = (IoString *) new IoString(pc);
		}
		else
		{
			//m_val.pc = (char *)new char [100];
			//m_val.pc [0] = '\0';
			m_val.pc = (IoString *) new IoString("String");
		}
	}

	//delete [] tempstr;
	//delete tempstr;
	delete pParam;
}

void CIoParam::SetParam(string pc,int iUpdate)
{
	//char *tempstr= (char *)new char [strlen(pc)];
	//strcpy(tempstr, pc);

	IoString *tempstr = (IoString *) new IoString(pc);
    CIoParam *pParam = (CIoParam *)new CIoParam();

	pParam->m_val.pc = tempstr;
	pParam->m_type=IO_PARAM_STRING;
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{
		if(m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

 		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;


		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete m_val.pc;


		m_type = IO_PARAM_STRING;
			m_val.pc = (IoString *) new IoString(pc);

	}

	//delete [] tempstr;
	//delete tempstr;
	//delete pParam;
}


void CIoParam::SetParam(float *pf, int iUpdate)
{
	float ftemp = *pf;
	CIoParam *pParam = (CIoParam *)new CIoParam();
	pParam->m_val.pf = ftemp;
	pParam->m_type=IO_PARAM_FLOAT;
	
	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{ 
		if(m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;


		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete m_val.pc;

		m_type = IO_PARAM_FLOAT;
		m_val.pf = *pf;
	}
	delete pParam;
}

void CIoParam::SetParam(int *pi, int iUpdate)
{
    int itemp = *pi;
	CIoParam *pParam = (CIoParam *)new CIoParam();
	pParam->m_val.pi = itemp;
	pParam->m_type=IO_PARAM_INTEGER;

	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{
		if(m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
			delete m_pParam;
			m_pParam = NULL;
		}

		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;


		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete [] m_val.pc;

		m_type = IO_PARAM_INTEGER;
		m_val.pi = *pi;
	}
	delete pParam;
}

void CIoParam::SetParam(int i, CIoParam *pParam,int iUpdate)
{
  	if(!iUpdate || (iUpdate && CheckParam(pParam)))
	{
		CIoParam *pCurrentParam, *pPreviousParam, *pNextParam;
		int count = 0;
		pNextParam = NULL;

		CIoParam *pNewParam = (CIoParam *)new CIoParam(pParam);

		if(m_type == IO_PARAM_MAT)
			if(m_val.pm)
				delete m_val.pm;

		if(m_type == IO_PARAM_VEC)
			if(m_val.pv)
				delete m_val.pv;

		if(m_type == IO_PARAM_IVEC)
			if(m_val.piv)
				delete m_val.piv;

		if(m_type == IO_PARAM_STRING)
			if(m_val.pc)
				delete [] m_val.pc;

		if(pNewParam && m_pParam && GetType()== (IO_PARAM_ARRAY))
		{
		pCurrentParam = m_pParam;
		pPreviousParam = m_pParam;
		while(count<i && pCurrentParam)
		{
				count ++;
				pPreviousParam = pCurrentParam;
				pCurrentParam = pCurrentParam->GetNextParam();
				if(pCurrentParam)
					pNextParam = pCurrentParam->GetNextParam();
		}

			if(i==0)
			{
					pNewParam->SetNextParam(m_pParam->GetNextParam());
					delete m_pParam;
					m_pParam = pNewParam;
					
			}
			else
			{
				pPreviousParam->SetNextParam(pNewParam);
				pNewParam->SetNextParam(pNextParam);
				delete pCurrentParam;
			}
		}
    
	}

}

void CIoParam::SetParam(int i, ivec *pv,int  iUpdate)
{
	if(pv) 
	{
		CIoParam pParam(pv);
		SetParam(i, &pParam, iUpdate);
	}

}


void CIoParam::SetParam(int i, vec *pv,int  iUpdate)
{
	if(pv) 
	{
		CIoParam pParam(pv);
		SetParam(i, &pParam, iUpdate);
	}

}

void CIoParam::SetParam(int i, matrix *pm,int  iUpdate)
{
	if(pm) 
	{
		CIoParam pParam(pm);
		SetParam(i, &pParam, iUpdate);
	}
}
void CIoParam::SetParam(int i, float *pf,int  iUpdate)
{
	if(pf) 
	{
		CIoParam pParam(pf);
		SetParam(i, &pParam, iUpdate);
	}
}

void CIoParam::SetParam(int i, int *pi,int  iUpdate)
{
	if(pi) 
	{
		CIoParam pParam(pi);
		SetParam(i, &pParam, iUpdate);
	}

}

void CIoParam::SetParam(int i, char *pc,int  iUpdate)
{
	if(pc) 
	{
		CIoParam pParam(pc);
		SetParam(i, &pParam, iUpdate);
	}
}

void CIoParam::SetParam(int i, string s,int  iUpdate)
{

		CIoParam pParam(s);
		SetParam(i, &pParam, iUpdate);
}


void CIoParam::DeleteParams()
{

	CIoParam *pCurrentParam=NULL, *pNextParam=NULL;
	pCurrentParam = m_pNextParam;

    if(m_pParam && GetType()== (IO_PARAM_ARRAY))
	{

        pCurrentParam = m_pParam->m_pNextParam;
		while(pCurrentParam)
		{
			pNextParam = pCurrentParam->m_pNextParam;
			//pCurrentParam->DeleteParams();
			delete pCurrentParam;
			pCurrentParam=pNextParam;
		}
		//m_pParam->DeleteParams();
		delete m_pParam;
		m_pParam = NULL;
	}

	if(m_type == IO_PARAM_MAT)
		if(m_val.pm)
		{
			delete m_val.pm;
			m_val.pm=NULL;
		}

	if(m_type == IO_PARAM_VEC)
		if(m_val.pv)
		{
			delete m_val.pv;
			m_val.pv=NULL;
		}

	if(m_type == IO_PARAM_STRING)
		if(m_val.pc)
		{
			//strcpy(m_val.pc, "");
			delete m_val.pc;
			m_val.pc=NULL;
		}

	m_pNextParam = NULL;
	m_pParam = NULL;
	m_type =(IoVarType)9999;


}



CIoParam *CIoParam::GetChildParam(int i, char *carraymap)
{
	char seps[] = " ,\t\n";
	int depth, index;
	int j=0;
	int *iarray;
	char *tokenptr;
	char *s;

	CIoParam *pParam = NULL;
	CIoParam *pCurrentParam = NULL;

    if(carraymap)
	{
		s = (char *)new char [1+strlen(carraymap)];
		//First read the parameter map 
		strcpy(s, carraymap);

		//Find the parameter!
		index=0;

		tokenptr = strtok(s, seps);
		if((depth=atoi(tokenptr))<1) depth =1;
		
		iarray = (int *)new int [depth];

		while(tokenptr !=NULL)
		{
			tokenptr = strtok(NULL, seps);
			if(j<depth)
				iarray [j]=atoi(tokenptr);

			j++;
		}

		j=1;
		pParam = GetParam(iarray[0]);
		do
		{
			index=iarray[j];
			pParam = pParam->GetParam(index);
		}while(++j<depth && pParam !=NULL);

		if(pParam)
			return (pParam->GetParam(i));
		else
			return NULL;
	}
	else
		return GetParam(i);

}

void CIoParam::SetChildParam(int i, char *carraymap, CIoParam *pParam,int iUpdate)
{

	CIoParam *pActualParam = NULL;
	CIoParam *pCurrentParam = NULL;

    if(carraymap)
	{
		if(pActualParam=GetChildParam(i, carraymap))
						pActualParam->SetParam(i, pParam, iUpdate);
	}
	else
		SetParam(i, pParam, iUpdate);
}

void CIoParam::SetChildParam(int i, char *carraymap, ivec *pv, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pv, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);


}



void CIoParam::SetChildParam(int i, char *carraymap, vec *pv, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pv, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);


}

void CIoParam::SetChildParam(int i, char *carraymap, matrix *pm, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pm, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);
}

void CIoParam::SetChildParam(int i, char *carraymap, float *pf, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pf, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);
}

void CIoParam::SetChildParam(int i, char *carraymap, int *pi, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pi, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);
}

void CIoParam::SetChildParam(int i, char *carraymap, char *pc, int iUpdate)
{
	CIoParam *pParam = (CIoParam *)new CIoParam;
	pParam->SetParam(pc, iUpdate);

	SetChildParam(i, carraymap, pParam, iUpdate);

}

//get and setting

ivec *CIoParam::GetChildIVecParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetIVecParam(i);
	else
		return NULL;
}


vec *CIoParam::GetChildVecParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetVecParam(i);
	else
		return NULL;
}

float CIoParam::GetChildFloatParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetFloatParam(i);
	else
		return 0;
}

int CIoParam::GetChildIntParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetIntParam(i);
	else
		return 0;
}

matrix *CIoParam::GetChildMatParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetMatParam(i);
	else
		return NULL;
}

char *CIoParam::GetChildStringParam(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetStringParam(i);
	else
		return NULL;
}

CIoParam * CIoParam::GetChildParamArray(int i, char *carraymap)
{
	CIoParam *pParam = GetChildParam(i, carraymap);
	if(pParam)
		return pParam->GetParamArray(i);
	else
		return NULL;
}

ivec *CIoParam::GetIVecParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetIVecParam();
	else return NULL;
}

vec *CIoParam::GetVecParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetVecParam();
	else return NULL;
}

float CIoParam::GetFloatParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetFloatParam();
	else return 0;
}

int CIoParam::GetIntParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetIntParam();
	else return 0;
}

matrix *CIoParam::GetMatParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetMatParam();
	else return NULL;
}

char *CIoParam::GetStringParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetStringParam();
	else return NULL;
}

string *CIoParam::GetSStringParam(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetSStringParam();
	else return NULL;
}


CIoParam *CIoParam::GetParamArray(int i)
{
	CIoParam *pParam = GetParam(i);
	if(pParam)
		return pParam->GetParamArray();
	else return NULL;
}
//Setting definitions
int CIoParam::CheckParam(CIoParam *pParam)
{
	CIoParam *pCurrentParam;
	CIoParam *pCurrentThisParam;

	int result=0;
	if(pParam && (pParam->GetType()==m_type))
	{
		switch(m_type)
		{
			case IO_PARAM_ARRAY :
			{
				pCurrentParam = pParam->GetParamArray();
				pCurrentThisParam = m_pParam;
				while(pCurrentParam && pCurrentThisParam)
				{
					result = pCurrentParam->CheckParam(pCurrentThisParam);
					if(result==0) break;
					pCurrentParam=pCurrentParam->GetNextParam();
					pCurrentThisParam=pCurrentThisParam->GetNextParam();
				}
				break;
			}
			case IO_PARAM_INTEGER :
			case IO_PARAM_FLOAT :
				result = 1;
				break;
			case IO_PARAM_VEC :
				{
					vec *currentvec = pParam->GetVecParam();
					vec *v=GetVecParam();
					int vsize1=0;
					int vsize2=v->length();
					if(currentvec) vsize1 = currentvec->length();

					if(vsize1==vsize2)
								result=1;
				}
				break;
			case IO_PARAM_MAT :
				{
					matrix *currentmat = pParam->GetMatParam();
					matrix *m = GetMatParam();
					int mr1=0, mc1=0;
					int mr2 = m->depth();
					int mc2 = m->width();
		
					if(currentmat)
					{
						mr1 = m->depth();
						mc1 = m->width();
					}

					if(mr1==mr2 && mc1==mc2)
						result = 1;
				}
				break;
			case IO_PARAM_STRING :
				{
					char *currentstr = pParam->GetStringParam();
					char *s=GetStringParam();
					int csize1=0;
					int csize2=strlen(s);
					if(currentstr) csize1 = strlen(currentstr);

					if(csize1==csize2)
						result = 1;
				}
				break;

		}
	}
	return result;
}

std::istream& operator>>(std::istream& s, CIoParam &p)
// format: list of floating point numbers  followed by ','
{
	int i, iNumParam=0;
	
	int iin;
	float fin;
	
    CIoParam *pCurrentParam;
    
    if(s.eof())
			return s;
	//Two cases can be read
	//1 the param is a single param
	//the param is an array of params
	if((p.m_type)==IO_PARAM_ARRAY)
	{
				pCurrentParam = p.GetParamArray();				
				while(pCurrentParam)
				{
					
					s>> *pCurrentParam;
					pCurrentParam=pCurrentParam->GetNextParam();
				}		
	}
	else
	{
		switch(p.m_type)
		{
			case IO_PARAM_INTEGER :
			    s>>iin;
			    p.SetParam(&iin);			
								
			break;
			
			case IO_PARAM_FLOAT :
			    s>>fin;
				p.SetParam(&fin);
			break;

			case IO_PARAM_VEC :
				s>> *(p.GetVecParam());
			break;			

			case IO_PARAM_MAT :
				s>> *(p.GetMatParam());
			break;			

			case IO_PARAM_STRING :
				s>> (p.GetStringParam());
			break;
			
			case IO_PARAM_IVEC :
				s>> *(p.GetIVecParam());
			break;			
			
		}
		
	}
	return s;
}



std::ostream& operator<<(std::ostream& s, CIoParam &p)
// format: list of floating point numbers  followed by ','
{
	int i, iNumParam=0;
	
	int iin;
	float fin;
	
    CIoParam *pCurrentParam;
    
    //if(s.eof())
	//		return s;
	//Two cases can be read
	//1 the param is a single param
	//the param is an array of params
	if((p.m_type)==IO_PARAM_ARRAY)
	{
				pCurrentParam = p.GetParamArray();				
				while(pCurrentParam)
				{
					
					s<< *pCurrentParam ;
					s<< " ";
					s<< (pCurrentParam->GetDelimeter());
					pCurrentParam=pCurrentParam->GetNextParam();
				}		
	}
	else
	{
		switch(p.m_type)
		{
			case IO_PARAM_INTEGER :
			    s<<p.GetIntParam();
			    //p.SetParam(&iin);			
								
			break;
			
			case IO_PARAM_FLOAT :
			    s<<p.GetFloatParam();
				//p.SetParam(&fin);
			break;

			case IO_PARAM_VEC :
				s<< *(p.GetVecParam());
			break;			

			case IO_PARAM_MAT :
				s<< *(p.GetMatParam());
			break;			

			case IO_PARAM_STRING :
				s<< (p.GetStringParam());
			break;
			
			case IO_PARAM_IVEC :
				s<< *(p.GetIVecParam());
			break;			
			
		}
		
	}
	return s;
}



istream &CIoParam::ReadParam(istream &s, int noparams, int index)
{
	char keyword[16], val[16];
	CIoParam *pParam=NULL;
	//s>>keyword;
	if(!s || s.eof() || s.fail())  //end of file or read failure
	   return s;
	   
	if(m_pParam)
		s >> *m_pParam;
		
	else
		return s;
	
	return s;
}

ostream &CIoParam::WriteParam(ostream &s, int noparams, int index)
{
	char keyword[16], val[16];
	CIoParam *pParam=NULL;
	//s>>keyword;
	if(!s || s.eof() || s.fail())  //end of file or read failure
	   return s;
	   
	if(m_pParam)
		s << *m_pParam;
		
	else
		return s;
	
	return s;
}

istream &CIoParam::ReadFlaggedParamArray(istream &s, int flag, dqint pFlagArray, int noparams, int index)
{
	char keyword[16], val[16];
	CIoParam *pParam=NULL;
	//s>>keyword;
	if(!s || s.eof() || s.fail())  //end of file or read failure
	   return s;
	   
	if(m_pParam)
		ReadFlaggedParam(s, m_pParam, flag, pFlagArray, noparams, index);		
	else
		return s;
	
	return s;
}

ostream &CIoParam::WriteFlaggedParamArray(ostream &s, int flag, dqint pFlagArray, int noparams, int index)
{
	char keyword[16], val[16];
	CIoParam *pParam=NULL;
	//s>>keyword;
	if(!s || s.eof() || s.fail())  //end of file or read failure
	   return s;
	   
	if(m_pParam)
		WriteFlaggedParam(s, m_pParam, flag, pFlagArray, noparams, index);		
		
	else
		return s;
	
	return s;
}

istream &CIoParam::ReadFlaggedParam(istream &s, CIoParam p, int flag, dqint pFlagArray, int noparams, int index)
{
	int i, iNumParam=0, count, arraycount;
	
	int iin, itempflag;
	float fin;
	
    CIoParam *pCurrentParam;
	CIoParam *pCurrentFlags;
	CIoParam *pCurrentFlagArray, *pTempFlagArray;
	int pTempFlag, pFlag;
    
    if(s.eof())
			return s;
	//Two cases can be read
	//1 the param is a single param
	//the param is an array of params
	if((p.m_type)==IO_PARAM_ARRAY)
	{
		if(flag)
		{
				count=p.GetIntParam();
			for(i=0; i<count; i++)
			{
				if(i<=pFlagArray.size())
					pTempFlag=pFlagArray[i];

				pCurrentParam=p.GetParam(i);
				ReadFlaggedParam(s, *pCurrentParam, pTempFlag, pFlagArray, 1);

			}
				
		}
	
				
	
	}
	else if(flag)
	{
		switch(p.m_type)
		{
			case IO_PARAM_INTEGER :
			    s>>iin;
			    p.SetParam(&iin);			
								
			break;
			
			case IO_PARAM_FLOAT :
			    s>>fin;
				p.SetParam(&fin);
			break;

			case IO_PARAM_VEC :
				s>> *(p.GetVecParam());
			break;			

			case IO_PARAM_MAT :
				s>> *(p.GetMatParam());
			break;			

			case IO_PARAM_STRING :
				s>> (p.GetStringParam());
			break;
			
			case IO_PARAM_IVEC :
				s>> *(p.GetIVecParam());
			break;			
			
		}
		
	}
	return s;

}

ostream &CIoParam::WriteFlaggedParam(ostream &s, CIoParam p, int flag, dqint pFlagArray, int noparams, int index)
{
	int i, iNumParam=0, count, arraycount;
	
	int iin;
	float fin;
	
    CIoParam *pCurrentParam;
	dqint pCurrentFlags;
	dqint pCurrentFlagArray, pTempFlagArray;
	int pTempFlag, pFlag;
    
    //if(s.eof())
	//		return s;
	//Two cases can be read
	//1 the param is a single param
	//the param is an array of params
	if((p.m_type)==IO_PARAM_ARRAY)
	{
		if(flag)
		{
				count=p.GetIntParam();
			for(i=0; i<count; i++)
			{
				if(i<=pFlagArray.size())
					pTempFlag=pFlagArray[i];

				pCurrentParam=p.GetParam(i);
				WriteFlaggedParam(s, *pCurrentParam, pTempFlag, pFlagArray, 1);

			}
		}
	}
	else if(flag)
	{
		switch(p.m_type)
		{
			case IO_PARAM_INTEGER :
			    s<<p.GetIntParam();
			    //p.SetParam(&iin);			
								
			break;
			
			case IO_PARAM_FLOAT :
			    s<<p.GetFloatParam();
				//p.SetParam(&fin);
			break;

			case IO_PARAM_VEC :
				s<< *(p.GetVecParam());
			break;			

			case IO_PARAM_MAT :
				s<< *(p.GetMatParam());
			break;			

			case IO_PARAM_STRING :
				s<< (p.GetStringParam());
			break;
			
			case IO_PARAM_IVEC :
				s<< *(p.GetIVecParam());
			break;			
			
		}
		
	}

	return s;
}



int CIoParam::ReadParams(int n,  char *name)
{
	char fn[16];
	sprintf(fn, "%s", name);
	int i=0;
	
	ifstream def(fn, std::ios::in);
	
	if(!def)
	{
		std::cerr << "Failed to find parameter definition fille.\n";
		return 0;			
	}
	
	while(ReadParam(def, n, i++) && !def.eof())
	;
	
	return 0;	
}



int CIoParam::skipcmt(ifstream &s)
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

