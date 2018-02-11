#ifndef IO_UTIL_PROPERTYMANAGER_H
#define IO_UTIL_PROPERTYMANAGER_H


// IoPropertyManager.h: interface for the CIoPropertyManager class.
//
//////////////////////////////////////////////////////////////////////

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
	



#ifdef IO_MSVC
	#include "IoParams.h"
#else
	#include "IoParams.h"
#endif

//WHY DO I EXIST?
//A property manager is a class which adds functionality to the 
//manipulation of params

//Has methods enabling checking and creation ofparameter structures
//Has methods for defining names of the structural elements

//Rule for memory management by property managers
//A parameter must always be stuffed into a property manager
//if it was created externally to the property manager it must be deleted
//immediately  .... ensure no memory leaks

//For a property manager we therefore only ever need to delete 
//m_pProperties  ....

//The same rule will apply to CIoParam

//The action of setting and adding
//always result in the creation of a copy of the parameter
//the parameter will then be destroyed by the calling function
#include <fstream>
#include <deque>
#include <string>

using std::string;
using std::deque;
using std::ifstream;

typedef deque<string> dqstr;
typedef deque<int>    dqint;

class CIoPropertyManager  
{
public:
	CIoPropertyManager();
	virtual ~CIoPropertyManager();

	void DeleteNames();
	void DeleteFlags();
    void DeleteProperties();
	void CreateProperties(CIoParam *pParam=NULL);
	CIoParam *m_pProperties;

	deque<string> m_pPropNames;
	deque<int> m_pPropFlags;
	deque<string> m_pArrayNames;
	deque<int> m_pArrayFlags;
	
	void CopyProps(CIoPropertyManager *pPropMan);
	void CopyNames(CIoPropertyManager *pPropMan);
	void CopyFlags(CIoPropertyManager *pPropMan);


	//Each time prop names are set must remember to delete the old property names
	dqstr &GetPropNames(){return m_pPropNames;}
	//void SetPropNames(CIoParam *pPropNames);
	//CIoParam *GetPropNames(){return m_pPropNames;}
	
	dqint &GetPropFlags(){return m_pPropFlags;}

	int GetNumPropNames(){return (int)(m_pPropNames.size());}
	int GetNumArrayNames(){return (int)(m_pArrayNames.size());}
	int GetNumPropFlags(){return (int)(m_pPropFlags.size());}
	int GetNumArrayFlags(){return (int)(m_pArrayFlags.size());}

	void ReadPropNames(istream &s);
	void ReadArrayNames(istream &s);
	void ReadPropFlags(istream &s);
	void ReadArrayFlags(istream &s);
	void WritePropNames(ostream &s);
	void WriteArrayNames(ostream &s);
	void WritePropFlags(ostream &s);
	void WriteArrayFlags(ostream &s);

	//Names of the arrays
	//This array is constructed as follows

	//Parameter array of two elements
	//0  Name of the array
	//1 array containing names of the arrays
	//contained by m_pProperties
    //Element 1 always exists if it is null array names have not been defined
	//or the array contains no arrays
	//Each element of this array is then organised as follows
	//elements organiesd in pairs
	//0 The name of the array
	//1 array of names for subarrays

	//....
	//....
	//....

	//0 The name of the array
	//1 array of names for subarrays

	

	//Each time prop array names are set must remember to delete the old property array names
	dqstr GetPropArrayNames(){return m_pArrayNames;}
	//void SetPropArrayNames(CIoParam *pPropArrayNames);
	//CIoParam *GetPropArrayNames(){return m_pPropNames;}


	dqint GetPropArrayFlags(){return m_pArrayFlags;}


	//Methods for accessing individual names
	char *GetName();  //Gets name of m_pProperties array
	string GetPropertyName(int i);
	string GetArrayName(int i=0);
	string GetChildPropertyName(int i, char *carraymap);
	string GetChildArrayName(int i, char *carraymap);
	int GetPropertyArrayNameIndex(int index, CIoParam *pPropertyNames);
 
	void SetPropertyFlag(int i, int flag);
	void SetArrayFlag(int i, int flag);
	void SetPropertyName(int i, char *sname);
	void SetArrayName(int i, char *sname);

    int GetPropertyFlag(int i);
	int GetArrayFlag(int i);
    int GetChildPropertyFlag(int i, char *carraymap);
	int GetChildArrayFlag(int i, char *carraymap);
	int GetPropertyArrayFlagIndex(int index, CIoParam *pPropertyFlags=NULL);

	void AddPropName(string name);
	void AddArrayName(string name);
	void AddPropName(char *name);
	void AddArrayName(char *name);
	void AddPropFlag(int flag);
	void AddArrayFlag(int flag);


	//Each property manager has a method
	//for creating property names
	//this method will always call set prop names
	virtual void CreatePropNames(){;}
	virtual void CreatePropArrayNames(){;}

	virtual void CreatePropFlags(){;}
	virtual void CreatePropArrayFlags(){;}
	int GetNumProps(){return(m_pProperties?m_pProperties->GetNumParams():0);}
	inline CIoParam *GetProperty(int i){return(m_pProperties?m_pProperties->GetParam(i):NULL);}
	inline CIoParam *GetArray(int i){return(m_pProperties?m_pProperties->GetParamArray(i):NULL);}
	inline float GetFloat(int i){return(m_pProperties?m_pProperties->GetFloatParam(i):0);}
	inline int GetInt(int i){return(m_pProperties?m_pProperties->GetIntParam(i):0);}
	inline vec *GetVec(int i){return(m_pProperties?m_pProperties->GetVecParam(i):NULL);}
	inline ivec *GetIVec(int i){return(m_pProperties?m_pProperties->GetIVecParam(i):NULL);}
	inline matrix *GetMat(int i){return(m_pProperties?m_pProperties->GetMatParam(i):NULL);}
	inline char *GetString(int i){return(m_pProperties?m_pProperties->GetStringParam(i):NULL);}
	inline string *GetSString(int i){return(m_pProperties?m_pProperties->GetSStringParam(i):NULL);}

		//Method to read parameters from child, grand child, great grand child arrays....
	inline CIoParam *GetChildParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildParam(i,carraymap):NULL);}
	inline ivec *GetChildIVecParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildIVecParam(i,carraymap):NULL);}
	inline vec *GetChildVecParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildVecParam(i,carraymap):NULL);}
	inline float GetChildFloatParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildFloatParam(i,carraymap):0);}
	inline int GetChildIntParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildIntParam(i,carraymap):0);}
	inline matrix *GetChildMatParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildMatParam(i,carraymap):NULL);}
	inline char *GetChildStringParam(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildStringParam(i,carraymap):NULL);}

	inline CIoParam * GetChildParamArray(int i, char *carraymap=NULL){return(m_pProperties?m_pProperties->GetChildParamArray(i,carraymap):NULL);}
	IoVarType GetType(int i, char *carraymap=NULL);


	//The parameter iUpdate is 1 if it is being updated int the case of matrices strings and vectors
	//we check that the matrix, vector or string has the same size as the original string
	//else if iUpdate=0 we are initialising and can do whatever we like
	inline void SetParam(int i, CIoParam *pParam, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pParam, iUpdate);}
	inline void SetParam(int i, ivec *pv, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pv, iUpdate);}
	inline void SetParam(int i, vec *pv, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pv, iUpdate);}
	inline void SetParam(int i, matrix *pm, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pm, iUpdate);}
	inline void SetParam(int i, float *pf, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pf, iUpdate);}
	inline void SetParam(int i, int *pi, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pi, iUpdate);}
	inline void SetParam(int i, char *pc, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,pc, iUpdate);}
	inline void SetParam(int i, string s, int iUpdate=0){if(m_pProperties)m_pProperties->SetParam(i,s, iUpdate);}

	//Method to set parameters for child, grand child, great grand chhild arrays....
	inline void SetChildParam(int i, char *carraymap=NULL, CIoParam *pParam=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pParam, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, ivec *pv=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pv, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, vec *pv=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pv, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, matrix *pm=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pm, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, float *pf=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pf, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, int *pi=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pi, iUpdate);}
	inline void SetChildParam(int i, char *carraymap=NULL, char *pc=NULL, int iUpdate=0){if(m_pProperties)m_pProperties->SetChildParam(i,carraymap,pc, iUpdate);}

	void SetProperty(int i, CIoParam *pParam=NULL, int iUpdate=0);
	void SetArray(int i, CIoParam *pParam=NULL, int iUpdate=0);
	void SetFloat(int i, float f=0, int iUpdate=0);
	void SetInt(int i, int ii=0, int iUpdate=0);
	void SetIVec(int i, ivec *v=NULL, int iUpdate=0);
	void SetVec(int i, vec *v=NULL, int iUpdate=0);
	void SetMat(int i, matrix *m=NULL, int iUpdate=0);
	void SetString(int i, char *s=NULL, int iUpdate=0);
	void SetString(int i, string s, int iUpdate=0);

	void AddProperty(CIoParam *pParam);
	void AddArray(CIoParam *pParam);
	void AddFloat(float f);
	void AddInt(int ii);
	void AddIVec(ivec *v);
	void AddVec(vec *v);
	void AddMat(matrix *m);
	void AddString(char *s);
	void AddString(string s);



	//A property manager will always require methods for creating the default
	//parameters and for checking that a given parameter set has the same format 
	// as this manager
	virtual void CreateDefaultParams()=0;
	int CheckParamFormat(CIoParam *pParamData=NULL);
	void UpdateProperties(CIoParam *pPropertyData);
	int skipcmt(ifstream &s);
	istream &ReadProps(istream &s, int noparams, int index=0);
	ostream &WriteProps(ostream &s, int noparams, int index=0);
	istream &ReadFlaggedProps(istream &s, int flag, dqint pArrayFlags, int noparams, int index=0);
	ostream &WriteFlaggedProps(ostream &s, int flag, dqint pArrayFlags, int noparams, int index=0);

	istream &ReadFlaggedPropNames(istream &s, int flag, dqint pArrayFlags, int noparams, int index=0);
	ostream &WriteFlaggedPropNames(ostream &s, int flag, dqint pArrayFlags, int noparams, int index=0);


	//Get property methods index returns particular type
	//always checks that property with particular index exists and is correct

	//set property
};

#endif

