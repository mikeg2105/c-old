#ifndef IO_UTIL_PARAMS_H
#define IO_UTIL_PARAMS_H

// IoParams.h: interface for the CIoParams class.
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
	




#include <stdlib.h>
//#include <io.h>
//#include <conio.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <float.h>
//#include <sys\stat.h>
//in the space
#include <iostream>
#include <fstream>
#include <iomanip>

#include <deque>
#include <string>

using std::string;
using std::deque;
using std::ifstream;

typedef deque<string> dqstr;
typedef deque<int>    dqint;

#include "vec.h"
#include "ivec.h"
#include "mat.h"
#include "IoString.h"

typedef vec VEC;
typedef VEC *vecptr;

typedef ivec IVEC;
typedef IVEC *ivecptr;


typedef matrix MATRIX;
typedef MATRIX *matptr;

//typedef char *sstring;
typedef IoString IOSTRING;
typedef IOSTRING *sstring;


 

//typedef  int UINT;


//Further checking required for memory leaks!!!
//Particularly matrix and vec!

//Add overloaded methods for CIoParam opertions  + - * / = == += *= -= /=....
//enum BOOL {FALSE=0, TRUE=1, yes=1, no=0, on=1, off=0};

enum IoVarType {IO_PARAM_ARRAY, IO_PARAM_INTEGER, IO_PARAM_FLOAT, IO_PARAM_VEC, IO_PARAM_MAT, IO_PARAM_STRING, IO_PARAM_IVEC, IO_PARAM_NULL};
using std::ifstream;
class CIoParam  
{

	friend istream& operator>> (istream& s, CIoParam& p);
	friend ostream& operator<< (ostream& s, CIoParam& p);

public:

	CIoParam();

	//Copy constructor
	CIoParam(CIoParam *pParam);
	CIoParam(float *f){SetParam(f);}
	CIoParam(int *i){SetParam(i);}
	CIoParam(vec *v){SetParam(v);}
	CIoParam(matrix *m){SetParam(m);}
	CIoParam(char *c){SetParam(c);}
	CIoParam(string s){SetParam(s);}
	CIoParam(ivec *v){SetParam(v);}

	virtual ~CIoParam();
	void DeleteParams();
	
    
	   
    CIoParam *m_pParam;
	CIoParam *m_pNextParam;

	IoVarType m_type;
	string m_sDelimeter; //Delimeter character used to separate parameters
	                   //when using ostream operator

	union IoData {

		float pf;
		int pi;
		vecptr pv;
		ivecptr piv;
		matptr pm;
		sstring pc;
		

	} m_val;


	CIoParam *GetParam(int i);
	
	string GetDelimeter(){return m_sDelimeter;}
	void SetDelimeter(char *sDeli){m_sDelimeter=sDeli;}

	//Method to read parameters from child, grand child, great grand child arrays....
	CIoParam *GetChildParam(int i, char *carraymap=NULL);

	ivec *GetChildIVecParam(int i, char *carraymap=NULL);
	vec *GetChildVecParam(int i, char *carraymap=NULL);
	float GetChildFloatParam(int i, char *carraymap=NULL);
	int GetChildIntParam(int i, char *carraymap=NULL);
	matrix *GetChildMatParam(int i, char *carraymap=NULL);
	char *GetChildStringParam(int i, char *carraymap=NULL);

	CIoParam * GetChildParamArray(int i, char *carraymap=NULL);

	ivec *GetIVecParam(int i);
	vec *GetVecParam(int i);
	float GetFloatParam(int i);
	int GetIntParam(int i);
	matrix *GetMatParam(int i);
	char *GetStringParam(int i);
	CIoParam *GetParamArray(int i);
	string *GetSStringParam(int i);

	CIoParam *GetParamArray(){return m_pParam;}
	CIoParam *GetNextParam(){return m_pNextParam;}
	void SetNextParam(CIoParam *pParam){m_pNextParam = pParam;}
	void AddParam(CIoParam *pParam = NULL);
	void AddParam(ivec *pv);
	void AddParam(vec *pv);
	void AddParam(matrix *pm);
	void AddParam(float *pf);
	void AddParam(int *pi);
	void AddParam(char *pc);
	void AddParam(string pc);

	void DeleteParam(CIoParam *pParam);
	void DeleteParam(int i);
	int GetNumParams();

	ivec *GetIVecParam(){return (m_type==IO_PARAM_IVEC?m_val.piv:NULL);}
	vec *GetVecParam(){return (m_type==IO_PARAM_VEC?m_val.pv:NULL);}
	float GetFloatParam(){return (m_type==IO_PARAM_FLOAT?m_val.pf:NULL);}
	int GetIntParam(){return ((m_type==IO_PARAM_INTEGER || m_type==IO_PARAM_ARRAY)?m_val.pi:NULL);}
	matrix *GetMatParam(){return (m_type==IO_PARAM_MAT?m_val.pm:NULL);}
	char *GetStringParam(){return (m_type==IO_PARAM_STRING?((m_val.pc)->GetString()):NULL);}
	string *GetSStringParam(){return (m_type==IO_PARAM_STRING?((m_val.pc)->GetSString()):NULL);}

	/*!
		Default input parameter for this function is -1 if
		user supplies a value greater zero then the input
		value will be compared with the array size
	*/
	int IsArrayOfArrays(int isize=-1);

	//A routine that checks this param with a test parameter
	//It checks that parameters are of the same type
	//If parameter is an array then we check that each element of the array 
	//is of the same type
	//if parameter is an matrix check that it has same nummber of rows and columns
	//if parameter is sstring or vector check that same length
    int CheckParam(CIoParam *pParam);

	void SetParam(ivec *pv,int iUpdate=0);
	void SetParam(vec *pv,int iUpdate=0);
	void SetParam(matrix *pm,int  iUpdate=0);
	void SetParam(float *pf,int  iUpdate=0);
	void SetParam(int *pi,int  iUpdate=0);
	void SetParam(char *pc,int  iUpdate=0);
	void SetParam(string pc,int  iUpdate=0);
	void SetParam(CIoParam *pParam,int  iUpdate=0);



	void SetParam(int i, CIoParam *pParam,int  iUpdate=0);

	void SetParam(int i, ivec *pv,int  iUpdate=0);
	void SetParam(int i, vec *pv,int  iUpdate=0);
	void SetParam(int i, matrix *pm,int  iUpdate=0);
	void SetParam(int i, float *pf,int  iUpdate=0);
	void SetParam(int i, int *pi,int  iUpdate=0);
	void SetParam(int i, char *pc,int  iUpdate=0);
	void SetParam(int i, string s,int  iUpdate=0);

	//Method to set parameters for child, grand child, great grand chhild arrays....
	void SetChildParam(int i, char *carraymap=NULL, CIoParam *pParam=NULL, int iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, ivec *pv=NULL,int  iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, vec *pv=NULL,int  iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, matrix *pm=NULL,int  iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, float *pf=NULL, int iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, int *pi=NULL, int iUpdate=0);
	void SetChildParam(int i, char *carraymap=NULL, char *pc=NULL, int iUpdate=0);
	
	//Assume parameters have been defined with default values
	istream &ReadParam(istream &s, int noparams, int index=0);
	ostream &WriteParam(ostream &s, int noparams, int index=0);

	//Assume parameters have been defined with default values
	istream &ReadFlaggedParam(istream &s, CIoParam p, int flag, dqint pFlagArray, int noparams, int index=0);
	ostream &WriteFlaggedParam(ostream &s, CIoParam p, int flag, dqint pFlagArray, int noparams, int index=0);

	istream &ReadFlaggedParamArray(istream &s, int flag, dqint pFlagArray, int noparams, int index=0);
	ostream &WriteFlaggedParamArray(ostream &s, int flag, dqint pFlagArray, int noparams, int index=0);

	int ReadParams(int n, char *name);
	int skipcmt(ifstream &s);
	IoVarType GetType(){return m_type;}


};


//Example usage making use of symbolic definitions
//Parameters form another object child array 1
/*enum eIoManEntity1Params{IO_PROPMANENTITY1_FLOAT_P1,
						  IO_PROPMANENTITY1INTEGER_P2,
						  IO_PROPMANENTITY1_ARRAY_P3,
						  IO_PROPMANENTITY1_ARRAY_P4};*/


//parameters for child array 2
/*enum eIoManEntity2Params{IO_PROPMANENTITY2_FLOAT_P1,
						  IO_PROPMANENTITY2_INTEGER_P2,
						  IO_PROPMANENTITY2_ARRAY_P3
						  };*/


//Grand Children
/*enum eIoManEntity11Params{IO_PMEGRANDCHILD11_FLOAT_P1,
						  IO_PMEGRANDCHILD11_INTEGER_P2,
						  IO_PMEGRANDCHILD11_ARRAY_P3
						  };

enum eIoManEntity12Params{IO_PMEGRANDCHILD12_FLOAT_P1,
						  IO_PMEGRANDCHILD12_INTEGER_P2,
						  };

enum eIoManEntity21Params{IO_PMEGRANDCHILD21_FLOAT_P1,
						  IO_PMEGRANDCHILD21_INTEGER_P2,
						  };*/
//Parameters used to map elements of property array
//template for property array structure
/*enum eIoManEntityParams{IO_PROPMANENTITY_FLOAT_P1,
						  IO_PROPMANENTITY_INTEGER_P2,
					      IO_PROPMANENTITY_VEC_P3, 
						  IO_PROPMANENTITY_MAT_P4, 
						  IO_PROPMANENTITY_ARRAY_P5,
						  IO_PROPMANENTITY_ARRAY_P6};*/

//Sub array access definitions
//child  parameter map array map
//#define IO_PROPMANENTITY_CHILD1 "1 IO_PROPMANENTITY_ARRAY_P5"
//#define IO_PROPMANENTITY_CHILD2 "1 IO_PROPMANENTITY_ARRAY_P6"

//Grand Child parameter array definitions
//#define IO_PROPMANENTITY_AMAP_GCHILD1 "2 IO_PROPMANENTITY_ARRAY_P5 IO_PROPMANENTITY1_ARRAY_P3"
//#define IO_PROPMANENTITY_AMAP_GCHILD2 "2 IO_PROPMANENTITY_ARRAY_P6 IO_PROPMANENTITY1_ARRAY_P4"
//#define IO_PROPMANENTITY_AMAP_GCHILD3 "2 IO_PROPMANENTITY_ARRAY_P6 IO_PROPMANENTITY2_ARRAY_P3"

//Great Grand Child parameter array definitions*/
//#define IO_PROPMANENTITY_AMAP_GGCHILD1 "3 IO_PROPMANENTITY_ARRAY_P5 IO_PROPMANENTITY1_ARRAY_P3 IO_PMEGRANDCHILD11_ARRAY_P3"

#endif
