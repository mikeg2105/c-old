#ifndef IO_UTIL_MMAT3D_H
#define IO_UTIL_MMAT3D_H
// mmat3d.h: interface for the mmat3d class.
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

Author contact informmat3dion:
mikeg@photon0.freeserve.co.uk
--------------------------------------------------------------------------------
*/
	

//#pragma once



#include "vec.h"
#include "mat.h"



class vecpair;
using std::istream;
using std::ostream;
class mmat3d {
	//friend class hop;
	//friend class tsp;
	friend ostream& operator<<(ostream& s, mmat3d& m1);
	friend istream& operator>>(istream& s, mmat3d& m1);
	protected:
		float *****m; // the mmat3d representation
		int r,c,d;   // number of rows and columns and depth
		int mr,mc;

	public:
		//constructors
		//mmat3d(int n, int p, int q, int mn, int mm, float range);
		//mmat3d(int n, int p, int q, int mn, int mm, float value,  float range);
		//mmat3d(int n, int p, int q, int mn, int mm,  char *fn);
		
		mmat3d(int n=ROWS, int p=COLS, int q=DEPTH, int mn=0, int mm=0, float range=0);
		mmat3d(int n, int p, int q, int mn, int mm, float value, float range);
		mmat3d(int n, int p, int q, int mn, int mm, char *fn);		
		
		//mmat3d(const vecpair& vp);
		mmat3d(mmat3d& m1); //copy-initializer
		
		virtual ~mmat3d();
		int rows();
		int cols();
		int depth();
		
		int mrows(){return mr;}
		int mcols(){return mc;}
		mmat3d& operator=(const mmat3d& m1);
		mmat3d& operator+(const mmat3d& m1);
		mmat3d& mmat3d::operator+=(const float d);
		void resize(int nr, int nc, int nd, int nmr, int nmc);

		int load(FILE *f);
		int save(FILE *f);
		float**** operator[] (const int i) { return m[i]; }
		mmat3d& operator+=(const mmat3d& m1);
		mmat3d& operator*(const float d);
		mmat3d& operator*=(const float d);
		//void initvals(const vec& v1, const vec& v2,
		//	const float rate=1.0, const float momentum=0.0);
		float getval(int row, int col, int dep, int lr, int lc);
		void setval (int row, int col, int dep, int lr, int lc, float val);

		//vec &getvval(int row, int col, int dep, int lr);
		//void setvval (int row, int col, int dep, int lr, vec val);
		
		//matrix getmval(int row, int col, int dep);
		//void setmval (int row, int col, int dep, matrix val);

	}; //mmat3d class

#endif



