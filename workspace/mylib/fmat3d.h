#ifndef IO_UTIL_FMAT3D_H
#define IO_UTIL_FMAT3D_H
// fmat3d.h: interface for the fmat3d class.
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

Author contact inforfmat3dion:
mikeg@photon0.freeserve.co.uk
--------------------------------------------------------------------------------
*/
	

//#pragma once



#include "vec.h"
#include "mat.h"



class vecpair;
using std::istream;
using std::ostream;
class matrix3d {
	//friend class hop;
	//friend class tsp;
	friend ostream& operator<<(ostream& s, matrix3d& m1);
	friend istream& operator>>(istream& s, matrix3d& m1);
	protected:
		float ***m; // the matrix3d representation
		int r,c,d;   // number of rows and columns and depth

	public:
		//constructors
		matrix3d(int n=ROWS, int p=COLS, int q=DEPTH, float range=0);
		matrix3d(int n, int p, int q, float value, float range);
		matrix3d(int n, int p, int q, char *fn);
		//matrix3d(const vecpair& vp);
		matrix3d(matrix3d& m1); //copy-initializer
		
		virtual ~matrix3d();
		int rows();
		int cols();
		int height();
		matrix3d& operator=(const matrix3d& m1);
		matrix3d& operator+(const matrix3d& m1);
		void resize(int nr, int nc, int nd);
		/*vec operator*(vec& v1);*/
		matrix *colslice(int col);
		void colslice(int col, matrix *v);
		matrix *rowslice(int row);
		void rowslice(int row, matrix *v);
		matrix *depslice(int dep);
		void depslice(int dep, matrix *v);
		void insertcol(matrix& v, int col);
		void insertrow(matrix& v, int row);
		//int closestcol(vec& v);
		//int closestcol(vec& v, int *wins, float scaling);
		//int closestrow(vec& v);
		//int closestrow(vec& v, int *wins, float scaling);
		int load(FILE *f);
		int save(FILE *f);
		float** operator[] (const int i) { return m[i]; }
		matrix3d& operator+=(const matrix3d& m1);
		matrix3d& operator*(const float d);
		matrix3d& operator*=(const float d);
		//void initvals(const vec& v1, const vec& v2,
		//	const float rate=1.0, const float momentum=0.0);
		float getval(int row, int col, int dep=0);
		void setval (int row, int col, int dep, float val);



	}; //matrix3d class

#endif



