#ifndef IO_UTIL_VMAT3D_H
#define IO_UTIL_VMAT3D_H
// mat.h: interface for the mat class.
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
	

//#pragma once



#include "vec.h"




class vecpair;
using std::istream;
using std::ostream;
class vmatrix3d {
	//friend class hop;
	//friend class tsp;
	friend ostream& operator<<(ostream& s, vmatrix3d& m1);
	friend istream& operator>>(istream& s, vmatrix3d& m1);
	protected:
		float **m; // the vmatrix3d representation
		int r,c;   // number of rows and columns

	public:
		//constructors
		vmatrix3d(int n=ROWS, int p=COLS, float range=0);
		vmatrix3d(int n, int p, float value, float range);
		vmatrix3d(int n, int p, char *fn);
		//vmatrix3d(const vecpair& vp);
		vmatrix3d(vmatrix3d& m1); //copy-initializer
		
		virtual ~vmatrix3d();
		int depth();
		int width();
		vmatrix3d& operator=(const vmatrix3d& m1);
		vmatrix3d& operator+(const vmatrix3d& m1);
		void resize(int nr, int nc);
		vec operator*(vec& v1);
		vec *colslice(int col);
		void colslice(int col, vec *v);
		vec *rowslice(int row);
		void rowslice(int row, vec *v);
		void insertcol(vec& v, int col);
		void insertrow(vec& v, int row);
		int closestcol(vec& v);
		int closestcol(vec& v, int *wins, float scaling);
		int closestrow(vec& v);
		int closestrow(vec& v, int *wins, float scaling);
		int load(FILE *f);
		int save(FILE *f);

		vmatrix3d& operator+=(const vmatrix3d& m1);
		vmatrix3d& operator*(const float d);
		vmatrix3d& operator*=(const float d);
		void initvals(const vec& v1, const vec& v2,
			const float rate=1.0, const float momentum=0.0);
		float getval(int row, int col);
		void setval (int row, int col, float val);



	}; //vmatrix3d class

#endif



