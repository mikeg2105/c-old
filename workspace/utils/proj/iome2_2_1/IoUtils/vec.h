
#ifndef IO_UTIL_VEC_H
#define IO_UTIL_VEC_H
// vec.h: interface for the vec class.
//
//////////////////////////////////////////////////////////////////////
//hrllo

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

#include <iostream>
#include <fstream>
#include <iomanip>


// No max or min for C++
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

//#include "depug.h"

double logistic(double activation);
// will be changed to values much higher than these
const int ROWS = 64; //Number of rows (length of first pattern)
const int COLS = 64; //Number of colums (length of second pattern)
const int DEPTH = 1; //Number of colums (length of second pattern)
const int MAXVEC = 64; //Default size of vector


class matrix;
class CIoPropertyManager;

using std::ostream;
using std::istream;

class vec  
{

	    friend ostream& operator<< (ostream& s, vec& v1);
        friend class matrix;
		friend class CIoPropertyManager;

        friend istream& operator>> (istream& s, vec& v1);
		int n;

public:

		float *v;


	void unit();
	float dot(vec &v1);
	vec  cross(vec &v1);
	vec(int size=MAXVEC, float val=0); //constructor
	virtual ~vec();

		vec(vec &v1); //copy-initializer
		int length();
		float magnitude();

		float distance(vec& A);
		vec& normalise();
		vec& normaliseon();
		vec& scale(vec& minvec, vec& maxvec);
		// dot product of vector and complement
		float d_logistic();
		float maxval();
		void add( vec *v1, float c=1);
		void multiply( float c);
		vec& garble(float noise);
		vec& randomize(float range);
		vec& operator=(const vec& v1); //vector assignment
		vec& operator+(const vec& v1); //vector addition
		vec& operator+(const float d);
		//vector additive-assignment
		vec& operator+=(const vec& v1);
		vec& operator-=(const vec& v1);

		//vector multiply by constant
		vec& operator*=(float c);
		// vector transpose multiply needs access to v array
		int operator==(const vec& v1);
		float operator[](int x);
		int vec::maxindex();
		vec& getstr(char *s);
		void putstr(char *s);
		float get(int x);
		
		vec operator-(const vec& v1);	//subtraction
		vec operator-(const float d);   //subtraction
		float operator*(const vec& v1); //dot-product
		vec operator*(float c); //multiply by constant
		int operator !=(const  vec v1);
		vec& sigmoid();
		vec& sigmoid(vec &thresh);
		vec& set(int i, float f=0);
		vec& setvec(int size, float *pf=NULL);
		     int load (FILE *f);
		     int save (FILE *f);
	    ostream& write(ostream& s, vec& v1);
		istream& read(istream& s, vec& v1);

		//Converts a space separated list of numbers
		//to a vector
		void stringtovec(char *s);
		void vectostr(char *vstring);
		int vecstrlen();


}; //vector class

#endif


