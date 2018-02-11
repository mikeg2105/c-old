
#ifndef IO_UTIL_IVEC_H
#define IO_UTIL_IVEC_H
// ivec.h: interface for the ivec class.
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


// No imax or imin for C++
#define imax(a,b) (((a) > (b)) ? (a) : (b))
#define imin(a,b) (((a) < (b)) ? (a) : (b))

//#include "depug.h"

double logistic(double activation);
// will be changed to values much higher than these
const int IROWS = 64; //Number of rows (length of first pattern)
const int ICOLS = 64; //Number of colums (length of second pattern)
const int MAXIVEC = 64; //Default size of ivector


class matrix;
class CIoPropertyManager;

using std::istream;
using std::ostream;

class ivec  
{
		friend class CIoPropertyManager;
	    friend ostream& operator<< (ostream& s, ivec& v1);
        friend class matrix;
        friend istream& operator>> (istream& s, ivec& v1);
		int n;

public:

		int *v;

	int dot(ivec &v1);
	ivec  cross(ivec &v1);
	ivec(int size=MAXIVEC, int val=0); //constructor
	virtual ~ivec();

		ivec(ivec &v1); //copy-initializer
		int length();
		float magnitude();

		ivec& scale(ivec& iminivec, ivec& imaxivec);
		int d_logistic(); //return ivec * (1-ivec)

		// dot product of ivector and complement
		int imaxval();
		void add( ivec *v1, int c=1);
		void multiply( int c);
		ivec& garble(float noise);
		ivec& operator=(const ivec& v1); //ivector assignment
		ivec operator+(const ivec& v1); //ivector addition
		ivec operator+(const int d);
		//ivector additive-assignment
		ivec& operator+=(const ivec& v1);
		ivec& operator-=(const ivec& v1);

		//ivector multiply by constant
		ivec& operator*=(int c);
		// ivector transpose multiply needs access to v array
		int operator==(const ivec& v1);
		int operator[](int x);
		int ivec::imaxindex();
		ivec& getstr(char *s);
		void putstr(char *s);
		int get(int x);
		
		ivec operator-(const ivec& v1);	//subtraction
		ivec operator-(const int d);   //subtraction
		int operator*(const ivec& v1); //dot-product
		ivec operator*(int c); //multiply by constant
		int operator !=(const  ivec v1);
		ivec& set(int i, int f=0);
		ivec& setivec(int size, int *pf);
		int load (FILE *f);
		int save (FILE *f);
	    ostream& write(ostream& s, ivec& v1);
		istream& read(istream& s, ivec& v1);

		int inivec(int i); //check if the integer is in the vector
		//Converts a space separated list of numbers
		//to a ivector
		void stringtoivec(char *s);
		void ivectostr(char *vstring);
		int ivecstrlen();
		void ivectocomsepstr(char *vstring, int skip);
}; //ivector class

#endif


