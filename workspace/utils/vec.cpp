// vec.cpp: implementation of the vec class.
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
	

#include "vec.h"



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////
// vector class member functions

vec::vec(int size, float val)
{
	int i;
	v = new float[n=size];
	for(i=0; i<n; i++)
		v[i] = val;

} //constructor

vec::~vec() {delete [] v; } //destructor

vec::vec(vec &v1) //copy-initializer
{

}

void vec::resize(int size, float val)
{
	int i;
	delete [] v;
	v=new float[n=size];
	for(i=0; i<n; i++)
		v[i]=val;	
}

vec& vec::operator=(const vec& v1)
{
	int i;
	//delete [] v;
	//v=new float[n=v1.n];
	for(i=0; i<n, i<v1.n; i++)
		v[i]=v1.v[i];
	return *this;
}

vec& vec::operator+(const vec& v1)
{
	int i;
	vec sum(n,0);
	for (i=0; i<v1.n; i++)
		sum.v[i]=v1.v[i]+v[i];
	return sum;
}

vec& vec::operator+=(const vec& v1)
{
	int i;
	for(i=0; i<v1.n; i++)
		v[i]+=v1.v[i];
	return *this;
}

vec& vec::operator-=(const vec& v1)
{
	int i;
	for(i=0; i<v1.n; i++)
		v[i]-=v1.v[i];
	return *this;
}

float vec::operator*(const vec& v1) //dot-product
{
	int i;
	float sum=0;
	for(i=0; i<min(n,v1.n);i++)
		sum+=(v1.v[i]*v[i]);
	//D(cout << "dot product " << *this << v1 << sum << "\n";)
	return sum;
}

int vec::operator==(const vec& v1)
{
	int i;
	if(v1.n!=n) return 0;
	for(i=0; i<min(n,v1.n); i++)
	{
		if(v1.v[i]!=v[i])
			return 0;

		return 1;
	}
}

float vec::magnitude()
{
	float sum=0;
	for(int i=0; i<n; i++)
		sum+= ( v[i]*v[i]);

	float mag=sqrt(sum);
	return mag;
}

float vec::get(int x)
{
if(x<length() && x>=0)
		return v[x];
	else
		return 9.9e99;

	return 0;
}

float vec::operator[](int x)
{
	if(x<length() && x>=0)
		return v[x];
	else
		return 9.9e99;

	return 0;
}

int vec::length(){return n;} //length method

vec& vec::garble(float noise) //corrupt vector w/random noise
{
	int i;
	time_t t;
	time(&t);
	srand((unsigned)t);
	for(i=0; i<n; i++)
	{
		if((rand()%10)/10<noise)
			v[i]=1-v[i];

	}

	return *this;
}

vec& vec::randomize(float range) //corrupt vector w/random noise
{
	int i;
	time_t t;
	time(&t);
	float fval;
	srand((unsigned)t);
	for(i=0; i<n; i++)
	{
		fval=(float)((rand()%1000)/1000);
		v[i]=fval*range;
	}

	return *this;
}

vec& vec::normalise() //normalise by length
{
	int i;
	float fmod=0;
	for(i=0; i<n; i++)
		fmod+=(v[i]*v[i]);

	if(fmod=0) fmod=1;

	for(i=0; i<n; i++)
		v[i]/=fmod;

	return *this;
}

vec& vec::normaliseon() //normalise by non zero elements
{
	
	int i, on=0;
	for(i=0; i<n; i++)
		if(v[i])
			on++;
	for(i=0; i<n; i++)
		v[i]/=on;
	return *this;
}

float vec::maxval() // returns maximum absolute value
{
	int i;
	float mx=0;
	for(i=0; i<n; i++)
		if(fabs(v[i])>mx)
			mx=fabs(v[i]);

	return mx;
}

vec& vec::scale(vec& minvec, vec& maxvec)
{
	int i;
	for(i=0; i<n; i++)
	{
		if(v[i]<minvec.v[i])
			v[i]=0;
		else if(v[i]>maxvec.v[i])
			v[i]=1;
		else if((maxvec.v[i]-minvec.v[i])==0)
			v[i]=1;
		else
			v[i] = (v[i]-minvec.v[i])/(maxvec.v[i]-minvec.v[i]);
	}
	
	return *this;
}

float vec::d_logistic() //return vec * (1-vec)
{
	int i;
	float sum=0.0;
	for(i=0; i,n; i++)
		sum+=(v[i]*(1-v[i]));
	return sum;
}

//Euclidean distance function ||A-B||
float vec::distance(vec& A)
{
	int i;
	float sum=0, d;
	for(i=0; i<n; i++)
	{
		d=v[i]-A.v[i];
		if(d)sum+=pow(d,2);
	}
	return sum?pow(sum,0.5):0;
}

//index of the highest item in vector
int vec::maxindex()
{
	int idx, i;
	float mx;
	for(i=0, mx=-FLT_MAX;i<n;i++)
		if(v[i]>mx)
		{
			mx=v[i];
			idx=i;
		}

	return idx;
}

double logistic(double activation)
{

//* These underflow limits were copied from Mc Clellands bp
//implementation. Problems with underflow with numbers that
//should have been small enough in magnitude. McClelland seems
// to have encountered this and established the numbers below as reasonable
//limits. */

	if(activation>11.5129)
		return 0.99999;
	if(activation<-11.5129)
		return 0.00001;
	return 1.0/(1.0+exp(-activation));

}


vec& vec::getstr(char *s)
{
	int i;
	for(i=0; i<MAXVEC&&s[i]; i++)
	{
		if(isalpha(s[i]))
			v[toupper(s[i])-'A']=1;
	}

	return *this;
}

void vec::putstr(char *s)
{
	int i;
	int ct=0;
	for(i=0; i<26; i++)
		if(v[i]>0.9)
			s[ct++]='A'+i;
}

vec vec::operator-(const vec& v1)
{
	int i;
	vec dif(n,0);
	for(i=0; i<n; i++)
		dif.v[i]=v[i]-v1.v[i];
	return dif;
}

vec vec::operator-(const float d)// subtraction of constant
{
	int i;
	
	for(i=0; i<n; i++)
		v[i]=v[i]-d;
	return *this;
}

vec vec::operator*(float c)
{
	int i;
	vec prod(n,0);
	for(i=0;i<n;i++)
		prod.v[i]=v[i]*c;
	return prod;
}

vec& vec::operator*=(float c)
{
	int i;
	for(i=0; i<n; i++)
		v[i]*=c;
	return *this;

} //vector multiply by constant

vec& vec::sigmoid()
{
	int i;
	for(i=0; i<n; i++)
		v[i]=(float)logistic((double)v[i]);
	return *this;
}

vec& vec::sigmoid(vec &thresh)
{
	int i;
	for(i=0; i<n; i++)
	 if(v[i]>=thresh[i])
		v[i]=(float)logistic((double)v[i]);
	return *this;
}


vec& vec::set(int i, float f)
{
  if(i<n)
	v[i]=f;
	return *this;
}

vec& vec::setvec(int size, float *pf)
{
	int i;
	if(size>0)
	{
		delete [] v;
		v = new float[n=size];
		for(i=0; i<size; i++)
		{
			if(pf != NULL)
				v[i] = pf[i];
			else
				v[i] = 0;
		}
	}

	return *this;
}

istream& vec::read(istream& s, vec &v1)
// format: list of floating point numbers  followed by ','
{

//WARNING CONFUSING LOOP STRUCTURE HERE
	float d;
	int i=0,c;
	for(;;)
	{
		s>>d;
		if(s.eof())
			return s;
		if(s.fail())
		{
			s.clear();
			do
				c=s.get();
		
		while(c!=',');
		return s;
	}

	v1.v[i++]=d;
	if(i==v1.n)
	{
		do
			c=s.get();
		while(c!=',');
		return s;
	}
	}
}


istream& operator>>(istream& s, vec &v1)
// format: list of floating point numbers  followed by ','
{

//WARNING CONFUSING LOOP STRUCTURE HERE
	float d;
	int i=0,c;
	for(;;)
	{
		s>>d;
		if(s.eof())
			return s;
		if(s.fail())
		{
			s.clear();
			do
				c=s.get();
		
		while(c!=',');
		return s;
	}

	v1.v[i++]=d;
	if(i==v1.n)
	{
		do
			c=s.get();
		while(c!=',');
		return s;
	}
	}
}

ostream& operator<<(ostream& s, vec& v1)
// format: list of floating-point numbers followed by ','
{
	int i;
	s.precision(2);
	for(i=0; i<v1.n; i++)
		s<<v1[i] <<" ";
	s << ",";
	return s;
}

ostream& vec::write(ostream& s, vec& v1)
// format: list of floating-point numbers followed by ','
{
	int i;
	s.precision(2);
	for(i=0; i<v1.n; i++)
		s<<v1[i] <<" ";
	s << ",";
	return s;
}

int vec::save(FILE *f) //save binary values of matrix from specified file
{
	int i;
	int success=1;
	for(i=0; i<n; i++)
		if(fwrite(&(v[i]),sizeof(v[i]),1,f)<1)
			success=0;
	return success;
}

int vec::load(FILE *f) //load binary values of vector from specified file
{
	int i;
	int success=1;
	for(i=0; i<n; i++)
		if(fread(&(v[i]),sizeof(v[0]),1,f)<1)
			success=0;
	return success;

}

int vec::operator !=(const  vec v1)
{
	int i;
	if(v1.n != n)
		return 1;
	
	for(i=0; i<n; i++)
	{
		if(v[i] != v1.v[i])
			return 1;

		return 0;
	}
}

vec vec::cross(vec &v1)
{
	//calculate cross product for three
	//vector

	float fv0, fv1, fv2;
	vec prod(length());

	if(n<3 || v1.n < 3)
						return *this;

	fv0 = v[1]*v1.v[2]-v[2] * v1.v[1];
	fv1 = -v[0]*v1.v[2]+v[2] * v1.v[0];
	fv2 = v[0]*v1.v[1]-v[1] * v1.v[0];

	prod.v[0] = fv0;
	prod.v[1] = fv1;
	prod.v[2] = fv2;

	return prod;
}

void vec::add( vec *v1, float c)
{
	int i;
    for(i=0; i<n; i++)
		if(v1 && i<v1->length())
					v[i] += (c*(v1->get(i)));


}

void vec::multiply( float c)
{
	int i;
    for(i=0; i<n; i++)
		    v[i] *= c;

}






float vec::dot(vec &v1)
{
	int i;
	float dp=0;

	if(n != v1.n)
			return 1;
	for(i=0; i<n; i++)
		dp+=v[i]*v1.v[i];

	return dp;

}

void vec::unit()
{
	int i;
	float dp;
	//turns this vector into a unit vector
	dp = 0;
	for(i=0; i<n; i++)
		dp+=v[i]*v[i];

	if(dp>0)
		dp = sqrt(dp);
	else
		dp = 1;

	for(i=0; i<n; i++)
		v[i] /= dp;


}

void vec::stringtovec(char *s)
{
			float fval;
			int ivecel=0;
			char *tokenptr;
			char *stopstr;
			char seps[] = " ,\t\n";
			if(s && strlen(s)>0)
			{
				tokenptr = strtok(s, seps);
				while(tokenptr !=NULL)
				{			
					//fval = (float)atof(tokenptr, &stopstr);
					fval = (float)atof(tokenptr);
					if(ivecel<n)
							v[ivecel]=fval;
					tokenptr = strtok(NULL, seps);
					ivecel++;
				}
			}

}

void vec::vectostr(char *vstring)
{
	int i;
	char strvec [10];
	int length=0;
	if(vstring)
	{
		vstring[0]='\0';
		for(i=0; i<n; i++)
		{
			if(i==(n-1))
				sprintf(strvec, "%f ", v[i]);
			else
				sprintf(strvec, "%f", v[i]);

			strcat(vstring, strvec);
			strvec[0]='\0';
		}
	}

}

int vec::vecstrlen()
{
	int i;
	char strvec [10];
	strvec[0]='\0';
	int length=0;
	for(i=0; i<n; i++)
	{
		sprintf(strvec, "%f", v[i]);
		length +=(strlen(strvec));
		strvec[0]='\0';
	}
	return (length+n-1);
}





