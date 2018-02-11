// ivec.cpp: implementation of the ivec class.
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
	

#include "ivec.h"



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////
// ivector class member functions

ivec::ivec(int size, int val)
{
	int i;
	v = new int[n=size];
	for(i=0; i<n; i++)
		v[i] = val;

} //constructor

ivec::~ivec() {delete [] v; } //delete [] v;} //destructor

ivec::ivec(ivec &v1) //copy-initializer
{
	int i;
	v=new int[n=v1.n];
	for(i=0; i<n; i++)
		v[i]=v1.v[i];
}

ivec& ivec::operator=(const ivec& v1)
{
	int i;
	delete v;
	v=new int[n=v1.n];
	for(i=0; i<n; i++)
		v[i]=v1.v[i];
	return *this;
}

ivec ivec::operator+(const ivec& v1)
{
	int i;
	ivec sum(v1.n);
	for (i=0; i<v1.n; i++)
		sum.v[i]=v1.v[i]+v[i];
	return sum;
}

ivec& ivec::operator+=(const ivec& v1)
{
	int i;
	for(i=0; i<v1.n; i++)
		v[i]+=v1.v[i];
	return *this;
}

ivec& ivec::operator-=(const ivec& v1)
{
	int i;
	for(i=0; i<v1.n; i++)
		v[i]-=v1.v[i];
	return *this;
}

int ivec::operator*(const ivec& v1) //dot-product
{
	int i;
	int sum=0;
	for(i=0; i<imin(n,v1.n);i++)
		sum+=(v1.v[i]*v[i]);
	//D(cout << "dot product " << *this << v1 << sum << "\n";)
	return sum;
}

int ivec::operator==(const ivec& v1)
{
	int i;
	if(v1.n!=n) return 0;
	for(i=0; i<imin(n,v1.n); i++)
	{
		if(v1.v[i]!=v[i])
			return 0;

		return 1;
	}
}

float ivec::magnitude()
{
	float sum=0;
	for(int i=0; i<n; i++)
		sum+=( v[i]*v[i]);

	float mag=sqrt(sum);
	return mag;
}

int ivec::get(int x)
{
if(x<length() && x>=0)
		return v[x];
	else
		return 9.9e99;

	return 0;
}

int ivec::operator[](int x)
{
	if(x<length() && x>=0)
		return v[x];
	else
		return 9.9e99;

	return 0;
}

int ivec::length(){return n;} //length method

ivec& ivec::garble(float noise) //corrupt ivector w/random noise
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





int ivec::imaxval() // returns imaximum absolute value
{
	int i;
	int mx=0;
	for(i=0; i<n; i++)
		if(fabs(v[i])>mx)
			mx=fabs(v[i]);

	return mx;
}

ivec& ivec::scale(ivec& iminivec, ivec& imaxivec)
{
	int i;
	for(i=0; i<n; i++)
	{
		if(v[i]<iminivec.v[i])
			v[i]=0;
		else if(v[i]>imaxivec.v[i])
			v[i]=1;
		else if((imaxivec.v[i]-iminivec.v[i])==0)
			v[i]=1;
		else
			v[i] = (v[i]-iminivec.v[i])/(imaxivec.v[i]-iminivec.v[i]);
	}
	
	return *this;
}

int ivec::d_logistic() //return ivec * (1-ivec)
{
	int i;
	int sum=0.0;
	for(i=0; i,n; i++)
		sum+=(v[i]*(1-v[i]));
	return sum;
}



//index of the highest item in ivector
int ivec::imaxindex()
{
	int idx, i;
	int mx;
	for(i=0, mx=-FLT_MAX;i<n;i++)
		if(v[i]>mx)
		{
			mx=v[i];
			idx=i;
		}

	return idx;
}




ivec& ivec::getstr(char *s)
{
	int i;
	for(i=0; i<MAXIVEC&&s[i]; i++)
	{
		if(isalpha(s[i]))
			v[toupper(s[i])-'A']=1;
	}

	return *this;
}

void ivec::putstr(char *s)
{
	int i;
	int ct=0;
	for(i=0; i<n; i++)
		if(v[i]>0.9)
			s[ct++]='A'+i;
}

ivec ivec::operator-(const ivec& v1)
{
	int i;
	
	for(i=0; i<n; i++)
		v[i]=v[i]-v1.v[i];
	return *this;
}

ivec ivec::operator-(const int d)// subtraction of constant
{
	int i;
	ivec diff(n);
	for(i=0; i<n; i++)
		diff.v[i]=v[i]-d;
	return diff;
}

ivec ivec::operator*(int c)
{
	int i;
	
	for(i=0;i<n;i++)
		v[i]=v[i]*c;
	return *this;
}

ivec& ivec::operator*=(int c)
{
	int i;
	for(i=0; i<n; i++)
		v[i]*=c;
	return *this;

} //ivector multiply by constant



ivec& ivec::set(int i, int f)
{
  if(i<n)
	v[i]=f;
	return *this;
}

ivec& ivec::setivec(int size, int *pf)
{
	int i;
	if(size>0 && (pf != NULL))
	{
		delete [] v;
		v = new int[n=size];
		for(i=0; i<size; i++)
		{
				if(i<n)  v[i] = pf[i];
		}
	}

	return *this;
}

istream& ivec::read(istream& s, ivec &v1)
// format: list of inting point numbers  followed by ','
{

//WARNING CONFUSING LOOP STRUCTURE HERE
	int d;
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


istream& operator>>(istream& s, ivec &v1)
// format: list of inting point numbers  followed by ','
{

//WARNING CONFUSING LOOP STRUCTURE HERE
	int d;
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

ostream& operator<<(ostream& s, ivec& v1)
// format: list of inting-point numbers followed by ','
{
	int i;
	s.precision(2);
	for(i=0; i<v1.n; i++)
		s<<v1[i] <<" ";
	s << ",";
	return s;
}

ostream& ivec::write(ostream& s, ivec& v1)
// format: list of inting-point numbers followed by ','
{
	int i;
	s.precision(2);
	for(i=0; i<v1.n; i++)
		s<<v1[i] <<" ";
	s << ",";
	return s;
}

int ivec::save(FILE *f) //save binary values of matrix from specified file
{
	int i;
	int success=1;
	for(i=0; i<n; i++)
		if(fwrite(&(v[i]),sizeof(v[i]),1,f)<1)
			success=0;
	return success;
}

int ivec::load(FILE *f) //load binary values of ivector from specified file
{
	int i;
	int success=1;
	for(i=0; i<n; i++)
		if(fread(&(v[i]),sizeof(v[0]),1,f)<1)
			success=0;
	return success;

}

int ivec::operator !=(const  ivec v1)
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

ivec ivec::cross(ivec &v1)
{
	//calculate cross product for three
	//ivector

	int fv0, fv1, fv2;
	ivec prod(length());

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

void ivec::add( ivec *v1, int c)
{
	int i;
    for(i=0; i<n; i++)
		if(v1 && i<v1->length())
					v[i] += (c*(v1->get(i)));


}

void ivec::multiply( int c)
{
	int i;
    for(i=0; i<n; i++)
		    v[i] *= c;

}






int ivec::dot(ivec &v1)
{
	int i;
	int dp=0;

	if(n != v1.n)
			return 1;
	for(i=0; i<n; i++)
		dp+=v[i]*v1.v[i];

	return dp;

}



void ivec::stringtoivec(char *s)
{
			int fval;
			int i, iivecel=0;
			char *tokenptr;
			char *stopstr;
			char seps[] = " ,\t\n";

			if(s && strlen(s)>0)
			{
				tokenptr = strtok(s, seps);
				while(tokenptr !=NULL)
				{			
					tokenptr = strtok(NULL, seps);
					iivecel++;
				}
			
			//Initialise the vector and set to zero
			if(iivecel>0)
			{
				delete [] v;
				n=iivecel;
				v = new int[iivecel];
				for(i=0; i<iivecel; i++)
					v[i]=0;
			}

			iivecel=0;
				tokenptr = strtok(s, seps);
				while(tokenptr !=NULL)
				{			
					//fval = (int)atof(tokenptr, &stopstr);
					fval = (int)atof(tokenptr);
					if(iivecel<n)
							v[iivecel]=fval;
					tokenptr = strtok(NULL, seps);
					iivecel++;
				}
			}

}

//check if the integer is in the vector
int ivec::inivec(int i)
{
	int ifound=0;
	for(int j=0; j<n; j++)
		if(v[j]==i)
		{
			ifound=1;
			break;
		}
	return ifound;
}

void ivec::ivectostr(char *vstring)
{
	char strivec [10];
	int length=0;
	int i;
	if(vstring)
	{
		vstring[0]='\0';
		for(i=0; i<n; i++)
		{
			if(i==(n-1))
				sprintf(strivec, "%f ", v[i]);
			else
				sprintf(strivec, "%f", v[i]);

			strcat(vstring, strivec);
			strivec[0]='\0';
		}
	}

}

void ivec::ivectocomsepstr(char *vstring, int skip)
{
	int i;
	char strivec [10];
	int length=0;
	if(vstring)
	{
		vstring[0]='\0';
		for(i=0; i<n; i++)
		{
			if(i==(n-1))
				sprintf(strivec, "%f ", v[i]);
			else if((i>0) && ((i%(skip-1))==0))
				sprintf(strivec, "%f, ", v[i]);
			else
				sprintf(strivec, "%f", v[i]);

			strcat(vstring, strivec);
			strivec[0]='\0';
		}
	}

}



int ivec::ivecstrlen()
{
	int i;
	char strivec [10];
	strivec[0]='\0';
	int length=0;
	for(i=0; i<n; i++)
	{
		sprintf(strivec, "%f", v[i]);
		length +=(strlen(strivec));
		strivec[0]='\0';
	}
	return (length+n-1);
}






