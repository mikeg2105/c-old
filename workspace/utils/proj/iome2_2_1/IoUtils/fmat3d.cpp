// fmat3d.cpp: implementation of the fmat3d class.
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
	
#include "fmat3d.h"



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////
//matrix3d member functions
matrix3d::matrix3d(int n,int p, int q, float range)
{
	int i,j,k, rnd;
	time_t t;
	int pct, val;

	m=new float **[n];
	if (range)
	{
		  time(&t);
		  srand((unsigned)t);
	}
	for(i=0;i<n;i++)
	{
		m[i]=new float *[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float [q];
			for(k=0; k<q; k++)
			{
				if(range)
				{
					rnd=rand();
					pct=(int) (range * 100.0);
					val =rnd % pct;
					m[i] [j] [k]= (float) val/100.0;
					if(range<0)
						m[i][j][k] = fabs(range) - (m[i][j][k] * 2.0);
				}
				else
					m[i][j][k] = 0;
			}
		}

	}
	r=n;
	c=p;
	d=q;
	
}

matrix3d::matrix3d(int n, int p, int d, float value, float range)
{
	int i,j;
	m=new float *[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float[p];
		for(j=0;j<p;j++)
			m[i][j]=value;
	}
	r=n;
	c=p;
}



matrix3d::matrix3d(int n, int p, int d, char *fn)
{
	int i,j,rnd;
	time_t t;
	m= new float *[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float[p];
	}
	r=n;
	c=p;
	//ifstream in(fn, ios::in);
	//in >> *this;
}

/*matrix3d::matrix3d(const vecpair  &vp)
{
	r=vp.a->length();
	c=vp.b->length();
	m=new float *[r];
	for(int i=0; i<r; i++)
	{
		m[i] = new float[c];
		for(int j=0; j<c; j++)
			m[i][j]=vp.a->v[i]*vp.b->v[j];
	}
}//constructor*/

matrix3d::matrix3d(matrix3d &m1) //copy-initialiser
{
	int i,j;
	//D(cout << "matrix3d copy initialiser\n";)
	r=m1.r;
	c=m1.c;
	m=new float *[r];
	for(i=0; i<r; i++)
	{
		m[i]=new float[c];
		for(j=0; j<c; j++)
			m[i][j] = m1.m[i][j];
	}
}

void matrix3d::resize(int nr, int nc) 
{
	int i,j;
	float value=0;
	for(i=0;i<r;i++)
		delete [] m[i];
	delete [] m;
	
	m=new float *[nr];

	for(i=0;i<nr;i++)
	{
		m[i]=new float[nc];
		for(j=0;j<nc;j++)
			m[i][j]=value;
	}
	r=nr;
	c=nc;

}

matrix3d::~matrix3d()
{
	int i;
	for(i=0;i<r;i++)
		delete [] m[i];
	delete [] m;
}//destructor

matrix3d& matrix3d::operator=(const matrix3d& m1)
{
	int i,j;
	for(i=0; i<r; i++)
		delete m[i];
	r=m1.r;
	c=m1.c;
	m=new float *[r];
	for(i=0;i<r;i++)
	{
		m[i]=new float [c];
		for(j=0; j<r; j++)
			m[i][j]=m1.m[i][j];
	}
	return *this;
}

matrix3d& matrix3d::operator+(const matrix3d& m1)
{
	int i,j;
	matrix3d msum(r,c);
	for(i=0;i<r;i++)
		for(j=0;j<r;j++)
			msum.m[i][j]=m1.m[i][j]+m[i][j];
	return *this;
}


matrix3d& matrix3d::operator*(const float d)
{
	int i,j;
	for(i=0;i<r;i++)
		for(j=0;j<c;j++)
			m[i][j] *=d;
	return *this;
}

vec *matrix3d::colslice(int col)
{
	int i;
	vec *temp = new vec(r);

	for(i=0; i<r; i++)
		temp->v[i]=m[i][col];
	return temp;
}

void matrix3d::colslice(int col, vec *v)
{
    int i;
	if(v && (v->length()<=r))
		for(i=0; i<r; i++)
			v->set(i,m[i][col]);

}

vec *matrix3d::rowslice(int row)
{
    int i;
	vec *temp = new vec(c);
	for(i=0; i<c; i++)
		temp->v[i]=m[row][i];
	return temp;
}

void matrix3d::rowslice(int row, vec *v)
{
	int i;
	if(v && (v->length()<=c))
		for(i=0; i<c; i++)
				v->set(i, m[row][i]);
			

}

void matrix3d::insertcol(vec &v, int col)
{
	int i;
	for(i=0; i<v.n; i++)
		m[i][col]=v.v[i];
}

void matrix3d::insertrow(vec &v, int row)
{
    int i;
	for(i=0; i<v.n; i++)
		m[row][i]=v.v[i];
}

int matrix3d::depth(){return r;}
int matrix3d::width(){return c;}

float matrix3d::getval(int row, int col)
{
  if(row<r && col<c)
	return m[row][col];
  else
	 return 0;
}

void matrix3d::setval(int row, int col, float val)
{

	if(row<r && col<c)
		m[row][col] = val;
}

int matrix3d::closestcol(vec &v)
{
	int i,mincol;
	float d;
	float mindist = INT_MAX;
	vec w(r);
	for(i=0; i<c;i++)
	{
		w= *colslice(i);
		if((d=v.distance(w))<mindist)
		{
			mindist=d;
			mincol=i;
		}
	}

	return mincol;

}

int matrix3d::closestrow(vec  &v)
{
	int i, minrow;
	float d;
	float mindist=INT_MAX;
	vec w(c);
	for(i=0; i<r; i++)
	{
		w= *rowslice(i);
		if((d=v.distance(w))<mindist)
		{
			mindist=d;
			minrow=i;
		}
	}
	return minrow;
}

int matrix3d::closestrow(vec &v, int *wins, float scaling)
{
	int i, minrow;
	float d;
	float mindist=INT_MAX;
	vec w(c);
	for(i=0; i<r; i++)
	{
		w= *rowslice(i);
		d=v.distance(w);
		d*=(1+((float)wins[i]*scaling));
		if(d < mindist)
		{
			mindist=d;
			minrow=i;
		}
	}
	return minrow;
}


// save binary values of matrix3d to specified raw file
int matrix3d::save(FILE *f)
{

	int i, j, success=1;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			if(fwrite( &(m[i][j]), sizeof(m[0][0]),1,f)<1)
								success=0;
	return success;
}


//load binary values of matrix3d from specified raw file
int matrix3d::load(FILE *f)
{
	int i, j, success=1;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			if (fread(&(m[i][j]), sizeof(m[0][0]),1,f)<0)
				success = 0;
	return success;
}

matrix3d& matrix3d::operator+=(const matrix3d &m1)
{
	int i,j;
	for(i=0; i<r&&i<m1.r; i++)
		for(j=0; j<c&&j<m1.c; j++)
			m[i][j] += (m1.m[i][j]);
	return *this;
		
}

matrix3d& matrix3d::operator*=(const float d)
{
	int i,j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]*=d;
	return *this;
}


vec matrix3d::operator*(vec &v1)
{
	int i;
	vec temp(v1.n==r?c:r),temp2(v1.n==r?r:c);
	for(i=0; i<((v1.n==r)?c:r); i++)
	{
		if(v1.n==r)
			temp2= *colslice(i);
		else
			temp2= *rowslice(i);
		temp.v[i]=v1*temp2;
	}
	return temp;	
}

void matrix3d::initvals(const vec& v1, const vec& v2,
		      const float rate, const float momentum)
{
	int i, j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]=(m[i][j]*momentum)+((v1.v[i]*v2.v[j])*rate);
}

ostream& operator<<(ostream& s, matrix3d& m1)
//print a matrix3d
{
	int i, j;
	s<< m1.r << " " << m1.c << " ";
	if((m1.r*m1.c)>2500)
		s<<"\n";

	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			s << m1.m[i][j] << " ";
		}
		if((m1.r*m1.c)>2500)
				s << "\n";
	}
	return s;
}

istream& operator>>(istream& s, matrix3d& m1)
{
	int i, j;
	int newr, newc;
	s>>newr;
	s>>newc;
	m1.resize(newr, newc);
	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			s>>m1.m[i][j];
		}
	}
	return s;
}
