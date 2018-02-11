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

matrix3d::matrix3d(int n, int p, int q, float value, float range)
{
	int i,j,k;
	m=new float **[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float *[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float [q];
			for(k=0; k<q; k++)
			{
				m[i][j][k]=value;
			}
		}
	}
	r=n;
	c=p;
	d=q;
}



matrix3d::matrix3d(int n, int p, int q, char *fn)
{
	time_t t;

	int i,j,k;
	m=new float **[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float *[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float [q];

		}
	}
	r=n;
	c=p;
	d=q;	
	
	
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
	int i,j,k;
	//D(cout << "matrix3d copy initialiser\n";)
	r=m1.r;
	c=m1.c;
	d=m1.d;
	m=new float **[r];
	for(i=0; i<r; i++)
	{
		m[i]=new float*[c];
		for(j=0; j<c; j++)
		{
			m[i][j]=new float[d];
			for(k=0; k<d; k++)	
				m[i][j][k] = m1.m[i][j][k];
		}
	}
}

void matrix3d::resize(int nr, int nc, int nd) 
{
	int i,j,k;
	float value=0;
	
	
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
		delete [] m[i][j];
		
	delete [] m;
	
	m=new float **[n];
	for(i=0;i<nr;i++)
	{
		m[i]=new float *[p];
		for(j=0;j<nc;j++)
		{
			m[i][j]=new float [q];
			for(k=0; k<nd; k++)
			  m[i][j][k]=value;

		}
	}
	r=nr;
	c=nc;
	d=nd;

}

matrix3d::~matrix3d()
{
	int i,j;
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
		delete [] m[i][j];
		
	delete [] m;
}//destructor

matrix3d& matrix3d::operator=(const matrix3d& m1)
{
	int i,j;
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
		delete [] m[i][j];
		
	delete [] m;
	
	r=m1.r;
	c=m1.c;
	d=m1.d;
	
	m=new float **[n];
	for(i=0;i<r;i++)
	{
		m[i]=new float *[p];
		for(j=0;j<c;j++)
		{
			m[i][j]=new float [q];
			for(k=0; k<d; k++)
			  m[i][j][k]=m1.m[i][j][k];

		}
	}
	return *this;
}

matrix3d& matrix3d::operator+(const matrix3d& m1)
{
	int i,j,k;
	matrix3d msum(r,c,d);
	for(i=0;i<r;i++)
		for(j=0;j<r;j++)
			for(k=0;k<d;k++)
				msum.m[i][j][k]=m1.m[i][j][k]+m[i][j][k];
	return msum;
}


matrix3d& matrix3d::operator*(const float d)
{
	int i,j,k;
	for(i=0;i<r;i++)
		for(j=0;j<c;j++)
			for(k=0;k<d;k++)
				m[i][j][k] *=d;
	return *this;
}

matrix *matrix3d::colslice(int col)
{
	int i;
	matrix *temp = new matrix(r,d,0);

	for(i=0; i<r; i++)
		for(j=0;j<d; j++)
			temp->m[i][j]=m[i][col][j];
	return temp;
}

void matrix3d::colslice(int col, matrix *v)
{
    int i,j;
	if(v && (v->rows()<=r) && (v->cols()<=d))
		for(i=0; i<r; i++)
			for(j=0; j<d; j++)
				v->set(i,j,m[i][col][j]);

}

matrix *matrix3d::rowslice(int row)
{
    int i,j;
	matrix *temp = new matrix(c,d,0);
	for(i=0; i<c; i++)
		for(j=0; j<d; j++)
		temp->v[i][j]=m[row][i][j];
	return temp;
}

void matrix3d::rowslice(int row, matrix *v)
{
	int i,j;
	if(v && (v->rows()<=c) && (v->cols()<=d))
		for(i=0; i<c; i++)
		  for(j=0; j<d; j++)
				v->set(i,j, m[row][i][j]);
			

}

void matrix3d::depslice(int dep, matrix *v)
{
	int i,r;
	if(v && (v->rows()<=r)&& (v->cols()<=c))
		for(i=0; i<r; i++)
			for(j=0;j<c;j++)
				v->set(i,j, m[i][j][dep]);
			

}

void matrix3d::insertcol(matrix &v, int col)
{
	int i,j;
	for(i=0; i<v.r; i++)
		for(j=0;j<v.c;j++)
			m[i][col][j]=v.v[i][j];
}

void matrix3d::insertrow(matrix &v, int row)
{
	int i,j;
	for(i=0; i<v.r; i++)
		for(j=0;j<v.c;j++)
			m[row][i][j]=v.v[i][j];		
		
}

void matrix3d::insertdep(matrix &v, int dep)
{
	int i,j;
	for(i=0; i<v.r; i++)
		for(j=0;j<v.c;j++)
			m[i][j][dep]=v.v[i][j];			
}

int matrix3d::rows(){return r;}
int matrix3d::cols(){return c;}
int matrix3d::height(){return d;}

float matrix3d::getval(int row, int col, int dep)
{
  if(row<r && col<c && dep<d)
	return m[row][col][dep];
  else
	 return 0;
}

void matrix3d::setval(int row, int col, int dep, float val)
{

	if(row<r && col<c && dep<d)
		m[row][col][dep] = val;
}

/*int matrix3d::closestcol(vec &v)
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

}*/

/*int matrix3d::closestrow(vec  &v)
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
}*/

/*int matrix3d::closestrow(vec &v, int *wins, float scaling)
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
}*/


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
	int i,j,k;
	for(i=0; i<r,i<m1.r; i++)
		for(j=0; j<c,j<m1.c; j++)
				for(k=0; k<d,k<m1.d j<m1.c; j++)
			m[i][j][k] += (m1.m[i][j][k]);
	return *this;
}

matrix3d& matrix3d::operator*=(const float d)
{
	int i,j,k;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			for(k=0; k<d; k++)
				m[i][j][k]*=d;
	return *this;
}


/*vec matrix3d::operator*(vec &v1)
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
}*/

/*void matrix3d::initvals(const vec& v1, const vec& v2,
		      const float rate, const float momentum)
{
	int i, j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]=(m[i][j]*momentum)+((v1.v[i]*v2.v[j])*rate);
}*/

ostream& operator<<(ostream& s, matrix3d& m1)
//print a matrix3d
{
	int i, j,k;
	s<< m1.r << " " << m1.c << " " << m1.d << " ";
	if((m1.r*m1.c*m1.d)>2500)
		s<<"\n";

	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			for(k=0;k<m1.d; k++)
				s << m1.m[i][j][k] << " ";
				
			if((m1.r*m1.c*m1.d)>2500)
				s << "\n";
								
		}

	}
	return s;
}

istream& operator>>(istream& s, matrix3d& m1)
{
	int i, j,k;
	int newr, newc,newd;
	s>>newr;
	s>>newc;
	s>>newd;
	m1.resize(newr, newc,newd);
	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			for(k=0;k<m1.d;k++)
				s>>m1.m[i][j][k];
		}
	}
	return s;
}
