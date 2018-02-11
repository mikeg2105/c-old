// fmat3d.cpp: implementation of the mmat3d class.
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
	
#include "mmat3d.h"



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////
//mmat3d member functions
mmat3d::mmat3d(int n, int p, int q, int mn, int mm, float range )
{
	int i,j,k, rnd;
	int nr,nc;
	time_t t;
	int pct, val;

	if (range)
	{
		  time(&t);
		  srand((unsigned)t);
	}

	m=new float ****[n];

	for(i=0;i<n;i++)
	{
		m[i]=new float ***[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float **[q];
			for(k=0; k<q; k++)
			{
				m[i][j][k]=new float *[mn];
				for(nr=0; nr<mn; nr++)
				{
					m[i][j][k][nr]=new float  [mm];
					for(nc=0; nc<mm; nc++)
					{					

						if(range)
						{
							rnd=rand();
							pct=(int) (range * 100.0);
							val =rnd % pct;
							m[i] [j] [k][nr][nc]= (float) val/100.0;
							if(range<0)
								m[i][j][k][nr][nc] = fabs(range) - (m[i][j][k][nr][nc] * 2.0);
						}
						else
							m[i][j][k][nr][nc] = 0;
					}//nc
				}//nr
			}//k
		}//j

	}//i
	
	r=n;
	c=p;
	d=q;
	mr=mn;
	mc=mm;
	
}

mmat3d::mmat3d(int n, int p, int q, int mn, int mm, float value, float range)
{
	int i,j,k, lr, lc;
	m=new float ****[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float ***[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float **[q];
			for(k=0; k<q; k++)
			{
				
				m[i][j][k]=new float *[mn];
				for(lr=0; lr<q; lr++)
				{
					m[i][j][k][lr]=new float [mm];
					for(lc=0; lc<q; lc++)
					{
						m[i][j][k][lr][lc]=value;
					}//lc
				}//lr
				
			}//k
		}//j
	}//i
	r=n;
	c=p;
	d=q;
	mr=mn;
	mc=mm;
}



mmat3d::mmat3d(int n, int p, int q, int mn, int mm, char *fn)
{
	time_t t;

	int i,j,k, lr, lc;
	m=new float ****[n];
	for(i=0;i<n;i++)
	{
		m[i]=new float ***[p];
		for(j=0;j<p;j++)
		{
			m[i][j]=new float **[q];
			for(k=0; k<q; k++)
			{
				
				m[i][j][k]=new float *[mn];
				for(lr=0; lr<q; lr++)
				{
					m[i][j][k][lr]=new float [mm];
					
				}
				
			}
		}
	}
	r=n;
	c=p;
	d=q;
	mr=mn;
	mc=mm;
	
	
	//ifstream in(fn, ios::in);
	//in >> *this;
}

/*mmat3d::mmat3d(const vecpair  &vp)
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

mmat3d::mmat3d(mmat3d &m1) //copy-initialiser
{
	int i,j,k,mm,mn;
	//D(cout << "mmat3d copy initialiser\n";)
	r=m1.r;
	c=m1.c;
	d=m1.d;
	
	mr=m1.mr;
	mc=m1.mc;
	m=new float ****[r];
	for(i=0; i<r; i++)
	{
		m[i]=new float***[c];
		for(j=0; j<c; j++)
		{
			m[i][j]=new float **[d];
			for(k=0; k<d; k++)
			{
						m[i][j][k]=new float *[d];
						for(mm=0; mm<mr; mm++)
						{
										m[i][j][k][mm]=new float[d];
										for(mn=0; mn<mc; mn++)
										{
											m[i][j][k][mm][mn] = m1.m[i][j][k][mm][mn];											
										}	
							
						}	
				
			}	

		}
	}
}

void mmat3d::resize(int nr, int nc, int nd, int nmr, int nmc) 
{
	int i,j,k;
	int lmr, lmc;
	float value=0;
	
	
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
	   for(k=0; k<d; k++)
	    for(lmr=0; lmr<mr; lmr++)
			delete [] m[i][j][k][lmr];
		
	delete [] m;
	
	m=new float ****[nr];
	for(i=0;i<nr;i++)
	{
		m[i]=new float ***[nc];
		for(j=0;j<nc;j++)
		{
			m[i][j]=new float **[nd];
			for(k=0; k<nd; k++)
			{
				m[i][j][k]=new float * [mr];
				for(lmr=0; lmr<mr; lmr++)
				{
					m[i][j][k][lmr]=new float [mc];
					for(lmc=0; lmc<mc; lmc++)
					{
						m[i][j][k][lmr][lmc]=value;
					}
				}			    	
			}
			  

		}
	}
	r=nr;
	c=nc;
	d=nd;
	
	mr=nmr;
	mc=nmc;

}

mmat3d::~mmat3d()
{
	int i,j,k,lmr;
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
	 for(k=0; k<d; k++)
	    for(lmr=0; lmr<mr; lmr++)
						delete [] m[i][j][k][lmr];
		
	delete [] m;
}//destructor

mmat3d& mmat3d::operator=(const mmat3d& m1)
{
	int i,j,k;
	int lr,lc;
	
	for(i=0;i<r;i++)
	 for(j=0; j<c; j++)
	   for(k=0; k<d; k++)
	     for(lr=0; lr<mr; lr++)
		delete [] m[i][j][k][lr];
		
	delete [] m;
	
	r=m1.r;
	c=m1.c;
	d=m1.d;
	
	mr=m1.mr;
	mc=m1.mc;
	
	m=new float ****[r];
	for(i=0;i<r;i++)
	{
		m[i]=new float ***[c];
		for(j=0;j<c;j++)
		{
			m[i][j]=new float **[d];
			for(k=0; k<d; k++)
			{
				m[i][j][k]=new float *[mr];
				for(lr=0; lr<mr; lr++)
				{
					m[i][j][k][lr]=new float [mc];
					for(k=0; k<d; k++)
					{
									  m[i][j][k][lr][lc]=m1.m[i][j][k][lr][lc];
					}
				}	
			}


		}
	}
	return *this;
}

mmat3d& mmat3d::operator+(const mmat3d& m1)
{
	int i,j,k;
	int lr,lc;
	
	mmat3d msum(r,c,d,mr,md);
	for(i=0;i<r;i++)
		for(j=0;j<r;j++)
			for(k=0;k<d;k++)
			  for(lr=0; lr<mr; lr++)
			    for(lc=0; lc<mc; lc++)
					msum.m[i][j][k][lr][lc]=m1.m[i][j][k][lr][lc]+m[i][j][k][lr][lc];
	return msum;
}


mmat3d& mmat3d::operator*(const float d)
{
	int i,j,k;
	int lr,lc;
	
	mmat3d mprod(r,c,d,mr,mc);
	for(i=0;i<r;i++)
		for(j=0;j<c;j++)
			for(k=0;k<d;k++)
			  for(lr=0; lr<mr, lr++)
			     for(lc=0; lc<mc; lc++)
						mprod[i][j][k][lr][lc]=m[i][j][k][lr][lc] *d;
	return mprod;
}

mmat3d& mmat3d::operator+=(const float d)
{
	int i,j,k;
	int lr,lc;
	
	for(i=0;i<r;i++)
		for(j=0;j<c;j++)
			for(k=0;k<d;k++)
				for(lr=0; lr<mr, lr++)
			     for(lc=0; lc<mc; lc++)
							m[i][j][k][lr][lc] +=d;
	return *this;
}



int mmat3d::rows(){return r;}
int mmat3d::cols(){return c;}
int mmat3d::depth(){return d;}

float mmat3d::getval(int row, int col, int dep, int lr, int lc)
{
  if(row<r && col<c && dep<d && lr<mr && lc<mc)
	return m[row][col][dep][lr][lc];
  else
	 return 0;
}

void mmat3d::setval(int row, int col, int dep, int lr, int lc, float val)
{

	if(row<r && col<c && dep<d && lr<mr && lc<mc)
		m[row][col][dep] [lr][lc] = val;
}

/*int mmat3d::closestcol(vec &v)
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

/*int mmat3d::closestrow(vec  &v)
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

/*int mmat3d::closestrow(vec &v, int *wins, float scaling)
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


// save binary values of mmat3d to specified raw file
int mmat3d::save(FILE *f)
{

	int i, j, success=1;
	int lr,lc;
	
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			for(lr=0; lr<mr, lr++)
			     for(lc=0; lc<mc; lc++)
			if(fwrite( &(m[i][j][k][lr][lc]), sizeof(m[0][0][0][0][0]),1,f)<1)
								success=0;
	return success;
}


//load binary values of mmat3d from specified raw file
int mmat3d::load(FILE *f)
{
	int i, j, success=1;
	int lr,lc;
	
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			for(lr=0; lr<mr, lr++)
			     for(lc=0; lc<mc; lc++)
			if (fread(&(m[i][j][k][lr][lc]), sizeof(m[0][0][0][0][0]),1,f)<0)
				success = 0;
	return success;
}

mmat3d& mmat3d::operator+=(const mmat3d &m1)
{
	int i,j,k;
	int lr,lc;
	
	for(i=0; i<r,i<m1.r; i++)
		for(j=0; j<c,j<m1.c; j++)
				for(k=0; k<d,k<m1.d, j<m1.c; j++)
								for(lr=0; lr<mr, lr++)
			     					for(lc=0; lc<mc; lc++)
										m[i][j][k][lr][lc] += (m1.m[i][j][k][lr][lc]);
	return *this;
}

mmat3d& mmat3d::operator*=(const float d)
{
	int i,j,k;
	int lr,lc;
	
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			for(k=0; k<d; k++)
					for(lr=0; lr<mr, lr++)
			     			for(lc=0; lc<mc; lc++)
				m[i][j][k][lr][lc]*=d;
	return *this;
}


/*vec mmat3d::operator*(vec &v1)
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

/*void mmat3d::initvals(const vec& v1, const vec& v2,
		      const float rate, const float momentum)
{
	int i, j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]=(m[i][j]*momentum)+((v1.v[i]*v2.v[j])*rate);
}*/

ostream& operator<<(ostream& s, mmat3d& m1)
//print a mmat3d
{
	int i, j,k;
	int lr,lc;
	s<< m1.r << " " << m1.c << " " << m1.d << " " << m1.mr << " " << m1.mc << "\n";
	if((m1.r*m1.c*m1.d)>2500)
		s<<"\n";

	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			for(k=0;k<m1.d; k++)
			{
					for(lr=0; lr<mr, lr++)
					{
			     		for(lc=0; lc<mc; lc++)
			     		{
							s << m1.m[i][j][k] << " ";
				
							if((m1.mr*m1.mc)>100)
											s << "\n";
			     		}
					}
					s << "\n";
			}
								
		}

	}
	return s;
}

istream& operator>>(istream& s, mmat3d& m1)
{
	int i, j,k,lr,lc;
	int newr, newc,newd, newmr, newmc;
	s>>newr;
	s>>newc;
	s>>newd;
	s>>newmr;
	s>>newmc;
	
	m1.resize(newr, newc,newd,newmr,newmc);
	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			for(k=0;k<m1.d;k++)
			{
					for(lr=0; lr<mr, lr++)
					{
			     		for(lc=0; lc<mc; lc++)
			     		{
			     						s>>m1.m[i][j][k][lr][lc];	
			     		}
					}
			}

		}
	}
	return s;
}
