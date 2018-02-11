// mat.cpp: implementation of the mat class.
//
//////////////////////////////////////////////////////////////////////

#include "mat.h"



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////
//matrix member functions
matrix::matrix(int n,int p, float range)
{
	int i,j,rnd;
	time_t t;
	int pct, val;

	m=new float *[n];
	if (range)
	{
		  time(&t);
		  srand((unsigned)t);
	}
	for(i=0;i<n;i++)
	{
		m[i]=new float[p];
		for(j=0;j<p;j++)
		{
			if(range)
			{
				rnd=rand();
				pct=(int) (range * 100.0);
				val =rnd % pct;
				m[i] [j] = (float) val/100.0;
				if(range<0)
					m[i][j] = fabs(range) - (m[i][j] * 2.0);
			}
			else
				m[i] [j] = 0;
		}

	}
	r=n;
	c=p;
	
}

matrix::matrix(int n, int p, float value, float range)
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

matrix::matrix(int n, int p, char *fn)
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

/*matrix::matrix(const vecpair  &vp)
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

matrix::matrix(matrix &m1) //copy-initialiser
{
	int i,j;
	//D(cout << "matrix copy initialiser\n";)
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



matrix::~matrix()
{
	int i;
	for(i=0;i<r;i++)
		delete [] m[i];
	delete [] m;
}//destructor

matrix& matrix::operator=(const matrix& m1)
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

matrix& matrix::operator+(const matrix& m1)
{
	int i,j;
	matrix msum(r,c);
	for(i=0;i<r;i++)
		for(j=0;j<r;j++)
			msum.m[i][j]=m1.m[i][j]+m[i][j];
	return *this;
}


matrix& matrix::operator*(const float d)
{
	int i,j;
	for(i=0;i<r;i++)
		for(j=0;j<c;j++)
			m[i][j] *=d;
	return *this;
}

vec *matrix::colslice(int col)
{
	int i;
	vec *temp = new vec(r);

	for(i=0; i<r; i++)
		temp->v[i]=m[i][col];
	return temp;
}

void matrix::colslice(int col, vec *v)
{
    int i;
	if(v && (v->length()<=r))
		for(i=0; i<r; i++)
			v->set(i,m[i][col]);

}

vec *matrix::rowslice(int row)
{
    int i;
	vec *temp = new vec(c);
	for(i=0; i<c; i++)
		temp->v[i]=m[row][i];
	return temp;
}

void matrix::rowslice(int row, vec *v)
{
	int i;
	if(v && (v->length()<=c))
		for(i=0; i<c; i++)
				v->set(i, m[row][i]);
			

}

void matrix::insertcol(vec &v, int col)
{
	int i;
	for(i=0; i<v.n; i++)
		m[i][col]=v.v[i];
}

void matrix::insertrow(vec &v, int row)
{
    int i;
	for(i=0; i<v.n; i++)
		m[row][i]=v.v[i];
}

int matrix::depth(){return r;}
int matrix::width(){return c;}

float matrix::getval(int row, int col)
{
  if(row<r && col<c)
	return m[row][col];
  else
	 return 0;
}

void matrix::setval(int row, int col, float val)
{

	if(row<r && col<c)
		m[row][col] = val;
}

int matrix::closestcol(vec &v)
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

int matrix::closestrow(vec  &v)
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

int matrix::closestrow(vec &v, int *wins, float scaling)
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


// save binary values of matrix to specified raw file
int matrix::save(FILE *f)
{

	int i, j, success=1;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			if(fwrite( &(m[i][j]), sizeof(m[0][0]),1,f)<1)
								success=0;
	return success;
}


//load binary values of matrix from specified raw file
int matrix::load(FILE *f)
{
	int i, j, success=1;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			if (fread(&(m[i][j]), sizeof(m[0][0]),1,f)<0)
				success = 0;
	return success;
}

matrix& matrix::operator+=(const matrix &m1)
{
	int i,j;
	for(i=0; i<r&&i<m1.r; i++)
		for(j=0; j<c&&j<m1.c; j++)
			m[i][j] += (m1.m[i][j]);
	return *this;
		
}

matrix& matrix::operator*=(const float d)
{
	int i,j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]*=d;
	return *this;
}


vec matrix::operator*(vec &v1)
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

void matrix::initvals(const vec& v1, const vec& v2,
		      const float rate, const float momentum)
{
	int i, j;
	for(i=0; i<r; i++)
		for(j=0; j<c; j++)
			m[i][j]=(m[i][j]*momentum)+((v1.v[i]*v2.v[j])*rate);
}

ostream& operator<<(ostream& s, matrix& m1)
//print a matrix
{
	int i, j;
	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			s << m1.m[i][j] << " ";
		}
		s << "\n";
	}
	return s;
}

istream& operator>>(istream& s, matrix& m1)
{
	int i, j;
	for(i=0; i<m1.r; i++)
	{
		for(j=0; j<m1.c; j++)
		{
			s>>m1.m[i][j];
		}
	}
	return s;
}
