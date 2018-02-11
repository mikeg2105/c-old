// vecpair.cpp: implementation of the vecpair class.
//
//////////////////////////////////////////////////////////////////////

//#include "stdafx.h"
//#include "simulantv2.h"
#include "vecpair.h"

//#ifdef _DEBUG
//#undef THIS_FILE
//static char THIS_FILE[]=__FILE__;
//#define new DEBUG_NEW
//#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////
// vecpair member functions
// constructor
vecpair::vecpair(int n, int p)
{
	a=new vec(n); b=new vec(p);
}

vecpair::vecpair(vec& A, vec& B)
{
	a=new vec(A.length());
	*a=A;
	b=new vec(B.length());
	*b=B;
}

vecpair::vecpair(const vecpair &AB)
{
	*this=vecpair(*(AB.a),*(AB.b));
}

vecpair::~vecpair()
{
	delete [] a; delete [] b;
} //destructor

vecpair& vecpair::operator=(const vecpair& v1)
{
	*a= *(v1.a);

	*b= *(v1.b);
	return *this;

}

vecpair& vecpair::scale(vecpair& minvecs, vecpair& maxvecs)
{	
	a->scale(*(minvecs.a), *(maxvecs.a));
	b->scale(*(minvecs.b), *(maxvecs.b));
	return *this;
}

int vecpair::operator==(const vecpair& v1)
{
	return (*a == *(v1.a)) && (*b == *(v1.b));
	
}

ifstream& operator>>(ifstream& s, vecpair &v1)
// input a vector pair
{
	v1.a->read(s, *v1.a);
	v1.b->read(s, *v1.b);
	
	//s>> *(v1.a) >> *(v1.b);
	return s;
}

ostream& operator<<(ostream& s, vecpair &v1)
// print a vector pair
{

    v1.a->write(s, *v1.a);
	v1.b->write(s, *v1.b);

	s<<"\n";
	return s;
	//return s<< *(v1.a) << *(v1.b) << "\n";
}

