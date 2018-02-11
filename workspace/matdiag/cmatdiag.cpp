#include "cmatdiag.h"

matdiag::matdiag(int n, int p, float range):matrix(n,p,range)
{
}

matdiag::~matdiag()
{
}

int matdiag::lu(matrix *l, matrix *u)
{
	int status=0;
	if(l && u)
	{
		l->resize(r,c);
		u->resize(r,c);		
	}
	else
	  return 1;
	  
	return status;
}
