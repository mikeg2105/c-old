#ifndef _MATDIAG_H_
#define _MATDIAG_H_
#include "../utils/mat.h"

/*
 * Classes with matrix diagonalisation method
 * 
 * */
class matdiag : public matrix
{
public:
	matdiag(int n=ROWS, int p=COLS, float range=0);
	virtual ~matdiag();
	
	/*! 
	 * lu decomposition of matrix
	 * 
	 * */
	int lu(matrix *l, matrix *u);
};

#endif //_MATDIAG_H_
