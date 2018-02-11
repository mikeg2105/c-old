#ifndef IO_UTIL_VECPAIR_H
#define IO_UTIL_VECPAIR_H
// vecpair.h: interface for the vecpair class.
//
//////////////////////////////////////////////////////////////////////

//#if !defined(AFX_VECPAIR_H__3A8BBA25_05A7_11D4_8915_080408C10000__INCLUDED_)
//#define AFX_VECPAIR_H__3A8BBA25_05A7_11D4_8915_080408C10000__INCLUDED_

//#if _MSC_VER > 1000
//#pragma once
//#endif // _MSC_VER > 1000

#include "mat.h"


class vecpair {
	friend class matrix;
	friend ifstream& operator>>(ifstream& s, vecpair& v1);
	friend ostream& operator<<(ostream& s, vecpair& v1);
	//friend matrix::matrix(const vecpair& vp);
		// flag signalling whether encoding succeeded
		int flag;
	public:
		vec *a;
		vec *b;
		vecpair(int n=ROWS, int p=COLS); //constructor
		vecpair(vec& A, vec& B);
		vecpair(const vecpair& AB); //copy initializer
		~vecpair();
		vecpair& operator=(const vecpair& v1);
		int operator ==(const vecpair& v1);
		vecpair& scale(vecpair& minvecs, vecpair& maxvecs);


};




#endif 