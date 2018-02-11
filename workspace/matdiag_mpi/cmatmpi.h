#ifndef _MATDIAG_H_
#define _MATDIAG_H_
#include "../utils/mat.h"
#include <mpi.h>

using std::cout;
using std::endl;
/*
 * Classes with matrix diagonalisation method
 * 
 * */

//integers define if proc sends or receives
const int RECV = 1; 
const int SEND = 0;
 
class matmpi : public matrix
{
public:
	matmpi(int n=ROWS, int p=COLS, int npcols=1, int ac=COLS, float range=0);
	matmpi(matmpi& m1); //copy-initializer
	virtual ~matmpi();
	
	MPI::Intracomm m_comm;	
	int m_NProcs;
	int m_iRank;
	
	//matrix distributed over a collection of processors
	//procs arranged as a matrix of m_npr rows and m_npc columns
	//m_pr proc row number
	//m_pc proc column number
	int m_npr, m_npc;
	int m_ncpp;
	int m_pr, m_pc;
	
	//actual number of columns for full matrix
	int m_ac;
	/*! 
	 * lu decomposition of matrix
	 * 
	 * */
	int ge(vec *xv, vec *sv);
	int backsub(vec *xv, vec *sv);
	
	int GetProcRank(int row, int col){return((row*m_npc)+m_pc);}
};

#endif //_MATDIAG_H_
