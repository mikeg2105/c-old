#ifndef _VECDIAG_H_
#define _VECDIAG_H_
#include "../utils/vec.h"
#include <mpi.h>

using std::cout;
using std::endl;



class cvecmpi : public vec{
public:

	cvecmpi(int size, float val=0);
	~cvecmpi();

	MPI::Intracomm m_comm;	
	int m_NProcs;
	int m_iRank;
	
	inline void Bcast(int rootproc){m_comm.Bcast(v, length(), MPI::FLOAT, rootproc); }
	
	
};

#endif
