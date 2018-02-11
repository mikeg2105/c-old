#include "cvecmpi.h"

cvecmpi::cvecmpi(int size, float val):vec(size,val)
{
		m_comm=MPI::COMM_WORLD;
		m_NProcs=m_comm.Get_size();
		m_iRank=m_comm.Get_rank();
}
cvecmpi::~cvecmpi()
{}


