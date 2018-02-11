#ifndef _ISING_STATISTICS_H_
#define _ISING_STATISTICS_H_
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <mpi.h>

double randno();

class CStatistics
{
	private:
		double m_sum_x;
		double m_sum_xsq;
		long int m_N;
	
	public:
		CStatistics();
		void add(double x);
		double average();
		double standard_error();
		
		MPI::Intracomm m_comm;	
		int m_NProcs;
		int m_iRank;
};



#endif //_ISING_STATISTICS_H_
