	#include "../include/ising_statistics.hpp"

	double ::randno()
	{
		return 1.0*rand()/(pow(2,31)-1);		
	}
	
	CStatistics::CStatistics()
	{
		m_sum_x=0.0;
		m_sum_xsq=0.0;
		m_N=0;
		
		m_comm=MPI::COMM_WORLD;
		m_NProcs=m_comm.Get_size();
		m_iRank=m_comm.Get_rank();
	}
	
	void CStatistics::add(double x)
	{
		m_sum_x+=x;
		m_sum_xsq+=x*x;
		m_N++;	
	}
	
	double CStatistics::average()
	{
		//calculate global total and sum over all procs
		int iglob_N=0;
		double fglob_sum_x=0;
		
		m_comm.Reduce(&m_sum_x, &fglob_sum_x, 1, MPI::DOUBLE, MPI::SUM, 0);
		m_comm.Reduce(&m_N, &iglob_N, 1, MPI::INT, MPI::SUM, 0);
		
		if(m_iRank==0)
		   cout << "total="<<iglob_N<<" local sum= "<<m_sum_x << " global sum="<<fglob_sum_x<<" proc=" <<m_iRank<<endl;
		
		//return m_sum_x/m_N;
		return fglob_sum_x/iglob_N;
	}
	
	double CStatistics::standard_error()
	{
		//calculate global total and sum over all procs
		int iglob_N=0;
		double fglob_sum_x=0;
		double fglob_sum_xsq=0;
		
		double variance;
		
		m_comm.Reduce(&m_sum_x, &fglob_sum_x, 1, MPI::DOUBLE, MPI::SUM, 0);
		m_comm.Reduce(&m_sum_xsq, &fglob_sum_xsq, 1, MPI::DOUBLE, MPI::SUM, 0);
		m_comm.Reduce(&m_N, &iglob_N, 1, MPI::INT, MPI::SUM, 0);
		
		//variance=(m_N*m_sum_xsq-m_sum_x*m_sum_x)/((m_N-1)*m_N);
		//return sqrt(variance/m_N);
		
		variance=(iglob_N*fglob_sum_xsq-fglob_sum_x*fglob_sum_x)/((iglob_N-1)*iglob_N);
		return sqrt(variance/iglob_N);
	}
	
