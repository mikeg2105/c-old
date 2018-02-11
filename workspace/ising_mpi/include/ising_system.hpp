#ifndef _ISING_SYSTEM_H_
#define _ISING_SYSTEM_H_

//System of spin 1/2 ising particles
#include "ising_particle.hpp"
#include "ising_statistics.hpp"

using std::cout;
using std::endl;
using std::cin;

class CIsing_System
{
	private:
		CIsing_Particle *m_particles;	//Particle array
		int m_current;					//1d index of current particle
		int m_iside;					//length of square lattice
		int m_Nparticles;				//number of particles
		int m_left;						//left neighbour to current particle
		int m_right;
		int m_up;
		int m_down;
		float m_h;
		
		void find_neighbours();
		inline int  find_index(int column, int row){return m_iside*row+column;}
		inline int  find_row(int i){return i/m_iside;}
		inline int  find_column(int i){return i%m_iside;}
		
	public:
	
		CIsing_System(int iside);
		~CIsing_System();
		void perturb();
		void restore();
		double energy();
		double magnetisation();	
};

#endif //_ISING_SYSTEM_H_
