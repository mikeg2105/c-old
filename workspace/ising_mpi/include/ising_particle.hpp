#ifndef _ISING_PARTICLE_H_
#define _ISING_PARTICLE_H_


class CIsing_Particle
{
	private:
		int m_spin;
	
	
	public:
		CIsing_Particle();
		~CIsing_Particle();
		void flip_spin(){m_spin=-m_spin;}
		inline int spin_value(){return m_spin;}	
	
	
	
};

#endif //_ISING_PARTICLE_H_
