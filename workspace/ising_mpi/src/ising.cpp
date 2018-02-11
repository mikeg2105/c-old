
//1st April 2005
//Taken from a C++ teaching example by:
//William Barford and Lee Thomson
//Department of Physics
//The University of Sheffield

#include <mpi.h>


#include "../include/ising.hpp"
#include "../include/ising_statistics.hpp"

using std::fstream;
using std::cout;
using std::endl;
using std::cin;

MPI::Intracomm comm;
int NProcs;
int iRank;
		
int main(int argc, char **argv)
{
	fstream fout;
	
	int Nside, Niterations;
	float temp;
	MPI::Init(argc, argv);
	comm=MPI::COMM_WORLD;
	NProcs=comm.Get_size();
	iRank=comm.Get_rank();	
	
	if(argc > 2)
	{	
		
	    if(iRank==0)
		  fout.open("results.txt", std::ios::out);
		  
		Nside=atoi(argv[1]);
		Niterations=atoi(argv[2]);
		
		
		cout << "This is Ising Particle Monte Carlo Simulation Rank " << iRank << " of " << NProcs << endl;		
		cout << "The length of the lattice is: "<<Nside<<endl;
		cout << "The number of Monte-carlo iterations is: " << Niterations<<endl;
		comm.Barrier();
		//cin >> Niterations;
	
		//cout << "Enter the temperature: ";
		//cin >> temp;
	
	
		for(int i=1; i<30; i++)
		{
			temp = 0.1*i;
			MC_simulation(Nside, Niterations, temp, fout);
			
			//barrier to synch procs
			comm.Barrier();
		}
		
		if(iRank==0)
				fout.close();
	
		
	}  //end of check for vars on input line
	
	MPI::Finalize();
	return 0;	
}

void MC_simulation(int Nside, int Niterations, double T, fstream &fout)
{
	//Instantiate a two dimensional ising system of square Nside
	CIsing_System MC_system(Nside);
	CStatistics MC_statistics;
	
	//Warm up iterations
	for(int iter=1; iter<=100*Nside*Nside; iter++)
	{
		MC_system.perturb();
		double energy=MC_system.energy();
		if(exp( -energy/T)<randno())
					MC_system.restore();		
	}
	
	//The Monte-carlo simulation loop
	for(int iter=1; iter<=Niterations; iter++)
	{
		MC_system.perturb();
		double energy=MC_system.energy();
		if(exp( -energy/T)<randno())
					MC_system.restore();
		else
		{
			double magnetisation=MC_system.magnetisation();
			MC_statistics.add(magnetisation);	
		}			
	}
	
	//Total iterations is summed across all processors
	int NTotalits=0;
	comm.Reduce(&Niterations, &NTotalits, 1, MPI::INT, MPI::SUM, 0);
	
	//Barrier to make sure all procs synchronised at this point
	comm.Barrier();
	cout << "Proc "<<iRank<<" reached barrier."<<endl;
	float faverage=MC_statistics.average();
	float ferr=MC_statistics.standard_error();
	comm.Barrier();
	
	if(iRank==0)
	{
		fout<<"Monte Carlo simulation of a " << Nside << "x" << Nside << " spin 1/2 Ising system." << endl;
		fout<<"The temperature is: " << T << endl;
		fout<<"The number of iterations were: " << NTotalits << endl;
		fout<<"The average magnetisation is: " << faverage<<endl;
		fout<<"The standard error of the mean is: " << ferr<<endl<<endl;
	}
	comm.Barrier();
}
