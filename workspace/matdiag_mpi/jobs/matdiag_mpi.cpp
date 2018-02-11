

//Include deque below removes g++ 3.4.3 error caused by including
//deque from IoParams.h header
#include <deque>


#ifndef IO_MSVC
	#include "../utils/vec.h"
//	#include "../utils/mat.h"
#else
	#include "..\utils\vec.h"
//	#include "..\utils\mat.h"
#endif

#include "cmatmpi.h"



#include <iostream>
#include <fstream>
#include <stdlib.h>

#include <mpi.h>

int InitialiseMatrix(char  *sfilename, matmpi *pmat, int nside, int npcols);
int readrhsvec(char *sfilename, vec *invec);


using namespace std;
//using namespace debug_channels;
MPI::Intracomm comm;
int NProcs;
int iRank;

int main(int argc, char *argv[])
{	
	int i,j;
    
    vec *v1, *v2, *v3;
    vec v4(3,0);
    float ff=3.5;
    vec vpos(2,0);
    

    int Nside=8, Npcols=4;  //2 proc columns and 4*4 matrix for each proc
	MPI::Init(argc, argv);
	comm=MPI::COMM_WORLD;
	NProcs=comm.Get_size();
	Npcols=NProcs;
	iRank=comm.Get_rank();
    
    if(argc > 1)
	{	
		
	   // if(iRank==0)
	   //  fout.open("results.txt", std::ios::out);
		  
		Nside=atoi(argv[1]);
	}
    
    std::cout << "Matrix is " << Nside << "x" <<  Nside << std::endl;
    std::cout << "There are " << Npcols << " processor columns." << std::endl;
   
    //num proc rowsxnum proc columns=numprocs
    //i.e. factorise exactly 
    matmpi *mptr=new matmpi(Nside,Nside/NProcs,NProcs,Nside,0.0);
    vec *sv=new vec(Nside, 0);
    vec *xv=new vec(Nside, 0);
    matmpi *ml, *mu;
   InitialiseMatrix("mat.in", mptr, Nside, Npcols);
    comm.Barrier();
    
    std::cout << iRank << "reading rhs vec " << std::endl;
    readrhsvec("vec.in", xv);
    comm.Barrier();
    
    	//wait for all procs to initialise
		
		for(j=0; j<NProcs; j++)
		{
			if(j==iRank)
 		   		std::cout << "Matrix on proc " << iRank << " is: " << std::endl <<  *mptr << std::endl;
 		   	comm.Barrier();
		}
    	
    	if(iRank==0)
    	std::cout << "Vec on proc " << iRank << " is: " << std::endl <<  *xv << std::endl;
    	comm.Barrier();
    	
  
    	
    	//Solve input matrix using Gauss-Jordon elimination
    	mptr->ge(sv, xv);
    	
    	comm.Barrier();
    	if(iRank==0)
	{
    	     std::cout << "Solution vector:"<< std::endl << *xv << std::endl;
             ofstream outfile("sol.out", ofstream::out);
             outfile << *xv;
             outfile.close();
	}
    	     
     	comm.Barrier();   
    
if(iRank==0)
	std::cout << *mptr << std::endl;

	//std::cout << " v1 " << *v1 << std::endl;	
	//std::cout << " v2 " << *v2 << std::endl;	
	//std::cout << " v3 " << *v3 << std::endl;

	
	//v4=(*v1+*v2);
	
	//std::cout << " v1 " << *v1 << std::endl;	
	//std::cout << " v2 " << *v2 << std::endl;	
	//std::cout << " v3 " << *v3 << std::endl;		
	//std::cout << " v4 " << v4 << std::endl;	

	//delete v1;
	//delete v2;
	//delete v3;

	//delete vptr;
	delete mptr;
	delete sv;
	//pPropMan.DeleteProperties();
	//delete v4;
	//delete vtest1;
	MPI::Finalize();
	return 0;
}

int InitialiseMatrix(char  *sfilename, matmpi *pmat, int nside, int npcols)
{
	int status=0;
	int i,j,k;
	float *invals;
	
	
	ifstream infile;
	
	int arows, acols;
	int ncpp=nside/npcols;
	
	int proc;
	
	invals=(float *)calloc(nside,sizeof(float));
	for(i=0; i<nside; i++) invals[i]=0;

	
	//if(pmat && (pmat->depth())>=nside && (pmat->width())>=nside)
	//{
		if(iRank==0)
		{
			infile.open (sfilename, ifstream::in);
			infile>>arows>>acols;
			//std::cout << arows << "  " << acols << std::endl;
		}
			
			for(i=0 ;i<nside; i++)
			{
				if(iRank==0)
				{
					//std::cout << "row " << i << std::endl;
					//std::cout << arows << "  " << acols << std::endl;
			   		for(j=0; j<nside; j++)
			   		{
			     	
			   			//read a row
			   			infile>>invals[j];
			   				
			  		}
				}//end of proc 0 only
			   proc=j/npcols;
			   
			   if(iRank==0)
			   {
				   //send column to correct processor
				   for(j=1; j<npcols; j++)
				   		comm.Ssend( (invals+(ncpp*j)) , ncpp , MPI_FLOAT , j, 0 );
				   		
				   //	comm.Barrier();
			   }//end of proc 0 only
			   else
			   {
			   	
			   		 //receive row from proc 0
			  		 comm.Recv( invals , ncpp , MPI_FLOAT , 0 , 0 );
			   		
			   		//comm.Barrier();

			   }//end of other procs
			   		
			   comm.Barrier();
			   
			   for(j=0; j<ncpp; j++)
			       pmat->setval(i,j,invals[j]);
			   
  
			}//end count over i
			
			 if(iRank==0)  
			   infile.close();
			   
			   status=1;

	
	return status;		
}

int readrhsvec(char *sfilename, vec *invec)
{
	int status=0;
	ifstream infile;
	int vsize;
	float fval;
	int i;
	
	if(iRank==0)
	{
		infile.open(sfilename, ifstream::in);
		
		    infile >> vsize;
		    for(i=0; i<vsize; i++)
		    {
					infile>> fval;
					invec->set(i, fval);
		    }	
			infile.close();
			status=1;
		
	}
	else
	   status=1;
	
	return status;	
}
