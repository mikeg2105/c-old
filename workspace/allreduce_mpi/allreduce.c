#include <mpi.h>
#include <stdio.h>

#include <sys/time.h>

#define ARRAY_SIZE  4096
#define NUM_TIMINGS 10

int main(int argc, char** argv)
{
	int i,t,myRank;

	int iiarray[ARRAY_SIZE];
	float ifarray[ARRAY_SIZE];
	int oiarray[ARRAY_SIZE];
	float ofarray[ARRAY_SIZE];

	float timings[NUM_TIMINGS];

	struct timeval before,after;


	MPI_Init(&argc, &argv);

	MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

	/* Put some garbage into our input arrays */
	if(myRank==0)
		printf("MPI initialised...\nLoading arrays\n");
	for(i=0;i<ARRAY_SIZE;i++)
	{
		iiarray[i] = i + myRank;
		ifarray[i] = iiarray[i] * 2.0f;
	}
	if(myRank==0)
		printf("Arrays loaded....\nStarting timing\n");
	for(t=0;t<NUM_TIMINGS;t++)
	{
		if(myRank==0)
		{
			printf("Timing %d\n", t);
			gettimeofday(&before,NULL);
		}

		MPI_Allreduce(iiarray,
									oiarray,
									ARRAY_SIZE, 
									MPI_INT,
									MPI_SUM,
									MPI_COMM_WORLD);

		MPI_Allreduce(ifarray,
									ofarray,
									ARRAY_SIZE, 
									MPI_FLOAT,
									MPI_SUM,
									MPI_COMM_WORLD);

		if(myRank==0)
		{
			gettimeofday(&after,NULL);

			timings[t] = ((float)(after.tv_sec - before.tv_sec))
				+ ((float)(after.tv_usec - before.tv_usec)) / 1000000.0f;
			printf("Timing %d complete.\n Reported time %f\n", t,timings[t]);	
		}
	}

	if(myRank==0)
	{
		printf("Timings complete, summary follows.\n");
		for(t=0;t<NUM_TIMINGS;t++)
		{
			printf("Timing %02d: %g seconds\n",t,timings[t]);
		}
	}


	MPI_Finalize();
}

