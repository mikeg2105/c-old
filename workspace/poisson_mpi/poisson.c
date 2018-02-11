/*
 * poisson_mpi.c
 * Poisson's equation in 2-D - Jacobi algorithm
 * MPI version using 1-D domain decomposition
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mpi.h"

double L = 1.0;			/* linear size of square region */
int N = 32;			/* number of interior points per dim */

double *u, *u_new;		/* linear arrays to hold solution */
double *f;			/* linear array for source term */

/* macro to index into a 2-D (N+2)x(N+2) array */
#define INDEX(i,j) ((N+2)*(i)+(j))

int num_procs;			/* number of MPI processes */
int my_rank;			/* rank of this process */

int *proc;			/* process indexed by vertex */
int *i_min, *i_max;		/* min, max vertex indices of processes */
int *left_proc, *right_proc;	/* processes to left and right */

void allocate_arrays (void);
void make_source (void);
void make_domains (void);
void Jacobi (void);

int main (int argc, char *argv[]) {
     int i, j, step, n, my_n;
     double change, my_change, *swap;
     double epsilon = 1e-3, wall_time;
     char file_name[100] = "poisson_mpi.out";

     MPI_Init(&argc, &argv);
     wall_time = MPI_Wtime();
     MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
     MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

     if (argc > 1)
	  sscanf(argv[1], "%d", &N);
     if (argc > 2)
	  sscanf(argv[2], "%lf", &epsilon);
     if (argc > 3)
	  strcpy(file_name, argv[3]);

     if (my_rank == 0) {
	  printf("\n 2-D Poisson's equation using Jacobi algorithm");
	  printf("\n =============================================");
	  printf("\n MPI version: 1-D domains, non-blocking send/receive");
	  printf("\n Number of processes         = %d", num_procs);
	  printf("\n Number of interior vertices = %d", N);
	  printf("\n Desired fractional accuracy = %f\n\n", epsilon);
     }

     allocate_arrays();
     make_source();
     make_domains();

     step = 0;
     do {
	  Jacobi();
	  ++step;
	  /* estimate error */
	  change = my_change = 0;
	  n = my_n = 0;
	  for (i = i_min[my_rank]; i <= i_max[my_rank]; i++)
	       for (j = 1; j <= N; j++)
		    if (u_new[INDEX(i,j)] != 0) {
			 my_change += fabs(1 - u[INDEX(i,j)] /
			      u_new[INDEX(i,j)]);
			 ++my_n;
		    }
	  MPI_Allreduce(&my_change, &change, 1, MPI_DOUBLE, MPI_SUM,
			MPI_COMM_WORLD);
	  MPI_Allreduce(&my_n, &n, 1, MPI_INT, MPI_SUM,
			MPI_COMM_WORLD);
	  if (n != 0)
	       change /= n;
	  if (my_rank == 0 && step%10 == 0) {
	       printf(" Step %4d\tError = %g\n", step, change);
	  }
	  /* interchange u and u_new */
	  swap = u;
	  u = u_new;
	  u_new = swap;
     } while (change > epsilon);

     /* copy solution to process 0 and print to file */

     wall_time = MPI_Wtime() - wall_time;
     if (my_rank == 0)
	  printf("\n Wall clock time = %f secs\n", wall_time);
     MPI_Finalize();
     return 0;
}

void Jacobi (void) {
     int i, j;
     double h;
     int requests;
     MPI_Request request[4];
     MPI_Status status[4];

     h = L / (N + 1);		/* lattice spacing */

     /* update ghost layers using non-blocking send/receive */
     requests = 0;
     if (left_proc[my_rank] >= 0 && left_proc[my_rank] < num_procs) {
	  MPI_Irecv(u + INDEX(i_min[my_rank] - 1, 1), N, MPI_DOUBLE,
		    left_proc[my_rank], 0, MPI_COMM_WORLD,
		    request + requests++);
	  MPI_Isend(u + INDEX(i_min[my_rank], 1), N, MPI_DOUBLE,
		    left_proc[my_rank], 1, MPI_COMM_WORLD,
		    request + requests++);
     }
     if (right_proc[my_rank] >= 0 && right_proc[my_rank] < num_procs) {
	  MPI_Irecv(u + INDEX(i_max[my_rank] + 1, 1), N, MPI_DOUBLE,
		    right_proc[my_rank], 1, MPI_COMM_WORLD,
		    request + requests++);
	  MPI_Isend(u + INDEX(i_max[my_rank], 1), N, MPI_DOUBLE,
		    right_proc[my_rank], 0, MPI_COMM_WORLD,
		    request + requests++);
     }

     /* Jacobi update for internal vertices in my domain */
     for (i = i_min[my_rank] + 1; i <= i_max[my_rank] - 1; i++)
	  for (j = 1; j <= N; j++)
	       u_new[INDEX(i,j)] =
		    0.25 * (u[INDEX(i-1,j)] + u[INDEX(i+1,j)] +
			    u[INDEX(i,j-1)] + u[INDEX(i,j+1)] +
			    h * h * f[INDEX(i,j)]);

     /* wait for all non-blocking communications to complete */
     MPI_Waitall(requests, request, status);

     /* Jacobi update for boundary vertices in my domain */
     i = i_min[my_rank];
     for (j = 1; j <= N; j++)
	  u_new[INDEX(i,j)] =
	       0.25 * (u[INDEX(i-1,j)] + u[INDEX(i+1,j)] +
		       u[INDEX(i,j-1)] + u[INDEX(i,j+1)] +
		       h * h * f[INDEX(i,j)]);
     i = i_max[my_rank];
     if (i != i_min[my_rank])
	  for (j = 1; j <= N; j++)
	       u_new[INDEX(i,j)] =
		    0.25 * (u[INDEX(i-1,j)] + u[INDEX(i+1,j)] +
			    u[INDEX(i,j-1)] + u[INDEX(i,j+1)] +
			    h * h * f[INDEX(i,j)]);
}

void allocate_arrays (void) {
     int size, i;

     size = (N + 2) * (N + 2);
     u = (double*) malloc(size * sizeof(double));
     u_new = (double*) malloc(size * sizeof(double));
     f = (double*) malloc(size * sizeof(double));
     for (i = 0; i < size; i++)
	  u[i] = u_new[i] = f[i] = 0;
}

void make_source (void) {
     int i, j;
     double q;

     /* make a dipole */
     q = 10;
     i = j = 1 + N/4;
     f[INDEX(i,j)] = q;
     i = j = 1 + 3*N/4;
     f[INDEX(i,j)] = -q;
}

void make_domains (void) {
     int i, p;
     double eps, d, x_min, x_max;

     /* allocate arrays for process information */
     proc = (int*) malloc((N + 2) * sizeof(int));
     i_min = (int*) malloc(num_procs * sizeof(int));
     i_max = (int*) malloc(num_procs * sizeof(int));
     left_proc = (int*) malloc(num_procs * sizeof(int));
     right_proc = (int*) malloc(num_procs * sizeof(int));

     /* divide range [(-eps+1)..(N+eps)] evenly among processes */
     eps = 0.0001;
     d = (N - 1 + 2*eps) / num_procs;

     for (p = 0; p < num_procs; p++) {
	  /* process domain */
	  x_min = -eps + 1 + p*d;
	  x_max = x_min + d;

	  /* identify vertices belonging to this process */
	  for (i = 1; i <= N; i++)
	       if (x_min <= i && i < x_max)
		    proc[i] = p;
     }

     for (p = 0; p < num_procs; p++) {
	  /* find the smallest vertex index in domain */
	  for (i = 1; i <= N; i++)
	       if (proc[i] == p)
		    break;
	  i_min[p] = i;

	  /* find largest vertex index in domain */
	  for (i = N; i >= 1; i--)
	       if (proc[i] == p)
		    break;
	  i_max[p] = i;

	  /* find processes to left and right */
	  left_proc[p] = right_proc[p] = -1;
	  if (proc[p] != -1) {
	       if (i_min[p] > 1 && i_min[p] <= N)
		    left_proc[p] = proc[i_min[p] - 1];
	       if (i_max[p] > 0 && i_max[p] < N)
		    right_proc[p] = proc[i_max[p] + 1];
	  }
     }
}


