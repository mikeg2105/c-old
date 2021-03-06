#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#define mm 4
#define npart 4*mm*mm*mm
/*
 *  Function declarations
 */

  void
  dfill(int,double,double[],int);

  void
  domove(int,double[],double[],double[],double);

  void
  dscal(int,double,double[],int);

  void
  fcc(double[],int,int,double);

  void
  forces(int,double[],double[],double,double);

  double
  mkekin(int,double[],double[],double,double);

  void
  mxwell(double[],int,double,double);

  void
  prnout(int,double,double,double,double,double,double,int,double);

  double
  velavg(int,double[],double,double);

  double 
  secnds(void);

/*
 *  Variable declarations
 */

  double epot;
  double vir;
  double count;

/*
 *  Main program : Molecular Dynamics simulation.
 */
  void main(int argc, char *argv[]){
    int move;
    double x[npart*3], vh[npart*3], f[npart*3];
    double ekin;
    double vel;
    double sc;
    double start, time;


  /*
   *  Parameter definitions
   */
    double den, tref;
/*	den    = 0.83134;*/
        /*den=0.95;*/
        /*den=0.75;*/
        den=0.65;
    	tref   = 0.722;

	if(argc>2)
	{
		printf("%s \n", argv[0]);
                den = atof(argv[1]);
                tref=atof(argv[2]);

	}

	if(argc>1 && (strcmp(argv[1], "help")==0))
	{
			printf("Molecular dynamics simulation\n");
			printf("Command line takes the format:\n");
			printf("md density reducedtemperature\n");
			printf("Defualt Values are:\n");
			printf("density=0.83134\n");
			printf("reduced temperature=0.722\n");

    	}
else
{

    double side   = pow((double)npart/den,0.3333333);
    double rcoff  = (double)mm/4.0;
    double h      = 0.064;
    int    irep   = 10;
    int    istop  = 20;
    int    iprint = 5;
    int    movemx = 100;

    double a      = side/(double)mm;
    double hsq    = h*h;
    double hsq2   = hsq*0.5;
    double tscale = 16.0/((double)npart-1.0);
    double vaver  = 1.13*sqrt(tref/24.0);

  /*
   *  Initial output
   */

    printf(" Molecular Dynamics Simulation example program\n");
    printf(" ---------------------------------------------\n");
    printf(" number of particles is ............ %6d\n",npart);
    printf(" density is......................... %13.6f\n",den);
    printf(" side length of the box is ......... %13.6f\n",side);
    printf(" cut off is ........................ %13.6f\n",rcoff);
    printf(" reduced temperature is ............ %13.6f\n",tref);
    printf(" basic timestep is ................. %13.6f\n",h);
    printf(" temperature scale interval ........ %6d\n",irep);
    printf(" stop scaling at move .............. %6d\n",istop);
    printf(" print interval .................... %6d\n",iprint);
    printf(" total no. of steps ................ %6d\n",movemx);

  /*
   *  Generate fcc lattice for atoms inside box
   */
    fcc(x, npart, mm, a);
  /*
   *  Initialise velocities and forces (which are zero in fcc positions)
   */
    mxwell(vh, 3*npart, h, tref);
    dfill(3*npart, 0.0, f, 1);
  /*
   *  Start of md
   */
    printf("\n    i       ke         pe            e         temp   "
           "   pres      vel      rp\n  -----  ----------  ----------"
           "  ----------  --------  --------  --------  ----\n");

     start = secnds(); 


    for (move=1; move<=movemx; move++) {

    /*
     *  Move the particles and partially update velocities
     */
      domove(3*npart, x, vh, f, side);

    /*
     *  Compute forces in the new positions and accumulate the virial
     *  and potential energy.
     */
      forces(npart, x, f, side, rcoff);

    /*
     *  Scale forces, complete update of velocities and compute k.e.
     */
      ekin=mkekin(npart, f, vh, hsq2, hsq);

    /*
     *  Average the velocity and temperature scale if desired
     */
      vel=velavg(npart, vh, vaver, h);
      if (move<istop && fmod(move, irep)==0) {
        sc=sqrt(tref/(tscale*ekin));
        dscal(3*npart, sc, vh, 1);
        ekin=tref/tscale;
      }

    /*
     *  Sum to get full potential energy and virial
     */
      if (fmod(move, iprint)==0)
        prnout(move, ekin, epot, tscale, vir, vel, count, npart, den);
      
    }

    time = secnds() - start;  

    printf("Time =  %f\n",(float) time); 
    }/*End of testing for help*/ 

  }

time_t starttime = 0; 

double secnds()
{

  /*struct timeval ts; 

  double t;

  int err; 

  err = gettimeofday(&ts, NULL); 

  t = (double) (ts.tv_sec - starttime)  + (double) ts.tv_usec * 1.0e-6; 
 
  return t; */


        double retval;
	static long zsec=0;
	static long zusec=0;
	double esec;
	
	struct timeval tp;
	struct timezone tzp;
	
	gettimeofday(&tp, &tzp);
	
	if(zsec==0) zsec=tp.tv_sec;
	if(zusec==0) zusec=tp.tv_usec;
	
	retval=(tp.tv_sec - zsec)+(tp.tv_usec-zusec)*0.000001;
	return retval;



}

