/*getrow.c*/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int readfile(char *sinfile, double **ddata, int *rows, int *cols);
void getrowinfo(double *data, int r, int c, int *fr, int *lr);
void getcolinfo(double *data, int r, int c, int *fc, int *lc);
void writedata(char *soutfile, double *data, int nrow, int ncol, int fr, int lr, int fc, int lc, int pid, int
nproc);
void formlist(double radius, int **listx, int **listy, int *numpoint);
void avg(double *indat, double *outdat, int *listx, int *listy, int numpoint, int rows, int cols, int pid, int
nproc );
void initoutdat(double **ddata, int rows, int cols);

int main(int argc, char **argv)
{
	MPI_Status mstatus;
	int status=0;
	int rank,size;


	FILE *infile, *outfile;
    	char sinfile[20], soutfile[20];
	double *data=NULL;
	double *outdata=NULL;
	int nrow, ncol;
	
	int fr,fc;
	int lr,lc;
	
	int pid, nproc;
	
	double radius=10.0;
	int *listx=NULL;
	int *listy=NULL;
	int numpoint=0;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);



	
	if(argc<5)
	   status=1;
	else
	{
		strcpy(sinfile, argv [1]);
		strcpy(soutfile, argv [2]);
		/*pid=atoi(argv[3]);
		nproc=atoi(argv[4]);*/
		pid=rank;
		nproc=size;
		
		if(pid==0)
			readfile(sinfile, &data, &nrow, &ncol);
        /*getrowinfo(data, nrow, ncol, &fr, &lr);
        printf("%d %d %d\n", nrow, fr, lr);
        getcolinfo(data, nrow, ncol, &fc, &lc);
        printf("%d %d %d\n", ncol, fc, lc);*/
        
        /*Broad cast data to all processors*/
        /*if(pid==0)*/
        	MPI_Bcast(data,nrow*ncol,MPI_DOUBLE,0,MPI_COMM_WORLD);
        
	printf("nproc=%d pid=%d\n", nproc, pid);
        initoutdat(&outdata, nrow, ncol);
        formlist(radius, &listx, &listy, &numpoint);
        avg(data, outdata, listx, listy, numpoint, nrow, ncol,pid,nproc );        
        /*writedata(soutfile, outdata, nrow, ncol, fr, lr, fc, lc);*/
        writedata(soutfile, outdata, nrow, ncol, 0, nrow-1, 0, ncol-1,pid,nproc);			
	}
	
	if(data) free(data);
	
	
	return status;
}

int readfile(char *sinfile, double **ddata, int *rows, int *cols)
{
	int i,j;
	int status=0;
	FILE *infile;
	char sdesc[10];
	
	double *data;
	double *outdata;
	
		
	if((infile=fopen(sinfile, "r")) != NULL)
	{
		/*read nrows*/
		fscanf(infile, "%s %d\n", sdesc, rows);
		
		/*read ncols*/
		fscanf(infile, "%s %d\n", sdesc, cols);
		
		data = (double *)calloc(*rows* *cols, sizeof(double));
		*ddata = data;
		
		/*skip 4 lines*/
		for(i=0; i<4; i++)
			while(fgetc(infile) != '\n');
		   

        for(i=0; i< *rows; i++)
                for(j=0; j< *cols; j++)
					fscanf(infile, "%lf", data+(j+( *cols*i)));	
	
		
	}
	else
	  status=1;
	
	MPI_Finalize();
	return status;


}

void getrowinfo(double *data, int r, int c, int *fr, int *lr)
{
    int i, j;
    int gt0;
    double test;
    *fr=-999;
    *lr=-999;
    for(i=0; i<r; i++)
    {
        gt0=-1;
        for(j=0; j<c; j++)
        {
                test = *(data+(j+( c*i)));
                if((test >=0) &&  (*fr < 0))
                 *fr=i;
 
                if(test >= 0 && gt0 < 0) gt0 =1;                          
        }
        if(gt0>0)
            *lr=i;
    }
}

void getcolinfo(double *data, int r, int c, int *fc, int *lc)
{
    int i, j;
    int gt0;
    double test;
    *fc=-999;
    *lc=-999;
    for(j=0; j<c; j++)
    {
        gt0=-1;
        for(i=0; i<r; i++)
        {
                test = *(data+(j+( c*i)));
                if(test >=0 && *fc < 0)
                 *fc=j;
 
                if(test >=0 && gt0<0) gt0=1;             
        }
        if(gt0>0)
            *lc=j;
    }
}

void writedata(char *soutfile, double *data, int nrow, int ncol, int fr, int lr, int fc, int lc, int pid, int
nproc)
{
    int i,j;
	int status=0;
	FILE *outfile;
	
    int r, c;
    int upper, lower, nplayer, nrem;
       
    r=1+lr-fr;
    c=1+lc-fc;
    /*r=nrow;
    c=ncol;
    fc=0;
    fr=0;*/
    	


    nrem=ncol%nproc;
    nplayer=(ncol/nproc);
    lower=nplayer*(pid-1);
    if(pid<nproc)
    	upper=nplayer*pid;
    else
        upper=nrem+nplayer*pid;


		
	if((outfile=fopen(soutfile, "w+")) != NULL)
	{
	   if(pid==1)
	   	fprintf(outfile, "%d %d\n", r, c);
	   for(i=0; i<r; i++)
	   {
	     for(j=lower; j<upper; j++)
	        fprintf(outfile, "%f ", *(data+((fc+j)+( ncol*(fr+i))))); 

             fprintf(outfile, "\n");
           }
	}



}

void formlist(double radius, int **listx, int **listy, int *numpoint)
{
  int n=2*((int)radius);
  int i, j;
  int count=0;
  double dist;
  double rsq=radius*radius;
   
  /*Calculate number of points*/
  for(i=0; i<n; i++)
     for(j=0; j<n; j++)
          if(((i-(n/2))*(i-(n/2)) + (j-(n/2))*(j-(n/2)))<rsq)
          count++; 
 
 *numpoint=count;
 *listx=(int *)calloc(*numpoint, sizeof(int));
 *listy=(int *)calloc(*numpoint, sizeof(int)); 
 
 count=0;
 for(i=0; i<n; i++)
     for(j=0; j<n; j++)
          if(((i-(n/2))*(i-(n/2)) + (j-(n/2))*(j-(n/2)))<rsq)
          {
                    *((*listx)+count)=i-(n/2);
                    *((*listy)+count)=j-(n/2);
                    count++;               
          }  



}

void avg(double *indat, double *outdat, int *listx, int *listy, int numpoint, int rows, int cols, int pid, int
nproc)
{
    int i, j, k;
    int xp, yp;
    double res, avg,total;
    int count, np;
    
    int upper, lower, nplayer, nrem;
    nrem=cols%nproc;
    nplayer=(cols/nproc);
    lower=nplayer*(pid-1);
    if(pid<nproc)
    	upper=nplayer*pid;
    else
        upper=nrem+nplayer*pid;
	
    
    for(i=0; i<rows; i++)
      for(j=lower; j<upper; j++)
      {
            count=0;
            for(k=0; k<numpoint; k++)
            {
                 xp=i+ *(listx+k);       
                 yp=j+ *(listy+k);
                 
                 if((xp<rows) && (xp>=0) && (yp<cols) && (yp>=0) && ((res= *(indat+yp+(xp*cols)))>=0))
                 {
                                  total += res;
                                  count++;
                 }     
            }
            if(count>0)
                    *(outdat+j+(i*cols)) = total/count;
                 else        
                    *(outdat+j+(i*cols)) = 0;
            total=0;
      
      
      }

}

void initoutdat(double **ddata, int rows, int cols)
{
    int i, j;
    double *data;
    
    data = (double *)calloc(rows*cols, sizeof(double));
    *ddata = data;
    
    for(i=0; i<rows; i++)
      for(j=0; j<cols; j++)
            *(data+(j+(i*cols)))=0.0;
				
}


