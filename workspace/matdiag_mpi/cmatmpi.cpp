#include "cmatmpi.h"

matmpi::matmpi(int n, int p, int npcols, int ac, float range):matrix(n,p,range)
{
		m_comm=MPI::COMM_WORLD;
		m_NProcs=m_comm.Get_size();
		m_iRank=m_comm.Get_rank();
		
		m_npr=m_NProcs/npcols;
		m_npc=npcols;
		m_ncpp=ac/npcols;
	    //m_pr=m_iRank/m_npc;
	    m_pr=0;
	    m_pc=m_iRank-(m_pr*m_npc);
	    
	    m_ac=ac;
}

matmpi::matmpi(matmpi &m1):matrix(m1) //copy-initialiser
{
	m_comm=m1.m_comm;
	m_NProcs=m1.m_NProcs;
	m_iRank=m1.m_iRank;
	
	m_npr=m1.m_npr;
	m_npc=m1.m_npc;
	m_ncpp=m1.m_ncpp;
	m_pr=m1.m_pr;
	m_pc=m1.m_pc;
	
	m_ac=m1.m_ac;
}

matmpi::~matmpi()
{
}

int matmpi::ge(vec *xv, vec *sv)
{
	int status=0;
	int jini;
	int i,j,k,l;
	
	int ncpp=m_ac/m_npc;
	int p,ac;
	int col;
	
	int rmax;//row with max val
	float rmval;//value of row with max val
	float rmdiag;
	float temp;
	int brroot; //processor which is root for broadcasts
	float mfactor;
	
	//Must be a non-singular square matrix
	//fits exactly and evenly onto processors
	
	//input solution vector must
	//have size equal to number of rows
	
	//if(sv && (sv->length())>=r)
	//{
    //for(i=0; i<r-1; i++)
    //{
    	for(col=0; col<m_ac-1; col++)
    	{
    		
    		ac=col-ncpp*m_pc;
    		brroot=col/ncpp;
    		i=col;
    		
    		//cout << *this << endl;
    		    		
    		if(ac>=0 && ac<c)
    		{
     			//determine row with largest element
    			rmax=i;
    			
    			//only pivot if the current element to divide by is zero
    			if(m[i][ac]==0)
    			{
	    			rmval=-9e30;
	    			for(k=0; k<r; k++)
	    			{
	    				if( m[k][ac]>rmval)
	    				{
	    					rmval=m[k][ac];
	    					rmax=k;	
	    				}
	    			}//k
    			}
			  			
    			//send row to swap
    			m_comm.Barrier();
    			m_comm.Bcast(&rmax, 1, MPI::INT, brroot);
     			m_comm.Barrier();   			
    			//swap rows inmatrix
    			if(rmax != i)
    			{
    				for(k=0; k<c; k++)
    				{
    					temp=m[rmax][k];
    					m[rmax][k]=m[i][k];
    					m[i][k]=temp;   					
    				} 
    				//swap the vector element
					if(m_iRank==0)
					{
						temp=xv->get(rmax);
						xv->set(rmax, xv->get(i));
						xv->set(i, temp);  						
					}  				
    			}
        	  // cout <<"swapped: " << *this << endl;    			
    			//cout << "c=" << c << endl;
 
    				//mult row j by factor   				
    				//subtract row i from row j
    				for(k=1; k<r; k++)
    				{
    					if(    k != i)
    					{
    					//mult factor
    					mfactor=m[k][ac]/m[i][ac];
    					 //cout << "proc " <<m_iRank << " root " << brroot << " factor  " << mfactor << endl;
    					
    				   	m_comm.Barrier();
    					//send multfactor
    					m_comm.Bcast(&mfactor, 1, MPI::FLOAT, brroot);    					
     				   	m_comm.Barrier();   					
    					for(l=0; l<c;l++)
    					{
    					   m[k][l]=m[k][l]-mfactor*m[i][l];
 
 
    					}

    					if(m_iRank==0)
    			   				xv->set(k, (xv->get(k))-mfactor*(xv->get(i))); 
    					}   					
    				}
         	  		//cout <<"subtracted: " << *this << endl;   				
    				m_comm.Barrier();
 
         	 //  cout <<"normaised: " << *this << endl<<endl;   			
    		}//ifac check
    		else
    		{
     			//receive row to swap
     			m_comm.Barrier();
    			m_comm.Bcast(&rmax, 1, MPI::INT, brroot);
    			m_comm.Barrier();
    			//m_comm.Bcast(&rmval, 1, MPI::INT, brroot);
    			//swap rows
    			//if(m_iRank>brroot)
    			//{
    			
	     		if(rmax != i)
	    			{
	    				for(k=0; k<c; k++)
	    				{
	    					temp=m[rmax][k];
	    					m[rmax][k]=m[i][k];
	    					m[i][k]=temp;   					
	    				} 
	    				//swap the vector element
						if(m_iRank==0)
						{
							temp=xv->get(rmax);
							xv->set(rmax, xv->get(i));
							xv->set(i, temp);  						
						}  				
	    			}
				

   	
    				for(k=1; k<r; k++)
    				{
    					if(k != i)
    					{
	    					 //receive mult factor
	    					m_comm.Barrier();	    				
	    					m_comm.Bcast(&mfactor, 1, MPI::FLOAT, brroot);
	    				    m_comm.Barrier(); 
	    					for(l=0; l<c;l++)
	    					{
	    					   m[k][l]=m[k][l]-mfactor*m[i][l];
	 
	
	    					}
	      				   	if(m_iRank==0)
	    			   			xv->set(k, (xv->get(k))-mfactor*(xv->get(i)));   
    					}
 				
    				}
    				m_comm.Barrier();			    			   			
    		}
    	}//end of count over cols
    	
    	//backsub(xv,sv);

        //cout << m_iRank << " returning " << endl;
	
		m_comm.Barrier();
		return status;
}

int matmpi::backsub(vec *xv, vec *sv)
{
	int status=0;
       int z, i,j,k,col,brroot,ac;
        //get solution by back substitution
        float prodsum, prod,divisor,val;
        float *psv=(float *)new float[r];
        float *pxv=(float *)new float[r];
        int kinitial,kfinal,ac1;
        int ncpp=m_ac/m_npc;
        for(i=0; i<r; i++) psv[i]=0; 
        //broadcast solution to all procs 
  
        if(m_iRank==0)
        {
	        for(i=0; i<r; i++)
	           pxv[i]=(xv->get(i));
	           
	    	m_comm.Barrier();	    				
	    	m_comm.Bcast(pxv, r, MPI::FLOAT, 0);
	    	m_comm.Barrier(); 	  	           
        }
        else
        {
        	
  	    	m_comm.Barrier();	    				
	    	m_comm.Bcast(pxv, r, MPI::FLOAT, 0);
	    	m_comm.Barrier();       	       	
        }
        
   		/*for(z=0; z<m_NProcs; z++)
    	{
    			m_comm.Barrier();
    			if(z==m_iRank) cout << "pxv "<<m_iRank << "  " <<pxv[0] << "  " << pxv[1] << " "<< pxv[2]<< " " << psv[3]<< endl;   				
    	}*/        
        
        
       // if(m_iRank==0)
       	//			cout << "back sub" << endl;
        //perform back substitution
       //  m_comm.Barrier();
		for(j=r-1; j>=0; j--)
		{
                               // m_comm.Barrier();
 				    //if(m_iRank==0)
       				//	cout << "back sub step " << j << endl;
 				   
 				    //cout << "proc " << m_iRank << " row " << j <<endl;
 				    col=j;		
    				ac=col-ncpp*m_pc;
    				brroot=col/ncpp;
    				i=col;
   					//m_comm.Barrier();
    				if(ac>=0 && ac<c)
    				{
                                        
    					divisor=m[j][ac];
                                        //divisor=2;
                                       // cout << "divisor " << j << " " << ac << " on  " << m_iRank << "is " << divisor << endl;
    					m_comm.Barrier();	    				
	    				m_comm.Bcast(&divisor, 1, MPI::FLOAT, brroot);
	    				m_comm.Barrier();
	    				prod=0;
	    				 
    					//for(k=i+1; k<m_ac;k++)
    					//kinitial=i+1-ncpp*m_pc;
    					//kfinal=m_ac-ncpp*m_pc;
    					
    					//if(kinitial<kfinal)
    					//{
    						//if(kinitial<0) kinitial=0;
    						
    						//count over columns
    						for(k=i+1; k<m_ac; k++)
	    					//for(k=kinitial; k<kfinal;k++)
	    					{
	    					  	//aj=k-ncpp*m_pc
	    						//if(k>=0 && k<c)
	    						//{  
	    						   	ac1=k-ncpp*m_pc;
	    						   	if(ac1>=0 && ac1 <c)
	    						   	{					  
	    					  			prod+=(m[j][ac1]*psv[k]);
	    					  			//cout << "proc " << m_iRank << " updated product " << prod<< endl;
	    						   	}
	    						//}
	    					}
    					//}	    				 
    					//m_comm.Reduce(&prod, &prodsum, 1, MPI::FLOAT, MPI::SUM, brroot);
    					m_comm.Barrier();
    					m_comm.Reduce(&prod, &prodsum, 1, MPI::FLOAT, MPI::SUM, brroot);
    					m_comm.Barrier();
    					psv[j]=(pxv[j]-prodsum)/divisor;
    					
    				
    					val=psv[j];
    					m_comm.Barrier();	    				
	    				m_comm.Bcast(&val, 1, MPI::FLOAT, brroot);
	    				m_comm.Barrier();  
    					
    				}
    				else
    				{
    					m_comm.Barrier();	    				
	    				m_comm.Bcast(&divisor, 1, MPI::FLOAT, brroot);
	    				m_comm.Barrier(); 
 	    				prod=0; 
    					kinitial=i+1-ncpp*m_pc;
    					kfinal=m_ac-ncpp*m_pc;
    					
    					//if(kinitial<kfinal)
    					//{
    						//if(kinitial<0) kinitial=0;
	    					//for(k=kinitial; k<kfinal;k++)
	    					for(k=i+1; k<m_ac; k++)
	    					{
	    					  	//aj=k-ncpp*m_pc
	    						//if(k>=0 && k<c)
	    						//{   
	    						   	ac1=k-ncpp*m_pc;
	    						   	if(ac1>=0 && ac1 <c)		    											  
	    					  			prod+=(m[j][ac1]*psv[k]);
	    						//}
	    					}
    					//}	    		    				 
    					//m_comm.Reduce(&prod, &prodsum, 1, MPI::FLOAT, MPI::SUM, brroot);
    					m_comm.Barrier();
						m_comm.Reduce(&prod, &prodsum, 1, MPI::FLOAT, MPI::SUM, brroot);
    					m_comm.Barrier();
    					//val=psv[j];
    					m_comm.Barrier();	    				
	    				m_comm.Bcast(&val, 1, MPI::FLOAT, brroot);
	    				m_comm.Barrier();
	    				psv[j]=val;
  
	    				//m_comm.Barrier();      					
    				}
    				//m_comm.Barrier(); 
    				//cout << "proc " << m_iRank << "processed col " << col << endl;
    				//m_comm.Barrier();
    				//int z;
    				/*for(z=0; z<m_NProcs; z++)
    				{
    					m_comm.Barrier();
    					if(z==m_iRank) cout << "psv "<<m_iRank << "  " <<psv[0] << "  " << psv[1] << " "<< psv[2]<< " " << psv[3]<< endl;   				
    				}*/
    				
    				 
			}//end count over rows
 				    //if(m_iRank==0)
 				    //m_comm.Barrier(); 
       				//cout << "proc " << m_iRank << "got to  final "  << endl;
       				//m_comm.Barrier(); 		
		
		//Update correct solution vector on root processor
	    //m_comm.Barrier();
            //cout << "Proc " << m_iRank << " here " << endl;
        m_comm.Barrier();
        if(m_iRank==0)
	        for(i=0; i<r; i++)
	           sv->set(i,psv[i]);
	           
		//delete [] psv;
		//delete [] pxv;	           

		
	return status;		
}
