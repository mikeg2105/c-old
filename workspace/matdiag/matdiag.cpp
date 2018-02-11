

//Include deque below removes g++ 3.4.3 error caused by including
//deque from IoParams.h header
#include <deque>


#ifndef IO_MSVC
	#include "../utils/vec.h"
	#include "../utils/mat.h"
#else
	#include "..\utils\vec.h"
	#include "..\utils\mat.h"
#endif





#include <iostream>
#include <fstream>
#include <stdlib.h>






using namespace std;
//using namespace debug_channels;


int main(int argc, char *argv[])
{	
	int i;
    
    vec *v1, *v2, *v3;
    vec v4(3,0);
    float ff=3.5;
    vec vpos(2,0);

    
	matrix *mptr=new matrix(2,2,0.0);

	mptr->rowslice(0, &vpos);

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
	//pPropMan.DeleteProperties();
	//delete v4;
	//delete vtest1;

	return 0;
}
