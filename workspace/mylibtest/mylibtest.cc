//ex5a.cc

//Program to test the use of the 
//static library module ex5_rtputils.a


#include "../mylib/vec.h"
#include "../mylib/mat.h"



main()
{
	int status=0;
	int i=0;

	//Create 2 vectors add them and display result.
	vec v1(3, 0);
	vec v2(3, 0);
	vec v3;
	//vec v3(v1);
	
	
	for(i=0; i<3; i++)
	{
		v1.set(i, i);
		v2.set(i, 2*i);
	}
	
	v3 = v1 + v2;
	cout << "v1 is: " << v1 << endl;
	cout << "v2 is: " << v2 << endl;	
	cout << "v1+v2 is: " << v3 << endl;
	
	return status;
}
