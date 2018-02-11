
//acml test of acml library
#include <iostream>

#include <acml.h>

using std::cout;
using std::endl;



int main(int argc, char **argv)
{
	complex r;
  	complex x[3], y[3];
  	int n = 3;
  	int i;
	
	cout << "ACML example: dot product of two complex vectors using cdotu" << endl;
	
  x[0] = compose_complex(1.0, 2.0);
  x[1] = compose_complex(2.0, 1.0);
  x[2] = compose_complex(1.0, 3.0);
  y[0] = compose_complex(3.0, 1.0);
  y[1] = compose_complex(1.0, 4.0);
  y[2] = compose_complex(1.0, 2.0);

  cout<<"Vector x: ";
  for (i = 0; i < n; i++)
    cout<<x[i].real << " "<< x[i].imag << endl;
    cout << "Vector y: ";
  for (i = 0; i < n; i++)
    cout<< y[i].real << " " << y[i].imag << endl;

  r = cdotu(n, x, 1, y, 1);

  cout << "r = x.y =("<< r.real << ", " <<  r.imag << ")" << endl;

	return 0;	
}

