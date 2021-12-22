#include <iostream>

void dump (
	int evNumber,
	int n,
	float x, float y, float z,
	int *eCharges,
	float *Mx, float *My, float *Mz
	)

{
std::cout << evNumber << std::endl
          << x <<' ' <<y <<' ' <<z <<std::endl
	  <<n <<std::endl;
int i;
for (i=0; i<n; ++i)
{ 
std::cout <<eCharges[i] <<' ' 
	  <<Mx[i] <<' '
	  <<My[i] <<' ' 
	  <<Mz[i] <<std::endl; 
}

return;
}
