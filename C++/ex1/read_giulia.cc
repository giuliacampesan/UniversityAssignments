#include <iostream>
#include <fstream>

int read (
	std::ifstream& file,
	float &x, float &y, float &z,
	int *eCharges,
	float *Mx, float *My, float *Mz
	)
{

int n;

file >> x >>y >>z;
file >>n;
int i;
for (i=0; i<n; ++i)
{
file >> eCharges[i] ;
file >> Mx[i] >>My[i] >>Mz[i];	
}
return n;
}


