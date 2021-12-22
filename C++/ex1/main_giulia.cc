#include <iostream>
#include <fstream>

int read (
	std::ifstream& file,
	float &x, float &y, float &z,
	int *eCharges,
	float *Mx, float *My, float *Mz
	);

void dump (
	int evNumber,
	int n,
	float x, float y, float z,
	int *eCharges,
	float *Mx, float *My, float *Mz
	);

int main( int argc, char* argv[] ) {

  // open input file
  const char* name = argv[1];
  std::ifstream file( name );

int evNumber;
float x, y, z;
int n;
int eCharges[10];
float Mx[10], My[10], Mz[10];

while (file >> evNumber)
{
n = read (file, x, y, z, eCharges, Mx, My, Mz);
dump (evNumber, n, x, y, z, eCharges, Mx, My, Mz);
}


return 0;
}






