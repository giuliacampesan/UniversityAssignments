#include <iostream>
#include <fstream>
#include "Event.h"


const Event* read (std::ifstream& file)
{
int n;
float x, y, z;
//nparticles;

if (file >> n) {
	file >> x  >> y  >> z ;
	Event* ev = new Event (n, x, y, z);
	
	int np ;
	file >> np;
	for (int i=0; i<np; ++i) {
		int ch; 
		float px, py, pz;
		file >> ch >> px >>py >> pz;
		ev->add(px, py, pz, ch);
		} 
	return ev;
	}

else return 0;
}

