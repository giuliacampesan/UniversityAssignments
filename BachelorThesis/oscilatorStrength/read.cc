#include <iostream>
#include <fstream>
#include "eigenvector.h"
#include "atomicDisplacement.h"
#include <string>
#include <cmath>

using namespace std;

 eigenvector* read (std::ifstream& file, vector <double**> matrici)
{
string input;
int n;
 
double fTHz;
double fcm;




if (file >> input) {
	file >> n;
	eigenvector* ev = new eigenvector( n );
	file >> fTHz;
	file >> fcm;
	ev->readFrequence( fcm );
	for (int i=0; i<ev->vSize; ++i) {
		atomicDisplacement* aD = new atomicDisplacement(i);

		double x, y, z, d;
		file >> x >> d >>y >>d >>z >>d;
		//cout <<x << "\t"  <<y << "\t" <<z << endl;
	
		double** m = matrici[i];

		aD->add(x); 
		aD->add(y); 
		aD->add(z);

		

		
		atomicDisplacement* mP = new atomicDisplacement(i);
		mP = aD->matrixProduct (m);
		ev->add( mP );
	

		}

 
return ev;
}

else return 0;
}

