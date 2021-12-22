#include <iostream>
#include "eigenvector.h"
#include "atomicDisplacement.h"

void dump (const eigenvector& ev) {

std::cout
		<<ev.vectorNumber() 			<<std::endl; 


int i;
for (i=0; i<ev.vectorSize(); ++i)	{
	
	std::cout <<i <<' ';  

	atomicDisplacement* j = ev.atDispReturn( i );
	for (int k=0; k<j->vectorSize(); k++) {

	std::cout <<j->componentReturn( k ) <<' ';

} 	
std::cout <<std::endl;	  
	}

return;
}


