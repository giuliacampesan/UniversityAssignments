#include <iostream>
#include "Event.h"


void dump (const Event& ev) {

std::cout
<<ev.eventNumber() 	<<std::endl 
<<ev.x() <<' ' 
<<ev.y() <<' ' 
<<ev.z() <<' ' 		<<std::endl;

int particleN = ev.nParticles();
std::cout
<<particleN  		<<std::endl;

int i;
for (i=0; i<particleN; ++i)	{
	Event::Particle* pt = ev.particle(i);
	std::cout 
	<< pt->eCharge <<' ' 
	<<pt->Mx <<' ' 
	<<pt->My <<' ' <<pt->Mz   <<std::endl;
	}

return;
}


