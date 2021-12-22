#include <iostream>
#include "Event.h"
#include "Particle.h"

void dump (const Event& ev)
{
std::cout <<ev.evNumber <<std::endl 
<<ev.x <<' ' <<ev.y <<' ' <<ev.z <<' ' <<std::endl;
std::cout<<ev.n<<std::endl;
int i;
for (i=0; i<ev.n; ++i)
{
Particle* pt = ev.pt_array[i];
std::cout << pt->eCharge <<' ' <<pt->Mx <<' ' <<pt->My <<' ' <<pt->Mz <<std::endl;
}

return;
}


