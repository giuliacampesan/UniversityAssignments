#include "Event.h"
#include "Particle.h"

void clear (const Event* ev)
{
int i;
for (i=0; i<ev->n; ++i) 
{
delete ev->pt_array[i];
}

delete[] ev->pt_array;

delete ev;

return;
}




