#include <iostream>
#include <fstream>
#include "Event.h"
#include "Particle.h"

const Event* read (std::ifstream& file)
{

int value;
if (file >> value)
{
Event* ev = new Event;
ev-> evNumber = value;
file >> ev->x >>ev->y >>ev->z ;
file >> ev->n;
int N = ev->n;
int i;


Particle** pt_array = ev->pt_array = new Particle*[N];
for (i=0; i<N; ++i)
{
Particle* pt = new Particle;
file >> pt->eCharge >>pt->Mx >>pt->My >>pt->Mz;
pt_array[i]  = pt ;
} 
return ev;
}

else return 0;
}


/* 
Particle** pt_array = new Particle*[N];
for (i=0; i<N; ++i)
{
Particle* pt = new Particle;
file >> pt->eCharge >>pt->Mx >>pt->My >>pt->Mz;
pt = pt_array[i] ;
} 
return ev;
}

else return 0;
}

mi da

giulia@giulia-Lenovo:~/c++/programma 2$ ./esercizio2.out particle.txt
2
0.450944 -0.322479 10.2081 
Errore di segmentazione (core dump creato)


cosi
Particle** pt_array = new Particle*[N];
for (i=0; i<N; ++i)
{
Particle* pt = new Particle;
pt = pt_array[i] ;
file >> pt->eCharge >>pt->Mx >>pt->My >>pt->Mz;

} 
return ev;
}

else return 0;
}

mi da
giulia@giulia-Lenovo:~/c++/programma 2$ ./esercizio2.out particle.txt
Errore di segmentazione (core dump creato) */



