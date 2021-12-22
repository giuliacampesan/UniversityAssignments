#include "Utilities.h"
#include <cmath>

Utilities::Utilities ():
{
}


Utilities::~Utilities ():
{
}

static long double Energy( float px, float py, float pz, long double mass )

{
long double s = pow (Mx, 2) + pow (My, 2) + pow (Mz, 2) + pow (Mass, 2);
return sqrt(s);
};


static long double Mass ( float px, float py, float pz, long double energy )
{
long double t = pow (Energy, 2) - (pow (Mx, 2) + pow (My, 2) + pow (Mz, 2)) ;
return sqrt(t);
};

