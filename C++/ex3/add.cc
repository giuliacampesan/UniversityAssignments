#include <iostream>
#include "Event.h"
double mass(const Event& ev);

bool add (const Event& ev, float Mmin, float Mmax, double &Msum, double &Msquares_sum)
{
double invariant_mass = mass ( ev);
if ( invariant_mass < Mmin || invariant_mass > Mmax) return false;

else  
{
double c = invariant_mass;
Msum += c;
Msquares_sum += (c * c);
}
return true;
}

