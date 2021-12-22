#include "Utilities.h"
#include <cmath>


Utilities::Utilities()
{
}



Utilities::~Utilities()
{
}

double Utilities::Energy ( float px, float py, float pz, double mass )

{
double s = pow (px, 2) + pow (py, 2) + pow (pz, 2) + pow (mass, 2);
return sqrt(s);
};


double Utilities::Mass ( float px, float py, float pz, double energy )
{
double t = pow (energy, 2) - (pow (px, 2) + pow (py, 2) + pow (pz, 2)) ;
return sqrt(t);
};

