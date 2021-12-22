#include <iostream>
#include <fstream>
#include <cmath>
struct Particle;
struct Event;

const Event* read (std::ifstream& file);
void dump (const Event& ev);
void clear (const Event* ev);
double mass(const Event& ev);
bool add (const Event& ev, float Mmin, float Mmax, double &Msum, double &Msquares_sum);

int main (int argc, char* argv[])
{
const char* name = argv[1];
std::ifstream file (name);

int accepted_events;
double Msum;
double Msquares_sum;
double Mmean;
double Mrms;

const Event* ev;

while ( (ev = read ( file )) != 0 )
{
if (add (*ev, 0.490 , 0.505, Msum, Msquares_sum) ) ++accepted_events;
//dump(*ev);
clear( ev );
}

Mmean = Msum / accepted_events;
double v =  Msquares_sum / accepted_events - Mmean * Mmean ;
Mrms = ( v > 0 ? sqrt(v) : 0.0);

std::cout << Mmean << ' ' <<Mrms <<std::endl;

return 0;
}



