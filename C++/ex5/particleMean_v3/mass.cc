#include <iostream>
#include <fstream>
#include <cmath>
#include "Event.h"
#include "Constants.h"
#include "Utilities.h"


double mass(const Event& ev) {

  
unsigned int i;
int pos=0;
int neg=0;
double S_Mx=0;
double S_My=0;
double S_Mz=0;
double Sum_energy_k0=0;
double Sum_energy_L0=0;
/*unsigned int n = ev.nParticles();*/
Constants Ct = Constants();
Utilities Ut = Utilities();
for  (i=0; i<ev.nParticles(); ++i)  {
  
	auto pt = ev.particle(i);
	double en_pion = Ut.Energy (pt->Mx, 		pt->My, pt->Mz, Ct.masspion());
	double en_proton = Ut.Energy (pt->Mx, 		pt->My, pt->Mz, Ct.massproton());
	 
	S_Mx += pt->Mx;
	S_My += pt->My;
	S_Mz += pt->Mz;

  	if (pt->eCharge == 1 )	{ 
		Sum_energy_k0 += en_pion;
		Sum_energy_L0 += en_proton;
		pos += 1;
		}

	else if (pt->eCharge == -1)   { 
		Sum_energy_k0 += en_pion;
		Sum_energy_L0 += en_pion;
		neg += 1;
		}
    
  	}
  
double massKaon;
double massLambda;
  
if ( pos != 1 || neg != 1)  {
	massKaon = -1;
	massLambda = -1;
	}

if (pos == 1 && neg ==1) {
	massKaon = Ut.Mass ( S_Mx, S_My, 	S_Mz, Sum_energy_k0 );
	massLambda = Ut.Mass ( S_Mx, S_My, 	S_Mz, Sum_energy_L0 );
	}

 
double d_k0 = fabs ( massKaon - Ct.masskaon());
double d_L0 = fabs (massLambda - Ct.masslambda());
double d;

if ( d_k0 <= d_L0) d = massKaon;
else if (d_k0 > d_L0) d = massLambda;

return d;
}

