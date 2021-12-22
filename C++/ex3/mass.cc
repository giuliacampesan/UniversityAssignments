#include <iostream>
#include <fstream>
#include <cmath>
#include "Event.h"
#include "Particle.h"



long double Energy(double Mx, double My, double Mz, double Mass) 
{
long double s = pow (Mx, 2) + pow (My, 2) + pow (Mz, 2) + pow (Mass, 2);
return sqrt(s);
} 


long double Mass ( double Mx, double My, double Mz, double Energy ) 
{
long double t = pow (Energy, 2) - (pow (Mx, 2) + pow (My, 2) + pow (Mz, 2)) ;
return sqrt(t);
} 

const double massPion    = 0.1395706;   // GeV/c^2
const double massProton  = 0.938272;    // GeV/c^2
const double massK0      = 0.497611;    // GeV/c^2
const double massLambda0 = 1.115683;    // GeV/c^2

double mass(const Event& ev) {

  
  typedef const Particle* part_ptr; /*la locazione puntata da ptr è 					     immutabile ma il puntatore ptr è  					     modificabile, cioè posso farlo 					     puntare ad altro indirizzo di 					     memoria.*/
  const part_ptr* particles = ev.pt_array;

  
  int i;
  int pos=0;
  int neg=0;
  long double S_Mx=0;
  long double S_My=0;
  long double S_Mz=0;
  long double Sum_energy_k0=0;
  long double Sum_energy_L0=0;


  for  (i=0; i<ev.n; ++i)
 {
  
  auto pt = particles[i];
  long double en_pion = Energy (pt->Mx, pt->My, pt->Mz, massPion);
  long double en_proton = Energy (pt->Mx, pt->My, pt->Mz, massProton);
	 
	S_Mx += pt->Mx;
	S_My += pt->My;
	S_Mz += pt->Mz;

  	if (pt->eCharge == 1 )
	{ 
	Sum_energy_k0 += en_pion;
	Sum_energy_L0 += en_proton;
	pos += 1;
	}

	else if (pt->eCharge == -1)
	{ 
	Sum_energy_k0 += en_pion;
	Sum_energy_L0 += en_pion;
	neg += 1;
	}
    
  }
  
 double massKaon;
 double massLambda;
  if ( pos != 1 || neg != 1) 
{
massKaon = -1;
massLambda = -1;
}

  if (pos == 1 && neg ==1)
{
massKaon = Mass ( S_Mx, S_My, S_Mz, Sum_energy_k0 );
massLambda = Mass ( S_Mx, S_My, S_Mz, Sum_energy_L0 );
}

 
double d_k0 = fabs ( massKaon - massK0);
double d_L0 = fabs (massLambda - massLambda0);
double d;
if ( d_k0 <= d_L0) d = massKaon;
else if (d_k0 > d_L0) d = massLambda;
return d;
}

