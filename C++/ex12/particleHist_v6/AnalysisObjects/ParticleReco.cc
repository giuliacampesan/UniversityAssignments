#include "ParticleReco.h"
#include "AnalysisFramework/Event.h"
#include "AnalysisUtilities/Utilities.h"
#include "AnalysisUtilities/Constants.h"
#include <iostream>
#include <math.h>

using namespace std;


ParticleReco::ParticleReco() {
}


ParticleReco::~ParticleReco() {
}


// recompute tag informations for new event
void ParticleReco::update( const Event& ev ) {

  // set default quantities
  type   = unknown;
  energy = -1.0;
  mass   = -1.0;

  // code to compute total energy and invariant mass for the two
  // mass hypotheses for the decay products
  unsigned int i;
  int pos=0;
  int neg=0;
  double S_Mx=0;
  double S_My=0;
  double S_Mz=0;
  double Sum_energy_k0=0;
  double Sum_energy_L0=0;

  Constants Ct = Constants();
  Utilities Ut = Utilities();
  for  (i=0; i<ev.nParticles(); ++i)  {
  
	auto pt = ev.particle(i);
	double en_pion = Ut.Energy (pt->Mx, pt->My, pt->Mz, Ct.masspion());
	double en_proton = Ut.Energy (pt->Mx, pt->My, pt->Mz, Ct.massproton());
	 
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


  float massKaon;
  float massLambda;
  
  if ( pos != 1 || neg != 1)  {
	massKaon = -1;
	massLambda = -1;
	}

  if (pos == 1 && neg ==1) {
	massKaon = Ut.Mass ( S_Mx, S_My, S_Mz, Sum_energy_k0 );
	massLambda = Ut.Mass ( S_Mx, S_My, S_Mz, Sum_energy_L0 );
	}


  // compare invariant masses with known values and set final values
  // ( type, energy and mass )
  double d_k0 = fabs ( massKaon - Ct.masskaon());
  double d_L0 = fabs (massLambda - Ct.masslambda());


  if ( d_k0 <= d_L0) {
	mass = massKaon;
	type = K0;
	energy = Sum_energy_k0;
  }
  else if (d_k0 > d_L0) {
  	mass = massLambda;
	type = Lambda0;
	energy = Sum_energy_k0;
  }


  return;

}


float ParticleReco::totalEnergy() {
  // check for new event and return result
  check();
  return energy;
}


float ParticleReco::invariantMass() {
  // check for new event and return result
  check();
  return mass;
}


ParticleReco::ParticleType ParticleReco::particleType() {
  // check for new event and return result
  check();
  return type;
}

