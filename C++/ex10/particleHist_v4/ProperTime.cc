#include "ProperTime.h"
#include "ParticleReco.h"
#include "Event.h"
#include "Utilities.h"
#include "Constants.h"
#include <iostream>
#include <math.h>

using namespace std;


ProperTime::ProperTime() {
}


ProperTime::~ProperTime() {
}


// recompute tag informations for new event
void ProperTime::update( const Event& ev ) {

  // set default quantities
  
  time = -1;
  Constants Ct = Constants();
  double c = Ct.lightV();
  static ParticleReco* Pr = ParticleReco::instance();
  float m = Pr->invariantMass();
  float e = Pr->totalEnergy();
  double d = sqrt (  pow ( ev.x(), 2 ) + pow ( ev.y(), 2 ) + pow ( ev.z(), 2 )  );
  float p = sqrt (  pow ( e, 2 ) - pow ( m, 2 )  );
  time = d * m / ( p * c );
  
  
  return;

}




float ProperTime::decayTime() {
  check();
  return time;
}

