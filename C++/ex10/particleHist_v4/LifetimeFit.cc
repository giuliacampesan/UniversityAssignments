#include "LifetimeFit.h"
#include "ParticleReco.h"
#include "Event.h"
#include <math.h>


double mass( const Event& ev );


LifetimeFit::LifetimeFit( float min, float max ):
 // initializations
minMass(min),
maxMass(max),
takenEvents(0)

{
}


LifetimeFit::~LifetimeFit() {
}


// add data from a new event
bool LifetimeFit::add( const Event& ev ) {

static ParticleReco* Pr = ParticleReco::instance();
float s = Pr->invariantMass();


  
// check for mass being in range
if ( s<minMass || s>maxMass) return false;

else if ( s>minMass && s<maxMass )   {
  // update number of events and sums
	takenEvents += 1;
  	}
return true;
}


// compute mean and rms
void LifetimeFit::compute() {
return;
}


// return number of selected events
int LifetimeFit::nEvents() const {
return takenEvents;
}



