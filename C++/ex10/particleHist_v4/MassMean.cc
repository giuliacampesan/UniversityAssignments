#include "MassMean.h"
#include "ParticleReco.h"
#include "Event.h"
#include <math.h>


double mass( const Event& ev );


MassMean::MassMean( float min, float max ):
 // initializations
minMass(min),
maxMass(max),
takenEvents(0),
massSum (0),
massSquaresSum (0),
massMean (0),
massRms (0)

{
}


MassMean::~MassMean() {
}


// add data from a new event
bool MassMean::add( const Event& ev ) {

static ParticleReco* Pr = ParticleReco::instance();
float s = Pr->invariantMass();


  
// check for mass being in range
if ( s<minMass || s>maxMass) return false;

else if ( s>minMass && s<maxMass )   {
  // update number of events and sums
	takenEvents += 1;
	massSum += s;
	massSquaresSum += s*s;
  	}
return true;
}


// compute mean and rms
void MassMean::compute() {

massMean = massSum / takenEvents;

double v = massSquaresSum / takenEvents - massMean * massMean;

massRms = ( v > 0 ? sqrt(v) : 0.0);

return;
}


// return number of selected events
int MassMean::nEvents() const {
return takenEvents;
}


// return mean and rms
double MassMean::mMean() const {
return massMean;
}

double MassMean::mRMS() const {
return massRms;
}

