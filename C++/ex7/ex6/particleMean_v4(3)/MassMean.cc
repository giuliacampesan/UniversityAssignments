#include "MassMean.h"
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
void MassMean::add( const Event& ev ) {

double s = mass(ev);

//DOMANDA: PERCHÈ NON MASS(*EV) COME FACCIO PER DUMP, CHE INVECE È VOID?
  
// check for mass being in range
if ( s<minMass || s>maxMass) return;

else if ( s>minMass && s<maxMass )   {
  // update number of events and sums
	takenEvents += 1;
	massSum += s;
	massSquaresSum += s*s;
  	}
return;
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

