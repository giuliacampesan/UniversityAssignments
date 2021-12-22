#include "LifetimeFit.h"
#include "ParticleReco.h"
#include "AnalysisFramework/Event.h"
#include "AnalysisUtilities/QuadraticFitter.h"
#include <math.h>
#include "ProperTime.h"



LifetimeFit::LifetimeFit(  float minM, float maxM, double minT, double maxT, double minS, double maxS, double scanS ):



massMin ( minM ),
massMax ( maxM ),
tRangeMin ( minT ),
tRangeMax ( maxT ),
sRangeMin ( minS ),
sRangeMax ( maxS ),
scanStep ( scanS )

{

}


LifetimeFit::~LifetimeFit() {
}


// add data from a new event
bool LifetimeFit::add( const Event& ev ) {

static ParticleReco* Pr = ParticleReco::instance();
float s = Pr->invariantMass();


static ProperTime* Pt = ProperTime::instance();
float decayT = Pt->decayTime();

  
// check for mass being in range
if ( s<massMin || s>massMax ) return false;

else if ( s>massMin && s<massMax &&    decayT > tRangeMin && decayT < tRangeMax)   {
  // update number of events and sums
	decayTimes.push_back ( decayT );
  	}
return true;
}


// compute mean and rms
void LifetimeFit::compute() {


QuadraticFitter* Qf = new QuadraticFitter();
  double ts = sRangeMin;

  while ( ts <= sRangeMax ) {
	double L=0;
	int n = nEvents();
	
	for (int i=0; i<n; i++) {
		double ti = decayTimes[i];
		L += ( ti / ts  + log ( ts ) + log ( exp ( ( - tRangeMin / ts ) ) - exp ( ( - tRangeMax / ts ) ) ) );
	}

	Qf -> add ( ts, L );
 	ts += scanStep;

}

mLifetime = - Qf->b() / ( 2 * Qf->c() );
mLtError = 1 / sqrt ( 2 * Qf->c() );	


return;
}


// return number of selected events
int LifetimeFit::nEvents() const {
return decayTimes.size();
}


double LifetimeFit::lifeTime () {
  return mLifetime;
}


double LifetimeFit::lifeTimeError() {
  return mLtError;
}

