#ifndef ParticleMass_h
#define ParticleMass_h

#include "AnalysisSteering.h"
#include <vector>

class Event;
class MassMean;

class ParticleMass: public AnalysisSteering {

public:
	ParticleMass();
	virtual ~ParticleMass();


	virtual void beginJob();
	virtual void endJob();
	virtual void process( const Event& ev );

private:

	ParticleMass ( const ParticleMass& x );
	ParticleMass& operator= ( const ParticleMass& x );

std::vector<MassMean*> pList;

};

#endif
