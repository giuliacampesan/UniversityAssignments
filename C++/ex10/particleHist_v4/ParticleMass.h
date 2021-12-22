#ifndef ParticleMass_h
#define ParticleMass_h


#include "AnalysisSteering.h"
#include "util/include/ActiveObserver.h"
#include "MassMean.h"
#include "Event.h"
#include <vector>
#include <string>
#include "TH1F.h"
#include "TFile.h"




class Event;
class MassMean;

class ParticleMass: public AnalysisSteering, public ActiveObserver<Event> {

public:
	ParticleMass( const AnalysisInfo* info);
	virtual ~ParticleMass();


	virtual void beginJob();
	virtual void endJob();
	virtual void update( const Event& ev );
	

private:

	ParticleMass ( const ParticleMass& x );
	ParticleMass& operator= ( const ParticleMass& x );

struct Particle {
std::string name;
MassMean* mMean;
TH1F* histo;
};

std::vector<Particle*> pList;

void pCreate ( const std::string& name, float min, float max );

};

#endif
