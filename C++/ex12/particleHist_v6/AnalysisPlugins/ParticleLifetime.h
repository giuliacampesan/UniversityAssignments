#ifndef ParticleLifetime_h
#define ParticleLifetime_h


#include "AnalysisFramework/AnalysisSteering.h"
#include "util/include/ActiveObserver.h"
#include "AnalysisObjects/LifetimeFit.h"
#include "AnalysisFramework/Event.h"
#include <vector>
#include <string>
#include "TH1F.h"
#include "TFile.h"




class Event;
class LifetimeFit;

class ParticleLifetime: public AnalysisSteering, public ActiveObserver<Event> {

public:
	ParticleLifetime( const AnalysisInfo* info);
	virtual ~ParticleLifetime();


	virtual void beginJob();
	virtual void endJob();
	virtual void update( const Event& ev );
	

private:

	ParticleLifetime ( const ParticleLifetime& x );
	ParticleLifetime& operator= ( const ParticleLifetime& x );

struct Particle {
std::string name;
LifetimeFit* ltFit;
TH1F* histo;
};

std::vector<Particle*> pList;

void pCreate ( const std::string& name, float minM, float maxM, double minT, double maxT, double minS, double maxS, double sStep );

};

#endif
