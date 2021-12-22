#include "ParticleLifetime.h"

#include "AnalysisFramework/Event.h"
#include "AnalysisObjects/LifetimeFit.h"
#include "AnalysisFramework/AnalysisInfo.h"
#include "AnalysisFramework/AnalysisFactory.h"
#include "TH1F.h"
#include "TFile.h"
#include "AnalysisObjects/ProperTime.h"

#include <vector>
#include <iostream>
#include <string>
#include <fstream>



using namespace std;



class ParticleLifetimeFactory: public AnalysisFactory::AbsFactory {

 public:

  ParticleLifetimeFactory():AnalysisFactory::AbsFactory( "mass.root" ) {}

 virtual AnalysisSteering* create ( const AnalysisInfo* info ) {
	return new ParticleLifetime( info );
 }

};




static ParticleLifetimeFactory pm;




ParticleLifetime::ParticleLifetime( const AnalysisInfo* info): AnalysisSteering( info )  {
}

ParticleLifetime::~ParticleLifetime()  {
}


void ParticleLifetime::beginJob() {
 
pList.reserve( 5 );

ifstream file (  aInfo->value ( "massRanges" ).c_str()  );
string name; 
double mMin, mMax, tMin, tMax, sMin, sMax, step;

while ( file >> name >> mMin >> mMax >> tMin >> tMax >> sMin >> sMax >> step ) pCreate ( name, mMin, mMax, tMin, tMax, sMin, sMax, step );


return;
}


void ParticleLifetime::endJob() {

int n =pList.size();
int i;



TDirectory* currentDir = gDirectory;

TFile* file = new TFile ( aInfo->value( "mass.root" ).c_str(), "CREATE" );


for (i=0; i<n; ++i) {

LifetimeFit *m = pList[i]->ltFit;

m->compute();

cout << m->nEvents()  <<" " << m->lifeTime()
<< " " <<m->lifeTimeError() <<endl;
TH1F* h = pList[i]->histo;
h->Write();

}

file->Close();
delete file;

currentDir->cd();


return;
}

void ParticleLifetime::update ( const Event& ev)  {

int n=pList.size();

for (int i=0; i<n; i++) {

LifetimeFit* m = pList[i]->ltFit;


static ProperTime* Pt = ProperTime::instance();
float x = Pt->decayTime();

if ( ( m->add(ev)  ) ) {
	 pList[i]->histo->Fill( x );
	}

}

return;
}

void ParticleLifetime::pCreate (  const std::string& name, float minM, float maxM, double minT, double maxT, double minS, double maxS, double sStep )  {


int nBin=50;
std::string m = "time";
std::string Name = m + name;
const char* hName = Name.c_str();

Particle* p = new Particle;

p->name = name;
p->ltFit = new LifetimeFit ( minM, maxM, minT, maxT, minS, maxS, sStep );
p->histo = new TH1F ( hName, hName, nBin, minT, maxT );

pList.push_back( p );


return;
}













