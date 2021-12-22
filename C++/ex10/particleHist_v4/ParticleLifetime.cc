#include "ParticleLifetime.h"

#include "Event.h"
#include "LifetimeFit.h"
#include "AnalysisInfo.h"
#include "AnalysisFactory.h"
#include "TH1F.h"
#include "TFile.h"
#include "ProperTime.h"

#include <vector>
#include <iostream>
#include <string>



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
pCreate ("kaon", 0.495, 0.500, 10.0, 500.0) ;
pCreate( "lambda", 1.115, 1.116, 10.0, 1000.0 );

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

cout << m->nEvents()  <<endl;

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

void ParticleLifetime::pCreate (  const std::string& name, float min, float max, float timeMin, float timeMax )  {


int nBin=50;
std::string m = "time";
std::string Name = m + name;
const char* hName = Name.c_str();

Particle* p = new Particle;

p->name = name;
p->ltFit = new LifetimeFit ( min, max );
p->histo = new TH1F ( hName, hName, nBin, timeMin, timeMax );

pList.push_back( p );


return;
}













