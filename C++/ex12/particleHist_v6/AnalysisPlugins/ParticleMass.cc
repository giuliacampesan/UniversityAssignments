#include "ParticleMass.h"

#include "AnalysisFramework/Event.h"
#include "AnalysisObjects/MassMean.h"
#include "AnalysisFramework/AnalysisInfo.h"
#include "AnalysisFramework/AnalysisFactory.h"
#include "TH1F.h"
#include "TFile.h"
#include "AnalysisObjects/ParticleReco.h"

#include <vector>
#include <iostream>
#include <string>
#include <fstream>



using namespace std;



class ParticleMassFactory: public AnalysisFactory::AbsFactory {

 public:

  ParticleMassFactory():AnalysisFactory::AbsFactory( "plot" ) {}

 virtual AnalysisSteering* create ( const AnalysisInfo* info ) {
	return new ParticleMass( info );
 }

};




static ParticleMassFactory pm;




ParticleMass::ParticleMass( const AnalysisInfo* info): AnalysisSteering( info )  {
}

ParticleMass::~ParticleMass()  {
}


void ParticleMass::beginJob() {

string name;
double min, max;

ifstream file( aInfo->value( "ranges" ).c_str() );
pList.reserve( 5 );

while ( file >> name >> min   >> max ) pCreate (name, min,max );


return;
}


void ParticleMass::endJob() {

int n =pList.size();
int i;



TDirectory* currentDir = gDirectory;

TFile* file = new TFile ( aInfo->value( "plot" ).c_str(), "CREATE" );


for (i=0; i<n; ++i) {

MassMean *m = pList[i]->mMean;

m->compute();

cout << m->nEvents() <<" " << m->mMean() <<" " << m->mRMS()   <<endl;

TH1F* h = pList[i]->histo;
h->Write();

}

file->Close();
delete file;

currentDir->cd();


return;
}

void ParticleMass::update ( const Event& ev)  {

int n=pList.size();

for (int i=0; i<n; i++) {

MassMean* m = pList[i]->mMean;


static ParticleReco* Pr = ParticleReco::instance();
float x = Pr->invariantMass();

if ( ( m->add(ev)  ) ) {
	 pList[i]->histo->Fill( x );
	}

}

return;
}

void ParticleMass::pCreate ( const std::string& name, float min, float max )  {


int nBin=50;
std::string m = "mass";
std::string Name = m + name;
const char* hName = Name.c_str();

Particle* p = new Particle;

p->name = name;
p->mMean = new MassMean ( min, max);
p->histo = new TH1F (hName, hName, nBin, min, max);

pList.push_back( p );


return;
}













