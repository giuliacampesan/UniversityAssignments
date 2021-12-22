#include "ParticleMass.h"

#include "Event.h"
#include "MassMean.h"
#include "TH1F.h"
#include "TFile.h"

#include <vector>
#include <iostream>

double mass(const Event& ev);

using namespace std;

ParticleMass::ParticleMass()  {
}

ParticleMass::~ParticleMass()  {
}


void ParticleMass::beginJob() {
 
pList.reserve( 5 );
pCreate ("kaon", 0.495, 0.500) ;
pCreate( "lambda", 1.115, 1.116 );

return;
}


void ParticleMass::endJob() {

int n =pList.size();
int i;



TDirectory* currentDir = gDirectory;

TFile* file = new TFile ( "hist.root", "CREATE" );


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

void ParticleMass::process ( const Event& ev)  {

int n=pList.size();

for (int i=0; i<n; i++) {

MassMean* m = pList[i]->mMean;
bool t = m->add( ev );

double x = mass(ev);

if ( ( t = true ) ) {
	 pList[i]->histo->Fill( x );
	}

}

return;
}

void ParticleMass::pCreate ( const std::string& name, float min, float max )  {


int nBin=50;

const char* hName = name.c_str();

Particle* p = new Particle;

p->name = name;
p->mMean = new MassMean ( min, max);
p->histo = new TH1F (hName, hName, nBin, min, max);

pList.push_back( p );


return;
}













