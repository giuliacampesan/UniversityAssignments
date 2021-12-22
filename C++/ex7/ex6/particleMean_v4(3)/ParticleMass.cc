#include "ParticleMass.h"

#include "Event.h"
#include "MassMean.h"

#include <vector>
#include <iostream>

using namespace std;

ParticleMass::ParticleMass()  {
}

ParticleMass::~ParticleMass()  {
}


void ParticleMass::beginJob() {
 
//pList.reserve( 5 );
pList.push_back ( new MassMean (0.490, 0.505) );
pList.push_back ( new MassMean (1.114, 1.118) );

return;
}


void ParticleMass::endJob() {

int n =pList.size();
int i;

for (i=0; i<n; ++i) {

MassMean *m = pList[i];

m->compute();

cout << m->mMean() <<" " << m->mRMS() <<" " << m->nEvents() <<endl;

}

return;
}

void ParticleMass::process ( const Event& ev)  {

int n=pList.size();

for (int i=0; i<n; i++) {

MassMean* m = pList[i];
m->add( ev );

}

return;
}













