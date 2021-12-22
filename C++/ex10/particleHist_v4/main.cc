#include "Event.h"
#include "EventSource.h"
#include "SourceFactory.h"
#include "AnalysisInfo.h"
#include "AnalysisSteering.h"
#include "AnalysisFactory.h"
#include "ParticleLifetime.h"
#include "EventDump.h"
#include "ParticleMass.h"


using namespace std;

int main( int argc, char* argv[] ) {

  AnalysisInfo* info = new AnalysisInfo( argc, argv);
 
  EventSource* es = SourceFactory::create( info );


  vector<AnalysisSteering*> aList = AnalysisFactory::create( info ); 



  int l = aList.size();
  int i;

  for ( i=0; i<l; ++i) aList[i]->beginJob();
  
  es->run();
  
  for ( i=0; i<l; i++ ) aList[i]->endJob();

return 0;

}
