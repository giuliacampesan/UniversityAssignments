#include "Event.h"
#include "EventSource.h"
#include "SourceFactory.h"
#include "AnalysisInfo.h"
#include "AnalysisSteering.h"
#include "AnalysisFactory.h"
#include "ParticleMass.h"
#include "EventDump.h"


using namespace std;

int main( int argc, char* argv[] ) {

  AnalysisInfo* info = new AnalysisInfo( argc, argv);
 
  EventSource* es = SourceFactory::create( info );


  vector<AnalysisSteering*> aList = AnalysisFactory::create( info ); 



  int l = aList.size();
  int i;

  for ( i=0; i<l; ++i) aList[i]->beginJob();

  const Event* ev;
  while ( ( ev=es->get() ) !=0 ) {
	for ( i=0; i<l; ++i ) aList[ i ]->process( *ev );
	
	delete ev;
  }


  for ( i=0; i<l; i++ ) aList[i]->endJob();

return 0;

}
