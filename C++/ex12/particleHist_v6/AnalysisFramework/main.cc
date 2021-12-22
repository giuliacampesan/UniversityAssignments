#include "Event.h"
#include "EventSource.h"
#include "SourceFactory.h"
#include "AnalysisInfo.h"
#include "AnalysisSteering.h"
#include "AnalysisFactory.h"



using namespace std;

int main( int argc, char* argv[] ) {

  AnalysisInfo* info = new AnalysisInfo( argc, argv);
 
  EventSource* es = SourceFactory::create( info );


  AnalysisFactory::create( info ); 

  Dispatcher<AnalysisInfo::AnalysisStatus>::notify( AnalysisInfo::begin );
  
  es->run();
  
  Dispatcher<AnalysisInfo::AnalysisStatus>::notify( AnalysisInfo::end );

return 0;

}
