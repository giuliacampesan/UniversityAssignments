#include "EventDump.h"
#include "Event.h"
#include "AnalysisFactory.h"

#include <iostream>
#include <math.h>
#include <stdio.h>

using namespace std;


class EventDumpFactory: public AnalysisFactory::AbsFactory {

 public:

  EventDumpFactory(): AnalysisFactory::AbsFactory( "dump" ) {}

  virtual AnalysisSteering* create ( const AnalysisInfo* info ) {
	return new EventDump( info );
  }

};



static EventDumpFactory ed;

EventDump::EventDump ( const AnalysisInfo* info ): AnalysisSteering ( info ) {
}


EventDump::~EventDump() {
}
  



// function to be called at execution start
void EventDump::beginJob() {
  return;
}


// function to be called at execution end
void EventDump::endJob() {
  return;
}


// function to be called for each event
void EventDump::update( const Event& ev ) {

std::cout
<<ev.eventNumber() 	<<std::endl 
<<ev.x() <<' ' 
<<ev.y() <<' ' 
<<ev.z() <<' ' 		<<std::endl;

int particleN = ev.nParticles();
std::cout
<<particleN  		<<std::endl;

int i;
for (i=0; i<particleN; ++i)	{
	Event::Particle* pt = ev.particle(i);
	std::cout 
	<< pt->eCharge <<' ' 
	<<pt->Mx <<' ' 
	<<pt->My <<' ' <<pt->Mz   <<std::endl;
	}

return;

}

