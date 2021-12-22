#include "EventDump.h"
#include "Event.h"
#include <iostream>
#include <math.h>
#include <stdio.h>

using namespace std;

EventDump::EventDump() {
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
void EventDump::process( const Event& ev ) {

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

