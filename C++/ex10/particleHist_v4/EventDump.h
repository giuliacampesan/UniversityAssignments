#ifndef EventDump_h
#define EventDump_h

#include "AnalysisSteering.h"
#include "util/include/ActiveObserver.h"

class Event;

class EventDump: public AnalysisSteering, public ActiveObserver <Event> {

 public:

  EventDump( const AnalysisInfo* info );
  virtual ~EventDump();

  // function to be called at execution start
  virtual void beginJob();
  // function to be called at execution end
  virtual void   endJob();
  // function to be called for each event
  virtual void update( const Event& ev );

 private:

  // dummy copy constructor and assignment to prevent unadvertent copy
  EventDump           ( const EventDump& x );
  EventDump& operator=( const EventDump& x );

};

#endif

