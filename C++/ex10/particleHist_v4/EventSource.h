#ifndef EventSource_h
#define EventSource_h

class Event;

class EventSource {

 public:

  EventSource();
  virtual ~EventSource();

  virtual void run();
  

 private:

  // dummy copy constructor and assignment to prevent unadvertent copy
  EventSource           ( const EventSource& x );
  EventSource& operator=( const EventSource& x );

   // get an event
  virtual const Event* get() = 0;

};

 
#endif

