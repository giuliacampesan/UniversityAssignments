#include "EventSource.h"
#include "util/include/Dispatcher.h"
#include "Event.h"

EventSource::EventSource() {
}


EventSource::~EventSource() {
}


void EventSource::run () {
 const Event* ev;
 while ( ( ev = get() ) != 0 )  {
  Dispatcher<Event>::notify( *ev );


delete ev;

}

return;
}


