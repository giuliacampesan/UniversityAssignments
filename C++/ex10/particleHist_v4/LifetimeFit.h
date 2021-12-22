#ifndef LifetimeFit_h
#define LifetimeFit_h

#include <string>

class Event;

class LifetimeFit {

 public:

  LifetimeFit( float min, float max ); // mass range
  ~LifetimeFit();

  bool add( const Event& ev );      // add data from a new event
  void compute();                   // compute mean and rms

  int nEvents() const;                               // return number of accepted events


 private:

  double minMass; // min mass
  double maxMass; // max mass

  int takenEvents; // number of accepted events
  
};

#endif

