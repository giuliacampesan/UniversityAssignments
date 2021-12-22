#ifndef LifetimeFit_h
#define LifetimeFit_h

#include <vector>

class Event;

class LifetimeFit {

 public:

  LifetimeFit( float minM, float maxM, double minT, double maxT, double minS, double maxS, double scanS  ); // mass range
  ~LifetimeFit();

  bool add( const Event& ev );      // add data from a new event
  void compute();                   // compute mean and rms

  int nEvents() const;                               // return number of accepted events
  double lifeTime();
  double lifeTimeError();

 private:

  double massMin; // min mass
  double massMax; // max mass
  
  double tRangeMin;
  double tRangeMax;
  double sRangeMin;
  double sRangeMax;
  double scanStep;

  std::vector <double> decayTimes;

  double mLifetime;
  double mLtError;
  
};

#endif

