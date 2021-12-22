#ifndef MassMean_h
#define MassMean_h

class Event;

class MassMean {

 public:

  MassMean( float min, float max ); // mass range
  ~MassMean();

  void add( const Event& ev );      // add data from a new event
  void compute();                   // compute mean and rms

  int nEvents() const;                               // return number of accepted events
 double mMean() const;                            // return mean mass
 double mRMS() const;                          // return rms  mass

 private:

  double minMass; // min mass
  double maxMass; // max mass

  int takenEvents; // number of accepted events
  double massSum; // sum of masses
  double massSquaresSum;// sum of masses square

  double massMean;// mean mass
  double massRms; // rms  mass

};

#endif

