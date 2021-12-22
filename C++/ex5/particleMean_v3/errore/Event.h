#ifndef Event_h
#define Event_h
#include <vector>

class Event {

 public:

  Event( int n, float x, float y, float z ); // create an event with number "n"
                                             // and coordinates x, y, z
  ~Event();

  // composite object Particle to hold all information for each particle
  // ( x,y,z momentum components and electric charge )
  struct Particle {
  int eCharge;
	float Mx;
	float My;
	float Mz;
  };
  typedef Particle* part_ptr;

  // add a particle to the event
  void add( float px, float py, float pz, int charge );

  // get event id.
  int eventNumber() const;
  // get decay point coordinates
  float x() const;
  float y() const;
  float z() const;
  // get number of particles
  unsigned int nParticles() const;
  // get particle
  Event::part_ptr particle( unsigned int i ) const ;

 private:

// dummy copy constructor and assignment to prevent unadvertent copy
  Event           ( const Event& x );
  Event& operator=( const Event& x );

  // event-specific informations:
  int evNumber;
  float x_coordinate;
  float y_coordinate;
  float z_coordinate;

  // particles: number and array of pointers
  
  std::vector < part_ptr > particles_vector;
  //ricorda std::
};

#endif


