#include "Event.h"



Event::Event( int n, float x, float y, float z ):
  evNumber(n),
  x_coordinate(x),
  y_coordinate(y),
  z_coordinate(z)

  
 
 {
  // allocate a buffer for particle pointers vector
  particles_vector.reserve( 10 );
}


Event::~Event() {
  
}


void Event::add( float px, float py, float pz, int charge ) {

  // check for the number of particles, if maximum reached do nothing
  // and return
  if ( particles_vector.size() > 10) return;
  else if ( particles_vector.size() < 10)
  {
  // create the new particle and fill with data
  part_ptr pt;
  pt->Mx = px;
  pt->My = py;
  pt->Mz = pz;
  pt->eCharge = charge;


  // store the new particle pointer in the   
  //array   and increase counter
  particles_vector.push_back ( pt );
  
		
  return;
  }
}


// get event id.
int Event::eventNumber() const {
  return evNumber;
}


// get decay point coordinates
float Event::x() const {
  return x_coordinate;
}

float Event::y() const {
  return y_coordinate;
}

float Event::z() const {
  return z_coordinate;
}

// get number of particles
unsigned int Event::nParticles() const /*questo significa che non può modificare in alcun modo l'oggetto della classe	che la esegue, sia esso stato qualificato o no a sua volta come const.*/
			{
  return particles_vector.size();
}


// get particle
Event::part_ptr Event::particle( unsigned int i ) const {
  if ( i < particles_vector.size() ) return particles_vector[i];
  return 0;
}

