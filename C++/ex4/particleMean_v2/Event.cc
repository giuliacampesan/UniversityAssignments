#include "Event.h"

unsigned int Event::nMax = 10;

Event::Event( int n, float x, float y, float z ):
  evNumber(n),
  x_coordinate(x),
  y_coordinate(y),
  z_coordinate(z),

  particleN( 0 )
 
 {
  // allocate a buffer for particle pointers
  particles_array = new part_ptr[nMax];
}


Event::~Event() {
  // delete all the particle pointers
  for (unsigned int i=0; i<particleN; i++) delete particles_array[i]; 
  // delete the pointers array
  delete[] particles_array;
}


void Event::add( float px, float py, float pz, int charge ) {

  // check for the number of particles, if maximum reached do nothing
  // and return
  if ( particleN >= nMax) return;
  else if ( particleN < nMax)
  {
  // create the new particle and fill with data
  part_ptr pt = new Particle;
  pt->Mx = px;
  pt->My = py;
  pt->Mz = pz;
  pt->eCharge = charge;


  // store the new particle pointer in the   
  //array   and increase counter
  particles_array [particleN++] = pt;
  
		
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
  return particleN;
}


// get particle
Event::part_ptr Event::particle( unsigned int i ) const {
  if ( i < particleN ) return particles_array[i];
  else if (i >= particleN) return 0;
}

