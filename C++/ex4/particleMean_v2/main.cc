#include <iostream>
#include <fstream>
#include "Event.h"
#include "MassMean.h"
#include "Constants.h"
#include "Utilities.h"

 //include header files 

//declare functions to read and dump events
void dump (const Event& ev);
//double mass(const Event& ev);
const Event* read (std::ifstream& file);

int main( int argc, char* argv[] ) {

  // open input file
  const char* name = argv[1];
  std::ifstream file (name);


  // create MassMean objects
  MassMean K0 (0.490, 0.505);
  MassMean L0 (1.114, 1.118);

  // loop over events
  const Event* ev;
  while ( ( ev = read( file ) ) != 0 ) {
  dump(*ev);
  K0.add(*ev);
  L0.add(*ev);
  delete ev;
  }

  // compute results
  K0.compute();
  L0.compute();

  // print number of selected events and results for both particles
  std::cout 	<<K0.nEvents() <<' ' 
		<<K0.mMean() <<' ' 
		<<K0.mRMS() <<std::endl;

  std::cout 	<<L0.nEvents() <<' ' 
		<<L0.mMean() <<' ' 
		<<L0.mRMS() <<std::endl;

  return 0;

}

