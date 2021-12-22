#include "EventReadFromFile.h"

#include "Event.h"

#include <iostream>
#include <fstream>
#include <string>

using namespace std;

// read data from file "name"
EventReadFromFile::EventReadFromFile( const string& name ) {
  file = new ifstream( name.c_str() );
}


EventReadFromFile::~EventReadFromFile() {
  delete file;
}


// get an event
const Event* EventReadFromFile::get() {
  return readFile();
}


// read an event
const Event* EventReadFromFile::readFile() {

int n;
float x, y, z;


if (*file >> n) {
	*file >> x  >> y  >> z ;
	Event* ev = new Event (n, x, y, z);
	
	int np ;
	*file >> np;
	for (int i=0; i<np; ++i) {
		int ch; 
		float px, py, pz;
		*file >> ch >> px >>py >> pz;
		ev->add(px, py, pz, ch);
		} 
	return ev;
	}

else return 0;


}

