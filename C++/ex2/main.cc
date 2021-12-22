#include <iostream>
#include <fstream>

struct Particle;
struct Event;

const Event* read (std::ifstream& file);
void dump (const Event& ev);
void clear (const Event* ev);

int main (int argc, char* argv[] )
{
const char* name = argv[1];
std::ifstream file (name);

const Event* ev;
while ( ( ev = read( file ) ) != 0 ) {
    dump( *ev );
    clear( ev );
  }

  return 0;

}
