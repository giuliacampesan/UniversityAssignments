#ifndef Event_h
#define Event_h

struct Particle;
struct Event 
	{
	int evNumber;
	float x;
	float y;
	float z;
	int n;
	struct Particle** pt_array;
	};
#endif
