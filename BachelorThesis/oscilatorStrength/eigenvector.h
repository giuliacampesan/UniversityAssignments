#ifndef eigenvector_h
#define eigenvector_h
#include <vector>

class atomicDisplacement;


class eigenvector {

public:
	static unsigned int vSize;
	eigenvector( int n );
	~eigenvector();

	void add ( atomicDisplacement* u );
	int vectorNumber() const;
	unsigned int vectorSize() const;
	atomicDisplacement* atDispReturn( unsigned int i) const;
	static unsigned int vectorTheoreticalSize();
	atomicDisplacement* oscillatorStrength();
	double norma() const;
	double prod_scalare(const eigenvector& ev) const;
	void readFrequence ( double f );
	double eigenmodeReturn() const;
	void normalization();

private:


  //shared variable to contain eigenvector size
	

	int vNumber;
	unsigned int vDimension;
	atomicDisplacement** v;
	double freq;

};
#endif
