#include "atomicDisplacement.h"
#include <cmath>
#include <vector>
#include <set>

unsigned int atomicDisplacement::vSize = 3;
/*error: ‘static’ may not be used when defining (as opposed to declaring) a static data member [-fpermissive]
 static unsigned int atomicDisplacement::vSize = 3;*/

atomicDisplacement::atomicDisplacement( int n ):

	vNumber ( n ),
	vDimension ( 0 )
	{
	v = new double [vSize];
	}

atomicDisplacement::~atomicDisplacement()
	{
	delete[] v;
	}


void atomicDisplacement::add ( double component ) {
	if  ( vDimension >= vSize ) return;
	else if  ( vDimension < vSize )
 v[vDimension++] = component;
	return;
}

int atomicDisplacement::vectorNumber () const {
	return vNumber;
}

unsigned int  atomicDisplacement::vectorSize () const {
	return vDimension;
}

double atomicDisplacement::componentReturn ( unsigned int i) const {
	if ( i < vDimension ) return v[ i ];
	else if ( i >= vDimension) return -1;
}
	

unsigned int atomicDisplacement::vectorTheoreticalSize () {
	return vSize;
}

/* error: cannot declare member function ‘static unsigned int atomicDisplacement::vectorTheoreticalSize()’ to have static linkage [-fpermissive]
 ned int atomicDisplacement::vectorTheoreticalSize () {
AKA STATIC VA USATO SOLO SOLO NELLE DICHIARAZIONI .H, NON NELLE DEFINIZIONI .CC*/  

double atomicDisplacement::norma() const {
	double nm = 0; 
	for ( int i = 0; i <vSize; i++) 
		{
		nm += pow ( v[ i ], 2 );
		}
	return sqrt( nm );
}

double atomicDisplacement::prod_scalare
(const atomicDisplacement& ev) const {
	double ps = 0;
	for ( int i = 0; i < vSize; i++ )
		{
		ps += v [ i ] * ev.v [ i ];
		}
	return ps;
 }

atomicDisplacement* atomicDisplacement::matrixProduct (  double** matrice )  {

	atomicDisplacement* g;
	
	
	for (int i=0; i<3; i++) {

	double q=0;

		for (int j=0; j<3; j++) {
		q += matrice[i][j] * v[j];

		
		} g->add( q );
		
	}

return g;
}

void atomicDisplacement::changeComponent(int i, double c) {
 	v[i] = c;
}

