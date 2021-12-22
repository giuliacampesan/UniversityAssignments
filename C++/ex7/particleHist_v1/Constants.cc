#include "Constants.h"

double Constants::Pion_Mass = 0.1395706; 
double Constants::Proton_Mass = 0.938272;
double Constants::Kaon_Mass = 0.497611;;
double Constants::Lambda_Mass = 1.115683;;

Constants::Constants()
{
};

Constants::~Constants()
{
};


  double Constants::masspion() {
	return Pion_Mass;
	}
  double Constants::massproton()  {
	return Proton_Mass;
	 }
  double Constants::masskaon()  {
	return Kaon_Mass;
	}
  double Constants::masslambda()  {
	return Lambda_Mass;
	}

