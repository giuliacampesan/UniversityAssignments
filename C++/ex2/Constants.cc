#include "Constants.h"

static double Constants::Pion_Mass = 0.1395706; 
static double Constants::Proton_Mass = 0.938272;
static double Constants::Kaon_Mass = 0.497611;;
static double Constants::Lambda_Mass = 1.115683;;

Constants::Constants()
{
};

Constants::~Constants()
{
};


  static double masspion()  {
	return Pion_Mass;
	}
  static double massproton()  {
	return Proton_Mass;
	 }
  static double masskaon()  {
	return Kaon_Mass;
	}
  static double masslambda()  {
	return Lambda_Mass;
	}

