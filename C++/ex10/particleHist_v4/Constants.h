#ifndef Constants_h
#define Constants_h



class Constants {

  public:

  Constants();
  ~Constants();

  static double masspion() ;
  static double massproton() ;
  static double masskaon() ;
  static double masslambda() ;
  static double lightV();

  private:
  static double Pion_Mass;
  static double Proton_Mass;
  static double Kaon_Mass;
  static double Lambda_Mass;
  static double lightVelocity;
  
};

#endif
