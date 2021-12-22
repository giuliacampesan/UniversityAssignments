#include <iostream>
#include <fstream>
#include <ostream>
#include <cmath>
#include <string>
#include <vector>
#include "TString.h"
#include "TGraph.h"
#include "TF1.h"
#include "TFitResultPtr.h"
#include "TROOT.h"
#include "TMath.h"

using namespace std;

struct SumTF1 { 

   SumTF1(const std::vector<TF1 *> & flist) : fFuncList(flist) {}
   
   Double_t operator() (const Double_t * x, const Double_t *p) {
      Double_t result = 0;
      for (unsigned int i = 0; i < fFuncList.size(); ++i) 
         result += fFuncList[i]->EvalPar(x,p); 
      return result; 
   } 
   
   std::vector<TF1*> fFuncList; 
};      


Double_t Re (Double_t* x, Double_t* par)  {

return 8.6*50000* 8065.5 * 8065.5* 4* TMath::Pi() / ( 3 * par[3] * par[3] * par[3]) * 4 * TMath::Pi() / ( 3 * par[3] * par[3] * par[3]) * par[0]*par[0] * ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) / ( ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) * ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) + par[2] * par[2] * x[0] *x[0] );
} 

Double_t Re1 (Double_t* x, Double_t* par)  {

return   par[0]*par[0] * ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) / ( ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) * ( par[1]*par[1] - x[0]*x[0] + par[2] * par[2] ) + par[2] * par[2] * x[0] *x[0] ) ;
} 


Double_t Im (Double_t *x, Double_t* par )   {

return 20 * 8065.5 *  8065.5 * 0.5 * 4 * TMath::Pi() * TMath::Pi() / ( 3 *  par[3] * par[3] * par[3] ) * par[0] * par[0] / (2 * par[4] ) *
 TMath::Gaus( x[0], par[1], par[2], 0);
}


Double_t Im1 (Double_t *x, Double_t* par )   {

return par[0] * TMath::Gaus( x[0], par[1], par[2], 0);
}




int main () {

Double_t passo = 16.4970560;
Double_t EInf = 5.6;
Double_t conversion = 2 * 13.6 * 8065.5;

Double_t sigmaHartree = 0.0001;
Double_t evToHartree = 0.07717;
Double_t cm_1toHartree = 0.0000046;


Double_t sigma =8;
Double_t wEl = 1.55 *  16000;
Double_t Fel = sqrt ( EInf - 1 ) * wEl/220000;
Double_t eta= 16;
Double_t etaEl = 16;



const string name = "/home/giulia/TESI/MAPbI3/oscilatorStrength/eigenmodes.txt";
ifstream file ( name );

vector <Double_t> freqs;
int n;
Double_t freq;

vector <Double_t> freqsHartree;

Double_t freqHartree;

while ( file >> n) {
	file >> freq;
	
	freqsHartree.push_back( (freq * 0.0000046  ) );
	
	freqs.push_back( freq );
}

file.close();








const string name1 = "/home/giulia/TESI/MAPbI3/oscilatorStrength/OscillatorStrengthModulo1.txt";
ifstream file1 ( name1 );

vector <Double_t> moduli;

Double_t mod;

while ( file1 >> mod) {
	
	moduli.push_back( mod );
}

file1.close();

cout <<moduli.size() <<endl;





TCanvas *c1 = new TCanvas("c1", "Dielectric function");



vector <TF1*> Real;
vector <TF1*> Imag;


for (int i=3; i<freqs.size(); i++) {


TF1 *q =  new TF1 ("q", "Re", 0, 3300, 4); 
q->SetParameters (moduli.at(i), freqs.at(i), eta, passo);
Real.push_back( q );
//q->Draw();
q->SetNpx(10000);

}

TF1 *Fwel =  new TF1 ("Fwel", "Re", 0, 3300, 4); 
Fwel->SetParameters (Fel, wEl, etaEl, passo);
Real.push_back( Fwel );

TF1 *cost =  new TF1 ("cost", "pol0", 0, 3300); 
cost->SetParameter (0, 1);
Real.push_back( cost );



cout <<freqs.size() <<endl;

for (int i=3; i<freqs.size(); i++) {


TF1 *q =  new TF1 ("q", "Im", 0, 3300, 5); 
q->SetParameters (moduli.at(i), freqs.at(i), sigma, passo, freqs.at(i));
Imag.push_back( q );
//q->Draw();
q->SetNpx(10000);
}

//c1->Update();



TF1* ReSum = new TF1("ReSum", SumTF1(Real), 0, 3300, 0);
TF1* ImSum = new TF1("ImSum", SumTF1(Imag), 0,3300, 0);


const string name2 = "/home/giulia/TESI/MAPbI3/oscilatorStrength/reEval.txt";
ofstream file2 ( name2 );

for (int i=0; i<=3300; i++) {

	file2 <<i <<"\t" <<ReSum->Eval(i) <<endl;
	

}

const string name3 = "/home/giulia/TESI/MAPbI3/oscilatorStrength/imEval.txt";
ofstream file3 ( name3 );

for (int i=0; i<=3300; i++) {

	file3 <<i <<"\t" <<ImSum->Eval(i) <<endl;
	

}


ReSum->SetNpx(100000);
//ReSum->SetTitle("Dielectric function, real part");

ReSum->GetXaxis()->SetTitle("Energy [cm^{-1}]");
ReSum->GetYaxis()->SetTitle("#varepsilon(w)" );
ReSum->GetXaxis()->SetRangeUser(0, 282);
ReSum->GetYaxis()->SetRangeUser(-30, 60);
ReSum->SetLineWidth(1);
ReSum->SetLineStyle(1);
ReSum->Draw();

ImSum->SetNpx(100000);
//ImSum->SetTitle("Dielectric function, imaginary part");

ImSum->GetXaxis()->SetTitle("Energy [cm^{-1}]");
ImSum->GetYaxis()->SetTitle("#varepsilon(w)" );

ImSum->SetLineColor(kRed);
ImSum->SetLineWidth(1);
ImSum->SetLineStyle(2);
ImSum->GetXaxis()->SetRangeUser(0, 3300);
ImSum->GetYaxis()->SetRangeUser(-30, 60);
//c1->SetLogx();
ImSum->Draw("SAME");

auto legend = new TLegend(0.1,0.8,0.2,0.9);
//auto legend = new Tlegend();
   //legend->SetHeader("The Legend Title","C"); // option "C" allows to center the header
   legend->AddEntry("ReSum","Re#varepsilon(w)","l");
   legend->AddEntry("ImSum","Im#varepsilon(w)","l");   
   legend->SetTextSize(0.03);   
   legend->Draw("SAME");
    

const string name7 = "/home/giulia/TESI/MAPbI3/oscilatorStrength/realpart.txt";
ofstream file7 ( name7 );

for (int i=-50; i<=3300; i++) {

	file7 <<i <<"\t" <<ReSum->Eval(i) <<endl;
	

}

const string name8 = "/home/giulia/TESI/MAPbI3/oscilatorStrength/imagpart.txt";
ofstream file8 ( name8 );

for (int i=-50; i<=3300; i++) {

	file7 <<i <<"\t" <<ImSum->Eval(i) <<endl;
	

}

c1->Print("diel3300_MAPbI3_range.png");


return 0;
}
