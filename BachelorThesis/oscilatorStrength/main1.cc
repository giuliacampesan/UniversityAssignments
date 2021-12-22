#include <iostream>
#include <fstream>
#include <ostream>
#include "eigenvector.h"
#include "atomicDisplacement.h"
#include <vector>

using namespace std;

vector <double**>  matrixRead (std::ifstream& file);
eigenvector* read (std::ifstream& file, vector <double**> matrici);

void dump (const eigenvector& ev);



int main ( int argc, char* argv[] ) {

const char* name = argv[1];
std::ifstream file (name);

const char* name1 = argv[2];
std::ifstream file1 (name1);

vector <double**> matrici = matrixRead ( file1 );
int n1 = matrici.size();
//cout <<"il vettore con le matrici di carica efficace ha " <<n1 <<" elementi " <<endl;

vector <double**> zetastar;
for (int i=0; i<48; i++) {
double** l=matrici[i];
zetastar.push_back(l);
}

int n = zetastar.size();
//cout <<"il vettore con le matrici di carica efficace ha " <<n <<" elementi " <<endl;

for (int g=0; g<n; g++) {

double** f = zetastar[g];

//cout <<g <<endl;

//for(int i = 0; i < 3; i++) { 
//for(int j = 0; j < 3; j++) { 

//cout << f[i][j] <<' ' ;

		//}
//cout <<endl;
	//}
}

eigenvector* ev;
eigenvector* eigenvectors_array[144];
int N=0;





while ( ( ev = read( file, zetastar ) ) != 0 ) {

	//dump(*ev);
	eigenvectors_array[ N++ ] = ev;
	
	
	}

//cout <<N <<endl;

/*for (int i=0; i<N; i++) {
const eigenvector* c = eigenvectors_array[i];
dump(*c);

}*/

atomicDisplacement* oscillStrength[144];
int Z=0;

for (int p=0; p<144; p++) {
 eigenvector* v = eigenvectors_array[p];
atomicDisplacement* j = new atomicDisplacement(p);
j = v->oscillatorStrength();
oscillStrength [Z++] = j;
}


ofstream oFile("OscillatorStrength1.txt");

for (int i=0; i<144; i++) {

oFile <<i <<' ';
//cout <<i <<' ';


auto c = oscillStrength[i];

for (int j=0; j<3; j++) {

oFile  << c->componentReturn(j) <<' ';
//cout << c->componentReturn(j) <<' ';

	} 

oFile <<endl;
//cout <<endl;

}

oFile.close();

ofstream oFile1("OscillatorStrengthModulo1.txt");
double oscStrModulo[144];
int G=0;
for (int i=0; i<144; i++)   {

	double s = oscillStrength[i]->norma();
	oscStrModulo[G++] = s;
	oFile1  <<s <<endl;
	//cout <<i <<' ' <<s <<endl;

}

oFile1.close();



return 0;
}




	
