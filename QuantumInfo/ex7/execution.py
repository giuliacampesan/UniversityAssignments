import subprocess as sp
import random
import shlex
import numpy as np
import os
import csv
import pandas as pd




comp = "gfortran "
exe = "ising.out"
src  = ' main.f90 ising.f90 debugging.f90 matrix.f90'
flag=' -llapack -lblas'



nMin=12
nMax=12
nStep=1

lambdaMin = 0
lambdaMax = 5
lStep = 0.1

k=4


nVector=np.arange(nMin, nMax+nStep, nStep)
lVector = np.arange(lambdaMin, lambdaMax, lStep)
kVector = np.arange(k)





os.system(comp+ flag+ src+ ' -o '+ exe)



for n in nVector:
    print(n)
    
    filename = f'outfile/eigv_N_{n}.csv'
    Rs = []
   
    for l in lVector:
        
        output=sp.Popen(['./'+exe, str(n), str(l), str(k)], stdout=sp.PIPE).communicate()[0]
        results = output.decode('utf-8').strip().split()
        Rs.append(results)

    data = np.column_stack((np.array(lVector),Rs))
    df = pd.DataFrame(data=data.astype(float))
    df.to_csv(filename, sep=' ', header=False, index=False)
        

    
    
   
    




