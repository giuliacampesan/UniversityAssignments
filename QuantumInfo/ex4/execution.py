import subprocess as sp
import random
import shlex
import numpy as np
import os

dx=float(input('insert lattice spacing  '))
L1= float(input('insert interval left border'))
L2 = float(input('insert interval right border'))


comp = "gfortran "
exe = "Hamiltonian.out"
src  = 'Hamiltonian.f90 debugging.f90'
flag=' -llapack -lblas -ofast '



os.system(comp+ flag+ src+ ' -o '+ exe)

output=sp.Popen(['./'+exe, str(L1), str(L2), str(dx)], stdout=sp.PIPE).communicate()[0]
results = output.decode('utf-8').strip().split()
print(results)




