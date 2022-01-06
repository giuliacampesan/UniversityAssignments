import subprocess as sp
import random
import shlex
import numpy as np
import os

nmin=int(input('insert minimum matrix size  '))
nmax=int(input('insert maximum matrix size  '))
step=int(input('insert step '))
#nmin=10
#nmax=20
#step=5

comp = "gfortran "
exe = "performance.out"
src  = 'matrixmultiplication.f90 debugging.f90 performance.f90'
flag=''
Nvector=np.arange(nmin, nmax, step)


frow = open("outfile/matmul_row.dat", "w")
fcol = open("outfile/matmul_col.dat", "w")
froutine = open("outfile/matmul_routine.dat", "w")

files = [frow, fcol, froutine]


#mdim = np.arange(nmin, nmax, step)
#sp.run([comp,flag, '-o', exe, src], stdout=sp.PIPE)
os.system(comp+ flag+ src+ ' -o '+ exe)
for n in Nvector:
    if (n%100==0): print(n)
    output=sp.Popen(['./'+exe, str(n)], stdout=sp.PIPE).communicate()[0]
    #output=sp.run(['./'+exe, str(n)], stdout=sp.PIPE).communicate()[0]
    results = output.decode('utf-8').strip().split()
    #print(results[6])
    for i in range(len(files)):
        files[i].write(str(n)+'\t'+results[i]+'\n')
    print(results)

    #print(results)

#chuck = sp.run(shlex.split('/full/path/to/chuck /full/path/to/inputfile.txt /full/path/for/outputfile.txt'), stdout=sp.PIPE, stderr=sp.PIPE)
frow.close()
fcol.close()
froutine.close()


