import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import mean_squared_error, mean_absolute_error

def mse(poly, graph):
    z=sum((poly-graph)**2)/len(poly)
    return z

def mae(poly, graph):
    z=sum(abs(poly-graph))/len(poly)
    return z


def read_file(file):
    lst = []

    for line in file:
        lst += [line.split()]
        x = [float(x[0]) for x in lst]
        y = [float(x[1]) for x in lst]
    return x, y

    
def plot_fit(x, y, label, file):

    plt.plot(x, y, label='by '+label)
    plt.grid(True, which="both", ls="-", color='0.9')
    plt.xlabel(r'$n_{rows}=n_{cols}$')
    plt.ylabel('CPUTIME [s]')



    z3,resz3, _, _, _ = np.polyfit(x, y, deg=3, full=True)
    p3 = np.poly1d(z3)

    z2,resz2, _, _, _ = np.polyfit(x, y, deg=2, full=True)
    p2 = np.poly1d(z2)

    z4,resz4, _, _, _ = np.polyfit(x, y, deg=4, full=True)
    p4 = np.poly1d(z4)

    
    z_list = [z2, z3, z4]
    mse_2 = mean_squared_error(p2(x), y)
    mse_3 = mean_squared_error(p3(x), y)
    mse_4 = mean_squared_error(p4(x), y)
    mae_2 = mean_absolute_error(p2(x), y)
    mae_3 = mean_absolute_error(p3(x), y)
    mae_4 = mean_absolute_error(p4(x), y)
    
    for z in z_list:
        
        file.write(str(z)+'\n')
    file.close()
    

    xp = np.linspace(0, 2000, 200)
    plt.plot(xp, p2(xp), '-', label=f'n=2, MSE={mse_2:.5f}')
    plt.plot(xp, p3(xp), '-', label=f'n=3, MSE={mse_3:.5f}')
    plt.plot(xp, p4(xp), '-', label=f'n=4, MSE={mse_4:.5f}')
    plt.legend()
    plt.title(f'polynomial fit (deg=n) for Matrix size vs CPUTIME,\n by {label} multiplication')
    plt.savefig(f'outfile/polyfitby{label}.png', dpi=300)
    plt.show()
    






file = open('outfile/matmul_col.dat')
x, ycol = read_file(file)
file = open('outfile/matmul_row.dat')
x, yrow = read_file(file)
file = open('outfile/matmul_routine.dat')
x, yF = read_file(file)


plt.plot(x, yrow, label='by rows')
plt.plot(x, ycol, label='by cols')
plt.plot(x, yF, label='Fortran subroutine')
plt.yscale('log', nonpositive='clip')
plt.legend()
plt.grid(True, which="both", ls="-", color='0.9')
plt.xlabel(r'$n_{rows}=n_{cols}$')
plt.ylabel('CPUTIME [s]')
#plt.tight_layout()
plt.savefig(f'outfile/CPUTIME.png', dpi=300)
plt.show()



plot_fit(x, ycol, 'cols', open('outfile/zcols.dat', 'w'))
plot_fit(x, yrow, 'rows', open('outfile/zrows.dat', 'w'))












