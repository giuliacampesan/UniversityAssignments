import serial

def convert_tosigned(x, nbits):
    if x >= 2**(nbits-1):
       x -= 2**nbits
    return x

ser = serial.Serial('/dev/ttyUSB27', baudrate=115200)
ifile =  open("input_signal.txt", "r")
lines = ifile.readlines()
ifile.close()
ofile = open("output_signal.txt", "w")
for line in lines:
    c = chr(int(line))
    ser.write(c)
    d = ser.read()
    out = convert_tosigned(ord(d), 8)
    ofile.write(str(out)+'\n')
ofile.close()
