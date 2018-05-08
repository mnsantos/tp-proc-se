#/usr/bin/env python

fh = open('10068.MIB')
lineas = fh.readlines() # lee todo el archivo
nlines = len(lineas)

fw = open('10068.MIB.csv','w')
fw.writelines('time,RR,beat')

tiempo = 0.

for j in range(5,nlines-1):
    line = lineas[j]
    w0 = line[0:1]
    w1 = line[1:-1]
    print(w0, w1, tiempo)
    tiempo = tiempo + float(w1)/1000.
    wline = str(tiempo) + ',' + w1 + ',' + w0 
    fw.writelines(wline)
    
