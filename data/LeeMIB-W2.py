#/usr/bin/env python

fh = open('50200.MIB')
lineas = fh.readlines() # lee todo el archivo
nlines = len(lineas)

fw = open('50200.MIB.csv','w')
fw.writelines('time,RR\n')

tiempo = 0.

for j in range(5,nlines-1):
    line = lineas[j]
    #print line + "a"
    w0 = line[0:1]
    if (w0=='Q'):
	    w1 = line[1:-2]
	    #print(w0, w1, tiempo)
	    tiempo = tiempo + float(w1)/1000.
	    wline = str(tiempo) + ',' + w1 + '\n'
	    fw.writelines(wline)
    
