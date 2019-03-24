from math import sin, asin, cos
slong = -88.24556
slat = 30.68833
pi = 3.1419
phi = 15 * round(slong / 15.0)
test = [
    [19970101,4,64,64,5,250,0,1,0,0,0,19970101,0,64,63.75,2,97.5,0,0,0,0.5625,0,'1/1/1997','12:00 AM',97001,0.00],
    [19970101,14,64,64,0,0,0,0.75,0,0,0,19970101,1,64,63.5,4,155,0,0,0,0.75,0,'1/1/1997','1:00 AM',97001,0.04],
    [19970101,226,64,64,6,30,0,1,0,0,0,19970101,8,64,64,1,86.66666412,0,0,0,0.666666687,65.62859542,'1/1/1997','8:00 AM',97001,0.33]]
row = len(test)
for counter in range(row):
    jday1 = test[counter][25]
    hcloud = test[counter][20]
    pi = 3.1419
    '''
    a = 4 * pi * (int(jday1) - 80) / 373
    a = round(a,6)
    f = sin(a)
    b = 2 * pi * int((jday1) - 8) / 355
    b = round(b,7)
    g = sin(b)
    equation = 0.17 *  f - 0.129 * g'''
    eqt = 0.17 * sin(4 * pi * (int(jday1) - 80) / 373) - 0.129 * sin(2 * pi * int((jday1) - 8) / 355)
    hour1 = round(24 * (jday1 - int(jday1)))
    hh = round(2 * pi / 24 * (hour1 - ((slong - phi) * 24 / 360) + eqt - 12.0),6)
    taud = 2 * pi * (int(jday1) - 1) / 365
    val = round(0.006918 - 0.399912 * cos(taud) + 0.070257 * sin(taud) - 0.006758 * cos(2 * taud) + 0.000907 * sin(2 * taud) - 0.002697 * cos(3 * taud) + 0.00148 * sin(3 * taud),7)
    f_sin = sin(slat * 0.01743)
    d_sin = sin(val)
    s_cos = cos(slat * 0.01743)
    d_cos = cos(val)
    h_cos = cos(hh)
    a_asin = f_sin * d_sin + s_cos * d_cos * h_cos
    ao = asin(a_asin)
    ao = ao * 180 / pi
    if ao < 0:
        phis = 0
        sro = 0
    else:
        phis = 24 * (2.044 * ao + 0.1296 * ao**2 - 1.941 * 0.001 * ao**3 + 7.591 * 0.000001 * ao**4) * 0.1314
        sro = (1 - 0.65 * (hcloud)**2) * phis
    print(sro)
    test[counter][21] = sro