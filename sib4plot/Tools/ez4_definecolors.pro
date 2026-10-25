PRO EZ4_DEFINECOLORS

COMMON Colors_COM

;;;Set Device
DEVICE, TRUE_COLOR=24, DECOMPOSE=0, RETAIN=2

;;;Create the color table

;----------------
; Specific Colors
nColord = 18
cwhite   = 255
cblack   = 0
cred     = [250,  0,  0]
corange  = [255,128,  0]
cdgreen  = [  0,153,  0]
cgreen   = [  0,255,  0]
cdblue   = [  0,  0,204]
cblue    = [  0,128,255]
clblue   = [102,255,255]
cdindigo = [ 75,  0,140]
clindigo = [178,102,255]
cviolet  = [238,130,238]
cdpink   = [204,  0,100]
clpink   = [255,102,180]
cdgray   = [ 64, 64, 64]
cgray    = [128,128,128]
clgray   = [200,200,200]
cyellow  = [250,250, 80]

r = [cwhite, cblack, cred[0], corange[0], $
     cdgreen[0], cdblue[0], cdindigo[0], cviolet[0], cdpink[0], $
     cgreen[0], cblue[0], clblue[0], clindigo[0],  clpink[0], $
     cdgray[0], cgray[0], clgray[0], cyellow[0]]
g = [cwhite, cblack, cred[1], corange[1], $
     cdgreen[1], cdblue[1], cdindigo[1], cviolet[1], cdpink[1], $
     cgreen[1], cblue[1], clblue[1], clindigo[1],  clpink[1], $
     cdgray[1], cgray[1], clgray[1], cyellow[1]]
b = [cwhite, cblack, cred[2], corange[2], $
     cdgreen[2], cdblue[2], cdindigo[2], cviolet[2], cdpink[2], $
     cgreen[2], cblue[2], clblue[2], clindigo[2],  clpink[2], $
     cdgray[2], cgray[2], clgray[2], cyellow[2]]
TVLCT, r, g, b
cref = nColord

;...set the indices for the specific colors
white=0 & black=1 & red=2 & orange=3 & yellow=4
dgreen=5 & green=6 & dblue=7 & blue=8 & lblue=9
dindigo=10 & lindigo=11 & violet=12 & dpink=13 & lpink=14
dgray=15 & gray=16 & lgray=17

specNColors = nColord - 1  ;not including white
specColors=[black, red, orange, yellow, $
    dgreen, green, dblue, blue, lblue, dindigo, lindigo, $
    violet, dpink, lpink, dgray, gray, lgray]

;--------------------------------------
; Load the Default Rainbow Color Table
nref0 = cref
ncolor0 = 19
r = BYTE([150,195,0,0,1,0,0,0,32,113,178,255,255,255, $
           255,255,255,255,255])
g = BYTE([0,0,0,0,125,206,255,228,246,222,252,255,227, $
           199,171,133,85,0,0])
b = BYTE([150,221,191,255,255,255,255,181,112, $
           0,0,0,0,0,0,0,70,0,146])

TVLCT, r, g, b, nref0
cref += ncolor0

; Load the reverse of the 19-color rainbow color table
nref1 = cref
ncolor1 = 19
TVLCT, REVERSE(r), REVERSE(g), REVERSE(b), nref1
cref += ncolor1

; Load 12-color scheme
ncolor2 = 12
nref2 = cref
LOADCT, 33, ncolors=ncolor2, bottom=nref2, /SILENT
cref += ncolor2

; Load the reverse of the 12-color table into the color table
ncolor3 = 12
nref3 = cref
TVLCT, reds2, greens2, blues2, /GET
r = reds2[60:71]
g = greens2[60:71]
b = blues2[60:71]
TVLCT, REVERSE(r), REVERSE(g), REVERSE(b), nref3
cref += ncolor3

; Load an 11-color blue/white/red color table for difference plots
; Blue -> White -> Red
ncolor4 = 11
nref4 = cref
r = Make_Array(ncolor4, Value=255, /Int)
g = Make_Array(ncolor4, Value=255, /Int)
b = Make_Array(ncolor4, Value=255, /Int)
r[0] = 0
b[ncolor4-1] = 0
half = ncolor4/2
factor = 255.0/float(ncolor4)

FOR i=1,half-1 DO r[i]=2*i*factor

k = half-1
For i = half+1,ncolor4-2 Do Begin
    b[i]= r[k]
    k=k-1
EndFor

For i = 0,half-1 Do g[i] = r[i]
For i = half+1,ncolor4-1 Do g[i] = b[i]

TVLCT, r, g, b, nref4
cref += ncolor4

; Load the reverse blue/white/red color table
ncolor5 = 11
nref5 = cref
TVLCT, REVERSE(r), REVERSE(g), REVERSE(b), nref5
cref += ncolor5

; Load a white/green color table for LAI
ncolor6 = 20
nref6 = cref
   r = [ 251,222,203,196,183, $
         171,158,145,132, $
         119,106,93,80,   $
         67,54,41,29,16,6,0]
   g = [ 255,231,223,207, $
         189,181,167,159, $
         143,130,117,101,89,81, $
         73,65,57,49,45,41]
   b = [248,206,179,168,148,131, $
        110,93,72,55,34,17,0,0, $
        0,0,0,0,0,0]
    TVLCT, r, g, b, nref6
    cref += ncolor6

; Load gray color table
nGray = 13    
minGray = 80
maxGray = 245
interval = FLOAT(maxGray-minGray)/(nGray-1)
gray = minGray + BYTE(INDGEN(nGray)*interval)

ncolor7 = nGray
nref7 = cref
TVLCT, gray, gray, gray, nref7
cref += ncolor7

; Load the reverse gray color table
ncolor8 = nGray
nref8 = cref

grayr = REVERSE(gray)
TVLCT, grayr, grayr, grayr, nref8
cref += ncolor8

; Set combined color table information
coltabStartIndex = [nref0,nref1,nref2,nref3, $
                    nref4,nref5,nref6,nref7,nref8]
coltabnColors = [ncolor0,ncolor1,ncolor2,ncolor3, $
                 ncolor4,ncolor5,ncolor6,ncolor7,ncolor8]
maxNColors = MAX(coltabnColors)

IF N_ELEMENTS(defaultColorChoice) EQ 0 THEN defaultColorChoice = 0

END ; (PRO EZ4_DEFINECOLORS)
