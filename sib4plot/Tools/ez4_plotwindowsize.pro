PRO ez4_plotwindowsize, plotWidth, plotHeight

@ez4_BasicParams.com

CASE 1 OF
  (plotWindowSize EQ '512x400') : BEGIN
    plotWidth = 512 & plotHeight = 400 
  END
  (plotWindowSize EQ '640x500') : BEGIN
    plotWidth = 640 & plotHeight = 500 
  END
  (plotWindowSize EQ '768x600') : BEGIN
    plotWidth = 768 & plotHeight = 600 
  END
  (plotWindowSize EQ '896x700') : BEGIN
    plotWidth = 896 & plotHeight = 700 
  END
  (plotWindowSize EQ '1024x800') : BEGIN
    plotWidth = 1024 & plotHeight = 800 
  END
  (plotWindowSize EQ '1152x900') : BEGIN
    plotWidth = 1152 & plotHeight = 900 
  END
  (plotWindowSize EQ '1280x1000') : BEGIN
    plotWidth = 1280 & plotHeight = 1000 
  END
ENDCASE
 
END 
