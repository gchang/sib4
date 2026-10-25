; This file is included at the top of many EZPLOT procedures
; It includes global parameters in COMMON blocks.

COMMON BasicParameters_COM, ez4_dead, ez4_initialized, $
       dopsplot, defaultPrinter
COMMON Colors_COM, true_color_display, true_color_extra, $
       specNColors, specColors, $ 
       white, black, red, orange, yellow, $
       dgreen, green, dblue, blue, lblue, dindigo, lindigo, $
       violet, dpink, lpink, dgray, gray, lgray, $
       defaultColorChoice, coltabncolors, coltabstartindex, maxNColors
COMMON File_COM,  dirButton, dirButtonName, $
       dirInput, dirOutput, dirName, $
       gridInfoFile, $
       nCustomDirs, quitPrompt, sortChoice
COMMON FontGeo_COM, $
       CommentFont, MainFont, TitleFont, $
       DateFont, FileNameFont, LabelFont, MeanFont, $
       psFont, psFontSize, $
       psCommentSize, psMainSize, psTitleSize, $
       psDateSize, psFileNameSize, psLabelSize, psMeanSize, $
       defaultcbarSize, defaultcbarLabelSize, defaultSize, $
       defaultCBarFormat, defaultMeanFormat, defaultTitleLength
COMMON FontLine_COM,  $
       CommentFontL, MainFontL, TitleFontL, $
       DateFontL, FileNameFontL, LabelFontL, MeanFontL, SimFontL, $
       psFontL, psFontSizeL, $
       psCommentSizeL, psMainSizeL, psTitleSizeL, $
       psDateSizeL, psFileNameSizeL, psLabelSizeL, psMeanSizeL, $
       psSimSizeL, defaultMeanFormatL, $
       defaultTitleLengthL, defaultXYSizeL
COMMON Month_COM, monstart, monstop, monlen, monname
COMMON PasteBoard_COM, geoPasteBoard, linePasteBoard, histPasteBoard
COMMON PrefsGeo_COM, $
       defaultDateDisplay, datePosX, datePosY, dateAlign, $
       defaultFileNameDisplay, filePosX, filePosY, fileAlign, $
       defaultLabelDisplay, labelPosX, labelPosY, labelAlign, $
       defaultMeanDisplay, meanPosX, meanPosY, meanAlign, $
       dPosX1, dPosY1, dPosX2, dPosY2
COMMON PrefsLine_COM, $
       defaultDateDisplayL, datePosXL, datePosYL, dateAlignL, $
       defaultFileNameDisplayL, filePosXL, filePosYL, fileAlignL, $
       defaultMeanDisplayL, meanPosXL, meanPosYL, meanAlignL, $
       defaultSimDisplayL, simPosXL, simPosYL, simAlignL, $
       dPosX1L, dPosY1L, dPosX2L, dPosY2L, $
       sPosXL, sPosYL, lineColorSty
COMMON SiBInfo_COM, $
       nlu, luNames, $
       npft, pftNames, pftRefs, $
       ncrop, cropNames, $
       npoolcan, poolcanNames,  $
       npoolpft, poolpftNames,  $
       npoollu, poolluNames, $
       nsnow, snowNames, $
       nsoil, soilNames, $
       ntot, totNames
COMMON Missing_COM, missing
COMMON Window_COM, ezBase, ezWindowList, $
       ezWindowFormat, plotWindowSize, $
       xLeftMargin, xRightMargin, $
       yBotMargin, yTopMargin

