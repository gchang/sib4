PRO GEO4_Struct_Window, window, $
    windowTitle=windowTitle, $
    group_leader=group_leader, undo=undo, $
    menuBar=menuBar, windowBase=windowBase, $
    fileMenu=fileMenu, editMenu=editMenu, $
    viewMenu=viewMenu, analysisMenu=analysisMenu, $
    regionMenu=regionMenu, windowMenu=windowMenu, $
    plotwindow=plotwindow, cMinField=cMinField, $
    intervalField=intervalField

IF N_ELEMENTS(windowTitle) EQ 0 THEN windowTitle=''
IF N_ELEMENTS(group_leader) EQ 0 THEN group_leader=-1

IF N_ELEMENTS(menuBar) EQ 0 THEN menuBar=-1
IF N_ELEMENTS(windowBase) EQ 0 THEN windowBase=-1
IF N_ELEMENTS(fileMenu) EQ 0 THEN fileMenu=-1
IF N_ELEMENTS(editMenu) EQ 0 THEN editMenu=-1
IF N_ELEMENTS(viewMenu) EQ 0 THEN viewMenu=-1
IF N_ELEMENTS(analysisMenu) EQ 0 THEN analysisMenu=-1
IF N_ELEMENTS(regionMenu) EQ 0 THEN regionMenu=-1
IF N_ELEMENTS(windowMenu) EQ 0 THEN windowMenu=-1

IF N_ELEMENTS(undo) EQ 0 THEN undo=-1
IF N_ELEMENTS(plotwindow) EQ 0 THEN plotwindow=-1
IF N_ELEMENTS(cMinField) EQ 0 THEN cMinField=-1
IF N_ELEMENTS(intervalField) EQ 0 THEN intervalField=-1

window = {windowTitle:windowTitle, $
          group_leader:group_leader, undo:undo, $
          menuBar:menuBar, windowBase:windowBase, $
          fileMenu:fileMenu, editMenu:editMenu, $
          viewMenu:viewMenu, analysisMenu:analysisMenu, $
          regionMenu:regionMenu, windowMenu:windowMenu,  $
          plotwindow:plotwindow, cMinField:cMinField, $
          intervalField:intervalField}

END
