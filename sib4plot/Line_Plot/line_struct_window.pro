PRO Line_Struct_Window, window, $
    group_leader=group_leader, $
    windowTitle=windowTitle, $
    undo=undo, diurnal=diurnal, $
    menuBar=menuBar, windowBase=windowBase, $
    fileMenu=fileMenu, editMenu=editMenu, $
    viewMenu=viewMenu, analysisMenu=analysisMenu, $
    windowMenu=windowMenu, plotwindow=plotwindow, $
    xminField=xminField, xmaxField=xmaxField, $
    yminField=yminField, ymaxField=ymaxField

IF N_ELEMENTS(group_leader) EQ 0 THEN group_leader=-1
IF N_ELEMENTS(windowTitle) EQ 0 THEN windowTitle='Line Plot'

;...specific buttons
IF N_ELEMENTS(undo) EQ 0 THEN undo=-1
IF N_ELEMENTS(diurnal) EQ 0 THEN diurnal=-1

;...menu information
IF N_ELEMENTS(menuBar) EQ 0 THEN menuBar=-1
IF N_ELEMENTS(windowBase) EQ 0 THEN windowBase=-1
IF N_ELEMENTS(fileMenu) EQ 0 THEN fileMenu=-1
IF N_ELEMENTS(editMenu) EQ 0 THEN editMenu=-1
IF N_ELEMENTS(viewMenu) EQ 0 THEN viewMenu=-1
IF N_ELEMENTS(analysisMenu) EQ 0 THEN analysisMenu=-1
IF N_ELEMENTS(windowMenu) EQ 0 THEN windowMenu=-1
IF N_ELEMENTS(plotwindow) EQ 0 THEN plotwindow=-1

;...plot-specific fields
IF N_ELEMENTS(xminField) EQ 0 THEN xminField=-1
IF N_ELEMENTS(xmaxField) EQ 0 THEN xmaxField=-1
IF N_ELEMENTS(yminField) EQ 0 THEN yminField=-1
IF N_ELEMENTS(ymaxField) EQ 0 THEN ymaxField=-1

window = {group_leader:group_leader, $
          windowTitle:windowTitle, $
          undo:undo, diurnal:diurnal, $
          menuBar:menuBar, windowBase:windowBase, $
          fileMenu:fileMenu, editMenu:editMenu, $
          viewMenu:viewMenu, analysisMenu:analysisMenu, $
          windowMenu:windowMenu, plotwindow:plotwindow, $
          xminField:xminField, xmaxField:xmaxField, $
          yminField:yminField, ymaxField:ymaxField}

END
