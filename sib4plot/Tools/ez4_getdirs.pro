;------------------------------------------------------------------------------ 
;       procedure GETDIRS
;------------------------------------------------------------------------------ 
; This routine finds the directories at the current directory level. 
;------------------------------------------------------------------------------ 
 
FUNCTION EZ4_GetDirs 

retval = ['../'] 
list = FILE_SEARCH(/TEST_DIRECTORY)
retval = [retval, list]

RETURN, retval
END ; function GetDirs 
