(defun c:SorterOfObjectsCurrentSpaceNested (/ *error* ;
                                            ; subroutines
                                            ; half-global variables
                                            *GlobVarLst
                                            ; other variables
                                            Blk Blks
                                           )
  (defun *error* (Msg)
    (cd:SYS_UndoEnd)
    (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    (princ (strcat "\nError: " Msg))
  )
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq *GlobVarLst (list "draworderctl"))
      (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
      (cd:SYS_UndoBegin)
      (setvar "draworderctl" 3)

      (setq Blk (_SSOCurrentSpace))
      (setq Blks (_ADS_BLOCK_ListNested Blk 3))
      (setq Blks (cons Blk Blks))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )

      (cd:SYS_UndoEnd)
      (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)