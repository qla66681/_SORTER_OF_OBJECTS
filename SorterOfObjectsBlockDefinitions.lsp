(defun c:SorterOfObjectsBlockDefinitions (/ *error* ;
                                         ; subroutines
                                         ; half-global variables
                                          *GlobVarLst
                                         ; other variables
                                         Blks)
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

      (setq Blks (cd:SYS_CollList "BLOCK" (+ 2 4 8))) ; no xref no xref dependent no anonymus
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