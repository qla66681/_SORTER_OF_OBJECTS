(defun c:SorterOfObjects (/ *error* ;
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

      (setq Blks (_SSOGetLayoutsBlocks))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acAllViewports)
      )

      (cd:SYS_UndoEnd)
      (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)