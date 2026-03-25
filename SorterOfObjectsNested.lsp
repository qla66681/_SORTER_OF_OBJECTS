(defun c:SorterOfObjectsNested (/ *error* ;
                                ; subroutines
                                ; half-global variables
                                *GlobVarLst
                                ; other variables
                                MBlk MBlks PBlk PBlks Blks
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

      (setq MBlk "*MODEL_SPACE")
      (setq MBlks (_ADS_BLOCK_ListNested MBlk 3))
      (setq PBlk "*PAPER_SPACE_ALL_LAYOUTS")
      (setq PBlks (_ADS_BLOCK_ListNested PBlk 3))
      (setq Blks (append MBlks PBlks (_SSOGetLayoutsBlocks)))
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