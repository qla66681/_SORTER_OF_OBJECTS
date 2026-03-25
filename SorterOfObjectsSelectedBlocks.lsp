(defun c:SorterOfObjectsSelectedBlocks (/ *error* ;
                                       ; subroutines
                                       ; half-global variables
                                        *GlobVarLst
                                       ; other variables
                                       Blks Sel)
  (defun *error* (Msg)
    (cd:SYS_UndoEnd)
    (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    (princ (strcat "\nError: " Msg))
  )
  (princ "\nSelect blocks: ")
  (if
    (and
      (setq Sel (ssget (list (cons -4 "<or") (cons 0 "INSERT") (cons -4 "or>"))))
      (setq Blks (vl-remove-if
                   '(lambda (x)
                      (= 4
                         (logand 4
                                 (cdr
                                   (assoc 70
                                          (entget
                                            (tblobjname "block"
                                                        x
                                            )
                                          )
                                   )
                                 )
                         )
                      )
                    )
                   (LM:Unique
                     (mapcar
                       '(lambda (obj)
                          (vla-get-EffectiveName
                            obj
                          )
                        )
                       (cd:SSX_Convert Sel 1)
                     )
                   )
                 )
      )
    )
    (progn
      (setq *GlobVarLst (list "draworderctl"))
      (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
      (cd:SYS_UndoBegin)
      (setvar "draworderctl" 3)

      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )

      (cd:SYS_UndoEnd)
      (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    )
    (princ "\nWrong object selected/nothing selected. ")
  )
  (princ)
)