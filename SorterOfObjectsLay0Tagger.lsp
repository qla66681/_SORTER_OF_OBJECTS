(defun c:SorterOfObjectsLay0Tagger (/ *error*
                                    ; subroutines
                                    _SubTagger
                                    ; half-global variables
                                    ; other variables
                                    BlocksColl Pick InsEnt Blk BlockEditorFlag
                                   )
  (defun *error* (Msg)
    (cd:SYS_UndoEnd)
    (princ (strcat "\nError: " Msg))
  )
  (defun _SubTagger (#BlkName #SaveFlag / BlocksColl)
    (setq BlocksColl (vla-get-blocks (vla-get-activedocument (vlax-get-Acad-Object))))
    (if
      (=
        (cdr
          (car
            (cd:ACX_GetProp
              (vla-item BlocksColl
                        #BlkName
              )
              '("Comments")
            )
          )
        )
        "LAYER_0_MOVE_UP"
      )
      (progn
        (cd:ACX_SetProp
          (vla-item BlocksColl
                    #BlkName
          )
          '(("Comments" . ""))
        )
        (if (= #BlkName "*MODEL_SPACE")
          (progn

            (princ "\nLayer '0' will be sent back in future use of 'SORTEROFOBJECTS' commands in currently edited / 'MODEL_SPACE' block. ")
            (if #SaveFlag
              (vl-cmdf "_.bsave")
            )
          )
          (princ
            (strcat "\nLayer '0' will be sent back in future use of 'SORTEROFOBJECTS' commands in block '"
                    #BlkName
                    "'. "
            )
          )
        )
      )
      (progn
        (cd:ACX_SetProp
          (vla-item BlocksColl
                    #BlkName
          )
          '(("Comments" . "LAYER_0_MOVE_UP"))
        )
        (if (= #BlkName "*MODEL_SPACE")
          (progn

            (princ "\nLayer '0' will be brought to front in future use of 'SORTEROFOBJECTS' commands in currently edited / 'MODEL_SPACE' block. ")
            (if #SaveFlag
              (vl-cmdf "_.bsave")
            )
          )
          (princ
            (strcat "\nLayer '0' will be brought to front in future use of 'SORTEROFOBJECTS' commands in block '"
                    #BlkName
                    "'. "
            )
          )
        )
      )
    )
  )
  (cd:SYS_UndoBegin)
  (if (= 1 (getvar 'blockeditor))  ; blockeditor
    (setq BlockEditorFlag T)
  )
  (initget "Current Exit  ")
  (setq Pick (entsel "\nSelect the block in which layer '0' will be brought to front / sent back in future use of 'SORTEROFOBJECTS' commands, or press 'C' to select the currently edited / 'MODEL_SPACE' block. [Current/Exit] <Exit>: "))
  (cond
    ((and
       Pick
       (= (type Pick) 'LIST)
       (= (cdr (assoc 0 (entget (setq InsEnt (car Pick)))))
          "INSERT"
       )

       (setq Blk (strcase (vla-get-effectivename (vlax-ename->vla-object InsEnt))))
       (not
         (= 4
            (logand 4
                    (cdr
                      (assoc 70
                             (entget
                               (tblobjname "block" Blk)
                             )
                      )
                    )
            )
         )
       )
     )
     (_SubTagger Blk BlockEditorFlag)
    )
    ((and
       (= (type Pick) 'STR)
       (= Pick "Current")
     )
     (_SubTagger "*MODEL_SPACE" BlockEditorFlag)
    )
    ((and
       (= (type Pick) 'STR)
       (or (= Pick "Exit") (= Pick ""))
     )
     (princ "\nCommand will be terminated.")
    )
    (T
     (princ "\nWrong object selected / Nothing selected. ")
    )
  )
  (cd:SYS_UndoEnd)
  (princ)
)