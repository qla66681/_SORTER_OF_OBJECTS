(defun c:SorterOfObjectsAll (/ AllCommands Pop)
  (setq AllCommands (list (cons "SorterOfObjects" " - (SOO)")  ;
                          (cons "SorterOfObjectsNested" " - (SOON)") ;
                          (cons "SorterOfObjectsModel" " - (SOOM)") ;
                          (cons "SorterOfObjectsCurrentSpace" " - (SOOC)") ;
                          (cons "SorterOfObjectsCurrentSpaceNested"
                                " - (SOOCN)"
                          ) ;
                          (cons "SorterOfObjectsSelectedBlocks"
                                " - (SOOS)"
                          ) ;
                          (cons "SorterOfObjectsSelectedBlocksNested"
                                " - (SOOSN)"
                          ) ;
                          (cons "SorterOfObjectsLay0Tagger" " - (SOO0)") ;
                          (cons "SorterOfObjectsReminder" " - (SOOR)") ;
                          (cons "SorterOfObjectsBlockDefinitions" " - (SOOD)") ;
                    )
  )
  (setq Pop (dos_popupmenu
              (mapcar
                '(lambda (%)
                   (strcat (strcase (car %)) (strcase (cdr %)))
                 )
                AllCommands
              )
            )
  )
  (if Pop
    (eval
      (read
        (nth Pop
             (mapcar
               '(lambda (%)
                  (strcat "(c:" (strcase (car %)) ")")
                )
               AllCommands
             )
        )
      )
    )
    (princ "\nNothing selected. ")
  )
)