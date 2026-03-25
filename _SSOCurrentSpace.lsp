(defun _SSOCurrentSpace (/ ; subroutines
                        ; half-global variables
                        ; other variables
                        WhereIam)
  (cond
    ((= 1 (getvar 'blockeditor)) ; blockeditor
     (setq WhereIam "*MODEL_SPACE")
    )
    ((and (/= 1 (getvar 'cvport)) (= 0 (getvar 'tilemode))) ; viwport
     (setq WhereIam "*MODEL_SPACE")
    )
    ((and (/= 1 (getvar 'cvport)) (= 1 (getvar 'tilemode))) ;model
     (setq WhereIam "*MODEL_SPACE")
    )
    ((= 1 (getvar 'cvport)) ;layout
     (setq WhereIam "*PAPER_SPACE")
    )
  )
)