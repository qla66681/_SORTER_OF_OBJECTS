(defun _SSOGetLayoutsBlocks (/ ; subroutines
                            ; half-global variables
                            ; other variables
                            LayBlkNames)
  (vlax-for blkContainerName (vla-get-layouts (vla-get-activedocument (vlax-get-Acad-Object)))
    (setq LayBlkNames (cons (strcase (vla-get-name (vla-get-block blkContainerName)))
                            LayBlkNames
                      )
    )
  )
  LayBlkNames
)