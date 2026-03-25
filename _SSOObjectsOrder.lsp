(defun _SSOObjectsOrder (#IniLst #AlertFlag /
                         ; subroutines
                         _SSOLayerOrder _SSODescriptiveBlocksFilter
                         ; half-global variables
                         ; other variables
                         ObjOrd ObOrdLst DesBlk
                        )
  (defun _SSOLayerOrder (#IniLst)
    (_ADS_INI_List2FlatKeyList
      #IniLst
      "LayersOrder"
      "Layer*"
    )
  )
  (defun _SSODescriptiveBlocksFilter (#IniLst)
    (_ADS_INI_List2FlatKeyList
      #IniLst
      "DescriptiveBlocks"
      "Block*"
    )
  )
  (setq ObjOrd (list "REVISION CLOUDS"))
  (if (_ADS_INI_GetKeyValue #IniLst "Props" "BasicModeFlag")
    (setq #IniLst nil)
  )
  (if
    (setq ObOrdLst (mapcar 'strcase
                           (_ADS_INI_List2FlatKeyList
                             #IniLst
                             "ObjectsOrder"
                             "Object*"
                           )
                   )
    )
    (setq ObjOrd ;(append
                 (if #AlertFlag  ; order for alert is difrent than order required to shift object
                   (append  ; those object will be bring to front
                           ObjOrd
                           (if
                             (setq DesBlk (mapcar 'strcase
                                                  (_SSODescriptiveBlocksFilter
                                                    #IniLst
                                                  )
                                          )
                             )
                             (list "DESCRIPTIVE BLOCKS")
                           )
                           ObOrdLst
                           ; object below will be sent back
                           (if
                             (setq LayOrd (mapcar 'strcase
                                                  (_SSOLayerOrder #IniLst)
                                          )
                             )
                             (list "LAYERS")
                           )
                           (list "HATCHES" "MASKS")
                   )
                   (append  ; those object will be bring to front
                           ; object below will be sent back
                           (if
                             (setq LayOrd (mapcar 'strcase
                                                  (_SSOLayerOrder #IniLst)
                                          )
                             )
                             (list "LAYERS")
                           )
                           (list "HATCHES" "MASKS")
                           (reverse ObOrdLst)
                           (if
                             (setq DesBlk (mapcar 'strcase
                                                  (_SSODescriptiveBlocksFilter
                                                    #IniLst
                                                  )
                                          )
                             )
                             (list "DESCRIPTIVE BLOCKS")
                           )
                           ObjOrd
                   )
                 ) ;)
    )
    (setq ObjOrd ;(append
                 (if #AlertFlag
                   (append  ; those object will be bring to front
                           ObjOrd
                           (if
                             (setq DesBlk (mapcar 'strcase
                                                  (_SSODescriptiveBlocksFilter
                                                    #IniLst
                                                  )
                                          )
                             )
                             (list "DESCRIPTIVE BLOCKS")
                           )
                           (list "MTEXTS" "LEADERS" "MULTILEADERS" "DIMENSIONS")
                           ; object below will be sent back
                           (if
                             (setq LayOrd (mapcar 'strcase
                                                  (_SSOLayerOrder #IniLst)
                                          )
                             )
                             (list "LAYERS")
                           )
                           (list "HATCHES"
                                 "MASKS"
                           )
                   )
                   (append  ; those object will be bring to front
                           ; object below will be sent back
                           (if
                             (setq LayOrd (mapcar 'strcase
                                                  (_SSOLayerOrder #IniLst)
                                          )
                             )
                             (list "LAYERS")
                           )
                           (list "HATCHES"
                                 "MASKS"
                           )
                           (reverse
                             (list "MTEXTS"
                                   "LEADERS"
                                   "MULTILEADERS"
                                   "DIMENSIONS"
                             )
                           )
                           (if
                             (setq DesBlk (mapcar 'strcase
                                                  (_SSODescriptiveBlocksFilter
                                                    #IniLst
                                                  )
                                          )
                             )
                             (list "DESCRIPTIVE BLOCKS")
                           )
                           ObjOrd
                   )
                 ) ;)
    )
  )
  (if #AlertFlag
    (list (if LayOrd (append (list "'LAYERS ORDER' SECTION") LayOrd (list "")))
          (if DesBlk
            (append (list "'DESCRIPTIVE BLOCKS' SECTION") DesBlk (list ""))
          )
          (if ObjOrd (append (list "'OBJECTS ORDER' SECTION") ObjOrd (list "")))
    )
    (list LayOrd
          DesBlk
          ObjOrd
    )
  )
)