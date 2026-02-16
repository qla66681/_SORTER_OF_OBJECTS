;;; SorterOfObjects -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks on all layouts and model {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsNested -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks on all layouts and model, nested blocks are also supported {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsModel -> Works exactly the same as command 'SorterOfObjectsCurrentSpace' {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsCurrentSpace -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks on current space {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsCurrentSpaceNested -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks on current space, nested blocks are also supported {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsSelectedBlocks -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks inside selected blocks {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsSelectedBlocksNested -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks inside selected blocks, nested blocks are also supported {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;;; SorterOfObjectsLay0Tagger -> Tags blocks in which layer '0' will be brought to front / sent back {#}BLOCK
;;; SorterOfObjectsReminder  -> Reminder: The pop-up includes information on layer and object sorting order. Blocks are alphabetically sorted, with the order being insignificant {#}BLOCK,REMINDER
;;; SorterOfObjectsAll -> The pop-up menu with all modes of the 'SorterOfObjects' command
;;; SorterOfObjectsBlockDefinitions -> Controls draw order of: layers, hatches, dimensions, mtexts, leaders, multileaders, revclouds, descriptive blocks inside all block definitions in drawing (model and all layouts are also included). {#}BLOCK,COMPANY_SETTINGS,DIMENSION,HATCH,LAYER,MULTILEADER,TEXT
;/; 2017-03-09 - v1.0.0 : First release (by bartosz)
;/; 2023-02-09 - v1.0.1 : Added new company: Crestmark_Millwork (by romek)
;/; 2023-03-03 - v1.0.2 : Added new companies: ABC_Stone Precision_Stone (by bartosz)
;/; 2023-03-13 - v1.0.3 : Added new company: Wittrock_NEW (by romek)
;/; 2023-07-18 - v1.0.4 : Updated DReis Furniture Layers (by khydzik)
;/; 2023-07-31 - v1.0.5 : New routine 'SorterOfObjectsModel0Top' added (by bartosz)
;/; 2023-08-25 - v1.0.6 : Added new company: Beaubois_MM (by romek)
;/; 2023-10-16 - v1.0.7 : Layer specific for dimensional constraints '*ADSK_CONSTRAINTS' exclude from selection set (by bartosz)
;/; 2023-10-16 - v1.0.8 : Any kind of mask will always be moved to the very bottom (by bartosz)
;/; 2023-10-16 - v1.0.9 : Updated DReis Furniture Layers (by khydzik)
;/; 2023-10-17 - v1.0.10 : Added new company: PH_NEW (by bartosz)
;/; 2023-10-31 - v1.0.11 : Added new company: Crestmark_Millwork_NEW_NO_ANNO (by romek)
;/; 2023-11-27 - v1.0.12 : New layers added: PH_NEW  (by bartosz)
;/; 2023-12-07 - v1.0.13 : DescriptiveBlocksFilter for Precision Stone modified (by bartosz)
;/; 2023-12-15 - v1.0.14 : Updated DReis Furniture Layers (by khydzik)
;/; 2023-12-18 - v1.0.15 : Updated DReis Furniture DescriptiveBlocksFilter list (by bartosz / karol)
;/; 2024-01-08 - v1.0.16 : Added new company: ISP_Millwork (by bartosz)
;/; 2024-01-16 - v1.0.17 : Updated PH_NEW Layers (by bartosz)
;/; 2024-04-09 - v2.0.0 : Rewriten. New Commands: SorterOfObjectsNested, SorterOfObjectsCurrentSpaceNested, SorterOfObjectsSelectedBlock, SorterOfObjectsSelectedBlockNested (by bartosz)
;/; 2024-04-15 - v2.0.1 : New Command: SorterOfObjectsLay0Tagger, commends modified: SorterOfObjectsSelectedBlocks SorterOfObjectsSelectedBlocksNested (by bartosz)
;/; 2024-04-19 - v3.0.0 : Rewritten to work with Cad_Util v3.0 (by bartosz)
;/; 2024-04-19 - v3.0.1 : New Command: SorterOfObjectsReminder (by bartosz)
;/; 2024-05-16 - v3.0.2 : Fixed object order, routine '_SSOObjectsOrder' (by bartosz)
;/; 2024-06-10 - v3.0.3 : 'SorterOfObjectsLay0Tagger' modified to support '*MODEL_SPACE' block (by bartosz)
;/; 2024-06-10 - v3.0.5 : '#AnyAlertFlag' added to routine '_MainSorterOfObjectsRoutine' required by routine '_ADS_DWS_StealIfRequired' (by bartosz)
;/; 2024-06-10 - v3.0.6 : New Command: 'SorterOfObjectsAll' (by bartosz)
;/; 2025-01-08 - v3.0.7 : New Command: 'SorterOfObjectsBlockDefinitions' (by bartosz)
;/; 2025-08-20 - v3.0.8 : Routine '_ADS_ALERT_LispWorksInBasicMode' updated. (by bartosz)
;/; 2026-02-13 - v3.0.9 : If '#AlertFlag' is nil and debug mode is active, a special version of '_ADS_Alert*' will be printed to the command line (by bartosz)
;/; 2026-02-16 - v3.1.0 : Minor correction to debug mode alerts (by bartosz)

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
(defun _MainSorterOfObjectsRoutine (#BlockNames #AnyAlertFlag / *error*
                                    ; subroutines
                                    _MultiSSOSub
                                    ; half-global variables
                                    *GlobVarLst
                                    ; other variables
                                    Com LispName IniFile BasicFlag RegenFlag CheckEntries AllowedEntries
                                   )

  (defun *error* (Msg)
    (cd:SYS_UndoEnd)
    (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
    (princ (strcat "\nError: " Msg))
  )

  (defun _MultiSSOSub (#BlockNames #IniLst #Flag #AnyAlertFlag /
                       ; subroutines
                       _SingleSSOSub
                       ; half-global variables
                       ; other variables
                       BlockNamesDyn LayOrder DesBlockNames ObjectsTypesLst BlocksColl MoveUpBlksDyn MoveUpBlks
                      )


    (defun _SingleSSOSub (#BlockName #BlksMove0Up #LayOrder #DesBlockNames #ObjectOrder /
                          ; subroutines
                          _SSOCommonObjAlert
                          ; half-global variables
                          ; other variables
                          BlocksColl Ent EntType SolPat Col62 Col420 Lay LayLst RegHatchLst MaskLst DimMaskLst
                          DimNoMaskLst MLLst LLst MTLst RCloLst InsLst
                         )

      (defun _SSOCommonObjAlert (#Debug #AnyAlertFlag #ObjType #LstType #AltText)
        (if
          (and (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
               #AnyAlertFlag
          )
          (progn
            (princ (strcat "\n" #ObjType))
            (princ
              (strcat "\n"
                      (itoa (length #LstType))
                      #AltText
              )
            )
          )
        )
      )

      (setq BlocksColl (vla-get-blocks (vla-get-activedocument (vlax-get-Acad-Object))))
      (vlax-for obj (vla-item BlocksColl #BlockName)
        (setq Ent (vlax-vla-object->ename obj))
        (setq EntType (cdr (assoc 0 (entget Ent))))
        (setq SolPat (cdr (assoc 2 (entget Ent))))
        (setq Col62 (cdr (assoc 62 (entget Ent))))
        (setq Col420 (cdr (assoc 420 (entget Ent))))
        (if (cdr (assoc 8 (entget Ent)))  ; fix region 0
          (setq Lay (strcase (cdr (assoc 8 (entget Ent)))))
        )
        ;layers
        (if (assoc Lay LayLst)
          (setq LayLst (subst
                         (append (assoc Lay LayLst) (list obj))
                         (assoc Lay LayLst)
                         LayLst
                       )
          )
          (setq LayLst (append
                         (list (list Lay obj))
                         LayLst
                       )
          )
        )
        ;layers
        ; regular hatch
        (if
          (and
            (= EntType "HATCH")
            (not
              (and
                (= SolPat "SOLID")
                (= Lay "MASK")
              )
            )
            (not
              (and
                (= SolPat "SOLID")
                Col62
                (= Col62 7)
                Col420
                (= Col420 16777215)
              )
            )
          )
          (setq RegHatchLst (cons obj RegHatchLst))
        )
        ; regular hatch
        (if
          ; Mask white
          (or
            (and
              (= EntType "HATCH")
              (= SolPat "SOLID")
              Col62
              (= Col62 7)
              Col420
              (= Col420 16777215)
            )
            ; Mask white
            ;Mask black
            (and
              (= EntType "HATCH")
              (= SolPat "SOLID")
              (= Lay "MASK")
            )
            ;Mask black
            ;wipeout
            (= EntType "WIPEOUT")
            ;wipeout
          )
          (setq MaskLst (cons obj MaskLst))
        )
        ;with mask
        (if
          (and
            (= EntType "DIMENSION")
            (not
              (= Lay "`*ADSK_CONSTRAINTS")
            )
            (= 1 (getpropertyvalue Ent "Dimtfill"))
          )
          (setq DimMaskLst (cons obj DimMaskLst))
        )
        ;with mask
        ;without mask
        (if
          (and
            (= EntType "DIMENSION")
            (not
              (= Lay "`*ADSK_CONSTRAINTS")
            )
            (= 0 (getpropertyvalue Ent "Dimtfill"))
          )
          (setq DimNoMaskLst (cons obj DimNoMaskLst))
        )
        ;without mask
        (if (= EntType "MULTILEADER")
          (setq MLLst (cons obj MLLst))
        )
        (if (= EntType "LEADER")
          (setq LLst (cons obj LLst))
        )
        (if (= EntType "MTEXT")
          (setq MTLst (cons obj MTLst))
        )
        (if
          (and (= EntType "INSERT")
               (member (strcase (vla-get-EffectiveName obj)) #DesBlockNames)
          )
          (setq InsLst (cons obj InsLst))
        )
        (if (LM:RevCloud-p Ent)
          (setq RCloLst (cons obj RCloLst))
        )
      )
      (if (and (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1) #AnyAlertFlag)
        (progn
          (princ "\n###########################################################")
          (cond
            ((or (wcmatch (strcase #BlockName) "`*MODEL_SPACE")
                 (wcmatch (strcase #BlockName) "`*PAPER_SPACE*")
             )
             (princ
               (strcat
                 "\nLAYOUT: '"
                 (strcase
                   (vla-get-name (vla-get-layout (vla-item BlocksColl #BlockName)))
                 )
                 "'"
               )
             )
            )
            ((wcmatch (strcase #BlockName) "[*]@#*")
             (princ
               (strcat "\nBLOCK NAME: '"
                       (strcase (LM:name->effectivename #BlockName))
                       " / '"
                       #BlockName
                       "'"
               )
             )
            )
            (T
             (princ (strcat "\nBLOCK NAME: '" #BlockName "'"))
            )
          )
        )
      )
      (foreach % #ObjectOrder
        (cond
          ((and (= % "HATCHES") RegHatchLst)
           (LM:MovetoBottom
             (vla-get-ActiveDocument (vlax-get-acad-object))
             RegHatchLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             RegHatchLst
             " OBJECT(S) SENT BACK. "
           )
          )
          ((and (= % "MASKS") MaskLst)
           (LM:MovetoBottom
             (vla-get-ActiveDocument (vlax-get-acad-object))
             MaskLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             MaskLst
             " OBJECT(S) SENT BACK. "
           )
          )
          ((and (= % "LAYERS") LayLst)
           (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             (princ (strcat "\n" %))
           )
           (foreach %L #LayOrder
             (if (assoc %L LayLst)
               (progn
                 (setq EntsLst (cdr (assoc %L LayLst)))
                 (if (and (member #BlockName MoveUpBlksDyn) (= "0" %L))
                   (progn
                     (LM:MovetoTop
                       (vla-get-ActiveDocument (vlax-get-acad-object))
                       EntsLst
                     )
                     (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
                       (princ
                         (strcat "\n"
                                 (itoa (length EntsLst))
                                 " OBJECT(S) on Layer - \""
                                 %L
                                 "\" BROUGHT TO FRONT. "
                         )
                       )
                     )
                   )
                   (progn
                     (LM:MovetoBottom
                       (vla-get-ActiveDocument (vlax-get-acad-object))
                       EntsLst
                     )
                     (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
                       (princ
                         (strcat "\n"
                                 (itoa (length EntsLst))
                                 " OBJECT(S) on Layer - \""
                                 %L
                                 "\" SENT BACK. "
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
          )
          ((and (= % "DIMENSIONS") (or DimNoMaskLst DimMaskLst))
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             DimNoMaskLst
           )
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             DimMaskLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             (append DimNoMaskLst DimMaskLst)
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          ((and (= % "DESCRIPTIVE BLOCKS") InsLst)
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             InsLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             InsLst
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          ((and (= % "REVISION CLOUDS") RCloLst)
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             RCloLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             RCloLst
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          ((and (= % "MULTILEADERS") MLLst)
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             MLLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             MLLst
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          ((and (= % "LEADERS") LLst)
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             LLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             LLst
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          ((and (= % "MTEXTS") MTLst)
           (LM:MovetoTop
             (vla-get-ActiveDocument (vlax-get-acad-object))
             MTLst
           )
           (_SSOCommonObjAlert
             (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
             #AnyAlertFlag
             %
             MTLst
             " OBJECT(S) BROUGHT TO FRONT. "
           )
          )
          (T)
        )
      )
    )

    (if (not #Flag)
      (progn
        (setq LayOrder (nth 0 (_SSOObjectsOrder #IniLst nil)))
        (setq DesBlockNames (nth 1 (_SSOObjectsOrder #IniLst nil)))
        (setq ObjectsTypesLst (nth 2 (_SSOObjectsOrder #IniLst nil)))
      )
      (progn
        (setq Inifile nil) ; in that way we can allow lisp work using standard sorting order
        (setq ObjectsTypesLst (nth 2 (_SSOObjectsOrder #IniLst nil)))
      )
    )
    (setq BlockNamesDyn (mapcar 'strcase
                                (apply 'append

                                       (mapcar
                                         '(lambda (obj)
                                            (cd:BLK_GetDynBlockNames obj)
                                          )
                                         #BlockNames
                                       )
                                )
                        )
    )
    (setq BlocksColl (vla-get-blocks (vla-get-activedocument (vlax-get-Acad-Object))))
    (setq MoveUpBlksDyn (mapcar 'strcase
                                (apply 'append

                                       (mapcar
                                         '(lambda (obj)
                                            (cd:BLK_GetDynBlockNames obj)
                                          )
                                         (setq MoveUpBlks (vl-remove-if
                                                            '(lambda (x)
                                                               (not
                                                                 (=
                                                                   (cdr
                                                                     (car
                                                                       (cd:ACX_GetProp
                                                                         (vla-item BlocksColl
                                                                                   x
                                                                         )
                                                                         '("Comments")
                                                                       )
                                                                     )
                                                                   )
                                                                   "LAYER_0_MOVE_UP"
                                                                 )
                                                               )
                                                             )
                                                            #BlockNames
                                                          )
                                         )
                                       )
                                )
                        )
    )
    (foreach % BlockNamesDyn
      (_SingleSSOSub % MoveUpBlksDyn LayOrder DesBlockNames ObjectsTypesLst)
    )
    (if #AnyAlertFlag
      (progn (setq LayOrder (nth 0 (_SSOObjectsOrder #IniLst T)))
             (setq DesBlockNames (nth 1 (_SSOObjectsOrder #IniLst T)))
             (setq ObjectsTypesLst (nth 2 (_SSOObjectsOrder #IniLst T)))
             (if LayOrder
               (progn
                 (princ "\n")
                 (princ
                   (cd:STR_ReParse
                     LayOrder
                     "\n"
                   )
                 )
               )
             )
             (if DesBlockNames
               (progn
                 (princ "\n")
                 (princ
                   (cd:STR_ReParse
                     DesBlockNames
                     "\n"
                   )
                 )
               )
             )
             (princ "\n")
             (princ
               (cd:STR_ReParse
                 ObjectsTypesLst
                 "\n"
               )
             )
             (if MoveUpBlks
               (progn
                 (princ "\n")
                 (princ "LAYER '0' BROUGHT TO FRONT INSIDE BLOCKS")
                 (princ "\n")
                 (princ
                   (cd:STR_ReParse
                     (mapcar 'strcase (acad_strlsort MoveUpBlks))
                     "\n"
                   )
                 )
               )
             )
             (princ "\n")
      )
    )
  )

  (setq Com (_ADS_DICTIONARY_CheckCompany))
  (setq LispName "SORTER_OF_OBJECTS")
  (setq *GlobVarLst (list "draworderctl"))
  (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
  (cd:SYS_UndoBegin)
  (setvar "draworderctl" 3)
  (cond
    ; Check if file associated with company
    ((not Com)
     (if #AnyAlertFlag
       (_ADS_ALERT_FileNotAssociatedWithCompany)
       (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
         (princ
           (strcat  ;
                   "\n>> File is not associated with any company! <<"
           )
         )
       )
     )
    )
    ; Check if config *ini file exists
    ((and
       (not (setq IniFile (_ADS_INI_CheckGVar LispName)))
       (not (setq IniFile (_ADS_INI_CreateGVar Com LispName)))
     )
     (if #AnyAlertFlag
       (_ADS_ALERT_RoutineNotSetupForCompany Com LispName)
       (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
         (princ
           (strcat  ;
                   "\n>> Routine is not setup yet for this company ("
                   Com
                   "). <<"
           )
         )
       )
     )
    )
    ; Check if allowed entries are used in INI file at 'ObjectsOrder' section
    ((and
       (setq CheckEntries (_ADS_INI_List2FlatKeyList
                            inifile
                            "ObjectsOrder"
                            "Object*"
                          )
       )
       (not
         (apply
           'and
           (mapcar
             '(lambda (%)
                (member %
                        (setq AllowedEntries (list "DIMENSIONS"
                                                   "MULTILEADERS"
                                                   "LEADERS"
                                                   "MTEXTS"
                                             )
                        )
                )
              )
             CheckEntries
           )
         )
       )
     )

     (if #AnyAlertFlag
       (alert
         (strcat "\nIncorect entries detected at 'Objects Order' Section."
                 "\nAllowed entries listed below:"
                 "\n"
                 (cd:STR_ReParse
                   AllowedEntries
                   ", "
                 )
                 "\nPlease contact Romek or Bartosz if it is required."
         )
       )
       (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
         (princ
           (strcat  ;
                   "\n>> Incorect entries detected at 'Objects Order' Section. <<" ;
           )
         )
       )
     )
    )
    ; Check if lisp works in basic mode
    ((setq BasicFlag (_ADS_INI_GetKeyValue IniFile "Props" "BasicModeFlag"))
     (if #AnyAlertFlag
       (_ADS_ALERT_LispWorksInBasicMode Com nil LispName)
       (if (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1)
         (princ
           (strcat  ;
                   "\n>> Routine for this company ("
                   Com
                   ") works only in basic mode! <<"
           )
         )
       )
     )
     (_MultiSSOSub #BlockNames IniFile BasicFlag #AnyAlertFlag)
     (setq RegenFlag T)
    )
    (T
     (_MultiSSOSub #BlockNames IniFile BasicFlag #AnyAlertFlag)
     (setq RegenFlag T)
    )
  )
  (cd:SYS_UndoEnd)
  (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
  RegenFlag
)

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
(defun c:SorterOfObjects (/ ; subroutines
                         ; half-global variables
                         ; other variables
                         Blks)
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq Blks (_SSOGetLayoutsBlocks))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acAllViewports)
      )
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)
(defun c:SorterOfObjectsNested (/ ; subroutines
                               ; half-global variables
                               ; other variables
                               MBlk MBlks PBlk PBlks Blks)
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq MBlk "*MODEL_SPACE")
      (setq MBlks (_ADS_BLOCK_ListNested MBlk 3))
      (setq PBlk "*PAPER_SPACE_ALL_LAYOUTS")
      (setq PBlks (_ADS_BLOCK_ListNested PBlk 3))
      (setq Blks (append MBlks PBlks (_SSOGetLayoutsBlocks)))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acAllViewports)
      )
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)
(defun c:SorterOfObjectsModel ()
  (c:SorterOfObjectsCurrentSpace)
)
(defun c:SorterOfObjectsCurrentSpace (/ ; subroutines
                                     ; half-global variables
                                     ; other variables
                                     Blk Blks)
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq Blk (_SSOCurrentSpace))
      (setq Blks (cons Blk Blks))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)
(defun c:SorterOfObjectsCurrentSpaceNested (/ ; subroutines
                                           ; half-global variables
                                           ; other variables
                                           Blk Blks)
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq Blk (_SSOCurrentSpace))
      (setq Blks (_ADS_BLOCK_ListNested Blk 3))
      (setq Blks (cons Blk Blks))
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)
(defun c:SorterOfObjectsSelectedBlocks (/ ; subroutines
                                       ; half-global variables
                                       ; other variables
                                       Blks Sel)
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
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )
    )
    (princ "\nWrong object selected/nothing selected. ")
  )
  (princ)
)
(defun c:SorterOfObjectsSelectedBlocksNested (/ ; subroutines
                                             ; half-global variables
                                             ; other variables
                                             Blks Sel)
  (princ "\nSelect blocks (nested blocks will be sorted too): ")
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
      (setq Blks (LM:Unique
                   (mapcar 'strcase
                           (apply 'append
                                  (mapcar
                                    '(lambda (obj)
                                       (cons obj (_ADS_BLOCK_ListNested obj 3))
                                     )
                                    Blks
                                  )
                           )
                   )
                 )
      )
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )
    )
    (princ "\nWrong object selected/nothing selected. ")
  )
  (princ)
)
(defun c:SorterOfObjectsLay0Tagger (/
                                    ; subroutines
                                    _SubTagger
                                    ; half-global variables
                                    ; other variables
                                    BlocksColl Pick InsEnt Blk BlockEditorFlag
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
  (princ)
)
(defun c:SorterOfObjectsReminder (/
                                  ; subroutines
                                  ; half-global variables
                                  ; other variables
                                  Com Pop LispName IniFile CheckEntries AllowedEntries
                                 )
  (setq Com (_ADS_DICTIONARY_CheckCompany))
  (setq LispName "SORTER_OF_OBJECTS")
  (cond
    ; Check if file associated with company
    ((not Com)
     (_ADS_ALERT_FileNotAssociatedWithCompany)
     (exit)
    )
    ; Check if config *ini file exists
    ((and
       (not (setq IniFile (_ADS_INI_CheckGVar LispName)))
       (not (setq IniFile (_ADS_INI_CreateGVar Com LispName)))
     )
     (_ADS_ALERT_RoutineNotSetupForCompany Com LispName)
     (exit)
    )
    ; Check if allowed entries are used in INI file at 'ObjectsOrder' section
    ((and
       (setq CheckEntries (_ADS_INI_List2FlatKeyList
                            inifile
                            "ObjectsOrder"
                            "Object*"
                          )
       )
       (not
         (apply
           'and
           (mapcar
             '(lambda (%)
                (member %
                        (setq AllowedEntries (list "DIMENSIONS"
                                                   "MULTILEADERS"
                                                   "LEADERS"
                                                   "MTEXTS"
                                             )
                        )
                )
              )
             CheckEntries
           )
         )
       )
     )
     (alert
       (strcat "\nIncorect entries detected at 'Objects Order' Section."
               "\nAllowed entries listed below:"
               "\n"
               (cd:STR_ReParse
                 AllowedEntries
                 ", "
               )
               "\nPlease contact Romek or Bartosz if it is required."
       )
     )
     (exit)
    )
    ; Check if lisp works in basic mode
    ((_ADS_INI_GetKeyValue IniFile "Props" "BasicModeFlag")
     (_ADS_ALERT_LispWorksInBasicMode Com nil LispName)
    )
    (T)
  )
  (setq IniFile (_ADS_INI_CheckGVar LispName))
  (setq IniFile (_ADS_INI_CreateGVar Com LispName))
  (setq FillPopUp (apply 'append (_SSOObjectsOrder IniFile T)))
  (setq Pop (dos_popupmenu
              FillPopUp
            )
  )
  (princ "\nCommand terminated.")
  (princ)
)
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

(defun c:SorterOfObjectsBlockDefinitions (/ ; subroutines
                                         ; half-global variables
                                         ; other variables
                                         Blks)
  (if (= (getvar "REFEDITNAME") "")
    (progn
      (setq Blks (cd:SYS_CollList "BLOCK" (+ 2 4 8))) ; no xref no xref dependent no anonymus
      (if (_MainSorterOfObjectsRoutine Blks T)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object))
                   acActiveViewport
        )
      )
    )
    (princ "\nCommand is not avaible when 'REFEDIT' is active. ")
  )
  (princ)
)

(_ADS_AdservcoLispLoadInfo
  (strcat "\nLSP >> '" "SORTER_OF_OBJECTS" "_" "v3.0.9" "'" " was loaded. ")
)

(princ)