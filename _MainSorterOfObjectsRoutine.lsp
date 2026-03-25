(defun _MainSorterOfObjectsRoutine (#BlockNames #AnyAlertFlag /
                                    ; subroutines
                                    _MultiSSOSub
                                    ; half-global variables
                                    *GlobVarLst
                                    ; other variables
                                    Com LispName IniFile BasicFlag RegenFlag CheckEntries AllowedEntries
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
           (if (and (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1) #AnyAlertFlag)
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
                     (if (and (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1) #AnyAlertFlag)
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
                     (if (and (equal (_ADS_VARIABLES_GetGVar "`SUPPORT" "DEBUG_MODE") 1) #AnyAlertFlag)
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
  (setvar "draworderctl" 3) ; all those variables should be added to stand alone command which are using redefine or steal actions
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
  (_ADS_VARIABLES_StoreOrRestore "*GlobVarLst")
  RegenFlag
)