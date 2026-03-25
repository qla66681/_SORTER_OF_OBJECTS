(defun c:SorterOfObjectsReminder (/ *error*
                                  ; subroutines
                                  ; half-global variables
                                  ; other variables
                                  Com Pop LispName IniFile CheckEntries AllowedEntries
                                 )
  (defun *error* (Msg)
    (princ (strcat "\nError: " Msg))
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