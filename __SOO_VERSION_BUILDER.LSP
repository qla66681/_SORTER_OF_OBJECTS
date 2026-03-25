;;; SOOVEB -> Build main LSP from single files
;/; 2024-10-10 - v1.0.0 : First version (by romek)
;/; 2025-03-03 - v2.0.0 : Rewritten from scratch (by romek)

(defun c:SOOVEB (/ ;;  subroutines
                _GetDirectoryFiles _IgnoreDirectoryFiles ;;  local variable
                all dcl name_ver path)
  (defun _GetDirectoryFiles (#Path)
    ;; -1 - only folders
    ;;  0 - files and folders
    ;;  1 - only files
    (vl-directory-files #Path "*.lsp" 1)
  )
  (defun _IgnoreDirectoryFiles (#List #Ver / lst_exc)
    (if #Ver
      (setq lst_exc (list
                      "__SOO_VERSION_BUILDER.LSP"
                    )
      )
      (setq lst_exc (list
                      "__SOO_VERSION_BUILDER.LSP"
                    )
      )
    )
    (vl-remove-if
      (function
        (lambda (%)
          (member
            (strcase %)
            (mapcar 'strcase lst_exc)
          )
        )
      )
      #List
    )
  )
  (setq path ;(strcat *ADS_SESSION_CAD_UTIL_DEV_FOLDER_ROMEK*
         ;        "_"
         ;        *_SOO_LispName*
         ;        "\\"
         ;)
                 (strcat
                   (_ADS_LISP_DeveloperFolder)
                   "- CAD_UTIL DEV FOLDER\\"
                   (getvar "loginname")
                   "\\"
                   "_"
                   *_SOO_LispName*
                   "\\"
                 )
        name_ver (strcat
                   *_SOO_LispName*
                   "_"
                   *_SOO_VersionNumber*
                 )
  )
  ; ---------------------------------------------------
  ; main - no version
  ; ---------------------------------------------------
  ; read all files, omit excluded, create variable all
  (foreach %file (acad_strlsort (_IgnoreDirectoryFiles (_GetDirectoryFiles path) nil))
    (setq all (append all (cd:SYS_ReadFile nil (strcat path %file))))
  )
  ; remove *IncludeDcl* variable if exists
  (setq all (vl-remove-if
              (function
                (lambda (%)
                  (wcmatch % "(setq *_SOO_IncludeDCl*")
                )
              )
              all
            )
  )
  ; main DCL - no version
  (if *_SOO_IncludeDCl*
    (progn
      (setq dcl (cd:SYS_ReadFile
                  nil
                  (strcat path *_SOO_LispName* ".DCL")
                )
      )
      (setq dcl (cd:LST_ReplaceItem
                  1
                  dcl
                  (strcat "  label = \"..:: " ;
                  *_SOO_LispName* ;
                  " " *_SOO_VersionNumber* " ::..\";")
                )
      )
      (cd:SYS_WriteFile
        (strcat path "- VERSION\\" *_SOO_LispName* ".DCL")
        dcl
        nil
      )
      (princ (strcat "\nFile    '" *_SOO_LispName* ".DCL' build succesfully. "))
    )
  )
  ; replace _VFL_DCL_LoadDialogDEV loader with _VFL_DCL_LoadDialog
  (setq all (mapcar
              (function
                (lambda (%)
                  (if (wcmatch % "*(setq dcl (_*")
                    "  (setq dcl (_VFL_DCL_LoadDialog))"
                    %
                  )
                )
              )
              all
            )
  )
  ; write to main LSP file
  (cd:SYS_WriteFile
    (strcat path "- VERSION\\" *_SOO_LispName* ".LSP")
    all
    nil
  )
  (princ (strcat "\nFile    '" *_SOO_LispName* ".LSP' build succesfully. "))

  ; ---------------------------------------------------
  ; main - version
  ; ---------------------------------------------------
  ; read all files, omit excluded, create variable all
  (setq all nil)
  (foreach %file (acad_strlsort (_IgnoreDirectoryFiles (_GetDirectoryFiles path) T))
    (setq all (append all (cd:SYS_ReadFile nil (strcat path %file))))
  )
  ; remove *IncludeDcl* variable if exists
  (setq all (vl-remove-if
              (function
                (lambda (%)
                  (wcmatch % "(setq *_SOO_IncludeDCl*")
                )
              )
              all
            )
  )
  ; main DCL - version
  (if *_SOO_IncludeDCl*
    (progn
      (setq dcl (cd:SYS_ReadFile
                  nil
                  (strcat path *_SOO_LispName* ".DCL")
                )
      )
      (setq dcl (cd:LST_ReplaceItem
                  1
                  dcl
                  (strcat "  label = \"..:: " ;
                  *_SOO_LispName* ;
                  " " *_SOO_VersionNumber* " ::..\";")
                )
      )
      (princ "\n")
      (cd:SYS_WriteFile
        (strcat path "- VERSION\\" name_ver ".DCL")
        dcl
        nil
      )
      (princ (strcat "\nVersion '" name_ver ".DCL' build succesfully. "))
    )
  )
  ; replace _VFL_DCL_LoadDialogDEV loader with _VFL_DCL_LoadDialogVER
  ; replace _VFL_DCL_LoadDialog_Alert loader with _VFL_DCL_LoadDialogVER_Alert
  (setq all (mapcar
              '(lambda (%)
                 (cond
                   ((wcmatch % "*(setq dcl (_*")
                    "  (setq dcl (_VFL_DCL_LoadDialogVER))"
                   )
                   ((wcmatch % "*(_VFL_DCL_LoadDialog_Alert)*")
                    "    (_VFL_DCL_LoadDialogVER_Alert)"
                   )
                   (T %)
                 )
               )
              all
            )
  )
  ; write to version LSP file
  (cd:SYS_WriteFile
    (strcat path "- VERSION\\" name_ver ".LSP")
    all
    nil
  )
  (princ (strcat "\nVersion '" name_ver ".LSP' build succesfully. "))
  (princ)
)
(princ)