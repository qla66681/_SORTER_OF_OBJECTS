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
;/; 2026-03-25 - v3.1.1 : Undo markers and error handling have been removed from the '_MainSorterOfObjectsRoutine' routine, as it is used in other functions that have their own error handlers and undo markers (by bartosz)

(setq *_SOO_LispName* "SORTER_OF_OBJECTS")
(setq *_SOO_VersionNumber* "v3.1.1")
(setq *_SOO_IncludeDCl* nil)
(setq *_SOO_CreateLog* nil)

; =========================================================================================== ;
