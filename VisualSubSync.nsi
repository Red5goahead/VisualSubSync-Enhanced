; ---------------------------------------------------------------------------
; VisualSubSync NSIS install script
; ---------------------------------------------------------------------------

!define NAME "VisualSubSync"
!define VERSION "0.9.22"
!define OUTFILE "Redist\VisualSubSync-${VERSION}-Setup.exe"
!define INPUT_PATH "Release\"

!define FILE1 "VisualSubSync.exe"
!define FILE2 "WavWriter.dll"
!define FILE3 "js3215R.dll"
!define FILE4 "VSSCustomVSFilter.dll"
!define FILE5 "libhunspell.dll"
!define FILE6 "VisualSubSync.map"

!define DIR_WEB "web\"
!define FILE_WEB1 "index.shtml"
!define FILE_WEB2 "details.shtml"
!define FILE_WEB3 "send_suggestion.shtml"
!define FILE_WEB4 "style.css"
!define FILE_WEB5 "details.css"
!define FILE_WEB6 "script.js"
!define FILE_WEB7 "index_wav.shtml"
!define FILE_WEB8 "index_wav_export.shtml"

!define DIR_HELP "help\"
!define FILES_HELP1 "*.html"
!define FILES_HELP2 "*.css"
!define DIR_HELP_IMAGES "help\images\"
!define FILES_HELP_IMAGE1 "*.png"

!define DIR_JSPLUGIN "jsplugin\"
!define FILES_JSPLUGIN1 "french_typo.js"
!define FILES_JSPLUGIN2 "overlapping.js"
!define FILES_JSPLUGIN3 "too_long_display_time.js"
!define FILES_JSPLUGIN4 "too_long_line.js"
!define FILES_JSPLUGIN5 "too_short_display_time.js"
!define FILES_JSPLUGIN6 "too_many_lines.js"
!define FILES_JSPLUGIN7 "readme.txt"
!define FILES_JSPLUGIN8 "scene_change.js"

!define DIR_JSPLUGIN_GENERAL "jsplugin\general\"
!define FILES_JSPLUGIN_GENERAL1 "general_plugin.js"
!define FILES_JSPLUGIN_GENERAL2 "action_quick_stats.js"

!define DIR_JSPLUGIN_COMMON "jsplugin\common\"
!define FILES_JSPLUGIN_COMMON1 "tools.js"

!define DIR_PRESETS "presets\"

!define DIR_DICT "dict\"
!define FILE_DICT1 "_about_dictionaries_.txt"


!define UNINST_NAME "VisualSubSync-uninstall.exe"

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Configuration

  ;General
  Name "${NAME}"
  OutFile "${OUTFILE}"
  Caption "${NAME} ${VERSION}"
	BrandingText " "

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${NAME}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${NAME}" ""
   

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Main (required)" SecMain
	; Make section read only as it's required
	SectionIn RO

  SetOutPath "$INSTDIR"
  
  ; Main files
  File "${INPUT_PATH}${FILE1}"
  File "${INPUT_PATH}${FILE2}"
  File "${INPUT_PATH}${FILE3}"
  File "${INPUT_PATH}${FILE4}"
  File "${INPUT_PATH}${FILE5}"
  File "${INPUT_PATH}${FILE6}"

	; Web files
  SetOutPath "$INSTDIR\${DIR_WEB}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB1}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB2}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB3}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB4}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB5}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB6}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB7}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB8}"

	; Help files
  SetOutPath "$INSTDIR\${DIR_HELP}"
  File "${INPUT_PATH}${DIR_HELP}${FILES_HELP1}"
  File "${INPUT_PATH}${DIR_HELP}${FILES_HELP2}"
  SetOutPath "$INSTDIR\${DIR_HELP_IMAGES}"
  File "${INPUT_PATH}${DIR_HELP_IMAGES}${FILES_HELP_IMAGE1}"
  
  ; Javascript plugins
  SetOutPath "$INSTDIR\${DIR_JSPLUGIN}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN1}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN2}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN3}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN4}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN5}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN6}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN7}"
  File "${INPUT_PATH}${DIR_JSPLUGIN}${FILES_JSPLUGIN8}"
  SetOutPath "$INSTDIR\${DIR_JSPLUGIN_GENERAL}"
  File "${INPUT_PATH}${DIR_JSPLUGIN_GENERAL}${FILES_JSPLUGIN_GENERAL1}"
  File "${INPUT_PATH}${DIR_JSPLUGIN_GENERAL}${FILES_JSPLUGIN_GENERAL2}"
  SetOutPath "$INSTDIR\${DIR_JSPLUGIN_COMMON}"
  File "${INPUT_PATH}${DIR_JSPLUGIN_COMMON}${FILES_JSPLUGIN_COMMON1}"
  
  ; Presets
  SetOutPath "$INSTDIR\${DIR_PRESETS}"

  ; Dictionaries
  SetOutPath "$INSTDIR\${DIR_DICT}"
  File "${INPUT_PATH}${DIR_DICT}${FILE_DICT1}"
    
  ; Store install folder
  WriteRegStr HKCU "Software\VisualSubSync" "" $INSTDIR
	  
  ; Create uninstaller
	WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME} (remove only)"
	WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" '"$INSTDIR\${UNINST_NAME}"'  
  WriteUninstaller "$INSTDIR\${UNINST_NAME}"
SectionEnd

;--------------------------------

Section "Start Menu Shortcuts" SecStartShortcut
  CreateDirectory "$SMPROGRAMS\VisualSubSync"
  CreateShortCut "$SMPROGRAMS\VisualSubSync\Uninstall.lnk" "$INSTDIR\${UNINST_NAME}" "" "$INSTDIR\${UNINST_NAME}" 0
  CreateShortCut "$SMPROGRAMS\VisualSubSync\Help.lnk" "$INSTDIR\help\index.html"
  CreateShortCut "$SMPROGRAMS\VisualSubSync\VisualSubSync.lnk" "$INSTDIR\VisualSubSync.exe" "" "$INSTDIR\VisualSubSync.exe" 0
SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecMain ${LANG_ENGLISH} "The main program."
  LangString DESC_SecStartShortcut ${LANG_ENGLISH} "Create a shortcut in the start menu."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecMain} $(DESC_SecMain)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecStartShortcut} $(DESC_SecStartShortcut)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

	; Delete installer
	Delete "$INSTDIR\${UNINST_NAME}"
	
	; Delete main files
	Delete "$INSTDIR\${FILE1}"
  Delete "$INSTDIR\${FILE2}"
  Delete "$INSTDIR\${FILE3}"
  Delete "$INSTDIR\${FILE4}"
  Delete "$INSTDIR\${FILE5}"
  Delete "$INSTDIR\${FILE6}"
  
  ; Delete web files and directory if empty
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB1}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB2}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB3}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB4}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB5}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB6}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB7}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB8}"
  RMDir "$INSTDIR\${DIR_WEB}"
  
	; Delete whole help directory
  RMDir /r "$INSTDIR\${DIR_HELP}"
  
  ; Javascript plugins  
  Delete "$INSTDIR\${DIR_JSPLUGIN_COMMON}${FILES_JSPLUGIN_COMMON1}"
  RMDir "$INSTDIR\${DIR_JSPLUGIN_COMMON}"
  Delete "$INSTDIR\${DIR_JSPLUGIN_GENERAL}${FILES_JSPLUGIN_GENERAL1}"
  Delete "$INSTDIR\${DIR_JSPLUGIN_GENERAL}${FILES_JSPLUGIN_GENERAL2}"
  RMDir "$INSTDIR\${DIR_JSPLUGIN_GENERAL}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN1}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN2}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN3}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN4}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN5}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN6}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN7}"
  Delete "$INSTDIR\${DIR_JSPLUGIN}${FILES_JSPLUGIN8}"
  RMDir "$INSTDIR\${DIR_JSPLUGIN}"
  
  ; Presets
  RMDir "$INSTDIR\${DIR_PRESETS}"

  ; Dictionaries
  Delete "$INSTDIR\${DIR_DICT}${FILE_DICT1}"
  RMDir "$INSTDIR\${DIR_DICT}"
  
  ; Delete install directory if empty
  RMDir "$INSTDIR"
  
  ; Delete shortcuts
  Delete "$SMPROGRAMS\VisualSubSync\Uninstall.lnk"
  Delete "$SMPROGRAMS\VisualSubSync\Help.lnk"
  Delete "$SMPROGRAMS\VisualSubSync\VisualSubSync.lnk"  
  RMDir "$SMPROGRAMS\VisualSubSync"

	; Delete registry keys
  DeleteRegKey /ifempty HKCU "Software\VisualSubSync"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
SectionEnd
