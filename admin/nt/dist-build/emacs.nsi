!include MUI2.nsh
!include LogicLib.nsh
!include x64.nsh

Outfile "emacs-${OUT_VERSION}-installer.exe"


SetCompressor /solid lzma

Var StartMenuFolder

!define MUI_WELCOMEPAGE_TITLE "Welcome to GNU Emacs ${OUT_VERSION} Setup Installer"
!define MUI_WELCOMEPAGE_TITLE_3LINES
!define MUI_WELCOMEPAGE_TEXT "GNU Emacs is the advanced, extensible, customizable, self-documenting editor and real-time display editor of a lifetime.$\r$\n\
GNU Emacs includes:$\r$\n\
- Content-aware editing modes.$\r$\n\
- Complete built-in documentation.$\r$\n\
- Full Unicode support.$\r$\n\
- Highly customizable, using Emacs Lisp code or a graphical interface.$\r$\n\
- A wide range of functionality beyond text editing.$\r$\n\
- A packaging system for downloading and installing extensions.$\r$\n\
$\r$\n\
Click Next to continue."

!define MUI_WELCOMEFINISHPAGE_BITMAP "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\emacs-wizard-banner1.bmp"
!define MUI_ICON "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\icons\hicolor\scalable\apps\emacs.ico"
!define MUI_UNICON "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\icons\hicolor\scalable\apps\emacs.ico"

!define MUI_HEADERIMAGE_RIGHT
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\emacs-wizard-banner2.bmp"

!define MUI_ABORTWARNING

!insertmacro MUI_PAGE_WELCOME

!define MUI_LICENSEPAGE_TEXT_TOP "The GNU General Public License"
!insertmacro MUI_PAGE_LICENSE "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\lisp\COPYING"

Section /o "Create Desktop Shortcut" DesktopShorcutSection
    CreateShortcut "$DESKTOP\GNU Emacs.lnk" "$INSTDIR\bin\runemacs.exe"
SectionEnd

Section /o "Add to PATH" AddToPathSection
    ReadRegStr $0 HKCU "Environment" "Path" ; Add to local user PATH
    StrCpy $0 "$INSTDIR;$0"
    WriteRegStr HKCU "Environment" "Path" '$0'
SectionEnd

!insertmacro MUI_PAGE_COMPONENTS

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
!insertmacro MUI_DESCRIPTION_TEXT ${DesktopShorcutSection} "Create a Shortcut to Desktop."
!insertmacro MUI_DESCRIPTION_TEXT ${AddToPathSection} "Add installation directory to the PATH environment variable."
!insertmacro MUI_FUNCTION_DESCRIPTION_END


!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

# Checkbox for run emacs after installation
!define MUI_FINISHPAGE_RUN "$INSTDIR\bin\runemacs.exe -mm" ; Should -mm be here? this could impress a newcomer user

# Link to help GNU
!define MUI_FINISHPAGE_LINK_LOCATION "https://www.gnu.org/help/help.html"
!define MUI_FINISHPAGE_LINK "Help GNU and Free Software Movement"

!define MUI_FINISHPAGE_TITLE "Completing GNU Emacs ${OUT_VERSION} Setup"
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

Name "GNU Emacs"

function .onInit
    StrCpy $INSTDIR "$PROGRAMFILES64\GNU Emacs"
functionend


Section

  SetOutPath $INSTDIR

  RMDir /r "$INSTDIR"
  File /r emacs-${VERSION_BRANCH}\*

  # define uninstaller name
  WriteUninstaller $INSTDIR\Uninstall.exe

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  ;Create shortcuts
  CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
  CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_END
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Emacs.lnk" "$INSTDIR\bin\runemacs.exe"
SectionEnd


# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall"

  # Always delete uninstaller first
  Delete "$INSTDIR\Uninstall.exe"

  # now delete installed directory
  RMDir /r "$INSTDIR"
  RMDir "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder

  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
SectionEnd
