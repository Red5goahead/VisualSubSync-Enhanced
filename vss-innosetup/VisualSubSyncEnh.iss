; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "VisualSubSync Enhanced"
#define MyAppDefaultDir "VisualSubSyncEnh"
#define MyAppVersion "1.2.12.0"
#define MyAppPublisher "VisualSubSync Team"
#define MyAppURL "https://red5goahead.github.io/VisualSubSync-Enhanced/"
#define MyAppExeName "VisualSubSyncEnh.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{CA4574E3-AD52-47FA-9D38-DCB6319A5A48}
AppName={#MyAppName}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppDefaultDir}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputDir=vss-innosetup\output
OutputBaseFilename={#MyAppDefaultDir}-{#MyAppVersion}-Installer
Compression=lzma/Max
SolidCompression=true
ShowLanguageDialog=auto
UninstallDisplayIcon={app}\VisualSubSyncEnh.exe
UninstallDisplayName={#MyAppName} v. {#MyAppVersion}
AppVersion={#MyAppVersion}
AppCopyright=Italiansubs.net
SetupIconFile=vss-innosetup\Resources\VSS.ico

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "vss-innosetup\vss-release\VisualSubSyncEnh.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\ICSharpCode.SharpZipLib.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\js3215R.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\libhunspell.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\MediaInfo.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\VSSCustomVSFilterV2a.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\WavWriter.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\avcodec-lav-57.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\avfilter-lav-6.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\avformat-lav-57.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\avresample-lav-3.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\avutil-lav-55.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\IntelQuickSyncDecoder.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\LAVAudio.ax"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\LAVSplitter.ax"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\LAVVideo.ax"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\libbluray.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\swscale-lav-4.dll"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\codecLav\LAVFilters.Dependencies.manifest"; DestDir: "{app}\codecLav"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\duration.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\itasa_typos_kickaha.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\itasa_zplit_kickaha.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\overlapping.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\readme.txt"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\speed.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\too_long_line.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\too_many_lines.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\un-closed_tags.js"; DestDir: "{app}\jsplugin"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\common\itasa.js"; DestDir: "{app}\jsplugin\common"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\common\tools.js"; DestDir: "{app}\jsplugin\common"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\general\action_itasa.js"; DestDir: "{app}\jsplugin\general"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\general\action_quick_stats.js"; DestDir: "{app}\jsplugin\general"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\general\general_plugin.js"; DestDir: "{app}\jsplugin\general"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\jsplugin\general\action_nixxo.js"; DestDir: "{app}\jsplugin\general"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\mkvtoolnix\mkvmerge.exe"; DestDir: "{app}\mkvtoolnix"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\mkvtoolnix\mkvextract.exe"; DestDir: "{app}\mkvtoolnix"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\mkvtoolnix\ffmpeg.exe"; DestDir: "{app}\mkvtoolnix"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\web\details.css"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\details.shtml"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\index.shtml"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\index_wav.shtml"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\index_wav_export.shtml"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\script.js"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\send_suggestion.shtml"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\web\style.css"; DestDir: "{app}\web"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\deploy\VisualSubSync.ini"; DestDir: "{app}\deploy"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\deploy\perso.dic"; DestDir: "{app}\deploy"; Flags: ignoreversion
Source: "vss-innosetup\vss-release\dict\_about_dictionaries_.txt"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\it_IT.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\it_IT.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\th_it_IT_v2.dat"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\th_it_IT_v2.idx"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\th_it_IT_v2m.xml"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\th_it_IT_v2s.xml"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\th_it_IT_v2w.xml"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_US.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_US.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_GB.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_GB.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_CA.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_CA.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_AU.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\en_AU.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\fr_FR.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\fr_FR.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\de_DE.aff"; DestDir: "{app}\dict"; Flags: IgnoreVersion
Source: "vss-innosetup\vss-release\dict\de_DE.dic"; DestDir: "{app}\dict"; Flags: IgnoreVersion

[Run]
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"

[CustomMessages]
italian.AdditionalInstallOptions=Opzioni:
english.AdditionalInstallOptions=Options:
italian.adminpermission=Esegui questo programma come amministratore
english.adminpermission=Run as administrator
italian.lavcodec=Non ho i Lav codec installati nel sistema (se deselezionato assicurarsi eventualmente di mantenere aggiornati i Lav Filter 32 bit dal sito http://1f0.de/downloads/)
english.lavcodec=I have not the Lav codec installed on my system (if unchecked make sure to keep Lav Filter 32 bit updated  from the site http://1f0.de/downloads/)

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\VisualSubSyncEnh.exe"; IconIndex: 2; Tasks: desktopicon

[UninstallDelete]
Type: files; Name: "{userappdata}\VisualSubSyncEnh\VisualSubSync.ini"
Type: files; Name: "{userappdata}\VisualSubSyncEnh\Perso.dic"
Type: dirifempty; Name: "{userappdata}\VisualSubSyncEnh"
