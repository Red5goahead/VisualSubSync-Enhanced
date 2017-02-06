// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------

unit GlobalUnit;

interface

uses MiscToolsUnit, Forms, WAVDisplayerUnit, ProjectUnit, SysUtils;

type
  TContext = class
    Parent : TContext;
    function GetFieldValue(Name : string; var Found : Boolean) : string; virtual; abstract;
  end;

  TGlobalContext = class(TContext)
    WavAverageBytePerSecond : Integer;
    SubList : TRangeList;
    CurrentProject : TVSSProject;
    function GetFieldValue(Name : string; var Found : Boolean) : string; override;
  end;

  procedure CheckBackupDirectory;
  procedure CheckDictDirectory;
  procedure Deploy;
  Function ItaliansubsAuthorization(User: String; Password : String): boolean;

var
  ApplicationName : string = 'VisualSubSync';
  g_ApplicationVersion : TFileVersion;
  g_GlobalContext : TGlobalContext;
  // Synchronization object from application<->web server threads
  g_WebRWSynchro : TMultiReadExclusiveWriteSynchronizer;
  g_BackupDirectory : WideString;
  g_AppDataBackupDirectory : WideString;
  g_PluginPath : WideString;
  g_ApplicationPath : WideString;
  g_PresetsPath : WideString;
  g_DictPath : WideString;
  g_AppDataDictPath : WideString;
  g_WavExtractorGraphDebugInfo : WideString;
  g_VideoGraphDebugInfo : WideString;
  g_AudioGraphDebugInfo : WideString;
  g_ItaliansubsAuthorization : Boolean;
  // enhanced Flag
  IsEnhanced : Boolean = False;
  // universal app enviroment
  IsUniversalAppEnviroment : Boolean = False;
  {$IFNDEF enhanced}
  RootAppData : String = 'VisualSubSync';
  {$ELSE}
  RootAppData : String = 'VisualSubSyncEnh';
  {$ENDIF}

implementation

uses TntSysUtils, TntForms, KAZip, HTTPSend;

function TGlobalContext.GetFieldValue(Name : string; var Found : Boolean) : string;
begin
  Found := True;
  if (Name = 'version') then
    Result := g_ApplicationVersion.VersionString
  else if (Name = 'project-filename') then
    Result := ExtractFileName(CurrentProject.Filename)
  else
    Found := False
end;

{$IFNDEF enhanced}
procedure CheckBackupDirectory;
begin
  if WideDirectoryExists(g_AppDataBackupDirectory) then
    begin
      g_BackupDirectory := g_AppDataBackupDirectory;
      Exit;
    end;
  if not WideDirectoryExists(g_BackupDirectory) then
    WideForceDirectories(g_BackupDirectory);
end;
{$ELSE}
procedure CheckBackupDirectory;
begin
  g_BackupDirectory := g_AppDataBackupDirectory;
end;
{$ENDIF}

procedure CheckDictDirectory;
begin
  if WideDirectoryExists(g_AppDataDictPath) then
    g_DictPath := g_AppDataDictPath;
end;

{$IFNDEF enhanced}
procedure Deploy;
 var KAZip : TKAZip;
begin
  // deploy folder -> itasa archive will be extract to application data into VisualSubSync folder
  if WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\deploy\default.zip') then
   if not WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\VisualSubSync.ini') then
     begin
      KAZip := TKAZip.Create(nil);
      KAZip.OverwriteAction := oaOverwrite;
      KAZip.Open(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\deploy\default.zip');
      KAZip.ExtractAll(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData);
      KAZip.Close;
     end;
  // deploy folder -> perso archive will be extract to application data into VisualSubSync\dict folder
  if WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\deploy\perso.zip') then
   if not WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\dict\perso.dic') then
     begin
      KAZip := TKAZip.Create(nil);
      KAZip.OverwriteAction := oaOverwrite;
      KAZip.Open(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\deploy\perso.zip');
      KAZip.ExtractAll(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\dict');
      KAZip.Close;
     end;
end;
{$ELSE}
procedure Deploy;
begin
  // deploy folder -> itasa archive will be extract to application data into VisualSubSyncEhn folder
  if WideFileExists(WideIncludeTrailingBackslash(g_ApplicationPath) + 'deploy\VisualSubSync.ini') then
   if not WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\VisualSubSync.ini') then
     begin
      WideCopyFile(WideIncludeTrailingBackslash(g_ApplicationPath) + 'deploy\VisualSubSync.ini',
        WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\VisualSubSync.ini', False);
     end;
  // deploy folder -> perso archive will be extract to application data into VisualSubSyncEnh folder
  if WideFileExists(WideIncludeTrailingBackslash(g_ApplicationPath) + 'deploy\perso.dic') then
   if not WideFileExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\perso.dic') then
     begin
      WideCopyFile(WideIncludeTrailingBackslash(g_ApplicationPath) + 'deploy\perso.dic',
        WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\perso.dic', False);
     end;
end;
{$ENDIF}

Function ItaliansubsAuthorization(User: String; Password : String): boolean;
var
  HTTP: THTTPSend;
  UrlItasa, LoginParam : string;
begin
  if (User = 'Your Username') OR (Password = 'Your Password') OR
     (User = '') OR (Password = '') then
  begin
    Result := False;
    Exit;
  end;

  if g_ItaliansubsAuthorization then
  begin
   Result := true;
   Exit;
  end;

  HTTP := THTTPSend.Create;
  UrlItasa := 'http://www.italiansubs.net/index.php';
  LoginParam :='username='+User
              +'&passwd='+Password
              +'&option=com_user&Submit=Login&task=login';
  try
    HTTP.Document.Write(Pointer(LoginParam)^, Length(LoginParam));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.HTTPMethod('POST', UrlItasa);
    if Pos('SMFCookie', HTTP.Cookies.Text) > 0
    then
    begin
     g_ItaliansubsAuthorization := True;
     Result := True;
    end
    else
    begin
     g_ItaliansubsAuthorization := False;
     Result := False;
    end;
  finally
    HTTP.Free;
  end;

end;

initialization
  g_ApplicationPath := WideIncludeTrailingBackslash(WideExtractFilePath(TntApplication.ExeName));
  {$IFDEF enhanced}
  IsEnhanced := True;
  if Pos('windowsapp', lowercase(g_ApplicationPath)) > 0 then
  begin
    IsUniversalAppEnviroment := True;
    RootAppData := '';
  end;
  if not WideDirectoryExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData) then
  begin
   WideForceDirectories(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData);
  end;
  if not WideDirectoryExists(WideIncludeTrailingBackslash(GetUserDocumentsFolder) + 'VisualSubSync\Backup\') then
   begin
     WideForceDirectories(WideIncludeTrailingBackslash(GetUserDocumentsFolder) + 'VisualSubSync\Backup\');
   end;
  {$ELSE}
  IsEnhanced := False;
  IsUniversalAppEnviroment := False;
  {$ENDIF}
  if WideDirectoryExists(WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData) then Deploy;
  g_ApplicationVersion := TFileVersion.Create(Application.ExeName);
  g_GlobalContext := TGlobalContext.Create;
  g_WebRWSynchro := TMultiReadExclusiveWriteSynchronizer.Create;
  g_BackupDirectory := g_ApplicationPath + 'Backup\';
  {$IFNDEF enhanced}
  g_AppDataBackupDirectory := WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\Backup\';
  {$ELSE}
  g_AppDataBackupDirectory := WideIncludeTrailingBackslash(GetUserDocumentsFolder) + 'VisualSubSync\Backup\';
  {$ENDIF}
  CheckBackupDirectory;
  g_PluginPath := g_ApplicationPath + 'jsplugin\';
  g_PresetsPath := g_ApplicationPath + 'presets\';
  {$IFNDEF enhanced}
  g_DictPath := g_ApplicationPath + 'dict\';
  g_AppDataDictPath := WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData + '\dict\';
  {$ELSE}
  g_DictPath := g_ApplicationPath + 'dict\';
  g_AppDataDictPath := WideIncludeTrailingBackslash(GetUserApplicationDataFolder) + RootAppData;
  {$ENDIF}
  CheckDictDirectory;
  g_ItaliansubsAuthorization := False;

finalization
  g_WebRWSynchro.Free;
  g_WebRWSynchro := nil;
  g_GlobalContext.Free;
  g_GlobalContext := nil;
  g_ApplicationVersion.Free;
  g_ApplicationVersion := nil;

end.

