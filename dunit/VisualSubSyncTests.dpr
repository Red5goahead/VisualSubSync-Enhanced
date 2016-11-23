// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program VisualSubSyncTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  JavaScriptPluginUnitTests in 'JavaScriptPluginUnitTests.pas',
  JavaScriptPluginUnit in '..\JavaScriptPluginUnit.pas',
  MiscToolsUnitTests in 'MiscToolsUnitTests.pas',
  MiscToolsUnit in '..\MiscToolsUnit.pas',
  SubtitleModelTests in 'SubtitleModelTests.pas',
  SubtitleModel in '..\SubtitleModel.pas';

{$R *.RES}

begin
  Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.

 