unit TranslateFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntExtCtrls, TntDialogs, ExtCtrls;

type
  TTranslateSelectionType = (tstAll, tstMissingOnly);
  TTranslateTextType = (tttEmpty, tttCopyOriginal, tttCopyTagsOnly, tttCustomText);

  TTranslator = class
  public
    function Translate(Text : WideString) : WideString;  dynamic; abstract;
  end;

  TTranslatorEmpty = class(TTranslator)
  public
    function Translate(Text : WideString) : WideString;  override;
  end;

  TTranslatorCopyOriginal = class(TTranslator)
  public
    function Translate(Text : WideString) : WideString;  override;
  end;

  TTranslatorCopyTagsOnly = class(TTranslator)
  public
    function Translate(Text : WideString) : WideString;  override;
  end;

  TTranslatorCustomText = class(TTranslator)
  private
    FCustomText : WideString;
  public
    constructor Create(CustomText : WideString);
    function Translate(Text : WideString) : WideString;  override;
  end;

  TTranslateForm = class(TForm)
    PanelBottom: TPanel;
    bttOK: TTntButton;
    bttCancel: TTntButton;
    PanelClient: TPanel;
    rgSelectionType: TTntRadioGroup;
    rgTextType: TTntRadioGroup;
    edCustomText: TTntEdit;
    edTargetFile: TTntEdit;
    bttBrowseTargetFile: TTntButton;
    TntSaveDialog1: TTntSaveDialog;
    procedure bttOKClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgTextTypeClick(Sender: TObject);
    procedure bttBrowseTargetFileClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rgSelectionTypeClick(Sender: TObject);
  private
    { Private declarations }
    HasBrowsedForFile : Boolean;
  public
    { Public declarations }
    function GetTranslateSelectionType : TTranslateSelectionType;
    function GetTranslateTextType : TTranslateTextType;

    procedure SetTranslateSelectionType(Value : TTranslateSelectionType);
    procedure SetTranslateTextType(Value : TTranslateTextType);
    procedure SetHasVO(Value : Boolean);

    function GetTargetFile : WideString;
    procedure SetTargetFile(Value : WideString);

    function GetTranslator : TTranslator;
    function GetHasBrowsedForFile : Boolean;
  end;

  function GetTranslatorCopyOriginalCommon : TTranslator;

var
  TranslateForm: TTranslateForm;

implementation

{$R *.dfm}

uses TntSysUtils, MiscToolsUnit;

var
  TranslatorCopyOriginalCommon : TTranslator;

// -------------------------------------------------------------------------------------------------

function GetTranslatorCopyOriginalCommon : TTranslator;
begin
  if (TranslatorCopyOriginalCommon = nil) then
    TranslatorCopyOriginalCommon := TTranslatorCopyOriginal.Create;
  Result := TranslatorCopyOriginalCommon;
end;

// -------------------------------------------------------------------------------------------------

function TTranslatorEmpty.Translate(Text : WideString) : WideString;
begin
  Result := '';
end;

function TTranslatorCopyOriginal.Translate(Text : WideString) : WideString;
begin
  Result := Text;
end;

function TTranslatorCustomText.Translate(Text : WideString) : WideString;
begin
  Result := FCustomText;
end;

constructor TTranslatorCustomText.Create(CustomText : WideString);
begin
  FCustomText := CustomText;
end;

function TTranslatorCopyTagsOnly.Translate(Text : WideString) : WideString;
begin
  Result := StripText(Text);
end;

// -------------------------------------------------------------------------------------------------

function TTranslateForm.GetTranslateSelectionType : TTranslateSelectionType;
begin
  Result := TTranslateSelectionType(rgSelectionType.ItemIndex);
end;

function TTranslateForm.GetTranslateTextType : TTranslateTextType;
begin
  Result := TTranslateTextType(rgTextType.ItemIndex);
end;

procedure TTranslateForm.SetTranslateSelectionType(Value : TTranslateSelectionType);
begin
  rgSelectionType.ItemIndex := Ord(Value);
  edTargetFile.Enabled := (GetTranslateSelectionType = tstAll);
  bttBrowseTargetFile.Enabled := edTargetFile.Enabled;
end;

procedure TTranslateForm.SetTranslateTextType(Value : TTranslateTextType);
begin
  rgTextType.ItemIndex := Ord(Value);
  edCustomText.Enabled := (GetTranslateTextType = tttCustomText);
end;

function TTranslateForm.GetTargetFile : WideString;
begin
  Result := edTargetFile.Text;
end;

procedure TTranslateForm.SetTargetFile(Value : WideString);
begin
  edTargetFile.Text := Value;
end;

procedure TTranslateForm.bttOKClick(Sender: TObject);
var res : Integer;
begin
  if (TranslateForm.GetTranslateSelectionType = tstAll) and (not GetHasBrowsedForFile)
      and WideFileExists(TranslateForm.GetTargetFile) then
  begin
    res := MessageBoxW(Handle, PWideChar(WideFormat(
    'The subtitle file "%s" already exists, do you want to overwrite it?',
      [TranslateForm.GetTargetFile])),
      PWideChar(WideString('Warning')),
      MB_YESNOCANCEL or MB_ICONWARNING);
    case res of
      IDYES: ModalResult := mrOk;
      IDNO: Exit;
      IDCANCEL: ModalResult := mrCancel;
    end;
  end
  else
    ModalResult := mrOk;
end;

procedure TTranslateForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTranslateForm.FormCreate(Sender: TObject);
begin
  SetTranslateSelectionType(tstAll);
  SetTranslateTextType(tttEmpty);
  HasBrowsedForFile := False;
end;

procedure TTranslateForm.rgSelectionTypeClick(Sender: TObject);
begin
  edTargetFile.Enabled := (GetTranslateSelectionType = tstAll);
  bttBrowseTargetFile.Enabled := edTargetFile.Enabled;
end;

procedure TTranslateForm.rgTextTypeClick(Sender: TObject);
begin
  edCustomText.Enabled := (GetTranslateTextType = tttCustomText);
end;

procedure TTranslateForm.bttBrowseTargetFileClick(Sender: TObject);
begin
    TntSaveDialog1.Filter := 'All files (*.*)|*.*';
    TntSaveDialog1.FileName := GetTargetFile;
    if TntSaveDialog1.Execute then
    begin
      HasBrowsedForFile := True;
      SetTargetFile(TntSaveDialog1.FileName);
    end;
end;

procedure TTranslateForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

function RadioGroupButton(itemIndex : integer; radioGroup : TTntRadioGroup) : TRadioButton; overload;
begin
   if (itemIndex < 0) OR (itemIndex >= radioGroup.Items.Count) then
   begin
     Result := nil;
     Exit;
   end;

   result := radioGroup.Controls[itemIndex] as TRadioButton;
end;

procedure TTranslateForm.SetHasVO(Value : Boolean);
begin
  RadioGroupButton(Ord(tstMissingOnly), rgSelectionType).Enabled := Value;
  if (Value) then
    SetTranslateSelectionType(tstMissingOnly)
  else
    SetTranslateSelectionType(tstAll);
end;

function TTranslateForm.GetTranslator : TTranslator;
begin
  case GetTranslateTextType of
    tttEmpty: Result := TTranslatorEmpty.Create;
    tttCopyOriginal: Result := TTranslatorCopyOriginal.Create;
    tttCopyTagsOnly: Result := TTranslatorCopyTagsOnly.Create;
    tttCustomText: Result := TTranslatorCustomText.Create(edCustomText.Text);
  else
    Result := TTranslatorEmpty.Create;
  end;
end;

function TTranslateForm.GetHasBrowsedForFile : Boolean;
begin
  Result := HasBrowsedForFile;
end;

initialization
  TranslatorCopyOriginalCommon := nil;

finalization
  if (TranslatorCopyOriginalCommon <> nil) then
    FreeAndNil(TranslatorCopyOriginalCommon);

// -------------------------------------------------------------------------------------------------
end.
// -------------------------------------------------------------------------------------------------

