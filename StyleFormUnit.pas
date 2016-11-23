unit StyleFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntExtCtrls, SSAParserUnit, ComCtrls,
  TntComCtrls, PopupTrackbarUnit;

type
  TSSAAlignment = (
    alssaBottomLeft = 1, alssaBottomCenter = 2, alssaBottomRight = 3,
    alssaMiddleLeft = 9, alssaMiddleCenter = 10, alssaMiddleRight = 11,
    alssaTopLeft = 5, alssaTopCenter = 6, alssaTopRight = 7);

  TASSAlignment = (
    alassBottomLeft = 1, alassBottomCenter = 2, alassBottomRight = 3,
    alassMiddleLeft = 4, alassMiddleCenter = 5, alassMiddleRight = 6,
    alassTopLeft = 7, alassTopCenter = 8, alassTopRight = 9);

  TStyleFormMode = (sfmSSA,sfmASS);

  TSSAStyle = class
    name : WideString;
    fontname : WideString;
    fontsize : double;
    primaryColor : cardinal;
    secondaryColor : cardinal;
    outlineColor : cardinal;
    backColor : cardinal;
    bold : integer;
    italic : integer;
    underline : integer;
    strikeout : integer;
    scaleX : double;
    scaleY : double;
    spacing : double;
    angle : double;
    borderStyle : integer;
    outline : double;
    shadow : double;
    alignment : integer;
    marginL : integer;
    marginR : integer;
    marginV : integer;
    alphaLevel : integer;
    encoding : integer;

    constructor Create;
    procedure Assign(style : TSSAStyle);
    function getAsSSA : WideString;
    function getAsASS : WideString;
    function Equals(style : TSSAStyle) : Boolean;
  end;

  TStyleForm = class(TForm)
    lstStyles: TTntListBox;
    stFontPreview: TTntStaticText;
    bttFont: TTntButton;
    rgVAlignment: TTntRadioGroup;
    TntGroupBox2: TTntGroupBox;
    edLMargin: TTntEdit;
    rgHAlignment: TTntRadioGroup;
    TntGroupBox1: TTntGroupBox;
    edVMargin: TTntEdit;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
    TntLabel4: TTntLabel;
    TntLabel5: TTntLabel;
    edOutline: TTntEdit;
    bttNew: TTntButton;
    bttDelete: TTntButton;
    bttClose: TTntButton;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    pnlPrimaryColor: TTntStaticText;
    TntLabel6: TTntLabel;
    pnlSecondaryColor: TTntStaticText;
    TntLabel7: TTntLabel;
    pnlOutlineColor: TTntStaticText;
    pnlBackColor: TTntStaticText;
    TntLabel8: TTntLabel;
    TntLabel9: TTntLabel;
    bttCopy: TTntButton;
    TntLabel10: TTntLabel;
    edStyleName: TTntEdit;
    edShadow: TTntEdit;
    bttApply: TTntButton;
    bttReset: TTntButton;
    gbScaleX: TTntGroupBox;
    edScaleX: TTntEdit;
    gbScaleY: TTntGroupBox;
    edScaleY: TTntEdit;
    TntLabel13: TTntLabel;
    TntLabel14: TTntLabel;
    TntLabel15: TTntLabel;
    edSpacing: TTntEdit;
    edAngle: TTntEdit;
    TntLabel11: TTntLabel;
    bttHValue: TTntButton;
    bttSValue: TTntButton;
    bttLValue: TTntButton;
    TntGroupBox3: TTntGroupBox;
    edRMargin: TTntEdit;
    procedure bttFontClick(Sender: TObject);
    procedure bttPrimaryColorClick(Sender: TObject);
    procedure pnlColorClick(Sender: TObject);
    procedure bttCloseClick(Sender: TObject);
    procedure bttNewClick(Sender: TObject);
    procedure bttCopyClick(Sender: TObject);
    procedure bttDeleteClick(Sender: TObject);
    procedure lstStylesClick(Sender: TObject);
    procedure bttApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure checkChangedSender(Sender: TObject);
    procedure bttResetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bttHSLButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FStylesChanged : Boolean;
    FPopupTrackbar : TPopupTrackbar;
    FPrimaryColor, FSecondaryColor, FOutlineColor, FBackColor : TColor;
    FHue, FSaturation, FLuminance : Double;
    
    procedure ShowSelection;
    procedure ClearForm;
    procedure ClearList;
    procedure FormToData(var style : TSSAStyle);
    procedure DataToForm(style : TSSAStyle);
    procedure UpdateSelectionData;
    procedure EnableControls(Enable : Boolean);
    procedure CheckChange;
    function GetSelectedStyle : TSSAStyle;
    procedure OnPopupTrackbarValueChanged(Sender: TObject);
    procedure ClearHSLBtt;
  public
    { Public declarations }
    procedure LoadStylesFromParser(ssaParser : TSSAParser);
    function GetCount : Integer;
    function GetStyleAt(Index : Integer) : TSSAStyle;
    function HaveStylesChanged : Boolean;
    procedure PreSelect(StyleName : WideString);
    procedure ClearAll;
    procedure ConfigureMode(StyleFormMode : TStyleFormMode);
    procedure AddDefaultStyle;
  end;

  function StyleFormInstance : TStyleForm;

implementation

{$R *.dfm}

uses MiscToolsUnit, Math, main, RGBHSLColorUnit;

var
  StyleForm: TStyleForm;

// =============================================================================

function StyleFormInstance : TStyleForm;
begin
  if (StyleForm = nil) then
    Application.CreateForm(TStyleForm, StyleForm);
  Result := StyleForm;
end;

// =============================================================================

function SSAToASSAlignment(SSAAlignment : TSSAAlignment) : TASSAlignment;
begin
  Result := alassBottomCenter;
  case SSAAlignment of
  alssaBottomLeft: Result := alassBottomLeft;
  alssaBottomCenter: Result := alassBottomCenter;
  alssaBottomRight: Result := alassBottomRight;
  alssaMiddleLeft: Result := alassMiddleLeft;
  alssaMiddleCenter: Result := alassMiddleCenter;
  alssaMiddleRight: Result := alassMiddleRight;
  alssaTopLeft: Result := alassTopLeft;
  alssaTopCenter: Result := alassTopCenter;
  alssaTopRight: Result := alassTopRight;
  end;
end;

function ASSToSSAAlignment(ASSAlignment : TASSAlignment) : TSSAAlignment;
begin
  Result := alssaBottomCenter;
  case ASSAlignment of
  alassBottomLeft: Result := alssaBottomLeft;
  alassBottomCenter: Result := alssaBottomCenter;
  alassBottomRight: Result := alssaBottomRight;
  alassMiddleLeft: Result := alssaMiddleLeft;
  alassMiddleCenter: Result := alssaMiddleCenter;
  alassMiddleRight: Result := alssaMiddleRight;
  alassTopLeft: Result := alssaTopLeft;
  alassTopCenter: Result := alssaTopCenter;
  alassTopRight: Result := alssaTopRight;
  end;
end;

constructor TSSAStyle.Create;
begin
  inherited;
  name := 'New Style';
  fontname := 'Arial';
  fontsize := 24;
  primaryColor := clYellow;
  secondaryColor := clBlack;
  outlineColor := clBlack;
  backColor := clBlack;
  bold := 0;
  italic := 0;
  underline := 0;
  strikeout := 0;
  scaleX := 100.0;
  scaleY := 100.0;
  spacing := 0.0;
  angle := 0.0;
  borderStyle := 1;
  outline := 2.0;
  shadow := 0.0;
  alignment := Ord(alassBottomCenter);
  marginL := 15;
  marginR := 15;
  marginV := 15;
  encoding := 0;
end;

// -----------------------------------------------------------------------------

procedure TSSAStyle.Assign(style : TSSAStyle);
begin
  name := style.name;
  fontname := style.fontname;
  fontsize := style.fontsize;
  primaryColor := style.primaryColor;
  secondaryColor := style.secondaryColor;
  outlineColor := style.outlineColor;
  backColor := style.backColor;
  bold := style.bold;
  italic := style.italic;
  underline := style.underline;
  strikeout := style.strikeout;
  scaleX := style.scaleX;
  scaleY := style.scaleY;
  spacing := style.spacing;
  angle := style.angle;
  borderStyle := style.borderStyle;
  outline := style.outline;
  shadow := style.shadow;
  alignment := style.alignment;
  marginL := style.marginL;
  marginR := style.marginR;
  marginV := style.marginV;
  alphaLevel := style.alphaLevel;
  encoding := style.encoding;
end;

function TSSAStyle.getAsSSA : WideString;
var SSAAlignment : Integer;
begin
  SSAAlignment := Ord(ASSToSSAAlignment(TASSAlignment(alignment)));
  // Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding
  // Style: Default,Arial,28,65535,255,16744448,-2147483640,-1,0,1,3,0,2,30,30,30,0,128
  Result := Format('Style: %s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
    [name, fontname, Round(fontsize), primaryColor, secondaryColor, outlineColor, backColor,
      bold, italic, borderStyle, Round(outline), Round(shadow), SSAAlignment,
      marginL, marginR, marginV, alphaLevel, encoding]);
end;

function TSSAStyle.getAsASS : WideString;
var ssaFormat : TFormatSettings;
begin
  GetLocaleFormatSettings(0,ssaFormat);
  ssaFormat.DecimalSeparator := '.';

  // Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic,  Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
  // Style: *Default,Arial,28,&H00FFFFFF,&H00400040,&H00C0C0C0,&H82C0C0C0,0,0,0,0,100,100,0,0,0,0,0,5,15,15,15,0
  Result := Format('Style: %s,%s,%d,%s,%s,%s,%s,%d,%d,%d,%d,%s,%s,%s,%s,%d,%s,%s,%d,%d,%d,%d,%d',
    [name, fontname, Round(fontsize), ABGRColor2AssColorString(primaryColor),
    ABGRColor2AssColorString(secondaryColor), ABGRColor2AssColorString(outlineColor),
    ABGRColor2AssColorString(backColor), bold, italic, underline, strikeout,
    FloatToStr(scaleX, ssaFormat), FloatToStr(scaleY, ssaFormat),
    FloatToStr(spacing, ssaFormat), FloatToStr(angle, ssaFormat),
    borderStyle,
    FloatToStr(outline, ssaFormat), FloatToStr(shadow, ssaFormat),
    alignment, marginL, marginR, marginV, encoding]);
end;

function TSSAStyle.Equals(style : TSSAStyle) : Boolean;
begin
  Result := (style.name = name)
    and (style.fontname = fontname)
    and (style.fontsize = fontsize)
    and (style.primaryColor = primaryColor)
    and (style.secondaryColor = secondaryColor)
    and (style.outlineColor = outlineColor)
    and (style.backColor = backColor)
    and (style.bold = bold)
    and (style.italic = italic)
    and (style.underline = underline)
    and (style.strikeout = strikeout)
    and (style.scaleX = scaleX)
    and (style.scaleY = scaleY)
    and (style.spacing = spacing)
    and (style.angle = angle)
    and (style.borderStyle = borderStyle)
    and (style.outline = outline)
    and (style.shadow = shadow)
    and (style.alignment = alignment)
    and (style.marginL = marginL)
    and (style.marginR = marginR)
    and (style.marginV = marginV)
    and (style.alphaLevel = alphaLevel)
    and (style.encoding = encoding);

end;

// =============================================================================

procedure TStyleForm.bttFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(stFontPreview.Font);
  if FontDialog1.Execute then
  begin
    stFontPreview.Font.Assign(FontDialog1.Font);
    stFontPreview.Caption := Font2String(stFontPreview.Font);
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttPrimaryColorClick(Sender: TObject);
begin
  ColorDialog1.Color := pnlPrimaryColor.Color;
  if ColorDialog1.Execute then
  begin
    pnlPrimaryColor.Color := ColorDialog1.Color;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.pnlColorClick(Sender: TObject);
var Panel : TTntStaticText;
begin
  Panel := Sender as TTntStaticText;
  ColorDialog1.Color := Panel.Color;
  if ColorDialog1.Execute then
  begin
    Panel.Color := ColorDialog1.Color;
    FPrimaryColor := pnlPrimaryColor.Color;
    FSecondaryColor := pnlSecondaryColor.Color;
    FOutlineColor := pnlOutlineColor.Color;
    FBackColor := pnlBackColor.Color;
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttNewClick(Sender: TObject);
var newStyle : TSSAStyle;
begin
  // Create a style with default settings
  newStyle := TSSAStyle.Create;
  lstStyles.AddItem(newStyle.name, newStyle);
  lstStyles.ItemIndex := lstStyles.Count-1;
  ShowSelection;
  EnableControls(True);
  FStylesChanged := True;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttCopyClick(Sender: TObject);
var style, newStyle : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    // Do a copy of the selcted style
    style := GetSelectedStyle;
    newStyle := TSSAStyle.Create;
    newStyle.Assign(style);
    newStyle.name := style.name + ' Copy';
    lstStyles.AddItem(newStyle.name, newStyle);
    lstStyles.ItemIndex := lstStyles.Count-1;
    ShowSelection;
    FStylesChanged := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttDeleteClick(Sender: TObject);
var style : TSSAStyle;
    index : integer;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    index := lstStyles.ItemIndex;
    style := TSSAStyle(lstStyles.Items.Objects[index]);
    style.Free;
    lstStyles.DeleteSelected;
    FStylesChanged := True;
    if (lstStyles.Count = 0) then
    begin
      // There is no more style ? Add back a default style.
      AddDefaultStyle;
    end;
    // calculate new selection index
    if (index >= lstStyles.Count) then
    begin
      index := lstStyles.Count-1;
    end;
    lstStyles.ItemIndex := index;
    ShowSelection;
  end
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.lstStylesClick(Sender: TObject);
begin
  ShowSelection;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.ShowSelection;
var style : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    style := GetSelectedStyle;
    DataToForm(style);
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

function TStyleForm.GetSelectedStyle : TSSAStyle;
begin
  Result := TSSAStyle(lstStyles.Items.Objects[lstStyles.ItemIndex]);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.DataToForm(style : TSSAStyle);
var newFontStyles : TFontStyles;
    ssaFormat : TFormatSettings;
begin
  GetLocaleFormatSettings(0,ssaFormat);
  ssaFormat.DecimalSeparator := '.';
  
  edStyleName.Text := style.name;
  stFontPreview.Font.Name := style.fontname;
  stFontPreview.Font.Size := Round(style.fontsize);
  newFontStyles := [];
  if (style.bold <> 0) then
    newFontStyles := newFontStyles + [fsBold];
  if (style.italic <> 0) then
    newFontStyles := newFontStyles + [fsItalic];
  if (style.underline <> 0) then
    newFontStyles := newFontStyles + [fsUnderline];
  if (style.strikeout <> 0) then
    newFontStyles := newFontStyles + [fsStrikeOut];
  stFontPreview.Font.Style := newFontStyles;
  stFontPreview.Font.Color := clBlack;
  stFontPreview.Caption := Font2String(stFontPreview.Font);

  // 7 8 9
  // 4 5 6
  // 1 2 3
  case style.alignment of
    Ord(alassTopLeft),Ord(alassMiddleLeft),Ord(alassBottomLeft): rgHAlignment.ItemIndex := 0;
    Ord(alassTopCenter),Ord(alassMiddleCenter),Ord(alassBottomCenter): rgHAlignment.ItemIndex := 1;
    Ord(alassTopRight),Ord(alassMiddleRight),Ord(alassBottomRight): rgHAlignment.ItemIndex := 2
  end;
  case style.alignment of
    Ord(alassTopLeft),Ord(alassTopCenter),Ord(alassTopRight): rgVAlignment.ItemIndex := 0;
    Ord(alassMiddleLeft),Ord(alassMiddleCenter),Ord(alassMiddleRight): rgVAlignment.ItemIndex := 1;
    Ord(alassBottomLeft),Ord(alassBottomCenter),Ord(alassBottomRight): rgVAlignment.ItemIndex := 2;
  end;
  edLMargin.Text := IntToStr(style.marginL);
  edRMargin.Text := IntToStr(style.marginR);
  edVMargin.Text := IntToStr(style.marginV);
  pnlPrimaryColor.Color := style.primaryColor;
  pnlSecondaryColor.Color := style.secondaryColor;
  pnlOutlineColor.Color := style.outlineColor;
  pnlBackColor.Color := style.backColor;
  FPrimaryColor := style.primaryColor;
  FSecondaryColor := style.secondaryColor;
  FOutlineColor := style.outlineColor;
  FBackColor := style.backColor;

  edOutline.Text := FloatToStr(style.outline, ssaFormat);
  edShadow.Text := FloatToStr(style.shadow, ssaFormat);

  edScaleX.Text := FloatToStr(style.scaleX, ssaFormat);
  edScaleY.Text := FloatToStr(style.scaleY, ssaFormat);
  edSpacing.Text := FloatToStr(style.spacing, ssaFormat);
  edAngle.Text := FloatToStr(style.angle, ssaFormat);

  ClearHSLBtt;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.ClearForm;
begin
  edStyleName.Text := '';
  stFontPreview.Caption := '';
  rgHAlignment.ItemIndex := -1;
  rgVAlignment.ItemIndex := -1;
  edLMargin.Text := '';
  edRMargin.Text := '';
  edVMargin.Text := '';
  pnlPrimaryColor.Color := clBlack;
  pnlSecondaryColor.Color := clBlack;
  pnlOutlineColor.Color := clBlack;
  pnlBackColor.Color := clBlack;
  edOutline.Text := '';
  edShadow.Text := '';
  edScaleX.Text := '';
  edScaleY.Text := '';
  edSpacing.Text := '';
  edAngle.Text := '';
  FPopupTrackbar.Value := 0;
  ClearHSLBtt;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.ClearList;
var i : integer;
    style : TSSAStyle;
begin
  for i := 0 to lstStyles.Count-1 do
  begin
    style := TSSAStyle(lstStyles.Items.Objects[i]);
    style.Free;
  end;
  lstStyles.Clear;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.FormToData(var style : TSSAStyle);
var ssaFloatFormatSettings : TFormatSettings;
begin
  GetLocaleFormatSettings(0,ssaFloatFormatSettings);
  ssaFloatFormatSettings.DecimalSeparator := '.';

  style.name := edStyleName.Text;
  style.fontname := stFontPreview.Font.Name;
  style.fontsize := stFontPreview.Font.Size;
  if fsBold in stFontPreview.Font.Style then
    style.bold := -1
  else
    style.bold := 0;
  if fsItalic in stFontPreview.Font.Style then
    style.italic := -1
  else
    style.italic := 0;
  if fsUnderline in stFontPreview.Font.Style then
    style.underline := -1
  else
    style.underline := 0;
  if fsStrikeOut in stFontPreview.Font.Style then
    style.strikeout := -1
  else
    style.strikeout := 0;
      
  case rgHAlignment.ItemIndex of
    0: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alassTopLeft);
          1: style.alignment := Ord(alassMiddleLeft);
          2: style.alignment := Ord(alassBottomLeft);
       end;
    1: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alassTopCenter);
          1: style.alignment := Ord(alassMiddleCenter);
          2: style.alignment := Ord(alassBottomCenter);
       end;
    2: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alassTopRight);
          1: style.alignment := Ord(alassMiddleRight);
          2: style.alignment := Ord(alassBottomRight);
       end;
  end;
  style.marginR := StrToIntDef(edRMargin.Text,0);
  style.marginL := StrToIntDef(edLMargin.Text,0);
  style.marginV := StrToIntDef(edVMargin.Text,0);

  style.primaryColor := pnlPrimaryColor.Color;
  style.secondaryColor := pnlSecondaryColor.Color;
  style.outlineColor := pnlOutlineColor.Color;
  style.backColor := pnlBackColor.Color;
  style.outline := StrToFloatDef(edOutline.Text, 0.0, ssaFloatFormatSettings);
  style.shadow := StrToFloatDef(edShadow.Text, 0.0, ssaFloatFormatSettings);

  style.scaleX := StrToFloatDef(edScaleX.Text, 0.0, ssaFloatFormatSettings);
  style.scaleY := StrToFloatDef(edScaleY.Text, 0.0, ssaFloatFormatSettings);
  style.spacing := StrToFloatDef(edSpacing.Text, 0.0, ssaFloatFormatSettings);
  style.angle := StrToFloatDef(edAngle.Text, 0.0, ssaFloatFormatSettings);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.UpdateSelectionData;
var style : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    style := GetSelectedStyle;
    // Check for comma in style name
    if Pos(',',edStyleName.Text) > 0 then
    begin
      MessageBoxW(Handle, PWideChar(WideString('Comma is not an authorized character in style name.')),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
      edStyleName.SetFocus;
      Exit;
    end;
    // If the style name has changed, check if doesn't exist yet
    if (style.name <> edStyleName.Text) then
    begin
      if (lstStyles.Items.IndexOf(edStyleName.Text) <> -1) then
      begin
        MessageBoxW(Handle, PWideChar(WideString('This style name already exists')),
          PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        edStyleName.SetFocus;
        Exit;
      end
      else
      begin
        // Rename style in subtitles
        MainForm.RenameStyle(style.name, edStyleName.Text);
      end;
    end;
    FormToData(style);
    lstStyles.Items.Strings[lstStyles.ItemIndex] := style.name;
    FStylesChanged := True;
    MainForm.CurrentProjectSetDirty;
    CheckChange;
    ClearHSLBtt;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttApplyClick(Sender: TObject);
begin
  UpdateSelectionData;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.EnableControls(Enable : Boolean);
begin
  edStyleName.Enabled := Enable;
  bttFont.Enabled := Enable;
  rgHAlignment.Enabled := Enable;
  rgVAlignment.Enabled := Enable;
  edLMargin.Enabled := Enable;
  edRMargin.Enabled := Enable;
  edVMargin.Enabled := Enable;
  pnlPrimaryColor.Enabled := Enable;
  pnlSecondaryColor.Enabled := Enable;
  pnlOutlineColor.Enabled := Enable;
  pnlBackColor.Enabled := Enable;
  edOutline.Enabled := Enable;
  edShadow.Enabled := Enable;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.FormCreate(Sender: TObject);
begin
  EnableControls(False);
  FPopupTrackbar := TPopupTrackbar.Create(Self);
  FPopupTrackbar.OnValueChanged := OnPopupTrackbarValueChanged;
  ClearHSLBtt;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.LoadStylesFromParser(ssaParser : TSSAParser);
var i : integer;
    newStyle : TSSAStyle;
    alignment : Integer;
begin
  ClearAll;
  for i := 0 to ssaParser.GetStylesCount-1 do
  begin
    newStyle := TSSAStyle.Create;
    newStyle.name := ssaParser.GetStyleValueAsString(i, 'Name');
    newStyle.fontname := ssaParser.GetStyleValueAsString(i, 'Fontname');
    newStyle.fontsize := ssaParser.GetStyleValueAsDouble(i, 'Fontsize');
    newStyle.primaryColor := AssColorString2ABGR(ssaParser.GetStyleValueAsString(i, 'PrimaryColour'));
    newStyle.secondaryColor := AssColorString2ABGR(ssaParser.GetStyleValueAsString(i, 'SecondaryColour'));
    if ssaParser.StyleKeyExists('TertiaryColour') then
      newStyle.outlineColor := AssColorString2ABGR(ssaParser.GetStyleValueAsString(i, 'TertiaryColour'))
    else
      newStyle.outlineColor := AssColorString2ABGR(ssaParser.GetStyleValueAsString(i, 'OutlineColour'));
    newStyle.backColor := AssColorString2ABGR(ssaParser.GetStyleValueAsString(i, 'BackColour'));
    newStyle.bold := ssaParser.GetStyleValueAsInteger(i, 'Bold');
    newStyle.italic := ssaParser.GetStyleValueAsInteger(i, 'Italic');
    newStyle.underline := ssaParser.GetStyleValueAsInteger(i, 'Underline');
    newStyle.strikeout := ssaParser.GetStyleValueAsInteger(i, 'Strikeout');
    if ssaParser.StyleKeyExists('ScaleX') then
      newStyle.scaleX := ssaParser.GetStyleValueAsDouble(i, 'ScaleX');
    if ssaParser.StyleKeyExists('ScaleY') then
      newStyle.scaleY := ssaParser.GetStyleValueAsDouble(i, 'ScaleY');
    newStyle.spacing := ssaParser.GetStyleValueAsDouble(i, 'Spacing');
    newStyle.angle := ssaParser.GetStyleValueAsDouble(i, 'Angle');
    newStyle.borderStyle := ssaParser.GetStyleValueAsInteger(i, 'BorderStyle');
    newStyle.outline := ssaParser.GetStyleValueAsDouble(i, 'Outline');
    newStyle.shadow := ssaParser.GetStyleValueAsDouble(i, 'Shadow');
    alignment := ssaParser.GetStyleValueAsInteger(i, 'Alignment');
    if ssaParser.GetIsASS then
      newStyle.alignment := alignment
    else
      newStyle.alignment := Ord(SSAToASSAlignment(TSSAAlignment(alignment)));
    newStyle.marginL := ssaParser.GetStyleValueAsInteger(i, 'MarginL');
    newStyle.marginR := ssaParser.GetStyleValueAsInteger(i, 'MarginR');
    newStyle.marginV := ssaParser.GetStyleValueAsInteger(i, 'MarginV');
    newStyle.alphaLevel := ssaParser.GetStyleValueAsInteger(i, 'AlphaLevel');
    newStyle.encoding := ssaParser.GetStyleValueAsInteger(i, 'Encoding');
    lstStyles.AddItem(newStyle.name, newStyle);
  end;
  if (ssaParser.GetStylesCount = 0) then
  begin
    AddDefaultStyle;
  end;
end;

function TStyleForm.GetCount : Integer;
begin
  Result := lstStyles.Count;
end;

function TStyleForm.GetStyleAt(Index : Integer) : TSSAStyle;
begin
  Result := TSSAStyle(lstStyles.Items.Objects[Index]);
end;

procedure TStyleForm.FormShow(Sender: TObject);
begin
  if (lstStyles.Count > 0) then
  begin
    EnableControls(True);
    ShowSelection;
  end;
  FStylesChanged := False;
  ShowSelection;
end;

procedure TStyleForm.CheckChange;
var MaybeChangedStyle : TSSAStyle;
begin
  if lstStyles.Count > 0 then
  begin
    MaybeChangedStyle := TSSAStyle.Create;
    MaybeChangedStyle.Assign(GetSelectedStyle);
    FormToData(maybeChangedStyle);
    bttApply.Enabled := not MaybeChangedStyle.Equals(GetSelectedStyle);
    bttReset.Enabled := bttApply.Enabled;
    MaybeChangedStyle.Free;
  end;
end;

procedure TStyleForm.checkChangedSender(Sender: TObject);
begin
  CheckChange;
end;

procedure TStyleForm.bttResetClick(Sender: TObject);
begin
  ShowSelection;
end;

function TStyleForm.HaveStylesChanged : Boolean;
begin
  Result := FStylesChanged;
end;

procedure TStyleForm.PreSelect(StyleName : WideString);
var StyleIndex : Integer;
begin
  StyleIndex := lstStyles.Items.IndexOf(StyleName);
  if (StyleIndex <> -1) then
  begin
    lstStyles.ItemIndex := StyleIndex;
  end
  else if (lstStyles.ItemIndex = -1) and (lstStyles.Count > 0) then
  begin
    lstStyles.ItemIndex := 0;
  end;
end;

procedure TStyleForm.ClearAll;
begin
  ClearForm;
  ClearList;
end;

procedure TStyleForm.ConfigureMode(StyleFormMode : TStyleFormMode);
begin
  edScaleX.Enabled := (StyleFormMode = sfmASS);
  edScaleY.Enabled := (StyleFormMode = sfmASS);
  edSpacing.Enabled := (StyleFormMode = sfmASS);
  edAngle.Enabled := (StyleFormMode = sfmASS);

  if (StyleFormMode = sfmASS) then
    FontDialog1.Options := FontDialog1.Options + [fdEffects]
  else
    FontDialog1.Options := FontDialog1.Options - [fdEffects];
end;

procedure TStyleForm.AddDefaultStyle;
var newStyle : TSSAStyle;
begin
  // Create a style with default settings
  newStyle := TSSAStyle.Create;
  newStyle.name := 'Default';
  lstStyles.AddItem(newStyle.name, newStyle);
end;

procedure TStyleForm.FormDestroy(Sender: TObject);
begin
  ClearAll;
end;

procedure TStyleForm.OnPopupTrackbarValueChanged(Sender: TObject);
var Panel : TPopupTrackbar;
    Button : TWinControl;
    text : string;
    H, S, L : Single;
begin
  Panel := Sender as TPopupTrackbar;
  Button := Panel.Parent as TControl;
  if (Button = bttHValue) then
  begin
    text := 'H=';
    FHue := Panel.Value;
  end
  else if (Button = bttSValue) then
  begin
    text := 'S=';
    FSaturation := Panel.Value;
  end
  else if (Button = bttLValue) then
  begin
    text := 'L=';
    FLuminance := Panel.Value;
  end;
  text := text + IntToStr(Round(Panel.Value));
  Button.SetTextBuf(@text[1]);

  H := FHue / 360.0;
  S := FSaturation / 100.0;
  L := FLuminance / 100.;
  pnlPrimaryColor.Color := ChangeColorHSL(FPrimaryColor, H, S, L);
  pnlSecondaryColor.Color := ChangeColorHSL(FSecondaryColor, H, S, L);
  pnlOutlineColor.Color := ChangeColorHSL(FOutlineColor, H, S, L);
  pnlBackColor.Color := ChangeColorHSL(FBackColor, H, S, L);
  CheckChange;
end;

procedure TStyleForm.bttHSLButtonClick(Sender: TObject);
var SenderWC : TWinControl;
begin
  SenderWC := Sender as TWinControl;

  if FPopupTrackbar.IsDisplayed then
  begin
    FPopupTrackbar.Close;
    if (FPopupTrackbar.Parent = SenderWC) then
      Exit;
  end;
  if FPopupTrackbar.IsDisplayed then
    FPopupTrackbar.Close;
  FPopupTrackbar.Parent := SenderWC;
  FPopupTrackbar.Width := 200;
  FPopupTrackbar.Height := SenderWC.Height;

  if (Sender = bttHValue) then
  begin
    FPopupTrackbar.MinValue := 0;
    FPopupTrackbar.MaxValue := 360;
    FPopupTrackbar.Value := FHue;
  end
  else if (Sender = bttSValue) then
  begin
    FPopupTrackbar.MinValue := -100;
    FPopupTrackbar.MaxValue := 100;
    FPopupTrackbar.Value := FSaturation;
  end
  else if (Sender = bttLValue) then
  begin
    FPopupTrackbar.MinValue := -100;
    FPopupTrackbar.MaxValue := 100;
    FPopupTrackbar.Value := FLuminance;
  end;

  FPopupTrackbar.Popup;
end;

procedure TStyleForm.ClearHSLBtt;
begin
  FHue := 0;
  FSaturation := 0;
  FLuminance := 0;
  bttHValue.Caption := 'H=0';
  bttSValue.Caption := 'S=0';
  bttLValue.Caption := 'L=0';
  FPopupTrackbar.Value := 0;
  FPrimaryColor := pnlPrimaryColor.Color;
  FSecondaryColor := pnlSecondaryColor.Color;
  FOutlineColor := pnlOutlineColor.Color;
  FBackColor := pnlBackColor.Color;
end;

procedure TStyleForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

// =============================================================================

initialization
  StyleForm := nil;

// =============================================================================
end.
// =============================================================================
