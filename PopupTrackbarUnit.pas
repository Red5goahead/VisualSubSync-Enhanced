unit PopupTrackbarUnit;

interface

uses Windows, ExtCtrls, Graphics, Classes, Messages, Controls;

type
  TPopupTrackbar = class(TCustomPanel)
  private
    FMinValue : Integer;
    FMaxValue : Integer;
    FValue : Double;
    FIsDisplayed : Boolean;
    FOffscreen : TBitmap;
    FOffscreenHighlight : TBitmap;

    FOnValueChanged : TNotifyEvent;
    FOnClose : TNotifyEvent;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetValue(NewValue : Double);
    function XToValue(X : Integer) : Double;
    function ValueToX(AValue : Double) : Integer;
    procedure UpdateCaption;
    procedure Paint; override;
    procedure CustomPaint(ACanvas : TCanvas; BColor, FColor : TColor);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup;
    procedure Close;

  published
    property OnValueChanged : TNotifyEvent read FOnValueChanged write FOnValueChanged;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
    property MinValue : Integer read FMinValue write FMinValue;
    property MaxValue : Integer read FMaxValue write FMaxValue;
    property Value : Double read FValue write SetValue;
    property IsDisplayed : Boolean read FIsDisplayed;
  end;

implementation

uses SysUtils;

// -----------------------------------------------------------------------------

constructor TPopupTrackbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0.0;
  FIsDisplayed := False;
  FDoubleBuffered := True;
  BevelOuter := bvLowered;
  //BevelInner := bvRaised;
  FOffscreen := TBitmap.Create;
  FOffscreenHighlight := TBitmap.Create;
  UpdateCaption;
end;

destructor TPopupTrackbar.Destroy;
begin
  FOffscreen.Free;
  FOffscreenHighlight.Free;
  inherited;
end;

procedure TPopupTrackbar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupTrackbar.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TPopupTrackbar.SetValue(NewValue : Double);
begin
  if (NewValue <> Value) then
  begin
    if (NewValue < FMinValue) then
      NewValue := FMinValue
    else if (NewValue > FMaxValue) then
      NewValue := FMaxValue;
    FValue := NewValue;
    UpdateCaption;
  end;
end;

function TPopupTrackbar.XToValue(X : Integer) : Double;
begin
  Result := FMinValue + (X / (Width / (FMaxValue - FMinValue)));
end;

function TPopupTrackbar.ValueToX(AValue : Double) : Integer;
begin
  Result := Round((Width / (FMaxValue - FMinValue)) * (AValue - FMinValue));
end;

procedure TPopupTrackbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  SetValue(XToValue(X));
  Close;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPopupTrackbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if GetCapture = 0 then
    SetCapture(Handle);
  SetValue(XToValue(X));
  if Assigned(FOnValueChanged) then
  begin
    FOnValueChanged(Self);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TPopupTrackbar.Popup;
var P: TPoint;
begin
  P := Parent.ClientToScreen(Point(0,0));
  SetWindowPos(Handle, HWND_TOP, P.X + Parent.Width, P.Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FIsDisplayed := True;
end;

procedure TPopupTrackbar.Close;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  if Assigned(FOnClose) then
  begin
    FOnClose(Self);
  end;
  FIsDisplayed := False;
  ReleaseCapture;
end;

procedure TPopupTrackbar.UpdateCaption;
begin
  Caption := IntToStr(Round(FValue));
end;

procedure TPopupTrackbar.CustomPaint(ACanvas : TCanvas; BColor, FColor : TColor);
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;

  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(ACanvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(ACanvas, Rect, Color, Color, BorderWidth);

  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(ACanvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  with ACanvas do
  begin
    Brush.Color := BColor;
    FillRect(Rect);    
    Brush.Style := bsClear;
    Font := Self.Font;
    Font.Color := FColor;
    FontHeight := TextHeight('W');
    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) div 2;
      Bottom := Top + FontHeight;
    end;
    Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Handle, PChar(Caption), -1, Rect, Flags);
  end;
end;

procedure TPopupTrackbar.Paint;
var
  Rect: TRect;
begin
  FOffscreen.Width := Width;
  FOffscreen.Height := Height;
  FOffscreenHighlight.Width := Width;
  FOffscreenHighlight.Height := Height;

  CustomPaint(FOffscreen.Canvas, clBtnFace, clWindowText);
  CustomPaint(FOffscreenHighlight.Canvas, clHighlight, clHighlightText);

  Rect := GetClientRect;
  Rect.Right := ValueToX(FValue);
  FOffscreen.Canvas.CopyRect(Rect, FOffscreenHighlight.Canvas, Rect);
  Canvas.Draw(0, 0, FOffscreen);
end;

// -----------------------------------------------------------------------------

end.
