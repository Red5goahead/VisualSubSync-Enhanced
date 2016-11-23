unit MiniScrollBarUnit;

interface

uses Windows, Messages, Classes, Controls, ExtCtrls, Graphics;

type
  TMiniScrollBarThumb = class(TCustomPanel)
  end;

  TMiniScrollBar = class(TCustomPanel)
  private
    FThumb : TMiniScrollBarThumb;
    FTracking : Boolean;
    FTrackStartPointX : Integer;
    FMax, FMin : Integer;
    FPageSize : Integer;
    FPosition : Integer;
    FThumbMinSize : Integer;
    FOnChange : TNotifyEvent;
    FColorNormal : TColor;
    FColorDown : TColor;
    FRepeatTimer : TTimer;
    FLastXOnMouse : Integer;
    FOnBeginScrolling : TNotifyEvent;
    FOnEndScrolling : TNotifyEvent;

    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnRepeatTimer(Sender: TObject);

    procedure UpdateThumb;
    procedure SetMax(Value : Integer);
    procedure SetMin(Value : Integer);
    procedure SetPageSize(Value : Integer);
    procedure SetPosition(Value : Integer);

  protected
    { Protected declarations }
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetPositionAndPageSize(NewPosition, NewPageSize : Integer);

  published
    property Max : Integer read FMax write SetMax;
    property Min : Integer read FMin write SetMin;
    property PageSize : Integer read FPageSize write SetPageSize;
    property Position : Integer read FPosition write SetPosition;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnBeginScrolling : TNotifyEvent read FOnChange write FOnChange;
    property OnEndScrolling : TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses Math, MiscToolsUnit;

//------------------------------------------------------------------------------

constructor TMiniScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax := 100;
  FMin := 0;
  FPosition := 10;
  FPageSize := 10;
  FThumbMinSize := 5;

  ParentBackground := False; // Control is transparent on WinXP if true
  FColorNormal := $00A56E3A;
  FColorDown := $00D5965E;

  Self.Color := clGray;
  //Self.BevelOuter := bvLowered;
  Self.BevelOuter := bvNone;

  FThumb := TMiniScrollBarThumb.Create(Self);
  FThumb.Parent := Self;
  FThumb.ParentBackground := False; // Control is transparent on WinXP if true
  FThumb.Color := FColorNormal;
  FThumb.BevelOuter := bvNone;

  FThumb.OnMouseDown := OnThumbMouseDown;
  FThumb.OnMouseMove := OnThumbMouseMove;
  FThumb.OnMouseUp := OnThumbMouseUp;

  FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := 150;
  FRepeatTimer.OnTimer := OnRepeatTimer;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.WMSize(var Msg: TWMSize);
begin
  inherited;
  FThumb.Height := Height;
  UpdateThumb;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.OnRepeatTimer(Sender: TObject);
begin
  // Are we on the thumb ?
  if (FLastXOnMouse >= FThumb.Left) and
     (FLastXOnMouse <= FThumb.Left+FThumb.Width) then
  begin
    FRepeatTimer.Enabled := False;
    Exit;
  end;

  if FLastXOnMouse > FThumb.Left then
    Position := Position + PageSize
  else if FLastXOnMouse < FThumb.Left then
    Position := Position - PageSize;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FMax = FMin) and (FMax = 0) then
    Exit;
  if Assigned(FOnBeginScrolling) then
    FOnBeginScrolling(Self);
  if X > FThumb.Left then
    Position := Position + PageSize
  else if X < FThumb.Left then
    Position := Position - PageSize;
  if Assigned(FOnChange) then
    FOnChange(Self);
  FLastXOnMouse := X;
  FRepeatTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FLastXOnMouse := X;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FRepeatTimer.Enabled and Assigned(FOnEndScrolling) then
    FOnEndScrolling(Self);
  FRepeatTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then
    Exit;
  if ssLeft in Shift then
  begin
    FTracking := True;
    FTrackStartPointX := X;
    FThumb.Color := FColorDown;
    FThumb.MouseCapture := True;
    if Assigned(FOnBeginScrolling) then
      FOnBeginScrolling(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var NewX : Integer;
begin
  if (FMax = FMin) and (FMax = 0) then
    Exit;
  if FTracking then
  begin
    NewX := X - FTrackStartPointX + FThumb.Left;
    Constrain(NewX,0,Width - FThumb.Width);
    if FThumb.Left <> NewX then
    begin
      FThumb.Left := NewX;
      FThumb.Color := FColorDown;
      // Update FPosition
      FPosition := MulDiv(NewX,
        (FMax - FMin + 1)-Math.Max(1,FPageSize),
        Width - FThumb.Width)+FMin;

      assert(FPosition >= FMin);
      assert(FPosition <= FMax);

      Constrain(FPosition, FMin, FMax - Math.Max(FPageSize-1,0));
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then
    Exit;
  FThumb.MouseCapture := False;
  FTracking := False;
  FThumb.Color := clBlue;
  FThumb.Color := FColorNormal;
  if Assigned(FOnEndScrolling) then
    FOnEndScrolling(Self);
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.SetMax(Value : Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;    
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.SetMin(Value : Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.SetPageSize(Value : Integer);
begin
  Constrain(Value,0,FMax - FMin + 1);
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.SetPosition(Value : Integer);
begin
  Constrain(Value,FMin,FMax - Math.Max(FPageSize-1,0));
  if Value <> FPosition then
  begin
    FPosition := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize : Integer);
begin
  Constrain(NewPageSize,0,FMax - FMin + 1);
  Constrain(NewPosition,FMin,FMax - Math.Max(NewPageSize-1,0));
  if (NewPageSize <> FPageSize) or (NewPosition <> FPosition) then
  begin
    FPageSize := NewPageSize;
    FPosition := NewPosition;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TMiniScrollBar.UpdateThumb;
var NewThumbWidth, NewPosition, MaxMin : Integer;
begin
  MaxMin := FMax - FMin + 1;
  if FPageSize = 0 then
    NewThumbWidth := FThumbMinSize
  else
  begin
    NewThumbWidth := MulDiv(FPageSize, Width,MaxMin);
    if NewThumbWidth < FThumbMinSize then
      NewThumbWidth := FThumbMinSize;
  end;

  NewPosition := MulDiv(FPosition - FMin,Width - NewThumbWidth, MaxMin - Math.Max(1,FPageSize));
  Constrain(NewPosition, 0, Width - NewThumbWidth);

  FThumb.SetBounds(NewPosition,FThumb.Top,NewThumbWidth,FThumb.Height);
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
