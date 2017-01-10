unit DetachedVideoFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, TntMenus;

type
  TDetachedVideoForm = class(TForm)
    VideoPopupMenu: TTntPopupMenu;
    pmiVideoFullscreen: TTntMenuItem;
    pmiVideoNormalSize: TTntMenuItem;
    procedure FormDblClick(Sender: TObject);
    procedure pmiVideoFullscreenClick(Sender: TObject);
    procedure pmiVideoNormalSizeClick(Sender: TObject);
    procedure VideoPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    FNormalLeft, FNormalTop, FNormalWidth, FNormalHeight : Integer;
    function IsFullscreen : Boolean;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetNormalPos(NormalLeft, NormalTop, NormalWidth, NormalHeight: Integer);
    procedure SetVideo25Size;
    procedure SetVideo33Size;
    procedure SetVideo50Size;
    procedure SetVideo75Size;
    procedure SetVideoFullSize;
    procedure SetVideo125Size;
    procedure SetVideo150Size;
    property NormalLeft : Integer read FNormalLeft;
    property NormalTop : Integer read FNormalTop;
    property NormalWidth : Integer read FNormalWidth;
    property NormalHeight : Integer read FNormalHeight;
  end;

var
  DetachedVideoForm: TDetachedVideoForm;

implementation

{$R *.dfm}

uses Main;

procedure TDetachedVideoForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style or WS_THICKFRAME);
  FNormalLeft := 0;
  FNormalTop := 0;
  FNormalWidth := 320;
  FNormalHeight := 240;
end;

procedure TDetachedVideoForm.FormDblClick(Sender: TObject);
begin
  MainForm.ActionDetachVideo.Execute;
end;

procedure TDetachedVideoForm.pmiVideoFullscreenClick(Sender: TObject);
var Monitor : TMonitor;
begin
  if (not IsFullscreen) then
  begin
    FNormalLeft := Self.Left;
    FNormalTop := Self.Top;
    FNormalWidth := Self.Width;
    FNormalHeight := Self.Height;
    Monitor := Screen.MonitorFromWindow(Self.Handle);
    Self.SetBounds(Monitor.Left, Monitor.Top, Monitor.Width, Monitor.Height);
  end;
end;

procedure TDetachedVideoForm.pmiVideoNormalSizeClick(Sender: TObject);
begin
  if IsFullscreen then
  begin
    if FNormalWidth = 0 then
      FNormalWidth := 320;
    if (FNormalHeight = 0) then
      FNormalHeight := 240;
    Self.SetBounds(FNormalLeft, FNormalTop, FNormalWidth, FNormalHeight);
  end;
end;

function TDetachedVideoForm.IsFullscreen : Boolean;
var Monitor : TMonitor;
begin
  Monitor := Screen.MonitorFromWindow(Self.Handle);
  Result := (Monitor.Left = Self.Left) and (Monitor.Top = Self.Top) and
    (Monitor.Width = Self.Width) and (Monitor.Height = Self.Height);
end;

procedure TDetachedVideoForm.VideoPopupMenuPopup(Sender: TObject);
begin
  pmiVideoNormalSize.Checked := not IsFullscreen;
  pmiVideoNormalSize.Enabled := IsFullscreen;
  pmiVideoFullscreen.Checked := IsFullscreen;
  pmiVideoFullscreen.Enabled := not IsFullscreen;
end;

procedure TDetachedVideoForm.SetNormalPos(NormalLeft, NormalTop, NormalWidth, NormalHeight : Integer);
begin
  FNormalLeft := NormalLeft;
  FNormalTop := NormalTop;
  FNormalWidth := NormalWidth;
  FNormalHeight := NormalHeight;
end;

procedure TDetachedVideoForm.SetVideo150Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 + MainForm.VideoRenderer.VideoWidth div 2 div 2,
    MainForm.VideoRenderer.VideoHeight div 2 + MainForm.VideoRenderer.VideoHeight div 2 div 2);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth + MainForm.VideoRenderer.VideoWidth div 2,
    MainForm.VideoRenderer.VideoHeight + MainForm.VideoRenderer.VideoHeight div 2);
  end;
end;

procedure TDetachedVideoForm.SetVideo125Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 + MainForm.VideoRenderer.VideoWidth div 2 div 4,
    MainForm.VideoRenderer.VideoHeight div 2 + MainForm.VideoRenderer.VideoHeight div 2 div 4);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth + MainForm.VideoRenderer.VideoWidth div 4,
    MainForm.VideoRenderer.VideoHeight + MainForm.VideoRenderer.VideoHeight div 4);
  end;
end;

procedure TDetachedVideoForm.SetVideoFullSize;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2,
    MainForm.VideoRenderer.VideoHeight div 2);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth,
    MainForm.VideoRenderer.VideoHeight);
  end;
end;

procedure TDetachedVideoForm.SetVideo75Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 * 3 div 4,
    MainForm.VideoRenderer.VideoHeight div 2 * 3 div 4);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth * 3 div 4,
    MainForm.VideoRenderer.VideoHeight * 3 div 4);
  end;
end;

procedure TDetachedVideoForm.SetVideo50Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 div 2,
    MainForm.VideoRenderer.VideoHeight div 2 div 2);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2,
    MainForm.VideoRenderer.VideoHeight div 2);
  end;
end;

procedure TDetachedVideoForm.SetVideo33Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 div 3,
    MainForm.VideoRenderer.VideoHeight div 2 div 3);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 3,
    MainForm.VideoRenderer.VideoHeight div 3);
  end;
end;

procedure TDetachedVideoForm.SetVideo25Size;
begin
 if MainForm.ConfigObject.DoubledSubResolution AND
     (StrToInt(MainForm.CurrentProject.VideoWidth) < 1280) AND
    (StrToInt(MainForm.CurrentProject.VideoHeight) < 720) then
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 2 div 4,
      MainForm.VideoRenderer.VideoHeight div 2 div 4);
  end
 else
  begin
   Self.SetBounds(FNormalLeft, FNormalTop, MainForm.VideoRenderer.VideoWidth div 4,
    MainForm.VideoRenderer.VideoHeight div 4);
  end;
end;

procedure TDetachedVideoForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  Pt: TPoint;
begin
  Pt := ScreenToClient(SmallPointToPoint(Message.Pos));
  if Pt.Y < 2 then
    Message.Result := HTCAPTION
  else
    inherited;
end;

end.
