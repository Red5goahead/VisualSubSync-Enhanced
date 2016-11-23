unit VerticalScalingFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, WAVDisplayerUnit;

type
  TVerticalScalingForm = class(TForm)
    GroupBox1: TGroupBox;
    ScalingTrackBar: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    bttReset: TButton;
    Label1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure bttResetClick(Sender: TObject);
    procedure ScalingTrackBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FWAVDisplayer : TWAVDisplayer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; WAVDisplayer : TWAVDisplayer); reintroduce;
  end;

var
  VerticalScalingForm: TVerticalScalingForm;

implementation

uses main;

{$R *.dfm}

constructor TVerticalScalingForm.Create(AOwner: TComponent;
  WAVDisplayer : TWAVDisplayer);
begin
  inherited Create(AOwner);
  FWAVDisplayer := WAVDisplayer;
end;

procedure TVerticalScalingForm.bttResetClick(Sender: TObject);
begin
  ScalingTrackBar.Position := 100;
end;

procedure TVerticalScalingForm.ScalingTrackBarChange(Sender: TObject);
begin
  FWAVDisplayer.VerticalScaling := ScalingTrackBar.Position;
  GroupBox1.Caption := Format(' Scaling : %d%% ',[ScalingTrackBar.Position]);
  MainForm.CurrentProject.IsDirty := True;
end;

procedure TVerticalScalingForm.FormShow(Sender: TObject);
begin
  ScalingTrackBar.Position := FWAVDisplayer.VerticalScaling;
end;

procedure TVerticalScalingForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;
end;

end.
