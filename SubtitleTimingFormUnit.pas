unit SubtitleTimingFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask, TntStdCtrls;

type
  TSubtitleTimingForm = class(TForm)
    bttClose: TTntButton;
    brrOk: TTntButton;
    edTime: TMaskEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bttCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SubtitleTimingForm: TSubtitleTimingForm;

implementation

{$R *.dfm}

procedure TSubtitleTimingForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TSubtitleTimingForm.bttCloseClick(Sender: TObject);
begin
 Close;
end;

end.
