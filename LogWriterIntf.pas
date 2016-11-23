unit LogWriterIntf;

interface

type
  TLogWriterIntf = class
  public
    procedure AddTextLine(AText : string); virtual; abstract;
    procedure AddPair(AKey,AValue : string); virtual; abstract;
  end;

  TLogWriterNull = class(TLogWriterIntf)
  public
    procedure AddTextLine(AText : string); override;
    procedure AddPair(AKey,AValue : string); override;
  end;
  
implementation

procedure TLogWriterNull.AddTextLine(AText : string);
begin
  // Do nothing
end;

procedure TLogWriterNull.AddPair(AKey,AValue : string);
begin
  // Do nothing
end;

end.
 