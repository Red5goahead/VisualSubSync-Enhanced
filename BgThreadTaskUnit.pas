unit BgThreadTaskUnit;

interface

uses Classes;

type
  TBgThreadTask = class(TThread)
  private
    FOnExecute : TNotifyEvent;
    FParam : Integer;
  public
    constructor Create(CreateSuspended : Boolean; ThreadPriority : TThreadPriority); overload;
    procedure Execute; override;
    property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
    property Param : Integer read FParam write FParam;
  end;

implementation

constructor TBgThreadTask.Create(CreateSuspended : Boolean; ThreadPriority : TThreadPriority);
begin
  inherited Create(CreateSuspended);
  Self.Priority := ThreadPriority;
end;

procedure TBgThreadTask.Execute;
begin
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self);
  finally
  end;
end;


end.
