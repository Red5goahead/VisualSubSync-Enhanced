unit CursorManager;

interface

uses Controls;

type
  ICursorManager = interface
    ['{333A50A2-A6CE-447E-BC20-83BAE8948054}']
  end;

  TCursorManager = class(TInterfacedObject, ICursorManager)
  private
    FOldCursor : TCursor;
  public
    constructor Create; overload;
    constructor Create(NewCursor : TCursor); overload;
    destructor Destroy; override;
  end;

implementation

uses Forms;

constructor TCursorManager.Create;
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
end;

constructor TCursorManager.Create(NewCursor : TCursor);
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := NewCursor;
end;

destructor TCursorManager.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;

end.
