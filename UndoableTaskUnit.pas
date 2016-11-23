// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2007 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------
unit UndoableTaskUnit;

interface

type
  TUndoableTask = class(TObject)
  public
    procedure DoTask; virtual; abstract;
    function GetName : WideString; virtual; abstract;
    function Merge(UndoableTask : TUndoableTask) : Boolean; virtual;
    procedure UndoTask; virtual; abstract;
  end;

implementation

function TUndoableTask.Merge(UndoableTask : TUndoableTask) : Boolean;
begin
  // Nothing to merge by default
  Result := False;
end;

end.
 