unit untObstacles;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, mutils, avContnrs, avTypes;

type
  IObstacleCellsArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec3i>;
  TObstacleCellsArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec3i>;

  { TObstacleDesc }

  TObstacleDesc = record
    name   : string;
    clsname: string;
    cells  : IObstacleCellsArr;
    procedure WriteStream(const AStream: TStream);
    procedure ReadStream(const AStream: TStream);
  end;
  PObstacleDesc = ^TObstacleDesc;
  TObstacleArr = {$IfDef FPC}specialize{$EndIf}TArray<TObstacleDesc>;
  IObstacleArr = {$IfDef FPC}specialize{$EndIf}IArray<TObstacleDesc>;

function LoadObstacles(const AFileName: string): IObstacleArr;

implementation

const
  cEmptyObstacle: TObstacleDesc = (name: ''; clsname: 'TObstacle'; cells: nil);

function LoadObstacles(const AFileName: string): IObstacleArr;

  function ParseName(const s: string): string;
  var sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Delimiter := ' ';
      sl.DelimitedText := s;
      Result := sl[1];
    finally
      sl.Free;
    end;
  end;

  function ParseCoords(const s: string): TVec3i;
  var sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Delimiter := ' ';
      sl.DelimitedText := s;
      Result.x := StrToInt(sl[1]);
      Result.y := StrToInt(sl[2]);
      Result.z := StrToInt(sl[3]);
    finally
      sl.Free;
    end;
  end;

var sl: TStringList;
    s: string;
    pObst: PObstacleDesc;
    i: Integer;
begin
  Result := TObstacleArr.Create();
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    for i := 0 to sl.Count - 1 do
    begin
      s := sl.Strings[i];
      if s[1] = 'N' then
      begin
        pObst := Result.PItem[Result.Add(cEmptyObstacle)];
        pObst^.name := ParseName(s);
        pObst^.cells := TObstacleCellsArr.Create();
      end;

      if s[1] = 'C' then
        pObst^.clsname := ParseName(s);

      if s[1] = 'O' then
        pObst^.cells.Add(ParseCoords(s));
    end;
  finally
    FreeAndNil(sl);
  end;
end;

{ TObstacleDesc }

procedure TObstacleDesc.WriteStream(const AStream: TStream);
var n: Integer;
begin
  StreamWriteString(AStream, name);
  StreamWriteString(AStream, clsname);
  n := cells.Count;
  AStream.WriteBuffer(n, SizeOf(n));
  if n > 0 then
    AStream.WriteBuffer(cells.PItem[0]^, n*SizeOf(cells.Item[0]));
end;

procedure TObstacleDesc.ReadStream(const AStream: TStream);
var n: Integer;
begin
  n := 0;
  StreamReadString(AStream, name);
  StreamReadString(AStream, clsname);
  AStream.ReadBuffer(n, SizeOf(n));
  cells := TObstacleCellsArr.Create();
  if n > 0 then
  begin
    cells.SetSize(n);
    AStream.ReadBuffer(cells.PItem[0]^, n*SizeOf(cells.Item[0]));
  end;
end;

end.

