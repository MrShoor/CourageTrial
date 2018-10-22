unit untObstacles;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, mutils, avContnrs;

type
  IObstacleCellsArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec3i>;
  TObstacleCellsArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec3i>;

  TObstacleDesc = record
    name : string;
    cells: IObstacleCellsArr;
  end;
  PObstacleDesc = ^TObstacleDesc;
  TObstacleArr = {$IfDef FPC}specialize{$EndIf}TArray<TObstacleDesc>;
  IObstacleArr = {$IfDef FPC}specialize{$EndIf}IArray<TObstacleDesc>;

function LoadObstacles(const AFileName: string): IObstacleArr;

implementation

const
  cEmptyObstacle: TObstacleDesc = (name: ''; cells: nil);

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

      if s[1] = 'O' then
        pObst^.cells.Add(ParseCoords(s));
    end;
  finally
    FreeAndNil(sl);
  end;
end;

end.

