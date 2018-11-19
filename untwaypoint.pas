unit untWayPoint;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avBase, avRes, mutils, avTypes, avTess, avContnrsDefaults,
  untLevel;

type

  { TRoomPoint }

  TRoomPoint = class(TObstacle)
  private
  protected
    procedure AfterRegister; override;
  public
    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    function BlockedViewCell(AIndex: Integer): Boolean; override;

    function GetVisible(): Boolean; override;
  end;

  { TRoomWaypoint }

  TRoomWaypoint = class(TRoomPoint)
  private
  public
  end;

function FindNextWaypoint(const AFrom: TRoomUnit; const ALastWaypoints: IVec2iArr; PeekRange: Integer = 3): TVec2i;

implementation

function FindNextWaypoint(const AFrom: TRoomUnit; const ALastWaypoints: IVec2iArr; PeekRange: Integer): TVec2i;

  function GetRandomPoint: TVec2i;
  const cRadius = 2;
  var RndPt: TVec2i;
      i: Integer;
  begin
      for i := 0 to 9 do
      begin
        RndPt := AFrom.RoomPos;
        RndPt.x := RndPt.x + Random(cRadius*2+1) - cRadius;
        RndPt.y := RndPt.y + Random(cRadius*2+1) - cRadius;
        if not AFrom.Room.IsCellExists(RndPt) then Continue;
        if AFrom.Room.ObjectAt(RndPt) <> nil then Continue;
        Exit(RndPt);
      end;
      Result := AFrom.RoomPos;
  end;

var bfs: IRoomMapBFS;
    map: IRoomMapNonWeightedGraph;
    pts: IVec2iArr;
    pt: TVec2i;

    waypts: IVec2iSet;
    i: Integer;
begin
  waypts := TVec2iSet.Create();
  for i := 0 to AFrom.Room.ChildCount - 1 do
  begin
    if AFrom.Room.Child[i] is TRoomWaypoint then
      waypts.AddOrSet(TRoomWaypoint(AFrom.Room.Child[i]).RoomPos);
  end;
  if waypts.Count = 0 then
  begin
    Result := GetRandomPoint;
    Exit;
  end;

  pts := TVec2iArr.Create();
  map := TRoomMapGraphExcludeSelfAndTarget.Create(AFrom.Room, AFrom, nil);
  bfs := TRoomMapBFS.Create(map);
  bfs.Reset(AFrom.RoomPos);
  while bfs.Next(pt) do
  begin
    if waypts.Contains(pt) then
      if ALastWaypoints.IndexOf(pt) < 0 then
      begin
        pts.Add(pt);
        if pts.Count >= PeekRange then Break;
      end;
  end;

  if pts.Count = 0 then
  begin
    Result := GetRandomPoint;
    Exit;
  end;

  Result := pts[Random(pts.Count)];
end;

{ TRoomPoint }

procedure TRoomPoint.AfterRegister;
begin
  inherited AfterRegister;
  if not Room.InEditMode then
    FNonRegistrable := True;
end;

function TRoomPoint.BlockedCellsCount: Integer;
begin
  if Room.InEditMode then
    Result := inherited BlockedCellsCount
  else
    Result := 0;
end;

function TRoomPoint.GetBlockedCell(AIndex: Integer): TVec2i;
begin
  if Room.InEditMode then
    Result := inherited GetBlockedCell(AIndex)
  else
    Result := Vec(0,0);
end;

function TRoomPoint.BlockedViewCell(AIndex: Integer): Boolean;
begin
  if Room.InEditMode then
    Result := inherited BlockedViewCell(AIndex)
  else
    Result := False;
end;

function TRoomPoint.GetVisible(): Boolean;
begin
  Result := Room.InEditMode;
end;

initialization
  RegRoomClass(TRoomWaypoint);

end.

