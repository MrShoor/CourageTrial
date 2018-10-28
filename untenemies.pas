unit untEnemies;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Math,
  intfUtils,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avModel,
  avPathFinder, untLevel;

type
  { TBot }

  TBot = class (TRoomUnit)
  protected type
    ILastSeenMap = {$IfDef FPC}specialize{$EndIf}IHashMap<IWeakRef, TVec2i>;
    TLastSeenMap = {$IfDef FPC}specialize{$EndIf}THashMap<IWeakRef, TVec2i>;
    ILastSeenArr = {$IfDef FPC}specialize{$EndIf}IArray<IWeakRef>;
    TLastSeenArr = {$IfDef FPC}specialize{$EndIf}TArray<IWeakRef>;

    IVec2iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec2i>;
    TVec2iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec2i>;

    IVec4iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec4i>;
    TVec4iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec4i>;

    IVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}IHashMap<TVec2i, Integer>;
    TVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}THashMap<TVec2i, Integer>;

    TMovePtsComparer = class(TInterfacedObject, IComparer)
    public
      function Compare(const Left, Right): Integer;
    end;

  protected
    procedure AfterRegister; override;
    function IsEnemy(const ARoomUnit: TRoomUnit): Boolean;
    function FindEnemy(): TRoomUnit; virtual;

    function GetPointsOnRange(const AStartPoint: TVec2i; const ARange: Integer): IVec2iArr;
  protected
    FBotID: Integer;
    FLastSeen: ILastSeenMap;
    function GetMovePath(): IRoomPath;
    procedure AddToLastSeenMap(const AUnit: TRoomUnit);
    procedure OnRegisterRoomObject(const ANewObject: TRoomObject); override;
  public
    procedure NewTurn(); virtual;
    function DoAction(): IBRA_Action; virtual; abstract;
  end;
  TBotArr = {$IfDef FPC}specialize{$EndIf} TArray<TBot>;
  IBotArr = {$IfDef FPC}specialize{$EndIf} IArray<TBot>;
  TBotSet = {$IfDef FPC}specialize{$EndIf} THashSet<TBot>;
  IBotSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TBot>;


  { TBotMutant1 }

  TBotMutant1 = class (TBot)
  private
    FAnim: IavAnimationController;

    FKick: IUnitItem;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;

    function DoAction(): IBRA_Action; override;
  end;

  { TBotArcher1 }

  TBotArcher1 = class (TBot)
  private
    FAnim: array of IavAnimationController;

    FOptimalPosPath: IRoomPath;
    FPlayer: TRoomUnit;
    FNeedTurnToPlayer: Boolean;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;

    procedure NewTurn(); override;
    function DoAction(): IBRA_Action; override;
  end;

implementation

uses
  untItems;

var gvBotID: Integer = 0;

{ TBot.TMovePtsComparer }

function TBot.TMovePtsComparer.Compare(const Left, Right): Integer;
var L: TVec4i absolute Left;
    R: TVec4i absolute Right;
begin
  Result := L.z - R.z;
  if Result = 0 then
    Result := L.w - R.w;
end;

{ TBotArcher1 }

procedure TBotArcher1.UpdateStep;
var i: Integer;
begin
  inherited UpdateStep;
  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].SetTime(World.GameTime);
end;

procedure TBotArcher1.SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean);
var i: Integer;
begin
  inherited SetAnimation(ANameSequence, ALoopedLast);
  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].AnimationSequence_StartAndStopOther(AddAnimationPrefix(ANameSequence), ALoopedLast);
end;

function TBotArcher1.GetUnitMoveSpeed: Single;
begin
  Result := 5;
end;

procedure TBotArcher1.LoadModels();
var
  i: Integer;
  bow: IUnitItem;
begin
  ViewRange := 16;
  ViewAngle := Pi/3+EPS;
  ViewWholeRange := 2.5;

  AddModel('Archer_Body', mtDefault);
  AddModel('Archer_Clothes', mtDefault);
  AddModel('Archer_Eyelashes', mtDefault);
  AddModel('Archer_Eyes', mtDefault);

  bow := TArcherBow.Create;
  Equip(bow);
  FInventory.Add(bow);

  FAnimationPrefix := 'Archer_';

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
    FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
  SetAnimation(['Idle0'], True);

  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;

  Preview96_128 := 'ui\units\archer.png';
end;

procedure TBotArcher1.NewTurn();

const
  cBestDistance = 9;

  function GetOptimalRangePosition(const ATargetPos: TVec2i): IRoomPath;
  var points: IVec2iArr;
      i: Integer;
  begin
    points := GetPointsOnRange(ATargetPos, cBestDistance);
    for i := 0 to points.Count - 1 do
      if FRoom.RayCastBoolean(points[i], ATargetPos) then
      begin
        Result := FindPath(points[i]);
        if RoomPos <> points[i] then
          Result.Add(points[i]);
        Exit;
      end;
    Result := TRoomPath.Create();
  end;

begin
  inherited NewTurn();
  FPlayer := FindEnemy();
  if FPlayer <> nil then
    FOptimalPosPath := GetOptimalRangePosition(FPlayer.RoomPos)
  else
    FOptimalPosPath := nil;
  FNeedTurnToPlayer := False;
end;

function TBotArcher1.DoAction(): IBRA_Action;
var
  path: IRoomPath;
  availPts: Integer;
  bullet: TRoomBullet;
begin
  if AP = 0 then Exit(nil);

  if FNeedTurnToPlayer then
  begin
    FNeedTurnToPlayer := False;
    if not CanSee(FPlayer) then
    begin
      Result := TBRA_UnitTurnAction.Create(Self, FPlayer.RoomPos);
      Exit;
    end;
  end;

  if FPlayer <> nil then
  begin
    availPts := AP - FOptimalPosPath.Count;
    if availPts > 3 then
    begin
      if (FOptimalPosPath.Count = 0) or
         (FRoom.Distance(RoomPos, FPlayer.RoomPos) < FRoom.Distance(FOptimalPosPath.Last, FPlayer.RoomPos)) then
      begin
        Result := FEquippedItem[esBothHands].DoAction(0, Self, FPlayer);
      end
      else
      begin
        Result := TBRA_UnitMovementAction.Create(Self, FOptimalPosPath);
        FOptimalPosPath := TRoomPath.Create();
        FNeedTurnToPlayer := True;
      end;
    end
    else
    begin
      if (FOptimalPosPath.Count = 0) then
        Exit(nil);
      Result := TBRA_UnitMovementAction.Create(Self, FOptimalPosPath);
      FOptimalPosPath := TRoomPath.Create();
      FNeedTurnToPlayer := True;
    end;
    Exit;
  end;

  path := GetMovePath();
  if (path <> nil) and (path.Count > 0) then
  begin
    Result := TBRA_UnitMovementAction.Create(Self, path);
    Exit;
  end;
  Exit(nil);
end;

{ TBot }

procedure TBot.AfterRegister;
begin
  inherited AfterRegister;
  FLastSeen := TLastSeenMap.Create();
  SubscribeForUpdateStep;
  FBotID := gvBotID;
  Inc(gvBotID);
end;

function TBot.IsEnemy(const ARoomUnit: TRoomUnit): Boolean;
begin
  Result := ARoomUnit is TPlayer;
  //Result := False;
end;

function TBot.FindEnemy(): TRoomUnit;
var i: Integer;
begin
  for i := 0 to Room.ChildCount() - 1 do
  begin
    if (Room.Child[i] is TRoomUnit) and IsEnemy(TRoomUnit(Room.Child[i])) then
    begin
      Result := Room.Child[i] as TRoomUnit;
      if not Result.IsDead() then
        if CanSee(Result) then
          Exit;
    end;
  end;
  Result := nil;
end;

function TBot.GetPointsOnRange(const AStartPoint: TVec2i; const ARange: Integer): IVec2iArr;
var graph: IRoomMapNonWeightedGraph;
    bfs: IRoomMapBFS;
    pt: TVec2i;
    depth, i: Integer;

    enemySet: IVec2iWeightMap;
    movePts: IVec4iArr;
    movePt4: TVec4i;
    moveCmp: IComparer;
begin
  enemySet := TVec2iWeightMap.Create();
  movePts := TVec4iArr.Create();

  graph := TRoomMapGraphExcludeSelfAndTarget.Create(FRoom, Self, nil);

  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(AStartPoint);
  while bfs.Next(pt, depth) do
    enemySet.Add(pt, abs(depth-ARange));

  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(RoomPos);
  while bfs.Next(pt, depth) do
  begin
    movePt4.xy := pt;
    if not enemySet.TryGetValue(pt, movePt4.z) then movePt4.z := 0;
    movePt4.w := depth;
    movePts.Add(movePt4);
  end;

  moveCmp := TMovePtsComparer.Create;
  movePts.Sort(moveCmp);

  Result := TVec2iArr.Create();
  Result.Capacity := movePts.Count;
  for i := 0 to movePts.Count - 1 do
    Result.Add(movePts[i].xy);
end;

function TBot.GetMovePath(): IRoomPath;

  function FindRandomPath(): IRoomPath;
  const cRadius = 2;
  var RndPt: TVec2i;
      i: Integer;
  begin
    Result := nil;
    for i := 0 to 9 do
    begin
      RndPt := RoomPos;
      RndPt.x := RndPt.x + Random(cRadius*2+1) - cRadius;
      RndPt.y := RndPt.y + Random(cRadius*2+1) - cRadius;
      if Room.Distance(RndPt, RoomPos) < cRadius then Continue;
      if Room.ObjectAt(RndPt) <> nil then Continue;
      Result := FindPath(RndPt);
      if Result <> nil then
      begin
        Result.Add(RndPt);
        Exit;
      end;
    end;
  end;

var obj: IWeakRef;
    pt: TVec2i;
    toDelete: ILastSeenArr;
    i: Integer;
begin
  toDelete := TLastSeenArr.Create();
  try
    FLastSeen.Reset;
    while FLastSeen.Next(obj, pt) do
    begin
      if (pt = RoomPos) or (obj.Obj = nil) then
      begin
        toDelete.Add(obj);
        Continue;
      end;
      Result := FindPath(pt);
      if (Result <> nil) then
      begin
        Result.Add(pt);
        Result.SetSize(1);
        Exit;
      end;
    end;

    Result := FindRandomPath();
  finally
    for i := 0 to toDelete.Count - 1 do
      FLastSeen.Delete(toDelete[i]);
  end;
end;

procedure TBot.AddToLastSeenMap(const AUnit: TRoomUnit);
begin
  FLastSeen.AddOrSet(AUnit.WeakRef, AUnit.RoomPos);
end;

procedure TBot.OnRegisterRoomObject(const ANewObject: TRoomObject);
begin
  if ANewObject is TRoomUnit then
      if IsEnemy(TRoomUnit(ANewObject)) then
        if CanSee(TRoomUnit(ANewObject)) then
          AddToLastSeenMap(TRoomUnit(ANewObject));
end;

procedure TBot.NewTurn();
begin

end;

{ TBotMutant1 }

procedure TBotMutant1.UpdateStep;
begin
  inherited UpdateStep;
  FAnim.SetTime(World.GameTime);
end;

procedure TBotMutant1.SetAnimation(const ANameSequence: array of string;
  const ALoopedLast: Boolean);
begin
  inherited SetAnimation(ANameSequence, ALoopedLast);
  FAnim.AnimationSequence_StartAndStopOther(ANameSequence, ALoopedLast);
end;

function TBotMutant1.GetUnitMoveSpeed: Single;
begin
  Result := 5;
end;

procedure TBotMutant1.LoadModels;
begin
  ViewRange := 10;
  ViewAngle := Pi/3+EPS;
  ViewWholeRange := 2.5;

  AddModel('Mutant1', mtDefault);
  FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);
  SetAnimation(['Idle0'], True);

  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;

  Preview96_128 := 'ui\units\mutant1.png';

  FKick := TDefaultKick.Create;
end;

function TBotMutant1.DoAction: IBRA_Action;
var player: TRoomUnit;
    path: IRoomPath;
begin
  player := FindEnemy();
  if AP = 0 then Exit(nil);

  if player = nil then
  begin
    path := GetMovePath();
    if (path <> nil) and (path.Count > 0) then
    begin
      Result := TBRA_UnitMovementAction.Create(Self, path);
      Exit
    end;
    Exit(nil);
  end;

  if FKick.CanUse(0, Self, player) then
  begin
    Result := FKick.DoAction(0, Self, player);
    Exit;
  end;

  if Room.Distance(player.RoomPos, RoomPos) > 1 then
  begin
    path := FindPath(player.RoomPos, player);
    if (path <> nil) and (path.Count > 0) then
    begin
      Result := TBRA_UnitMovementAction.Create(Self, path);
    end;
  end;
end;

end.

