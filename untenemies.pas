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
  IVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}IHashMap<TVec2i, Integer>;
  TVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}THashMap<TVec2i, Integer>;

  TBotState = (bsNothing, bsSeeEnemy, bsLostEnemy, bsRetreat, bsPatrol);

  { TbsDesc }

  TbsDesc = class
    procedure ClearState(); virtual;
    procedure NewTurn(); virtual;
  end;

  { TbsDesc_SeeEnemy }

  TbsDesc_SeeEnemy = class (TbsDesc)
    Enemy: TRoomUnit;
    OptimalRoute: IRoomPath;
    procedure SetState(const AEnemy: TRoomUnit);
    procedure ClearState(); override;
    //procedure NewTurn(); override;
  end;

  { TbsDesc_LostEnemy }

  TbsDesc_LostEnemy = class (TbsDesc)
    LastPt : TVec2i;
    CheckPt: IVec2iArr;
    procedure SetState(const ALastState: TbsDesc_SeeEnemy);
    procedure ClearState(); override;
    procedure AfterConstruction; override;
  end;

  { TbsDesc_Retreat }

  TbsDesc_Retreat = class (TbsDesc)
    DangerousPts: IVec2iArr;
    procedure SetState(const ABot: TRoomUnit; const ADangerPts: IVec2iArr); overload;
    procedure SetState(const ABot: TRoomUnit; const ADangerPt: TVec2i); overload;
    procedure ClearState(); override;
    procedure AfterConstruction; override;
  end;

  { TbsDesc_Patrol }

  TbsDesc_Patrol = class (TbsDesc)
    WayPts  : IVec2iArr;
    CurrPath: IRoomPath;
    procedure SetState_Random(ABot: TRoomUnit);

    procedure ClearState(); override;
    procedure AfterConstruction; override;
  end;

  { TBot }

  TBot = class (TRoomUnit)
  protected type
    ILastSeenMap = {$IfDef FPC}specialize{$EndIf}IHashMap<IWeakRef, TVec2i>;
    TLastSeenMap = {$IfDef FPC}specialize{$EndIf}THashMap<IWeakRef, TVec2i>;
    ILastSeenArr = {$IfDef FPC}specialize{$EndIf}IArray<IWeakRef>;
    TLastSeenArr = {$IfDef FPC}specialize{$EndIf}TArray<IWeakRef>;

    IVec4iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec4i>;
    TVec4iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec4i>;

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
  protected
    FBState: TBotState;
    FBS_SeeEnemy : TbsDesc_SeeEnemy;
    FBS_LostEnemy: TbsDesc_LostEnemy;
    FBS_Retreat  : TbsDesc_Retreat;
    FBS_Patrol   : TbsDesc_Patrol;
  public
    property BState: TBotState read FBState;

    procedure NewTurn(); virtual;
    function DoAction(): IBRA_Action; virtual; abstract;
  public
    destructor Destroy; override;
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

{ TbsDesc_Patrol }

procedure TbsDesc_Patrol.SetState_Random(ABot: TRoomUnit);
const cRadius = 2;
var RndPt: TVec2i;
    i: Integer;
begin
  ClearState();

  for i := 0 to 9 do
  begin
    RndPt := ABot.RoomPos;
    RndPt.x := RndPt.x + Random(cRadius*2+1) - cRadius;
    RndPt.y := RndPt.y + Random(cRadius*2+1) - cRadius;
    if ABot.Room.Distance(RndPt, ABot.RoomPos) < cRadius then Continue;
    if ABot.Room.ObjectAt(RndPt) <> nil then Continue;
    CurrPath := ABot.FindPath(RndPt);
    if CurrPath <> nil then
    begin
      WayPts.Add(RndPt);
      CurrPath.Add(RndPt);
      Exit;
    end;
  end;
end;

procedure TbsDesc_Patrol.ClearState();
begin
  WayPts.Clear();
  CurrPath := nil;
end;

procedure TbsDesc_Patrol.AfterConstruction;
begin
  inherited AfterConstruction;
  WayPts := TVec2iArr.Create;
end;

{ TbsDesc_Retreat }

procedure TbsDesc_Retreat.SetState(const ABot: TRoomUnit; const ADangerPts: IVec2iArr);
var graph: IRoomMapNonWeightedGraph;
    bfs: IRoomMapBFS;
    pt: TVec2i;
    depth, i: Integer;

    distanceSet: IVec2iWeightMap;
begin
  DangerousPts := ADangerPts;

  graph := TRoomMapGraphExcludeSelfAndTarget.Create(ABot.Room, ABot, nil);

  distanceSet := TVec2iWeightMap.Create();
  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(DangerousPts);
  while bfs.Next(pt, depth) do
    distanceSet.Add(pt, depth);

  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(ABot.RoomPos);
  //while bfs.Next(pt, depth) do
  //begin
  //  movePt4.xy := pt;
  //  if not enemySet.TryGetValue(pt, movePt4.z) then movePt4.z := 0;
  //  movePt4.w := depth;
  //  movePts.Add(movePt4);
  //end;
end;

procedure TbsDesc_Retreat.SetState(const ABot: TRoomUnit; const ADangerPt: TVec2i);
var arr: IVec2iArr;
begin
  arr := TVec2iArr.Create;
  arr.Add(ADangerPt);
  SetState(ABot, arr);
end;

procedure TbsDesc_Retreat.ClearState();
begin
  inherited ClearState();
  DangerousPts.Clear();
end;

procedure TbsDesc_Retreat.AfterConstruction;
begin
  DangerousPts := TVec2iArr.Create;
end;

{ TbsDesc_LostEnemy }

procedure TbsDesc_LostEnemy.SetState(const ALastState: TbsDesc_SeeEnemy);
begin

end;

procedure TbsDesc_LostEnemy.ClearState();
begin
  CheckPt.Clear();
end;

procedure TbsDesc_LostEnemy.AfterConstruction;
begin
  inherited AfterConstruction;
  CheckPt := TVec2iArr.Create;
end;

{ TbsDesc }

procedure TbsDesc.ClearState();
begin

end;

procedure TbsDesc.NewTurn();
begin

end;

{ TbsDesc_SeeEnemy }

procedure TbsDesc_SeeEnemy.SetState(const AEnemy: TRoomUnit);
begin
  ClearState();
  Enemy := AEnemy;
end;

procedure TbsDesc_SeeEnemy.ClearState();
begin
  Enemy := nil;
  OptimalRoute := nil;
end;

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

  function GetOptimalRangePosition(const ATargetPos: TVec2i): IRoomPath;
  const
    cBestDistance = 9;
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

  function GetOptimalRangePosition(const ATargetPos: TVec2i): IRoomPath;
  const
    cBestDistance = 9;
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

var
  path: IRoomPath;
  availPts: Integer;
  enemy: TRoomUnit;
begin
  if AP = 0 then Exit(nil);

  {
  case FBState of
    bsNothing:
      begin
        enemy := FindEnemy();
        if enemy = nil then
        begin
          FBState := bsPatrol;
          FBS_Patrol.SetState_Random(Self);
        end
        else
        begin
          FBState := bsSeeEnemy;
          FBS_SeeEnemy.SetState(enemy);
        end;
        Result := DoAction();
      end;

    bsSeeEnemy:
      begin
        if FBS_SeeEnemy.OptimalRoute = nil then
          FBS_SeeEnemy.OptimalRoute := GetOptimalRangePosition(FBS_SeeEnemy.Enemy.RoomPos);
        if FBS_SeeEnemy.OptimalRoute = nil then
        begin
          FBState := bsRetreat;
          FBS_Retreat.SetState(Self, FBS_SeeEnemy.Enemy.GetShootPoints());
        end
        else
        begin
          if not InViewField(FBS_SeeEnemy.Enemy.RoomPos) then
          begin
            Result := TBRA_UnitTurnAction.Create(Self, FBS_SeeEnemy.Enemy.RoomPos);
            Exit;
          end;

          availPts := AP - FBS_SeeEnemy.OptimalRoute.Count;
          if (FEquippedItem[esBothHands] <> nil) and (availPts > FEquippedItem[esBothHands].ActionCost(0)) then
          begin
            if (FBS_SeeEnemy.OptimalRoute.Count = 0) or
               (FRoom.Distance(RoomPos, FBS_SeeEnemy.Enemy.RoomPos) < FRoom.Distance(FBS_SeeEnemy.OptimalRoute.Last, FBS_SeeEnemy.Enemy.RoomPos)) then
            begin
              Result := FEquippedItem[esBothHands].DoAction(0, Self, FBS_SeeEnemy.Enemy);
              Exit;
            end
            else
            begin
              Result := TBRA_UnitMovementAction.Create(Self, FBS_SeeEnemy.OptimalRoute);
              FBS_SeeEnemy.OptimalRoute.Clear();
              Exit;
            end;
          end
          else
          begin
            if FBS_SeeEnemy.OptimalRoute.Count = 0 then
            begin
              Exit(nil);
            end
            else
            begin
              Result := TBRA_UnitMovementAction.Create(Self, FBS_SeeEnemy.OptimalRoute);
              FBS_SeeEnemy.OptimalRoute.Clear();
              Exit;
            end;
          end;
        end;
      end;

    bsRetreat:
      begin

      end;
  end;

  Exit;
  }
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

  FBS_SeeEnemy := TbsDesc_SeeEnemy.Create;
  FBS_LostEnemy:= TbsDesc_LostEnemy.Create;
  FBS_Retreat  := TbsDesc_Retreat.Create;
  FBS_Patrol   := TbsDesc_Patrol.Create;
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

destructor TBot.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBS_SeeEnemy);
  FreeAndNil(FBS_LostEnemy);
  FreeAndNil(FBS_Retreat);
  FreeAndNil(FBS_Patrol);
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

