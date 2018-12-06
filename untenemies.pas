unit untEnemies;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Math,
  intfUtils,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avModel,
  untLevel,
  untWayPoint;

type
  IVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}IHashMap<TVec2i, Integer>;
  TVec2iWeightMap = {$IfDef FPC}specialize{$EndIf}THashMap<TVec2i, Integer>;
  IVec4iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec4i>;
  TVec4iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec4i>;

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
  end;

  { TbsDesc_LostEnemy }

  TbsDesc_LostEnemy = class (TbsDesc)
    LastPt : TVec2i;
    CheckPt: IVec2iSet;
    OptimalRoute: IRoomPath;
    SkipTurn: Boolean;
    Step: Integer;
    procedure SetState(const ALastSeenPos: TVec2i; const ACheckPts: IVec2iSet);
    procedure ClearState(); override;
    procedure AfterConstruction; override;
  end;

  { TbsDesc_Retreat }

  TbsDesc_Retreat = class (TbsDesc)
  private type
    TTargetPtsComparer = class(TInterfacedObject, IComparer)
      function Compare(const Left, Right): Integer;
    end;
  public
    DangerousPts: IVec2iArr;
    OptimalRoute: IRoomPath;
    EnemyPt: TVec2i;
    NeedTurnToEnemy: Boolean;
    Step: Integer;
    procedure RecalculateOptimalRoute(const ABot: TRoomUnit; const AReservedPts: Integer);
    procedure SetState(const ABot: TRoomUnit; const ADangerPts: IVec2iArr; const AReservedPts: Integer); overload;
    procedure SetState(const ABot: TRoomUnit; const ADangerPt: TVec2i; const AReservedPts: Integer); overload;
    procedure SetEnemyPt(const APt: TVec2i);
    procedure ClearState(); override;
    procedure AfterConstruction; override;
  end;

  { TbsDesc_Patrol }

  TbsDesc_Patrol = class (TbsDesc)
    WayPts  : IVec2iArr;
    CurrPath: IRoomPath;
    procedure SetState_Random(ABot: TRoomUnit);
    procedure SetState(ABot: TRoomUnit);

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

    TMovePtsComparer = class(TInterfacedObject, IComparer)
    public
      function Compare(const Left, Right): Integer;
    end;

    THidePtsComparer = class(TInterfacedObject, IComparer)
    public
      function Compare(const Left, Right): Integer;
    end;

    TMoveFilter = class(TInterfacedObject, IRoomCellFilter)
    private
      FBot : TBot;
      FTarget: TRoomUnit;
    public
      function IsValid(const ACell: TVec2i): Boolean;
      constructor Create(const ABot: TBot; const ATarget: TRoomUnit);
    end;

  protected
    procedure AfterRegister; override;
    function IsEnemy(const ARoomUnit: TRoomUnit): Boolean;
    function FindEnemy(): TRoomUnit; virtual;

    function GetPointsOnRange(const AStartPoint: TVec2i; const ARange, AMovePoints: Integer): IVec2iArr;

    function GetHiddenNeighbours(const AOriginPts: IVec2iArr): IVec2iArr; overload;
    function GetHiddenNeighbours(const AOriginPt: TVec2i): IVec2iArr; overload;

    function Action_MoveWithRoute(const ARoute: IRoomPath): IBRA_Action;
  protected
    FBotID: Integer;
    FLastSeen: ILastSeenMap;

    FEmptyCells: IVec2iSet;

    FMoveAction: IBRA_Action;

    FRetreatLimits: Integer;

    function GetMovePath(): IRoomPath; //to delete
    function CanSeeFromThisPoint(const ACheckPoint: TVec2i): Boolean;
    procedure AddToLastSeenMap(const AUnit: TRoomUnit);
    procedure OnRegisterRoomObject(const ANewObject: TRoomObject); override;

    function GetPossibleEnemyPoints(const AStartPoint: TVec2i): IVec2iSet; overload;
    function GetPossibleEnemyPoints(const AOldEnemyPoints: IVec2iSet): IVec2iSet; overload;
    function GetBestDirectionForCheck(const ACheckPoints: IVec2iSet; out ANewPointsCount: Integer): Integer;
    function GetPointsForHide(const AShootPoints: IVec2iSet): IVec4iArr; //z - move weight, w - distance from shoot pts
  protected
    FBState: TBotState;
    FBS_SeeEnemy : TbsDesc_SeeEnemy;
    FBS_LostEnemy: TbsDesc_LostEnemy;
    FBS_Retreat  : TbsDesc_Retreat;
    FBS_Patrol   : TbsDesc_Patrol;
    procedure SetBSState(const ABotState: TBotState);
  protected
    function FindOptimalRouteForCheck(const ACheckPts: IVec2iSet): IRoomPath; virtual; abstract;
    function RayCastBooleanExcludeSelf(const AFromPt, AToPt: TVec2i): Boolean;

    procedure BehaviourCheck_LostEnemy();
    procedure BehaviourCheck_Retreat(AHPLimit: Integer; var ARetreatLimits: Integer);
    procedure BehaviourCheck_Retreat_Ranged(AHPLimit: Integer; var ARetreatLimits: Integer);

    function Behaviour_DefaultNothing(): IBRA_Action;
    function Behaviour_DefaultPatrol(): IBRA_Action;
    function Behaviour_DefaultRetreat(ARunAwayCount, AScaryCount: Integer): IBRA_Action;
    function Behaviour_RangedEnemySearch(ATryCount: Integer): IBRA_Action;
    function Behaviour_MeleeEnemySearch(ATryCount: Integer): IBRA_Action;
  public
    property BState: TBotState read FBState;

    procedure NewTurn(); virtual;
    function DoAction(): IBRA_Action; virtual; abstract;

    procedure LogAction(const AStr: string);

    procedure DealDamage(ADmg: Integer; AFromUnit: TRoomUnit); override;
  public
    destructor Destroy; override;
  end;
  TBotArr = {$IfDef FPC}specialize{$EndIf} TArray<TBot>;
  IBotArr = {$IfDef FPC}specialize{$EndIf} IArray<TBot>;
  TBotSet = {$IfDef FPC}specialize{$EndIf} THashSet<TBot>;
  IBotSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TBot>;

  { TBotMutant1 }

  TBotMutant1 = class (TBot)
  private type
    TComparer_OptimalRouteForCheck = class(TInterfacedObject, IComparer)
    private
      FRoom: TRoomMap;
      FCheckPts: IVec2iArr;
    public
      function Compare(const Left, Right): Integer;
      constructor Create(const ARoom: TRoomMap; const ACheckPts: IVec2iArr);
    end;
  private
    FAnim: IavAnimationController;
  protected
    function FindOptimalRouteForCheck(const ACheckPts: IVec2iSet): IRoomPath; override;

    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;

    function DoAction(): IBRA_Action; override;
  end;

  { TBotArcher1 }

  TBotArcher1 = class (TBot)
  private type
    TComparer_OptimalRouteForCheck = class(TInterfacedObject, IComparer)
    private
      FRoom: TRoomMap;
      FCheckPts: IVec2iArr;
    public
      function Compare(const Left, Right): Integer;
      constructor Create(const ARoom: TRoomMap; const ACheckPts: IVec2iArr);
    end;
  private
    FAnim: array of IavAnimationController;

    FOptimalPosPath: IRoomPath;
    FPlayer: TRoomUnit;
    FNeedTurnToPlayer: Boolean;
  protected
    function FindOptimalRouteForCheck(const ACheckPts: IVec2iSet): IRoomPath; override;
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;

    procedure NewTurn(); override;
    function DoAction(): IBRA_Action; override;
  end;

var gvDebugPoints : IVec2iArr;
    gvDebugPoints2: IVec2iArr;

implementation

uses
  untSkills, untItems;

var gvBotID: Integer = 0;

{ TBot.THidePtsComparer }

function TBot.THidePtsComparer.Compare(const Left, Right): Integer;
var L: TVec4i absolute Left;
    R: TVec4i absolute Right;
begin
  Result := L.z - R.z;
  if Result = 0 then
    Result := L.w - R.w;
end;

{ TBot.TMoveFilter }

function TBot.TMoveFilter.IsValid(const ACell: TVec2i): Boolean;
var
  obj: TRoomObject;
begin
  obj := FBot.Room.ObjectAt(ACell);
  if obj = nil then Exit(True);
  if obj = FBot then Exit(True);
  if obj = FTarget then Exit(True);
  if obj is TRoomUnit then
      if FBot.IsEnemy(TRoomUnit(obj)) then
        Exit(True);
  Result := False;
end;

constructor TBot.TMoveFilter.Create(const ABot: TBot; const ATarget: TRoomUnit);
begin
  FBot := ABot;
end;

{ TBotMutant1.TComparer_OptimalRouteForCheck }

function TBotMutant1.TComparer_OptimalRouteForCheck.Compare(const Left, Right): Integer;
begin

end;

constructor TBotMutant1.TComparer_OptimalRouteForCheck.Create(const ARoom: TRoomMap; const ACheckPts: IVec2iArr);
begin

end;

{ TBotArcher1.TComparer_OptimalRouteForCheck }

function TBotArcher1.TComparer_OptimalRouteForCheck.Compare(const Left, Right): Integer;
  function minDistTo(const APt: TVec2i): Integer;
  var dist: Integer;
      i: Integer;
  begin
    Result := 10000;
    for i := 0 to FCheckPts.Count - 1 do
    begin
      dist := Self.FRoom.Distance(FCheckPts[i], APt);
      if dist < Result then
        Result := dist;
    end;
  end;
var L: TVec4i absolute Left;
    R: TVec4i absolute Right;
    mDistL, mDistR: Integer;
begin
  Result := R.z - L.z;
  if (Result = 0) then
  begin
    mDistL := minDistTo(L.xy);
    mDistR := minDistTo(R.xy);
    Result := mDistR - mDistL;
    if (Result = 0) then
      Result := L.z - R.z;
  end;
end;

constructor TBotArcher1.TComparer_OptimalRouteForCheck.Create(const ARoom: TRoomMap; const ACheckPts: IVec2iArr);
begin
  FRoom := ARoom;
  FCheckPts := ACheckPts;
end;

{ TbsDesc_Retreat.TTargetPtsComparer }

function TbsDesc_Retreat.TTargetPtsComparer.Compare(const Left, Right): Integer;
var L: TVec4i absolute Left;
    R: TVec4i absolute Right;
begin
  Result := R.z - L.z;
  if Result = 0 then
    Result := L.w - R.w;
end;

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

procedure TbsDesc_Patrol.SetState(ABot: TRoomUnit);
var newPoint: TVec2i;
    i: Integer;
begin
  CurrPath := nil;

  if WayPts <> nil then
  begin
    while WayPts.Count > 4 do
      WayPts.Delete(0);
  end
  else
    WayPts := TVec2iArr.Create();

  for i := 0 to 9 do
  begin
    newPoint := FindNextWaypoint(ABot, WayPts);
    if newPoint = ABot.RoomPos then Continue;
    CurrPath := ABot.FindPath(newPoint);
    if CurrPath = nil then
      Continue
    else
    begin
      WayPts.Add(newPoint);
      CurrPath.Add(newPoint);
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

procedure TbsDesc_Retreat.RecalculateOptimalRoute(const ABot: TRoomUnit; const AReservedPts: Integer);
var graph: IRoomMapNonWeightedGraph;
    bfs: IRoomMapBFS;
    pt: TVec2i;
    depth, i: Integer;

    dist: Integer;
    distanceSet: IVec2iWeightMap;
    targetPts: IVec4iArr;
    cmp: IComparer;
begin
  graph := TRoomMapGraphExcludeSelfAndTarget.Create(ABot.Room, ABot, nil);

  distanceSet := TVec2iWeightMap.Create();
  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(DangerousPts);
  while bfs.Next(pt, depth) do
    distanceSet.Add(pt, depth);

  targetPts := TVec4iArr.Create;
  bfs := TRoomMapBFS.Create(graph);
  bfs.Reset(ABot.RoomPos);
  while bfs.Next(pt, depth) do
  begin
    if depth > ABot.AP - AReservedPts then Break;
    if not distanceSet.TryGetValue(pt, dist) then Continue;
    targetPts.Add(Vec(pt.x, pt.y, dist, depth));
  end;

  cmp := TTargetPtsComparer.Create;
  targetPts.Sort(cmp);

  for i := 0 to targetPts.Count - 1 do
  begin
    OptimalRoute := ABot.FindPath(targetPts[i].xy);
    if (OptimalRoute <> nil) then
    begin
      OptimalRoute.Add(targetPts[i].xy);
      Exit;
    end;
  end;
  OptimalRoute := TRoomPath.Create;
end;

procedure TbsDesc_Retreat.SetState(const ABot: TRoomUnit; const ADangerPts: IVec2iArr; const AReservedPts: Integer);
begin
  ClearState();
  DangerousPts := ADangerPts;
  RecalculateOptimalRoute(ABot, AReservedPts);
end;

procedure TbsDesc_Retreat.SetState(const ABot: TRoomUnit; const ADangerPt: TVec2i; const AReservedPts: Integer);
var arr: IVec2iArr;
begin
  ClearState();
  arr := TVec2iArr.Create;
  arr.Add(ADangerPt);
  SetState(ABot, arr, AReservedPts);
end;

procedure TbsDesc_Retreat.SetEnemyPt(const APt: TVec2i);
begin
  EnemyPt := APt;
end;

procedure TbsDesc_Retreat.ClearState();
begin
  inherited ClearState();
  DangerousPts.Clear();
  Step := 0;
  NeedTurnToEnemy := False;
end;

procedure TbsDesc_Retreat.AfterConstruction;
begin
  DangerousPts := TVec2iArr.Create;
end;

{ TbsDesc_LostEnemy }

procedure TbsDesc_LostEnemy.SetState(const ALastSeenPos: TVec2i; const ACheckPts: IVec2iSet);
begin
  ClearState();
  LastPt := ALastSeenPos;
  CheckPt := ACheckPts;
end;

procedure TbsDesc_LostEnemy.ClearState();
begin
  Step := 0;
  OptimalRoute := nil;
  CheckPt.Clear();
  SkipTurn := False;
end;

procedure TbsDesc_LostEnemy.AfterConstruction;
begin
  inherited AfterConstruction;
  CheckPt := TVec2iSet.Create;
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

function TBotArcher1.FindOptimalRouteForCheck(const ACheckPts: IVec2iSet): IRoomPath;
var movePtsWeighted: IVec2iWeightMap;
    movePts: IVec2iSet;
    shootPts: IVec2iSet;
    pt, checkPt: TVec2i;
    checkPtsWeighted: IVec2iWeightedSet;
    pWeight: PInteger;
    weight : Integer;
    i: Integer;

    ptsList: IVec4iArr;
    cmp: IComparer;
begin
  Result := nil;

  movePtsWeighted := GetMovePointsWeighted(AP - 1);
  movePts := TVec2iSet.Create();
  movePts.Capacity := NextPow2(movePtsWeighted.Count);
  movePtsWeighted.Reset;
  while movePtsWeighted.NextKey(pt) do
    movePts.Add(pt);

  checkPtsWeighted := TVec2iWeightedSet.Create;
  ACheckPts.Reset;
  while ACheckPts.Next(checkPt) do
  begin
    shootPts := Room.GetShootPointsSet(checkPt, max(3, ViewRange-3), movePts);
    shootPts.Reset;
    while shootPts.Next(pt) do
    begin
      Assert(movePtsWeighted.Contains(pt));

      if not checkPtsWeighted.TryGetPValue(pt, Pointer(pWeight)) then
        checkPtsWeighted.Add(pt, 0)
      else
        Inc(pWeight^);
    end;
  end;

  ptsList := TVec4iArr.Create();
  ptsList.Capacity := checkPtsWeighted.Count;
  checkPtsWeighted.Reset;
  while checkPtsWeighted.Next(pt, weight) do
    ptsList.Add(Vec(pt.x, pt.y, weight, movePtsWeighted[pt]));

  cmp := TComparer_OptimalRouteForCheck.Create(Room, FBS_LostEnemy.CheckPt.ToIArray());
  ptsList.Sort(cmp);

  for i := 0 to ptsList.Count - 1 do
  begin
    Result := FindPath(ptsList[i].xy, TMoveFilter.Create(Self, nil));
    if Result <> nil then
    begin
      Result.Add(ptsList[i].xy);
      Exit;
    end;
  end;
end;

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
  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;
  AP := MaxAP;

  Name := 'Лучница';
  ViewRange := 16;
  ViewAngle := Pi/3+EPS;
  ViewWholeRange := 2.5;

  AddModel('Archer_Body', mtDefault);
  AddModel('Archer_Clothes', mtDefault);
  AddModel('Archer_Eyelashes', mtDefault);
  AddModel('Archer_Eyes', mtDefault);

  bow := TArcherBow.Create;
  Equip(bow);
  Inventory().Push(bow, 0);

  FUnitSkills.Add(TSkill_Shoot.Create(nil, 0));

  FAnimationPrefix := 'Archer_';

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
    FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
  SetAnimation(['Idle0'], True);

  FRetreatLimits := 5;

  Preview96_128 := 'ui\units\archer.png';
end;

procedure TBotArcher1.NewTurn();

  function GetOptimalRangePosition(const ATargetPos: TVec2i): IRoomPath;
  const
    cBestDistance = 9;
  var points: IVec2iArr;
      i: Integer;
  begin
    points := GetPointsOnRange(ATargetPos, cBestDistance, AP - 1);
    for i := 0 to points.Count - 1 do
      if RayCastBooleanExcludeSelf(points[i], ATargetPos) then
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
    points := GetPointsOnRange(ATargetPos, cBestDistance, AP - 1);
    for i := 0 to points.Count - 1 do
      if RayCastBooleanExcludeSelf(points[i], ATargetPos) then
      begin
        Result := FindPath(points[i]);
        if RoomPos <> points[i] then
          Result.Add(points[i]);
        Exit;
      end;
    Result := TRoomPath.Create();
  end;

var
  availPts: Integer;
begin
  if AP = 0 then Exit(nil);
  //BehaviourCheck_Retreat(40, FRetreatLimits);

  LogAction('DoAction AP = ' + IntToStr(AP));

  case FBState of
    bsNothing: Result := Behaviour_DefaultNothing();
    bsSeeEnemy:
      begin
        if (FBS_SeeEnemy.OptimalRoute = nil) or (FBS_SeeEnemy.OptimalRoute.Count = 0) then
          FBS_SeeEnemy.OptimalRoute := GetOptimalRangePosition(FBS_SeeEnemy.Enemy.RoomPos);
        if FBS_SeeEnemy.OptimalRoute = nil then
        begin
          LogAction('  Cant find optimal route');
          SetBSState(bsRetreat);
          FBS_Retreat.SetState(Self, FBS_SeeEnemy.Enemy.GetShootPoints(), 0);
          FBS_Retreat.SetEnemyPt(FBS_SeeEnemy.Enemy.RoomPos);
        end
        else
        begin
          if not InViewField(FBS_SeeEnemy.Enemy.RoomPos) then
          begin
            LogAction('  Dont see enemy. Turn on.');
            Result := TBRA_UnitTurnAction.Create(Self, FBS_SeeEnemy.Enemy.RoomPos);
            Exit;
          end;

          availPts := AP - FBS_SeeEnemy.OptimalRoute.Count;
          if (FEquippedItem[esBothHands] <> nil) and (availPts > FUnitSkills[0].Cost) then
          begin
            if (FBS_SeeEnemy.OptimalRoute.Count = 0) or
               (FRoom.Distance(RoomPos, FBS_SeeEnemy.Enemy.RoomPos) < FRoom.Distance(FBS_SeeEnemy.OptimalRoute.Last, FBS_SeeEnemy.Enemy.RoomPos)) then
            begin
              LogAction('  Time to attack.');
              Result := FUnitSkills[0].DoAction(0, Self, FBS_SeeEnemy.Enemy);
              Exit;
            end
            else
            begin
              LogAction('  Move to safety distance 1.');
              Result := Action_MoveWithRoute(FBS_SeeEnemy.OptimalRoute);
              Exit;
            end;
          end
          else
          begin
            if FBS_SeeEnemy.OptimalRoute.Count = 0 then
            begin
              LogAction('  No optimal route.');
              Exit(nil);
            end
            else
            begin
              LogAction('  Move to safety distance 2.');
              Result := Action_MoveWithRoute(FBS_SeeEnemy.OptimalRoute);
              Exit;
            end;
          end;
        end;
      end;
    bsLostEnemy: Result := Behaviour_RangedEnemySearch(10);
    bsRetreat: Result := Behaviour_DefaultRetreat(1, 2);
    bsPatrol: Result := Behaviour_DefaultPatrol();
  end;
end;

{ TBot }

procedure TBot.AfterRegister;
begin
  inherited AfterRegister;
  FLastSeen := TLastSeenMap.Create();
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

function TBot.GetPointsOnRange(const AStartPoint: TVec2i; const ARange, AMovePoints: Integer): IVec2iArr;
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
    if depth > AMovePoints then Break;
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

function TBot.GetHiddenNeighbours(const AOriginPts: IVec2iArr): IVec2iArr;
var all: IVec2iSet;
    hidden: IVec2iArr;
    i, j: Integer;
begin
  all := TVec2iSet.Create();
  for i := 0 to AOriginPts.Count - 1 do
  begin
    hidden := GetHiddenNeighbours(AOriginPts[i]);
    for j := 0 to hidden.Count - 1 do
      all.Add(hidden[j]);
  end;
  Result := all.ToIArray();
end;

function TBot.GetHiddenNeighbours(const AOriginPt: TVec2i): IVec2iArr;
var neighbour: TVec2i;
    i: Integer;
    obj: TRoomObject;
begin
  Result := TVec2iArr.Create();
  for i := 0 to 5 do
  begin
    neighbour := Room.NeighbourTile(AOriginPt, i);
    if Room.IsCellExists(neighbour) and (not CanSee(neighbour)) then
    begin
      obj := Room.ObjectAt(neighbour);
      if (obj = nil) or (obj is TRoomUnit) then
        Result.Add(neighbour);
    end;
  end;
end;

function TBot.Action_MoveWithRoute(const ARoute: IRoomPath): IBRA_Action;
begin
  FMoveAction := TBRA_UnitMovementAction.Create(Self, ARoute);
  Result := FMoveAction;
  ARoute.Clear();
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

function TBot.CanSeeFromThisPoint(const ACheckPoint: TVec2i): Boolean;
var range: Single;
begin
  Result := RayCastBooleanExcludeSelf(RoomPos, ACheckPoint);
  if Result then
  begin
    range := max(ViewRange, ViewWholeRange);
    if Room.Distance(RoomPos, ACheckPoint) > range then Exit(False);
  end;
end;

procedure TBot.AddToLastSeenMap(const AUnit: TRoomUnit);
begin
  FLastSeen.AddOrSet(AUnit.WeakRef, AUnit.RoomPos);
end;

procedure TBot.OnRegisterRoomObject(const ANewObject: TRoomObject);
var
  newEnemy: TRoomUnit;
  newPts: IVec2iSet;
begin
  if ANewObject is TRoomUnit then
  begin
      if IsEnemy(TRoomUnit(ANewObject)) then
        if CanSee(TRoomUnit(ANewObject)) then
        begin
          AddToLastSeenMap(TRoomUnit(ANewObject));
          if FBState <> bsSeeEnemy then
          begin
            if (FMoveAction <> nil) then
              FMoveAction.TryCancel;
            SetBSState(bsSeeEnemy);
            FBS_SeeEnemy.SetState(TRoomUnit(ANewObject));
          end;
        end;
      if (ANewObject = Self) then
      begin
        if FEmptyCells <> nil then
        begin
          newPts := GetObservablePointSet(nil, FEmptyCells);
          if newPts.Count > 0 then
          begin
            FEmptyCells := FEmptyCells.Union( newPts );
            if FBState = bsLostEnemy then
              FBS_LostEnemy.CheckPt := FBS_LostEnemy.CheckPt.Diff(newPts);
          end;
        end;

        newEnemy := FindEnemy();
        if newEnemy <> nil then
        begin
          AddToLastSeenMap(newEnemy);
          if FBState <> bsSeeEnemy then
          begin
            if (FMoveAction <> nil) then
              FMoveAction.TryCancel;
            SetBSState(bsSeeEnemy);
            FBS_SeeEnemy.SetState(TRoomUnit(newEnemy));
          end;
        end;
      end;
  end;
end;

function TBot.GetPossibleEnemyPoints(const AStartPoint: TVec2i): IVec2iSet;
var pts: IVec2iSet;
begin
  pts := TVec2iSet.Create();
  pts.Add(AStartPoint);
  Result := GetPossibleEnemyPoints(pts);
end;

function TBot.GetPossibleEnemyPoints(const AOldEnemyPoints: IVec2iSet): IVec2iSet;
var graph: IRoomMapNonWeightedGraph;
    iterator: IRoomMapBFS;
    pt: TVec2i;
    depth: Integer;
begin
  Result := AOldEnemyPoints.Clone();
  graph := TRoomMapGraph_CustomFilter.Create(Room, TMoveFilter.Create(Self, nil));
  iterator := TRoomMapBFS.Create(graph);
  iterator.Reset(Result);
  while iterator.Next(pt, depth) do
  begin
    if depth > 10 then Break;
    if not FEmptyCells.Contains(pt) then
      Result.Add(pt);
  end;
  Result := Result.Diff(FEmptyCells);
end;

function TBot.GetBestDirectionForCheck(const ACheckPoints: IVec2iSet; out ANewPointsCount: Integer): Integer;
var i: Integer;
    pt: TVec2i;
    viewCount: Integer;
    filter: IRoomCellFilter;
begin
  filter := TMoveFilter.Create(Self, nil);

  Result := RoomDir;
  ANewPointsCount := 0;
  for i := 0 to 5 do
  begin
    if i = RoomDir then Continue;

    viewCount := 0;
    ACheckPoints.Reset;
    while ACheckPoints.Next(pt) do
    begin
      if not InViewField(i, pt) then Continue;
      if Room.RayCastBoolean(RoomPos, pt, filter) then
        Inc(viewCount);
    end;

    if viewCount > ANewPointsCount then
    begin
      ANewPointsCount := viewCount;
      Result := i;
    end;
  end;
end;

function TBot.GetPointsForHide(const AShootPoints: IVec2iSet): IVec4iArr;
var graph: IRoomMapNonWeightedGraph;
    iterator: IRoomMapBFS;
    pt: TVec2i;
    depth, i: Integer;
    hidept: TVec4i;
begin
  Result := TVec4iArr.Create();

  graph := TRoomMapGraph_CustomFilter.Create(Room, TMoveFilter.Create(Self, nil));
  iterator := TRoomMapBFS.Create(graph);
  iterator.Reset(RoomPos);
  while iterator.Next(pt, depth) do
  begin
    if depth > AP then Break;
    if AShootPoints.Contains(pt) then Continue;
    hidept.xy := pt;
    hidept.z := depth;
    hidept.w := 0;
    Result.Add(hidept);
  end;

  for i := 0 to Result.Count - 1 do
  begin
    iterator := TRoomMapBFS.Create(graph);
    iterator.Reset(Result[i].xy);
    while iterator.Next(pt, depth) do
    begin
      if AShootPoints.Contains(pt) then
      begin
        Result[i] := Vec(Result[i].xyz, depth);
        Break;
      end;
    end;
  end;
end;

procedure TBot.SetBSState(const ABotState: TBotState);
begin
  FBState := ABotState;
  case ABotState of
    bsNothing: LogAction('bsNothing');
    bsSeeEnemy: LogAction('bsSeeEnemy');
    bsLostEnemy: LogAction('bsLostEnemy');
    bsRetreat: LogAction('bsRetreat');
    bsPatrol: LogAction('bsPatrol');
  end;
end;

function TBot.RayCastBooleanExcludeSelf(const AFromPt, AToPt: TVec2i): Boolean;
begin
  Result := FRoom.RayCastBoolean(AFromPt, AToPt, TRoomCellFilter_ViewableExclude_ObjectsExclude.Create(FRoom, [Self]));
end;

procedure TBot.BehaviourCheck_LostEnemy();
var
  lastSeen: TVec2i;
  lastSeenPts: IVec2iSet;
begin
  if FBState <> bsSeeEnemy then Exit;

  if FBS_SeeEnemy.Enemy.IsDead() then
  begin
    SetBSState(bsNothing);
    Exit;
  end;

  if not CanSee(FBS_SeeEnemy.Enemy) then
  begin
    lastSeen := FLastSeen[FBS_SeeEnemy.Enemy.WeakRef];
    lastSeenPts := TVec2iSet.Create();
    lastSeenPts.Add(lastSeen);
    SetBSState(bsLostEnemy);
    FBS_LostEnemy.SetState(lastSeen, GetPossibleEnemyPoints(lastSeen));
  end;
end;

procedure TBot.BehaviourCheck_Retreat(AHPLimit: Integer; var ARetreatLimits: Integer);
var hidePts: IVec4iArr;
    comparer: IComparer;
    i: Integer;
begin
  if FBState = bsSeeEnemy then
    if HP < AHPLimit then
    begin
      if ARetreatLimits > 0 then
      begin
        Dec(ARetreatLimits);
        SetBSState(bsRetreat);
        FBS_Retreat.SetState(Self, FBS_SeeEnemy.Enemy.GetShootPoints(), 0);

        hidePts := GetPointsForHide(FBS_SeeEnemy.Enemy.GetShootPointsSet());
        comparer := THidePtsComparer.Create;
        hidePts.Sort(comparer);
        gvDebugPoints := TVec2iArr.Create();
        for i := 0 to hidePts.Count - 1 do
          gvDebugPoints.Add(hidePts[i].xy);

        FBS_Retreat.SetEnemyPt(FBS_SeeEnemy.Enemy.RoomPos);
      end;
    end;
end;

procedure TBot.BehaviourCheck_Retreat_Ranged(AHPLimit: Integer; var ARetreatLimits: Integer);
begin
  if FBState = bsSeeEnemy then
    if HP < AHPLimit then
    begin
      if ARetreatLimits > 0 then
      begin
        Dec(ARetreatLimits);
        SetBSState(bsRetreat);
        FBS_Retreat.SetState(Self, FBS_SeeEnemy.Enemy.GetShootPoints(), 0);
        gvDebugPoints := FBS_SeeEnemy.Enemy.GetShootPoints();
        FBS_Retreat.SetEnemyPt(FBS_SeeEnemy.Enemy.RoomPos);
      end;
    end;
end;

function TBot.Behaviour_DefaultNothing(): IBRA_Action;
var
  enemy: TRoomUnit;
begin
  enemy := FindEnemy();
  if enemy = nil then
  begin
    SetBSState(bsPatrol);
    FBS_Patrol.SetState(Self);
  end
  else
  begin
    SetBSState(bsSeeEnemy);
    FBS_SeeEnemy.SetState(enemy);
  end;
  Result := DoAction();
end;

function TBot.Behaviour_DefaultPatrol(): IBRA_Action;
begin
  Result := nil;
  if (FBS_Patrol.CurrPath <> nil) and (FBS_Patrol.CurrPath.Count > 0) then
  begin
    Result := Action_MoveWithRoute(FBS_Patrol.CurrPath);
    Exit;
  end;

  if FBS_Patrol.WayPts.Last = RoomPos then
  begin
    FBS_Patrol.SetState(Self);
    Result := DoAction();
    Exit;
  end
  else
  begin
    FBS_Patrol.CurrPath := FindPath(FBS_Patrol.WayPts.Last);
    if (FBS_Patrol.CurrPath <> nil) and (FBS_Patrol.WayPts.Last <> RoomPos) then
      FBS_Patrol.CurrPath.Add(FBS_Patrol.WayPts.Last);
    if (FBS_Patrol.CurrPath = nil) or (FBS_Patrol.CurrPath.Count = 0) then
      FBS_Patrol.SetState(Self);
    Result := DoAction();
    Exit;
  end;
end;

function TBot.Behaviour_DefaultRetreat(ARunAwayCount, AScaryCount: Integer): IBRA_Action;
begin
  if (FBS_Retreat.OptimalRoute <> nil) and (FBS_Retreat.OptimalRoute.Count > 0) then
  begin
    LogAction('  Retreat Movement');
    Result := Action_MoveWithRoute(FBS_Retreat.OptimalRoute);
    if FBS_Retreat.Step = ARunAwayCount then
    begin
      FBS_Retreat.NeedTurnToEnemy := True;
      LogAction('  NeedTurnToEnemy');
    end;
    Exit;
  end
  else
  begin
    LogAction('  Retreat step ' + IntToStr(FBS_Retreat.Step));
    if FBS_Retreat.Step < ARunAwayCount then
    begin
      if AP <> MaxAP then
      begin
        Result := nil;
        Exit;
      end;
      FBS_Retreat.RecalculateOptimalRoute(Self, 1);
      FBS_Retreat.Step := FBS_Retreat.Step + 1;
      FBS_Retreat.NeedTurnToEnemy := True;
      Result := DoAction();
      Exit;
    end
    else
      if (FBS_Retreat.Step - ARunAwayCount) < AScaryCount then
      begin
        if (AP >= 1) and FBS_Retreat.NeedTurnToEnemy then
        begin
          Result := TBRA_UnitTurnAction.Create(Self, FBS_Retreat.EnemyPt);
          FBS_Retreat.NeedTurnToEnemy := False;
          Exit;
        end;
        if AP = MaxAP then
        begin
          FBS_Retreat.Step := FBS_Retreat.Step + 1;
          LogAction('  Scared');
        end;
        Result := nil;
        Exit;
      end
      else
      begin
        SetBSState(bsNothing);
        Result := DoAction();
        Exit;
      end;
  end;
end;

function TBot.Behaviour_RangedEnemySearch(ATryCount: Integer): IBRA_Action;
var
  NewCheckCount, BestDir: Integer;
begin
  //go to another state
  if FBS_LostEnemy.Step >= ATryCount then
  begin
    LogAction('  Reach max try count. Cant find enemy. Go to another mode.');

    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  //turn on some points
  BestDir := GetBestDirectionForCheck(FBS_LostEnemy.CheckPt, NewCheckCount);
  if (BestDir <> RoomDir) and (NewCheckCount > FBS_LostEnemy.CheckPt.Count div 10) then
  begin
    LogAction('  Turn on check points.');
    Result := TBRA_UnitTurnAction.Create(Self, BestDir);
    Exit;
  end;

  //no points for check, go to another state
  if FBS_LostEnemy.CheckPt.Count = 0 then
  begin
    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  Inc(FBS_LostEnemy.Step);

  //find new route for check
  FBS_LostEnemy.OptimalRoute := FindOptimalRouteForCheck(FBS_LostEnemy.CheckPt);

  //no checkroute, go to another state
  if FBS_LostEnemy.OptimalRoute = nil then
  begin
    LogAction('  No route for check. Go to another mode.');
    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  //move with optimal route
  if (FBS_LostEnemy.OptimalRoute.Count > 0) then
  begin
    LogAction('  Checking for lost enemy with optimal route.');
    Result := Action_MoveWithRoute(FBS_LostEnemy.OptimalRoute);
    Exit;
  end;

  //tryagain
  Result := DoAction();
end;

function TBot.Behaviour_MeleeEnemySearch(ATryCount: Integer): IBRA_Action;
var
  newEnemy: TRoomUnit;
  newPts: IVec2iArr;
  i: Integer;
  worldPt: TVec3;
begin
  //go to another state
  if FBS_LostEnemy.Step >= ATryCount then
  begin
    LogAction('  Cant find enemy. Go to another mode.');

    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  //try find enemy
  newEnemy := FindEnemy();
  if newEnemy <> nil then
  begin
    LogAction('  I see enemy!');
    SetBSState(bsSeeEnemy);
    FBS_SeeEnemy.SetState(newEnemy);
    Result := DoAction();
    Exit;
  end;

  //turn on last enemy point
  if not InViewField(FBS_LostEnemy.LastPt) and CanSeeFromThisPoint(FBS_LostEnemy.LastPt) then
  begin
    LogAction('  Turn on lost enemy.');
    Result := TBRA_UnitTurnAction.Create(Self, FBS_LostEnemy.LastPt);
    Exit;
  end
  else
  begin
    if FBS_LostEnemy.CheckPt.Count = 0 then
    begin
      FBS_LostEnemy.CheckPt.Add(FBS_LostEnemy.LastPt);
    end;
  end;

  //update check points
  //newPts := GetHiddenNeighbours(FBS_LostEnemy.CheckPt);
  //for i := 0 to FBS_LostEnemy.CheckPt.Count - 1 do
  //  if not CanSee(FBS_LostEnemy.CheckPt[i]) then
  //    newPts.Add(FBS_LostEnemy.CheckPt[i]);

  //no points for check, go to another state
  if newPts.Count = 0 then
  begin
    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  //evaluate estimated enemy position
  //worldPt := Vec(0,0,0);
  //for i := 0 to newPts.Count - 1 do
  //  worldPt := worldPt + Room.UI.TilePosToWorldPos(newPts[i]);
  //worldPt := worldPt / newPts.Count;
  //FBS_LostEnemy.LastPt := Room.UI.GetTileAtCoords(Vec(worldPt.x, worldPt.z));
  //FBS_LostEnemy.CheckPt := newPts;
  //FBS_LostEnemy.OptimalRoute := nil;
  Inc(FBS_LostEnemy.Step);

  //find new route for check
  if FBS_LostEnemy.OptimalRoute = nil then
    FBS_LostEnemy.OptimalRoute := FindOptimalRouteForCheck(FBS_LostEnemy.CheckPt);

  //no checkroute, go to another state
  if FBS_LostEnemy.OptimalRoute = nil then
  begin
    LogAction('  No route for check. Go to another mode.');
    SetBSState(bsNothing);
    Result := DoAction();
    Exit;
  end;

  //move with optimal route
  if (FBS_LostEnemy.OptimalRoute.Count > 0) then
  begin
    LogAction('  Checking for lost enemy with optimal route.');
    Result := Action_MoveWithRoute(FBS_LostEnemy.OptimalRoute);
    Exit;
  end;

  //tryagain
  Result := DoAction();
end;

procedure TBot.NewTurn();
begin
  FEmptyCells := GetObservablePointSet(nil, nil);
  BehaviourCheck_Retreat(40, FRetreatLimits);
  if FBState = bsLostEnemy then
    FBS_LostEnemy.CheckPt := GetPossibleEnemyPoints(FBS_LostEnemy.CheckPt)
  else
    BehaviourCheck_LostEnemy();
end;

procedure TBot.LogAction(const AStr: string);
begin
  WriteLn(AStr);
end;

procedure TBot.DealDamage(ADmg: Integer; AFromUnit: TRoomUnit);
begin
  inherited DealDamage(ADmg, AFromUnit);
  if AFromUnit <> nil then
    if not (FBState in [bsRetreat, bsSeeEnemy]) then
    begin
      AddToLastSeenMap(AFromUnit);
      SetBSState(bsSeeEnemy);
      FBS_SeeEnemy.SetState(AFromUnit);
    end;
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

function TBotMutant1.FindOptimalRouteForCheck(const ACheckPts: IVec2iSet): IRoomPath;
//var movePtsWeighted: IVec2iWeightMap;
//    movePts: IVec2iSet;
//    shootPts: IVec2iSet;
//    pt: TVec2i;
//    checkPtsWeighted: IVec2iWeightedSet;
//    pWeight: PInteger;
//    weight : Integer;
//    i: Integer;
//
//    ptsList: IVec4iArr;
//    //cmp: IComparer;

var graph: IRoomMapNonWeightedGraph;
    iterator: IRoomMapBFS;
    checkPtsSet: IVec2iSet;
    pt: TVec2i;
    depth, i: Integer;
begin
  Result := nil;
  Exit;
  if AP < 4 then Exit;

  //checkPtsSet := TVec2iSet.Create();
  //for i := 0 to ACheckPts.Count - 1 do
  //  checkPtsSet.Add(ACheckPts[i]);

  graph := TRoomMapGraph_CustomFilter.Create(Room, TRoomCellFilter_ExcludeUnits.Create(Room));
  iterator := TRoomMapBFS.Create(graph);
  iterator.Reset(RoomPos);
  while iterator.Next(pt, depth) do
  begin
    if checkPtsSet.Contains(pt) then
    begin
      Result := FindPath(pt);
      if Result <> nil then
      begin
        Result.Add(pt);
        Exit;
      end;
    end;
  end;

  {
  movePtsWeighted := GetMovePointsWeighted(AP - 3);
  movePts := TVec2iSet.Create();
  movePts.Capacity := NextPow2(movePtsWeighted.Count);
  movePtsWeighted.Reset;
  while movePtsWeighted.NextKey(pt) do
    movePts.Add(pt);

  checkPtsWeighted := TVec2iWeightedSet.Create;
  for i := 0 to ACheckPts.Count - 1 do
  begin
    shootPts := Room.GetShootPointsSet(ACheckPts[i], max(3, ViewRange-3), movePts);
    shootPts.Reset;
    while shootPts.Next(pt) do
    begin
      Assert(movePtsWeighted.Contains(pt));

      if not checkPtsWeighted.TryGetPValue(pt, Pointer(pWeight)) then
        checkPtsWeighted.Add(pt, 0)
      else
        Inc(pWeight^);
    end;
  end;

  ptsList := TVec4iArr.Create();
  ptsList.Capacity := checkPtsWeighted.Count;
  checkPtsWeighted.Reset;
  while checkPtsWeighted.Next(pt, weight) do
    ptsList.Add(Vec(pt.x, pt.y, weight, movePtsWeighted[pt]));

  //cmp := TComparer_OptimalRouteForCheck.Create(Room, FBS_LostEnemy.CheckPt);
  //ptsList.Sort(cmp);

  for i := 0 to ptsList.Count - 1 do
  begin
    Result := FindPath(ptsList[i].xy);
    if Result <> nil then
    begin
      Result.Add(ptsList[i].xy);
      Exit;
    end;
  end;
  }
end;

procedure TBotMutant1.UpdateStep;
begin
  inherited UpdateStep;
  FAnim.SetTime(World.GameTime);
end;

procedure TBotMutant1.SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean);
begin
  inherited SetAnimation(ANameSequence, ALoopedLast);
  FAnim.AnimationSequence_StartAndStopOther(AddAnimationPrefix(ANameSequence), ALoopedLast);
end;

function TBotMutant1.GetUnitMoveSpeed: Single;
begin
  Result := 5;
end;

procedure TBotMutant1.LoadModels();
begin
  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;
  AP := MaxAP;

  Name := 'Мутант';
  ViewRange := 10;
  ViewAngle := Pi/3+EPS;
  ViewWholeRange := 2.5;

  FAnimationPrefix := 'Mut1_';
  AddModel('Mutant1', mtDefault);
  FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);

  SetAnimation(['Idle0'], True);

  Preview96_128 := 'ui\units\mutant1.png';

  FUnitSkills.Add(TSkill_Kick.Create(nil, 0));
end;

function TBotMutant1.DoAction(): IBRA_Action;
var player: TRoomUnit;
    path: IRoomPath;
begin
  if AP = 0 then Exit(nil);

  case FBState of
    bsNothing: Result := Behaviour_DefaultNothing();
    bsSeeEnemy:
      begin
        if FUnitSkills[0].CanUse(0, Self, FBS_SeeEnemy.Enemy) then
        begin
          Result := FUnitSkills[0].DoAction(0, Self, FBS_SeeEnemy.Enemy);
          Exit;
        end;

        if Room.Distance(FBS_SeeEnemy.Enemy.RoomPos, RoomPos) > 1 then
        begin
          path := FindPath(FBS_SeeEnemy.Enemy.RoomPos, FBS_SeeEnemy.Enemy);
          if (path <> nil) and (path.Count > 0) then
          begin
            Result := TBRA_UnitMovementAction.Create(Self, path);
            Exit;
          end;
        end;

      end;
    bsLostEnemy: Result := Behaviour_MeleeEnemySearch(5);
    bsRetreat: Result := Behaviour_DefaultRetreat(1, 2);
    bsPatrol: Result := Behaviour_DefaultPatrol();
  end;
end;

end.

