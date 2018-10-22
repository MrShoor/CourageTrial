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
  avPathFinder, untLevel;

type
  { TBot }

  TBot = class (TRoomUnit)
  protected type
    ILastSeenMap = {$IfDef FPC}specialize{$EndIf}IHashMap<IWeakRef, TVec2i>;
    TLastSeenMap = {$IfDef FPC}specialize{$EndIf}THashMap<IWeakRef, TVec2i>;
    ILastSeenArr = {$IfDef FPC}specialize{$EndIf}IArray<IWeakRef>;
    TLastSeenArr = {$IfDef FPC}specialize{$EndIf}TArray<IWeakRef>;
  protected
    procedure AfterRegister; override;
    function IsEnemy(const ARoomUnit: TRoomUnit): Boolean;
    function FindEnemy(): TRoomUnit; virtual;
  protected
    FLastSeen: ILastSeenMap;
    function GetMovePath(): IRoomPath;
    procedure AddToLastSeenMap(const AUnit: TRoomUnit);
    procedure OnRegisterRoomObject(const ANewObject: TRoomObject); override;
  public
    function DoAction(): IBRA_Action;
  end;
  TBotArr = {$IfDef FPC}specialize{$EndIf} TArray<TBot>;
  IBotArr = {$IfDef FPC}specialize{$EndIf} IArray<TBot>;
  TBotSet = {$IfDef FPC}specialize{$EndIf} THashSet<TBot>;
  IBotSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TBot>;


  { TBotMutant1 }

  TBotMutant1 = class (TBot)
  private
    FAnim: IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;
  end;

implementation

{ TBot }

procedure TBot.AfterRegister;
begin
  inherited AfterRegister;
  FLastSeen := TLastSeenMap.Create();
  SubscribeForUpdateStep;
end;

function TBot.IsEnemy(const ARoomUnit: TRoomUnit): Boolean;
begin
  Result := ARoomUnit is TPlayer;
end;

function TBot.FindEnemy(): TRoomUnit;
var i: Integer;
begin
  for i := 0 to Room.ChildCount() - 1 do
    if Room.Child[i] is TPlayer then
    begin
      Result := Room.Child[i] as TRoomUnit;
      if not Result.IsDead() then
        if CanSee(Result) then
          Exit;
    end;
  Result := nil;
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

function TBot.DoAction(): IBRA_Action;
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
  end;

  if TBRA_UnitDefaultAttack.CanUse(Self, player) then
  begin
    Result := TBRA_UnitDefaultAttack.Create(Self, player, 1000, 300);
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
  ViewAngle := Pi/3;
  ViewWholeRange := 2.5;

  AddModel('Mutant1', mtDefault);
  FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);
  SetAnimation(['Idle0'], False);

  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;

  Preview96_128 := 'ui\units\mutant1.png';
end;

end.

