unit untInteractiveObjects;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, bWorld, avBase, untLevel, untObstacles, avModel, avMesh, mutils, untItems;

type

  { TRoomChest }

  TRoomChest = class(TRoomInteractiveObject)
  private
    FInventory: IInventory;
    FAnim: IavAnimationController;
  protected
    procedure UpdateStep; override;
    procedure AfterRegister; override;
  public
    procedure SetAnimation(const AName: string);
    procedure LoadModels(const AObstacle: TObstacleDesc); override;

    function Inventory: IInventory; override;

    function Interactive_CellsCount: Integer; override;
    function Interactive_GetCell(AIndex: Integer): TVec2i; override;

    function Interactive_Cost(AIndex: Integer): Integer; override;
    function Interactive_Try(AUnit: TRoomUnit): IBRA_Action; override;
  end;

  { TRoomAltar }

  TRoomAltar = class(TRoomInteractiveObject)
  private
    FItem: IUnitItem;
    FItemInstance: IavModelInstance;
    FAnim: IavAnimationController;
    procedure Equip(const AItem: IUnitItem);
  protected
    procedure UpdateStep; override;
    procedure AfterRegister; override;
  public
    procedure WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType); override;
  public
    function PickItem: IUnitItem;

    procedure SetAnimation(const AName: string);
    procedure LoadModels(const AObstacle: TObstacleDesc); override;

    function Interactive_CellsCount: Integer; override;
    function Interactive_GetCell(AIndex: Integer): TVec2i; override;

    function Interactive_Cost(AIndex: Integer): Integer; override;
    function Interactive_Try(AUnit: TRoomUnit): IBRA_Action; override;
  end;

implementation

type

  { TBRA_LootChestAction }

  TBRA_LootChestAction = class(TBRA_Action)
  private
    FInAction: Boolean;
    FChest: TRoomChest;
    FUnit : TRoomUnit;
    FMenuStart: Int64;
    function GetBattleRoom: TBattleRoom;
    procedure ShowOtherInventory;
    procedure HideOtherInventory;
  public
    procedure TryCancel; override;
    function ProcessAction: Boolean; override;
    constructor Create(AChest: TRoomChest; AUnit: TRoomUnit);
  end;

  { TBRA_AltarPick }

  TBRA_AltarPick = class(TBRA_Action)
  private
    FAltar: TRoomAltar;
    FUnit : TRoomUnit;
    FActionTime: Int64;
  public
    function ProcessAction: Boolean; override;
    constructor Create(AAltar: TRoomAltar; AUnit: TRoomUnit);
  end;

{ TBRA_AltarPick }

function TBRA_AltarPick.ProcessAction: Boolean;
begin
  Result := True;
  if FUnit.World.GameTime >= FActionTime then
  begin
    Result := False;
    FUnit.Inventory().Push(FAltar.PickItem, 0);
  end;
end;

constructor TBRA_AltarPick.Create(AAltar: TRoomAltar; AUnit: TRoomUnit);
begin
  FAltar := AAltar;
  FUnit := AUnit;
  FActionTime := AUnit.World.GameTime + 800;
end;

{ TRoomAltar }

procedure TRoomAltar.Equip(const AItem: IUnitItem);
var
  m: TMat4;
  inst: IavModelInstanceArr;
begin
  FItem := AItem;

  m := FModels[0].Mesh.BindPoseTransform;
  inst := World.Renderer.CreateModelInstances([AItem.Model]);
  inst[0].Mesh.Transform := m;
  inst[0].Mesh.Pose := FModels[0].Mesh.Pose;
  inst[0].Mesh.Transform := Transform();
  FItemInstance := inst[0];
end;

procedure TRoomAltar.UpdateStep;
begin
  inherited UpdateStep;
  if FAnim <> nil then
  begin
    FAnim.SetTime(World.GameTime);
    FItemInstance.Mesh.Transform := Transform();
  end;
end;

procedure TRoomAltar.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
end;

procedure TRoomAltar.WriteModels(const ACollection: IavModelInstanceArr;
  AType: TModelType);
begin
  inherited WriteModels(ACollection, AType);
  if (AType = mtDefault) and (FItemInstance <> nil) then
    ACollection.Add(FItemInstance);
end;

function TRoomAltar.PickItem: IUnitItem;
begin
  Result := FItem;
  FItem := nil;
  FItemInstance := nil;
  FAnim := nil;
  UnSubscribeFromUpdateStep;
end;

procedure TRoomAltar.SetAnimation(const AName: string);
begin
  FAnim.AnimationSequence_StartAndStopOther([AName, 'Altar_Idle0'], True);
end;

procedure TRoomAltar.LoadModels(const AObstacle: TObstacleDesc);
begin
  inherited LoadModels(AObstacle);
  FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);
  FAnim.AnimationSequence_StartAndStopOther(['Altar_Idle0'], True);

  Equip(TArcherBow.Create);
end;

function TRoomAltar.Interactive_CellsCount: Integer;
begin
  if FItem = nil then
    Result := 0
  else
    Result := 2;
end;

function TRoomAltar.Interactive_GetCell(AIndex: Integer): TVec2i;
begin
  if AIndex = 0 then
    Result := Vec(1, -1)
  else
    Result := Vec(0, 1);
end;

function TRoomAltar.Interactive_Cost(AIndex: Integer): Integer;
begin
  Result := 2;
end;

function TRoomAltar.Interactive_Try(AUnit: TRoomUnit): IBRA_Action;
var
  idx: Integer;
begin
  Result := nil;
  idx := GetInteractiveIdx(AUnit);
  if idx < 0 then Exit;
  if AUnit.AP < Interactive_Cost(idx) then Exit;
  AUnit.AP := AUnit.AP - Interactive_Cost(idx);
  SetAnimation('Altar_Action' + IntToStr(idx));
  Result := TBRA_AltarPick.Create(Self, AUnit);
end;

{ TBRA_LootChestAction }

function TBRA_LootChestAction.GetBattleRoom: TBattleRoom;
var obj: TavObject;
begin
  obj := FChest;
  repeat
    obj := obj.parent;
    if obj is TBattleRoom then Exit(TBattleRoom(obj));
  until obj = nil;
end;

procedure TBRA_LootChestAction.ShowOtherInventory;
begin
  GetBattleRoom.UI_SetOtherInventory(FChest.Inventory());
end;

procedure TBRA_LootChestAction.HideOtherInventory;
begin
  GetBattleRoom.UI_SetOtherInventory(nil);
end;

procedure TBRA_LootChestAction.TryCancel;
begin
  FInAction := False;
  FChest.SetAnimation('Chest_Close');
  HideOtherInventory;
end;

function TBRA_LootChestAction.ProcessAction: Boolean;
begin
  Result := FInAction;
  if not Result then Exit;

  if FMenuStart < FChest.World.GameTime then
    ShowOtherInventory;
end;

constructor TBRA_LootChestAction.Create(AChest: TRoomChest; AUnit: TRoomUnit);
begin
  FChest := AChest;
  FUnit := AUnit;
  FInAction := True;
  FMenuStart := FChest.World.GameTime + 500;

  AChest.SetAnimation('Chest_Open');
end;

{ TRoomChest }

procedure TRoomChest.UpdateStep;
begin
  inherited UpdateStep;
  FAnim.SetTime(World.GameTime);
end;

procedure TRoomChest.AfterRegister;
begin
  inherited AfterRegister;
  FInventory := TInventory.Create(Self);

  FInventory.Push(TAxe.Create, 0);
  FInventory.Push(TArcherBow.Create, 0);

  SubscribeForUpdateStep;
end;

procedure TRoomChest.SetAnimation(const AName: string);
begin
  FAnim.AnimationSequence_StartAndStopOther([AName], False);
end;

procedure TRoomChest.LoadModels(const AObstacle: TObstacleDesc);
begin
  inherited LoadModels(AObstacle);
  FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);
end;

function TRoomChest.Inventory: IInventory;
begin
  Result := FInventory;
end;

function TRoomChest.Interactive_CellsCount: Integer;
begin
  Result := 1;
end;

function TRoomChest.Interactive_GetCell(AIndex: Integer): TVec2i;
begin
  Result := Vec(1, 0);
end;

function TRoomChest.Interactive_Cost(AIndex: Integer): Integer;
begin
  Result := 1;
end;

function TRoomChest.Interactive_Try(AUnit: TRoomUnit): IBRA_Action;
var absCrd: TVec2i;
    i: Integer;
begin
  Result := nil;
  if AUnit = nil then Exit;

  for i := 0 to Interactive_CellsCount - 1 do
  begin
    if AUnit.AP < Interactive_Cost(i) then Continue;

    absCrd := TTileUtils.RotateTileCoord(Interactive_GetCell(i), RoomDir);
    absCrd := absCrd + RoomPos;
    if absCrd <> AUnit.RoomPos then Continue;

    AUnit.AP := AUnit.AP - Interactive_Cost(i);
    AUnit.RoomDir := Room.Direction(AUnit.RoomPos, RoomPos);

    Result := TBRA_LootChestAction.Create(Self, AUnit);
    Exit;
  end;
end;

initialization
RegRoomClass(TRoomChest);
RegRoomClass(TRoomAltar);

finalization

end.

