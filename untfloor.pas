unit untFloor;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avRes, avContnrs, avMiniControls, avTypes, untLevel, mutils, intfUtils,
  avContnrsDefaults,
  avPathFinder,
  ui_unit, ui_inventory, ui_skills, ui_gamecamera, ui_messages;

type
  TFloorMap = class;
  IFloorMapNonWeightedGraph = {$IfDef FPC}specialize{$EndIf}INonWeightedGraph<TVec2i>;

  { TGameUI }

  TGameUI = class(TavMainRenderChild, IGameUI)
  private
    FRootControl    : TavmBaseControl;
    FUnitMenu       : TavmCustomControl;
    FPlayerInventory: TavmCustomControl;
    FPlayerSkills   : TavmCustomControl;
    FOtherInventory : TavmCustomControl;
    FMessages       : TavmMessages;

    FOnEndTurnBtnClick: TNotifyEvent;
  private
    function IsMouseOnUI: Boolean;

    procedure SetActiveUnit(const ARoomUnit: TRoomUnit);
    procedure SetPlayerActiveSkill(const ASkill: IUnitSkill);
    procedure SetOtherInventory(const AInventory: IInventory);

    procedure AddMessage(const AMsg: string);
    procedure UpdateStep(const AIsPlayerTurn: Boolean);
  private
    procedure AlignControls;
    procedure DoOnEndTurnBtnClick(ASender: TObject);
  private
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  protected
    procedure CreateUI();
    procedure AfterRegister; override;
  public
    procedure SetPlayer(const APlayer: TPlayer);
    procedure Draw2DUI();
    property OnEndTurnBtnClick: TNotifyEvent read FOnEndTurnBtnClick write FOnEndTurnBtnClick;
  end;

  { TFloorMapGraph }

  TFloorMapGraph = class(TInterfacedObject, IEqualityComparer, IFloorMapNonWeightedGraph)
  private
    FFloor: TFloorMap;
    function Hash(const Value): Cardinal;
    function IsEqual(const Left, Right): Boolean;

    function MaxNeighbourCount(const ANode: TVec2i): Integer;
    function NodeComparer: IEqualityComparer;
    function GetNeighbour(Index: Integer; const ACurrent: TVec2i; out ANeighbour: TVec2i): Boolean; overload;
  public
    constructor Create(AFloor: TFloorMap);
  end;

  TBattleRoomAdapter = record
    dummy: TBattleRoom;
    room : TBattleRoom;
  end;
  PBattleRoomAdapter = ^TBattleRoomAdapter;

  { TFloorMap }

  TFloorMap = class(TavMainRenderChild)
  private type
    TRoomsMap = {$IFDef FPC}specialize{$EndIf} THashMap<TVec2i, TBattleRoomAdapter>;
    IRoomsMap = {$IFDef FPC}specialize{$EndIf} IHashMap<TVec2i, TBattleRoomAdapter>;
  private
    FCurrentRoom: TBattleRoom;
    FRooms: IRoomsMap;

    FUI: TGameUI;
    FPlayer: TPlayer;
    procedure CreatePlayer;
    procedure DoOnEndTurnBtnClick(ASender: TObject);
    procedure DoLeaveBattleRoom(const ABattleRoom: TBattleRoom; const ADoorIdx: Integer);

    function  GetRoomDoors(const ARoomCoord: TVec2i): TDoors;
    procedure SetCurrentRoom(const ARoomCoord: TVec2i);
  protected
    procedure AfterRegister; override;
  public
    procedure Create2Rooms;

    procedure Draw2DUI();

    property CurrentRoom: TBattleRoom read FCurrentRoom;
  end;

implementation

{ TFloorMapGraph }

function TFloorMapGraph.Hash(const Value): Cardinal;
begin
  Result := Murmur2DefSeed(Value, SizeOf(TVec2i));
end;

function TFloorMapGraph.IsEqual(const Left, Right): Boolean;
var nl: TVec2i absolute Left;
    nr: TVec2i absolute Right;
begin
  Result := (nl.x = nr.x) and (nl.y = nr.y);
end;

function TFloorMapGraph.MaxNeighbourCount(const ANode: TVec2i): Integer;
begin
  Result := 6;
end;

function TFloorMapGraph.NodeComparer: IEqualityComparer;
begin
  Result := Self;
end;

function TFloorMapGraph.GetNeighbour(Index: Integer; const ACurrent: TVec2i; out ANeighbour: TVec2i): Boolean;
begin
  ANeighbour := TTileUtils.NeighbourTile(ACurrent, Index);
  Result := FFloor.FRooms.Contains(ANeighbour);
end;

constructor TFloorMapGraph.Create(AFloor: TFloorMap);
begin
  FFloor := AFloor;
end;

{ TGameUI }

function TGameUI.IsMouseOnUI: Boolean;
var curpt: TVec2;
    hit: TavmBaseControl;
begin
  curpt := (Main.Cursor.WindowCur*Vec(0.5, -0.5) + Vec(0.5, 0.5) )*Main.WindowSize;
  if FRootControl.InputConnector.Captured <> nil then Exit(True);
  hit := FRootControl.HitTest(curpt, True);
  Result := (hit <> nil) and (hit <> FRootControl);
end;

procedure TGameUI.SetOtherInventory(const AInventory: IInventory);
begin
  (FOtherInventory as TavmInventory).Inventory := AInventory;
  FOtherInventory.Visible := (FOtherInventory as TavmInventory).Inventory <> nil;
end;

procedure TGameUI.SetPlayerActiveSkill(const ASkill: IUnitSkill);
begin
  TavmSkills(FPlayerSkills).ActiveSkill := ASkill;
end;

procedure TGameUI.SetActiveUnit(const ARoomUnit: TRoomUnit);
begin
  (FUnitMenu as TavmUnitMenu).RoomUnit := ARoomUnit;
  (FUnitMenu as TavmUnitMenu).SkillSlots.ActiveSkill := nil;
end;

procedure TGameUI.AddMessage(const AMsg: string);
begin
  if FMessages = nil then Exit;
  FMessages.AddMessage(AMsg);
end;

procedure TGameUI.UpdateStep(const AIsPlayerTurn: Boolean);
begin
  AlignControls;

  if AIsPlayerTurn then
    TavmUnitMenu(FUnitMenu).SkillSlots.ActiveSkill := TavmSkills(FPlayerSkills).ActiveSkill
  else
    TavmUnitMenu(FUnitMenu).SkillSlots.ActiveSkill := nil;
end;

procedure TGameUI.AlignControls;
begin
  if FPlayerSkills <> nil then
    FPlayerSkills.Pos := Vec(Main.WindowSize.x - 10, 10);
  if FMessages <> nil then
    FMessages.Pos := Vec(10, Main.WindowSize.y - 200);
end;

procedure TGameUI.DoOnEndTurnBtnClick(ASender: TObject);
begin
  if Assigned(FOnEndTurnBtnClick) then FOnEndTurnBtnClick(Self);
end;

function TGameUI.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TGameUI._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TGameUI._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

procedure TGameUI.Draw2DUI();
begin
  if FRootControl <> nil then
    FRootControl.Draw();
end;

procedure TGameUI.CreateUI();
var
  menu: TavmUnitMenu;
  inv_ui: TavmInventory;
  skills_ui: TavmSkills;
  lookAt: TVec3;
begin
  FRootControl := TavmCameraControl.Create(Self);
  FRootControl.Size := Vec(10000, 10000);

  lookAt :=  Vec(0,0,0);// FMap.UI.TilePosToWorldPos(FPlayer.RoomPos);
  TavmCameraControl(FRootControl).LookAt(lookAt, -lookAt);

  FMessages := TavmMessages.Create(FRootControl);
  FMessages.Origin := Vec(0, 1);

  menu := TavmUnitMenu.Create(FRootControl);
  menu.OnEndTurnClick := {$IfDef FPC}@{$EndIf}DoOnEndTurnBtnClick;
  FUnitMenu := menu;

  inv_ui := TavmInventory.Create(FRootControl);
  FPlayerInventory := inv_ui;

  skills_ui := TavmSkills.Create(FRootControl);
  skills_ui.Pos := Vec(30, 10);
  skills_ui.HintDirection := Vec(-0.7, 0.7);
  FPlayerSkills := skills_ui;

  inv_ui := TavmInventory.Create(FRootControl);
  inv_ui.Pos := Vec(inv_ui.Pos.x + 500, inv_ui.Pos.y);
  inv_ui.Visible := False;
  FOtherInventory := inv_ui;
end;

procedure TGameUI.AfterRegister;
begin
  inherited AfterRegister;
  CreateUI();
end;

procedure TGameUI.SetPlayer(const APlayer: TPlayer);
begin
  TavmInventory(FPlayerInventory).Inventory := APlayer.Inventory;
  TavmSkills(FPlayerSkills).Skills := TAllSkillListAdapter.Create(APlayer);
end;

{ TFloorMap }

procedure TFloorMap.CreatePlayer;

  function GetSpawnPlace(): TVec2i;
  begin
    repeat
      Result.x := Random(FCurrentRoom.Map.Radius*2+1) - FCurrentRoom.Map.Radius;
      Result.y := Random(FCurrentRoom.Map.Radius*2+1) - FCurrentRoom.Map.Radius;
      if FCurrentRoom.Map.IsCellExists(Result) and not FCurrentRoom.Map.IsCellBlocked(Result) then
        Exit(Result);
    until False;
  end;

begin
  Assert(FCurrentRoom <> nil);

  FPlayer := TPlayer.Create(FCurrentRoom.Map);
  //FPlayer.SetRoomPosDir(Vec(5, 5), 0);
  FPlayer.LoadModels();
  FPlayer.SetRoomPosDir(GetSpawnPlace(), Random(6));
end;

procedure TFloorMap.DoOnEndTurnBtnClick(ASender: TObject);
begin
  if FCurrentRoom = nil then Exit;
  if not FCurrentRoom.IsPlayerTurn then Exit;
  FCurrentRoom.EndTurn();
end;

procedure TFloorMap.DoLeaveBattleRoom(const ABattleRoom: TBattleRoom; const ADoorIdx: Integer);
var map: IFloorMapNonWeightedGraph;
    newRoom: TVec2i;
begin
  map := TFloorMapGraph.Create(Self);
  if map.GetNeighbour(ADoorIdx, ABattleRoom.RoomPos, newRoom) then
    SetCurrentRoom(newRoom);
end;

function TFloorMap.GetRoomDoors(const ARoomCoord: TVec2i): TDoors;
var map: IFloorMapNonWeightedGraph;
    dummy: TVec2i;
    i: Integer;
begin
  map := TFloorMapGraph.Create(Self);
  for i := 0 to map.MaxNeighbourCount(ARoomCoord) - 1 do
    Result[i] := map.GetNeighbour(i, ARoomCoord, dummy);
end;

procedure TFloorMap.SetCurrentRoom(const ARoomCoord: TVec2i);
var broom: TBattleRoom;
    pRoom: PBattleRoomAdapter;
begin
  if FCurrentRoom <> nil then
    if FCurrentRoom.RoomPos = ARoomCoord then Exit;

  pRoom := PBattleRoomAdapter(FRooms.PItem[ARoomCoord]);
  if pRoom^.room = nil then
  begin
    broom := TBattleRoom.Create(Self);
    broom.RoomPos := ARoomCoord;
    broom.GenerateWithLoad('rooms\r1.room', GetRoomDoors(ARoomCoord));
    broom.UI := FUI;
    broom.OnLeaveBattleRoom := {$IfDef FPC}@{$EndIf}DoLeaveBattleRoom;

    pRoom^.room := broom;
  end;

  FCurrentRoom := pRoom^.room;

  if FPlayer = nil then CreatePlayer;
  FUI.SetPlayer(FPlayer);
  FCurrentRoom.AttachPlayer(FPlayer, Vec(0,0), 0);
end;

procedure TFloorMap.AfterRegister;
begin
  inherited AfterRegister;
  FRooms := TRoomsMap.Create();
  FUI := TGameUI.Create(Self);
  FUI.OnEndTurnBtnClick := {$IfDef FPC}@{$EndIf}DoOnEndTurnBtnClick;
end;

procedure TFloorMap.Create2Rooms;
var empty: TBattleRoomAdapter;
begin
  ZeroClear(empty, SizeOf(empty));
  FRooms.Add(Vec(0, 0), empty);
  FRooms.Add(Vec(1, 0), empty);

  SetCurrentRoom(Vec(0, 0));
end;

procedure TFloorMap.Draw2DUI();
begin
  FUI.Draw2DUI();
end;

end.

