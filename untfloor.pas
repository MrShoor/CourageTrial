unit untFloor;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avRes, avContnrs, avMiniControls, avTypes, untLevel, mutils, intfUtils,
  avBase,
  avContnrsDefaults,
  avPathFinder,
  ui_wndbutton,
  ui_unit, ui_inventory, ui_skills, ui_gamecamera, ui_messages, ui_enemies, ui_ingame_menu,
  generator;

type
  TFloorMap = class;
  IFloorMapNonWeightedGraph = {$IfDef FPC}specialize{$EndIf}INonWeightedGraph<TVec2i>;

  { TGameUI }

  TGameUI = class(TavMainRenderChild, IGameUI)
  private
    FInGameMenu : TavmInGameMenu;

    FRootControl    : TavmCameraControl;
    FUnitMenu       : TavmCustomControl;
    FEnemiesBar     : TavmEnemiesBar;
    FPlayerInventory: TavmCustomControl;
    FPlayerSkills   : TavmCustomControl;
    FOtherInventory : TavmCustomControl;
    FMessages       : TavmMessages;

    FInventoryBtn   : TavmWndCheckButton;
    FSkillsBtn      : TavmWndCheckButton;

    FOtherInventory_TakeAllBtn: TavmOtherInventoryBtn;
    FOtherInventory_CloseBtn  : TavmOtherInventoryBtn;

    FOnEndTurnBtnClick: TNotifyEvent;
    FOnLootGroundClick: TNotifyEvent;

    FOtherInventoryCloseCallback: TNotifyEvent;

    procedure InventoryBtnCheck(ASender: TObject);
    procedure SkillsBtnCheck(ASender: TObject);
    procedure OnOtherInventoryBtnTakeAll(ASender: TObject);
    procedure OnOtherInventoryBtnClose(ASender: TObject);
  private
    function IsMouseOnUI: Boolean;

    procedure SetActiveUnit(const ARoomUnit: TRoomUnit);
    procedure SetPlayerActiveSkill(const ASkill: IUnitSkill);
    procedure SetOtherInventory(const AInventory: IInventory; ACloseCallback: TNotifyEvent);

    procedure AdjustCameraToPlayer();
    procedure SetCameraBounds(const ABounds: TRectF);

    procedure InvalidateEnemiesBar;
    procedure SetEnemiesList(const AEnemies: IRoomUnitArr);

    procedure AddMessage(const AMsg: string);
    procedure UpdateStep(const AIsPlayerTurn: Boolean);
  private
    procedure AlignControls;
    procedure DoOnEndTurnBtnClick(ASender: TObject);
    procedure DoOnLootGroundClick(ASender: TObject);
    procedure DoOnAdjustCameraToUnit(ASender: TObject);
  protected
    FOnMenuExit: TNotifyEvent;
    FOnMenuNewGame: TNotifyEvent;
    function GetInGameMenuVisible: Boolean;
    procedure SetInGameMenuVisible(const AValue: Boolean);
    procedure DoOnMenuResume(ASender: TObject);
    procedure DoOnMenuNewGame(ASender: TObject);
    procedure DoOnMenuExit(ASender: TObject);
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
    property OnLootGroundClick: TNotifyEvent read FOnLootGroundClick write FOnLootGroundClick;

    property InGameMenuVisible: Boolean read GetInGameMenuVisible write SetInGameMenuVisible;
    property OnMenuNewGame: TNotifyEvent read FOnMenuNewGame write FOnMenuNewGame;
    property OnMenuExit   : TNotifyEvent read FOnMenuExit    write FOnMenuExit;
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
    RoomRot: Integer;
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
    FAllRoomFiles: TStringList;

    FCurrentRoom: TBattleRoom;
    FRooms: IRoomsMap;

    FVisitedRooms: IVisitedRooms;

    FUI: TGameUI;
    FPlayer: TPlayer;
    procedure CreatePlayer;
    procedure DoOnEndTurnBtnClick(ASender: TObject);
    procedure DoOnLootGroundClick(ASender: TObject);
    procedure DoLeaveBattleRoom(const ABattleRoom: TBattleRoom; const ADoorIdx: Integer);

    function ObtainBattleRoom(const ARoomCoord: TVec2i; const AForPlayer: TPlayer): TBattleRoom;

    function  GetRoomDoors(const ARoomCoord: TVec2i): TDoors;
    procedure MoveToRoom(const ARoomCoord: TVec2i; const AFromDoor: Integer);
    procedure SetCurrentRoom(const ARoomCoord: TVec2i);

    procedure ShowAllRooms();
    procedure UpdateBoundsAtVisibleRooms;
  protected
    procedure AfterRegister; override;
  public
    class function RoomsDirectory: UnicodeString;

    procedure Create2Rooms;
    procedure CreateLab(ARoomCount: Integer);

    procedure Draw2DUI();

    property CurrentRoom: TBattleRoom read FCurrentRoom;
    property UI: TGameUI read FUI;
  public
    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
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

procedure TGameUI.InventoryBtnCheck(ASender: TObject);
begin
  FPlayerInventory.Visible := FInventoryBtn.Checked;
end;

procedure TGameUI.SkillsBtnCheck(ASender: TObject);
begin
  FPlayerSkills.Visible := FSkillsBtn.Checked;
end;

procedure TGameUI.OnOtherInventoryBtnTakeAll(ASender: TObject);
var otherInv: IInventory;
    thisInv : IInventory;
begin
  thisInv := TavmInventory(FPlayerInventory).Inventory;
  otherInv := TavmInventory(FOtherInventory).Inventory;
  if otherInv = nil then Exit;
  if thisInv = nil then Exit;
  while otherInv.Items.Count > 0 do
   thisInv.Push(otherInv.Pop(0), thisInv.Items.Count - 1);
  TavmInventory(FPlayerInventory).UpdateScroll;
  TavmInventory(FOtherInventory).UpdateScroll;
end;

procedure TGameUI.OnOtherInventoryBtnClose(ASender: TObject);
begin
  if Assigned(FOtherInventoryCloseCallback) then
    FOtherInventoryCloseCallback(nil);
end;

function TGameUI.IsMouseOnUI: Boolean;
var curpt: TVec2;
    hit: TavmBaseControl;
begin
  if FInGameMenu.Visible then Exit(True);

  curpt := (Main.Cursor.WindowCur*Vec(0.5, -0.5) + Vec(0.5, 0.5) )*Main.WindowSize;
  if FRootControl.InputConnector.Captured <> nil then Exit(True);
  hit := FRootControl.HitTest(curpt, True);
  Result := (hit <> nil) and (hit <> FRootControl);
end;

procedure TGameUI.SetOtherInventory(const AInventory: IInventory; ACloseCallback: TNotifyEvent);
begin
  (FOtherInventory as TavmInventory).Inventory := AInventory;
  FOtherInventory.Visible := (FOtherInventory as TavmInventory).Inventory <> nil;
  FOtherInventory_TakeAllBtn.Visible := FOtherInventory.Visible;
  FOtherInventory_CloseBtn.Visible := FOtherInventory.Visible;
  FOtherInventoryCloseCallback := ACloseCallback;
end;

procedure TGameUI.SetPlayerActiveSkill(const ASkill: IUnitSkill);
begin
  TavmSkills(FPlayerSkills).ActiveSkill := ASkill;
end;

procedure TGameUI.SetActiveUnit(const ARoomUnit: TRoomUnit);
begin
  FEnemiesBar.SetHighlighedEnemy(ARoomUnit);
  if ARoomUnit is TPlayer then
    (FUnitMenu as TavmUnitMenu).RoomUnit := ARoomUnit;
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

procedure TGameUI.AdjustCameraToPlayer();
begin
  DoOnAdjustCameraToUnit(nil);
end;

procedure TGameUI.SetCameraBounds(const ABounds: TRectF);
begin
  FRootControl.FloorBoundingRect := ABounds;
end;

procedure TGameUI.InvalidateEnemiesBar;
begin
  FEnemiesBar.Invalidate;
end;

procedure TGameUI.SetEnemiesList(const AEnemies: IRoomUnitArr);
begin
  FEnemiesBar.SetEnemies(AEnemies);
end;

procedure TGameUI.AlignControls;
begin
  if FPlayerSkills <> nil then
  begin
    FPlayerSkills.Pos := Vec(Main.WindowSize.x - 10, 100);
    if FSkillsBtn <> nil then
      FSkillsBtn.Pos := Vec(FPlayerSkills.Pos.x - Round(FPlayerSkills.Size.x) div 2, 5);
  end;
  if FMessages <> nil then
    FMessages.Pos := Vec(10, Main.WindowSize.y - 145);
end;

procedure TGameUI.DoOnEndTurnBtnClick(ASender: TObject);
begin
  if Assigned(FOnEndTurnBtnClick) then FOnEndTurnBtnClick(Self);
end;

procedure TGameUI.DoOnLootGroundClick(ASender: TObject);
begin
  if Assigned(FOnLootGroundClick) then FOnLootGroundClick(Self);
end;

procedure TGameUI.DoOnAdjustCameraToUnit(ASender: TObject);
var unt: TRoomUnit;
begin
  unt := TavmUnitMenu(FUnitMenu).RoomUnit;
  if unt = nil then Exit;
  FRootControl.LookAt(unt.Room.UI.TilePosToWorldPos(unt.RoomPos), True);
end;

procedure TGameUI.DoOnMenuResume(ASender: TObject);
begin
  InGameMenuVisible := False;
end;

procedure TGameUI.DoOnMenuNewGame(ASender: TObject);
begin
  if Assigned(FOnMenuNewGame) then FOnMenuNewGame(Self);
end;

procedure TGameUI.DoOnMenuExit(ASender: TObject);
begin
  if Assigned(FOnMenuExit) then FOnMenuExit(Self);
end;

function TGameUI.GetInGameMenuVisible: Boolean;
begin
  Result := FInGameMenu.Visible;
end;

function TGameUI.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

procedure TGameUI.SetInGameMenuVisible(const AValue: Boolean);
begin
  FInGameMenu.Visible := AValue;
  FRootControl.Disabled := AValue;
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
  if (FInGameMenu <> nil) and FInGameMenu.Visible then
  begin
    FInGameMenu.Pos := Vec(Integer(Main.WindowSize.x div 2), Main.WindowSize.y div 2);
    FInGameMenu.Draw();
  end;
end;

procedure TGameUI.CreateUI();
var
  menu: TavmUnitMenu;
  inv_ui: TavmInventory;
  skills_ui: TavmSkills;
  lookAt: TVec3;
begin
  FInGameMenu := TavmInGameMenu.Create(Self);
  FInGameMenu.Visible := False;
  FInGameMenu.OnResume := {$IfDef FPC}@{$EndIf}DoOnMenuResume;
  FInGameMenu.OnNewGame := {$IfDef FPC}@{$EndIf}DoOnMenuNewGame;
  FInGameMenu.OnExit := {$IfDef FPC}@{$EndIf}DoOnMenuExit;

  FRootControl := TavmCameraControl.Create(Self);
  FRootControl.Size := Vec(10000, 10000);

  lookAt :=  Vec(0,0,0);// FMap.UI.TilePosToWorldPos(FPlayer.RoomPos);
  TavmCameraControl(FRootControl).LookAt(lookAt, -lookAt);

  FMessages := TavmMessages.Create(FRootControl);
  FMessages.Origin := Vec(0, 1);

  menu := TavmUnitMenu.Create(FRootControl);
  menu.OnEndTurnClick := {$IfDef FPC}@{$EndIf}DoOnEndTurnBtnClick;
  menu.OnLootGround   := {$IfDef FPC}@{$EndIf}DoOnLootGroundClick;
  menu.OnAdjustToUnit := {$IfDef FPC}@{$EndIf}DoOnAdjustCameraToUnit;
  FUnitMenu := menu;

  FEnemiesBar := TavmEnemiesBar.Create(FRootControl);

  inv_ui := TavmInventory.Create(FRootControl);
  inv_ui.Pos := Vec(inv_ui.Pos.x, 100);
  FPlayerInventory := inv_ui;

  skills_ui := TavmSkills.Create(FRootControl);
  skills_ui.Pos := Vec(30, 100);
  skills_ui.HintDirection := Vec(-0.7, 0.7);
  FPlayerSkills := skills_ui;

  inv_ui := TavmInventory.Create(FRootControl);
  inv_ui.Pos := Vec(FPlayerInventory.Pos.x + 430, FPlayerInventory.Pos.y);
  inv_ui.Visible := False;
  FOtherInventory := inv_ui;

  FOtherInventory_TakeAllBtn := TavmOtherInventoryBtn.Create(FRootControl);
  FOtherInventory_TakeAllBtn.Text := 'Взять всё';
  FOtherInventory_TakeAllBtn.Pos := Vec(FOtherInventory.Pos.x, FOtherInventory.Pos.y + FOtherInventory.Size.y);
  FOtherInventory_TakeAllBtn.Visible := False;
  FOtherInventory_TakeAllBtn.OnClick := {$IfDef FPC}@{$EndIf}OnOtherInventoryBtnTakeAll;

  FOtherInventory_CloseBtn   := TavmOtherInventoryBtn.Create(FRootControl);
  FOtherInventory_CloseBtn.Text := 'Закрыть';
  FOtherInventory_CloseBtn.Origin := Vec(1, 0);
  FOtherInventory_CloseBtn.Pos := Vec(FOtherInventory.Pos.x + FOtherInventory.Size.x, FOtherInventory.Pos.y + FOtherInventory.Size.y);
  FOtherInventory_CloseBtn.Visible := False;
  FOtherInventory_CloseBtn.OnClick := {$IfDef FPC}@{$EndIf}OnOtherInventoryBtnClose;

  FInventoryBtn := TavmWndCheckButton.Create(FRootControl);
  FInventoryBtn.Origin := Vec(0.5, 0);
  FInventoryBtn.Pos := Vec(FPlayerInventory.Pos.x + Round(FPlayerInventory.Size.x) div 2, 5);
  FInventoryBtn.Text := 'Инвентарь';
  FInventoryBtn.Checked := FPlayerInventory.Visible;
  FInventoryBtn.OnCheck := {$IfDef FPC}@{$EndIf}InventoryBtnCheck;

  FSkillsBtn := TavmWndCheckButton.Create(FRootControl);
  FSkillsBtn.Origin := Vec(0.5, 0);
  FSkillsBtn.Text := 'Навыки';
  FSkillsBtn.Checked := FPlayerSkills.Visible;
  FSkillsBtn.OnCheck := {$IfDef FPC}@{$EndIf}SkillsBtnCheck;
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

procedure TFloorMap.DoOnLootGroundClick(ASender: TObject);
begin
  if FCurrentRoom = nil then Exit;
  if not FCurrentRoom.IsPlayerTurn then Exit;
  if FCurrentRoom.InAction() then Exit;
  FCurrentRoom.AddAction(TBRA_LootGround.Create(FCurrentRoom.Player));
end;

procedure TFloorMap.DoLeaveBattleRoom(const ABattleRoom: TBattleRoom; const ADoorIdx: Integer);
var map: IFloorMapNonWeightedGraph;
    worldDir: Integer;
    newRoom: TVec2i;
    newDoor: Integer;
begin
  map := TFloorMapGraph.Create(Self);
  worldDir := (ADoorIdx + ABattleRoom.RoomDir) mod 6;
  if map.GetNeighbour(worldDir, ABattleRoom.RoomPos, newRoom) then
  begin
    newDoor := worldDir + 3;
    MoveToRoom(newRoom, newDoor);
  end;
end;

function TFloorMap.ObtainBattleRoom(const ARoomCoord: TVec2i; const AForPlayer: TPlayer): TBattleRoom;
var broom: TBattleRoom;
    pRoom: PBattleRoomAdapter;
    dir  : string;
    newRoomName: string;
begin
  dir := string(RoomsDirectory);
  pRoom := PBattleRoomAdapter(FRooms.PItem[ARoomCoord]);
  if pRoom^.room = nil then
  begin
    broom := TBattleRoom.Create(Self);
    broom.RoomPos := ARoomCoord;
    broom.RoomDir := pRoom^.RoomRot;
    //broom.GenerateWithLoad('rooms\r'+IntToStr(Random(3)+1)+'.room', GetRoomDoors(ARoomCoord));
    newRoomName := GenRoom(FVisitedRooms, FAllRoomFiles);
    broom.GenerateWithLoad(dir + '\' + newRoomName, GetRoomDoors(ARoomCoord), AForPlayer, FVisitedRooms);
    broom.UI := FUI;
    broom.OnLeaveBattleRoom := {$IfDef FPC}@{$EndIf}DoLeaveBattleRoom;
    FVisitedRooms.Add(newRoomName);

    pRoom^.room := broom;
  end;
  Result := pRoom^.room;
  UpdateBoundsAtVisibleRooms;
end;

function TFloorMap.GetRoomDoors(const ARoomCoord: TVec2i): TDoors;
var map: IFloorMapNonWeightedGraph;
    dummy: TVec2i;
    i: Integer;
    rot: Integer;
begin
  rot := FRooms[ARoomCoord].RoomRot;
  map := TFloorMapGraph.Create(Self);
  for i := 0 to map.MaxNeighbourCount(ARoomCoord) - 1 do
  begin
    Result[i] := map.GetNeighbour(i + rot, ARoomCoord, dummy);
  end;
end;

procedure TFloorMap.MoveToRoom(const ARoomCoord: TVec2i; const AFromDoor: Integer);
var broom: TBattleRoom;
    doorIdx: Integer;
begin
  if FCurrentRoom <> nil then
    if FCurrentRoom.RoomPos = ARoomCoord then Exit;

  broom := ObtainBattleRoom(ARoomCoord, FPlayer);
  FCurrentRoom := broom;

  if FPlayer = nil then CreatePlayer;
  FUI.SetPlayer(FPlayer);

  doorIdx := (AFromDoor - broom.RoomDir) mod 6;
  if doorIdx < 0 then doorIdx := doorIdx + 6;
  FCurrentRoom.AttachPlayer(FPlayer, doorIdx);
end;

procedure TFloorMap.SetCurrentRoom(const ARoomCoord: TVec2i);
var broom: TBattleRoom;
begin
  if FCurrentRoom <> nil then
    if FCurrentRoom.RoomPos = ARoomCoord then Exit;

  broom := ObtainBattleRoom(ARoomCoord, FPlayer);
  FCurrentRoom := broom;

  if FPlayer = nil then CreatePlayer;
  FUI.SetPlayer(FPlayer);
  FCurrentRoom.AttachPlayer(FPlayer, Vec(0,0), 0);

  FUI.AdjustCameraToPlayer();
end;

procedure TFloorMap.ShowAllRooms();
var roomCoords: TVec2i;
    allRooms: array of TVec2i;
    i: Integer;
begin
  SetLength(allRooms, FRooms.Count);
  i := 0;
  FRooms.Reset;
  while FRooms.NextKey(roomCoords) do
  begin
    allRooms[i] := roomCoords;
    Inc(i);
  end;

  for i := 0 to Length(allRooms) - 1 do
    ObtainBattleRoom(allRooms[i], FPlayer);
end;

procedure TFloorMap.UpdateBoundsAtVisibleRooms;
var crd: TVec2i;
    broomAdapt: TBattleRoomAdapter;
    broom: TBattleRoom;
    box: TAABB;
begin
  box := EmptyAABB;
  FRooms.Reset;
  while FRooms.Next(crd, broomAdapt) do
  begin
    if broomAdapt.room <> nil then
      broom := broomAdapt.room
    else
      broom := broomAdapt.dummy;
    if broom <> nil then
      box := box + AABB(broom.WorldPos - Vec(21, 2, 21), broom.WorldPos + Vec(21, 5, 21));
  end;
  if not box.IsEmpty then
    FUI.SetCameraBounds(RectF(Vec(box.min.x, box.min.z), Vec(box.max.x, box.max.z)));
end;

procedure TFloorMap.AfterRegister;
begin
  inherited AfterRegister;
  FRooms := TRoomsMap.Create();
  FUI := TGameUI.Create(Self);
  FUI.OnEndTurnBtnClick := {$IfDef FPC}@{$EndIf}DoOnEndTurnBtnClick;
  FUI.OnLootGroundClick := {$IfDef FPC}@{$EndIf}DoOnLootGroundClick;
end;

class function TFloorMap.RoomsDirectory: UnicodeString;
begin
  Result := UnicodeString(ExtractFileDir(ParamStr(0))+'\rooms');
end;

procedure TFloorMap.Create2Rooms;
var empty: TBattleRoomAdapter;
begin
  ZeroClear(empty, SizeOf(empty));
  empty.RoomRot := 0;
  FRooms.Add(Vec(0, 0), empty);
  empty.RoomRot := 2;
  FRooms.Add(Vec(1, 0), empty);

  //empty.RoomRot := 3;
  //FRooms.Add(Vec(2, 0), empty);
  //
  //empty.RoomRot := 4;
  //FRooms.Add(Vec(3, 0), empty);
  //
  //empty.RoomRot := 5;
  //FRooms.Add(Vec(4, 0), empty);
  //
  //empty.RoomRot := 6;
  //FRooms.Add(Vec(5, 0), empty);
  //
  //empty.RoomRot := 7;
  //FRooms.Add(Vec(6, 0), empty);

  SetCurrentRoom(Vec(0, 0));

  ShowAllRooms();
end;

procedure TFloorMap.CreateLab(ARoomCount: Integer);

  function AddRoomCoords: TVec2i;

    function GetNeighboursCount(const ACrd: TVec2i): Integer;
    var i: Integer;
    begin
      Result := 0;
      for i := 0 to 5 do
        if FRooms.Contains(TTileUtils.RotateTileCoord(Vec(1, 0), i) + ACrd) then
          Inc(Result);
    end;

  var roomCrd: TVec2i;
      rooms2: IVec2iArr;
      i, phase: Integer;
  begin
    rooms2 := untLevel.TVec2iArr.Create;

    FRooms.Reset;
    while FRooms.NextKey(roomCrd) do
      if GetNeighboursCount(roomCrd) < 3 then
        rooms2.Add(roomCrd);

    repeat
      roomCrd := rooms2[Random(rooms2.Count)];
      phase := Random(6);
      for i := 0 to 5 do
      begin
        Result := TTileUtils.RotateTileCoord(Vec(1, 0), i + phase) + roomCrd;
        if FRooms.Contains(Result) then Break;
        if GetNeighboursCount(Result) < 3 then Exit;
      end;
    until False;
    Result := Vec(0,0);
  end;

var empty: TBattleRoomAdapter;

begin
  ZeroClear(empty, SizeOf(empty));
  empty.RoomRot := Random(6);
  FRooms.Add(Vec(0, 0), empty);

  while ARoomCount > 1 do
  begin
    empty.RoomRot := Random(6);
    FRooms.Add(AddRoomCoords(), empty);
    Dec(ARoomCount);
  end;

  SetCurrentRoom(Vec(0, 0));
//  ShowAllRooms();
end;

procedure TFloorMap.Draw2DUI();
begin
  FUI.Draw2DUI();
end;

constructor TFloorMap.Create(AParent: TavObject);

  procedure ReadDirectory(const ADir: UnicodeString);
  var sr: TUnicodeSearchRec;
      filter: UnicodeString;
  begin
    filter := ADir+'\*.room';
    if FindFirst(filter, faAnyFile, sr) = 0 then
    begin
      repeat
        FAllRoomFiles.Add(sr.name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

begin
  inherited Create(AParent);
  FVisitedRooms := TVisitedRooms.Create;
  FAllRoomFiles := TStringList.Create;
  ReadDirectory(RoomsDirectory);
end;

destructor TFloorMap.Destroy;
begin
  FreeAndNil(FAllRoomFiles);
  inherited Destroy;
end;

end.

