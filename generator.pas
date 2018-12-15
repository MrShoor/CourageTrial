unit generator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avContnrs, untLevel;

function GenRoom(const AVisitedRooms: IVisitedRooms; const AAllRooms: TStrings): string;
function GenBots(const AMap: TRoomMap; const AVisitedRooms: IVisitedRooms; const AForPlayer: TRoomUnit): IRoomUnitArr;

function GenAltarLoot(const AForUnit: TRoomUnit): IUnitItem;
function GenChestLoot(const AForUnit: TRoomUnit): IUnitItemArr;
procedure GenStdBotInventory(const ABotInventory: IInventory);

implementation

uses
  mutils, Math, untItems, untEnemies;

type
  IRoomWeight = {$IfDef FPC}specialize{$EndIf} IHashMap<string, Integer>;
  TRoomWeight = {$IfDef FPC}specialize{$EndIf} THashMap<string, Integer>;

function GenRoom(const AVisitedRooms: IVisitedRooms; const AAllRooms: TStrings): string;
var defWeights: IRoomWeight;
    roomWeights: array of Integer;
    w, i: Integer;
begin
  defWeights := TRoomWeight.Create();
  case AVisitedRooms.Count of
    0, 1: begin
      defWeights.AddOrSet('r2.room', 50);
      defWeights.AddOrSet('6_castle.room', 50);
      defWeights.AddOrSet('5_snake.room', 50);
      defWeights.AddOrSet('4_flower.room', 20);
      defWeights.AddOrSet('3_richman.room', 5);
    end;
    2, 3: begin
      defWeights.AddOrSet('r2.room', 5);
      defWeights.AddOrSet('6_castle.room', 5);
      defWeights.AddOrSet('5_snake.room', 5);
      defWeights.AddOrSet('4_flower.room', 30);
      defWeights.AddOrSet('3_richman.room', 50);
    end;
  end;

  SetLength(roomWeights, AAllRooms.Count);
  for i := 0 to AAllRooms.Count - 1 do
  begin
    if not defWeights.TryGetValue(AAllRooms[i], w) then w := 10;
    if AVisitedRooms.Contains(AAllRooms[i]) then w := max(1, w div 10);
    roomWeights[i] := w;
  end;

  Result := AAllRooms[ WeightedRandom(roomWeights) ];
end;

function GenBots(const AMap: TRoomMap; const AVisitedRooms: IVisitedRooms; const AForPlayer: TRoomUnit): IRoomUnitArr;

  function GetSpawnPlace(): TVec2i;
  begin
    repeat
      Result.x := Random(AMap.Radius*2+1) - AMap.Radius;
      Result.y := Random(AMap.Radius*2+1) - AMap.Radius;
      if AMap.IsCellExists(Result) and (not AMap.RoomFloor.IsHole[Result]) and not AMap.IsCellBlocked(Result) then
        Exit(Result);
    until False;
  end;

  function SpawnBot(const ABotClass: TBotClass): TRoomUnit;
  var bot: TBot;
  begin
    bot := ABotClass.Create(AMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    Result := bot;
  end;

var bossRoom: Boolean;
    botsToSpawn: Integer;
    wispToSpawn, i: Integer;
begin
  Result := TRoomUnitArr.Create();

  case AVisitedRooms.Count of
    0: bossRoom := False;
    1: bossRoom := Random(100) < 1;
    2: bossRoom := Random(100) < 3;
    3: bossRoom := Random(100) < 5;
    4: bossRoom := Random(100) < 10;
    5: bossRoom := Random(100) < 15;
  else
    bossRoom := Random(100) < 20;
  end;

  if bossRoom then
  begin
    if AVisitedRooms.Count > 5 then
      botsToSpawn := 2
    else
      botsToSpawn := 1;
    if AVisitedRooms.Count < 3 then
      wispToSpawn := 2
    else
      wispToSpawn := 3;
    for i := 0 to wispToSpawn - 1 do
      Result.Add(SpawnBot(TBotWisp));
    for i := 0 to botsToSpawn - 1 do
      Result.Add(SpawnBot(TBotHunter1));
    Exit;
  end;

  case AVisitedRooms.Count of
    0: botsToSpawn := 2 + Random(2);
    1: botsToSpawn := 3 + Random(2);
    2: botsToSpawn := 3 + Random(3);
    3: botsToSpawn := 4 + Random(4);
    4: botsToSpawn := 5 + Random(4);
    5: botsToSpawn := 6 + Random(5);
  else
    botsToSpawn := 7 + Random(6);
  end;
  wispToSpawn := Clamp(Random(botsToSpawn div 3 + 1) + Random(2), 0, (botsToSpawn+2) div 3);
  if (botsToSpawn > 6) and (wispToSpawn = 0) then wispToSpawn := 1;
  botsToSpawn := botsToSpawn - wispToSpawn;

  for i := 0 to wispToSpawn - 1 do
    Result.Add(SpawnBot(TBotWisp));

  for i := 0 to botsToSpawn - 1 do
  begin
    if Random(2) = 0 then
      Result.Add(SpawnBot(TBotMutant1))
    else
      Result.Add(SpawnBot(TBotArcher1));
  end;
end;

function GenAltarLoot(const AForUnit: TRoomUnit): IUnitItem;
type
  TGenAltarItems = (aiBottle50, aiScrollResonantArmor, aiSocks, aiPoison, aiBow, aiHuntersBow, aiScrollAxe, aiScrollBow);
const
  cBasicWeights: array [TGenAltarItems] of Integer = (2000, 1000, 2000, 200, 1000, 300, 200, 200);
var
  i: Integer;
  weights: array [TGenAltarItems] of Integer;
  inv: IInventory;
  newItemID: TGenAltarItems;
begin
  weights := cBasicWeights;
  if AForUnit <> nil then
  begin
    inv := AForUnit.Inventory();
    if inv <> nil then
      for i := 0 to inv.Items.Count - 1 do
      begin
        if inv.Items[i].ID = TUnitItemID.LuckySocks then
          weights[aiSocks] := weights[aiSocks] div 10;
      end;
  end;
  newItemID := TGenAltarItems(WeightedRandom(weights));
  case newItemID of
    aiBottle50           : Result := THealBottle2.Create;
    aiScrollResonantArmor: Result := TScroll_ResonantArmor.Create;
    aiSocks              : Result := TSocks.Create;
    aiPoison             : Result := TPoisonBottle.Create;
    aiBow                : Result := TArcherBow.Create;
    aiHuntersBow         : Result := THuntersBow.Create;
    aiScrollAxe          : Result := TScroll_Axe_Mastery.Create;
    aiScrollBow          : Result := TScroll_Bow_Mastery.Create;
  else
    Result := nil;
  end;
end;

function GenChestLoot(const AForUnit: TRoomUnit): IUnitItemArr;
type
  TGenChestItems = (ciBottle30, ciBottle50, ciScrollResonantArmor, ciSocks, ciPoison, ciScrollAxe, ciScrollBow);
const
  cBasicWeights: array [TGenChestItems] of Integer = (4000, 1000, 500, 1000, 200, 50, 50);
var luck: Integer;
    itemsCount: Integer;
    weights: array [TGenChestItems] of Integer;
    newItemID: TGenChestItems;
    newItem: IUnitItem;
    i: LongInt;
begin
  luck := AForUnit.Stats.Lucky div 10;
  itemsCount := WeightedRandom([10, 20, 5]);
  if luck > 0 then
    itemsCount := Random(luck + 1) + luck;
  Result := TUnitItemArr.Create();

  weights := cBasicWeights;
  for i := 0 to AForUnit.Inventory().Items.Count - 1 do
  begin
    if AForUnit.Inventory().Items[i].ID = TUnitItemID.LuckySocks then
      weights[ciSocks] := weights[ciSocks] div 10;
  end;

  for i := 0 to itemsCount - 1 do
  begin
    newItemID := TGenChestItems(WeightedRandom(weights));
    case newItemID of
      ciBottle30           : newItem := THealBottle.Create;
      ciBottle50           : newItem := THealBottle2.Create;
      ciScrollResonantArmor: newItem := TScroll_ResonantArmor.Create;
      ciSocks              : newItem := TSocks.Create;
      ciPoison             : newItem := TPoisonBottle.Create;
      ciScrollAxe          : newItem := TScroll_Axe_Mastery.Create;
      ciScrollBow          : newItem := TScroll_Bow_Mastery.Create;
    else
      newItem := nil;
    end;
    if newItem <> nil then
      Result.Add(newItem);
  end;
end;

procedure GenStdBotInventory(const ABotInventory: IInventory);
type
  TGenBotItems = (biBottle30, biBottle50, biScrollResonantArmor, biPoison);
const
  cBasicWeights: array [TGenBotItems] of Integer = (4000, 1000, 500, 300);
var
  i, itemsCount: Integer;
  newItem: IUnitItem;
  newItemID: TGenBotItems;
begin
  itemsCount := WeightedRandom([100, 30, 10]);
  for i := 0 to itemsCount - 1 do
  begin
    newItemID := TGenBotItems(WeightedRandom(cBasicWeights));
    case newItemID of
      biBottle30           : newItem := THealBottle.Create;
      biBottle50           : newItem := THealBottle2.Create;
      biScrollResonantArmor: newItem := TScroll_ResonantArmor.Create;
      biPoison             : newItem := TPoisonBottle.Create;
    end;
    ABotInventory.Push(newItem, ABotInventory.Items.Count - 1);
  end;
end;

end.

