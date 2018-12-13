unit generator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avContnrs, untLevel;

type
  IVisitedRooms = {$IfDef FPC}specialize{$EndIf}IHashSet<string>;
  TVisitedRooms = {$IfDef FPC}specialize{$EndIf}THashSet<string>;

function GenRoom(const AVisitedRooms: IVisitedRooms; const AAllRooms: TStrings): string;

function GenAltarLoot(const AForUnit: TRoomUnit): IUnitItem;
function GenChestLoot(const AForUnit: TRoomUnit): IUnitItemArr;

implementation

uses
  mutils, Math, untItems;

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

function GenAltarLoot(const AForUnit: TRoomUnit): IUnitItem;
type
  TGenAltarItems = (aiBottle50, aiScrollResonantArmor, aiSocks, aiPoison, aiBow);
const
  cBasicWeights: array [TGenAltarItems] of Integer = (2000, 1000, 2000, 200, 1000);
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
  else
    Result := nil;
  end;
end;

function GenChestLoot(const AForUnit: TRoomUnit): IUnitItemArr;
type
  TGenChestItems = (ciBottle30, ciBottle50, ciScrollResonantArmor, ciSocks, ciPoison);
const
  cBasicWeights: array [TGenChestItems] of Integer = (5000, 1000, 500, 1000, 200);
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
    else
      newItem := nil;
    end;
    if newItem <> nil then
      Result.Add(newItem);
  end;
end;

end.

