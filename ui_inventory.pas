unit ui_inventory;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, untLevel, mutils, avTypes, ui_scroll;

const
  cCellSize   = 48;
  cCellBorderSize = 5;
  cScrollBarWidth = 24;

type

  { TavmItemHint }

  TavmItemHint = class(TavmCustomControl)
  private
    FItem: IUnitItem;

    FNameText  : ITextLines;
    FStatsText : ITextLines;
    //FReqText   : ITextLines;
    procedure BuildTextLines;
    procedure SetItem(const AValue: IUnitItem);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); override;
  public
    property Item: IUnitItem read FItem write SetItem;
  end;

  { TavmInventory }

  TavmInventory = class(TavmCustomControl)
  private
    FHint  : TavmItemHint;
    FLastMoveTime: Int64;
    FLastMoveCoords: TVec2;
    FScroll: TavmDefaultScroll;

    FGridHeight: Integer;
    FGridWidth: Integer;

    FInventory: IInventory;
    FLastStateID: Integer;

    FDraggedItem: Integer;
    FDraggetItemCoord: TVec2;
  private
    procedure SetGridHeight(const AValue: Integer);
    procedure SetGridWidth(const AValue: Integer);
    procedure SetInventory(const AValue: IInventory);
    procedure UpdateSize;
    procedure ScrollEvent(ASender: TObject);
  private
    function ItemRect(CellX, CellY: Integer): TRectF; overload;
    function ItemRect(AIndex: Integer): TRectF; overload;
    function HitToItems(const APt: TVec2): Integer;
    function ItemAt(const APt: TVec2): IUnitItem;
  protected
    procedure Notify_MouseEnter; override;
    procedure Notify_MouseLeave; override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragMove (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragStop (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); override;
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
    procedure DrawControl(const AMat: TMat3); override;
    procedure OnUPS; override;
  public
    property GridWidth : Integer read FGridWidth write SetGridWidth;
    property GridHeight: Integer read FGridHeight write SetGridHeight;
    property Inventory: IInventory read FInventory write SetInventory;
  private
    FDropPosition: Integer;
    FDropTarget: TavmInventory;
    procedure SetDropPosition(const AValue: Integer);
    function  FindDropTarget(const APt: TVec2): TavmInventory;
    procedure SetDropTarget(const AValue: TavmInventory);
    property  DropTarget: TavmInventory read FDropTarget write SetDropTarget;
  public
    procedure UpdateScroll;
    procedure DropItem(const AFrom: IInventory; const AIndex: Integer);
    procedure SetDropPoint(const APt: TVec2);
    property DropPosition: Integer read FDropPosition write SetDropPosition;
  end;

implementation

{ TavmItemHint }

procedure TavmItemHint.BuildTextLines;

  function GetDamageStr(): string;
  var dmg: TVec2i;
  begin
    dmg := FItem.Weapon_Damage;
    if dmg.x = dmg.y then
      Result := IntToStr(dmg.x)
    else
      Result := IntToStr(dmg.x) + '-' + IntToStr(dmg.y);
  end;

const cTextYSpace = 10;
      cTextXSpace = 30;
var
  tb: ITextBuilder;
  y : Single;
begin
  if FItem = nil then
  begin
    FNameText := nil;
    FStatsText := nil;
    Exit;
  end;

  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size  := 32;
  Canvas.Font.Style := [gsBold];
  tb := Canvas.TextBuilder;
  tb.Align := laCenter;
  tb.WriteWrapped(FItem.Name);
  tb.WriteWrappedEnd(Size.x - cTextXSpace*2, True);

  FNameText := tb.Finish();
  FNameText.BoundsX := Vec(cTextXSpace, Size.x - cTextXSpace);

  Canvas.Font.Size := 24;
  Canvas.Font.Style := [];
  tb := Canvas.TextBuilder;
  tb.Align := laLeft;
  if FItem.Kind in cUnitItemKind_Weapons then
    tb.WriteLn(string('Урон: ') + GetDamageStr());
  if FItem.Kind = ikConsumable then
    tb.WriteLn(string('Потребляемое'));
  if FItem.ExtraDesc <> '' then
  begin
    tb.WriteWrapped(FItem.ExtraDesc);
    tb.WriteWrappedEnd(Size.x - cTextXSpace*2, True);
  end;

  FStatsText := tb.Finish();
  FStatsText.BoundsX := Vec(cTextXSpace, Size.x - cTextXSpace);

  y := cTextYSpace;
  FNameText.BoundsY := Vec(y, y + FNameText.TotalHeight());
  y := y + FNameText.TotalHeight() + cTextYSpace;
  FStatsText.BoundsY := Vec(y, y + FStatsText.TotalHeight());
  y := y + FStatsText.TotalHeight() + cTextYSpace;

  Size := Vec(Size.x, y);
end;

procedure TavmItemHint.SetItem(const AValue: IUnitItem);
begin
  if FItem = AValue then Exit;
  FItem := AValue;
  BuildTextLines;
  Invalidate;
end;

procedure TavmItemHint.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(340, 100);
end;

procedure TavmItemHint.DoValidate;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Brush.Color := Vec(0.125, 0.125, 0.125, 1);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);

  Canvas.AddText(FNameText);
  Canvas.AddText(FStatsText);
  //Canvas.AddText(FReqText);
end;

procedure TavmItemHint.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin
  inherited HitTestLocal(ALocalPt, AControl);
end;

{ TavmInventory }

procedure TavmInventory.SetInventory(const AValue: IInventory);
begin
  if FInventory = AValue then Exit;
  FInventory := AValue;
  if FInventory <> nil then
    FLastStateID := FInventory.StateID;
  UpdateScroll;
  Invalidate;
end;

procedure TavmInventory.UpdateSize;
var s: TVec2;
begin
  s.x := GridWidth*cCellSize  + (GridWidth +1)*cCellBorderSize + (cScrollBarWidth + cCellBorderSize);
  s.y := GridHeight*cCellSize + (GridHeight+1)*cCellBorderSize;

  Size := s;

  FScroll.Size := Vec(cScrollBarWidth, Size.y - 2*cCellBorderSize);
  FScroll.Pos := Vec(Size.x - cCellBorderSize - cScrollBarWidth, cCellBorderSize);
end;

procedure TavmInventory.UpdateScroll;
begin
  if FInventory = nil then
  begin
    FScroll.Range := 1;
    FScroll.ViewportPos := 0;
    FScroll.ViewportWidth := 1;
    Exit;
  end;
  FScroll.Range := (FInventory.Items.Count + GridWidth - 1) div GridWidth;
  FScroll.ViewportWidth := GridHeight;
  Invalidate;
end;

procedure TavmInventory.ScrollEvent(ASender: TObject);
begin
  Invalidate;
end;

function TavmInventory.ItemRect(CellX, CellY: Integer): TRectF;
begin
  Result.min.x := cCellBorderSize + (cCellSize + cCellBorderSize) * CellX;
  Result.min.y := cCellBorderSize + (cCellSize + cCellBorderSize) * CellY;
  Result.max := Result.min + Vec(cCellSize, cCellSize);
end;

function TavmInventory.ItemRect(AIndex: Integer): TRectF;
var x, y: Integer;
begin
  x := AIndex mod GridWidth;
  y := AIndex div GridWidth - FScroll.ViewportPos;
  Result := ItemRect(x, y);
end;

function TavmInventory.HitToItems(const APt: TVec2): Integer;
var rct: TRectF;
    i: Integer;
begin
  Result := -1;
  if FInventory = nil then Exit;

  for i := 0 to FInventory.Items.Count - 1 do
  begin
    rct := ItemRect(i);
    if rct.PtInRect(APt) then Exit(i);
  end;
end;

function TavmInventory.ItemAt(const APt: TVec2): IUnitItem;
var idx: Integer;
begin
  Result := nil;
  if FInventory = nil then Exit;
  idx := HitToItems(APt);
  if idx < 0 then Exit;
  if idx >= FInventory.Items.Count then Exit;
  Result := FInventory.Items[idx];
end;

procedure TavmInventory.Notify_MouseEnter;
begin
  inherited Notify_MouseEnter;
  UPSSubscribe;
end;

procedure TavmInventory.Notify_MouseLeave;
begin
  inherited Notify_MouseLeave;
  UPSUnSubscribe;
  FHint.Visible := False;
end;

procedure TavmInventory.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
const cHintDirection: TVec2 = (x: 0.7; y: 0.7);
var
  rct: TRectF;
begin
  inherited Notify_MouseMove(APt, AShifts);
  //update hint state
  if FLastMoveCoords = APt then Exit;
  FLastMoveCoords := APt;
  FLastMoveTime := Main.Time64;
  FHint.Item := ItemAt(APt);
  if FHint.Item <> nil then
  begin
    FHint.Origin := Sign(cHintDirection) * Vec(-0.5, -0.5) + Vec(0.5, 0.5);
    rct := ItemRect(HitToItems(APt));
    FHint.Pos := Lerp(rct.min, rct.max, cHintDirection * Vec(0.5, 0.5) + Vec(0.5, 0.5));
  end;
end;

procedure TavmInventory.Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStart(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  FDraggedItem := HitToItems(APt);
  BringToFront;
end;

procedure TavmInventory.Notify_DragMove(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragMove(ABtn, APt, AShifts);
  //update drag state
  if (ABtn <> 1) then Exit;
  if (FDraggedItem < 0) then Exit;
  FDraggetItemCoord := APt;

  DropTarget := FindDropTarget(APt);
  if DropTarget <> nil then
    DropTarget.SetDropPoint((APt * Transform) * DropTarget.TransformInv); //to do fix for non siblings objects

  Invalidate;
end;

procedure TavmInventory.Notify_DragStop(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStop(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  if (FDraggedItem < 0) then Exit;
  if DropTarget <> nil then
    DropTarget.DropItem(Inventory, FDraggedItem);
  FDraggedItem := -1;
  DropTarget := nil;
  UpdateScroll;
  Invalidate;
end;

procedure TavmInventory.Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
var itemIdx: Integer;
    item: IUnitItem;
    action: IBRA_Action;
    unt: TRoomUnit;
begin
  inherited Notify_MouseDblClick(ABtn, APt, AShifts);
  if FInventory = nil then Exit;
  if not (FInventory.Owner is TPlayer) then Exit;
  itemIdx := HitToItems(APt);
  if itemIdx < 0 then Exit;
  if itemIdx >= FInventory.Items.Count then Exit;
  item := FInventory.Items[itemIdx];
  case item.Kind of
    ikUnknown: Exit;
    ikConsumable:
      begin
        unt := FInventory.Owner as TRoomUnit;
        if not unt.IsDead() and not unt.Room.InAction then
        begin
          action := item.Consume(unt);
          if action <> nil then
            FInventory.Owner.Room.AddAction(action);
        end;
      end;
    ikBow, ikAxe:
      begin
        if item.Equipped then
          TPlayer(FInventory.Owner).Unequip(item.Slot)
        else
          TPlayer(FInventory.Owner).Equip(item);
      end;
  end;
  Invalidate;
end;

procedure TavmInventory.Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
  inherited Notify_MouseWheel(APt, AWheelShift, AShifts);
  FScroll.Scroll(-AWheelShift);
end;

procedure TavmInventory.SetGridWidth(const AValue: Integer);
begin
  if FGridWidth = AValue then Exit;
  FGridWidth := AValue;
  UpdateSize;
  UpdateScroll;
  Invalidate;
end;

function TavmInventory.FindDropTarget(const APt: TVec2): TavmInventory;
var rootPt: TVec2;
    rCtrl: TavmBaseControl;
    hit: TavmBaseControl;
begin
  rCtrl := RootControl;
  rootPt := Space_LocalToRootControl(APt);
  hit := rCtrl.HitTest(rootPt, True);
  if hit is TavmInventory then
    Result := TavmInventory(hit)
  else
    Result := nil;
end;

procedure TavmInventory.SetDropTarget(const AValue: TavmInventory);
begin
  if FDropTarget = AValue then Exit;
  if FDropTarget <> nil then
    FDropTarget.DropPosition := -1;
  FDropTarget := AValue;
end;

procedure TavmInventory.SetGridHeight(const AValue: Integer);
begin
  if FGridHeight = AValue then Exit;
  FGridHeight := AValue;
  UpdateSize;
  UpdateScroll;
  Invalidate;
end;

procedure TavmInventory.DoValidate;

  function GetCellSpriteInfo(AIndex: Integer; out AEquipped: Boolean): string; overload;
  var item: IUnitItem;
  begin
    AEquipped := False;
    if FInventory = nil then Exit('');
    if AIndex < 0 then Exit('');
    if AIndex >= FInventory.Items.Count then Exit('');
    item := FInventory.Items[AIndex];
    if item = nil then Exit('');

    Result := ExeRelativeFileName('ui\items\48\'+item.Ico48);
    AEquipped := item.Equipped;
  end;

  function GetCellSpriteInfo(x,y: Integer; out AEquipped: Boolean): string; overload;
  begin
    Result := GetCellSpriteInfo((y + FScroll.ViewportPos) * GridWidth + x, AEquipped);
  end;

  procedure DrawItemSprite(const ACellPos: TVec2; const ASpriteFile: string; AEquipped: Boolean);
  var
    cellRB: TVec2;
  begin
    cellRB := ACellPos + Vec(cCellSize, cCellSize);

    if ASpriteFile <> '' then
    begin
      Canvas.Brush.Color := Vec(0.1, 0.1, 0.1, 1.0);
      Canvas.AddFill(ACellPos, cellRB);
      Canvas.Brush.Color := Vec(1,1,1,1);
      Canvas.AddSprite(ACellPos, cellRB, ASpriteFile);

      if AEquipped then
        Canvas.AddSprite(cellRB - Vec(16,16), cellRB, ExeRelativeFileName('ui\items\equipped.png'));
    end;
  end;

var
  i, j: Integer;
  cellPos: TVec2;
  itemSprite: string;
  itemEquipped: Boolean;
begin
  Canvas.Clear;

  Canvas.Brush.Color := Vec(1, 1, 1, 0.125);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);

  cellPos.y := cCellBorderSize;
  for j := 0 to FGridHeight - 1 do
  begin
    cellPos.x := cCellBorderSize;
    for i := 0 to FGridWidth - 1 do
    begin
      itemSprite := GetCellSpriteInfo(i, j, itemEquipped);
      DrawItemSprite(cellPos, itemSprite, itemEquipped);

      cellPos.x := cellPos.x + cCellSize + cCellBorderSize;
    end;
    cellPos.y := cellPos.y + cCellSize + cCellBorderSize;
  end;

  cellPos.y := cCellBorderSize;
  for j := 0 to FGridHeight - 1 do
  begin
    cellPos.x := cCellBorderSize;
    for i := 0 to FGridWidth - 1 do
    begin
      Canvas.AddRectangle(cellPos, cellPos + Vec(cCellSize, cCellSize));
      cellPos.x := cellPos.x + cCellSize + cCellBorderSize;
    end;
    cellPos.y := cellPos.y + cCellSize + cCellBorderSize;
  end;

  if FDraggedItem <> -1 then
  begin
    itemSprite := GetCellSpriteInfo(FDraggedItem, itemEquipped);
    cellPos := FDraggetItemCoord - Vec(cCellSize*0.5, cCellSize*0.5);
    DrawItemSprite(cellPos, itemSprite, itemEquipped);
  end;

  if FDropPosition >= 0 then
  begin
    i := FDropPosition mod GridWidth;
    j := FDropPosition div GridHeight - FScroll.ViewportPos;
    cellPos.x := cCellBorderSize + (cCellSize + cCellBorderSize) * i - cCellBorderSize*0.5;
    cellPos.y := cCellBorderSize + (cCellSize + cCellBorderSize) * j - cCellBorderSize*0.5;
    Canvas.Pen.Width := 3;
    Canvas.Pen.Color := Vec(1,0,0,1);
    Canvas.AddLine(cellPos, Vec(cellPos.x, cellPos.y + cCellSize + cCellBorderSize*0.5));
  end;
end;

procedure TavmInventory.AfterRegister;
begin
  inherited AfterRegister;
  FDropPosition := -1;
  FDraggedItem := -1;

  FScroll := TavmDefaultScroll.Create(Self);
  FScroll.OnScroll := {$IfDef FPC}@{$EndIf}ScrollEvent;

  Origin := Vec(0,0);
  Pos := Vec(10, 10);
  GridWidth := 7;
  GridHeight := 5;

  FHint := TavmItemHint.Create(Self);
  FHint.Pos := Vec(0,0);
  FHint.Origin := Vec(1, 0);
  FHint.Visible := False;
end;

procedure TavmInventory.DrawControl(const AMat: TMat3);
begin
  if FInventory <> nil then
    if FLastStateID <> FInventory.StateID then
    begin
      FLastStateID := FInventory.StateID;
      Invalidate;
    end;
  inherited DrawControl(AMat);
end;

procedure TavmInventory.OnUPS;
begin
  inherited OnUPS;
  FHint.Visible := (Main.Time64 - FLastMoveTime > 100) and
                   (not FDragStarted[0]) and
                   (FHint.Item <> nil);
end;

procedure TavmInventory.SetDropPosition(const AValue: Integer);
begin
  if FDropPosition = AValue then Exit;
  FDropPosition := AValue;
  Invalidate;
end;

procedure TavmInventory.DropItem(const AFrom: IInventory; const AIndex: Integer);
var item: IUnitItem;
begin
  if FDropPosition < 0 then Exit;
  item := AFrom.Pop(AIndex);

  if (AFrom = FInventory) and (FDropPosition > AIndex) then
    Dec(FDropPosition);

  FInventory.Push(item, FDropPosition);
  UpdateScroll;
end;

procedure TavmInventory.SetDropPoint(const APt: TVec2);
var minDist, Dist: Single;
    item, i: Integer;
    rct: TRectF;
begin
  if FInventory = nil then Exit;
  item := -1;
  minDist := HUGE;
  for i := 0 to FInventory.Items.Count do
  begin
    rct := ItemRect(i);
    dist := LenSqr(Vec(rct.min.x - cCellBorderSize*0.5, (rct.min.y+rct.max.y)*0.5) - APt);
    if dist < minDist then
    begin
      item := i;
      minDist := dist;
    end;
  end;

  if item >= 0 then
    DropPosition := item;
end;

end.

