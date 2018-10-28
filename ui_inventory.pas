unit ui_inventory;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, untLevel, mutils, avTypes;

const
  cCellSize   = 48;
  cCellBorderSize = 5;
  cScrollBarWidth = 24;

type
  { TavmInventoryScroll }

  TavmInventoryScroll = class(TavmCustomScrollBar)
  private
  protected
    procedure DoValidate; override;
  public
  end;

  { TavmInventory }

  TavmInventory = class(TavmCustomControl)
  private
    FScroll: TavmInventoryScroll;

    FGridHeight: Integer;
    FGridWidth: Integer;
    FInventory: IUnitItemArr;

    FDraggedItem: Integer;
    FDraggetItemCoord: TVec2;

    procedure SetGridHeight(const AValue: Integer);
    procedure SetGridWidth(const AValue: Integer);
    procedure SetInventory(const AValue: IUnitItemArr);
    procedure UpdateSize;
    procedure UpdateScroll;
    procedure ScrollEvent(ASender: TObject);
  private
    function ItemRect(CellX, CellY: Integer): TRectF;
    function ItemRect(AIndex: Integer): TRectF;
    function HitToItems(const APt: TVec2): Integer;
  protected
    procedure Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragMove (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragStop (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
  public
    property GridWidth : Integer read FGridWidth write SetGridWidth;
    property GridHeight: Integer read FGridHeight write SetGridHeight;
    property Inventory: IUnitItemArr read FInventory write SetInventory;
  end;

implementation

{ TavmInventoryScroll }

procedure TavmInventoryScroll.DoValidate;
var barRct: TRectF;
begin
  Canvas.Clear;
  barRct := BarRect();
  Canvas.Brush.Color := Vec(0, 0.5, 0.5, 1.0);
  Canvas.AddFill(barRct.min, barRct.max);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);
end;

{ TavmInventory }

procedure TavmInventory.SetInventory(const AValue: IUnitItemArr);
begin
  if FInventory = AValue then Exit;
  FInventory := AValue;
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
  FScroll.Range := (FInventory.Count + GridWidth - 1) div GridWidth;
  FScroll.ViewportWidth := GridHeight;
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
  y := AIndex div GridWidth + FScroll.ViewportPos;
  Result := ItemRect(x, y);
end;

function TavmInventory.HitToItems(const APt: TVec2): Integer;
var rct: TRectF;
    i: Integer;
begin
  Result := -1;
  if FInventory = nil then Exit;

  for i := 0 to FInventory.Count - 1 do
  begin
    rct := ItemRect(i);
    if rct.PtInRect(APt) then Exit(i);
  end;
end;

procedure TavmInventory.Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStart(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  FDraggedItem := HitToItems(APt);
end;

procedure TavmInventory.Notify_DragMove(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragMove(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  FDraggetItemCoord := APt;
  Invalidate;
end;

procedure TavmInventory.Notify_DragStop(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStop(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  FDraggedItem := -1;
  Invalidate;
end;

procedure TavmInventory.SetGridWidth(const AValue: Integer);
begin
  if FGridWidth = AValue then Exit;
  FGridWidth := AValue;
  UpdateSize;
  UpdateScroll;
  Invalidate;
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

  function GetCellSpriteFile(AIndex: Integer): string;
  begin
    if FInventory = nil then Exit('');
    if AIndex < 0 then Exit('');
    if AIndex >= FInventory.Count then Exit('');
    Result := ExeRelativeFileName('ui\items\48\'+FInventory[AIndex].Ico48);
  end;

  function GetCellSpriteFile(x,y: Integer): string;
  begin
    Result := GetCellSpriteFile(y * GridWidth + x);
  end;

  procedure DrawItemSprite(const ACellPos: TVec2; const ASpriteFile: string);
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
    end;

    Canvas.AddRectangle(ACellPos, cellRB);
  end;

var
  i, j: Integer;
  cellPos: TVec2;
  itemSprite: string;
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
      itemSprite := GetCellSpriteFile(i, j);
      DrawItemSprite(cellPos, itemSprite);

      cellPos.x := cellPos.x + cCellSize + cCellBorderSize;
    end;
    cellPos.y := cellPos.y + cCellSize + cCellBorderSize;
  end;

  if FDraggedItem <> -1 then
  begin
    itemSprite := GetCellSpriteFile(FDraggedItem);
    cellPos := FDraggetItemCoord - Vec(cCellSize*0.5, cCellSize*0.5);
    DrawItemSprite(cellPos, itemSprite);
  end;
end;

procedure TavmInventory.AfterRegister;
begin
  inherited AfterRegister;
  FDraggedItem := -1;

  FScroll := TavmInventoryScroll.Create(Self);
  FScroll.OnScroll := {$IfDef FPC}@{$EndIf}ScrollEvent;

  Origin := Vec(0,0);
  Pos := Vec(10, 10);
  GridWidth := 5;
  GridHeight := 10;
end;

end.

