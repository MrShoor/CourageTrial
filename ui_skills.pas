unit ui_skills;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, untLevel, mutils, avTypes, ui_scroll, intfUtils;

const
  cCellSize   = 48;
  cCellBorderSize = 5;

type
  ISkillsList = interface
    function StateID: Integer;
    function  PopSkill(AIndex: Integer): IUnitSkill;
    procedure PushSkill(AIndex: Integer; const ASkill: IUnitSkill);
    function Count: Integer;
    function GetSkill(AIndex: Integer): IUnitSkill;
  end;

  { TRoomUnitArapter }

  TRoomUnitArapter = class(TInterfacedObject)
  private
    FUnit: IWeakRef;// TRoomUnit;
  protected
    function RoomUnit: TRoomUnit;
  public
    constructor Create(AUnit: TRoomUnit);
  end;

  { TAllSkillListAdapter }

  TAllSkillListAdapter = class(TRoomUnitArapter, ISkillsList)
  private
    function StateID: Integer;
    function  PopSkill(AIndex: Integer): IUnitSkill;
    procedure PushSkill(AIndex: Integer; const ASkill: IUnitSkill);
    function Count: Integer;
    function GetSkill(AIndex: Integer): IUnitSkill;
  end;

  { TSkillSlots10Adapter }

  TSkillSlots10Adapter = class(TRoomUnitArapter, ISkillsList)
  private
    FStateID: Integer;
  private
    function StateID: Integer;
    function  PopSkill(AIndex: Integer): IUnitSkill;
    procedure PushSkill(AIndex: Integer; const ASkill: IUnitSkill);
    function Count: Integer;
    function GetSkill(AIndex: Integer): IUnitSkill;
  end;

  { TavmSkills }

  TavmSkills = class(TavmCustomControl)
  private
    FDraggedItem: Integer;

    FDraggetItemCoord: TVec2;

    FGridHeight: Integer;
    FGridWidth: Integer;
    FScroll: TavmDefaultScroll;
    FSkills: ISkillsList;
    procedure SetGridHeight(AValue: Integer);
    procedure SetGridWidth(AValue: Integer);
    procedure SetSkills(const AValue: ISkillsList);
    function  GetVScroll: Boolean;
    procedure SetVScroll(AValue: Boolean);
  private
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
  private
    FDropPosition: Integer;
    FDropTarget: TavmSkills;
    procedure SetDropPosition(const AValue: Integer);
    function  FindDropTarget(const APt: TVec2): TavmSkills;
    procedure SetDropTarget(const AValue: TavmSkills);
    property  DropTarget: TavmSkills read FDropTarget write SetDropTarget;
  public
    property GridWidth : Integer read FGridWidth write SetGridWidth;
    property GridHeight: Integer read FGridHeight write SetGridHeight;
    property Skills : ISkillsList read FSkills write SetSkills;
    property VScroll: Boolean read GetVScroll write SetVScroll;
  end;

implementation

{ TRoomUnitArapter }

function TRoomUnitArapter.RoomUnit: TRoomUnit;
begin
  Result := nil;
  if FUnit = nil then Exit;
  Result := TRoomUnit(FUnit.Obj);
  if Result = nil then FUnit := nil;
end;

constructor TRoomUnitArapter.Create(AUnit: TRoomUnit);
begin
  if AUnit = nil then
    FUnit := nil
  else
    FUnit := AUnit.WeakRef;
end;

{ TSkillSlots10Adapter }

function TSkillSlots10Adapter.StateID: Integer;
begin
  Result := FStateID;
end;

function TSkillSlots10Adapter.PopSkill(AIndex: Integer): IUnitSkill;
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt = nil then Exit(nil);
  Result := unt.SkillSlots[AIndex];
  unt.SkillSlots[AIndex] := nil;
  Inc(FStateID);
end;

procedure TSkillSlots10Adapter.PushSkill(AIndex: Integer; const ASkill: IUnitSkill);
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt = nil then Exit;
  unt.SkillSlots[AIndex] := ASkill;
  Inc(FStateID);
end;

function TSkillSlots10Adapter.Count: Integer;
begin
  Result := 10;
end;

function TSkillSlots10Adapter.GetSkill(AIndex: Integer): IUnitSkill;
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt = nil then Exit(nil);
  Result := unt.SkillSlots[AIndex];
end;

{ TAllSkillListAdapter }

function TAllSkillListAdapter.StateID: Integer;
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt = nil then Exit(0);
  Result := unt.Inventory.StateID;
end;

function TAllSkillListAdapter.PopSkill(AIndex: Integer): IUnitSkill;
begin
  Result := GetSkill(AIndex);
end;

procedure TAllSkillListAdapter.PushSkill(AIndex: Integer; const ASkill: IUnitSkill);
begin

end;

function TAllSkillListAdapter.Count: Integer;
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt <> nil then
    Result := unt.AllSkills().Count
  else
    Result := 0;
end;

function TAllSkillListAdapter.GetSkill(AIndex: Integer): IUnitSkill;
var unt: TRoomUnit;
begin
  unt := RoomUnit;
  if unt <> nil then
    Result := unt.AllSkills()[AIndex]
  else
    Result := nil;
end;

{ TavmSkills }

procedure TavmSkills.SetGridHeight(AValue: Integer);
begin
  if FGridHeight=AValue then Exit;
  FGridHeight:=AValue;
  UpdateSize;
  UpdateScroll;
  Invalidate;
end;

procedure TavmSkills.SetGridWidth(AValue: Integer);
begin
  if FGridWidth=AValue then Exit;
  FGridWidth:=AValue;
  UpdateSize;
  UpdateScroll;
  Invalidate;
end;

procedure TavmSkills.SetSkills(const AValue: ISkillsList);
begin
  if FSkills = AValue then Exit;
  FSkills := AValue;
  UpdateScroll;
  Invalidate;
end;

function TavmSkills.GetVScroll: Boolean;
begin
  Result := FScroll <> nil;
end;

procedure TavmSkills.SetVScroll(AValue: Boolean);
begin
  if GetVScroll = AValue then Exit;

  if AValue then
  begin
    FScroll := TavmDefaultScroll.Create(Self);
    FScroll.OnScroll := {$IfDef FPC}@{$EndIf}ScrollEvent;
  end
  else
    FreeAndNil(FScroll);

  UpdateSize;
  UpdateScroll;
  Invalidate;
end;

procedure TavmSkills.UpdateSize;
var s: TVec2;
begin
  s.x := GridWidth*cCellSize  + (GridWidth +1)*cCellBorderSize;
  s.y := GridHeight*cCellSize + (GridHeight+1)*cCellBorderSize;

  if FScroll <> nil then
  begin
    s.x := s.x + (cScrollBarWidth + cCellBorderSize);
    FScroll.Size := Vec(cScrollBarWidth, s.y - 2*cCellBorderSize);
    FScroll.Pos := Vec(s.x - cCellBorderSize - cScrollBarWidth, cCellBorderSize);
  end;

  Size := s;
end;

procedure TavmSkills.UpdateScroll;
begin
  if FScroll = nil then Exit;

  if FSkills = nil then
  begin
    FScroll.Range := 1;
    FScroll.ViewportPos := 0;
    FScroll.ViewportWidth := 1;
    Exit;
  end;
  FScroll.Range := (FSkills.Count + GridWidth - 1) div GridWidth;
  FScroll.ViewportWidth := GridHeight;
end;

procedure TavmSkills.ScrollEvent(ASender: TObject);
begin
  Invalidate;
end;

function TavmSkills.ItemRect(CellX, CellY: Integer): TRectF;
begin
  Result.min.x := cCellBorderSize + (cCellSize + cCellBorderSize) * CellX;
  Result.min.y := cCellBorderSize + (cCellSize + cCellBorderSize) * CellY;
  Result.max := Result.min + Vec(cCellSize, cCellSize);
end;

function TavmSkills.ItemRect(AIndex: Integer): TRectF;
var x, y: Integer;
begin
  x := AIndex mod GridWidth;
  y := AIndex div GridWidth;
  if FScroll <> nil then
    y := y + FScroll.ViewportPos;
  Result := ItemRect(x, y);
end;

function TavmSkills.HitToItems(const APt: TVec2): Integer;
var rct: TRectF;
    i: Integer;
begin
  Result := -1;
  if FSkills = nil then Exit;

  for i := 0 to FSkills.Count - 1 do
  begin
    rct := ItemRect(i);
    if rct.PtInRect(APt) then Exit(i);
  end;
end;

procedure TavmSkills.Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStart(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  FDraggedItem := HitToItems(APt);
  BringToFront;
end;

procedure TavmSkills.Notify_DragMove(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragMove(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  if (FDraggedItem < 0) then Exit;
  FDraggetItemCoord := APt;

  DropTarget := FindDropTarget(APt);
  if DropTarget <> nil then
    DropTarget.SetDropPosition(DropTarget.HitToItems((APt * AbsTransform) * DropTarget.AbsTransformInv)); //to do fix for non siblings objects

  Invalidate;
end;

procedure TavmSkills.Notify_DragStop(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_DragStop(ABtn, APt, AShifts);
  if (ABtn <> 1) then Exit;
  if (FDraggedItem < 0) then Exit;

  if (DropTarget = nil) then
    Skills.PopSkill(FDraggedItem);
  if (DropTarget <> nil) and (DropTarget.FDropPosition >= 0) and (Skills.GetSkill(FDraggedItem) <> nil) then
    DropTarget.Skills.PushSkill(DropTarget.FDropPosition, Skills.PopSkill(FDraggedItem));
  FDraggedItem := -1;
  DropTarget := nil;
  Invalidate;
end;

procedure TavmSkills.DoValidate;

  function GetCellSpriteFile(AIndex: Integer): string;
  begin
    if FSkills = nil then Exit('');
    if AIndex < 0 then Exit('');
    if AIndex >= FSkills.Count then Exit('');
    if FSkills.GetSkill(AIndex) = nil then Exit('');
    Result := ExeRelativeFileName('ui\skills\'+FSkills.GetSkill(AIndex).Ico);
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
  itemRct: TRectF;
begin
  Canvas.Clear;

  Canvas.Brush.Color := Vec(1, 1, 1, 0.125);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);

  if FDropPosition >= 0 then
  begin
    Canvas.Pen.Color := Vec(1,0,0,1);
    Canvas.Pen.Width := 3;
    itemRct := ItemRect(FDropPosition);
    Canvas.AddRectangle(itemRct.min, itemRct.max);
  end;

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;

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

  if (FDraggedItem <> -1) and (FSkills.GetSkill(FDraggedItem) <> nil) then
  begin
    itemSprite := GetCellSpriteFile(FDraggedItem);
    cellPos := FDraggetItemCoord - Vec(cCellSize*0.5, cCellSize*0.5);
    DrawItemSprite(cellPos, itemSprite);
  end;

end;

procedure TavmSkills.AfterRegister;
begin
  inherited AfterRegister;
  FDropPosition := -1;
  FDraggedItem := -1;

  Origin := Vec(1,0);
  Pos := Vec(10, 10);
  GridWidth := 5;
  GridHeight := 10;
end;

procedure TavmSkills.SetDropPosition(const AValue: Integer);
begin
  if FDropPosition = AValue then Exit;
  FDropPosition := AValue;
  Invalidate;
end;

function TavmSkills.FindDropTarget(const APt: TVec2): TavmSkills;
var rootPt: TVec2;
    rCtrl: TavmBaseControl;
    hit: TavmBaseControl;
begin
  rCtrl := RootControl;
  rootPt := Space_LocalToRootControl(APt);
  hit := rCtrl.HitTest(rootPt, True);
  if hit is TavmSkills then
    Result := TavmSkills(hit)
  else
    Result := nil;
end;

procedure TavmSkills.SetDropTarget(const AValue: TavmSkills);
begin
  if FDropTarget = AValue then Exit;
  if FDropTarget <> nil then
    FDropTarget.FDropPosition := -1;
  FDropTarget := AValue;
end;

end.

