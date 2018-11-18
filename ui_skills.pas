unit ui_skills;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avBase, avRes, avMiniControls, avCanvas, untLevel, mutils, avTypes, ui_scroll, intfUtils;

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

  { TavmSkillHint }

  TavmSkillHint = class(TavmCustomControl)
  private
    FSkill: IUnitSkill;

    FNameText  : ITextLines;
    FStatsText : ITextLines;
    procedure BuildTextLines;
    procedure SetSkill(AValue: IUnitSkill);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); override;
  public
    property Skill: IUnitSkill read FSkill write SetSkill;
  end;

  { TavmSkillHighlighter }

  TavmSkillHighlighter = class(TavmCustomControl)
  private
    FRect: TRectF;
    FSprite: ISpriteIndex;
    function ObtainSprite: ISpriteIndex;
    procedure SetRect(const AValue: TRectF);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure OnUPS; override;
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); override;
  public
    property Rect: TRectF read FRect write SetRect;
  end;

  { TavmSkills }

  TavmSkills = class(TavmCustomControl)
  private
    FSkillHighlighter: TavmSkillHighlighter;
    FSkillHint: TavmSkillHint;

    FDraggedItem: Integer;

    FDraggetItemCoord: TVec2;

    FGridHeight: Integer;
    FGridWidth: Integer;
    FScroll: TavmDefaultScroll;
    FSkills: ISkillsList;
    FLastStateID: Integer;

    FLastMoveTime: Int64;
    FHintDirection: TVec2;

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
    function SkillAt(const APt: TVec2): IUnitSkill;
  protected
    function GetBattleRoom: TBattleRoom;

    procedure Notify_MouseEnter; override;
    procedure Notify_MouseLeave; override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragMove (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_DragStop (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp  (ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
  protected
    procedure DrawControl(const AMat: TMat3); override;
    procedure DoValidate; override;
    procedure AfterRegister; override;
    procedure OnUPS; override;
  private
    FActiveSkill: IUnitSkill;
    FDropPosition: Integer;
    FDropTarget: TavmSkills;
    procedure SetActiveSkill(const AValue: IUnitSkill);
    procedure SetDropPosition(const AValue: Integer);
    function  FindDropTarget(const APt: TVec2): TavmSkills;
    procedure SetDropTarget(const AValue: TavmSkills);
    property  DropTarget: TavmSkills read FDropTarget write SetDropTarget;
  public
    property GridWidth : Integer read FGridWidth write SetGridWidth;
    property GridHeight: Integer read FGridHeight write SetGridHeight;
    property Skills : ISkillsList read FSkills write SetSkills;
    property ActiveSkill: IUnitSkill read FActiveSkill write SetActiveSkill;
    property VScroll: Boolean read GetVScroll write SetVScroll;

    property HintDirection: TVec2 read FHintDirection write FHintDirection; // in range [-1; 1]
  end;

implementation

uses
  Math;

{ TavmSkillHint }

procedure TavmSkillHint.BuildTextLines;
const cTextYSpace = 10;
      cTextXSpace = 30;
var tb: ITextBuilder;
    s : TVec2;
begin
  if FSkill = nil then
  begin
    FNameText  := nil;
    FStatsText := nil;
    Exit;
  end;

  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size  := 32;
  Canvas.Font.Style := [gsBold];
  tb := Canvas.TextBuilder;
  tb.Align := laCenter;
  tb.WriteLn(FSkill.Name);

  Canvas.Font.Size := 24;
  Canvas.Font.Style := [];
  tb.Align := laLeft;
  tb.WriteLn(FSkill.Desc);

  FNameText := tb.Finish();
  FNameText.BoundsX := Vec(cTextXSpace, FNameText.MaxLineWidth());

  tb := Canvas.TextBuilder;
  tb.Align := laLeft;
  tb.WriteLn('Стоимость: ' + IntToStr(FSkill.Cost));
  tb.WriteLn('Дистанция: ' + FormatFloat('0.0', FSkill.Range));
  FStatsText := tb.Finish();
  FStatsText.BoundsX := Vec(cTextXSpace, FStatsText.MaxLineWidth());

  s.y := cTextYSpace;
  s.y := s.y + FNameText.TotalHeight() + cTextYSpace;
  FStatsText.BoundsY := Vec(s.y, s.y + FStatsText.TotalHeight());
  s.y := s.y + FStatsText.TotalHeight() + cTextYSpace;

  s.x := FNameText.MaxLineWidth();
  s.x := Max(s.x, FStatsText.MaxLineWidth());
  s.x := s.x + cTextXSpace*2;

  Size := s;
end;

procedure TavmSkillHint.SetSkill(AValue: IUnitSkill);
begin
  if FSkill=AValue then Exit;
  FSkill:=AValue;
  BuildTextLines;
end;

procedure TavmSkillHint.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(200, 100);
end;

procedure TavmSkillHint.DoValidate;
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
end;

procedure TavmSkillHint.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin
  AControl := nil;
end;

{ TavmSkillHighlighter }

function TavmSkillHighlighter.ObtainSprite: ISpriteIndex;
begin
  if FSprite = nil then
    FSprite := Canvas.GetSprite(ExeRelativeFileName('ui\skills\particle1.png'));
  Result := FSprite;
end;

procedure TavmSkillHighlighter.SetRect(const AValue: TRectF);
begin
  if FRect = AValue then Exit;
  FRect := AValue;
end;

procedure TavmSkillHighlighter.AfterRegister;
begin
  inherited AfterRegister;
  UPSSubscribe;
end;

procedure TavmSkillHighlighter.DoValidate;

const cSnakeSpeed = 30.0;
      cSnakePtStep = 4.0;
      cSnakePtCount = 24;
      cSnakeHeadSize = 5;

  procedure AddSnake(AHeadPos, ARectLen: Single);

    procedure GetStartStopPointAt(pos: Single; out ptStart, ptEnd: TVec2; out t: Single);
    begin
      pos := frac(pos / ARectLen) * ARectLen;
      if pos < 0 then pos := pos + ARectLen;
      if pos < Rect.Size.y then
      begin
        ptStart := Rect.min;
        ptEnd := Vec(Rect.min.x, Rect.max.y);
        t := pos / Rect.Size.y;
        Exit;
      end
      else
        pos := pos - Rect.Size.y;

      if pos < Rect.Size.x then
      begin
        ptStart := Vec(Rect.min.x, Rect.max.y);
        ptEnd := Rect.max;
        t := pos / Rect.Size.x;
        Exit;
      end
      else
        pos := pos - Rect.Size.x;

      if pos < Rect.Size.y then
      begin
        ptStart := Rect.max;
        ptEnd := Vec(Rect.max.x, Rect.min.y);
        t := pos / Rect.Size.y;
        Exit;
      end
      else
        pos := pos - Rect.Size.y;

      ptStart := Vec(Rect.max.x, Rect.min.y);
      ptEnd := Rect.min;
      t := clamp(pos / Rect.Size.x, 0, 1);
    end;

    procedure GetPointNormalAt(pos: Single; out pt, norm: TVec2);
    var ptStart: TVec2;
        ptEnd: TVec2;
        t: Single;
    begin
      GetStartStopPointAt(pos, ptStart, ptEnd, t);
      pt := Lerp(ptStart, ptEnd, t);
      norm := Normalize(Rotate90(ptStart - ptEnd, true));
    end;

  var i: Integer;
      pt: TVec2;
      norm: TVec2;
      pos: Single;
      scale: Single;
  begin
    for i := 0 to cSnakePtCount - 1 do
    begin
      scale := (i / cSnakePtCount);
      pos := AHeadPos + i * cSnakePtStep;
      GetPointNormalAt(pos, pt, norm);
      //pt := pt + norm*1*Sin(pos/ARectLen*2*Pi*4*2);
      Canvas.AddSprite(pt - Vec(cSnakeHeadSize, cSnakeHeadSize) * scale, pt + Vec(cSnakeHeadSize, cSnakeHeadSize) * scale, ObtainSprite());
    end;
  end;

var rectLen: Double;
    headPos: Double;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Brush.Color := Vec(4,4,4,1);
  Canvas.Brush.Hinting := [];

  rectLen := (FRect.Size.x + FRect.Size.y) * 2;
  headPos := cSnakeSpeed * Main.Time;
  headPos := frac(headPos/rectLen) * rectLen;
  AddSnake(headPos, rectLen);
  AddSnake(headPos+rectLen*0.5, rectLen);
end;

procedure TavmSkillHighlighter.OnUPS;
begin
  inherited OnUPS;
  Invalidate;
end;

procedure TavmSkillHighlighter.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin
  AControl := nil;
end;

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
  Result := RoomUnit.Inventory().StateID + FStateID;
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
  if FSkills <> nil then
    FLastStateID := FSkills.StateID;
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

function TavmSkills.SkillAt(const APt: TVec2): IUnitSkill;
var idx: Integer;
begin
  Result := nil;
  if FSkills = nil then Exit;
  idx := HitToItems(APt);
  if idx < 0 then Exit;
  if idx >= FSkills.Count then Exit;
  Result := FSkills.GetSkill(idx);
end;

function TavmSkills.GetBattleRoom: TBattleRoom;
var obj: TavObject;
begin
  obj := Self;
  repeat
    obj := obj.Parent;
    if obj is TBattleRoom then Exit(TBattleRoom(obj));
  until obj = nil;
  Result := nil;
end;

procedure TavmSkills.Notify_MouseEnter;
begin
  inherited Notify_MouseEnter;
  UPSSubscribe;
end;

procedure TavmSkills.Notify_MouseLeave;
begin
  inherited Notify_MouseLeave;
  UPSUnSubscribe;
  FSkillHint.Visible := False;
end;

procedure TavmSkills.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var rct: TRectF;
begin
  inherited Notify_MouseMove(APt, AShifts);
  FLastMoveTime := Main.Time64;
  FSkillHint.Skill := SkillAt(APt);
  if FSkillHint.Skill <> nil then
  begin
    FSkillHint.Origin := Sign(FHintDirection) * Vec(-0.5, -0.5) + Vec(0.5, 0.5);
    rct := ItemRect(HitToItems(APt));
    FSkillHint.Pos := Lerp(rct.min, rct.max, FHintDirection * Vec(0.5, 0.5) + Vec(0.5, 0.5));
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

procedure TavmSkills.Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
var itemIdx: Integer;
    bRoom: TBattleRoom;
    skill: IUnitSkill;
begin
  if (ABtn = 1) and (not FDragStarted[ABtn]) then
  begin
    bRoom := GetBattleRoom;
    if bRoom = nil then Exit;
    skill := nil;

    itemIdx := HitToItems(APt);
    if (itemIdx >= 0) and (itemIdx < FSkills.Count) then
      skill := FSkills.GetSkill(itemIdx);

    bRoom.UI_SetPlayerActiveSkill(skill);
  end;

  inherited Notify_MouseUp(ABtn, APt, AShifts);
end;

procedure TavmSkills.DrawControl(const AMat: TMat3);
begin
  if FSkills <> nil then
    if FLastStateID <> FSkills.StateID then
    begin
      Invalidate;
      FLastStateID := FSkills.StateID;
    end;

  inherited DrawControl(AMat);
end;

procedure TavmSkills.DoValidate;

  function GetCellSpriteInfo(AIndex: Integer; out AUseReady: Boolean): string;
  var skill: IUnitSkill;
  begin
    AUseReady := True;
    if FSkills = nil then Exit('');
    if AIndex < 0 then Exit('');
    if AIndex >= FSkills.Count then Exit('');
    skill := FSkills.GetSkill(AIndex);
    if skill = nil then Exit('');
    AUseReady := skill.UseReady;
    Result := ExeRelativeFileName('ui\skills\'+skill.Ico);
  end;

  function GetCellSpriteInfo(x,y: Integer; out AUseReady: Boolean): string;
  begin
    Result := GetCellSpriteInfo(y * GridWidth + x, AUseReady);
  end;

  procedure DrawItemSprite(const ACellPos: TVec2; const ASpriteFile: string; ASkillReady: Boolean);
  var
    cellRB: TVec2;
  begin
    cellRB := ACellPos + Vec(cCellSize, cCellSize);

    if ASpriteFile <> '' then
    begin
      Canvas.Brush.Color := Vec(0.1, 0.1, 0.1, 1.0);
      Canvas.AddFill(ACellPos, cellRB);
      if ASkillReady then
        Canvas.Brush.Color := Vec(1,1,1,1)
      else
        Canvas.Brush.Color := Vec(0.3,0.3,0.3,0.3);
      Canvas.AddSprite(ACellPos, cellRB, ASpriteFile);
    end;

    Canvas.AddRectangle(ACellPos, cellRB);
  end;

var
  i, j: Integer;
  cellPos: TVec2;
  itemSprite: string;
  itemRct: TRectF;
  skillReady: Boolean;
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
      itemSprite := GetCellSpriteInfo(i, j, skillReady);
      DrawItemSprite(cellPos, itemSprite, skillReady);

      cellPos.x := cellPos.x + cCellSize + cCellBorderSize;
    end;
    cellPos.y := cellPos.y + cCellSize + cCellBorderSize;
  end;

  if (FDraggedItem <> -1) and (FSkills.GetSkill(FDraggedItem) <> nil) then
  begin
    itemSprite := GetCellSpriteInfo(FDraggedItem, skillReady);
    cellPos := FDraggetItemCoord - Vec(cCellSize*0.5, cCellSize*0.5);
    DrawItemSprite(cellPos, itemSprite, skillReady);
  end;

end;

procedure TavmSkills.AfterRegister;
begin
  inherited AfterRegister;
  FLastStateID := -1;
  FDropPosition := -1;
  FDraggedItem := -1;

  Origin := Vec(1,0);
  Pos := Vec(10, 10);
  GridWidth := 5;
  GridHeight := 10;

  FSkillHighlighter := TavmSkillHighlighter.Create(Self);
  FSkillHighlighter.Pos := Vec(0,0);
  FSkillHighlighter.Size := Vec(0,0);
  FSkillHighlighter.Visible := False;

  FSkillHint := TavmSkillHint.Create(Self);
  FSkillHint.Pos := Vec(0,0);
  FSkillHint.Origin := Vec(1, 0);
  FSkillHint.Visible := False;
end;

procedure TavmSkills.OnUPS;
begin
  inherited OnUPS;
  FSkillHint.Visible := (Main.Time64 - FLastMoveTime > 100) and
                        (not FDragStarted[0]) and
                        (FSkillHint.Skill <> nil);
end;

procedure TavmSkills.SetDropPosition(const AValue: Integer);
begin
  if FDropPosition = AValue then Exit;
  FDropPosition := AValue;
  Invalidate;
end;

procedure TavmSkills.SetActiveSkill(const AValue: IUnitSkill);
var
  i: Integer;
begin
  if FActiveSkill = AValue then Exit;
  FActiveSkill := AValue;
  if FActiveSkill = nil then
  begin
    FSkillHighlighter.Visible := False;
    Exit;
  end;

  for i:= 0 to FSkills.Count - 1 do
  begin
    if FSkills.GetSkill(i) = FActiveSkill then
    begin
      FSkillHighlighter.Rect := ItemRect(i);
      FSkillHighlighter.Visible := True;
      Exit;
    end;
  end;
  FSkillHighlighter.Visible := False;
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

