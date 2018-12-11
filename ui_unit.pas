unit ui_unit;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avBase, avCanvas, untLevel, mutils, ui_skills, ui_buffs;

type

  { TavmUnitBtn }

  TavmUnitBtn = class(TavmCustomButton)
  private
  protected
    procedure DoValidate; override;
  public
  end;

  { TavmUnitIco }

  TavmUnitIco = class(TavmCustomButton)
  protected
    procedure DoValidate; override;
  public
    constructor Create(AParent: TavObject); override;
  end;

  { TavmUnitMenu }

  TavmUnitMenu = class(TavmCustomControl)
  const
    cHPBarWidth = 536;
    cHPBarTop = 84;
    cAPBarTop = 53;
    cSkillSlotsTop = 154;
  private
    FRoomUnit: TRoomUnit;
    FLastAP: Integer;

    FEndTurnBtn: TavmUnitBtn;
    FUnitIco : TavmUnitIco;

    FSkillSlots: TavmSkills;
    FBuffsBar: TavmUnitBuffs;

    function GetOnAdjustToUnit: TNotifyEvent;
    function GetOnEndTurnClick: TNotifyEvent;
    procedure SetOnAdjustToUnit(const AValue: TNotifyEvent);
    procedure SetOnEndTurnClick(const AValue: TNotifyEvent);
    procedure SetRoomUnit(const AValue: TRoomUnit);
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
    procedure DrawControl(const AMat: TMat3); override;
  public
    property SkillSlots: TavmSkills read FSkillSlots;
    property RoomUnit: TRoomUnit read FRoomUnit write SetRoomUnit;
    property OnEndTurnClick: TNotifyEvent read GetOnEndTurnClick write SetOnEndTurnClick;
    property OnAdjustToUnit: TNotifyEvent read GetOnAdjustToUnit write SetOnAdjustToUnit;
  end;

implementation

{ TavmUnitIco }

procedure TavmUnitIco.DoValidate;
begin
  inherited DoValidate;
  Canvas.Clear;
  if Text <> '' then
    Canvas.AddSprite(Vec(0, 0), Size, Text);
  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0, 0), Size);
end;

constructor TavmUnitIco.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  Size := Vec(96, 128);
end;

{ TavmUnitBtn }

procedure TavmUnitBtn.DoValidate;
var bkColor: TVec4;
  txt: ITextLines;
begin
  inherited DoValidate;

  if Downed then
    bkColor := Vec(0.2, 0.2, 1.0, 1.0)
  else
    if Moved then
      bkColor := Vec(0.4, 0.4, 1.0, 1.0)
    else
      bkColor := Vec(0.3, 0.3, 1.0, 1.0);

  Canvas.Clear;
  Canvas.Brush.Color := bkColor;
  Canvas.AddFill(Vec(0,0), Size);
  Canvas.AddRectangle(Vec(0,0), Size);

  with Canvas.TextBuilder do
  begin
    Align := TLineAlign.laCenter;
    WriteLn(Text);
    txt := Finish();
    txt.BoundsX := Vec(0, Size.x);
    txt.BoundsY := Vec(0, Size.y);
    txt.VAlign := 0.5;
    Canvas.AddText(txt);
  end;
end;

{ TavmUnitMenu }

function TavmUnitMenu.GetOnEndTurnClick: TNotifyEvent;
begin
  Result := FEndTurnBtn.OnClick;
end;

function TavmUnitMenu.GetOnAdjustToUnit: TNotifyEvent;
begin
  Result := FUnitIco.OnClick;
end;

procedure TavmUnitMenu.SetOnAdjustToUnit(const AValue: TNotifyEvent);
begin
  FUnitIco.OnClick := AValue;
end;

procedure TavmUnitMenu.SetOnEndTurnClick(const AValue: TNotifyEvent);
begin
  FEndTurnBtn.OnClick := AValue;
end;

procedure TavmUnitMenu.SetRoomUnit(const AValue: TRoomUnit);
begin
  if FRoomUnit = AValue then Exit;
  FRoomUnit := AValue;
  FSkillSlots.Skills := TSkillSlots10Adapter.Create(AValue);
  if FRoomUnit = nil then
  begin
    FUnitIco.Text := '';
    FBuffsBar.Buffs := nil;
  end
  else
  begin
    FUnitIco.Text := FRoomUnit.Preview96_128;
    FBuffsBar.Buffs := FRoomUnit.AllBuffs();
  end;
  Invalidate;
end;

procedure TavmUnitMenu.DoValidate;

  procedure BuildHPText(const lt, rb: TVec2);
  var text: ITextLines;
  begin
    with Canvas.TextBuilder do
    begin
      Align := laCenter;
      WriteLn(IntToStr(RoomUnit.HP)+'/'+IntToStr(RoomUnit.MaxHP));
      text := Finish();
      text.BoundsX := Vec(lt.x, rb.x);
      text.BoundsY := Vec(lt.y, rb.y);
      text.VAlign := 0.5;
      Canvas.AddText(text);
    end;
  end;

var lt, rb, elsize: TVec2;
    cellpos: TVec2;
    cellname: string;
    i: Integer;
begin
  inherited DoValidate;
  Canvas.Clear;
  if RoomUnit = nil then Exit;
  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;

  //hp bar
    //background
  elsize := Vec(cHPBarWidth, 64);
  lt := Vec((Size.x - elsize.x)*0.5, cHPBarTop);
  rb := lt + elsize;
  Canvas.Brush.Color := Vec(0.3,0.3,0.3,0.3);
  Canvas.AddFill(lt, rb);
  Canvas.AddRectangle(lt, rb);
    //hp
  elsize := Vec(cHPBarWidth - 60, 24);
  lt := Vec((Size.x - elsize.x)*0.5, cHPBarTop + 24);
  rb.x := Lerp(lt.x, lt.x + elsize.x, clamp(RoomUnit.HP / RoomUnit.MaxHP, 0.0, 1.0));
  rb.y := lt.y + elsize.y;
  Canvas.Brush.Color := Vec(1,0,0,1);
  Canvas.AddFill(lt, rb);
  rb := lt + elsize;
  Canvas.AddRectangle(lt, rb);
  Canvas.Font.Size := 24;
  Canvas.Font.Color := Vec(0,0,0,0.5);
  Canvas.Font.Style := [gsBold];
  BuildHPText(lt, rb);
  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Style := [];
  BuildHPText(lt, rb);


  //AP bar
  Canvas.Brush.Color := Vec(1,1,1,1);
  elsize := Vec((RoomUnit.MaxAP - 1.0)*26.0, 26);
  for i := 0 to RoomUnit.MaxAP - 1 do
  begin
    cellpos.y := cAPBarTop;
    cellpos.x := Lerp((Size.x - elsize.x)*0.5, (Size.x + elsize.x)*0.5, i / (RoomUnit.MaxAP - 1));
    if i < RoomUnit.AP then
      cellname := 'ui\greencell.png'
    else
      cellname := 'ui\emptycell.png';
    Canvas.AddSprite(cellpos - Vec(13, 0), cellpos + Vec(13, 26), cellname);
  end;
  FLastAP := RoomUnit.AP;
end;

procedure TavmUnitMenu.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 1.0);
  Size := Vec(740, 223);

  FEndTurnBtn := TavmUnitBtn.Create(Self);
  FEndTurnBtn.Size := Vec(96, 64);
  FEndTurnBtn.Pos := Vec(Size.x - FEndTurnBtn.Size.x, cHPBarTop);
  FEndTurnBtn.Text := 'Конец хода';

  FUnitIco := TavmUnitIco.Create(Self);
  FUnitIco.Origin := Vec(0, 0);
  FUnitIco.Pos := Vec(0, cHPBarTop);

  FSkillSlots := TavmSkills.Create(Self);
  FSkillSlots.GridWidth := 10;
  FSkillSlots.GridHeight := 1;
  FSkillSlots.Origin := Vec(0.5, 0);
  FSkillSlots.HintDirection := Vec(0, -1.5);

  FBuffsBar := TavmUnitBuffs.Create(Self);
  FBuffsBar.Origin := Vec(0.5, 0.0);
  FBuffsBar.Pos := Vec(Size.x * 0.5, 0);
end;

procedure TavmUnitMenu.DrawControl(const AMat: TMat3);
begin
  if RoomUnit = nil then Exit;
  if FLastAP <> RoomUnit.AP then Invalidate;

  Pos := Vec(Main.WindowSize.x * 0.5, Main.WindowSize.y);
  FSkillSlots.Pos := Vec(Size.x*0.5, cSkillSlotsTop);
  inherited DrawControl(AMat);
end;

end.

