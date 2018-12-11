unit ui_buffs;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  untLevel,
  avMiniControls, avCanvas, avTypes, mutils;

type
  { TavmBuffHint }

  TavmBuffHint = class(TavmCustomControl)
  private
    FBuff: IUnitBuff;
    FNameText : ITextLines;
    FDescText : ITextLines;
    procedure BuildTextLines;
    procedure SetBuff(AValue: IUnitBuff);
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); override;
  public
    property Buff: IUnitBuff read FBuff write SetBuff;
  end;

  { TavmUnitBuffs }

  TavmUnitBuffs = class(TavmCustomControl)
  private const
    cIconSize = 48;
    cIconSpacing = 5;
  private
    FBuffHint : TavmBuffHint;
    FHintDirection: TVec2;

    FBuffs: IUnitBuffsArr;
    FOldBuffs: IUnitBuffsArr;
    FOldBuffsDuration: array of Integer;

    FLastMoveTime: Int64;

    function ItemRect(const ABuffIndex: Integer): TRectF;
    function HitToItems(const APt: TVec2): Integer;
    function BuffAt(const APt: TVec2): IUnitBuff;
    procedure SetBuffs(const AValue: IUnitBuffsArr);
  protected
    procedure Notify_MouseEnter; override;
    procedure Notify_MouseLeave; override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;

    procedure AfterRegister; override;
    procedure DoValidate; override;
    procedure DrawControl(const AMat: TMat3); override;
    procedure OnUPS; override;
  public
    property Buffs: IUnitBuffsArr read FBuffs write SetBuffs;
    property HintDirection: TVec2 read FHintDirection write FHintDirection;
  end;

implementation

uses Math;

{ TavmBuffHint }

procedure TavmBuffHint.BuildTextLines;
const cTextYSpace = 10;
      cTextXSpace = 15;
var
  tb: ITextBuilder;
  y: Single;
begin
  if FBuff = nil then
  begin
    FNameText := nil;
    FDescText := nil;
    Exit;
  end;

  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size  := 32;
  Canvas.Font.Style := [gsBold];
  tb := Canvas.TextBuilder;
  tb.Align := laCenter;
  tb.WriteWrapped(FBuff.Name);
  tb.WriteWrappedEnd(Size.x - cTextXSpace*2, True);

  FNameText := tb.Finish();
  FNameText.BoundsX := Vec(cTextXSpace, Size.x - cTextXSpace);

  Canvas.Font.Size := 24;
  Canvas.Font.Style := [];
  tb := Canvas.TextBuilder;
  tb.Align := laLeft;
  tb.WriteWrapped(FBuff.Desc);
  tb.WriteWrappedEnd(Size.x - cTextXSpace*2, True);

  FDescText := tb.Finish();
  FDescText.BoundsX := Vec(cTextXSpace, Size.x - cTextXSpace);

  y := cTextYSpace;
  FNameText.BoundsY := Vec(y, y + FNameText.TotalHeight());
  y := y + FNameText.TotalHeight() + cTextYSpace;
  FDescText.BoundsY := Vec(y, y + FDescText.TotalHeight());
  y := y + FDescText.TotalHeight() + cTextYSpace;

  Size := Vec(Size.x, y);
end;

procedure TavmBuffHint.SetBuff(AValue: IUnitBuff);
begin
  if FBuff = AValue then Exit;
  FBuff := AValue;
  BuildTextLines;
  Invalidate;
end;

procedure TavmBuffHint.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(300, 200);
end;

procedure TavmBuffHint.DoValidate;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Brush.Color := Vec(0.125, 0.125, 0.125, 1);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);

  Canvas.AddText(FNameText);
  Canvas.AddText(FDescText);
end;

procedure TavmBuffHint.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin
  AControl := nil;
end;

{ TavmUnitBuffs }

function TavmUnitBuffs.ItemRect(const ABuffIndex: Integer): TRectF;
begin
  Result.min := Vec(ABuffIndex * (cIconSize + cIconSpacing), cIconSpacing);
  Result.max := Result.min + Vec(cIconSize, cIconSize);
end;

function TavmUnitBuffs.HitToItems(const APt: TVec2): Integer;
var i: Integer;
begin
  Result := -1;
  if FOldBuffs = nil then Exit;
  for i := 0 to FOldBuffs.Count - 1 do
    if ItemRect(i).PtInRect(APt) then
      Exit(i);
end;

function TavmUnitBuffs.BuffAt(const APt: TVec2): IUnitBuff;
var n: Integer;
begin
  n := HitToItems(APt);
  if n < 0 then Exit(nil);
  Result := FOldBuffs[n];
end;

procedure TavmUnitBuffs.SetBuffs(const AValue: IUnitBuffsArr);
begin
  if FBuffs = AValue then Exit;
  FBuffs := AValue;
  Invalidate;
end;

procedure TavmUnitBuffs.Notify_MouseEnter;
begin
  inherited Notify_MouseEnter;
  UPSSubscribe;
end;

procedure TavmUnitBuffs.Notify_MouseLeave;
begin
  inherited Notify_MouseLeave;
  UPSUnSubscribe;
  FBuffHint.Visible := False;
end;

procedure TavmUnitBuffs.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var
  rct: TRectF;
begin
  inherited Notify_MouseMove(APt, AShifts);
  FLastMoveTime := Main.Time64;
  FBuffHint.Buff := BuffAt(APt);
  if FBuffHint.Buff <> nil then
  begin
    FBuffHint.Origin := Sign(FHintDirection) * Vec(-0.5, -0.5) + Vec(0.5, 0.5);
    rct := ItemRect(HitToItems(APt));
    FBuffHint.Pos := Lerp(rct.min, rct.max, FHintDirection * Vec(0.5, 0.5) + Vec(0.5, 0.5));
  end;
end;

procedure TavmUnitBuffs.AfterRegister;
begin
  inherited AfterRegister;
  FBuffHint := TavmBuffHint.Create(Self);
  FBuffHint.Visible := False;
  FHintDirection := Vec(0, -1.5);
end;

procedure TavmUnitBuffs.DoValidate;
var i: Integer;
    rct: TRectF;
    tl, br: TVec2;
    txtBuilder: ITextBuilder;
    txtLines  : ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Font.Size := 24;
  Canvas.Font.Color := Vec(1,1,1,1);

  if FOldBuffs = nil then Exit;
  if FOldBuffs.Count = 0 then Exit;

  for i := 0 to FOldBuffs.Count - 1 do
  begin
    Canvas.Brush.Color := Vec(1,1,1,1);

    rct := ItemRect(i);
    tl := rct.min;
    br := rct.max;
    Canvas.AddSprite(tl, br, 'ui\buffs\' + FOldBuffs[i].Ico);

    txtBuilder := Canvas.TextBuilder;
    txtBuilder.Align := laRight;
    txtBuilder.Write(IntToStr(FOldBuffsDuration[i]));
    txtLines := txtBuilder.Finish();
    txtLines.BoundsX := Vec(tl.x, br.x);
    txtLines.BoundsY := Vec(tl.y, br.y);
    txtLines.VAlign := 1;

    Canvas.Brush.Color := Vec(0,0,0,1);
    Canvas.AddFill(br - Vec(txtLines.MaxLineWidth() + 3, txtLines.TotalHeight()), br + Vec(3, 0));

    Canvas.Brush.Color := Vec(1,1,1,1);
    Canvas.AddText(txtLines);
  end;
end;

procedure TavmUnitBuffs.DrawControl(const AMat: TMat3);

  function IsEqual(const ABuffs1, ABuffs2: IUnitBuffsArr): Boolean;
  var i: Integer;
  begin
    if ABuffs1 = ABuffs2 then Exit(True);
    if ABuffs1 = nil then Exit(False);
    if ABuffs2 = nil then Exit(False);
    if ABuffs1.Count <> ABuffs2.Count then Exit(False);
    for i := 0 to ABuffs1.Count - 1 do
      if ABuffs1[i] <> ABuffs2[i] then Exit(False);
    Result := True;
  end;

  function SyncBuffsDuration: Boolean;
  var i: Integer;
  begin
    Result := False;
    if FOldBuffs = nil then Exit;
    if Length(FOldBuffsDuration) <> FOldBuffs.Count then
    begin
      SetLength(FOldBuffsDuration, FOldBuffs.Count);
      Result := True;
    end;
    for i := 0 to FOldBuffs.Count - 1 do
      if FOldBuffsDuration[i] <> FOldBuffs[i].Duration then
      begin
        Result := True;
        FOldBuffsDuration[i] := FOldBuffs[i].Duration;
      end;
  end;

begin
  if not IsEqual(FOldBuffs, FBuffs) then
  begin
    if FBuffs <> nil then
      FOldBuffs := FBuffs.Clone()
    else
      FOldBuffs := nil;
    Invalidate;
  end;

  if SyncBuffsDuration then
    Invalidate;

  if FOldBuffs <> nil then
    Size := Vec(FOldBuffs.Count * cIconSize + Max(0, FOldBuffs.Count - 1) * cIconSpacing, cIconSize + cIconSpacing)
  else
    Size := Vec(0,0);

  inherited DrawControl(AMat);
end;

procedure TavmUnitBuffs.OnUPS;
begin
  inherited OnUPS;
  FBuffHint.Visible := (Main.Time64 - FLastMoveTime > 100) and
                       (FBuffHint.Buff <> nil);
end;

end.

